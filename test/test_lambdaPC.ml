open Alcotest
open PCLib

let check_eval source expected =
  let actual = Interface.Eval2.evalClosed (Interface.parse source) in
  check string source expected (LambdaPC.Val.string_of_t actual)

let test_parse_zero () =
  match Interface.parse "zero{Zd}" with
  | { LambdaPC.Expr.node =
        LExpr { Ast.Expr.node = Zero;
                ty = Some { Ast.Type.node = Unit; loc = Some _ };
                loc = Some _ };
      loc = Some _; _ } -> ()
  | ast ->
      failf "Expected a source-located, typed core zero, got %s"
        (LambdaPC.Expr.string_of_t ast)

let test_parse_braced_injections () =
  let check_in1 () =
    match Interface.parse "in1{Pauli} X" with
    | { LambdaPC.Expr.node =
          In1
            { tp = { LambdaPC.Type.node = Pauli; _ }
            ; v = { LambdaPC.Expr.node = LExpr _; _ }
            }
      ; _ } -> ()
    | ast ->
        failf "Expected braced in1 term, got %s"
          (LambdaPC.Expr.pretty_string_of_t ast)
  in
  let check_in2 () =
    match Interface.parse "in2{Pauli} X" with
    | { LambdaPC.Expr.node =
          In2
            { tp = { LambdaPC.Type.node = Pauli; _ }
            ; v = { LambdaPC.Expr.node = LExpr _; _ }
            }
      ; _ } -> ()
    | ast ->
        failf "Expected braced in2 term, got %s"
          (LambdaPC.Expr.pretty_string_of_t ast)
  in
  check_in1 ();
  check_in2 ()

let test_parse_reports_resolve_errors () =
  match Interface.parse "let x = X in\n y" with
  | _ -> fail "Expected unresolved identifier to raise Parse_error"
  | exception Interface.Parse_error (loc, msg) ->
      check string "unbound variable location" "<stdin>:2:1-2" loc;
      check bool "resolve error mentions unbound variable"
        true (String.starts_with ~prefix:"unbound LambdaPC variable" msg)

let test_parse_shadowing () =
  match Interface.parse "let x = X in let x = x in x" with
  | { LambdaPC.Expr.node = Let { x = outer;
        body = { node = Let { x = inner;
          expr = { node = Var rhs; _ };
          body = { node = Var use; _ } }; _ }; _ }; _ } ->
      check bool "shadowed binders are distinct" false
        (Ident.Ident.equal outer inner);
      check bool "let rhs uses the outer binder" true
        (Ident.Ident.equal outer rhs);
      check bool "let body uses the inner binder" true
        (Ident.Ident.equal inner use);
      check bool "binder and occurrence retain distinct locations" true
        (inner.loc <> use.loc)
  | _ -> fail "Expected nested core let expressions"

let test_core_seed_fresh_expr () =
  match Interface.parse "let x = X in x" with
  | { LambdaPC.Expr.node = Let { x; _ }; _ } as expr ->
      Ast.Expr.update_env (LambdaPC.SymplecticForm.psi_of expr);
      let fresh = Fresh.fresh ~hint:"after_parse" () in
      check bool "fresh is above resolved core binder" true (fresh > x.sym)
  | _ -> fail "Expected a core let expression"

let test_parse_evaluation () =
  List.iter (fun (source, expected) -> check_eval source expected)
    [ "X", "<0> X"
    ; "zero{Zd + Zd}", "<0> I"
    ; "X + Z", "<0> Y"
    ; "1 .* X", "<0> X"
    ; "X^{1}", "<0> X"
    ; "<1> Z", "<1> Z"
    ; "let x = X in let x = Z in x", "<0> Z"
    ; "(lambda q : Pauli. case q of { X -> Z | Z -> X }) @ X", "<0> Z"
    ; "in1{Pauli} X", "<0> (X, I)"
    ; "in2{Pauli} Z", "<0> (I, Z)"
    ; "case in1{Pauli} X of { in1 q -> q | in2 r -> r }", "<0> X"
    ; "suspend X", "<0> X"
    ; "(lambda x : Zd. var x) .@ 1", "<0> 1"
    ; ". case X of { in1 x -> var x | in2 z -> var z }", "<0> 1"
    ];
  Interface.eval (Interface.parse "X")

let test_parse_pc_application () =
  let clifford = Interface.pc "lambda q : Pauli. q" in
  let actual = Interface.Eval2.evalClosed Interface.(clifford @ parse "Y") in
  check string "parsed Clifford applied to parsed expression"
    "<0> Y" (LambdaPC.Val.string_of_t actual);
  Interface.typecheck clifford

let test_parse_from_file () =
  let filename = Filename.temp_file "lambdapc-parser-" ".pc" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Out_channel.with_open_bin filename
        (fun channel -> output_string channel "let x = Y in x");
      let expr = Interface.parseFromFile filename in
      (match expr.loc with
       | Some loc -> check string "source filename" filename loc.sp.pos_fname
       | None -> fail "Expected source location from file");
      check string "file expression evaluates" "<0> Y"
        (LambdaPC.Val.string_of_t (Interface.Eval2.evalClosed expr)))

let test_rename_resolved_variable () =
  let pc = Interface.pc "lambda q : Pauli. q" in
  let LambdaPC.Expr.Lam { x; body; _ } = pc.node in
  let replacement = Ident.Ident.fresh () in
  match (LambdaPC.Expr.rename_var x replacement body).node with
  | Var actual ->
      check bool "rename follows symbol identity across locations" true
        (Ident.Ident.equal replacement actual)
  | _ -> fail "Expected renamed variable"

let test_rename_preserves_bound_variables () =
  let expr = Interface.parse "let x = X in x" in
  match expr.node with
  | Let { x; body = { node = Var use; _ }; _ } ->
      let replacement = Ident.Ident.fresh () in
      (match (LambdaPC.Expr.rename_var use replacement expr).node with
       | Let { body = { node = Var actual; _ }; _ } ->
           check bool "let binder stops renaming despite different locations" true
             (Ident.Ident.equal x actual)
       | _ -> fail "Expected let expression")
  | _ -> fail "Expected resolved let expression"

let suite =
  ["TestPCZ2", [
    test_case "Parser accepts typed core zero" `Quick test_parse_zero;
    test_case "Parser accepts braced injections" `Quick test_parse_braced_injections;
    test_case "Parser reports resolve errors" `Quick test_parse_reports_resolve_errors;
    test_case "Resolver preserves shadowing and locations" `Quick test_parse_shadowing;
    test_case "Core AST seeds freshness" `Quick test_core_seed_fresh_expr;
    test_case "Parsed expressions evaluate" `Quick test_parse_evaluation;
    test_case "Parsed Clifford applies and typechecks" `Quick test_parse_pc_application;
    test_case "File parsing returns executable core AST" `Quick test_parse_from_file;
    test_case "Rename resolved variable by symbol" `Quick test_rename_resolved_variable;
    test_case "Rename respects source-located binders" `Quick test_rename_preserves_bound_variables;
  ]]
