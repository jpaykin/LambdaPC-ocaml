type t = int

module Shared = struct
  (* Shared symbol state: id stream + name tables. *)
  let next : t ref = ref 0
  let tbl : (string, t) Hashtbl.t = Hashtbl.create 1024
  let rev : (t, string) Hashtbl.t = Hashtbl.create 1024
end

(* Low-level API for generating new binder ids *)
module Fresh = struct
  let counter : t ref = Shared.next

  (*
     If we are loading an AST with pre-exising ids,
     we want to make sure our counter is up to date
  *)
  let ensure_ge (n : int) : unit =
    if !Shared.next < n then Shared.next := n

  let fresh_id () : t =
    let id = !Shared.next in
    Shared.next := id + 1;
    id

  (* Generate uniqe identifier with symbol hint.
     Basically we want to create a new var that shares
     a 'hint' with another one
  *)
  let gensym ?(hint = "x") () : t =
    let id = fresh_id () in
    Hashtbl.replace Shared.rev id (hint ^ "$" ^ string_of_int id);
    id
end

(* Low-level api for identity ops *)
module Id = struct
  type nonrec t = t

  let compare : t -> t -> int = Int.compare
  let equal (a : t) (b : t) : bool = (a = b)

  (* Store mapping from id -> name *)
  let register (id : t) (s : string) : unit =
    Hashtbl.replace Shared.rev id s

  let name (id : t) : string =
    match Hashtbl.find_opt Shared.rev id with
    | Some s -> s
    | None -> "v$" ^ string_of_int id

  (* Allocate new id for symbol, or return existing id
     if symbol is already interned
  *)
  let intern (s : string) : t =
    match Hashtbl.find_opt Shared.tbl s with
    | Some id -> id
    | None ->
        let id = Fresh.fresh_id () in
        Hashtbl.add Shared.tbl s id;
        register id s;
        id
end

module Ord = struct
  type nonrec t = t
  let compare = Id.compare
end

(* Persistent env data for eval step *)
module Map = Map.Make(Ord)
module Set = Set.Make(Ord)

module Map_helpers = struct
  let of_list (xs : (t * 'a) list) : 'a Map.t =
    List.fold_left (fun acc (k, v) -> Map.add k v acc) Map.empty xs

  let add_list (xs : (t * 'a) list) (m : 'a Map.t) : 'a Map.t =
    List.fold_left (fun acc (k, v) -> Map.add k v acc) m xs

  let find_exn (key : t) (m : 'a Map.t) : 'a =
    match Map.find_opt key m with
    | Some v -> v
    | None -> failwith ("Symbol.Map: key not found (" ^ Id.name key ^ ")")

  let keys (m : 'a Map.t) : Set.t =
    Map.fold (fun k _ acc -> Set.add k acc) m Set.empty

  let values (m : 'a Map.t) : 'a list =
    Map.fold (fun _ v acc -> v :: acc) m [] |> List.rev

  let lookup m sym = Map.find_opt sym m
  let bind m sym v = Map.add sym v m
  let unbind m sym = Map.remove sym m
  let mem m sym = Map.mem sym m
  let singleton sym v = Map.singleton sym v
end

module Set_helpers = struct
  let of_list (xs : t list) : Set.t =
    List.fold_left (fun acc x -> Set.add x acc) Set.empty xs

  let to_list (s : Set.t) : t list =
    Set.elements s
end

(* Temp binding for resolve step, seperate from eval *)
module Resolve_scope = struct
  (* GLOBAL BINDING MAP:
     The idea comes from: https://craftinginterpreters.com/local-variables.html
     Keep a stack of scopes, each scope traks local var bindings,
     entering a scope pushes a new array to the stack, and exiting scope
     pops top most scope, taking all bindings with it.
  *)

  (* key symbol -> currently active resolved symbol *)
  let bindings : (t, t) Hashtbl.t = Hashtbl.create 1024
  let scope_stack : (t * t option) list list ref = ref [ [] ]

  let clear_bindings () : unit =
    Hashtbl.reset bindings;
    scope_stack := [ [] ]

  (* Push nested scope frame for temp bindings *)
  let push_scope () : unit =
    scope_stack := [] :: !scope_stack

  (* Pop current scope and restore prev bindings *)
  let pop_scope () : unit =
    match !scope_stack with
    | [] ->
        invalid_arg "pop_scope: no active scope"
    | [ _ ] ->
        invalid_arg "pop_scope: cannot pop root scope"
    | changes :: tl ->
        (* Roll back bindings made in scope *)
        List.iter
          (fun (key, prev) ->
            match prev with
            | Some value -> Hashtbl.replace bindings key value
            | None -> Hashtbl.remove bindings key)
          changes;
        scope_stack := tl

  (* Bind key -> value in current scope/log prev value for rollback *)
  let bind (key : t) (value : t) : unit =
    let prev = Hashtbl.find_opt bindings key in
    (* Record prev mapping in the current frame *)
    (match !scope_stack with
    | [] ->
        scope_stack := [ [ (key, prev) ] ]
    | scope :: tl ->
        scope_stack := ((key, prev) :: scope) :: tl);
    Hashtbl.replace bindings key value

  (* Return the currently active binding for key *)
  let resolve_binding (key : t) : t option =
    Hashtbl.find_opt bindings key

  (* Create a fresh symbol and bind key -> fresh in current scope *)
  let bind_fresh ?hint (key : t) : t =
    let hint' =
      match hint with
      | Some h -> h
      | None -> Id.name key
    in
    let fresh = Fresh.gensym ~hint:hint' () in
    bind key fresh;
    fresh

  (* Run f inside a temporary scope that gets popped *)
  let with_scope (f : unit -> 'a) : 'a =
    push_scope ();
    (* Always pop scope, including on exceptions *)
    try
      let v = f () in
      pop_scope ();
      v
    with ex ->
      pop_scope ();
      raise ex

  (* Run f with a temporary key -> value binding *)
  let with_binding (key : t) (value : t) (f : unit -> 'a) : 'a =
    with_scope (fun () ->
        bind key value;
        f ())


  (* Run f with a temporary key -> fresh binding *)
  let with_fresh_binding ?hint (key : t) (f : t -> 'a) : 'a =
    with_scope (fun () ->
        let fresh = bind_fresh ?hint key in
        f fresh)
end

(*
module HOAS = struct
  let var (loc : Loc.t) (x : Ident.t) : LambdaPC_Surface.expr =
    { loc; ty = None; node = Var x }

  let letin (loc : Loc.t)
      (rhs : LambdaPC_Surface.expr)
      (k : LambdaPC_Surface.expr -> LambdaPC_Surface.expr)
      : LambdaPC_Surface.expr =
    let x = Ident.fresh ~hint:"x" loc () in
    { loc; ty = None; node = Let { x; rhs; body = k (var loc x) } }

  let lambda ~(loc : Loc.t) tp
      (k : LambdaPC_Surface.expr -> LambdaPC_Surface.expr)
      : LambdaPC_Surface.pc =
    let x = Ident.fresh ~hint:"x" loc () in
    { loc; node = Lam { x; tp; body = k (var loc x) } }
end
*)
