type t = int

let compare : t -> t -> int = Int.compare
let equal (a : t) (b : t) : bool = (a = b)

(* global id stream shared by intern + gensym *)
let next : t ref = ref 0

let tbl : (string, t) Hashtbl.t = Hashtbl.create 1024
let rev : (t, string) Hashtbl.t = Hashtbl.create 1024

let counter : t ref = next

(*
   If we are loading an AST with pre-exising ids,
   we want to make sure our counter is up to date
*)
let ensure_ge (n : int) : unit =
  if !next < n then next := n


let fresh_id () : t =
  let id = !next in
  next := id + 1;
  id

(* Store mapping from id -> name *)
let register (id : t) (s : string) : unit =
  Hashtbl.replace rev id s

(* Allocate new id for symbol, or return existing id
   if symbol is already interned
*)
let intern (s : string) : t =
  match Hashtbl.find_opt tbl s with
  | Some id -> id
  | None ->
      let id = fresh_id () in
      Hashtbl.add tbl s id;
      register id s;
      id

(* Generate uniqe identifier with symbol hint.
   Basically we want to create a new var that shares
   a 'hint' with another one
*)
let gensym ?(hint = "x") () : t =
  let id = fresh_id () in
  register id (hint ^ "$" ^ string_of_int id);
  id

let name (id : t) : string =
  match Hashtbl.find_opt rev id with
  | Some s -> s
  | None -> "v$" ^ string_of_int id


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
let bind ~(key : t) ~(value : t) : unit =
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
    | None -> name key
  in
  let fresh = gensym ~hint:hint' () in
  bind ~key ~value:fresh;
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
let with_binding ~(key : t) ~(value : t) (f : unit -> 'a) : 'a =
  with_scope (fun () ->
      bind ~key ~value;
      f ())

(* Run f with a temporary key -> fresh binding *)
let with_fresh_binding ?hint ~(key : t) (f : t -> 'a) : 'a =
  with_scope (fun () ->
      let fresh = bind_fresh ?hint key in
      f fresh)
