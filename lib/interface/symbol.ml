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
