type t = int

val compare : t -> t -> int
val equal : t -> t -> bool

val counter : t ref

val ensure_ge : int -> unit
val fresh_id : unit -> t
val register : t -> string -> unit
val intern : string -> t
val gensym : ?hint:string -> unit -> t
val name : t -> string

(* Global, scoped variable binding map: key symbol -> resolved symbol *)
val clear_bindings : unit -> unit
val resolve_binding : t -> t option

(* Run within a temporary scope. Restores previous bindings on exit. *)
val with_scope : (unit -> 'a) -> 'a

(* Temporarily bind key -> value while running f. *)
val with_binding : key:t -> value:t -> (unit -> 'a) -> 'a

(* Create a fresh value symbol, bind key -> fresh symbol while running f. *)
val with_fresh_binding : ?hint:string -> key:t -> (t -> 'a) -> 'a
