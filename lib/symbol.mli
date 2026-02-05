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
