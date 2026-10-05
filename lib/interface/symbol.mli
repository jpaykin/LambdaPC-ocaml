type t = int

module Id : sig
  type nonrec t = t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val register : t -> string -> unit
  val intern : string -> t
  val name : t -> string
end

module Ord : Map.OrderedType with type t = t
module Map : Map.S with type key = t
module Set : Set.S with type elt = t

module Map_helpers : sig
  val of_list : (t * 'a) list -> 'a Map.t
  val add_list : (t * 'a) list -> 'a Map.t -> 'a Map.t
  val find_exn : t -> 'a Map.t -> 'a
  val keys : 'a Map.t -> Set.t
  val values : 'a Map.t -> 'a list

  val lookup : 'a Map.t -> t -> 'a option
  val bind : 'a Map.t -> t -> 'a -> 'a Map.t
  val unbind : 'a Map.t -> t -> 'a Map.t
  val mem : 'a Map.t -> t -> bool
  val singleton : t -> 'a -> 'a Map.t
end

module Set_helpers : sig
  val of_list : t list -> Set.t
  val to_list : Set.t -> t list
end

module Fresh : sig
  val counter : t ref
  val ensure_ge : int -> unit
  val fresh_id : unit -> t
  val gensym : ?hint:string -> unit -> t
end

module Resolve_scope : sig
  val clear_bindings : unit -> unit
  val resolve_binding : t -> t option
  val with_scope : (unit -> 'a) -> 'a
  val with_binding : t -> t -> (unit -> 'a) -> 'a
  val with_fresh_binding : ?hint:string -> t -> (t -> 'a) -> 'a
end
