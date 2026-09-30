
module type FIN = sig val dim : int end
module FIN2 : FIN 
module FIN3 : FIN
module FIN4 : FIN

module Extend : functor (_ : FIN) -> FIN
module ExtendMod : functor (_ : FIN) -> FIN

module type Z_SIG = sig
    type t
    module Dim : FIN
    val t_of_int : int -> t
    val int_of_t : t -> int
    val string_of_t : t -> string
    val normalize : int -> int
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( - ) : t -> t -> t
end
module Z : functor (_ : FIN) ->  Z_SIG

module type SCALARS = sig
    module Zd : Z_SIG
    module Zd' : Z_SIG
    module Zd0 : Z_SIG

    val inc_d_d' : Zd.t -> Zd'.t
    val mod_d'_d  : Zd'.t -> Zd.t
    val times_d   : Zd0.t -> Zd'.t
    val times_d2  : Zd0.t -> Zd.t (* d/2 *)
    val div_d     : Zd'.t -> Zd0.t

    val sgn       : Zd'.t -> Zd0.t
end

module Scalars : functor (_ : FIN) -> SCALARS

(*
module MakeZd : functor (_ : FIN) -> ZD

module Make : functor (D : FIN) -> sig
    module Zd : ZD
    module Zd' : ZD
    module Zd'_div_d : ZD
end

module type SCALAR_TYPE = sig
    module Zd : ZD
    module Zd' : ZD
    val sgn_extended : Zd'.t -> int
end
module MakeScalar : functor (_ : FIN) -> SCALAR_TYPE
*)