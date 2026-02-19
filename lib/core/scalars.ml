module type FIN = sig val dim : int end
module FIN2 : FIN = struct let dim = 2 end
module FIN3 : FIN = struct let dim = 3 end
module FIN4 : FIN = struct let dim = 4 end


module Extend (D : FIN) : FIN = struct
  let dim = if D.dim mod 2 = 0
                then 2 * D.dim
                else D.dim
end
module ExtendMod (D : FIN) : FIN = struct
  let dim = if D.dim mod 2 = 0
            then 2 (*d is even*)
            else 1 (*d is odd*)
end


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
module Z (D : FIN) : Z_SIG = struct
  type t = int
  module Dim = D

  let modpos a b = let r = a mod b in if r < 0 then r + b else r
  let t_of_int x = modpos x D.dim
  let int_of_t x = x
  let string_of_t = string_of_int
  let normalize x = int_of_t (t_of_int x)
  let (+) x y = modpos (x + y) D.dim
  let (-) x y = modpos (x - y) D.dim
  let ( * ) x y = modpos (x*y) D.dim
end


module type SCALARS = sig
    module Zd  : Z_SIG
    module Zd' : Z_SIG
    module Zd0 : Z_SIG

    val inc_d_d' : Zd.t -> Zd'.t
    val mod_d'_d  : Zd'.t -> Zd.t
    val times_d   : Zd0.t -> Zd'.t
    val times_d2  : Zd0.t -> Zd.t (* d/2 *)
    val div_d     : Zd'.t -> Zd0.t

    val sgn       : Zd'.t -> Zd0.t
end
module Scalars (D : FIN) : SCALARS = struct
  module Zd = Z(D)
  module Zd' = Z(Extend(D))
  module Zd0 = Z(ExtendMod(D))

  let inc_d_d' (r : Zd.t) : Zd'.t =
    Zd'.t_of_int (Zd.int_of_t r)
  let mod_d'_d (r' : Zd'.t) : Zd.t =
    Zd.t_of_int (Zd'.int_of_t r')
  let times_d (r0 : Zd0.t) : Zd'.t =
    let x0 = Zd0.int_of_t r0 in
    Zd'.t_of_int (D.dim * x0)

  let div_d (r' : Zd'.t) : Zd0.t =
    let x' = Zd'.int_of_t r' in
    Zd0.t_of_int (x' / D.dim)


  let times_d2 (r0 : Zd0.t) : Zd.t =
    let x0 = Zd0.int_of_t r0 in
    let d = Zd.Dim.dim in
    Zd.t_of_int ((d * x0) / 2)

  let sgn = div_d
end

(*
module type SCALAR_TYPE = sig
    module Zd : ZD
    module Zd' : ZD
    val sgn_extended : Zd'.t -> int
end
module MakeScalar (D : FIN) = struct
  module Zd = MakeZd(D)

  module Zd' = MakeZd(Extend(D))

  let sgn_extended (r' : Zd'.t) : int = 
    let x = Zd'.int_of_t r' in
    (x - (x mod D.dim)) / D.dim
end
*)
