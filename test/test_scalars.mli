open PCLib
open Scalars

module TestZ (ZD : Z_SIG) : sig

  val check_eq : string -> ZD.t -> ZD.t -> Alcotest.return
  val check_neq : string -> ZD.t -> ZD.t -> Alcotest.return
  val test_eq_of_int : int -> int -> Alcotest.return
  val test_neq_of_int : int -> int -> Alcotest.return

end

module TestConversions (_ : SCALARS) : sig

  val test_inc_d_d' : int -> Alcotest.return
  val test_mod_d'_d : int -> Alcotest.return
  val test_sgn : int -> Alcotest.return

end

val suite : (string * Alcotest.return Alcotest.test_case list) list
