(** Basic arithmetics with built-in integers *)
open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
  
let rec gcd a b =
  let rec pgcd a b n =
     if modulo a n = 0 && modulo b n = 0 then
      n
    else
      pgcd a b (n-1)
  in
  if ((a >0) && (b >0)) || ((a < 0) && (b<0)) then
    if a >= b then 
      pgcd a b b
    else
      pgcd b a a
  else
    if a >= 0 then
      pgcd a b a
    else
      pgcd b a b
      ;;
     
  

(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = (0, 0, 0)
