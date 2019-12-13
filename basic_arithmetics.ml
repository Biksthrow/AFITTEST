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
let rec bezout a b =
  let r = a in
  let u = 1 in
  let v = 0 in
  let r_prime = b in
  let u_prime = 0 in
  let v_prime = 1 in
  let rec calcul r u v r_prime u_prime v_prime =
    if r_prime = 0 then 
      (u,v,r)
    else 
      calcul r_prime u_prime v_prime (r-((r/r_prime)*r_prime)) (u-((r/r_prime)*u_prime)) (v-((r/r_prime)*v_prime))
  in 
   calcul r u v r_prime u_prime v_prime;;
