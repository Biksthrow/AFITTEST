(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
*)

let pow x n =
  if x = 0 && n=0 then
    1
  else
    let rec power x n r =
      if n!=0 then
        power x (n-1) (r*x)
      else
        r
    in
    power x n 1;;
(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n mod 2 = 0 then
    pow (pow x 2) (n/2)
  else
    x*(pow(pow x 2) ((n-1)/2));;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec pow x n m =
    if n=0 then
      1
    else
      if modulo n 2 = 0 then
        let z = pow x (n/2) m
        in modulo (z*z) m
      else modulo ((modulo x m)* pow x (n-1) m) m
       
  in
     pow x n m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if x = 0 then
    0
  else
    if n = p then
      mod_power x 1 p
    else
      if n = p-1 then
        1
      else
        mod_power x (modulo n (p-1)) p;;


(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)

