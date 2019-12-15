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
  let rec pow c e =
    if e <= n then
      pow(modulo (c*x) m) (e+1)
    else
      c
  in
     pow 1 1;;

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
        mod_power x (modulo n (p-1)) p
        
        
        
        
        let modulo a b =
  let rec sous_quot1 a b =
      if (a>=0) && (a< (abs b)) then
        a
      else 
        sous_quot1 (a-b) b 
  in
  let rec sous_quot2 a b =
      if (a>=0) && (a<(abs b)) then
        a
      else 
        sous_quot2 (a+b) b 
    in
      if (a >0 && b >0) || (a<0 && b <0) then 
        sous_quot1 a b 
      else
         sous_quot2 a b ;;
