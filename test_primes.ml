(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power


(** Deterministic primality test *)
let is_prime n =
  let rec second acu =
    match acu with 
     1 -> true
    | acu when (n mod acu != 0) && (acu!=1) -> second (acu-1)
    |_ -> false
  in second (n-1);;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec pseudo liste =
    match liste with 
     [] -> true
    | e::l -> (mod_power e p p = modulo e p) && pseudo l
  in 
  if p <=1 then 
    false 
  else 
    pseudo test_seq;;
