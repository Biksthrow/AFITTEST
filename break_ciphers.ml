(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
*)

let isprime n =
  let rec second acu =
    match acu with
        acu when ( n mod acu !=0) && (acu !=1) -> second (acu-1)
      |_ -> false
  in second (n-1);;



let break key =
  let(x,_) = key in
  let rec destruc x arg =
      if (x mod arg) = 0 then
        (arg,x/arg)
      else
        destruc x (arg+1)
  in destruc x 2;;
  
  
