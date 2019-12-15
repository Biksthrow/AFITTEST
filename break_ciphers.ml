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



let break (key,autre) =
  let rec destruc key arg =
    if isprime arg then
      if (modulo key arg) = 0 then
        if isprime (quot key arg) && isprime arg then
          ((quot key arg),arg)
        else
          destruc key (arg+1)
      else
        destruc key (arg+1)
    else
      destruc key (arg+1)
  in destruc key 2;;
  
  
