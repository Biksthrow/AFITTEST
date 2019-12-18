(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  let x = sign_b bA in
  let y = sign_b bB in
  let rec pgcd a b n =
     if mod_b a n = 0 && mod_b b n = 0 then
       match (x,y) with
         |(0,0) -> 0 :: n
         |(1,1) -> 0 :: n
         |_ -> 1 :: n
    else
      pgcd a b (diff_n n 1)
  in
  if (>>) abs_b(bA) abs_b(b) then
    match (bA,bB) with
      | [] -> []
      |(e::l,r::q) -> pgcd l q q
  else
     match (bA,bB) with
      | [] -> []
      |(e::l,r::q) -> pgcd q l l;;
(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB = ([], [], [] )
