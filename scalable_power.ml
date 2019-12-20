(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec po l n acu=
    if compare_b n [0;0] = 1 then
      po l (diff_b n [0;1]) (mult_n acu l)
    else
      if (>>) x [0;0] then
        0:: acu
      else
        if mod_b n [0;0;1] = [0;0] then
         0::  acu
        else
          1 :: acu
  in
  match x with
      e:: l -> po l [0;0] [1]
    |_ -> [0;0];;
      

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
*)
let rec search bitarray x =
  match bitarray with
    |[] -> 0
    |e::l when e = x -> 1
    |_::l -> search l x;;


let power x n =
  if search (mod_b n [0;0;2]) 1 = 0 then
    pow (pow x [0;0;2]) (quot_b n [0;0;2])
  else
    mult_b x (pow(pow x 2)) (quot_b (diff_b n [0;1]) 2);;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec pow x n m =
    if search (mod_b n [0;0;2]) 1 = 0 then
      [0;1]

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = []
