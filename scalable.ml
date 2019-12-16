(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let comple liste x =
   let rec change liste relance  =
     match (liste,relance) with
      |([],_) -> []
      |(e::l,false) when e = 0 -> 0 :: change l false
      |(e::l,false) when e =1 -> 1 :: change l true
      |(e::l,true) when e =1 -> 0 :: change l true
      |(_::l,_) -> 1 :: change l true
   in
   match liste with
     |[] -> []
     |e::l -> if x >= 0 then
                0 :: change l false
             else
                1 :: change l false;;
     

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
let abs x =
  if x > 0 then
    x
  else
    -x;;

let rec found num puiss =
  if power 2 puiss > num then
    puiss
  else
    found num (puiss+1);;


let en_bits integer bits =
  let rec conversion bits integer acu=
    if bits = -1 then
      acu
    else
      let pow = power 2 bits in
      if pow > integer then
         conversion (bits-1) integer (0::acu)
      else
         conversion (bits-1) (integer-pow) (1::acu)
  in conversion (bits-1) integer [];;

let from_int x =
  let puiss = found (abs(x)) 0 in
  if x > 0 then
    0:: en_bits x puiss
  else
    1 :: en_bits (abs(x)) puiss;;

  (*let y = abs x in
  let max = puissance x in
  if x < 0 then*)
    
    
   
        

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)
let list_to_dec list long =
  let rec resultat liste long =
      match liste with
          [] -> 0
        |(e::l) -> e* power 2 long + resultat l (long-1)
  in resultat list long;;

let to_int bA =
  

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = ()

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB = 0

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = true

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = true

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = true

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = 0

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = true

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = true

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = true
;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = 0

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = []

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (0, 0)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = []

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
