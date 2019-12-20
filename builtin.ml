(** Tweaking OCaml built-in euclidean division
The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)

(* Integer quotient implementation ; main use is in case of quotient
   of an integer by a natural number.
 *)

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
*)

let sign x =
  if x >= 0 then
    1
  else
    -1;;

let abs n =
  if n > 0 then
    n
  else
    -n;;


let quot a b =
  let x = a in
  let y = b in
  let rec sous_quot1 a b acu=
      if (a>=0) && (a< (abs b)) then
        (sign x) * (sign y) * acu
      else 
        sous_quot1 (a-b) b (acu+1)
  in
  let rec sous_quot2 a b acu=
      if (a>=0) && (a<(abs b)) then
        (sign x) * (sign y) * acu
      else 
        sous_quot2 (a+b) b (acu+1)
    in
      if (a >0 && b >0) || (a<0 && b <0) then 
        sous_quot1 x y 0
      else
         sous_quot2 x y 0;;

(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Modulo of two integers.
    Following Euclidean division. NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    @param a input integer
    @param b moduli a natural number.
*)
let modulo a b =
  let rec sous_quot2 a b =
      if (a>=0) && (a<(abs b)) then
        a
      else 
        sous_quot2 (a+b) b
  in
  if (a >0 && b >0) || (a<0 && b <0) then
    abs(a) mod abs(b)
  else
    sous_quot2 a b ;;



(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b =
  let x = a in
  let y = b in
  let rec sous_quot1 a b acu=
      if (a>=0) && (a< (abs b)) then
        (acu*(sign x)*(sign y),a)
      else 
        sous_quot1 (a-b) b (acu+1)
  in
  let rec sous_quot2 a b acu=
      if (a>=0) && (a<(abs b)) then
        (acu*(sign x)*(sign y),a)
      else 
        sous_quot2 (a+b) b (acu+1)
    in
      if (a >0 && b >0) || (a<0 && b <0) then 
        sous_quot1 x y 0
      else
         sous_quot2 x y 0;;


