let sign n =
  match n with
   n when n > 0 -> 1
   |_ -> -1;;

let quot a b =
    let rec sous_quot a b acu=
      if a > b then
        sous_quot (a-b) b (acu+1)
      else 
        acu
    in
      if a < b then 
        sous_quot b a 0
      else
        sous_quot a b 0;;

let modulo a b =
  let rec sous_quot a b =
    if a > b then
      sous_quot (a-b) b 
    else 
      a
  in
    if a < b then 
      sous_quot b a 
    else
      sous_quot a b ;;

let div a b = 
  let rec sous_quot a b acu =
    if a > b then
      sous_quot (a-b) b (acu+1)
    else 
      (acu,a)
  in
    if a < b then 
      sous_quot b a 0
    else
      sous_quot a b 0;;

(*CHANGEMENT DE FICHIER*)

let gcd a b =
  let rec pgcd a b n=
    if a mod n = 0 && b mod n = 0 then 
      n 
    else 
      pgcd a b (n-1)
  in
    if a > b then
      pgcd a b b
    else 
      pgcd b a a;;

(*let pgcd_quotient_etape a b = (*si a >b*)
  let quotient_b a b q =
    if a < (b*q) then
      q
    else 
      quotient_b a b (q+1)
  in quotient_b a b 1 

let pgcd_reste_etape a b = (*si a >b*)
  let reste_a_b a b r =
    if a < (b*q) then
      r
    else 
      reste_a_b a b (r+1)
  in reste_a_b a b 0 *)



let rec bezout a b =
  let r = a in
  let u = 1 in
  let v = 0 in
  let r_prime = b in
  let u_prime = 0 in
  let v_prime = 1 in
  let q = r/r_prime in
  let rec calcul r u v r_prime u_prime v_prime q =
    if r_prime != 0 then 
      calcul r_prime u_prime v_prime (r-(q*r_prime)) (u-(q*u_prime)) (v-(q*v_prime)) ( r/r_prime)
    else 
      (u,v,r)
  in 
   calcul r u v r_prime u_prime v_prime q;;
    

(*CHANGEMENT DE FICHIER : power.ml*)

let pow a b =
  let rec power a b n=
    if b != 0 then 
      power a (b-1) (n*a)
    else 
      n 
  in 
    power a b 1;;

let power a b =
  if b mod 2 = 0 then 
    pow (pow a 2) (b/2) 
  else 
    a*(pow (pow a 2) ((b-1)/2));;

let mod_power b e m =
  let rec pow c e_prime =
    if e_prime <= e then
      pow ((c*b)mod m) (e_prime+1)  
    else 
      c
  in pow 1 0;;

let prime_mod_power prime integer power =

    
let is_prime n =
  let rec second acu =
    match acu with 
     1 -> true
    | acu when (n mod acu != 0) && (acu!=1) -> second (acu-1)
    |_ -> false
  in second (n-1);;

let is_pseudo_prime n liste =
  let rec pseudo liste =
    match liste with 
     [] -> false
    | e::l when (((power e (n-1))-1) mod n =0) && (is_prime(n)=false) -> true
    | e::l -> pseudo l
  in 
  if n <=1 then 
    false 
  else 
    pseudo liste;;

let rec help_era n max =
  if n>= max then 
   help_era 3 max
  else
     if is_prime(n) = false then
       help_era (n+1) max
     else 
        n ;;


let init_eratosthenes n =
  let rec generate e = 
    match e with 
    | e when e = n+1 -> []
    | e when e=2  -> 2:: generate (e+1)
    | e when (e mod 2 != 0) -> e :: generate (e+1)
    |_ -> generate (e+1)
  in generate 2;;

let suppression_multiples n liste =
  let rec suppr count liste  =
    match (liste,count) with 
    | ([],_) -> []
    | (e::l,_) when (e mod n = 0) && (e!= n) -> suppr (count+1) l
    | (e::l,_) -> e :: suppr (count+1) l
  in suppr 3 liste;;

let eratosthenes n =
  let liste = init_eratosthenes n in
  let x = int_of_float(sqrt(float_of_int(n))) + 1 in
  let rec traitement e liste=
    if e <= x then 
      traitement (e+1) (suppression_multiples e liste) 
    else 
      liste 
  in traitement 3 liste;;
       
(*let rec write_list_primes*)
(*let rec read_list_primes*)


let rec appartenance x liste = 
  match liste with 
  | [] -> false
  | e :: l when e = x -> true 
  |_ -> appartenance x l;;


let double_primes n f =
  let primes = erathosthene n in
  let test primes =
    match primes with 
    | [] -> []
    | e :: l when f ((e*2)+1) = true -> (e,(e*2+1)):: test l
    |_ -> test l
  in test primes;;

let twin_prime n f = (*double et twin a refaire pas de test de pseudo primalite*)
  let primes = erathosthene n in
  let test primes =
    match primes with 
    | [] -> []
    | e :: l when f (e+2) = true -> (e,(e+2)):: test l
    |_ -> test l
  in test primes;;

(*CHIFFRES.ml*)

let encrypt key int_letter nb_letter= 
  let rec modulo terminus =
    if terminus > nb_letter then
       modulo (terminus-nb_letter)
    else
        terminus
  in
  let terminus = int_letter + key in
  modulo terminus;;
   


let rec encrypt_cesar key liste nb_letter =
  match liste with 
  | [] -> []
  | e::l -> (encrypt key e nb_letter) :: encrypt_cesar key l nb_letter;;



  let decrypt key int_letter nb_letter= 
    let rec modulo terminus =
      if terminus < 0 then
         modulo (nb_letter+terminus)
      else
          terminus
    in
    let terminus = int_letter - key in
    modulo terminus;;
    
    let rec decrypt_cesar key liste nb_letter =
      match liste with 
      | [] -> [] 
      | e::l -> (decrypt key e nb_letter) :: (decrypt_cesar key l nb_letter);;

      
      let gcd a b = (*utile pour generate_prime_with*)
        let rec pgcd a b n=
          if a mod n = 0 && b mod n = 0 then 
            n 
          else 
            pgcd a b (n-1)
        in
          if a > b then
            pgcd a b b
          else 
            pgcd b a a;;
      

      let rec generate_prime_with phi_de_n= (*utile pour RSA_key*)
        let ran = Random.int(phi_de_n-2)+2 in 
        if gcd phi_de_n ran  = 1 then 
          ran
        else 
          generate_prime_with phi_de_n  ;;


    let inv_modulaire e modu = (*utile pour generate_keys_rsa*)
      let rec search d = 
      if e*d = modu +1 then 
        d
      else 
        search (d+1)
      in search 1;;
    

      let generate_keys_rsa p q = (*Incroyable, quand j'echange la postion des deux dernieres sortie ca marche plus mdr*)
        let n = p*q in
        let phi_de_n = (p-1)*(q-1) in
        let e = generate_prime_with phi_de_n in
        let d = inv_modulaire e phi_de_n in 
        ((n,e),(d,n));;
        


        let pow a b = (*utile pour power*)
          let rec power a b n=
            if b != 0 then 
              power a (b-1) (n*a)
            else 
              n 
          in 
            power a b 1;;
        
        let power a b = (*utile pour encrypt_rsa*)
          if b mod 2 = 0 then 
            pow (pow a 2) (b/2) 
          else 
            a*(pow (pow a 2) ((b-1)/2));;

    let encrypt_rsa entier (n,e) =
      let rec crypypte c=
        if power entier e = n+c then
          c
        else 
          crypypte (c+1)  
      in crypypte 0;;

      

    let decrypt_rsa entier (n,d) =
      let rec crypypte c=
        if power entier d > n*c then
          crypypte (c+1)
        else 
          (power entier d) - (n*(c-1))
      in crypypte 0
      ;;



      (*if prime1 = prime2 then 
      invalid_arg"Your primes should be different"*)
