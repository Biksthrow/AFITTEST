(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
*)
let encrypt key int_letter nb_letter= 
  let rec modulo terminus =
    if terminus > nb_letter then
       modulo (terminus-nb_letter)
    else
        terminus
  in
  let terminus = int_letter + key in
  modulo terminus;;

let rec encrypt_cesar k m b =
  match m with 
  | [] -> []
  | e::l -> (encrypt k e b) :: encrypt_cesar k l b;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
*)

let decrypt key int_letter nb_letter= 
    let rec modulo terminus =
      if terminus < 0 then
         modulo (nb_letter+terminus)
      else
          terminus
    in
    let terminus = int_letter - key in
    modulo terminus;;

let rec decrypt_cesar k m b =
  match m with 
      | [] -> [] 
      | e::l -> (decrypt k e b) :: (decrypt_cesar k l b);;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
     



    let generate_keys_rsa p q =
      let n = p*q in
        let phi_de_n = (p-1)*(q-1) in
        let e = 65537  in
        let (d,l,m) = bezout e phi_de_n in 
        ((n,e),(n,d));;


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
*)

    
let encrypt_rsa m (n, e) = mod_power m e n ;;
(** Decryption using RSA cryptosystem.

    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;
  


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let g = Random.int p in
  (g,p)

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a= Random.int (p-1) +1 in (mod_power g a p,a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = Random.int(p-1) +1 in
  (mod_power g k p,msg*mod_power kA k p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.<
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let z = mod_power msgA a p in
  let (d,l,m) = bezout z p in 
  modulo (d*msgB) p;;
