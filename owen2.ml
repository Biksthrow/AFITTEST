type arete = {a : int ; b : int};;
type graphe = { n : int; v : arete list};;

let rec insere l s  = 
  match l with 
  | [] -> l
  |t::q when t < s -> t :: (insere q s)
  |t::q when t = s -> t::q
  |t::q -> s::t::q ;;

let voisins g s =
  let aretes = g.v in
  let rec test aretes liste=
    match aretes with
    | [] -> liste
    |t::q when t.a = s -> test q (insere liste t.a) 
    |t::q when t.b = s -> test q (insere liste t.b)
    |t::q -> test q liste  
  in test aretes [];;






  for i = 0 to  g.n do 
    let couple 