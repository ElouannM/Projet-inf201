type candidat = string;;
type buletin = candidat;;
type urne = candidat list;;
type scorec = int;;
type panel = candidat list;;
type resultat = (candidat*scorec) list;;
let lc1:panel = ["Eric";"Kyle";"Stan"];;
let u1:urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";
"Eric";"Eric";"Stan";"Stan"];;

let rec compte(c:candidat)(u:urne):scorec =
    match u with
    |[]->0
    |h::t when h = c -> 1+compte c t
    |h::t -> compte c t
;;
assert(compte "Eric" u1= 7);;

let rec depouiller(p:panel)(u:urne):resultat=
    match p with
    |[]-> []
    |h::t->(h,compte h u) ::depouiller t u
;;
assert(depouiller lc1 u1=[("Eric", 7); ("Kyle", 4); ("Stan", 5)]);;
let r = depouiller lc1 u1;;
let r1: resultat = [("Jean",5);("Bob",2);("Nico",9)];;
let rec union (r1:resultat)(r2:resultat):resultat=
  match r1 with
  |[]->r2
  | x::r'1 -> x::(union r'1 r2)
;;
union r r1;;

let rec max_depouiller(r:resultat):resultat=
    match r with
    |[] ->[]
    |(a,b)::r'->(match max_depouiller r' with
    |[]-> [(a,b)]
    |[(x,y)]->if b>y then [(a,b)] else [(x,y)])
;;
max_depouiller r1;;

let vainqueur_scrutin_uninominal(u:urne)(p:panel):candidat=
    let r = depouiller p u in 
    let (a,b)::r = max_depouiller r in a
;;
vainqueur_scrutin_uninominal u1 lc1;;

let rec suppr_e(lst:'a list)(e:'a list):'a list =
  match lst with
  |[] -> []
  |e::t -> t
  |h::t -> suppr_e t e
;;
suppr_e r1 [("Bob",2)];;

let rec deux_prem(u:urne)(p:panel):resultat=
    let r = depouiller p u in 
    let e = max_depouiller r in
    let r' = suppr_e r e in
    let e' =max_depouiller r' in
    union e e'
  ;;
  deux_prem u1 lc1;;


  (*#2*)
  (*6*12 = 72 possibilités; commentaire: c'est + que 13*)

type mention = |Arejeter |Insuffisant |Passable |Assezbien |Bien |Tresbien ;;
type bulletin_jm = mention list ;;
type urne_jm = bulletin_jm list ;;

let b :bulletin_jm = [Tresbien; Assezbien; Arejeter; Passable]

let u : urne_jm =
  [[Tresbien; Assezbien; Arejeter; Passable]; (* Premier bulletin *)
   [Assezbien; Assezbien; Arejeter; Tresbien]; (* Second bulletin *)
   [Tresbien; Arejeter; Arejeter; Tresbien]] 
;;

let rec  depouille_jm(b: urne_jm)=
  match b with
  |[] -> []
  |[]::_ -> []
  |_ -> let p = List.map List.hd b in let d = List.map List.tl b in p:: depouille_jm d

;;
let x = depouille_jm u;;

let tri_mentions(u : urne_jm)=
  List.map (fun l -> List.sort compare l) u;;
tri_mentions x;;

let mediane (l:'a list)=
  let n = List.length l in
  List.nth l (n/2)
;;

mediane [Passable;Bien;Tresbien]
;;

(*let meilleur_mediane(u: urne_jm):mention =*)
