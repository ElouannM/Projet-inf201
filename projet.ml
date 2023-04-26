(*
-----------------------------------------------------------------------
   inf201_Dubois_Marfil_Ninive_Quesnel_projet.ml : Projet Ocaml Inf201
   Enzo DUBOIS <enzo.dubois@etu.univ-grenoble-alpes.fr>
   Elouann MARFIL <elouann.marfil@etu.univ-grenoble-alpes.fr>
   Bastien NINIVE <bastien.ninive@etu.univ-grenoble-alpes.fr>
   Maxime QUESNEL <maxime.quesnel@etu.univ-grenoble-alpes.fr>
-----------------------------------------------------------------------
*)

(*Code                                                                                                        Sorties*)
(* Partie 2 : Scrutin Uninominal *)
(*Question 1*)
type candidat = string;;
type buletin = candidat;;
type urne = candidat list;;
type scorec = int;;
type panel = candidat list;;

(* Urnes de test *)
let lc1 : panel = ["Eric";"Kyle";"Stan"];;
let u1 : urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;

(*Question 2*)
let rec compte (c:candidat) (u:urne) : scorec =
    match u with
    |[] -> 0
    | h::t when h = c -> 1+compte c t
    | h::t -> compte c t;;

assert(compte "Eric" u1= 7);;        
(*Question 3*)
type resultat = (candidat * scorec) list;;

let rec depouiller (p:panel) (u:urne) : resultat=
    match p with
    | []-> []
    | h::t -> (h , compte h u) :: depouiller t u;;
   
assert(depouiller lc1 u1=[("Eric", 7); ("Kyle", 4); ("Stan", 5)]);;

(*Question 4*)
let r = depouiller lc1 u1;;
let r1: resultat = [("Jean",5);("Bob",2);("Nico",9)];;
let r2: resultat = [("Jean",5);("Bob",2);("Nico",9); ("Gigi",9)];;

let rec union (r1:resultat)(r2:resultat) : resultat =
  match r1 with
  | [] -> r2
  | x::r'1 -> x::(union r'1 r2);;

assert(union r r1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Jean", 5); ("Bob", 2); ("Nico", 9)]);;                                                 (*- : resultat = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Jean", 5); ("Bob", 2); ("Nico", 9)]*)

(*Question 5*)
let rec max_depouille (r:resultat) : resultat=
    match r with
    |[] ->[]
    |(a,b):: r' -> (
      match max_depouille r' with
      |[]-> [(a,b)]
      |[(x,y)]->if b>y then [(a,b)] else [(x,y)]);; (* Pattern pas exaustif (et fonction pas claire) *)
   
assert(max_depouille r1 = [("Nico", 9)]);;

(*Question 6*)
let vainqueur_scrutin_uninominal (u:urne) (p:panel) : candidat =
    let r = depouiller p u in (* On dépouille l'urne pour récupérer les voix*)
      let (a,b)::r = max_depouiller r in (* On récupère le max *)
        a;; (*On renvoie le nom *)

assert(vainqueur_scrutin_uninominal u1 lc1 = "Eric");;

(*Question 7*)
let rec suppr_elem (l : 'a list) (e : 'a) : 'a list =
  match l with
  | [] -> []
  | x::t when x = e -> t
  | x::t -> x::(suppr_elem t e);;

assert(suppr_elem r1 ("Bob",2) = [("Jean", 5); ("Nico", 9)]);;

let rec deux_premiers (u:urne) (p:panel) : resultat*resultat =
    let res = depouiller p u in (* On récupère les résultats dépoulliage *) 
      let premier = max_depouiller res in (* On enregistre le premier en voix *)
        let res' = suppr_e res premier in (* On récupère la liste sans le premier *)
          let deuxieme = max_depouiller res' in (* On récupère le second en voix *)
            premier, deuxieme (* On renvoie les 2 resultats *) ;;

assert(deux_premiers u1 lc1 = ([("Eric", 7)], [("Stan", 5)]));;

(* Question 8 *)
A FAIRE

(* Question 9 *)
A FAIRE

(* Partie 3 : Jugement majoritaire*)
(*#2*)(*6*12 = 72 possibilités; commentaire: c'est + que 13*)

type mention = |Arejeter | Insuffisant | Passable | Assezbien | Bien | Tresbien ;;
type bulletin_jm = mention list ;;
type urne_jm = bulletin_jm list ;;

let b : bulletin_jm = [Tresbien; Assezbien; Arejeter; Passable]

let u : urne_jm =
  [[Tresbien; Assezbien; Assezbien; Passable]; (* Premier bulletin *)
   [Assezbien; Assezbien; Tresbien; Tresbien]; (* Second bulletin *)
   [Tresbien; Arejeter; Arejeter; Tresbien]] 
;;

let ms_triee : urne_jm =
[[Assezbien; Tresbien; Tresbien];
 [Arejeter; Assezbien; Assezbien];
 [Arejeter; Arejeter; Arejeter];
 [Passable; Tresbien; Tresbien]]


let rec depouille_jm( b : urne_jm)=
  match b with
  |[] -> []
  |[]::_ -> [] (* Liste actuelle vide : problème dans le bulletin *)
  |_ -> let premiers = List.map List.hd b in (*On récupère les premiers éléments*) 
          let derniers = List.map List.tl b in (*On récupère la suite*)
            premiers :: depouille_jm derniers;; (*On les ajoute*)
  
let x = depouille_jm u;;

let tri_mentions(u : urne_jm)=
  List.map (fun l -> List.sort compare l) u;; (* Applique la fonction sort à chaque liste de mentions *)

tri_mentions x;;

let mediane (l:'a list)=
  match l with
  | [] -> Arejeter (*La liste est vide : on renvoie la pire mention pour ne pas perturber le calcul de la meilleure mediane*)
  | _ -> let n = List.length l in List.nth l (n/2);; (* Renvoie le taille/2ième élément de la liste*)

mediane [Passable;Bien;Tresbien];;              (*- : mention = Bien*)
mediane u;;                                     
mediane [];;                                    (*Exception: Failure "nth".*)

let meilleure_mediane(u: urne_jm) : mention =
  List.fold_left (fun (acc : mention) (x:bulletin_jm) -> let m = (mediane x) in if acc < m then m else acc) Arejeter u;; 

meilleure_mediane u;;

let rec supprime_mention (u : urne_jm) (x:mention)=
  match u with
  |[] -> [] 
  |t::h when t = x -> h (* Si la mention est égale, on la supprime*)
  |t::h -> t::(supprime_mention h x);; (*Sinon, on parcourt la suite*)

let supprime_perdants (u : urne_jm) : urne_jm =
  let m = meilleure_mediane u in
  List.map (fun x : bulletin_jm -> 
    if mediane x < m (*Le bulletin actuel est inférieur à la médianne*)
      then [] (*On le supprime *) else x (*On le garde*)) u;;

supprime_perdants u;;
supprime_perdants ms_triee;;

let supprime_meilleur_mediane (u : urne_jm) =
  let meilleur = meilleure_mediane u in 
  List.map (fun (l:bulletin_jm) -> supprime_mention l m) u;;

let rec vainqueur_jm (m : mention list list) : candidat =
  match m with 
  | [] -> ""
  | 

supprime_meilleur_mediane ms_triee;;



