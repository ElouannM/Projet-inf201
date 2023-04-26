(*
-----------------------------------------------------------------------
   inf201_Dubois_Marfil_Ninive_Quesnel_projet.ml : Projet Ocaml Inf201
   Enzo DUBOIS <enzo.dubois@etu.univ-grenoble-alpes.fr>
   Elouann MARFIL <elouann.marfil@etu.univ-grenoble-alpes.fr>
   Bastien NINIVE <bastien.ninive@etu.univ-grenoble-alpes.fr>
   Maxime QUESNEL <maxime.quesnel@etu.univ-grenoble-alpes.fr>
-----------------------------------------------------------------------
*)

(* Partie 2 : Scrutin Uninominal *)
(*Question 1*)
type candidat = string;;
type bulletin = candidat;;
type urne = bulletin list;;
type panel = candidat list;;
type score = int;;
type resultat = (candidat * score) list;;

(* Urnes de test *)
let lc1 : panel = ["Eric";"Kyle";"Stan"];;
let u1 : urne = ["Eric";"Kyle";"Stan";"Kyle";"Kyle";"Stan";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;

(*Question 2*)
let rec compte (c:candidat) (u:urne) : score =
    match u with
    |[] -> 0
    | x::xs if x = c -> 1 + compte c xs else compte c xs;;

let _ = assert((compte "Eric" u1) = 7);;   

(*Question 3*)

let rec depouiller (p:panel) (u:urne) : resultat=
    match p with
    | []-> []
    | h::t -> (h , compte h u) :: depouiller t u;;
   
let _ = assert(depouiller lc1 u1=[("Eric", 7); ("Kyle", 4); ("Stan", 5)]);;

(*Question 4*)
let r = depouiller lc1 u1;;
let r1: resultat = [("Jean",5);("Bob",2);("Nico",9)];;
let r2: resultat = [("Jean",5);("Bob",2);("Nico",9); ("Gigi",9)];;

let rec union (r1: resultat) (r2: resultat) : resultat =
  let rec ajouterVoix (c : candidat) (nbVoix : int) (r : resultat) : resultat =
    match r with
    |[]->[(c, nbVoix)]
    |(a, b)::xs->if a = c then (a, b+nbVoix)::xs else (a, b)::(ajouterVoix c nbVoix xs)
  in 
  match r1 with 
  |[]->r2
  |(a, b)::xs->(union xs (ajouterVoix a b r2));;

let _ = assert(union r r1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Jean", 5); ("Bob", 2); ("Nico", 9)]);;                                                 (*- : resultat = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Jean", 5); ("Bob", 2); ("Nico", 9)]*)
let _ = assert(union [("Eric", 7); ("Kyle", 4); ("Stan", 5)] [("Eric", 5); ("Kyle", 2); ("Stan", 2)] = [("Eric", 12); ("Kyle", 6); ("Stan", 7)]);;
(*Question 5*)
let max_depouille (l : resultat) : candidat*score =      (*renvoie ("", 0) si l est vide*)
  let rec aux (l:resultat) ((meilleur_c, meilleur_s): candidat*score) : candidat*score =
    match l with
    |[]->(meilleur_c, meilleur_s)
    |(a, b)::xs -> aux xs (if b>meilleur_s then (a, b) else (meilleur_c, meilleur_s)) (* Compare le score actuel avec le meilleur sccore enregistré*)
  in aux l ("", 0);;

let _ = assert(max_depouille r1 = [("Nico", 9)]);;

(*Question 6*)
let vainqueur_scrutin_uninominal (u:urne) (lc:panel) : candidat =
  let (a, b) = max_depouille(depouiller u lc) in a;;

let _ = assert(vainqueur_scrutin_uninominal u1 lc1 = "Eric");;

(*Question 7*)
let rec suppr_elem (l : 'a list) (e : 'a) : 'a list =
  match l with
  | [] -> []
  | x::xs ->if x = e then xs else x::(suppr_elem xs elem);;


let _ = assert(suppr_elem r1 ("Bob",2) = [("Jean", 5); ("Nico", 9)]);;

let deux_premiers (u:urne) (lc: panel) : (candidat*score)*(candidat*score) = 
  let res = depouiller u lc in 
  let c1 = max_depouille res in 
  let c2 = max_depouille (suppr_elem res c1) in 
  (c1, c2);;

let _ = assert(deux_premiers u1 lc1 = ([("Eric", 7)], [("Stan", 5)]));;

(* Question 8 *)
(*A FAIRE*)

(* Question 9 *)
(*A FAIRE*)

(* Partie 3 : Jugement majoritaire *)
(*Question 10*)
(*
on a 6^12 + 1 = 2 176 782 337 possibilité, 
ce qui est énormément plus que 13 pour le scrutin uninomimal.
Ainsi les votants peuvent exprimer très précisément leurs ressentis 
sur les candidats ce qui limite le paradoxe vu précédemment 
*)

(*Question 11*)
type mention = Arejeter | Insuffisant | Passable | Assezbien | Bien | Tresbien ;;
type bulletin_jm = mention list ;;
type urne_jm = bulletin_jm list ;;

let bulletin : bulletin_jm = [Tresbien; Assezbien; Arejeter; Passable]

let urne : urne_jm =
  [[Tresbien; Assezbien; Arejeter; Passable]; (* Premier bulletin *) 
   [Assezbien; Assezbien; Arejeter; Tresbien]; (* Second bulletin *) 
   [Tresbien; Arejeter; Arejeter; Tresbien]];; (* Troisieme bulletin *)

let urne_triee : urne_jm =
[[Tresbien; Assezbien; Tresbien]; (* Mentions du premier candidat *) 
 [Assezbien; Assezbien; Arejeter]; (* Mentions du second candidat *) 
 [Arejeter; Arejeter; Arejeter]; (* Mentions du troisiemecandidat *) 
 [Passable; Tresbien; Tresbien]] (* Mentions du quatrieme candidat *)

(*Question 12*)
let rec depouiller_jm (u : urne_jm) : mention list list =
  match u with
  |[]->[]
  |x::xs->let (premiers, suite) = (List.fold_left 
   (fun ((acc, newtail):(mention list)*(mention list list)) (x: mention list) -> match x with |[]->([], newtail) |y::ys->(y::acc, ys::newtail)) ([], []) u) 
  in premiers::(depouiller_jm suite);;
  
let _ = assert(depouiller_jm urne = urne_triee);;

(*Question 13*)
let urne_ordre : urne_jm =
[[Assezbien; Tresbien; Tresbien];
 [Arejeter; Assezbien; Assezbien];
 [Arejeter; Arejeter; Arejeter];
 [Passable; Tresbien; Tresbien]];;
 
let tri (l:'a list):'a list = List.sort compare l;;

let rec tris_mentions (u: urne_jm) : mention list list =
  suppr_elem (List.map tri u) [];; (*obligé de supprimer un [] qui apparait systématiquement*)

let _ = assert(tri_mentions urne_triee = urne_ordre);;

(*Question 14 *)
let mediane (l:'a list) : 'a =
  let n = List.length l in
  List.nth l (n/2);; (*pas de problèmes pour les listes vides les fonctions qui utilisent medianes ne l'appliquent jamais sur les listes vides.*)

let _ = assert((mediane [Tresbien; Bien; Assezbien; Arejeter; Passable]) = Assezbien);;

(*Question 15*)                            
let meilleure_mediane (u:urne_jm) : mention =
  List.fold_left (fun (acc : mention) (x: bulletin_jm) : mention ->if x != [] then (let m = (mediane x) in if acc<m then m else acc) else acc) Arejeter u;; (*tests pour les listes vides important !!*)

let _ = assert(meilleure_mediane urne_ordre = Tresbien);;

(*Question 16*)
let supprime_perdants (u: urne_jm) : urne_jm = 
  let m = meilleure_mediane u in
  List.map (fun (x: bulletin_jm) : bulletin_jm -> if x != [] then (let m' = mediane x in if m'<m then [] else x) else []) u;; (* pareil test pour le cas des listes vides important *) 

let _ = assert (supprime_perdants urne_ordre = [[Assezbien; Tresbien; Tresbien]; []; []; [Passable; Tresbien; Tresbien]]);;
let urne_supprime = supprime_perdants urne_ordre;;

(*Question 17*)
let rec supprime_mention (u : mention list) (x : mention) : mention list =
  match u with
  | [] -> [] 
  | h::t -> if h = x then t (* Si la mention est égale, on renvoie la fin *) else 
               h::(supprime_mention t x);; (*Sinon, on parcourt la suite*)

let supprime_meilleure_mediane (u:urne_jm) : urne_jm = 
  List.map (fun (x:bulletin_jm) : bulletin_jm ->let m = meilleure_mediane u in supprime_mention x m) u;;
  
let _ = assert (supprime_meilleure_mediane urne_supprime =  [[Assezbien; Tresbien]; []; []; [Passable; Tresbien]])

(*Question 18*)
let vainqueur_jm (u:urne_jm) (lc: candidat list) : candidat= 
  let (m, c, n, i) = List.fold_left (fun ((m, comptePotentielsGagnants, numeroCandidatVainqueur, indiceExplo) : mention*int*int*int) (x:bulletin_jm) -> (* Comptage des scores *) 
    match x with 
    |[]->(m, comptePotentielsGagnants, numeroCandidatVainqueur, (indiceExplo+1)) (* Cas du bulletin vide *)
    |_->let mx = mediane x in (* Calcul de la médiane du bulletin, le bulletijn est non vide *)
                  if mx = m then (m, (comptePotentielsGagnants+1), indiceExplo, (indiceExplo+1)) else  (* Le candidat est un potentiel gagnant parce que sa médianne est égale à la médianne de l'urne *)
                  if mx > m then (mx, 1, indiceExplo, (indiceExplo+1))  (*Le candidat est gagnant parce que sa médianne est supérieure *)
                  else (m, comptePotentielsGagnants, numeroCandidatVainqueur, (indiceExplo+1))) (* Sa médiane est inférieure, on continue d'avancer *)
                  (Arejeter, 0, -1, 0) u (*Seed du fold_left et application sur l'urne *)
    in if c = 1 (* Si c=1 on a un gagnant bien déterminé, impossible sinon (cas d'égalité, 0 gagnants, etc...)  *)
      then (List.nth lc n) else "";;

(*Question 19*)
(*A FAIRE Implémenter les grosses listes*)
let trouve_vainqueurs_jm (u: urne_jm) (lc: candidat list) : candidat =
  let rec aux (u:urne_jm) (lc: candidat list) : candidat = (*fonction auxiliaire pour ne pas dépouiller et trier encore et encore*)
    let res = (vainqueur_jm u lc) in (*On récupère le vainqueur *)
    if res = "" then (*Problème : il faut retirer des médianes*)
      aux (supprime_perdants (supprime_meilleure_mediane u)) lc
    else
      res in (*Problème résolu *)
  let urne_traitee = supprime_perdants(tris_mentions (depouiller_jm u)) (*On traite l'urne en la triant depouillant et en supprimant les perdants*)
    in aux urne_traitee lc;; (*On rapplique la fonction auxiliaire*)

(*Question 20*)
(*A FAIRE*)
  
(* Partie 4 : Recomptons les voix*)
(*Question 21*)
type ville = string;;
type zone = Reg of string | Dpt of string;;
type arbre = N of zone*(arbre list) | Bv of ville*resultat;;

(*Question 22*)
let trouve_bv (a:arbre) (v: ville) : resultat =
  let rec aux (a:arbre) (v: ville) : resultat option =
    match a with 
    |N(_, l)->List.fold_left (fun (acc: resultat option) (x:arbre) -> match acc with |Some(res) ->Some(res)|None->(aux x v)) None l
    |Bv(vi, res) when vi = v->Some(res)
    |_->None
  in 
  let res = aux a v in match res with
                       |Some(r)-> r
                       |None->failwith "L'élément n'existe pas dans l'arbre";;

(*Question 23*)
(* A FAIRE Rajouter le gros arbre pour tester*)
let res2022GrenobleFontaineValence = max_depouille(union (union (trouve_bv ara "Valence") (trouve_bv ara "Fontaine")) (trouve_bv ara "Grenoble"));;

(*Partie 5 : Conclusion*)
(* A FAIRE*)



