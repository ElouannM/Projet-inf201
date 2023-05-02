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
    | x::xs -> if x = c then 1 + (compte c xs) else compte c xs;;

let _ = assert((compte "Eric" u1) = 7);;   

(*Question 3*)
let rec depouiller (p:panel) (u:urne) : resultat =
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
    (* Fonction intermédiaire qui ajoute le nombre de voix d'un candidat à un résultat *)
    match r with
    |[]->[(c, nbVoix)] 
    |(a, b)::xs -> if a = c then (a, b+nbVoix)::xs (* Si le candidat est déjà dans le résultat, on ajoute le nombre de voix *)
      else (a, b)::(ajouterVoix c nbVoix xs) (* Sinon on ajoute la valeur continue de parcourir le résultat *)
  in match r2 with 
  |[]->r1
  |(a, b)::xs->(union xs (ajouterVoix a b r1));; (* On ajoute le candidat et son nombre de voix au résultat *)

let _ = assert(union r r1 = [("Eric", 7); ("Kyle", 4); ("Stan", 5); ("Jean", 5); ("Bob", 2); ("Nico", 9)]);;
let _ = assert(union r1 r2 = [("Jean", 5); ("Bob", 2); ("Nico", 9); ("Gigi", 9)]);;
(*Question 5*)
let max_depouille (l : resultat) : candidat*score =      (*renvoie ("", 0) si l est vide*)
  let rec aux (l:resultat) ((meilleur_c, meilleur_s): candidat*score) : candidat*score =
    match l with
    |[]->(meilleur_c, meilleur_s)
    |(a, b)::xs -> aux xs (if b>meilleur_s then (a, b) else (meilleur_c, meilleur_s)) (* Compare le score actuel avec le meilleur sccore enregistré*)
  in aux l ("", 0);;

let _ = assert(max_depouille r1 = ("Nico", 9));;

(*Question 6*)
let vainqueur_scrutin_uninominal (u:urne) (lc:panel) : candidat =
  let (a, b) = max_depouille(depouiller u lc) in a;; (* On récupère le candidat ayant le plus de voix dans le résultat du dépouillement *)

let _ = assert(vainqueur_scrutin_uninominal u1 lc1 = "Eric");;

(*Question 7*)
let rec suppr_elem (l : 'a list) (e : 'a) : 'a list =
  match l with
  | [] -> []
  | x::xs ->if x = e then xs else x::(suppr_elem xs e);;

let _ = assert(suppr_elem r1 ("Bob",2) = [("Jean", 5); ("Nico", 9)]);;

let deux_premiers (u:urne) (lc: panel) : (candidat*score)*(candidat*score) = 
  let res = depouiller u lc in 
  let c1 = max_depouille res in 
  let c2 = max_depouille (suppr_elem res c1) in 
  (c1, c2);;

let _ = assert(deux_premiers u1 lc1 = (("Eric", 7), ("Stan", 5)));;

(* Question 8 *)
let score1 = deux_premiers u1 lc1;;

let lc2 = ["Eric";"Kyle";"Stan";"Keny"];;
let u2 = ["Keny";"Kyle";"Keny";"Kyle";"Kyle";"Keny";"Eric";"Eric";"Kyle";"Eric";"Stan";"Eric";"Eric";"Eric";"Stan";"Stan"];;
let score2 = deux_premiers u2 lc2;;

(*
Le problème d'un scrutin à deux tours est donc que l'ajout d'un candidat peut faire perdre un candidat qui avait plus de voix que lui
car les voix sont plus réparties parmi les candidats. Si on avait 100 candidats et 100 votants, le candidat
qui a 50 voix peut perdre contre un candidat qui a 51 voix car il y a 49 candidats qui ont 1 voix, et ces voix auraient pu 
etre mises ailleurs s'il n'y avait eu que les 2 plus gros candidats lors de l'élection.
  
Par exemple, lors de l'élection présidentielle de 2022, si Fabien Roussel et Yannick Jadot ne s'étaient pas présentés,
Jean Luc Mélenchon aurait pu etre élu au second tour, car son programme aurait pu correspondre aux valeurs de gauche de ces deux candidats.
De meme dans l'introduction du sujet, c'est souvent la présentation de trop de candidats partageant des valeurs communnes qui fait perdre
les candidats adhérents à ces valeurs, et ce peu importe le bord politique.*)

(* Question 9 
Le problème du scrutin uninomial est donc qu'il manque énormément de nuances et de précision.
En effet, certaines personnes peuvent avoir des avis très différents sur les candidats, mais ne pas pouvoir bien l'exprimer, se contentant
de choisir celui qu'ils préferent parmis toute la liste, sans prendre en compte le fait qu'ils pourraient préférer certains candidats
à d'autres, mais moins que le candidat qu'ils ont choisi. 
On risque également d'avoir une polarisation des votes, ce qui veut dire que des gens préfereront le vote "utile", celui qui a le plus de chance
de remporter l'élection, plutot que de voter pour le candidat qu'ils préferent, mais qui a moins de chance de gagner.
En bref, le scrutin uninomial ne permet pas de bien exprimer ses préférences, et peut donc mener à des résultats qui ne correspondent pas
aux vraies préférences des votants.
*)


(* Partie 3 : Jugement majoritaire *)
(*Question 10
On a 6^12 + 1 = 2 176 782 337 possibilité, ce qui est énormément plus que 13 pour le scrutin uninomimal.
Ainsi les votants peuvent exprimer très précisément leurs ressentis sur les candidats ce qui limite le paradoxe vu précédemment 
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

let rec tri_mentions (u: urne_jm) : mention list list =
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
(*Pannels*)

(*4 candidats*)
let lc2:panel = ["Eric";"Kyle";"Stan";"Keny"];;

(*6 candidats*)
let lc3:panel = ["Eric";"Kyle";"Stan";"Keny";"Butters";"Wendy"];;

(* Urne jugement majoritaire *)
let (b1:bulletin_jm) = [Tresbien;Assezbien;Arejeter;Passable];;
let (b2:bulletin_jm) = [Assezbien;Assezbien;Arejeter;Tresbien];;
let (b3:bulletin_jm) = [Tresbien;Arejeter;Arejeter;Tresbien];;

let (ujm1:urne_jm) = [b1;b2;b3];;
let (ujm2:urne_jm) =
  [[Bien; Assezbien; Insuffisant; Bien]; [Bien; Bien; Passable; Bien];
   [Assezbien; Assezbien; Insuffisant; Tresbien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Assezbien; Arejeter; Assezbien; Bien];
   [Arejeter; Assezbien; Tresbien; Tresbien];
   [Assezbien; Arejeter; Insuffisant; Bien];
   [Passable; Passable; Insuffisant; Bien];
   [Insuffisant; Bien; Tresbien; Bien];
   [Arejeter; Assezbien; Passable; Tresbien];
   [Bien; Assezbien; Assezbien; Bien]; [Tresbien; Tresbien; Tresbien; Bien];
   [Passable; Tresbien; Insuffisant; Tresbien];
   [Insuffisant; Bien; Assezbien; Bien];
   [Arejeter; Passable; Insuffisant; Tresbien];
   [Arejeter; Tresbien; Assezbien; Tresbien];
   [Insuffisant; Passable; Bien; Tresbien];
   [Assezbien; Assezbien; Passable; Tresbien];
   [Passable; Insuffisant; Bien; Bien];
   [Assezbien; Insuffisant; Passable; Tresbien];
   [Tresbien; Arejeter; Insuffisant; Tresbien];
   [Passable; Arejeter; Bien; Tresbien];
   [Assezbien; Bien; Passable; Tresbien];
   [Tresbien; Tresbien; Assezbien; Tresbien];
   [Tresbien; Arejeter; Assezbien; Tresbien]; [Bien; Bien; Passable; Bien];
   [Tresbien; Arejeter; Passable; Bien];
   [Assezbien; Bien; Passable; Tresbien]; [Tresbien; Bien; Tresbien; Bien];
   [Arejeter; Arejeter; Tresbien; Bien];
   [Assezbien; Tresbien; Assezbien; Tresbien];
   [Passable; Tresbien; Bien; Tresbien];
   [Insuffisant; Bien; Arejeter; Tresbien];
   [Tresbien; Tresbien; Arejeter; Bien]; [Arejeter; Bien; Bien; Tresbien];
   [Arejeter; Assezbien; Assezbien; Tresbien];
   [Arejeter; Bien; Tresbien; Tresbien];
   [Passable; Bien; Arejeter; Tresbien];
   [Insuffisant; Tresbien; Insuffisant; Tresbien];
   [Assezbien; Bien; Insuffisant; Tresbien];
   [Passable; Insuffisant; Insuffisant; Tresbien];
   [Assezbien; Insuffisant; Passable; Tresbien];
   [Assezbien; Assezbien; Bien; Bien]; [Assezbien; Passable; Tresbien; Bien];
   [Insuffisant; Arejeter; Bien; Tresbien];
   [Assezbien; Bien; Passable; Tresbien];
   [Bien; Insuffisant; Arejeter; Tresbien]; [Passable; Bien; Tresbien; Bien];
   [Assezbien; Assezbien; Tresbien; Tresbien];
   [Tresbien; Bien; Tresbien; Tresbien];
   [Tresbien; Assezbien; Tresbien; Bien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Assezbien; Insuffisant; Tresbien; Tresbien];
   [Arejeter; Bien; Passable; Tresbien]; [Assezbien; Arejeter; Bien; Bien];
   [Arejeter; Assezbien; Insuffisant; Bien];
   [Arejeter; Passable; Passable; Tresbien];
   [Assezbien; Bien; Tresbien; Tresbien];
   [Tresbien; Arejeter; Bien; Tresbien];
   [Assezbien; Tresbien; Bien; Tresbien];
   [Passable; Arejeter; Arejeter; Tresbien];
   [Arejeter; Passable; Insuffisant; Bien];
   [Tresbien; Passable; Assezbien; Tresbien];
   [Assezbien; Insuffisant; Arejeter; Bien];
   [Insuffisant; Tresbien; Assezbien; Tresbien];
   [Insuffisant; Bien; Insuffisant; Bien]; [Tresbien; Tresbien; Bien; Bien];
   [Insuffisant; Insuffisant; Bien; Bien];
   [Passable; Arejeter; Assezbien; Bien];
   [Tresbien; Tresbien; Insuffisant; Bien];
   [Tresbien; Passable; Tresbien; Bien];
   [Assezbien; Tresbien; Passable; Bien]; [Bien; Assezbien; Assezbien; Bien];
   [Insuffisant; Arejeter; Passable; Tresbien];
   [Tresbien; Bien; Arejeter; Bien];
   [Arejeter; Arejeter; Passable; Tresbien];
   [Arejeter; Bien; Assezbien; Bien]; [Arejeter; Passable; Assezbien; Bien];
   [Tresbien; Tresbien; Passable; Bien]; [Bien; Insuffisant; Passable; Bien];
   [Bien; Tresbien; Bien; Tresbien]; [Tresbien; Bien; Passable; Bien];
   [Passable; Assezbien; Insuffisant; Tresbien];
   [Bien; Arejeter; Insuffisant; Tresbien];
   [Assezbien; Passable; Assezbien; Bien];
   [Assezbien; Arejeter; Tresbien; Bien];
   [Tresbien; Insuffisant; Arejeter; Tresbien];
   [Arejeter; Insuffisant; Arejeter; Bien];
   [Tresbien; Bien; Passable; Tresbien];
   [Assezbien; Arejeter; Arejeter; Tresbien];
   [Insuffisant; Arejeter; Insuffisant; Bien];
   [Assezbien; Insuffisant; Arejeter; Bien];
   [Assezbien; Tresbien; Arejeter; Tresbien];
   [Passable; Arejeter; Tresbien; Bien];
   [Insuffisant; Arejeter; Insuffisant; Bien];
   [Insuffisant; Passable; Tresbien; Tresbien];
   [Passable; Arejeter; Arejeter; Tresbien];
   [Arejeter; Arejeter; Bien; Tresbien]; [Arejeter; Insuffisant; Bien; Bien];
   [Arejeter; Insuffisant; Arejeter; Bien]];;

let (ujm3:urne_jm) = 
  [[Arejeter; Bien; Tresbien; Insuffisant; Bien; Assezbien];
   [Insuffisant; Bien; Bien; Insuffisant; Passable; Assezbien];
   [Assezbien; Assezbien; Tresbien; Arejeter; Assezbien; Tresbien];
   [Bien; Assezbien; Bien; Passable; Insuffisant; Passable];
   [Insuffisant; Arejeter; Bien; Bien; Arejeter; Tresbien];
   [Assezbien; Arejeter; Tresbien; Tresbien; Arejeter; Bien];
   [Arejeter; Tresbien; Bien; Passable; Passable; Insuffisant];
   [Passable; Passable; Bien; Bien; Bien; Passable];
   [Insuffisant; Tresbien; Bien; Arejeter; Tresbien; Tresbien];
   [Arejeter; Passable; Tresbien; Assezbien; Bien; Assezbien];
   [Assezbien; Passable; Bien; Assezbien; Insuffisant; Arejeter];
   [Arejeter; Insuffisant; Bien; Passable; Insuffisant; Arejeter];
   [Insuffisant; Arejeter; Bien; Bien; Arejeter; Assezbien];
   [Assezbien; Bien; Tresbien; Bien; Arejeter; Tresbien];
   [Passable; Passable; Tresbien; Assezbien; Assezbien; Bien];
   [Arejeter; Assezbien; Bien; Tresbien; Assezbien; Bien];
   [Passable; Arejeter; Tresbien; Tresbien; Assezbien; Bien];
   [Assezbien; Tresbien; Tresbien; Assezbien; Insuffisant; Assezbien];
   [Tresbien; Assezbien; Tresbien; Arejeter; Insuffisant; Bien];
   [Bien; Tresbien; Bien; Arejeter; Bien; Insuffisant];
   [Insuffisant; Assezbien; Tresbien; Insuffisant; Bien; Bien];
   [Arejeter; Insuffisant; Tresbien; Arejeter; Assezbien; Tresbien];
   [Insuffisant; Tresbien; Tresbien; Arejeter; Arejeter; Insuffisant];
   [Passable; Bien; Tresbien; Bien; Arejeter; Assezbien];
   [Insuffisant; Assezbien; Tresbien; Passable; Passable; Arejeter];
   [Assezbien; Arejeter; Bien; Bien; Assezbien; Passable];
   [Arejeter; Bien; Bien; Bien; Bien; Tresbien];
   [Tresbien; Assezbien; Bien; Tresbien; Passable; Bien];
   [Bien; Tresbien; Tresbien; Arejeter; Bien; Insuffisant];
   [Arejeter; Arejeter; Tresbien; Passable; Tresbien; Bien];
   [Arejeter; Assezbien; Bien; Insuffisant; Insuffisant; Insuffisant];
   [Assezbien; Passable; Bien; Tresbien; Bien; Assezbien];
   [Passable; Bien; Bien; Bien; Passable; Assezbien];
   [Tresbien; Arejeter; Tresbien; Assezbien; Assezbien; Bien];
   [Insuffisant; Passable; Bien; Bien; Tresbien; Bien];
   [Tresbien; Insuffisant; Bien; Bien; Arejeter; Bien];
   [Arejeter; Tresbien; Tresbien; Passable; Assezbien; Bien];
   [Arejeter; Assezbien; Bien; Assezbien; Assezbien; Arejeter];
   [Bien; Tresbien; Tresbien; Passable; Insuffisant; Bien];
   [Tresbien; Arejeter; Tresbien; Assezbien; Passable; Passable];
   [Passable; Tresbien; Tresbien; Passable; Assezbien; Assezbien];
   [Bien; Bien; Tresbien; Insuffisant; Assezbien; Passable];
   [Passable; Passable; Tresbien; Insuffisant; Passable; Tresbien];
   [Assezbien; Passable; Bien; Insuffisant; Passable; Bien];
   [Passable; Tresbien; Tresbien; Assezbien; Arejeter; Assezbien];
   [Bien; Passable; Bien; Assezbien; Arejeter; Insuffisant];
   [Assezbien; Insuffisant; Bien; Passable; Tresbien; Passable];
   [Bien; Arejeter; Bien; Insuffisant; Assezbien; Assezbien];
   [Bien; Bien; Bien; Assezbien; Tresbien; Insuffisant];
   [Assezbien; Arejeter; Bien; Passable; Assezbien; Bien];
   [Tresbien; Passable; Bien; Assezbien; Bien; Tresbien];
   [Bien; Tresbien; Tresbien; Assezbien; Bien; Passable];
   [Passable; Insuffisant; Tresbien; Bien; Insuffisant; Tresbien];
   [Bien; Assezbien; Tresbien; Tresbien; Bien; Arejeter];
   [Bien; Bien; Bien; Passable; Insuffisant; Passable];
   [Bien; Tresbien; Tresbien; Assezbien; Passable; Bien];
   [Insuffisant; Passable; Bien; Passable; Tresbien; Arejeter];
   [Bien; Insuffisant; Bien; Passable; Tresbien; Arejeter];
   [Passable; Assezbien; Tresbien; Tresbien; Assezbien; Passable];
   [Passable; Assezbien; Tresbien; Passable; Arejeter; Tresbien];
   [Insuffisant; Tresbien; Tresbien; Assezbien; Assezbien; Assezbien];
   [Bien; Tresbien; Tresbien; Bien; Assezbien; Insuffisant];
   [Arejeter; Assezbien; Bien; Assezbien; Tresbien; Arejeter];
   [Bien; Tresbien; Tresbien; Tresbien; Arejeter; Arejeter];
   [Insuffisant; Tresbien; Bien; Arejeter; Insuffisant; Insuffisant];
   [Bien; Arejeter; Tresbien; Insuffisant; Passable; Passable];
   [Insuffisant; Insuffisant; Tresbien; Passable; Arejeter; Bien];
   [Insuffisant; Insuffisant; Tresbien; Tresbien; Tresbien; Insuffisant];
   [Tresbien; Passable; Tresbien; Passable; Passable; Passable];
   [Passable; Insuffisant; Bien; Tresbien; Tresbien; Tresbien];
   [Bien; Passable; Tresbien; Passable; Passable; Tresbien];
   [Passable; Arejeter; Tresbien; Assezbien; Assezbien; Passable];
   [Bien; Assezbien; Tresbien; Insuffisant; Arejeter; Tresbien];
   [Bien; Tresbien; Tresbien; Insuffisant; Passable; Assezbien];
   [Arejeter; Assezbien; Bien; Arejeter; Bien; Assezbien];
   [Bien; Tresbien; Bien; Tresbien; Tresbien; Tresbien];
   [Passable; Arejeter; Bien; Passable; Assezbien; Tresbien];
   [Assezbien; Passable; Bien; Tresbien; Assezbien; Assezbien];
   [Insuffisant; Tresbien; Bien; Arejeter; Tresbien; Bien];
   [Insuffisant; Bien; Bien; Bien; Bien; Arejeter];
   [Passable; Bien; Bien; Insuffisant; Tresbien; Insuffisant];
   [Passable; Passable; Tresbien; Assezbien; Tresbien; Arejeter];
   [Tresbien; Assezbien; Bien; Assezbien; Insuffisant; Bien];
   [Arejeter; Passable; Tresbien; Insuffisant; Tresbien; Passable];
   [Tresbien; Passable; Bien; Arejeter; Insuffisant; Insuffisant];
   [Tresbien; Bien; Tresbien; Passable; Insuffisant; Insuffisant];
   [Tresbien; Arejeter; Tresbien; Assezbien; Passable; Assezbien];
   [Assezbien; Bien; Tresbien; Passable; Tresbien; Arejeter];
   [Bien; Arejeter; Bien; Insuffisant; Tresbien; Tresbien];
   [Insuffisant; Arejeter; Tresbien; Bien; Tresbien; Passable];
   [Bien; Insuffisant; Tresbien; Passable; Tresbien; Assezbien];
   [Assezbien; Passable; Bien; Insuffisant; Bien; Passable];
   [Insuffisant; Assezbien; Tresbien; Bien; Bien; Assezbien];
   [Bien; Tresbien; Bien; Bien; Assezbien; Tresbien];
   [Insuffisant; Bien; Tresbien; Tresbien; Passable; Arejeter];
   [Passable; Insuffisant; Tresbien; Bien; Insuffisant; Arejeter];
   [Passable; Arejeter; Bien; Arejeter; Passable; Assezbien];
   [Passable; Arejeter; Tresbien; Assezbien; Tresbien; Arejeter];
   [Arejeter; Assezbien; Bien; Passable; Tresbien; Bien];
   [Bien; Bien; Bien; Assezbien; Tresbien; Passable]];;

let trouve_vainqueurs_jm (u: urne_jm) (lc: candidat list) : candidat =
  let rec aux (u:urne_jm) (lc: candidat list) : candidat = (*fonction auxiliaire pour ne pas dépouiller et trier encore et encore*)
    let res = (vainqueur_jm u lc) in (*On récupère le vainqueur *)
    if res = "" then (*Problème : il faut retirer des médianes*)
      aux (supprime_perdants (supprime_meilleure_mediane u)) lc
    else
      res in (*Problème résolu *)
  let urne_traitee = supprime_perdants(tri_mentions (depouiller_jm u)) (*On traite l'urne en la triant depouillant et en supprimant les perdants*)
    in aux urne_traitee lc;; (*On rapplique la fonction auxiliaire*)

let score1 = trouve_vainqueurs_jm ujm1 lc2;;
let score2 = trouve_vainqueurs_jm ujm2 lc2;;
let score3 = trouve_vainqueurs_jm ujm3 lc3;;

(*Question 20
En résumé, un vote majoritaire permet aux électeurs d'apporter plus de nuances à leurs votes, ce qui permet de mieux refléter leurs préférences.
Plutôt que de voter stratégiquement pour éviter de « gaspiller » des votes sur des candidats qui ont peu de chance de gagner.
Cela permet aux électeurs d'exprimer pleinement leur choix plutôt que d'être contraints par des stratégies.

Cependant, une critique possible du vote à la majorité est qu'il est difficile à mettre en œuvre dans la pratique.
Par exemple, le processus de notation est complexe et difficile à comprendre pour certains électeurs, ce qui peut entraîner des erreurs et une confusion dans les résultats.
Il est aussi facile de manipuler le résultat du vote majoritaire pour obtenir un résultat pas forcément juste et faire gagner le candidat. 
Comme tout système de vote, il a ses forces et ses faiblesses, mais il peut sembler plus juste. 
*)
  
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
let ara =
N (Reg "Auvergne-Rhône-Alpes",
 [N (Dpt "Drôme",
   [Bv ("Valence",
     [("ARTHAUD", 161); ("ROUSSEL", 595); ("MACRON", 7756); ("LASSALLE", 590);
      ("LE PEN", 4679); ("ZEMMOUR", 2080); ("MÉLENCHON", 8398);
      ("HIDALGO", 519); ("JADOT", 1701); ("PÉCRESSE", 1423); ("POUTOU", 186);
      ("DUPONT-AIGNAN", 573)]);
    Bv ("Romans-sur-Isère",
     [("ARTHAUD", 181); ("ROUSSEL", 371); ("MACRON", 4030); ("LASSALLE", 334);
      ("LE PEN", 3270); ("ZEMMOUR", 1072); ("MÉLENCHON", 4108);
      ("HIDALGO", 251); ("JADOT", 850); ("PÉCRESSE", 631); ("POUTOU", 111);
      ("DUPONT-AIGNAN", 341)])]);
  N (Dpt "Isère",
   [Bv ("Meylan",
     [("ARTHAUD", 28); ("ROUSSEL", 169); ("MACRON", 4457); ("LASSALLE", 164);
      ("LE PEN", 1288); ("ZEMMOUR", 928); ("MÉLENCHON", 2198);
      ("HIDALGO", 251); ("JADOT", 906); ("PÉCRESSE", 763); ("POUTOU", 64);
      ("DUPONT-AIGNAN", 162)]);
    Bv ("Echirolles",
     [("ARTHAUD", 104); ("ROUSSEL", 506); ("MACRON", 3276); ("LASSALLE", 259);
      ("LE PEN", 2737); ("ZEMMOUR", 779); ("MÉLENCHON", 5121);
      ("HIDALGO", 223); ("JADOT", 590); ("PÉCRESSE", 360); ("POUTOU", 92);
      ("DUPONT-AIGNAN", 202)]);
    Bv ("Fontaine",
     [("ARTHAUD", 55); ("ROUSSEL", 363); ("MACRON", 2111); ("LASSALLE", 146);
      ("LE PEN", 1835); ("ZEMMOUR", 541); ("MÉLENCHON", 3113);
      ("HIDALGO", 185); ("JADOT", 493); ("PÉCRESSE", 212); ("POUTOU", 83);
      ("DUPONT-AIGNAN", 121)]);
    Bv ("Saint-Martin-d'Hères",
     [("ARTHAUD", 58); ("ROUSSEL", 436); ("MACRON", 2769); ("LASSALLE", 207);
      ("LE PEN", 2289); ("ZEMMOUR", 661); ("MÉLENCHON", 4763);
      ("HIDALGO", 242); ("JADOT", 777); ("PÉCRESSE", 300); ("POUTOU", 119);
      ("DUPONT-AIGNAN", 161)]);
    Bv ("Gières",
     [("ARTHAUD", 16); ("ROUSSEL", 66); ("MACRON", 1071); ("LASSALLE", 84);
      ("LE PEN", 641); ("ZEMMOUR", 205); ("MÉLENCHON", 844); ("HIDALGO", 96);
      ("JADOT", 301); ("PÉCRESSE", 155); ("POUTOU", 30);
      ("DUPONT-AIGNAN", 61)]);
    Bv ("Grenoble",
     [("ARTHAUD", 256); ("ROUSSEL", 1300); ("MACRON", 15968);
      ("LASSALLE", 845); ("LE PEN", 6444); ("ZEMMOUR", 3389);
      ("MÉLENCHON", 24568); ("HIDALGO", 1488); ("JADOT", 5644);
      ("PÉCRESSE", 2019); ("POUTOU", 508); ("DUPONT-AIGNAN", 661)])])]);;

let panel_2022 = ["ARTHAUD";"ROUSSEL";"MACRON";"LASSALLE";"LE PEN";"ZEMMOUR";"MÉLENCHON";"HIDALGO";"JADOT";"PÉCRESSE";"POUTOU";"DUPONT-AIGNAN"];;
let res2022GrenobleFontaineValence = max_depouille(union (union (trouve_bv ara "Valence") (trouve_bv ara "Fontaine")) (trouve_bv ara "Grenoble"));;

(*Partie 5 : Conclusion
En résumé, le système d'élection actuel présente plusieurs inconvénients, tels que la polarisation politique (ou "vote utile"), 
la représentation inégale des opinions et des valeurs, et le fait de favoriser les candidats les plus en vue par rapport aux candidats les plus capables. 
Ces lacunes peuvent poser des problèmes aux citoyens qui recherchent une représentation équitable et une prise de décision efficace, les décourageant parfois d'aller voter (26% d'abstention au premier tour de 2022).

Pour répondre à certains de ces problèmes et permettre aux électeurs d'exprimer pleinement leurs préférences,
Le vote majoritaire a été proposé à la place du scrutin majoritaire uninominal à un tour. 
Cependant, le vote à la majorité a aussi ses inconvénients, tels que la complexité du processus d'évaluation, le potentiel de fraude et de manipulation 
et le risque de favoriser des candidats modérés ou centristes.

En fin de compte, chaque système de vote a ses propres forces et faiblesses, et il n'y a pas de système de vote parfait.
Les lacunes des systèmes électoraux soulignent l'importance de développer des systèmes électoraux innovants, justes et efficaces pour assurer une représentation équitable 
et une prise de décision complète dans notre société.  
*)
