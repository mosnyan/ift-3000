(* Exercice 1 *)
(* Écrivez une fonction ayant le type suivant :
   ('a list * 'b list) -> ('a * 'b) list *)
let rec ex1 list1 list2 =
  match (list1, list2) with
  | [], [] -> []
  | [], _ -> []
  | _, [] -> []
  |(e1::r1), (e2::r2) -> (e1, e2) :: ex1 r1 r2

(* Exercice 2 *)
(* Étant donnée une liste de paires d’entiers, écrivez une fonction,
   sans utiliser la fonction map, qui renvoie une nouvelle liste
   constituée des sommes des éléments de chaque paire de la première liste *)

let rec addPair list =
  match list with
  | [] -> []
  | e::r -> fst e + snd e :: addPair r

(* Quel est le type de cette fonction ? *)
(* (int * int) list -> int list *)

(* Étant donnée une liste de paires de chaînes de caractères, écrivez une
   fonction, sans utiliser la fonction map, qui renvoie une nouvelle liste
   constituée des concaténations des deux chaînes de caractères qui
   constituent les paires de la première liste *)

let rec joinPair list =
  match list with
  | [] -> []
  | e::r -> (fst e ^ snd e) :: joinPair r

(* Quel est le type de cette fonction ? *)
(* (string * string) list -> string list *)

(* Étant donnée une liste de paires de chaînes de caractères et d’entiers,
   écrivez une fonction, sans utiliser la fonction map, qui renvoie une
   liste de booléens qui indiquent si l’entier associé à la chaîne de caractère
   représente sa longueur ou non *)

let rec checkSize list = 
  match list with
  | [] -> []
  | e::r -> (String.length (fst e) = snd e) :: checkSize r

(* Quel est le type de cette fonction ? *)
(* (string * int) list -> bool list *)

(* Utilisez la fonction map pour redéfinir les fonctions addPair,
   joinPair et checkSize *)

let addPairMap list =
  List.map (fun (f, s) -> f + s) list

let joinPairMap list =
  List.map (fun (f, s) -> f ^ s) list

let checkSizeMap list =
  List.map (fun (f, s) -> String.length f = s) list

(* Exercice 3 *)

type intArbre = Vide | Noeud of int * intArbre * intArbre

(* Implanter une fonction somme qui prend en argument un arbre
   d’entiers (intArbre), et retourne la somme de tous les entiers
   qui sont dans l’arbre *)

let rec somme arbre =
  match arbre with
  | Vide -> 0
  | Noeud (v, a1, a2) -> v + somme a1 + somme a2

(* Étant donné le type suivant : ( int −> int) −> intArbre −> intArbre) *)

(* Expliquer ce que représente ce type. *)
(* Cette fonction prend une fonction int -> int et une arbre en paramètre.
   Elle produit un nouvel arbre. *)

(* Implanter une fonction en Ocaml de votre choix qui a ce type. *)

let rec mapArbre f a =
  match a with
  | Vide -> Vide
  | Noeud (v, a1, a2) -> Noeud (f v, mapArbre f a1, mapArbre f a2)