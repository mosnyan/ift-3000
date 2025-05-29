open List
(* Exercice 1 *)
(* Déterminer le type de la fonction f expliquer ce qu’elle fait *)

let f lst = map (fun x -> fold_right (+) x 0) lst

(* int list list -> int list *)
(* Retourne une liste représentant les sommes de chacune des listes internes *)

(* Exercice 2 *)

let f a b = fold_right (fun x y -> a(x)::y) b []

(* ('a -> 'b) -> 'a t -> 'b t *)
(* Prend une fonction 'a -> 'b et une liste en paramètre et retourne une nouvelle
   liste avec ses éléments modifiés par la fonction, comme map. *)

let g a b = fold_right (fun x y -> if a(x) then x :: y else y) b []

(* ('a -> bool) -> 'a t -> 'a t *)
(* Prend une fonction 'a -> bool et une liste en paramètre et retourne une nouvelle 
   liste ne contenant que les éléments ayant retourné true pour la fonction, comme filter. *)

(* Exercice 3 *)
let f a b = fold_right (fun x c -> if (exists (fun y -> x=y) a) then x::c else c) b []

(* 'a t -> 'a t -> 'a t *)
(* C'est l'intersection des ensembles. *)

let k a b = fold_right (fun x y -> if a(x) then x :: y else y) b [] (* filtre *)
let g a b = length (k (fun x -> x=a) b)

(* ('a -> bool) -> 'a t -> 'a t *)
(* 'a -> 'a t -> int *)
(* Prend un élément 'a et une liste 'a t et retourne le nombre de fois que l'élément se 
   trouve dans la liste.*)

let h a b = fold_right (fun x (y,z) -> if a(x) then (x :: y,z) else (y,x :: z )) b ([],[])

(* ()'a -> bool) -> 'a t -> 'a t * 'a t *)
(* Prend une fonction 'a -> bool et une liste comme paramètre. Partitionne la liste selon le critère. *)

(* Exercice 5 *)
(* Étant donné une liste l triée de valeurs pouvant contenir plusieurs occurrences d’une même valeur, complétez la
   définition de la fonction split permettant de retourner deux listes, l1 et l2, la première liste contenant une seule
   occurrence de chaque valeur de l, et la deuxième liste contenant le nombre d’occurrences des valeurs de l1 dans l.
   Type : ’a list -> ’a list * int list *)

let rec split l =
  match l with
  | [] -> [],[]
  | e::r -> let (elems, freqs) = split r in
  if length (filter (fun x -> x = e) elems) <> 0 then (elems, freqs)
  else (e::elems, length (filter (fun x -> x = e) l)::freqs)

(* Écrire une fonction genList qui prend comme argument un élément e et un entier n et retourne une liste contenant
   n occurrences de la valeur e.
   Type : ’a -> int -> ’a list *)

let rec genList e n =
  match n with
  | 0 -> []
  | _ -> e::genList e (n - 1)

(* Écrire une fonction unsplit qui reconstitue, à partir de deux listes L1 et L2, une liste L contenant plusieurs occur-
   rences de chaque élément. Vous pouvez bien sûr utiliser la fonction genList.
   Type : ’a list * int list -> ’a list *)

let rec unsplit (l1, l2) = 
  match (l1, l2) with
  |[], [] -> []
  |x::r, n::r' -> genList x n :: unsplit (r, r')

(* Exercice 6 *)
(* Un parking automatique enregistre, dans une liste, le numéro de matricule de chaque voiture qui entre et sort. Il
ajoute également à la liste une valeur de temps à chaque heure. Les numéros de matricules et les valeurs de temps
sont représentés par le type de données suivant : *)

type park = Enter of string | Exit of string | Time of int

(* [ Time 5 ; Enter "ABC123 " ; Enter " DEF456 " ; Time 6 ; Exit "ABC123" ; Enter " GHI789 " ;
Time 7 ; Exit " GHI789 " ; Exit " DEF456 " ] *)

(* Les listes de parking doivent être traitées pour produire une liste de tuples des numéros de matricule et des temps
passés par les voitures au parking. Dans la liste, un temps positif est le temps passé au parking alors qu’un temps
négatif indique le temps d’entrée. *)

(* Écrivez une fonction pour ajouter un numéro de matricule et un temps d’entrée (ce temps d’entrée est négatif)
comme tuple à une liste de tuples, dans un ordre alphabétique selon le numéro de matricule.*)

let enter_car e t l = let (l1, l2) = partition (fun (e', _) -> e' < e) l in l1 @ (e, t)::l2

(* Écrivez une fonction qui met à jour une liste de tuples en trouvant le tuple pour un numéro de matricule donné
   et en ajoutant un temps donné de sortie au temps négatif d’entrée dans le tuple. *)
let rec exit_car s t l = match l with
| [] -> l
| (s', t')::r -> if s' = s then (s', t + t') :: filter (fun (s'', _) -> s'' <> s) r else exit_car s t r

(* Écrivez une fonction qui, étant données une liste de parking, l’heure courante et une liste de tuples, va traiter
   la liste de parking afin de produire une liste de tuples finale, comme suit :
   — Si la liste de parking est vide alors la fonction renvoie la liste de tuple.
   — Si la liste de parking commence par un temps ("Time"), alors la fonction traite le reste de la liste en utili-
   sant ce temps comme l’heure courante.
   — Si la liste de parking commence par un numéro de matricule pour un véhicule entrant dans le parking
   ("Enter"), alors la fonction ajoute un nouveau tuple composé du numéro de matricule et de l’inverse de
   l’heure courante à la liste de tuple, puis traite le reste de la liste de parking.
   — Si la liste de parking commence par le numéro de matricule pour un véhicule qui est sortie du parking alors
   la fonction ajoute le temps courant au temps d’entrée dans le tuple correspondant, puis traite le reste de la
   liste de parking. *)

let rec finalize pl t tl = match pl with
| [] -> tl
| Time(t')::pr -> finalize pr t' tl
| Enter(e')::pr -> finalize pr t (enter_car e' (-t) tl)
| Exit(s')::pr -> finalize pr t (exit_car s' t tl)