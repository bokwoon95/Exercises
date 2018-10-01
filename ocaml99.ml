(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [a] -> Some a
  | h::t -> last t
;;
last [ "a" ; "b" ; "c" ; "d" ];;
(* - : string option = Some "d" *)
last [];;
(* - : 'a option = None *)



(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *) 
let rec last_two = function
  (* | [] | [_] -> None (\* What's wrong with this? *\) *)
  | [] | [_] -> None
  (* | [_;_] -> Some _, Some _ (\* What's wrong with this? *\) *)
  | [a;b] -> Some (a,b)
  | h::t -> last_two t
;;
last_two [ "a" ; "b" ; "c" ; "d" ];;
(* - : (string * string) option = Some ("c", "d") *)
last_two [ "a" ];;
(* - : (string * string) option = None *)



(* 3. Find the k'th element of a list. (easy) *)
let rec at k = function
  | [] -> None
  | a::b -> if k = 1 then Some a else at (k-1) b
;; 
at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
(* - : string option = Some "c" *)
at 3 [ "a" ];;
(* - : string option = None *)



(* 4. Find the number of elements of a list. (easy) *)
let rec length = function
  | [] -> 0
  | a::b -> 1 + length b
;;
let length_tr list = (* tail-recursive version *)
  let rec aux n = function
    | [] -> n
    | a::b -> aux (n+1) b in
  aux 0 list
;;
length [ "a" ; "b" ; "c"];;
(* - : int = 3 *)
length [];;
(* - : int = 0 *)



(* 5. Reverse a list. (easy) *)
let rev list =
  let rec aux list_old list_new = match list_old with
    | [] -> list_new
    | h::t -> aux t (h::list_new) in
  aux list []
;;
rev ["a" ; "b" ; "c"];;
(* - : string list = ["c"; "b"; "a"] *)
rev ["a"];;



(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome list =
  let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list in
  list = rev list
;;
is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
(* - : bool = true *)
not (is_palindrome [ "a" ; "b" ]);;
(* - : bool = true *)



(* 7. Flatten a nested list structure. (medium) *)
(* There is no nested list type in OCaml, so we need to define one
    first. A node of a nested list is either an element, or a list of
    nodes. *)
type 'a node =
| One of 'a 
| Many of 'a node list;;
(* type 'a node = One of 'a | Many of 'a node list *)
(* I don't know how tf this type keyword works so I'm gonna skip it first *)



(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
(* let compress list =
 *   let rev rlist =
 *     let rec aux acc = function
 *       | [] -> acc
 *       | h::t -> aux (h::acc) t
 *     in
 *     aux [] list
 *   in
 * ;;
 * compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; *)
(* - : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

(* 19. Rotate a list N places to the left. (medium) *)
let rec rotate list n =
  if n = 0 then list
  else if n > 0 then
    match list with
    | [] -> []
    | h::t -> rotate (t @ [h]) (n-1)
  else (* n < 0 *)
    match List.rev list with
    | [] -> []
    | h::t -> rotate (List.rev (t @ [h])) (n+1);;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
(* - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] *)
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
(* - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] *)
