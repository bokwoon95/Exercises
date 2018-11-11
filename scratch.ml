module Stack = struct
  type 'a t
  let empty = []
  let push x s = x::s
  let pop = function
    | [] -> None
    | x::xs -> Some (x,xs)
end;;

module Stack : sig
  type 'a t
  val empty : 'a list
  val push : 'a -> 'a list -> 'a list
  val pop : 'a list -> ('a * 'a list) option
end = struct
  type 'a t
  let empty = []
  let push x s = x::s
  let pop = function
    | [] -> None
    | x::xs -> Some (x,xs)
end;;

module type StackSig = sig
  type 'a t
  val empty : 'a list
  val push : 'a -> 'a list -> 'a list
  val pop : 'a list -> ('a * 'a list) option
end;;
module Stack : StackSig = struct
  type 'a t
  let empty = []
  let push x s = x::s
  let pop = function
    | [] -> None
    | x::xs -> Some (x,xs)
end;;

module Forest = struct
  type 'a forest = 'a list
  module Tree = struct
    type 'a tree =
      | Leaf of 'a
      | Node of 'a tree forest
  end
end;;
let t = Forest.Tree.Leaf 42;;
let t = Forest.Tree.Leaf "yee";;

module Naturals : sig
  type t
  val zero : t
  val succ : t -> t
  val pred : t -> t
end = struct
  type t = int
  let zero = 0
  let succ n = if n = max_int then 0 else n+1
  let pred = function
    | 0 -> 0
    | n -> n - 1
end;;

module type NaturalSig = sig
  type t
  val zero : t
  val succ : t -> t
  val pred : t -> t
  val of_int : int -> t
  val to_int : t -> int
end;;
module Naturals : NaturalSig = struct
  type t = int
  let zero = 0
  let succ n = if n = max_int then 0 else n+1
  let pred = function
    | 0 -> 0
    | n -> n - 1
  let of_int x = x
  let to_int x = x
end;;
