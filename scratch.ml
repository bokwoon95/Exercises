type operation =
  | Op of string * operation * operation
  | Value of int
;;

type env = (string * (int -> int -> int)) list
;;

let rec lookup_function str env =
  match env with
  | [] -> invalid_arg "lookup_function"
  | (k,v)::t when k = str -> v
  | h::t -> lookup_function str t
;;

(* a value or a function name *)
type st = V of int | O of string;;

(* a recursive function *)
type 'a fix = Fix of ('a fix -> 'a );;

(* there are two stages 
   1. conversion from operation to reverse polish notation
   2. evaluation of the reverse polish notation using a stack 
*)

let compute2 env = function op -> 

  (* the algorithm *)
  (fun y1 y2 polish eval ->
     y1 eval [] (y2 polish [] op) )

  (* the fixed point combinator *)
    (fun f -> 
       (fun g -> g (Fix g)) 

         ( fun (Fix x) -> 
             fun a ->  f (x (Fix x)) a
         )
    )


  (* the fixed point combinator *)
    (fun f -> 
       (fun g -> g (Fix g)) 

         ( fun (Fix x) -> 
             fun a ->  f (x (Fix x)) a
         )
    )




    (* get reverse polish notation using stack s *)
    (fun f -> 

       fun s -> function
         | Value x ->  (V x)::s
         | Op (name, l, r) -> f  (f  ((O name)::s) r) l

    )                         


    (* evaluate the reverse polish expression using stack s *)
    (fun f -> 

       fun s -> function 
         | [] -> (match s with 
             |[x] -> x
             | _ -> invalid_arg "eval")
         | (V x)::r -> f  (x::s) r
         | (O name)::r -> 
             (match s with 
              | y::x::rr -> f ((lookup_function name env x y)::rr) r 
              | _ -> invalid_arg "eval"
             )

    )
