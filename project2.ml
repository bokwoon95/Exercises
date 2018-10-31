(* PART A: A FIRST DRAFT
 * Our first goal will be to build such a table and generate sentences from it, quick and dirty style, using lists and their predefined operators. Consider using as much as possible the List module (List.assoc, List.length, List.nth, etc.) and don't think about efficiency.
 * 
 * In this exercise, we will use associative lists as the data structure that links each word to its possible suffixes. Associative lists are often used in prototypes or non critical programs because they are very easy to use and debug. Their major downfall is the complexity of searching for an element. 
 * The type of an associative list that maps string keys to 'a values is simply (string * 'a) list. The value associated with a key "x" is simply the right component of the first pair in the list whose left component is "x". This lookup is already defined in the standard library as List.assoc. Hence, setting the value of "x" to 3, for instance, is just adding ("x",3) in front of the list. To remove an element, you can just use List.filter with the right predicate.
 * 
 * The type of lookup tables for this exercise is *)

type ltable = (string * string list) list

(* Write a function words : string -> string list that takes a sentence and returns the list of its words. As a first approximation, will work on single sentences in simple english, so you can consider sequences of roman letters and digits as words, and everything else as separators. If you want to build words bit by bit, you can experiment with the Buffer module. Beware, this preliminary function may not be as easy as it seems. *)
let teststring = "I am a man and my dog is a good dog and a good dog makes a good man"
;;
let str_len = String.length teststring;;
(* let buf = Buffer.create 16;; *)
(* Buffer.add_string buf teststring;; *)
(* Buffer.contents buf;; *)
(* let buf_len = Buffer.length buf;; *)
let word = Buffer.create 16;;
for i = 0 to 66 do
  Printf.printf "%c\n" (String.get teststring i)
done
;;
            
let words (str:string) : string list =
  ["yeet"]
;;

(* Write build_ltable : string list -> ltable that builds an associative list mapping each word present in the input text to all its possible successors (including duplicates). The table should also contain "START" that points to the first word and "STOP" that is pointed by the last word. 
 * For instance, a correct (and minimal) table for "x y z y x y" looks like:
 * [ ("z", [ "y" ]);
 *   ("x", [ "y" ; "y" ]);
 *   ("START", [ "x" ]);
 *   ("y", [ "x" ; "z" ; "STOP" ]) ] *)
let build_ltable words =
  ["yeet"]
;;

(* Write the random selection function next_in_ltable : (string * string list) list -> string - > string which takes a table, a given word and returns a valid successor of this word. Your function should respect the probability distribution (which should be trivially ensured by the presence of the duplicates in the successor lists). *)
let next_in_ltable table word =
  ["yeet"]
;;

(* Write the random generation function walk_ltable : (string * string list) list -> string list which takes a table and returns a sequence of words that form a valid random sentence (without the "START" and "STOP").
 * You can use display_quote: string list -> unit to display the generated texts. *)
let walk_ltable table =
  ["yeet"]
;;

