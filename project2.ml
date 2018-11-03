let rec assoc_opt alist key =
  match alist with
  | [] -> None
  | (k,v)::t when k = key -> v
  | (k,v)::t -> assoc_opt t key
;;

(* PART A: A FIRST DRAFT
 * Our first goal will be to build such a table and generate sentences from it, quick and dirty style, using lists and their predefined operators. Consider using as much as possible the List module (List.assoc, List.length, List.nth, etc.) and don't think about efficiency.
 *
 * In this exercise, we will use associative lists as the data structure that links each word to its possible suffixes. Associative lists are often used in prototypes or non critical programs because they are very easy to use and debug. Their major downfall is the complexity of searching for an element.
 * The type of an associative list that maps string keys to 'a values is simply (string * 'a) list. The value associated with a key "x" is simply the right component of the first pair in the list whose left component is "x". This lookup is already defined in the standard library as List.assoc. Hence, setting the value of "x" to 3, for instance, is just adding ("x",3) in front of the list. To remove an element, you can just use List.filter with the right predicate. *)

(* The type of lookup tables for this exercise is *)
type ltable = (string * string list) list;;

let rec display_quote strlist =
  match strlist with
  | [] -> ()
  | [x] -> Printf.printf "%s\n" x
  | h::t -> (Printf.printf "%s " h;
             display_quote t;
            )
;;

(* Write a function words : string -> string list that takes a sentence and returns the list of its words. As a first approximation, will work on single sentences in simple english, so you can consider sequences of roman letters and digits as words, and everything else as separators. If you want to build words bit by bit, you can experiment with the Buffer module. Beware, this preliminary function may not be as easy as it seems. *)
let alphanumerals = [
    'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
]
;;

let words str =
  let alphanumerals = [
    'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
  ] in
  let wordbuf = Buffer.create 16 in
  let str_len = String.length str in
  let rec aux i accu =
    let word = Buffer.contents wordbuf in
    let wordbuf_empty = Buffer.length wordbuf = 0 in
    if i = str_len
    then
      if wordbuf_empty then List.rev accu else List.rev (word::accu)
    else
      let c = String.get str i
      in
      if List.mem c alphanumerals
      then (
        Buffer.add_char wordbuf c;
        aux (i+1) accu;
      ) else (
        if wordbuf_empty
        then
          aux (i+1) accu
        else (
          Buffer.clear wordbuf;
          aux (i+1) (word::accu);
        )
      )
  in
  aux 0 []
;;

let teststring = "I am a man and my dog is a good dog and a good dog makes a good man"
;;

(* Write build_ltable : string list -> ltable that builds an associative list mapping each word present in the input text to all its possible successors (including duplicates). The table should also contain "START" that points to the first word and "STOP" that is pointed by the last word.
 * For instance, a correct (and minimal) table for "x y z y x y" looks like:
 * [ ("z", [ "y" ]);
 *   ("x", [ "y" ; "y" ]);
 *   ("START", [ "x" ]);
 *   ("y", [ "x" ; "z" ; "STOP" ]) ] *)

let update_alist alist word payload_f default =
  let rec aux alist_dc accu =
    match alist_dc with
    | [] -> List.rev ((word, default)::accu)
    | (k,v)::t when k = word -> List.rev ((k, payload_f v)::accu) @ t
    | (k,v)::t -> aux t ((k,v)::accu)
  in
  aux alist []
;;
let lis = [
    ("z", [ "y" ]);
    ("x", [ "y" ; "y" ]);
    ("START", [ "x" ]);
    ("y", [ "x" ; "z" ; "STOP" ]) ];;
let r = update_alist lis "ee" (fun a -> "yeet"::a) ["STAN"];;

let build_ltable words =
  let rec aux words2 accu =
    match words2 with
    | [] -> accu
    | [x] -> update_alist accu x (fun a->"STOP"::a) ["STOP"]
    | x::y::t -> aux (y::t) (update_alist accu x (fun a->y::a) [y])
  in
  aux ("START"::words) []
;;
let teststring = "I am a man and my dog is a good dog and a good dog makes a good man";;
let ts_lis = words teststring;;
let r = build_ltable ts_lis;;

(* Write the random selection function next_in_ltable : (string * string list) list -> string - > string which takes a table, a given word and returns a valid successor of this word. Your function should respect the probability distribution (which should be trivially ensured by the presence of the duplicates in the successor lists). *)
let next_in_ltable table word =
  try
    let list = (List.assoc word table) in
    List.nth list (Random.int (List.length list))
  with
  | _ -> "exception triggered"
;;

(* Write the random generation function walk_ltable : (string * string list) list -> string list which takes a table and returns a sequence of words that form a valid random sentence (without the "START" and "STOP").
 * You can use display_quote: string list -> unit to display the generated texts. *)
let walk_ltable table =
  let rec aux accu input =
    let output = next_in_ltable table input in
    match output with
    | "STOP" -> List.rev accu
    | _ -> aux (output::accu) output
  in
  aux [] "START"
;;

(* PART B: PERFORMANCE IMPROVEMENTS
 * Now, we want to use more efficient data structures, so that we can take larger inputs and build bigger transition tables.
 *
 * In this exercise, we will use hash tables, predefined in OCaml in the Hashtbl module. Used correctly, hash table provide both fast insertion and extraction. Have a look at the documentation of the module. In particular, don't miss the difference between Hashtbl.add and Hashtbl.replace (you'll probably want to use the latter most of the time). *)

(* The types for this exercise are: *)
type distribution =
  { total : int ;
    amounts : (string * int) list }
type htable = (string, distribution) Hashtbl.t

(* In the simple version, we stored for each word the complete list of suffixes, including duplicates. This is a valid data structure to use when building the table since adding a new suffix in front of the list is fast. But when generating, it means computing the length of this list each time, and accessing its random nth element, which is slow if the list is long.
 * Write compute_distribution : string list -> distribution that takes a list of strings and returns a pair containing its length and an association between each string present in the original list and its number of occurrences.
 * For instance, compute_distribution ["a";"b";"c";"b";"c";"a";"b";"c";"c";"c"] should give { total = 10 ; amounts = [("c", 5); ("b", 3); ("a", 2)] }.
 * Hint: a first step that simplifies the problem is to sort the list. *)
let compute_distribution l =
  let l_sorted = List.sort compare l in
  let rec aux l_dc alist total =
    match l_dc with
    | [] -> { total = total; amounts = alist }
    | h::t -> (
      match alist with
      | [] -> aux t [(h,1)] (total+1)
      | (k,v)::t2 when h = k -> aux t ((k,v+1)::t2) (total+1)
      | (k,v)::t2 -> aux t ((h,1)::(k,v)::t2) (total+1)
    )
  in
  aux l_sorted [] 0
;;
let r = compute_distribution ["a";"b";"c";"b";"c";"a";"b";"c";"c";"c"];;

(* Write a new version of build_htable : string list -> htable that creates a hash table instead of an associative list, so that both table building and sentence generation will be faster.
 * Like the associative list, the table is indexed by the words, each word being associated to its successors. But instead of just storing the list of successors, it will use the format of the previous question.
 * Hint: You can first define an intermediate table of type (string, string list) Hashtbl.t that stores the lists of successors with duplicates. Then you traverse this intermediate table with Hashtbl.iter, and for each word, you add the result of compute_distribution in the final table. *)

let introspect htable =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) htable [];;

let make_htable (list:('a*'b) list) =
  let htable = Hashtbl.create 16 in
  List.fold_left (fun () (k,v) -> Hashtbl.add htable k v) () list;
  htable
;;
let ht = make_htable [ ("a",1); ("b",2); ("c",3); ("d",4); ("e",5); ("f",6); ("g",7); ("h",8); ("i",9); ("j",10); ];;
introspect ht;;

let update_alist alist word payload_f default =
  let rec aux alist_dc accu =
    match alist_dc with
    | [] -> List.rev ((word, default)::accu)
    | (k,v)::t when k = word -> List.rev ((k, payload_f v)::accu) @ t
    | (k,v)::t -> aux t ((k,v)::accu)
  in
  aux alist []
;;

let sample_alist = [("a",1);("b",1);("c",1);("d",1);("e",1);("f",1);];;
let r = update_alist sample_alist "b" (fun x->x+1) 1;;
let r = update_alist sample_alist "z" (fun x->x+1) 1;;

let r = update_alist [] "z" (fun x->x+1) 1;;
let r = update_alist r "z" (fun x->x+1) 1;;
let r = update_alist r "c" (fun x->x+1) 1;;

let hashtbl_find_opt htable item =
  try
    Some (Hashtbl.find htable item)
  with
  | Not_found -> None
;;
let ht = Hashtbl.create 16;;
Hashtbl.replace ht "a" 99;;

let update_htable htable word payload =
  let var_opt = hashtbl_find_opt htable word
  in
  let num,alist = match var_opt with
    | None -> 0,[]
    | Some {total=a; amounts=alist} -> a,alist
  in
  let alist_new = update_alist alist payload (fun a->a+1) 1
  in
  Hashtbl.replace htable word {total=(num+1); amounts=alist_new}
;;

let build_htable words =
  let htable = Hashtbl.create 16 in
  let rec aux words_dc =
    match words_dc with
    | [] -> ()
    | [x] -> update_htable htable x "STOP"
    | x::y::t -> (update_htable htable x y;
                  aux (y::t);
                 )
  in
  aux ("START"::words);
  htable
;;

let teststring = "I am a man and my dog is a good dog and a good dog makes a good man";;
let ts_lis = words teststring;;
let ht = build_htable ts_lis;;
let r = introspect ht;;

(* Define next_in_htable : htable -> string -> string that does the same thing as next_in_ltable for the new table format. *)
let next_in_htable htable word =
  try
    let {total=num; amounts=alist} = Hashtbl.find htable word in
    let rand_idx = (Random.int num) + 1 in
    let rec aux i l =
      match l with
      | [] -> failwith "next_in_htable.aux: reached the end of the list but rand_idx is still larger than 0"
      | (k,v)::t -> (let i2 = i - v in
                     if i2 <= 0 then k
                     else
                       aux i2 t
                    )
    in
    aux rand_idx alist
  with
  | Not_found -> "word not found in htable"
;;

(* Finally, define walk_htable : htable -> string list *)

let walk_htable (htable:htable) : string list =
  let rec aux accu input =
    let output = next_in_htable htable input in
    match output with
    | "STOP" -> List.rev accu
    | _ -> aux (output::accu) output
  in
  aux [] "START"
;;

(* PART C: QUALITY IMPROVEMENTS *)
(* If we want to generate sentences from larger corpuses, such as the ones of the ebooks_corpus given in the prelude, we cannot just ignore the punctuation. We also want to generate text using not only the beginning of the original text, but the start of any sentence in the text. *)
(* Define sentences : string -> string list list that splits a string into a list of sentences such as: *)
(* uninterrupted sequences of roman letters, numbers, and non ASCII characters (in the range '\128'..'\255') are words; *)
(* single punctuation characters ';', ',', ':', '-', '"', '\'', '?', '!' and '.' are words; *)
(* punctuation characters '?', '!' and '.' terminate sentences; *)
(* everything else is a separator; *)
(* and your function should not return any empty sentence. *)

let alphanumeral_q char =
  let int = int_of_char char in
  if int >= (int_of_char '0') && int <= (int_of_char '9')
  || int >= (int_of_char 'a') && int <= (int_of_char 'z')
  || int >= (int_of_char 'A') && int <= (int_of_char 'Z')
  then
    true
  else
    false
;;

let punctuation_q char =
  let int = int_of_char char in
  if int = (int_of_char ';')
  || int = (int_of_char ',')
  || int = (int_of_char '-')
  || int = (int_of_char '\'')
  || int = (int_of_char '?')
  || int = (int_of_char '!')
  || int = (int_of_char '.')
  then
    true
  else
    false
;;

let terminator_q char =
  let int = int_of_char char in
  if int = (int_of_char '?')
  || int = (int_of_char '!')
  || int = (int_of_char '.')
  then
    true
  else
    false
;;

let separator_q char =
  not (alphanumeral_q char || punctuation_q char);;

let flush_properly char word_buf word_list sentence_list =
  let word_buf_not_empty = Buffer.length word_buf != 0 in
  let word = Buffer.contents word_buf in
  let strchar = String.make 1 char in

  if terminator_q char
  then
    if word_buf_not_empty
    then
      let () = Buffer.clear word_buf in
      let word_list = strchar::word::word_list in
      let sentence_list = (List.rev word_list)::sentence_list in
      let word_list = [] in
      word_list,sentence_list
    else
      let word_list = strchar::word_list in
      let sentence_list = (List.rev word_list)::sentence_list in
      let word_list = [] in
      word_list,sentence_list

  else if punctuation_q char
  then
    if word_buf_not_empty
    then
      let () = Buffer.clear word_buf in
      let word_list = strchar::word::word_list in
      word_list,sentence_list
    else
      let word_list = strchar::word_list in
      word_list,sentence_list

  else if alphanumeral_q char
  then
    let () = Buffer.add_char word_buf char in
    word_list,sentence_list

  else
  if word_buf_not_empty
  then
    let () = Buffer.clear word_buf in
    let word_list = word::word_list in
    word_list,sentence_list
  else
    word_list,sentence_list
;;

let flush_properly char word_buf word_list sentence_list =
  if alphanumeral_q char
  then
    let () = Buffer.add_char word_buf char in
    word_list,sentence_list
  else

    let word_list =
      if Buffer.length word_buf != 0
      then
        let word = Buffer.contents word_buf in
        let () = Buffer.clear word_buf in
        word::word_list
      else
        word_list
    in

    let word_list =
      if punctuation_q char
      then
        (String.make 1 char)::word_list
      else
        word_list
    in

    let word_list,sentence_list =
      if terminator_q char
      then
        let sentence = List.rev word_list in
        let word_list = [] in
        word_list,sentence::sentence_list
      else
        word_list,sentence_list
    in

    word_list,sentence_list
;;

let wb = Buffer.create 16;;
Buffer.clear wb;Buffer.add_string wb "yeet";;
let wl = ["twenty";"nineteen";"eighteen";"seventeen";"sixteen";];;
let sl = [
  ["eleven";"twelve";"thirteen";"fourteen";"fifteen";];
  ["six";"seven";"eight";"nine";"ten";];
  ["one";"two";"three";"four";"five";];
];;
let wl,sl = flush_properly 'a' wb wl sl;;
let wl,sl = flush_properly ';' wb wl sl;;
let wl,sl = flush_properly 'b' wb wl sl;;
let wl,sl = flush_properly 'c' wb wl sl;;
let wl,sl = flush_properly 'd' wb wl sl;;
let wl,sl = flush_properly '.' wb wl sl;;
Buffer.contents wb;;

let sentences str =
  let word_buf = Buffer.create 16 in
  let story_buf = Buffer.create 16 in
  let () = Buffer.add_string story_buf str in
  let story_len = Buffer.length story_buf
  in
  let rec aux i word_list sentence_list =
    if i = story_len
    then
      let word_list,sentence_list
        = flush_properly ' ' word_buf word_list sentence_list in
      (* (List.rev (List.rev word_list)::sentence_list) *)
      (* (List.rev word_list)::sentence_list *)
      List.rev sentence_list
    else
      let char = Buffer.nth story_buf i in
      let word_list,sentence_list =
        flush_properly char word_buf word_list sentence_list in
      aux (i+1) word_list sentence_list
  in
  aux 0 [] []
;;

let sntnc = "This is a\nsample, albeit short, sentence. That should spawn three--! sentences.";;
sentences sntnc;;
