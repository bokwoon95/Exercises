(* gcd that takes two non-negative integers n and m, and that returns the greatest common divisor of n and m, following Euclid's algorithm. *)
let rec gcd n m =
  if n = 0 then
    m
  else if m = 0 then
    n
  else if n>m then
    gcd m (n mod m)
  else (* m>=n *)
    gcd n (m mod n) ;;

(* multiple_upto : int -> int -> bool that takes two non-negative integers n and r, and that tells whether n admits at least one divisor between 2 and r, inclusive. In other words that there exists a number d >= 2 and <= r, such that the remainder of the division of n by d is zero. *)
let rec multiple_upto n r =
  if r = 1 then false else
    n mod r = 0 || multiple_upto n (r-1) ;;

(* is_prime a takes a non-negative integer n and checks whether it is a prime number. *)
let rec is_prime n =
  let rec aux_is_prime n m =
    m = 1 || not (n mod m = 0) && aux_is_prime n (m-1) in
  if n = 1 then false else aux_is_prime n (n-1);;


(* POINTS AND VECTORS
 * The given prelude defines three types, one for three dimensional points, another for velocity vectors in three dimensions, and another one representing moving objects in space.
 *
 * Write a function move : point -> dpoint -> point such that move p dp is the point p whose coordinates have been updated according to dp.
 * (x is now x +. dx, y is now y +. dy, z is now z +. dz.
 * Write a function next : physical_object -> physical_object such that next o is the physical object o at time t + dt.
 * The position of next o is the position of o moved according to its velocity vector.
 * Suppose that these objects are spheres whose radius is 1.0.
 * Write a function will_collide_soon : physical_object -> physical_object -> bool that tells if at the next instant, the two spheres will intersect. *)
(* THE GIVEN PRELUDE *)
type point  = { x : float; y : float; z : float };;
type dpoint = { dx : float; dy : float; dz : float };;
type physical_object = { position : point; velocity : dpoint };;
(* YOUR ANSWER *)
let move p dp =
  let {x=a;y=b;z=c} = p
  and {dx=da;dy=db;dz=dc} = dp
  in
  {x = a +. da; y = b +. db; z = c +. dc}
;;
let next obj =
  let {position;velocity} = obj
  in
  {position=(move position velocity); velocity=velocity}
;;
let will_collide_soon p1 p2 =
  let dist_between a b =
    let xoff = a.x -. b.x
    and yoff = a.y -. b.y
    and zoff = a.z -. b.z
    in sqrt (xoff**2. +. yoff**2. +. zoff**2.)
  in
  dist_between (next p1).position (next p2).position < 2.
;;

(* TIME ON PLANET SHADOKUS
 * On planet Shadokus, a year has 5 months, each month has 4 days, each day has 3 hours and each hour has 2 minutes. A calendar date is therefore defined as the record type date of the given prelude.
 *
 * A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and its minute index is >= 0 and <= 1.
 * The start of year 12 would be:
 * { year = 12; month = 1; day = 1; hour = 0; minute = 0 }
 * The end of year 12 would be:
 * { year = 12; month = 5; day = 4; hour = 2; minute = 1 }
 *
 * Write a function wellformed : date -> bool which checks that the input date is well formed.
 * On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a magical computer that evaluates the infinite lambda-term of time. It is defined by value the_origin_of_time of the given prelude.
 * Write a function next : date -> date which computes the date which comes one minute after the input date.
 * In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time).
 * Write a function of_int : int -> date that converts such an integer into a date. *)
(* THE GIVEN PRELUDE *)
type date =
  { year : int; month : int; day : int;
    hour : int; minute : int };;
let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 };;

(* YOUR ANSWER *)
let wellformed date =
  let check_bounds var min max =
    var >= min && var <= max
  in
  check_bounds date.year 1 max_int
  && check_bounds date.month 1 5
  && check_bounds date.day 1 4
  && check_bounds date.hour 0 2
  && check_bounds date.minute 0 1
;;
let next date =
  let a = date.year
  and b = date.month
  and c = date.day
  and d = date.hour
  and e = date.minute
  and cons_date a b c d e =
    { year=a; month=b; day=c; hour=d; minute=e }
  in
  if      date.minute+1 <= 1 then cons_date a b c d (e+1)
  else if date.hour+1   <= 2 then cons_date a b c (d+1) 0
  else if date.day+1    <= 4 then cons_date a b (c+1) 0 0
  else if date.month+1  <= 5 then cons_date a (b+1) 1 0 0
  else                            cons_date (a+1) 1 1 0 0;
;;
let of_int minutes =
  let rec aux date min =
    if min = 0 then date else aux (next date) (min-1)
  in
  aux the_origin_of_time minutes
;;

(* FINDING THE MINIMUM
 * Consider a non empty array of integers a.
 *
 * Write a function min : int array -> int that returns the minimal element of a.
 * Write a function min_index : int array -> int that returns the index of the minimal element of a. *)
let min a =
  Array.fold_left (fun x y -> if y < x then y else x) a.(0) a
;;
let min_index a =
  let rec aux i a smallest smallest_i =
    let lastidx = (Array.length a) in
    if i = lastidx
    then
      smallest_i
    else if a.(i) < smallest
    then
      aux (i+1) a a.(i) i
    else
      aux (i+1) a smallest smallest_i
  in
  aux 0 a a.(0) 0
;;

(* SEARCHING FOR STRINGS IN ARRAYS
 * Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
 * Using the binary search algorithm, an element can be found very quickly in a sorted array.
 * Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
 * The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable. *)
let is_sorted a =
  let second_last_index = Array.length a - 2
  and sorted = ref true in
  for i = 0 to second_last_index do
    sorted := !sorted && String.compare a.(i) a.(i+1) < 0
  done;
  !sorted
;;
let find arr str =
  let len = Array.length arr in
  if len = 0 then -1 else
    let last_index = len - 1
    in
    let rec aux l r str arr =
      let m = (l + r)/2
      in
      let cmpval = String.compare str arr.(m)
      in
      if cmpval = 0 then (Printf.printf "(A) l=%d\tr=%d\tm=%d\n" l r m;m)
      else if r < l then (Printf.printf "(B) l=%d\tr=%d\tm=%d\n" l r m;-1)
      else if cmpval > 0 then (Printf.printf "(C) l=%d\tr=%d\tm=%d\n" l r m;aux (m+1) r str arr)
      else (Printf.printf "(D) l=%d\tr=%d\tm=%d\n" l r m;aux l (m-1) str arr)
    in
    aux 0 last_index str arr
;;
let ifind arr str = (*iterative version of find, does not return a value but mutates a ref value 'm'*)
  let l = ref 0
  and r = ref ((Array.length arr) - 1)
  and m = ref 0
  and answer = ref (-1)
  in
  try
    while l <= r do
      m := ((!l + !r)/2); Printf.printf "l=%d\tr=%d\tm=%d\n" !l !r !m;
      if String.compare str arr.(!m) = 0 then (answer := !m;raise Exit)
      else if String.compare arr.(!m) str < 0
      then l := (!m)+1
      else r := (!m)-1
    done;
  with Exit -> ();
;;

(* A SMALL TYPED DATABASE
 * The code of the mini-database example is given in the prelude.
 *
 * You may have noticed that there is an error in the implementation of our database. This error leads to not finding users that should be in the database (because they have been added at some point, and not deleted since) after certain sequences of queries.
 * Find the bug and give a sequence of operations proof_of_bug of type query array that exhibits it when executed one after the other on an initially empty database.
 * The failure must be triggered by the last query.
 * To fix this bug, write a new version of delete that enforces the following invariant on the database, which is expected by the other functions.
 * All the contacts of a database db (and no others) should be stored in the array db.contacts between indexes 0 and db.number_of_contacts - 1 (inclusive).
 * Write a new function update : database -> contact -> (bool * database * contact) that either changes the number of an existing person or inserts a new contact. It should return true and the updated database if any of these two options succeeded, or false with the untouched database. The returned contact is not important, it is here just so the function has the same signature as the others.
 * Write an updated engine function that does an update when given a query with code 3, and uses your updated delete function. *)
(* THE GIVEN PRELUDE *)
(* A phone number is a sequence of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
    name         : string;
    phone_number : phone_number
  };;

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) };;

(* A database is a collection of contacts. *)
type database = {
    number_of_contacts : int;
    contacts : contact array;
  };;

(* [make n] is the database with no contact and at most [n] contacts
    stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  };;

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
    code    : int;
    contact : contact;
  }
(* YOUR ANSWER *)
let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (Printf.printf "Found in idx=%d: %s\n" idx db.contacts.(idx).name;
       (true, db, db.contacts.(idx)))
    else
      (Printf.printf "Searching idx=%d: %s\n" idx db.contacts.(idx).name;
       aux (idx + 1))
  in
  aux 0;;

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
        if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
          number_of_contacts = db.number_of_contacts + 1;
          contacts = Array.init (Array.length db.contacts) cells
        }
      in
      (true, db', contact);;

let old_delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

let delete db contact =
  let (status, db, contact) = search db contact
  and found = ref db.number_of_contacts
  in
  if not status then (false, db, contact)
  else
    let cells i =
      if i >= ((db.number_of_contacts)-1) then
        nobody
      else if db.contacts.(i).name = contact.name then
        (found:=i;db.contacts.(i+1))
      else if i > !found then
        db.contacts.(i+1)
      else
        db.contacts.(i)
    in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

let update db contact =
  let (status, db, _) = search db contact in
  if not status then insert db contact
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        contact
      else
        db.contacts.(i)
    in
    let db' = {
        number_of_contacts = db.number_of_contacts;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;

let db = make 10;;
(* Insert *)
let res,db,_ = engine db { code=0; contact={name="melvin";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="samuel";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="heather";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="jim";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="bom-bom";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="yolanda";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="jack";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="paul";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="vingo";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="drej";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="Anon";phone_number=(1,2,3,4)}};;
(* Search *)
let res,db,_ = engine db { code=2; contact={name="melvin";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="samuel";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="heather";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="jim";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="bom-bom";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="yolanda";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="jack";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="paul";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="vingo";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="drej";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=2; contact={name="Anon";phone_number=(1,2,3,4)}};;
(* Update *)
let res,db,_ = engine db { code=3; contact={name="melvin";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="samuel";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="heather";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="jim";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="bom-bom";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="yolanda";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="jack";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="paul";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="vingo";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="drej";phone_number=(4,3,2,1)}};;
let res,db,_ = engine db { code=3; contact={name="Anon";phone_number=(4,3,2,1)}};;
(* Delete *)
let res,db,_ = engine db { code=1; contact={name="melvin";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="samuel";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="heather";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="jim";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="bom-bom";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="yolanda";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="jack";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="paul";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="vingo";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=1; contact={name="drej";phone_number=(1,2,3,4)}};;
let res,db,_ = engine db { code=0; contact={name="Anon";phone_number=(1,2,3,4)}};;

(* PATTERN MATCHING EXHAUSTIVITY
 * We have seen in the course the example of non exhaustive pattern matching given below. Write the code for the missing cases. *)
(* THE GIVEN PRELUDE *)
type color = Black | Gray | White ;;
(* YOUR ANSWER *)
let lighter c1 c2 =
  match (c1, c2) with
  | (Black, Black) -> false
  | (White, White) -> false
  | (Gray, Gray) -> false
  | (Black, _) -> true
  | (_, White) -> true
  | (White, Gray) -> false
  | (Gray, Black) -> false
  | (white, Black) -> false
;;

(* A TYPE FOR ARRAY INDEXES
 * The previous week, we asked you the following question: Consider a non empty array of integers a, write a function min_index : int array -> int that returns the index of the minimal element of a.
 * As the arrays contain integers and the indices of arrays are also represented by integers, you might have confused an index and the content of a cell. To avoid such a confusion, let us define a type for index (given in the prelude below).
 * This type has a single constructor waiting for one integer.
 * For instance, if you want to represent the index 0, use the value Index 0.
 * Defining such a type is interesting because it allows the type-checker to check that an integer is not used where an index is expected (or the converse).
 *
 * Write a function read : int array -> index -> int such that read a (Index k) returns the k-th element of a.
 * Write a function inside : int array -> index -> bool such that inside a idx is true if and only if idx is a valid index for the array a.
 * Write a function next : index -> index such that next (Index k) is equal to Index (k + 1).
 * Consider a non empty array of integers a, write a function min_index : int array -> index that returns the index of the minimal element of a. *)
(* THE GIVEN PRELUDE *)
type index = Index of int
(* YOUR ANSWER *)
let int_of_index = function
  | Index n -> n;;

let read a index =
  a.(int_of_index index);;

let inside a index =
  let idx = int_of_index index in
  idx >= 0 && idx < (Array.length a);;

let next index =
  Index ((int_of_index index)+1);;

let min_index arr =
  let rec aux index minval mindex =
    if not (inside arr index) then
      mindex
    else
    let currval = arr.(int_of_index index) in
    if currval < minval then
      aux (next index) currval index
    else
      aux (next index) minval mindex
  in
  aux (Index 1) arr.(0) (Index 0)
;;

(* THE OPTION TYPE
 * Optional values are commonly used in OCaml in the return type of partial functions, i.e. functions that may fail on some input. The following questions illustrate such situations.
 * In the Pervasives module which is loaded automatically, there is a type option with two constructors:
 * Some (e) has type 't option if e has type 't and represents the presence of some value e of type 't.
 * None has type 't option and represents the absence of some value of type 't.
 *
 * Write a function find : string array -> string -> int option such that find a w = Some idx if a.(idx) = w and find a w = None if there is no such index.
 * Sometimes, when a value of type t is missing, a default value should be used.
 * Write a function default_int : int option -> int such that: default_int None = 0 and default_int (Some x) = x.
 * Write a function merge : int option -> int option -> int option such that:
 * merge None None = None
 * merge (Some x) None = merge None (Some x) = Some x
 * merge (Some x) (Some y) = Some (x + y) *)

let find a w =
  let len = Array.length a in
  let rec loop i =
    if i >= len then
      None
    else if String.compare a.(i) w = 0 then
      Some i
    else
      loop (i+1)
  in
  loop 0
;;

let default_int = function
  | None -> 0
  | Some n -> n
;;

let merge a b = match a,b with
  | None,None -> None
  | (Some n),None | None,(Some n) -> Some n
  | (Some n),(Some m) -> Some (n+m)
;;

(* FIRST IN FIRST OUT
 * A queue is a standard FIFO data structure. See wikipedia
 *
 * In this exercise, we implement a queue with a pair of two lists (front, back) such that front @ List.rev back represents the sequence of elements in the queue.
 *
 * Write a function is_empty : queue -> bool such that is_empty q is true if and only if q has no element.
 * Write a function enqueue : int -> queue -> queue such that enqueue x q is the queue as q except that x is at the end of the queue.
 * Write a function split : int list -> int list * int list such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
 * Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') where x is the front element of the queue q and q' corresponds to remaining elements. This function assumes that q is non empty. *)
(* THE GIVEN PRELUDE *)
type queue = int list * int list
(* YOUR ANSWER *)
let is_empty ((front, back):queue) =
  List.length front = 0 && List.length back = 0;;

let enqueue x ((front, back):queue) =
  ((front,x::back):queue);;

let split l =
  let halfway_len = (List.length l)/2
  in
  let rec loop i fron bac =
    if i >= halfway_len then
      List.rev fron, List.rev bac
    else
      match fron with
      | [] -> List.rev fron, List.rev bac (* should never reach here *)
      | h::t -> loop (i+1) t (h::bac)
  in
  loop 0 l []
;;

let rec dequeue (front, back) =
  match front with
  | [] -> dequeue (split (back @ List.rev front))
  | h::t -> h, (t,back)
;;

(* CLASSIC FUNCTIONS OVER LISTS
 * In this exercise, we implement the classic functions over lists.
 *
 * Write a function mem : int -> int list -> bool such that mem x l is true if and only if x occurs in l.
 * Write a function append : int list -> int list -> int list such that append l1 l2 is the concatenation of l1 and l2.
 * Write a function combine : int list -> int list -> (int * int) list such that combine l1 l2 is the list of pairs obtained by joining the elements of l1 and l2. This function assumes that l1 and l2 have the same length. For instance, combine [1;2] [3;4] = [(1, 3); (2, 4)].
 * Write a function assoc : (string * int) list -> string -> int option such that assoc l k = Some x if (k, x) is the first pair of l whose first component is k. If no such pair exists, assoc l k = None. *)

let rec rev list =
  let rec aux accu lis = match lis with
    | [] -> accu
    | h::t -> aux (h::accu) t
  in
  aux [] list
;;

let rec mem x l =
  match l with
  | [] -> false
  | h::t -> h=x || mem x t
;;

let append l1 l2 =
  let rec aux ll1 ll2 =
    match ll1 with
    | [] -> ll2
    | h::t -> aux t (h::ll2)
  in aux (rev l1) l2
;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h::(append t l2)
;; (*not tail recursive*)

let rec help_append_list l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> help_append_list t (h::l2);;

let combine l1 l2 =
  let rec aux ll1 ll2 accu =
    match ll1,ll2 with
    | [],[] -> accu
    | h::t,h'::t' -> aux t t' ((h,h')::accu)
    | _,_ -> failwith "lists are not of equal length!"
  in
  aux (rev l1) (rev l2) []
;;

let rec assoc l k =
    match l with
    | [] -> None
    | (x,y)::t when x = k -> Some y
    | _::t -> assoc t k
;;

(* SYMBOLIC MANIPULATION OF ARITHMETIC EXPRESSIONS
 * Abstract syntax trees are a convenient way to represent a syntactic expression in a structured way.
 * Let us consider arithmetic expressions formed by the following rules:
 *
 * an integer is an arithmetic expression ;
 * if lhs and rhs are arithmetic expressions then lhs + rhs is an arithmetic expression;
 * if lhs and rhs are arithmetic expressions then lhs * rhs is an arithmetic expression.
 * Such an expression can be represented by a value of type exp as defined in the given prelude (as well as the definition of 1 + 2 * 3 as an example).
 * Write the expression 2 * 2 + 3 * 3 in a variable my_example.
 * Write a function eval : exp -> int that computes the value of an arithmetic expression. The evaluation rules are:
 * If the expression is an integer x, the evaluation is x.
 * If the expression is lhs + rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x + y.
 * If the expression is lhs * rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x * y.
 * If an expression is of the form a * b + a * c then a * (b + c) is a factorized equivalent expression.
 * Write a function factorize : exp -> exp that implements this transformation on its input exp if it has the shape a * b + a * c or does nothing otherwise.
 * Write the reverse transformation of factorize, expand : exp -> exp, which turns an expression of the shape a * (b + c) into a * b + a * c.
 * Implement a function simplify: exp -> exp which takes an expression e and:
 * If e is of the shape e * 0 or 0 * e, returns the expression 0.
 * If e is of the shape e * 1 or 1 * e, returns the expression e.
 * If e is of the shape e + 0 or 0 + e, returns the expression e.
 * and does nothing otherwise.
 * Remarks:
 *
 * The symbols (a, b, c and e) can match any expressions, not just integers.
 * these are a syntactical rewritings, so two expressions are considered equal if and only if they are exactly the same expressions (simply use the = operator to check that).
 * The rewritings have to be done on the first level of the expression only, not recursively and not deeper in the expression. If the toplevel expression does not match the expected pattern, simply return the expression untouched. *)
(* THE GIVEN PRELUDE *)
type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp;;
let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3));;

let my_example =
  (EAdd ((EMul (EInt 2, EInt 2)), (EMul (EInt 3, EInt 3))))
;;
(* YOUR ANSWER *)
let rec eval e = match e with
| EAdd (e1, e2) -> (eval e1) + (eval e2)
| EMul (e1, e2) -> (eval e1) * (eval e2)
| EInt (e1) -> e1
;;

(* a * b + a * c *)
(* let example3 = EAdd (EMul (e1, e2), EMul (e3, e4));; *)
let factorize e = match e with
  | EAdd (EMul (e1,e2), EMul (e3,e4))
    -> if e1 = e3 then
         EMul (e1, EAdd (e2, e4))
       else if e1 = e4 then
         EMul (e1, EAdd (e2, e3))
       else if e2 = e3 then
         EMul (e2, EAdd (e1, e4))
       else if e2 = e4 then
         EMul (e2, EAdd (e1, e3))
       else e
  | _ -> e
;;

let expand e = match e with
  | EMul (e1, EAdd (e2, e3)) -> EAdd (EMul (e1, e2), EMul (e1, e3))
  | _ -> e
;;

let simplify e = match e with
  | EMul (e1, EInt 0) | EMul (EInt 0, e1) -> EInt 0
  | EMul (e1, EInt 1) | EMul (EInt 1, e1) -> e1
  | EAdd (e1, EInt 0) | EAdd (EInt 0, e1) -> e1
  | _ -> e
;;

(* TRIES
 * The data structure called trie is very convenient to represent a dictionary whose keys are strings. It is space-efficient way while providing a very fast lookup function.
 * See the page on Wikipedia.
 * In this exercise, we will implement such a data structure, assuming that we want to associate integers to the strings of the dictionary.
 * Let us define a trie using two mutually defined types (given in the prelude):
 *
 * trie which represents a trie, that is a tree whose root may contain an integer and whose children are indexed by characters ;
 * char_to_children which implements the associative data structure whose keys are characters and whose values are trie (childrens).
 * As a trade-off between speed and memory consumption, we choose an associative list to represent the association between characters and children.
 * The prelude also gives examples of empty trie and of another one that contains the following pairs (key, value):
 * [("A", 15); ("to", 7); ("tea", 3);("ted", 4); ("ten", 12); ("i", 11); ("in", 5); ("inn", 9)].
 *
 * Write a function children_from_char : char_to_children -> char -> trie option such that
 * children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
 * children_from_char m c = None if no such pair exists in m.
 * Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
 * children_from_char (update_children m c t) c = Some t ;
 * children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
 * If children_from_char m c = Some t then List.length (update_children m c t') = List.length m.
 * Write a function lookup : trie -> string -> int option such that lookup trie w = Some i if i is the value of the key w in trie and lookup trie w = None if w is not a key of trie.
 * To look for a key in a trie, iterate over the characters of the key from left to right. Given the current character c and the current node of the trie n, look for the children n for character c. If such a children exists, continue with that trie and the remainder of the key. If no such children exists, the key is not in the trie. When the characters of the key are entirely consumed, look at the root of the current trie. If there is an integer, this is the value you are looking for. If there is no integer, the key not in the trie.
 * Write a function insert : trie -> string -> int -> trie such that lookup (insert trie w k) w = Some k and lookup (insert trie w k) w' = lookup trie w' for w <> w'. *)
(* THE GIVEN PRELUDE *)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])
(* YOUR ANSWER *)
let rec children_from_char (m:char_to_children) (c:char) : trie option =
  match m with
  | [] -> None
  | (ch,tr)::t -> if ch = c then Some tr else children_from_char t c
;;

let update_children (m:char_to_children) (c:char) (t:trie) : char_to_children =
  let rec aux accu m' c t =
    match m' with
    | [] -> accu @ [(c,t)]
    | (ch,tr)::tail -> if ch = c then
                         (List.rev tail) @ [(c,t)] @ accu
                       else
                         aux ((ch,tr)::accu) tail c t
  in
  aux [] (List.rev m) c t
;;

let rec list_of_string string =
  let len = String.length string in
  match string with
  | "" -> []
  | _ -> (String.get string 0)::(list_of_string (String.sub string 1 (len-1)))
;;

let lookup triee (w:string) =
  let rec aux triee w_lis =
    match w_lis with
    | [] -> (match triee with
             | Trie (None,_) -> None
             | Trie (iopt,_) -> iopt
            )
    | h::t -> (let Trie (_,char2chil) = triee in
               let var = children_from_char char2chil h in
               match var with
               | None -> None
               | Some tr -> aux tr t
              )
  in
  aux triee (list_of_string w)
;;

let insert (triee:trie) w v : trie =
  let rec aux trie_opt w_lis v =
    match w_lis,trie_opt with
    | []  ,None    -> Trie (Some v, [])
    | []  ,Some tr -> (let Trie (iopt, ct_list) = tr in
                       Trie (Some v, ct_list)
                      )
    | h::t,None    -> (let tr' = aux None t v in
                       Trie (None, [(h, tr')])
                      )
    | h::t,Some tr -> (let Trie (iopt, ct_list) = tr in
                       let tr' = aux (children_from_char ct_list h) t v
                       in
                       Trie (iopt, (update_children ct_list h tr'))
                      )
  in
  aux (Some triee) (list_of_string w) v
;;

(* TYPE DIRECTED PROGRAMMING
 * In this exercise, you will experiment with type-directed programming.
 *
 * We give you the example program of the lecture in which two type definitions have been changed as in the given prelude. A case Tired has been added to type state, and a case Sleep has been added to type action.
 * By clicking the typecheck button, you can notice that several warnings are issued by the OCaml compiler. Go through the code and fix these warnings as follow.
 *
 * Update apply_action so that the Sleep action turns a character from the Tired state to the Hungry state.
 * Update possible_changes_for_character so that the Tired state behaves as the Hungry state.
 * Update describe_state so that the description of the Tired state is "tired".
 * Update tell_action so that tell_action Sleep is "took a nap". *)
(* THE GIVEN PRELUDE *)
type story = {
  context         : context;
  perturbation    : event;
  adventure       : event list;
  conclusion      : context;
}
and context = { characters : character list }
and character = { name  : string; state : state; location : location }
and event = Change of character * state | Action of character * action
and state = Happy | Hungry | Tired
and action = Eat | GoToRestaurant | Sleep
and location = Appartment | Restaurant

let compatible_actions_for_character character context =
  match character with
  | { location = Restaurant } -> [Eat]
  | { location = Appartment } -> [GoToRestaurant]
;;

let apply_action character = function
  | Eat ->
     { state = Happy;
       location = character.location; name = character.name }
  | GoToRestaurant ->
     { location = Restaurant;
       state = character.state; name = character.name }
  | Sleep ->
     { state = Hungry;
       location = character.location; name = character.name }
;;

let compatible_actions context =
  let rec aux = function
    | [] -> []
    | character :: cs ->
       let can_do = compatible_actions_for_character character context in
       let rec aux' = function
         | [] -> []
         | a :: actions -> Action (character, a) :: aux' actions
       in
       aux' can_do
  in
  aux context.characters
;;

let possible_changes_for_character character =
  match character with
  | { state = Happy } -> [Hungry]
  | { state = Hungry } | { state = Tired }-> []
;;
let apply_change character state =
  { name = character.name; state = state; location = character.location }
;;

let possible_changes context =
  let rec aux = function
    | [] -> []
    | character :: cs ->
       let possible_changes = possible_changes_for_character character in
       let rec aux' = function
         | [] -> []
         | c :: changes -> Change (character, c) :: aux' changes
       in
       aux' possible_changes
  in
  aux context.characters
;;

let character_of_event = function
  | Action (character, _) -> character
  | Change (character, _) -> character
;;

let apply event context =
  let rec aux = function
    | [] -> assert false
    | character :: cs ->
       if character = character_of_event event then
         match event with
         | Action (_, action) -> apply_action character action :: cs
         | Change (_, change) -> apply_change character change :: cs
       else
         character :: aux cs
  in
  { characters = aux context.characters }
;;

let rec is_one_of state states =
  match states with
  | [] -> false
  | state' :: ss -> state = state' || is_one_of state ss
;;

let rec all_characters_are states = function
  | [] ->
     true
  | character :: cs ->
     is_one_of character.state states && all_characters_are states cs
;;

let random_pick xs =
  List.nth xs (Random.int (List.length xs))
;;
let something_happens context =
  let what_can_happen = compatible_actions context @ possible_changes context in
  let event = random_pick what_can_happen in
  event, apply event context
;;

let happy context =
  all_characters_are [Happy] context.characters
;;

let rec end_story events context =
  if happy context then
    context, List.rev events
  else
    let event, context = something_happens context in
    end_story (event :: events) context
;;

let make_story initial_context =
  let perturbation, context = something_happens initial_context in
  let conclusion, adventure = end_story [] context in
  {
    context = initial_context;
    perturbation = perturbation;
    adventure = adventure;
    conclusion = conclusion
  }
;;

let describe_location = function
  | Appartment -> "at home"
  | Restaurant -> "at the restaurant"
;;
let describe_state = function
  | Happy -> "happy"
  | Hungry -> "hungry"
  | Tired -> "tired"
;;
let describe character =
  character.name ^ " was "
  ^ describe_location character.location
  ^ " and was " ^ describe_state character.state ^ ". "
;;

let tell_context context =
  let rec aux = function
    | [] -> ""
    | character :: characters -> describe character ^ aux characters
  in
  aux context.characters
;;

let tell_action = function
  | Eat -> "ate"
  | GoToRestaurant -> "went to the restaurant"
  | Sleep -> "took a nap"
;;

let tell_event = function
  | Action (character, action) ->
      character.name ^ " " ^ tell_action action ^ ". "
  | Change (character, state) ->
      character.name ^ " was made " ^ describe_state state ^ ". "
;;

let rec tell_adventure = function
  | [] -> ""
  | event :: adventure -> tell_event event ^ tell_adventure adventure
;;

let tell story =
  "Once upon a time, "
  ^ tell_context story.context
  ^ "One day, something wrong happened. "
  ^ tell_event story.perturbation
  ^ tell_adventure story.adventure
  ^ "At the end, the peace was restored. "
  ^ tell_context story.conclusion
;;

let story = tell (make_story {
    characters = [
      { name = "Sophie"; location = Appartment; state = Happy };
      { name = "Socrate"; location = Appartment; state = Happy };
    ]
  });;

(* BALANCED BINARY TREES
 * A binary tree t, of the 'a bt type given in the prelude, is either an empty tree, or the root of a tree with a value and two children subtrees.
 *
 * Write a function height : 'a bt -> int that computes the height of a tree.
 * A tree is balanced if, for all internal node n, its two subtrees have the same height. Write a function balanced : 'a bt -> bool that tells if a tree is balanced. *)
(* THE GIVEN PRELUDE *)
type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt
;;
(* YOUR ANSWER *)
let rec height t : int =
  match t with
  | Empty -> 0
  | Node (left, _, right) -> max (1 + height left) (1 + height right)
;;

let rec balanced t : bool =
  match t with
  | Empty -> true
  | Node (Empty, _, right) -> right = Empty
  | Node (left,  _, Empty) -> left = Empty
  | Node (left,  _, right) -> balanced left && balanced right
;;

(* AN IMPLEMENTATION OF LIST WITH AN EFFICIENT CONCATENATION
 * Concatenating two standard OCaml lists takes a time proportional to the length of the first list. In this exercise, we implement a data structure for lists with a constant time concatenation.
 * The preludes gives a type 'a clist, which is either a single element of type 'a, the concatenation of two 'a clist or an empty 'a clist.
 * This representation of a list is not linear: it is a tree-like datastructure since the CApp constructor contains two values of type 'a clist.
 * The sequence of elements contained in a value of type 'a clist is obtained by a depth-first traversal of the tree. For instance, the example given in the prelude, of type int clist is a valid representation for the sequence [1;2;3;4].
 *
 * Write a function to_list : 'a clist -> 'a list which computes the 'a list that contains the same elements as the input 'a clist, in the same order.
 * Write a function of_list : 'a list -> 'a clist which computes the 'a clist that contains the same elements as the input 'a list, in the same order.
 * Write a function append : 'a clist -> 'a clist -> 'a clist such that:
 * append CEmpty l = append l CEmpty = l
 * append l1 l2 = CApp (l1, l2) otherwise
 * Write a function hd : 'a clist -> 'a option that returns Some x where x is the first element of the input 'a clist, if it is not empty, and returns None otherwise.
 * Write a function tl : 'a clist -> 'a clist option that returns Some l where l is the input sequence without its first element, if this input sequence is not empty, or returns None otherwise. *)
(* THE GIVEN PRELUDE *)
type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty
;;

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))
;;
(* YOUR ANSWER *)
let splithalf l =
  let rec aux i a b =
    match b with
    | [] -> [],[]
    | h::t when i=0 -> (List.rev a), b
    | h::t -> aux (i-1) (h::a) t
  in
  aux ((List.length l) / 2) [] l
;;

let rec to_list (l:'a clist) : 'a list =
  match l with
  | CEmpty -> []
  | CSingle a -> [a]
  | CApp (a, b) -> (to_list a) @ (to_list b)
;;

let rec of_list (l:'a list) : 'a clist =
  let a,b = splithalf l in
  match a,b with
  |  [],[]  -> CEmpty
  | [a],[]  -> CApp (CSingle a, CEmpty)
  |   a,[]  -> CApp (of_list a, CEmpty)
  |  [],[b] -> CApp (CEmpty   , CSingle b)
  | [a],[b] -> CApp (CSingle a, CSingle b)
  |   a,[b] -> CApp (of_list a, CSingle b)
  |  [],b   -> CApp (CEmpty   , of_list b)
  | [a],b   -> CApp (CSingle a, of_list b)
  |   a,b   -> CApp (of_list a, of_list b)
;;

let append (l1:'a clist) (l2:'a clist) : 'a clist =
  CApp (l1, l2)
;;

let rec hd (l:'a clist) : 'a option =
  match l with
  | CEmpty -> None
  | CSingle a -> Some a
  | CApp (a,b) -> let head = hd a in
                  if head != None then head else hd b
;;

let rec hd (l:'a clist) : 'a option =
  match l with
  | CEmpty -> None
  | CSingle a -> Some a
  | CApp (CEmpty, b) -> hd b
  | CApp (CSingle a, b) -> Some a
  | CApp (a,b) -> let head_opt = hd a in
                  match head_opt with
                  | None -> hd b
                  | Some head -> Some head
;;

let rec tl (l:'a clist) : 'a clist option =
  match l with
  | CEmpty -> None
  | CSingle a -> Some CEmpty
  | CApp (CEmpty, b) -> tl b
  | CApp (CSingle a, b) -> Some b
  | CApp (a,b) -> let tail_opt = tl a in
                  match tail_opt with
                  | None -> tl b
                  | Some tail -> Some (CApp (tail, b))
;;

(* ADVANCED PATTERNS
 * Let's rewrite some pattern matching with advanced constructs.
 *
 * Factorize the pattern matching of function simplify using or-patterns. It should boil down to three cases.
 * The only_small_lists function takes a list as input and returns this list only if it contains two or less elements, otherwise the empty list is returned. Rewrite this function using or-patterns and an as-pattern. It should boil down to two cases.
 * Turn the third case of no_consecutive_repetition into two distinct cases, dropping the if construct in favor of a when clause. *)
(* THE GIVEN PRELUDE *)
type e = EInt of int | EMul of e * e | EAdd of e * e;;

let simplify = function
  | EMul (EInt 1, e) -> e
  | EMul (e, EInt 1) -> e
  | EMul (EInt 0, e) -> EInt 0
  | EMul (e, EInt 0) -> EInt 0
  | EAdd (EInt 0, e) -> e
  | EAdd (e, EInt 0) -> e
  | e -> e
;;

let simplify = function
  | EMul (e1, EInt 0) | EMul (EInt 0, e1) -> EInt 0
  | EMul (EInt 1, e) | EMul (e, EInt 1) | EAdd (EInt 0, e) | EAdd (e, EInt 0) -> e
  | e -> e
;;

let only_small_lists = function
  | [] -> []
  | [x] -> [x]
  | [x;y] -> [x;y]
  | _ -> []
;;

let only_small_lists = function
  | [_] | [_;_] as z -> z
  | [] | _::_ -> []
;;

let rec no_consecutive_repetition = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: ys ->
      if x = y then
        no_consecutive_repetition (y :: ys)
      else
        x :: (no_consecutive_repetition (y :: ys))
;;

let rec no_consecutive_repetition = function
  | [] | [_] as z -> z
  | x::y::ys when x=y -> no_consecutive_repetition (y :: ys)
  | x::y::ys -> x :: (no_consecutive_repetition (y :: ys))
;;

(* LAMBDA-EXPRESSIONS AS VALUES
 * In this exercise, we will define functions as values, also called lambda expressions, using the keyword function to do a pattern-matching on their argument. As a result, you are not allowed to use match ... with in your code.
 * 
 * Define a function last_element: 'a list -> 'a that returns the last element of a list. Your function may return (invalid_arg "last_element") when the list is empty.
 * Write a function is_sorted: 'a list -> bool that takes a list l as argument, and that checks that the list is sorted in increasing order according to the polymorphic comparison operator <. *)
let rec last_element = function 
  | [] -> invalid_arg "last_element"
  | [x] -> x
  | h::t -> last_element t
;;

let rec is_sorted = function
  | [] -> true
  | [x] -> true
  | x::y::t -> x < y && is_sorted (y::t)
;;

(* USING FIRST CLASS FUNCTIONS
 * Write a function compose : ('a -> 'a) list -> ('a -> 'a) that takes as argument a list l of functions, and that returns the function that is the composition of the functions in l. For instance, compose [f;g;h] x = f (g (h x)). Or with concrete functions, compose [(fun x -> x+1);(fun x -> 3*x);(fun x -> x-1)] 2 = 4.
 * Write a function fixedpoint : (float -> float) -> float -> float -> float that takes a function f of type float -> float and two floating-point arguments start and delta. The function fixedpoint applies repetitively f to the result of its previous application, starting from start, until it reaches a value y where the difference between y and (f y) is smaller than delta. In that case it returns the value of y. For instance, fixedpoint cos 0. 0.001 yields approximately 0.739 (ref). *)
(* THE GIVEN PRELUDE *)
type int_ff = int -> int;;
(* YOUR ANSWER *)
let rec compose = function
  | [] -> fun x -> x
  | [f] -> f
  | h::t -> fun x -> h (compose t x)
;;

let fabs x =
  if x < 0. then (-.x) else x
;;

let rec fixedpoint f start (delta:float) =
  let fy = f start in
  if fabs (fy -. start) < delta then
    start
  else
    fixedpoint f fy delta
;;

(* FUNCTIONS RETURNING FUNCTIONS
 * The following function checks the pairwise equality of the elements of two lists, on the common length of both lists:
 * 
 * let rec equal_on_common l1 l2 = match l1,l2 with
 *   | [],_ -> true
 *   | _,[] -> true
 *   | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2
 * Rewrite equal_on_common : 'a list -> 'a list -> bool by using nested function .. -> constructions. Using the match .. with construction or tuple patterns is forbidden. You can (and must) only call the operators && and =, and the function equal_on_common recursively. *)
let rec equal_on_common l1 l2 = match l1,l2 with
  | [],_ -> true
  | _,[] -> true
  | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2
;;

(* let rec equal_on_common l1 = function
 *   | [] -> true
 *   | h2::t2 -> if l1 = [] then true
 *               else
 *                 let h1::t1 = l1 in
 *                 h1 = h2 && equal_on_common t1 t2
 * ;; *)

(* A SMALL ARITHMETIC INTERPRETER
 * In this exercise, we will write a small program that computes some operations on integers. We will use a small datatype operation that describes all the operations to perform to compute the result. For example, suppose we want to do the following computation:
 * mul (add 0 1) (mul 3 4)
 * We can describe it as: Op ("mul", Op ("add", Value 0, Value 1), Op ("mul", Value 3, Value 4))
 * The Op constructor takes as a first argument a string, which is the name of the function that is stored in an environment. We suppose there exists a variable initial_env that contains some predefined functions.
 *
 * First of all, we need a way to find a function in an environment of type env, which is basically a list of tuples. Each of these tuples contains a string, which is the name of the function, and a value of type int -> int -> int, which is basically a function that takes two arguments of type int and returns an int as a result.
 * Write a function lookup_function : string -> env -> (int -> int -> int) that returns the function associated to a name in an environment. If there is no function with the name given, you can return invalid_arg "lookup_function".
 * Another useful feature would be to add functions to a given environment. Write the function add_function : string -> (int -> int -> int) -> env -> env that takes an environment e, a name for the function n and a function f, and returns a new environment that contains the function f that is associated to the name n.
 *
 * What you can notice now is that unless you put explicit annotations, those two previous functions should be polymorphic and work on any list of couples. Actually, lookup_function could have been written as List.assoc.
 * Create a variable my_env: env that is the initial environment plus a function associated to the name "min" that takes two numbers and returns the lowest. You cannot use the already defined Pervasives.min function, nor any let .. in. Take advantage of lambda expressions!
 * Now that we have correctly defined the operations to use the environment, we can write the function that computes an operation. Write a function compute: env -> operation -> int that takes an environment and an operation description, and computes this operation. The result is either:
 * Directly the value.
 * An operation that takes two computed values and a function from the environment.
 * Let's be a bit more efficient and use the over-application: suppose a function id: 'a -> 'a, then id id will also have type 'a -> 'a, since the 'a is instantiated with 'a -> 'a . Using that principle, we can apply id to itself infinitely since it will always return a function. Write a function compute_eff : env -> operation -> int that takes an environment and an operation, and computes it. However, you cannot use let inside the function! *)
(* THE GIVEN PRELUDE *)
type operation =
  | Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list
(* YOUR ANSWER *)
let rec lookup_function str env =
  match env with
  | [] -> invalid_arg "lookup_function"
  | (k,v)::t when k = str -> v
  | h::t -> lookup_function str t
;;

let add_function name op env =
  (name, op)::env
;;

let my_env =
  [("add", (+));("sub",(-));("mul",( * ));("div", (/));("min",fun x y -> if x < y then x else y)]
;;

let rec compute env op =
  match op with
  | Value int -> int
  | Op (string, x, y) -> (lookup_function string env) (compute env x) (compute env y)
;;

let rec compute_eff env = function 
  | Value int -> int
  | Op (string, x, y) -> (lookup_function string env) (compute_eff env x) (compute_eff env y)
;;

(* USING AND WRITING THE MAP FUNCTION
 * The idea of this exercise is to use the principle of the map function to implement algorithms that transform data structures using higher-order functions.
 * 
 * Using the function map from the module List, write a function wrap : 'a list -> 'a list list that transforms a list of elements 'ainto a list of singleton lists. 
 * For instance, wrap [1;2;3] is equal to [[1];[2];[3]]
 * Consider the definition of the type tree given in the prelude. It represents binary trees carrying data items, on its internal nodes, and on its leaves. 
 * Write a function tree_map : ('a -> 'b) -> 'a tree -> 'b tree such that tree_map f t yields a tree of the same structure as t, but with all its data values x replaced by f x 
 * For example, suppose a function string_of_int : int -> string, that takes an integer and generates the string that represent this integer. Applied to tree_map and a tree of integers (i.e. of type int tree), it would yield a tree of strings (i.e. of type string tree). *)
(* THE GIVEN PRELUDE *)
type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;
(* YOUR ANSWER *)
let wrap l =
  List.map (fun x -> [x]) l
;;

let rec tree_map f = function
  | Leaf a -> Leaf (f a)
  | Node (left, a, right) -> Node ((tree_map f left), (f a), (tree_map f right))
;;

(* USING FOLD TO PRODUCE LISTS
 * The idea of this exercise is to write functions that iterate on lists, using the fold_left and fold_right functions from the List module.
 * 
 * Write a function filter : ('a -> bool) -> 'a list -> 'a list that takes a predicate p (a function returning a boolean) and a list l and returns all the elements of l that satisfy p (for which p returns true).
 * Define, using List.fold_right, a function partition : ('a -> bool) -> 'a list -> 'a list * 'a list that takes a predicate p and a list l, and that partitions l by p. It returns a pair of two lists (lpos,lneg), where lpos is the list of all elements of l that satisfy p, and lneg is the list of all elements that do not satisfy p.
 * One way of sorting a list is as follows:
 * The empty list is already sorted.
 * If the list l has a head h and a rest r, then a sorted version of l can be obtained in three parts:
 * first a sorted version of all elements of r that are smaller or equal to h,
 * then h,
 * then a sorted version of all elements of r that are greater than h.
 * Write a function sort:'a list-> 'a list that implements this algorithm, using the function partition of the previous question. This sorting algorithm is also known as Quicksort where the pivot is always the first element of the list. *)

let filter p l =
  let rec aux accu p l =
    match l with
    | [] -> accu
    | h::t when p h -> aux (h::accu) p t
    | h::t -> aux accu p t
  in
  aux [] p (List.rev l)
;;

let partition p l =
  let f x (lpos,lneg) =
    match p x with
    | true  -> (x::lpos,   lneg)
    | false -> (   lpos,x::lneg)
  in
  List.fold_right f l ([],[])
;;

let rec sort = function
  | [] -> []
  | h::t -> (let lpos,lneg = partition ((>) h) t in
             (sort lpos) @ [h] @ (sort lneg)
            )
;;

(* USING FOLD TO CHECK PREDICATES
 * Using List.fold_left, write a function for_all : ('a -> bool) -> 'a list -> bool. It takes as argument a list l of type 'a list, and a predicate p of type 'a -> bool. It must return true if and only if all elements of l satisfy the predicate p.
 * Using List.fold_left, write a function exists : ('a -> bool) -> 'a list -> bool. It takes as argument a list l of type 'a list, and a predicate p of type 'a -> bool. It must returns true if at least one element of l satisfies the predicate p.
 * Write a function sorted : ('a -> 'a -> int) -> 'a list -> bool, using List.fold_left that checks that a list of elements l of type 'a is sorted, according to an ordering function cmp of type 'a -> 'a -> int. 
 * The ordering function returns:
 * 1 (or any positive number) if the first element is greater than the second,
 * -1 (or any negative number) if the first element is lesser than the second,
 * and 0 otherwise.
 * For the fold_left part, you can use the type 'a option as the accumulator: at each iteration of fold_left, if the list if sorted until now, the acccumulator is either Some v, where v is the previous element, or None otherwise. 
 * Remember, the empty list is sorted, so you can use the list with at least one element to check using fold_left. *)
let for_all p l =
  let f b x =
    b && p x
  in
  List.fold_left f true l
;;

let exists p l =
  let f b x =
    b || p x
  in
  List.fold_left f false l
;;

let sorted cmp l =
  let f opt x =
    match opt with 
    | Some v when (cmp v x <= 0) -> Some x
    | _ -> None
  in
  match l with
  | [] -> true
  | head::tail -> 
      (match List.fold_left f (Some head) l with
       | None -> false
       | Some v -> true
      )
;;

(* UNRAVELING THE AUTOMATIC GRADER
 * In this exercise, we will unveil the basics of the grading system.
 * 
 * Note: This exercise is about exceptions, but it uses the unit type that is only presented in the next sequence.
 * 
 * For each question, we call both your function and a reference function on a list of randomly sampled test cases, and observe the results. We also have to handle the case where a function raises an exception instead of producing a result. Sometimes, we even expect your function to raise an exception, and want to compare it to the exception raised by the reference function. 
 * For this, we use the 'a result type given in the prelude. 
 * Define a function exec: ('a -> 'b) -> 'a -> 'b result, that calls the given function on the given argument, and returns Ok with the result if everything went well, and Error with the exception raised, if there was one.
 * To be able to provide you with the nice error reports, we use an intermediate type for producing reports, similar to the one given in the prelude. 
 * Define a function compare with the following signature.
 * compare : 'a result -> 'a result -> ('a -> string) -> message
 * This function will take first the user function's result and then the reference function's result. It also takes a printer compatible with the return type, to display the results as in one the following cases.
 * ("got correct value 13", Successful)
 * ("got unexpected value 13", Failed)
 * ("got correct exception Exit", Successful)
 * ("got unexpected exception Exit", Failed)
 * You must respect the exact wording for your solution to be accepted. To display exceptions, you can use the provided exn_to_string function.
 * Then we define random samplers for each type of data that will be passed to your function. For a given type 'a, a random sampler simply has type unit -> 'a, an imperative function that returns a new value of type 'a every time you give it a unit. 
 * Define a function test with the following signature.
 * test : ('a -> 'b) -> ('a -> 'b) -> (unit -> 'a) -> ('b -> string) -> report
 * This function must proceed to exactly 10 tests, calling the sampler each time, and return the list of messages. For each sample, you will exec the user function (the first parameter), then exec the reference function, and compare the results. It will then return the list containing the 10 comparison messages. 
 * Your solution must respect the constraint that the first call to the sampler corresponds to the first message of the list, the second to the second, etc. Be cautious about not reversing the list. And since the sampler is an imperative, remember to use let ... in constructs if necessary, to force the order of evaluation. *)
(* THE GIVEN PRELUDE *)
type report = message list
and message = string * status
and status = Successful | Failed
type 'a result = Ok of 'a | Error of exn
(* YOUR ANSWER *)
let exec f x =
  try
    Ok (f x)
  with
    ex -> Error ex
;;

let compare user reference (to_string:'a -> string) =
  match user,reference with
  | Ok a, Ok b when a=b -> "got correct value of " ^ (to_string a), Successful
  | Ok a, _ -> "got unexpected value of " ^ (to_string a), Failed
  | Error a, Error b when a=b -> "got correct exception " ^ (Printexc.to_string a), Successful
  | Error a, _ -> "got unexpected exception " ^ (Printexc.to_string a), Failed
;;

let test user reference sample to_string =
  "Replace this string with your implementation." ;;

(* PRINTING LISTS
 * Write a function print_int_list : int list -> unit that takes a list of integers as input, and prints all the elements of the list, each on its own line.
 * Write a function print_every_other : int -> int list -> unit that takes a value k and a list of integers as input, and prints the elements of the list that are in positions multiple of k, each on its own line. Note: the first element of a list is at the position 0, not 1.
 * Write a function print_list : ('a -> unit) -> 'a list -> unit that takes a printer of values of some type 'a and a list of such values as input, and prints all the elements of the list, each on its own line. *)

let print_every_other k l =
  let rec aux m =
    try
      Printf.printf "This is aux %d\n" m;
      Printf.printf "m is %d, k is %d\n" m k;
      Printf.printf "calling aux m+k = aux %d+%d = aux %d\n" m k (m+k);
      Printf.printf "%d\n" (List.nth l m);
      aux (m+k);
    with
    | Failure a -> ()
    | _ -> failwith "not OOB failure"
  in
  aux k
;;

let rec print_list print l =
  match l with
  | [] -> ()
  | h::t ->
      print h;
      print_newline ();
      print_list print t;
;;

(* DISPLAYING A FILESYSTEM HIERARCHY
 * In this exercise, we will pretty-print directory structures.
 * 
 * The prelude gives the types that we will use to represent directory structures. A node in the filesystem is either a simple File, a Directory that contains a nested filesystem, or a Symlink. 
 * The latter, as on Unix systems, is a fake file that redirects to another file. For this, it provides the relative path to this target file. The path is the list of directory to traverse to get to the target file, followed by the later. If one has to go a directory up, we use the common ".." directory name that represents the parent directory. 
 * A filesystem is a list of named nodes. An example filesystem is given below, in the format that you will have to produce. Don't worry, we'll break this piece by piece.
 * 
 *   /photos
 *   | /march
 *   | | photo_1.bmp
 *   | | photo_2.bmp
 *   | | photo_3.bmp
 *   | | index.html
 *   | /april
 *   | | photo_1.bmp
 *   | | photo_2.bmp
 *   | | index.html
 *   /videos
 *   | video1.avi
 *   | video2.avi
 *   | video3.avi
 *   | video4.avi
 *   | best.avi -> video4.avi
 *   | index.html
 *   /indexes
 *   | videos.html -> ../videos/index.html
 *   | photos_march.html -> ../photos/march/index.html
 *   | photos_april.html -> ../photos/april/index.html
 *   | photos_may.html -> INVALID
 * This output was generated from the following OCaml structure.
 * 
 * [ "photos", Dir
 *     [ "march", Dir
 *         [ "photo_1.bmp", File ;
 *           "photo_2.bmp", File ;
 *           "photo_3.bmp", File ;
 *           "index.html", File ] ;
 *       "april", Dir
 *         [ "photo_1.bmp", File ;
 *           "photo_2.bmp", File ;
 *           "index.html", File ] ] ;
 *   "videos", Dir
 *     [ "video1.avi", File ;
 *       "video2.avi", File ;
 *       "video3.avi", File ;
 *       "video4.avi", File ;
 *       "best.avi", Symlink [ "video4.avi" ] ;
 *       "index.html", File ] ;
 *   "indexes", Dir
 *     [ "videos.html",
 *       Symlink [ ".." ; "videos" ; "index.html" ] ;
 *       "photos_march.html",
 *       Symlink [ ".." ; "photos" ; "march" ; "index.html" ] ;
 *       "photos_april.html",
 *       Symlink [ ".." ; "photos" ; "april" ; "index.html" ] ;
 *       "photos_may.html",
 *       Symlink [ ".." ; "photos" ; "may" ; "index.html" ] ] ]
 * Write a function print_path: string list -> unit that prints a relative path (the argument of a Symlink) and pretty prints it as shown in the example display, using slashes ('/') as separator.
 * As you can see in the example, the depth of a file in the filesystem (the number of nested folders that are its ancestors) is represented by a sequence of vertical lines. Write a function print_file: int -> string -> unit that prints a file name, with the given number of "| " in front of it.
 * Write a similar function print_symlink: int -> string -> string list -> unit that prints the link name, with the given number of "| " in front of it, and the relative path (preceded by an arrow " -> ").
 * Write a similar function print_dir: int -> string -> unit that prints the dir name, with the given number of "| " in front of it, and the prepended '/'.
 * Write a function print_filesystem: filesystem -> unit that traverses the filesystem, producing the same display as in the example. You will probably need an auxiliary, recursive function, and you will have to use the previous answers.
 * Write a function resolve: string list -> string list -> string list. It takes as parameters:
 * The full path from the root to a symlink, including its name. In the given example, that could be for instance [ "indexes" ; "photos_april.html" ].
 * The relative path for this symlink. Here, that would be [ ".." ; "photos" ; "april" ; "index.html" ].
 * The function returns the full path from the root to the target of the symlink. Here, we should get [ "photos" ; "april" ; "index.html" ]. Note that it may not be as easy as it seems, so you may think about it before plunging into the code.
 * Write a function file_exists : filesystem -> string list -> bool that tells if a file exists in the filesystem. The path is the full absolute path to the file, and the target must be a File, not a Dir or a Symlink.
 * Update your function print_filesystem: filesystem -> unit so that it replaces the printed relative path by "INVALID" when the symlink cannot be resolved to an existing file. *)
(* THE GIVEN PRELUDE *)
type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list

let rec print_path (path:string list) =
  match path with
  | [h] -> 
     print_string h;
  | h::t -> 
     print_string h;
     print_string "/";
     print_path t;
  | [] -> failwith "print_path error"
;;

let rec print_file lvl name =
  if lvl = 0 then
    print_string name
  else
    (print_string "| ";
     print_file (lvl-1) name;
    )
;;

let rec print_symlink lvl name path =
  if lvl = 0 then
    (print_string name;
     print_string " -> ";
     print_path path;
    )
  else
    (print_string "| ";
     print_symlink (lvl-1) name path;
    )
;;

let rec print_dir lvl name =
  if lvl = 0 then
    (print_string "/";
     print_string name;
    )
  else
    (print_string "| ";
     print_dir (lvl-1) name;
    )
;;

let print_filesystem (root:filesystem) = 
  let rec print_filesystem lvl items =
    match items with
    | [] -> ()
    | h::t -> (let string,node = h in
               match node with
               | File -> (print_file lvl string;
                          print_newline ();
                          print_filesystem lvl t;
                         )
               | Dir d -> (print_dir lvl string;
                           print_newline ();
                           print_filesystem (lvl+1) d;
                           print_filesystem lvl t;
                          )
               | Symlink l -> (print_symlink lvl string l;
                               print_newline ();
                               print_filesystem lvl t;
                              )
              )
  in
  print_filesystem 0 root
;;

let rec resolve sym path = 
  let rec resolve acc path =
    match acc,path with
    | acc,[] -> List.rev acc
    | [],h2::t2 when h2 = ".."-> resolve [] t2
    | h1::t1,h2::t2 when h2 = ".."-> resolve t1 t2
    | acc,h2::t2 -> resolve (h2::acc) t2
  in
  resolve (List.tl (List.rev sym)) path
;;

exception Found;;
exception NotFound;;

let rec file_exists root path =
  let rec findnode filesystem string =
    match (filesystem:filesystem) with
    | [] -> None
    | (str,nod)::t when str = string -> Some nod
    | (str,nod)::t -> findnode t string
  in
  try
    match (root:filesystem),(path:string list) with
    | root,[] -> raise Found
    | [],_ -> false
    | h1::t1 as root,h2::t2 -> (let node = findnode root h2 in
                                match node with
                                | None -> raise NotFound
                                | Some nod -> file_exists nod t2
                               )
    | _ -> false
  with
  | Found -> true
  | NotFound -> false
;;
