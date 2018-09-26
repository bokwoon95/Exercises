(* gcd that takes two non-negative integers n and m, and that returns the greatest common divisor of n and m, following Euclid's algorithm. *)
let rec gcd n m =
  if n = 0 then m else
  if m = 0 then n else
  if n>m then gcd m (n mod m) else
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
  if date.minute+1 <= 1 then cons_date a b c d (e+1) else
  if date.hour+1 <= 2   then cons_date a b c (d+1) 0 else
  if date.day+1 <= 4    then cons_date a b (c+1) 0 0 else
  if date.month+1 <= 5  then cons_date a (b+1) 1 0 0 else
                             cons_date (a+1) 1 1 0 0;
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

let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
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

let delete db contact =
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

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;

