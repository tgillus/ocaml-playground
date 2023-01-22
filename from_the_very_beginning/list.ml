let is_nil = function [] -> true | _ -> false
let rec length = function [] -> 0 | _ :: t -> 1 + length t

let length' lst =
  let rec helper lst acc =
    match lst with [] -> acc | _ :: t -> helper t acc + 1
  in
  helper lst 0

let rec sum = function [] -> 0 | h :: t -> h + sum t

let rec sum' lst =
  let rec helper lst acc =
    match lst with [] -> 0 | h :: t -> helper t acc + h
  in
  helper lst 0

let rec append xs ys = match xs with [] -> ys | h :: t -> h :: append t ys
let rec rev lst = match lst with [] -> lst | h :: t -> rev t @ [ h ]

let rev' lst =
  let rec helper lst acc =
    match lst with [] -> acc | h :: t -> helper t (h :: acc)
  in
  helper lst []

let rec evens = function [] | [ _ ] -> [] | _ :: x :: t -> x :: evens t

let rec count_true = function
  | [] -> 0
  | false :: t -> count_true t
  | true :: t -> 1 + count_true t

let rec count_true' = function
  | [] -> 0
  | h :: t -> (if h then 1 else 0) + count_true t

let count_true'' lst =
  let rec helper lst acc =
    match lst with [] -> acc | h :: t -> helper t acc + if h then 1 else 0
  in
  helper lst 0

let rec drop_last = function [] | [ _ ] -> [] | h :: t -> h :: drop_last t

let drop_last' lst =
  let rec helper lst acc =
    match lst with [] | [ _ ] -> acc | h :: t -> helper t (acc @ [ h ])
  in
  helper lst []

let rec member x = function
  | [] -> false
  | h :: t -> if h = x then true else member x t

let make_set lst =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h :: t -> if member h acc then helper t acc else helper t (acc @ [ h ])
  in
  helper lst []
;;

is_nil [] = true;;
is_nil [ 1 ] = false;;
is_nil [ 1; 2 ] = false;;
is_nil [ 1; 2; 3 ] = false;;
length [] = 0;;
length [ 1 ] = 1;;
length [ 1; 2 ] = 2;;
length [ 1; 2; 3 ] = 3;;
length' [] = 0;;
length' [ 'a' ] = 1;;
length' [ 'a'; 'b' ] = 2;;
length' [ 'a'; 'b'; 'c' ] = 3;;
sum [] = 0;;
sum [ 1 ] = 1;;
sum [ 1; 2 ] = 3;;
sum [ 1; 2; 3 ] = 6;;
sum' [] = 0;;
sum' [ 1 ] = 1;;
sum' [ 1; 2 ] = 3;;
sum' [ 1; 2; 3 ] = 6;;
append [] [] = [];;
append [ 1 ] [] = [ 1 ];;
append [ 1; 2 ] [] = [ 1; 2 ];;
append [ 1; 2 ] [ 3 ] = [ 1; 2; 3 ];;
append [ 1; 2 ] [ 3; 4 ] = [ 1; 2; 3; 4 ];;
append [ 1; 2 ] [ 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ];;
rev [] = [];;
rev [ 1 ] = [ 1 ];;
rev [ 1; 2 ] = [ 2; 1 ];;
rev [ 1; 2; 3 ] = [ 3; 2; 1 ];;
rev' [] = [];;
rev' [ 1 ] = [ 1 ];;
rev' [ 1; 2 ] = [ 2; 1 ];;
rev' [ 1; 2; 3 ] = [ 3; 2; 1 ];;
evens [] = [];;
evens [ 1 ] = [];;
evens [ 1; 2 ] = [ 2 ];;
evens [ 1; 2; 3 ] = [ 2 ];;
evens [ 1; 2; 3; 4 ] = [ 2; 4 ];;
count_true [] = 0;;
count_true [ false ] = 0;;
count_true [ false; true ] = 1;;
count_true [ false; true; false ] = 1;;
count_true [ false; true; false; true ] = 2;;
count_true' [] = 0;;
count_true' [ false ] = 0;;
count_true' [ false; true ] = 1;;
count_true' [ false; true; false ] = 1;;
count_true' [ false; true; false; true ] = 2;;
count_true'' [] = 0;;
count_true'' [ false ] = 0;;
count_true'' [ false; true ] = 1;;
count_true'' [ false; true; false ] = 1;;
count_true'' [ false; true; false; true ] = 2;;
drop_last [] = [];;
drop_last [ 1 ] = [];;
drop_last [ 1; 2 ] = [ 1 ];;
drop_last [ 1; 2; 3 ] = [ 1; 2 ];;
drop_last' [] = [];;
drop_last' [ 1 ] = [];;
drop_last' [ 1; 2 ] = [ 1 ];;
drop_last' [ 1; 2; 3 ] = [ 1; 2 ];;
member 2 [] = false;;
member 2 [ 1 ] = false;;
member 2 [ 1; 2 ] = true;;
member 2 [ 1; 2; 3 ] = true;;
make_set [];;
make_set [ 1 ];;
make_set [ 1; 2 ];;
make_set [ 1; 1; 2 ];;
make_set [ 1; 2; 1 ];;
make_set [ 1; 2; 3; 2; 4; 1 ]
