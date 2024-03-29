let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: rest -> last rest
;;

last [] = None;;
last [ 1 ] = Some 1;;
last [ 1; 2 ] = Some 2;;
last [ 1; 2; 3 ] = Some 3

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: rest -> last_two rest
;;

last_two [] = None;;
last_two [ 1 ] = None;;
last_two [ 1; 2 ] = Some (1, 2);;
last_two [ 1; 2; 3 ] = Some (2, 3)

let rec at lst n =
  match (lst, n) with
  | [], _ -> None
  | first :: _, 0 -> Some first
  | _ :: rest, _ -> at rest (n - 1)
;;

at [] 0 = None;;
at [ 1 ] 0 = Some 1;;
at [ 1 ] 1 = None;;
at [ 1; 2 ] 0 = Some 1;;
at [ 1; 2 ] 1 = Some 2;;
at [ 1; 2 ] 2 = None;;
at [ 1; 2; 3 ] 0 = Some 1;;
at [ 1; 2; 3 ] 1 = Some 2;;
at [ 1; 2; 3 ] 2 = Some 3;;
at [ 1; 2; 3 ] 3 = None

let rec length ?(acc = 0) = function
  | [] -> acc
  | _ :: rest -> length ~acc:(acc + 1) rest
;;

length [] = 0;;
length [ 1 ] = 1;;
length [ 1; 2 ] = 2;;
length [ 1; 2; 3 ] = 3

let rec reverse ?(acc = []) = function
  | [] -> acc
  | first :: rest -> reverse ~acc:(first :: acc) rest
;;

reverse [] = [];;
reverse [ 1 ] = [ 1 ];;
reverse [ 1; 2 ] = [ 2; 1 ];;
reverse [ 1; 2; 3 ] = [ 3; 2; 1 ]
