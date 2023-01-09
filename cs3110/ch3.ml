let xs = [ 1; 2; 3; 4; 5 ]
let ys = [ 1; 2; 3; 4; 5 ]
let zs = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]
let rec product = function [] -> 1 | first :: rest -> first * product rest
let rec concat = function [] -> "" | first :: rest -> first ^ concat rest
let bigred_first = function "bigred" :: _ -> true | _ -> false
let two_or_four = function [ _; _ ] | [ _; _; _; _ ] -> true | _ -> false
let first_two_equal = function x :: y :: _ -> x = y | _ -> false
let fifth lst = if List.length lst < 5 then 0 else List.nth lst 4
let sort_rev lst = lst |> List.sort compare |> List.rev

let last = function
  | [] -> None
  | lst -> Some (List.nth lst (List.length lst - 1))

let any_zeroes = List.exists (fun x -> x = 0)

let rec take n lst =
  if n = 0 then []
  else
    match lst with [] -> [] | first :: rest -> first :: take (n - 1) rest

let rec drop n lst =
  if n = 0 then lst
  else match lst with [] -> lst | _ :: rest -> drop (n - 1) rest

let rec take' ?(acc = []) n lst =
  if n = 0 then acc
  else
    match lst with
    | [] -> acc
    | first :: rest -> take' ~acc:(acc @ [ first ]) (n - 1) rest

let rec print_int_list lst =
  match lst with
  | [] -> ()
  | first :: rest ->
      Printf.printf "%i\n" first;
      print_int_list rest

let print_int_list' = List.iter (Printf.printf "%i\n")

type student = { first_name : string; last_name : string; gpa : float }

let s = { first_name = "Tramaine"; last_name = "Gillus"; gpa = 3.7 }
let full_name student = (student.first_name, student.last_name)
let create_student first_name last_name gpa = { first_name; last_name; gpa }

type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 79; ptype = Fire }
let ember = { name = "bubble"; hp = 40; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }
let bubble = { name = "bubble"; hp = 40; ptype = Water }
let facade = { name = "facade"; hp = 70; ptype = Normal }
let covet = { name = "covet"; hp = 60; ptype = Normal }
let safe_hd = function [] -> None | first :: _ -> Some first
let safe_tl = function [] -> None | _ :: rest -> Some rest

let rec max_hp = function
  | [] -> None
  | x :: rest -> (
      match max_hp rest with
      | None -> Some x
      | Some y -> Some (if x.hp > y.hp then x else y))

let date1 = (1981, 3, 30)
let date2 = (1986, 3, 26)
let date3 = (2000, 8, 9)

let is_before (year1, month1, day1) (year2, month2, day2) =
  year1 < year2
  || (year1 = year2 && month1 < month2)
  || (year1 = year2 && month1 = month2 && day1 < day2)

let rec earliest_date = function
  | [] -> None
  | x :: rest -> (
      match earliest_date rest with
      | None -> Some x
      | Some y -> Some (if is_before x y then x else y))
;;

product [] = 1;;
product [ 1 ] = 1;;
product [ 1; 2 ] = 2;;
product [ 1; 2; 3 ] = 6;;
product [ 1; 2; 3; 4 ] = 24;;
product [ 1; 2; 3; 4; 5 ] = 120;;
concat [] = "";;
concat [ "foo" ] = "foo";;
concat [ "foo"; "bar" ] = "foobar";;
concat [ "foo"; "bar"; "baz" ] = "foobarbaz";;
bigred_first [] = false;;
bigred_first [ "foo" ] = false;;
bigred_first [ "foo"; "bigred" ] = false;;
bigred_first [ "foo"; "bar"; "bigred" ] = false;;
bigred_first [ "bigred" ] = true;;
bigred_first [ "bigred"; "foo" ] = true;;
bigred_first [ "bigred"; "foo"; "bar" ] = true;;
two_or_four [] = false;;
two_or_four [ 1 ] = false;;
two_or_four [ 1; 2 ] = true;;
two_or_four [ 1; 2; 3 ] = false;;
two_or_four [ 1; 2; 3; 4 ] = true;;
two_or_four [ 1; 2; 3; 4; 5 ] = false;;
first_two_equal [] = false;;
first_two_equal [ 1 ] = false;;
first_two_equal [ 1; 2 ] = false;;
first_two_equal [ 1; 2; 3 ] = false;;
first_two_equal [ 1; 2; 3; 4 ] = false;;
first_two_equal [ 1; 1 ] = true;;
first_two_equal [ 1; 1; 3 ] = true;;
first_two_equal [ 1; 1; 3; 4 ] = true;;
fifth [] = 0;;
fifth [ 1 ] = 0;;
fifth [ 1; 2 ] = 0;;
fifth [ 1; 2; 3 ] = 0;;
fifth [ 1; 2; 3; 4 ] = 0;;
fifth [ 1; 2; 3; 4; 5 ] = 5;;
fifth [ 1; 2; 3; 4; 5; 6 ] = 5;;
fifth [ 1; 2; 3; 4; 5; 6; 7 ] = 5;;
sort_rev [] = [];;
sort_rev [ 1; 2 ] = [ 2; 1 ];;
sort_rev [ 2; 1; 3 ] = [ 3; 2; 1 ];;
sort_rev [ 2; 4; 1; 3 ] = [ 4; 3; 2; 1 ];;
last [] = None;;
last [ 1 ] = Some 1;;
last [ 1; 2 ] = Some 2;;
last [ 1; 2; 3 ] = Some 3;;
any_zeroes [] = false;;
any_zeroes [ 1 ] = false;;
any_zeroes [ 0 ] = true;;
any_zeroes [ 0; 0 ] = true;;
any_zeroes [ 0; 1 ] = true;;
any_zeroes [ 1; 0 ] = true;;
any_zeroes [ 1; 0; 2 ] = true;;
take 3 [] = [];;
take 3 [ 0 ] = [ 0 ];;
take 3 [ 0; 1 ] = [ 0; 1 ];;
take 3 [ 0; 1; 2 ] = [ 0; 1; 2 ];;
take 3 [ 0; 1; 2; 3 ] = [ 0; 1; 2 ];;
drop 3 [] = [];;
drop 3 [ 0 ] = [];;
drop 3 [ 0; 1 ] = [];;
drop 3 [ 0; 1; 2 ] = [];;
drop 3 [ 0; 1; 2; 3 ] = [ 3 ];;
drop 3 [ 0; 1; 2; 3; 4 ] = [ 3; 4 ];;
take' 3 [] = [];;
take' 3 [ 0 ] = [ 0 ];;
take' 3 [ 0; 1 ] = [ 0; 1 ];;
take' 3 [ 0; 1; 2 ] = [ 0; 1; 2 ];;
take' 3 [ 0; 1; 2; 3 ] = [ 0; 1; 2 ];;

print_int_list [];
print_int_list [ 1 ];
print_int_list [ 1; 2 ];
print_int_list [ 1; 2; 3 ];
print_int_list' [];
print_int_list' [ 1 ];
print_int_list' [ 1; 2 ];
print_int_list' [ 1; 2; 3 ];
full_name s
;;

create_student "Jane" "Doe" 3.2;;
safe_hd [] = None;;
safe_hd [ 1 ] = Some 1;;
safe_hd [ 1; 2 ] = Some 1;;
safe_hd [ 1; 2; 3 ] = Some 1;;
safe_tl [] = None;;
safe_tl [ 1 ] = Some [];;
safe_tl [ 1; 2 ] = Some [ 2 ];;
safe_tl [ 1; 2; 3 ] = Some [ 2; 3 ];;
max_hp [] = None;;
max_hp [ charizard ] = Some charizard;;
max_hp [ charizard; ember ] = Some charizard;;
max_hp [ ember; charizard ] = Some charizard;;
max_hp [ ember; facade; bubble ] = Some facade;;
max_hp [ ember; bubble; facade ] = Some facade;;
is_before date1 date2 = true;;
is_before date1 date1 = false;;
is_before date3 date2 = false;;
earliest_date [] = None;;
earliest_date [ date1 ] = Some date1;;
earliest_date [ date1 ] = Some date1;;
earliest_date [ date1; date2 ] = Some date1;;
earliest_date [ date2; date1 ] = Some date1;;
earliest_date [ date2; date3 ] = Some date2;;
earliest_date [ date3; date2 ] = Some date2;;
earliest_date [ date1; date2; date3 ] = Some date1;;
earliest_date [ date2; date1; date3 ] = Some date1;;
earliest_date [ date1; date3; date2 ] = Some date1;;
earliest_date [ date3; date2; date1 ] = Some date1
