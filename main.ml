let rec last list =
  match list with
    [] -> None
  | first::[] -> Some(first)
  | _::rest -> last(rest);;

let rec last_two list =
  match list with
    [] | [_] -> None
  | [x;  y] -> Some(x, y)
  | _::rest -> last_two(rest);;

last [];;
last [1];;
last [1; 2];;
last [1; 2; 3];;
last_two [];;
last_two [1];;
last_two [1; 2];;
last_two [1; 2; 3]
