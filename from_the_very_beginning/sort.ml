let rec insert x lst =
  match lst with
  | [] -> [ x ]
  | h :: t -> if x <= h then x :: lst else h :: insert x t

let rec sort lst = match lst with [] -> lst | h :: t -> insert h (sort t);;

sort [ 53; 9; 2; 6; 19 ] = [ 2; 6; 9; 19; 53 ]
