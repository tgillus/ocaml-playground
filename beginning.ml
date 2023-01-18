let mult_by_ten x = x * 10
let both_non_zero x y = x <> 0 && y <> 0
let rec sum = function 0 -> 0 | n -> n + sum (n - 1)
let rec power x = function 0 -> 1 | n -> x * power x (n - 1)

let is_consonant = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let is_lower = function 'a' .. 'z' -> true | _ -> false
let is_upper = function 'A' .. 'Z' -> true | _ -> false;;

mult_by_ten 10 = 100;;
mult_by_ten 5 = 50;;
mult_by_ten 23 = 230;;
mult_by_ten 47389 = 473890;;
both_non_zero 0 0 = false;;
both_non_zero 1 0 = false;;
both_non_zero 0 1 = false;;
both_non_zero 1 1 = true;;
sum 0 = 0;;
sum 1 = 1;;
sum 2 = 3;;
sum 3 = 6;;
power 2 0 = 1;;
power 2 1 = 2;;
power 2 2 = 4;;
power 2 3 = 8;;
is_consonant '1' = false;;
is_consonant '%' = false;;
is_consonant 'b' = false;;
is_consonant 'a' = true;;
is_lower '1' = false;;
is_lower '%' = false;;
is_lower 'T' = false;;
is_lower 't' = true;;
is_upper '1' = false;;
is_upper '%' = false;;
is_upper 't' = false;;
is_upper 'T' = true
