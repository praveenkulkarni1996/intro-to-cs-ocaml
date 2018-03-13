(* Author: Praveen Kulkarni
 * Date: 13 March 2018
 * File: middle_child.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* middleChild : int -> int -> int -> bool *)
let middleChild x y z = 
    if (x < 0 || y < 0 || z < 0) then -3
    else if (x = y && y = z) then -2
    else if (x = y || y = z || x = z) then -1
    else (x + y + z) - (max (max x y) z) - (min (min x y) z);;

(* test cases *)
let _ = middleChild 17 12 15;;
let _ = middleChild 3 3 5;;
let _ = middleChild 12 12 12;;

(* print_middle_child : int -> int -> int -> string *)
let print_middle_child x y z =
    let middle_child_value = middleChild x y z
    in
        if middle_child_value = -3 then "Invalid inputs!"
        else if middle_child_value = -2 then "There are triplets"
        else if middle_child_value = -1 then "There are twins!"
        else "The age of the middle child is:" ^ (string_of_int middle_child_value);;

(* test cases *)
let _ = print_middle_child 17 12 15;;
let _ = print_middle_child 3 3 5;;
let _ = print_middle_child 12 12 12;;
let _ = print_middle_child (-1) 12 12;;

(* Notice that when negative numbers are passed as arguments you 
 * should put paranthesis around them to avoid ambiguity (line 32)
 *)
