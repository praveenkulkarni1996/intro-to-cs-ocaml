(* Author: Praveen Kulkarni
 * Date: 13 March 2018
 * File: fizzbuzz.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* fizzbuzz : int -> string *)
let fizzbuzz index = 
    if index mod 15 = 0 then "Fizzbuzz"
    else if index mod 3 = 0 then "Fizz"
    else if index mod 5 = 0 then "Buzz" 
    else (string_of_int index);;

(* tests *)
let _ = fizzbuzz 17;;
let _ = fizzbuzz 18;;
let _ = fizzbuzz 20;;
let _ = fizzbuzz 30;;

(* fizzbuzz_string : int -> string *)
let rec fizzbuzz_string index = 
    if index <= 0 then ""
    else if index = 1 then "1"
    else (fizzbuzz_string (index-1)) ^ " " ^ (fizzbuzz index);;

(* tests *)
let _ = fizzbuzz_string 4;;
let _ = fizzbuzz_string 10;;
let _ = fizzbuzz_string 15;;
