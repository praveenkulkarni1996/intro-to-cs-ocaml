(* Author: Praveen Kulkarni
 * Date: 14 March 2018
 * File: josephus.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* This is a difficult recursion problem. Find a discussion of the 
 * problem here : 
 * https://en.wikipedia.org/wiki/Josephus_problem#The_general_case
 *)

(* Assume that the starting position is 1 *)
(* josephus1 : int -> int -> int *)
let rec josephus1 (n) (k) = 
    if n = 1 then 1 (* only one person remains, so he survives *)
    else (((josephus1 (n-1) k) + k - 1) mod n) + 1;;

(* josephus: int -> int -> int -> int *)
let josephus (n) (k) (start) = 
    let position1 = josephus1 (n) (k) in
    ((position1 - 1 + (start - 1)) mod n)+ 1;;

let _ = josephus 5 3 1;;
let _ = josephus 7 4 2;;
