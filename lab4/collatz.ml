(* Author: Praveen Kulkarni
 * Date: 14 March 2018
 * File: collatz.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* collatz : int -> int *)
let rec collatz x =
    if x = 1 then 0
    else if x mod 2 = 0 then 1 + collatz (x/2)
    else 1 + collatz (3*x + 1);;

(* test cases *)
let _ = collatz 1;;
let _ = collatz 12;;
let _ = collatz 19;;
let _ = collatz 27;;

(* 
 * bestx is the value of x such that collatz x is the largest
 * amongst all the values of x that we have seen so far. maxlen
 * is the corresponding value of collatz x.
 * Function tries out the x's in a descending order. 
 *)
(* f : int -> int -> int -> int *)
let rec f (x) (bestx) (maxlen) = 
    if x = 0 then bestx
    else
        let currlen = collatz x in
            if currlen > maxlen then f (x-1) (x) (currlen)
            else f (x-1) (bestx) (maxlen);;

(* max_collatz : int -> int *)
let max_collatz x =
    if x = 1 then 1
    else (f (x) (x) (collatz x));;

(* test cases *)
let _ = max_collatz 10;;
let _ = max_collatz 100;;
