(* Author: Praveen Kulkarni
 * Date: 14 March 2018
 * File: hanoi.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* The three rods are called the 
 * `src` : source rod
 * `dst` : destination rod
 * `aux` : auxiliary rod
 *
 * The logic of the solution is explained here:
 * https://en.wikipedia.org/wiki/Tower_of_Hanoi
 *)

(* hanoi1 : int -> int -> int -> int -> string *)
let rec hanoi1 (n) (src) (dst) (aux) = 
    let motion = "(" ^ (string_of_int src) ^ ", " ^ (string_of_int dst) ^ ")\n" in
        if n = 1 then motion
        else
            let prefix = hanoi1 (n-1) (src) (aux) (dst) in
            let suffix = hanoi1 (n-1) (aux) (dst) (src) in
            prefix ^ motion ^ suffix;;

(* hanoi : int -> string *)
let hanoi n = hanoi1 n 1 3 2;;

(* test cases *)
print_string(hanoi 2);;
print_string(hanoi 3);;
