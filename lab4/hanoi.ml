(* Author: Praveen Kulkarni
 * Date: 14 March 2018
 * File: hanoi.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* hanoi1 : int -> int -> int -> int -> string *)
let rec hanoi1 (n) (src) (dst) (via) = 
    let motion = "(" ^ (string_of_int src) ^ ", " ^ (string_of_int dst) ^ ")\n" in
        if n = 1 then motion
        else
            let prefix = hanoi1 (n-1) (src) (via) (dst) in
            let suffix = hanoi1 (n-1) (via) (dst) (src) in
            prefix ^ motion ^ suffix;;

(* hanoi : int -> string *)
let hanoi n = hanoi1 n 1 3 2;;

(* tests *)
print_string(hanoi 2);;
print_string(hanoi 3);;
