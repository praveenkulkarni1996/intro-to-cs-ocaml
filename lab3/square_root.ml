(* Author: Praveen Kulkarni
 * Date: 13 March 2018
 * File: square_root.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* newton: float -> float -> float *)
let rec newton a x1 times = 
    if times = 0 then x1
    else 
        let x2 = (x1 +. (a /. x1)) /. 2.0
        in (newton a x2 (times-1));;

(* square_root : float -> int -> float *)
let square_root num steps =
    if steps <= 0 then (newton num 1.0 20)
    else (newton num 1.0 steps);;

(* tests *)
let _ = square_root 4.0 2;;
let _ = square_root 4.0 0;;
