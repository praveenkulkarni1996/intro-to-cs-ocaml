(* Author: Praveen Kulkarni
 * Date: 13 March 2018
 * File: leap_year.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* isLeapYear: int -> bool *)
let isLeapYear year = 
    if (year mod 4 != 0) then false 
    else if (year mod 100 != 0) then true 
    else (year mod 400 = 0);;

(* testcases *)
let _ = isLeapYear 2004;;
let _ = isLeapYear 2016;;
let _ = isLeapYear 2000;;
let _ = isLeapYear 2017;;
let _ = isLeapYear 2018;;
let _ = isLeapYear 1900;;
