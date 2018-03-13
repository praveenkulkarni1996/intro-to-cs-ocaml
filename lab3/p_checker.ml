(* Author: Praveen Kulkarni
 * Date: 13 March 2018
 * File: p_checker.ml
 * All rights reserved. Copyright (c) 2018
 *)

(* EDITORIAL
 * ==========
 * There are two functions in this file - check_prime and isPrime. 
 *
 * isPrime: int -> bool
 * A number `num` is NOT prime if and only if there is a number x, such that 
 * 2 <= x <= sqrt(num).
 * If num is less than 2 then it is not prime, so isPrime returns false. 
 * If num is equal to 2, then it is prime, so isPrime returns true.
 * If num is greater than 2, then we have to search for an x from 2 to sqrt(x).
 *
 * For that we have built a recursive function `check_prime`.
 *
 * To understand recursion, you must use inductive proofs.
 * =========================================================
 * Intuition:
 * Assume that a number `num` is not divisible by 2, 3, ...., x-1.
 * Then three cases can happen: - 
 * 1.   If x * x > num, then num is PRIME. Because no number from 2 ...x 
 *      divided x, then no number in the range x+1 ... num-1 can possibly 
 *      divide num.
 * 2.   If x divides num, then num is NOT PRIME, by definition. Again we get a 
 *      result, so we can stop.
 * 3.   If x doesn't divide num, then we can now strengthen our assumption that
 *      `num` is not divisible by 2, 3, .... x-1, x.
 *
 * We can start with the assumption that the num > 2. We should start with x=2,
 * and apply the procedure above. If we repeat it enough number of times, then 
 * we will come to an answer whether num is prime or not.
 *
 * This is exactly what the `check_prime` function does.
 * When `check_prime num x` is called, then we have some guarantees, 
 * 1.   num is an integer > 2.
 * 2.   num is not divisible by any integer y such that 1 < y < x.
 *
 * From the intuition above, you should be able to write an inductive proof, why 
 * `check_prime num 2` works. 
 *)

(*  check_prime : int -> int -> bool *)
let rec check_prime num x = 
    if (x * x > num) then true
    else if num mod x = 0 then false
    else (check_prime num (x+1));;

(* isPrime: int -> bool *)
let isPrime num =
    if num <= 1 then false
    else if num = 2 then true
    else (check_prime num 2);;

let _ = isPrime 0;;
let _ = isPrime 1;;
let _ = isPrime 2;;
let _ = isPrime 3;;
let _ = isPrime 4;;
let _ = isPrime 25;;
let _ = isPrime 97;;
