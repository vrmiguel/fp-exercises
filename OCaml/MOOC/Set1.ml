(* I'm making this on sketch.sh
Link: https://sketch.sh/s/59Lqr7NgfV5zJRWWbK20rO/
 *)

(* Ex 1: define variables one and two. They should have type Int and
   values 1 and 2, respectively. *)

let one = 1;;
let two = 2;;

(* Ex 2: define the function double of type Integer->Integer. Double
   should take one argument and return it multiplied by two. *)
   
let double x = x + x;;
let double x = x +. x;;  (* A version for floats *)

(* Ex 3: define the function quadruple that uses the function double
   from the previous exercise to return its argument multiplied by
   four. *)
   
let quadruple x = x |> double |> double;;

(* Ex 4: define the function distance. It should take four arguments of
   type Double: x1, y1, x2, and y2 and return the (euclidean) distance
   between points (x1,y1) and (x2,y2).
   Examples:
     distance 0 0 1 1  ==>  1.4142135...
     distance 1 1 4 5  ==>  5.0 *)
     
let distance xi yi xf yf =
  let (xdist, ydist) = ((xf -. xi) ** 2.0, (yf -. yi) ** 2.0)
  in  sqrt (xdist +. ydist)
;;
  
(* Ex 5: define the function eeny that returns "eeny" for even inputs
   and "meeny" for odd inputs. *)

let is_even x = x mod 2 == 0
   
let eeny x = match is_even x with
  | true -> "eeny"
  | _    -> "false"
  
(* Ex 6: here's the function checkPassword from the course material.
   Modify it so that it accepts two passwords, "swordfish" and
   "mellon". *)

let check_password pw = match pw with
  | "swordfish" -> "You're in."
  | "mellon"    -> "You're in."
  | _           -> "ACCESS DENIED."
;;

(* Ex 7: A postal service prices packages the following way.
   Packages that weigh up to 500 grams cost 250 credits.
   Packages over 500 grams cost 300 credit + 1 credit per gram.
   Packages over 5000 grams cost a constant 6000 credits.

   Write a function postagePrice that takes the weight of a package
   in grams, and returns the cost in credits. *)
   
let postage_price weight = match weight with
  | weight when weight <= 500  -> 250
  | weight when weight <= 5000 -> 300 + weight
  | weight                     -> 6000
;;

(* Ex 8: define a function isZero that returns True if it is given an
   Integer that is 0, and False otherwise. Give isZero a type signature. *)

let is_zero x = x == 0;;

(* Ex 9: implement using recursion a function sumTo such that
   sumTo n computes the sum 1+2+...+n *)
   
let rec sum_to x = match x with 
  | 1 -> 1
  | x -> x + sum_to (x-1)
  
(* Ex 10: power n k should compute n to the power k (i.e. n^k)
   Use recursion. *)
   
let rec power n k = match (n, k) with
  | (_, 0)  -> 1
  | (n, k ) -> n * (power n (k - 1))
(*   
-- -- Ex 11: ilog3 n should be the number of times you can divide given
-- -- number by three (rounding down) before you get 0.
-- --
-- -- For example, ilog3 20 ==> 3 since
-- --   20/3 = 6.66 (gets rounded down to 6)
-- --   6/3 = 2
-- --   2/3 = 0.666 (gets rounded down to 0)
-- --
-- -- Use recursion to define ilog3. Use the function "div" for integer
-- -- division. It rounds down for you.
-- --
-- -- More examples:
-- --   ilog3 2 ==> 1
-- --   ilog3 7 ==> 2
 *)
 
 
 let rec ilog3 n = match n with 
   | 0 -> 0
   | x -> 1 + ilog3 (x/3)
