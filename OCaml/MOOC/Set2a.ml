(* I'm making this on sketch.sh
Link: https://sketch.sh/s/pWqydZjX9MS629QX0U2zi4/
*)

(* 
Ex 1: Define the constant years, that is a list of the values 1982,
      2004 and 2020 in this order.
 *)
 
let years = [1982; 2004; 2020] ;;
 
(* Some utilitary functions for this set *)
let rec take n xs = match (n, xs) with
  | (_, [])      -> []
  | (0, (x::xs)) -> []
  | (1, (x::xs)) -> [x]
  | (n, (x::xs)) -> x :: take (n-1) xs
;;  

(* I could use List.rev... but won't *)
let rec reverse xs = match xs with
  | []  -> []
  | [x] -> [x]
  | (x::xs) -> reverse xs @ [x]
;;

(* Ex 2: define the function takeFinal, which returns the n last
   elements of the given list.

   If the list is shorter than n, return all elements. 
*)

let take_final n xs = take n (reverse xs);;


