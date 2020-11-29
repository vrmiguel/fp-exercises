#use "topfind"
#thread;;
#require "core.top";;
#require "core.syntax";;

open Base

let sum_if_true test first second =
  (if test first then first else 0)
  + (if test second then second else 0)

let area_of_ring inner_radius outer_radius =
  let pi = Float.pi in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius

(* let upcase_first_entry line =
  let (first :: rest) = String.split ~on:',' line in
  String.concat ~sep:"," (String.uppercase first :: rest)
;; *)

let upcase_first_entry line =
  match String.split ~on:' ' line with
  | [] -> assert false (* unreachable *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
;;

(* Makes "john doe" into "John Doe" *)
let capitalize_name name = String.concat ~sep:" " (List.map (String.split ~on:' ' name) (String.capitalize));;

(* Get a value from a Some through pattern matching *)
let rec first_repetition list =
  match list with
  | [] | [_] -> None 
  | x :: y :: tl -> 
    if x = y then Some x else first_repetition (y :: tl)
;;

(* Optimized code is boring *)
let rec is_even x =
  if x = 0 then true else is_odd (x-1)
  and is_odd x = 
    if x = 0 then false else is_even (x-1)


let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/usr/bin";;
String.split ~on:':' path
|> List.dedup_and_sort ~compare:String.compare
|> List.iter ~f:Stdio.print_endline

let apply_to_tuple f (fst, snd) = f ~fst ~snd

(* Optional arguments *)
let concat ?sep x y =
  let sep = match sep with None -> "" | Some x -> x in 
  x ^ sep ^ y
;;

(* Optional arguments - a smaller way *)
let concat ?(sep="") x y = x ^ sep ^ y ;;

let numeric_deriv ~delta ~x ~y ~f =
  let x' = x +. delta in 
  let y' = y +. delta in
  let base = f ~x ~y in
  let dx = (f ~x:x' ~y -. base) /. delta in
  let dy = (f ~x ~y:y' -. base) /. delta in
  (dx, dy)
;;
