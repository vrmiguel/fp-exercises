#use "topfind"
#thread;;
#require "core.top";;
#require "core.syntax";;

open Base;;

(* Pattern matching testing for a specific value *)
let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd :: tl ->
    let new_tl = drop_value tl to_drop in
    if hd = to_drop then new_tl else hd :: new_tl
;;

(* Simple map example *)
List.map ~f:String.length ["Hello"; "World!"] ;;

(* Map2 example *)
List.map2_exn ~f:Int.max [1; 2; 3] [3; 2; 1] ;;
(* res[i] = l1[i] > l2[1] ? l1[i] : l2[i] *)

(* Folding example for summing a list *)
let sum l = List.fold ~init:0 ~f:(+) l ;;

(* Folding example for reversing a list *)
let reverse l = List.fold ~init:[] ~f:(fun acc hd -> hd :: acc) l
;;

let render_separator widths = 
  let pieces = List.map widths
    ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
;;  

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '
;;

(* Reduce example *)
let rsum l = List.reduce ~f:(+) l ;;

(* Filter example *)
let keep_evens l = List.filter ~f:(fun x -> x % 2 = 0) l ;;

let extensions filenames =
  List.filter_map filenames ~f:(fun fname ->
  match String.rsplit2 ~on:'.' fname with
  | None | Some ("", _) -> None
  | Some (_, ext) -> Some ext)
  |> List.dedup_and_sort ~compare:String.compare
;;