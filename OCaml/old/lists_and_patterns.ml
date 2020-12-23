open Base

let eval_cmd cmd = 
  Stdio.print_endline cmd

let read_cmd = 
  match read_line () with
  | "quit" -> 0
  | input -> eval_cmd input

let rec repl x = 
  if not x then 
    () (* Quit program *)
  else 
    let ans = read_cmd () in
    repl ans


let () = repl true