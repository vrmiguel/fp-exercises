
(* 
    Ex 2: define the function double of type Integer->Integer. 
    Double should take one argument and return it multiplied by two. 
*)

let double x = x +. x ;;

(*
    Ex 3: define the function quadruple that uses the function double
    from the previous exercise to return its argument multiplied by
    four.
*)

let quadruple x = double (double x)

(*
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance
-- between points (x1,y1) and (x2,y2).
-- Examples:
--   distance 0 0 1 1  ==>  1.4142135...
--   distance 1 1 4 5  ==>  5.0
*)

let distance xi yi xf yf = let xdist = (xf -. xi) ** 2.0
                        in sqrt (xdist)
;;