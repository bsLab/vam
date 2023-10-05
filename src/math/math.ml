(*
** Universal arithmetic support
**
** Supported data types:
**
**  int
**  float
**  complex (float*float) 
**
*)



external ( + ): 'a -> 'a -> 'a = "uni_add";;
external ( - ): 'a -> 'a -> 'a = "uni_sub";;
external ( * ): 'a -> 'a -> 'a = "uni_mul";;
external ( / ): 'a -> 'a -> 'a = "uni_div";;

external abs: 'a -> 'a = "uni_abs" ;;
external (~-): 'a -> 'a = "uni_neg" ;;

(* natural log *)
let ln = log ;;
(* base 10 log *)
let log = log10 ;;

let fmax a b = if a > b then a else b
