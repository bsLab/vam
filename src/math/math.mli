(*
** Basic arithmetic support.
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

val ln: float -> float ;;
val log: float -> float;; 

val fmax : 'a -> 'a -> 'a
