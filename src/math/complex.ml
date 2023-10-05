


(* Type complex: simple (float*float) tuple *)

type t = float*float


(* Extract the real part of a complex number *)

let real (z : t) = match (z) with (re,im) -> (re: float)   

(* Extract the imaginary part of a complex number *)

let imag (z : t) = match (z) with (re,im) -> (im: float)   

(* complex from a float tuple - only cosmetic *)

let complex (f : float*float) = (f : t) 

(* Convert float to complex type - imag part set to zero *)

let complex_of_float (f : float) = complex (f,0.0)

(* Absolute value of a complex value *)

let cabs (z : t) = match (z) with (re,im) 
                -> sqrt (re*.re +. im*.im)  


(* Inverte a complex number *)

let cinv (z : t) = 
        match (z) with (re,im)
            ->  let denum=(re*.re +. im*.im) in
                complex (re/.denum,(-.im/.denum))
        

(* Basic operators *)

external ( +@ ) : t -> t -> t = "ext_add_complex"
external ( -@ ) : t -> t -> t = "ext_sub_complex"
external ( *@ ) : t -> t -> t = "ext_mul_complex"
external ( /@ ) : t -> t -> t = "ext_div_complex"

let cneg (z: t) =
        match (z) with (re,im) -> complex   (-.re,-.im) 


(* Complex trigonometric functions *)

let csin (z: t) =
        match (z) with (re,im)
            -> complex   (sin (re) *. cosh (im) ,
                        cos (re) *. sinh (im) )


let ccos (z: t) =
        match (z) with (re,im)
            -> complex   (cos (re) *. cosh (im) ,
                        sin (re) *. sinh (im) )


let ctan (z: t) =
        match (z) with (re,im)
            -> let denum = cos (2.0*.re) +. cosh (2.0*.im) in
                complex   (sin (2.0*.re) /. denum ,
                         sinh (2.0*.im) /. denum )


let csinh (z: t) =
        match (z) with (re,im)
            -> complex   (sinh (re) *. cos (im) ,
                        cosh (re) *. sin (im) )


let ccosh (z: t) =
        match (z) with (re,im)
            -> complex   (cosh (re) *. cos (im) ,
                        sinh (re) *. sin (im) )


let ctanh (z: t) =
        match (z) with (re,im)
            -> let denum = cosh (2.0*.re) +. cos (2.0*.im) in
                complex   (sinh (2.0*.re) /. denum ,
                         sin (2.0*.im) /. denum )




(*
** Calculate the conjugate of a complex number 
*)

let conj (re,im) = ((re:float),-.(im:float)) 



(*
** some routines from the amos library
*)

external cexp: t -> t
    = "ext_func_zexp"

(* complex natural logarithm *) 
external cln: t -> t
    = "ext_func_zlog"

external csqrt: t -> t
    = "ext_func_zsqrt"

