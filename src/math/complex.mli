(*
**
** Module [Complex] : support of double float complex
** data types.
**
*)

type t = float*float

(* Convert float  to complex type *)
val complex_of_float : float -> t 

(* Extract the real part of a complex number *)
val real: t -> float 

(* Extract the imaginary part of a complex number *)
val imag: t -> float 

(* Convert a float tuple to complex - only cosmetic *)
val complex: t -> t 

(* Absolute value of a complex value *)
val cabs: t -> float 

(* Inverte a complex number *)
val cinv: t -> t 

(* Calculate the conjugate of a complex number *)
val conj: t -> t 

(* Basic operators *)
external ( +@ ) : t -> t -> t = "ext_add_complex"
external ( -@ ) : t -> t -> t = "ext_sub_complex"
external ( *@ ) : t -> t -> t = "ext_mul_complex"
external ( /@ ) : t -> t -> t = "ext_div_complex"

val cneg: t -> t 

(* Complex trigonometric functions *)
val csin: t -> t 
val ccos: t -> t 
val ctan: t -> t 
val csinh: t -> t 
val ccosh: t -> t 
val ctanh: t -> t 

external cexp: t -> t = "ext_func_zexp"
external cln:  t -> t = "ext_func_zlog"
external csqrt: t -> t = "ext_func_zsqrt"
