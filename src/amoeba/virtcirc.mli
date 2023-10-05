/*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse sci@bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.04
**
**    $INFO:
**
** Virtual Circuit Implementation (distributed circular buffers)
**
**    $ENDOFINFO
**
*/




open Amoeba
open Bytebuf
open Thread
open Mutex
open Sema
open Circbuf


(*
** Argument to close.
*)

val vc_IN:int              (* Close input virtual circuit      *)
val vc_OUT:int             (* Close output virtual circuit     *)
val vc_BOTH:int            (* Close virtual circuit completely *)
val vc_ASYNC:int           (* Don't wait for completion        *)

(*
** The virtual circuit control structure
*)


type virt_circ  =
{
    vc_mutex: Mutex.t;          (* VC control block mutex           *)

    vc_client_hup: semaphore;
    vc_client_dead: semaphore;
    vc_oclosed: semaphore;
    vc_iclosed: semaphore;
    vc_done: semaphore;

    mutable vc_status: int;     (* Virtual circuit status           *)
    vc_sigs: int array;         (* Client to srv/srv to client sig  *)
    vc_cb: circular_buf array;  (* In and output circular buffers   *)
    vc_port: port array;        (* Remote/local ports               *)
}


(*
** Returne a new vc control structure, initally empty, needed for
** references onto vc's.
*)


val nilvc: virt_circ


(*
** vc_close - Close one or both circular buffers.
**
** vc_close vc which
*)

val vc_close: virt_circ -> int -> unit

(*
** vc_create - create a full-duplex virtual circuit.
**
** vc_create ~iport:iport
**            ~oport:oport
**            ~isize:isize
**            ~osize:osize
*)

val vc_create: iport:Amoeba.port ->
               oport:Amoeba.port -> 
               isize:int -> osize:int 
            -> virt_circ

(*
** vc_reads - read string (maximal length len) from vc.
**
** vc_reads vc ~len:maxlength
*)

val vc_reads: virt_circ -> str:string -> pos:int -> len:int -> int


(*
** read instead in a  buffer at position pos (maximal length len)
**
** vc_readb vc ~buf:buf ~pos:pos ~len:len
*)

val vc_readb: virt_circ -> buf:buffer -> pos:int -> len:int -> int

(*
** Write a string to the vc
*)

val vc_writes: virt_circ -> str:string -> pos:int -> len:int -> int

(*
** Write to the vc from a buffer starting at position pos and length len.
**
** vc_writeb vc ~buf:buf ~pos:pos ~len:len
*)

val vc_writeb: virt_circ -> buf:buffer -> pos:int -> len:int -> int

(*
** vc_getp - get a circular buffer pointer to fetch data
** from
*)

val vc_getp: virt_circ -> int * int

(*
** vc_getpdone - mark it done
*)

val vc_getpdone: virt_circ -> int -> unit

(*
** vc_putp - get a circular buffer pointer to store data in.
*)

val vc_putp: virt_circ -> int * int 

(*
** vc_putpdone - mark it done
*)

val vc_putpdone: virt_circ -> int -> unit


val print_amoeba_vc: virt_circ -> unit
