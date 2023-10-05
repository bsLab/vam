(*
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
**    $VERSION:     1.04
**
**    $INFO:
**
**  Generic byte buffers. The data region is handled outisde of the heap!
**
**    $ENDOFINFO
**
*)




(*
** Raised if a get or put operations failed because the buffer was accessed
** beyond his boundary.
*)


exception Buf_overflow

type buffer


(*
** Create a new master buffer of specified size. Memory space
** will be allocated.
*)

external buf_physical:  size:int ->
                        buffer
            = "ext_buf_physical"

val buf_create : size:int -> buffer

(*
** Derive a slave buffer from a master buffer. The slave buffer is a
** window from the master buffer. No memory space will be allocted. The
** master can be itself a slave of a master, of course.
*)

external buf_logical:   src:buffer ->
                        pos:int ->
                        size:int ->
                        buffer 
            = "ext_buf_logical"

(*
** Same as above, but a new buffer is created and the desired content is
** copied into the new one.
*)

external buf_copy:      src:buffer ->
                        pos:int ->
                        size:int ->
                        buffer
            = "ext_buf_copy"

(*
** Get and set buffer functions
*)

external buf_get:       buffer ->
                        int ->          (* pos      *)
                        int             (* byte     *)
            = "ext_buf_get"

external buf_set:       buffer ->
                        int ->          (* pos      *)
                        int ->          (* byte     *)
                        unit
            = "ext_buf_set"

external buf_gets:      buffer ->
                        int ->          (* pos      *)
                        int ->          (* size     *)
                        string          (* string   *)
            = "ext_buf_gets"

external buf_sets:      buffer ->
                        int ->          (* pos      *)
                        string ->       (* string   *)
                        unit
            = "ext_buf_sets"


external buf_getc:      buffer ->
                        int ->          (* pos      *)
                        char          
            = "ext_buf_getc"

external buf_setc:      buffer ->
                        int ->          (* pos      *)
                        char ->       
                        unit
            = "ext_buf_setc"


external buf_len:       buffer ->
                        int   
            = "ext_buf_len"   


val buf_tostring: buffer -> int -> int -> string
val buf_ofstring: string -> int -> int -> buffer

val nilbuf: buffer

(*
** String replacement funcions
*)

val length: buffer -> int
val get: buffer -> int -> char
val set: buffer -> int -> char -> unit
val create: int -> buffer
val sub: buffer -> pos:int -> len:int -> buffer

val string_of_buf: buffer -> string
val buf_of_string: string -> buffer


(*
** Blit functions
*)

external blit_bb:      src:buffer ->
                       src_pos:int ->          
                       dst:buffer ->
                       dst_pos:int ->
                       len:int ->          
                       unit
            = "ext_blit_bb"
 
external blit_bs:      src:buffer ->
                       src_pos:int ->          
                       dst:string ->
                       dst_pos:int ->
                       len:int ->          
                       unit
            = "ext_blit_bs"
 
external blit_sb:      src:string ->
                       src_pos:int ->          
                       dst:buffer ->
                       dst_pos:int ->
                       len:int ->          
                       unit
            = "ext_blit_sb"

external fill:         buffer ->    
                       pos:int ->              
                       len:int ->    
                       int ->
                       unit
            = "ext_fill"   


external buf_info: buffer -> string
            = "ext_buf_info"

 
val print_amoeba_buf : buffer -> unit

(*
** Read and write buffers from and into files
*)
(*
** output_buf channel buffer pos_in_buf len 
** input_buf channel buffer pos_in_buf len
*)
val output_buf :  out_channel -> buffer -> int -> int -> unit
val input_buf:    in_channel -> buffer -> int -> int -> unit
