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
**    $VERSION:     1.03
**
**    $INFO:
**
**  Generic byte buffers handled outside the heap. Thread safe.
**
**    $ENDOFINFO
**
*)


(*
** Low level Buffer management
*)


type buffer

(*
** Raised if a get or put operations failed because the buffer was accessed
** beyond his boundary.
*)
 
exception Buf_overflow


(*
** Create a new master buffer of specified size. Memory space
** will be allocated.
*)

external buf_physical:  size:int ->
                        buffer
            = "ext_buf_physical"
    

let buf_create = buf_physical 

(*
** Derive a slave buffer from a master buffer. The slave buffer is a
** window from the master buffer. No memory space will be allocted.
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
** Get and set buffer functions. These functions raise
** the Buf_overflow exception on boundary excess.
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



let buf_tostring buf pos len =
    buf_gets buf pos len

let buf_ofstring str pos len =
    let str' = String.sub str pos len in
    let buf = buf_physical len in
    buf_sets buf pos str';
    buf        



let nilbuf = buf_physical ~size:1

(*
** Replacement for string module functions: string <-> buf
*)

let length = buf_len 
let get = buf_getc 
let set = buf_setc
let create size = buf_physical ~size:size
let copy buf =
    buf_copy buf 0 (buf_len buf)
let sub buf ~pos ~len =
    buf_copy buf pos len
let string_of_buf buf =
    let len = buf_len buf in
    buf_tostring buf 0 len

let buf_of_string str =
    let len = String.length str in
    buf_ofstring str 0 len

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
 
external buf_info:  buffer -> string
            = "ext_buf_info"

open Format

let print_amoeba_buf b =
    open_hvbox 0;
    print_string ("Buffer '" ^ (buf_info b) ^ "'");
    close_box ()


(*
** Read and write buffers from and into files
*)
(*
** output_buf channel buffer pos_in_buf len 
** input_buf channel buffer pos_in_buf len
*)
external output_buf_n :  out_channel -> buffer -> int -> int -> int
        = "ext_output_buf_n"

external input_buf_n:    in_channel -> buffer -> int -> int -> int
        = "ext_input_buf_n"

let output_buf oc buf pos len =
    let pos = ref pos in
    let len = ref len in
    while (!len > 0)
    do
        let n = output_buf_n oc buf !pos !len in
        pos := !pos + n;
        len := !len - n;
    done

let input_buf oc buf pos len =
    let pos = ref pos in
    let len = ref len in
    while (!len > 0)
    do
        let n = input_buf_n oc buf !pos !len in
        pos := !pos + n;
        len := !len - n;
    done
