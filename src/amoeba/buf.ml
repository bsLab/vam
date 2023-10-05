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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.08
**
**    $INFO:
**
** Machine independent storing and extracting
** of Amoeba structures in and from buffers with bound checking.
**
**
**    $ENDOFINFO
**
*)



open StdLabels

open Amoeba
open Bytebuf
open String
open Capset
open Os
open Stderr


(*
** High level Buffer management
*)

(*
** Put a short int8 value into a buffer -> machine independent
*)

let buf_put_int8 ~buf:buf ~pos:pos ~int8:int8 =
    buf_set buf pos (int8 land 0xff);
    (pos+1)
            



(*
** Put a short int16 value into a buffer -> machine dependent
*)

let buf_put_int16 ~buf:buf ~pos:pos ~int16:int16 =

    match vm_arch with
    | CPU_I386 ->   buf_set buf pos (int16 land 0xff);
                    buf_set buf (pos+1) ((int16 lsr 8) land 0xff);
                    (pos+2)
            

(*
** Put an int variable into a buffer (attention: caml integers
** are only 31/63 bit wide!)
*)

let buf_put_int32 ~buf ~pos ~int32 =

    match (vm_arch) with
    | CPU_I386 ->   buf_set buf pos (int32 land 0xff);
                    buf_set buf (pos+1) ((int32 lsr 8) land 0xff);
                    buf_set buf (pos+2) ((int32 lsr 16) land 0xff);
                    buf_set buf (pos+3) ((int32 lsr 24) land 0xff);
                    (pos+4)


(*
** Put a string in a buffer - simple, but dont't forget to add
** an EOS char '\000' !
*)

let buf_put_string ~buf:buf ~pos:pos ~str:str =

    let strlen = String.length str in
    buf_sets buf pos str;
    buf_set buf (pos+strlen) 0;
    (pos + strlen + 1)



(*
** Put a port
*)

let buf_put_port ~buf:buf ~pos:pos ~port:port =
    let (Port pstr) = port in
    buf_sets buf pos pstr;
    (pos+port_SIZE)

(*
** Put a private structure
*)


let buf_put_priv ~buf:buf ~pos:pos ~priv:prv =
    let bufpos = ref pos in
    let (Objnum objnum)         = prv.prv_object in
    let (Rights_bits rights)    = prv.prv_rights in
    let (Port pstr)             = prv.prv_random in

    (* first the prv_objnum - size objnumsize - LSB first *)
    buf_set buf (!bufpos) (objnum land 0xff); incr bufpos;
    buf_set buf (!bufpos) ((objnum lsr 8) land 0xff); incr bufpos;
    buf_set buf (!bufpos) ((objnum lsr 16) land 0xff); incr bufpos;
 
    (* now the prv_rights_bits - LSB first *)
    buf_set buf (!bufpos) (rights land 0xff); incr bufpos;
      
    buf_sets buf (!bufpos) pstr;
    (pos + priv_SIZE)
    

(*
** Put a capability
*)

let buf_put_cap ~buf:buf ~pos:pos ~cap:cap =
    let bufpos = ref pos in
    bufpos := buf_put_port ~buf:buf ~pos:(!bufpos) ~port:cap.cap_port;
    bufpos := buf_put_priv ~buf:buf ~pos:(!bufpos) ~priv:cap.cap_priv;
    (!bufpos)


(*
** Put a capset
*)

let buf_put_capset ~buf:buf ~pos:pos ~cs:cs =
    (* some sanity checks *)
    if (cs.cs_final < 0 || 
        cs.cs_initial < 0 ||
        cs.cs_initial < cs.cs_final ||
        cs.cs_final > capset_MAX) 
    then failwith "buf_put_capset: inavlid capability set";

    let bufpos = ref pos in

    bufpos := buf_put_int16 ~buf:buf ~pos:(!bufpos) ~int16:cs.cs_initial;
    bufpos := buf_put_int16 ~buf:buf ~pos:(!bufpos) ~int16:cs.cs_final;

    for i = 0 to (cs.cs_final - 1)
    do
        bufpos := buf_put_cap ~buf:buf ~pos:(!bufpos) 
                        ~cap:(cs.cs_suite.(i)).s_object;

        bufpos := buf_put_int16 ~buf:buf ~pos:(!bufpos)
                        ~int16:(
                                if ((cs.cs_suite.(i)).s_current = true)
                                then 1
                                else 0
                                );
    done;            
    (!bufpos)

(*
** Put right bits in a buffer
*)

let buf_put_right_bits ~buf ~pos ~right =
    buf_put_int32 ~buf:buf ~pos:pos ~int32:right

let buf_put_rights_bits ~buf ~pos ~rights =
    let Rights_bits r = rights in
    buf_put_int32 ~buf:buf ~pos:pos ~int32:r


(*
** Extracting parts
*)

(*
** Get a short int8 value into a buffer -> machine independent
*)

let buf_get_int8 ~buf:buf ~pos:pos =
    (pos+1),(buf_get buf pos)  

(*
** Get a short int16 value into a buffer -> machine dependent
*)

let buf_get_int16 ~buf:buf ~pos:pos =

    match (vm_arch) with
    | CPU_I386 ->   (   (pos+2),
                    ( 
                      (buf_get buf pos) lor 
                      ((buf_get buf (pos+1)) lsl 8) 
                    )
                )            

(*
** Get a int32 value into a buffer -> machine dependent
** Attention: OCaML's integers are only 31/63 bit wide!
*)

let buf_get_int32 ~buf:buf ~pos:pos =
    match (vm_arch) with
    | CPU_I386 ->   (   (pos+4),
                    ( 
                      (buf_get buf pos) lor 
                      ((buf_get buf (pos+1)) lsl 8) lor
                      ((buf_get buf (pos+2)) lsl 16) lor
                      ((buf_get buf (pos+3)) lsl 24) 
                      
                    )
                )            


(*
** Strings
*)

let buf_get_string ~buf:buf ~pos:pos =
    (*
    ** find the end of string char '\000' or fail with bound check
    *)
    let curpos    = ref pos in

    while ( (buf_get buf !curpos) <> 0)
    do
        incr curpos;
    done;


    ((!curpos+1),(buf_gets buf pos (!curpos - pos )))

(*
** Get a port
*)

let buf_get_port ~buf:buf ~pos:pos =

    ((pos+port_SIZE),
     Port (buf_gets buf pos port_SIZE))


(*
** Get a private structure
*)


let buf_get_priv ~buf:buf ~pos:pos =

    (* first the prv_objnum - size objnumsize - LSB first *)
    let objnum = 
        (buf_get buf pos) lor 
        ((buf_get buf (pos+1)) lsl 8) lor
        ((buf_get buf (pos+2)) lsl 16)
    in
 
    let bufpos = ref (pos + 3) in

    (* now the prv_rights_bits *)
    let rights = 
        buf_get buf (!bufpos)
    in
    incr bufpos;
      
    (
        (pos + priv_SIZE),
        {
            prv_object = Objnum objnum;
            prv_rights = Rights_bits rights;
            prv_random = Port (buf_gets buf !bufpos port_SIZE); 
        }
    )

        
(*
** Get a capability
*)

let buf_get_cap ~buf:buf ~pos:pos =

    let (bufpos,port) = buf_get_port ~buf:buf ~pos:pos in
    let (bufpos,priv) = buf_get_priv ~buf:buf ~pos:bufpos in
    (bufpos,{cap_port = port; cap_priv = priv})


(*
** Extract a capset
*)

let buf_get_capset ~buf:buf ~pos:pos =


    let (bufpos,csinit)  =  buf_get_int16 ~buf:buf ~pos:pos in
    let (bufpos,csfinal) =  buf_get_int16 ~buf:buf ~pos:bufpos in

    let cs_suite_arr = Array.create csfinal 
                        ({s_object = nilcap;s_current = false}) in

    for i = 0 to (csfinal - 1)
    do
        let (bufpos,cap) =  buf_get_cap ~buf:buf ~pos:bufpos in
        (cs_suite_arr.(i)).s_object <- cap;
        
        let (bufpos,ival)= buf_get_int16 ~buf:buf ~pos:bufpos in
        if (ival > 0) then
            (cs_suite_arr.(i)).s_current <- true
        else
            (cs_suite_arr.(i)).s_current <- false;

    done;            
    ((pos + csfinal * (cap_SIZE+2) + 4),
        {
            cs_initial = csinit;
            cs_final   = csfinal;
            cs_suite   = cs_suite_arr;
        })


(*
** Get right bits from a buffer
*)

let buf_get_right_bits ~buf ~pos =
    buf_get_int32 ~buf:buf ~pos:pos 

let buf_get_rights_bits ~buf ~pos =
    let p,r = buf_get_int32 ~buf:buf ~pos:pos in
    (p,Rights_bits r)


(*
** Some file utils
*)

(*
** Read a capability from a file.
*)
  
let read_cap name =
    try
    begin
        let ic = open_in_bin name in
        let buf = buf_create cap_SIZE in
        for i = 0 to cap_SIZE-1
        do
            let c = input_char ic in
            buf_setc buf i c; 
        done;
        let _,cap = buf_get_cap buf 0 in
        std_OK,cap
    end
    with
        | Buf_overflow -> std_OVERFLOW,nilcap
        | Sys_error _ -> std_IOERR,nilcap

(*
** Write a capability from a file.
*)
  
let write_cap name cap =
    try
    begin
        let oc = open_out_bin name in
        let buf = buf_create cap_SIZE in
        let _ = buf_put_cap buf 0 cap in

        for i = 0 to cap_SIZE-1
        do
            let c = buf_getc buf i in
            output_char oc c;
        done;
        close_out oc;
        std_OK
    end
    with
        | Sys_error _ -> std_IOERR

