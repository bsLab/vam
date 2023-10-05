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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     2001.00.00
**    $VERSION:     1.43
**
**    $INFO:
**
**  Amoeba basic specifications 
**
**  Some notes: extra care must be taken due to multi threaded programming.
**  OCaML tends to use "variables" in more than one structure. But different
**  threads need different variable storage space, for example in header
**  structures. Therfore, functions allocating new strucutures like headers
**  are outsourced to 'amoeba_ext.c'. 
**  Use always the XX_new functions to get a real fresh and unique
**  data structure.
**
**    $ENDOFINFO
**
*)


open StdLabels
open String

(*
** Buffer management based on strings
*)

open Format


(*
** Real size of an Amoeba basic types in bytes, as hopefully
** used by the C libs.
*)

let port_SIZE = 6
let oob_SIZE = 20
let header_SIZE = 32
let priv_SIZE = 10
let cap_SIZE = 16
let objnum_SIZE = 3
let rightsbits_SIZE = 4

let int16_SIZE = 2                                        
let int32_SIZE = 4

let max_PATHLEN = 256

(*
** Used in headers and capabilities
*)

type rights_bits = Rights_bits of int
type obj_num = Objnum of int
type command = Command of int
type errstat = Errstat of int
type status = Status of int


(*
** An amoeba exception has occurred
*)

exception Error of status



(*
** For now the capability only holds 24 rights bits
*)
 
let max_objnum = Objnum ((1 lsl 24) - 1 ) 


(*
** Amoeba port represented externally as a string of length
** portsize
*)

type port = Port of string (* length = portsize *)

(*
** private is a caml keyword -> use privat instead 
*)

type privat = {
    mutable prv_object: obj_num;        (* uint8[3] *)
    mutable prv_rights: rights_bits;    (* uint8    *)
    mutable prv_random: port;
}

(*
** Capability
*)

type capability = {
    mutable cap_port: port;
    mutable cap_priv: privat;
}

(*
** RPC header
*)
 
type header = {
    (* 
    ** RPC port address
    *)
    mutable h_port: port;

    (*
    ** Private part
    *)

    mutable h_priv: privat;


    (*
    ** Transaction command
    *)
    mutable h_command: command;
    
    (*
    ** overlapped with h_command: the status of the server request
    *)

    mutable h_status: status;

    (*
    ** User data
    *)
    
    mutable h_offset: int;  (* int 32 *)
    mutable h_size: int;    (* bufsize=uint16 *)
    mutable h_extra: int;   (* uint16 *)
}


(*
** Some externals
*)

external ext_hdr_new: unit -> header = "ext_hdr_new"
external ext_port_new: unit -> port = "ext_port_new"
external ext_priv_new: unit -> privat = "ext_priv_new"
external ext_cap_new: unit -> capability = "ext_cap_new"
    

(*
** Capability, port and private utilities
*)

(* hack, see begin of file *)
let zero () =
    let zero_val = 0 in
    zero_val


(*
** Create a fresh port and copy the content of the original one.
*)

let port_copy origport =
    let (Port str) = origport in
    (Port (String.copy str))

(*
** Create a fresh private structure and copy the content of the original one.
*)

let priv_copy origpriv =
    let priv =
    {
        prv_object = origpriv.prv_object;
        prv_rights = origpriv.prv_rights;
        prv_random = port_copy origpriv.prv_random;
    } in
    priv
   

(*
** Create a fresh capability and copy the content of the original one.
*)

let cap_copy origcap =
    let cap =
    {
        cap_port = port_copy origcap.cap_port;
        cap_priv = priv_copy origcap.cap_priv;
    } in
    cap



(*
** Zero port, capability, private 
*)

let nilport = 
    ext_port_new ()

let nilcap =
    ext_cap_new ()

let nilpriv =
    ext_priv_new ()

let nilheader = 
    ext_hdr_new ()


(*
** Return a new port initially empty
*)

let port_new =
    ext_port_new 

(*
** Return a new private structure initially empty
*)

let priv_new =
    ext_priv_new 

(*
** Return a new capabiliy initially empty
*)

let cap_new =
    ext_cap_new


(*
** Utilities to copy commands and status values.
*)

let cmd_copy cmd =
    let (Command cmdid) = cmd in
    (Command cmdid)

let stat_copy stat =
    let (Status statid) = stat in
    (Status statid)



(*
** Return a new header initially empty
*)

let header_new =
    ext_hdr_new 

(*
** Copy all from header h1 to header h2
*)

let header_copy h1 h2 = 
    h2.h_port <- h1.h_port;
    h2.h_priv <- h2.h_priv;
    h2.h_command <- h1.h_command;
    h2.h_status <- h1.h_status;
    h2.h_offset <- h1.h_offset;
    h2.h_size <- h1.h_size;
    h2.h_extra <- h1.h_extra

(*
** Compare two ports: return true if they are fully equal
*)

let portcmp (p1:port) (p2:port) = (p1 = p2)
let nullport p = (p = nilport)


(*
** Utils to get and set single bytes of a port
*)

let get_portbyte ~port:port ~byte:byte =
    let (Port str) = port in
    (int_of_char (String.get str byte))

let set_portbyte ~port:port ~byte:byte ~value:value =
    let (Port str) = port in
    String.set str byte (char_of_int value)


(*
** Convert a private port to a public port (get-port to put-port).
*)


(* 
** Encryption function
*)

open Des48
let one_way p =
    let key = Array.create 64 0 in
    let block = Array.create 48 0 in
    let pubport = String.make port_SIZE '\000' in
    let (Port portbytes) = p in
    let j = ref 0 in

    (*
    ** We actually need 64 bit key.
    ** Throw some zeroes in at bits 6 and 7 mod 8
    ** The bits at 7 mod 8 etc are not used by the algorithm
    *)

    for i = 0 to 63 
    do
        if ((i land 7) > 5) then
            key.(i) <- 0
        else
        begin
            key.(i) <- (if 
                         ( (int_of_char (String.get portbytes (!j lsr 3))) 
                           land
                         (1 lsl (!j land 7)) <> 0) then 
                            1
                        else
                            0);
            incr j;
        end;
    
    done;


    des_OWsetkey key;
    (*
    ** Now go encrypt constant 0
    *)

    des_OWcrypt48 block;


    (*
    ** and put the bits in the destination port
    *)
    let pb = ref 0 in

    for i = 0 to (port_SIZE - 1)
    do
        let pbyte = ref 0 in
        for j = 0 to 7
        do
                pbyte := !pbyte lor (block.(!pb) lsl j);
            incr pb;
        done;
        String.set pubport i (char_of_int (!pbyte));
    done;

    (Port pubport)


(*
** Support of a priv->pub port cache. A size of 20 entries should
** be a good choice.
*)

let priv2pub_cache = Cache.create ~size:20    

let priv2pub ~getport:getport  =
    
    try 
        Cache.lookup ~cache:priv2pub_cache ~key:getport 
    with
        Not_found ->
                    let putport = one_way getport in
                    Cache.add ~cache:priv2pub_cache 
                              ~key:getport 
                              ~data:putport;
                    putport




(*
** Private decoding and encoding
*)

let prv_all_rights = Rights_bits 0xff 

(*
** Operation on rights
*)

let rights_and r1 r2 =
    let Rights_bits r1' = r1 in
    let Rights_bits r2' = r2 in
    let Rights_bits pall = prv_all_rights in
    Rights_bits ((r1' land r2') land pall) 

let rights_or r1 r2 =
    let Rights_bits r1' = r1 in
    let Rights_bits r2' = r2 in
    let Rights_bits pall = prv_all_rights in
    Rights_bits ((r1' lor r2') land pall) 

let rights_xor r1 r2 =
    let Rights_bits r1' = r1 in
    let Rights_bits r2' = r2 in
    let Rights_bits pall = prv_all_rights in
    Rights_bits ((r1' lxor r2') land pall) 

let rights_not r1 =
    let Rights_bits r1' = r1 in
    let Rights_bits pall = prv_all_rights in
    Rights_bits ((lnot r1') land pall) 


(*
** Check wether the required rights [R1;R2;..] are
** present in the rights field rg. Return a boolean value.
*)

let rights_req ~rights ~required =
    let Rights_bits r = rights in
    let all = ref true in
    List.iter (fun rq ->
        let Rights_bits br = rq in
        if (br land r = 0) then
            all := false; 
    ) required;
    !all

(*
** Set rights bits [R1;R2;...]
*)

let rights_set rights =
    let r = ref 0 in
    List.iter (fun rq ->
        let Rights_bits br = rq in
        r := !r lor br;
    ) rights;
    (Rights_bits !r)
    

(*
** Decode a private structure.
**
*)

let prv_decode ~prv:prv ~rand:rand =

    if (prv.prv_rights = prv_all_rights) then    
        (portcmp prv.prv_random rand)
    else
    begin
        let tmp_port = port_copy rand in
        let pt0 = get_portbyte ~port:tmp_port ~byte:0 in
        let (Rights_bits pr0) = prv.prv_rights in
        set_portbyte ~port:tmp_port ~byte:0 ~value:(pt0 lxor pr0);   
        let tmp_port = one_way tmp_port in
        (portcmp prv.prv_random tmp_port)
    end



(*
** Encode a private part from the object number, the rights field
** and the random port.
** Returns the created private structure.
*)

let prv_encode ~obj:obj ~rights:rights ~rand:rand =
    if ( obj > max_objnum) then
        failwith "prv_encode: object number too large!";

    let tmp_port = port_copy rand in

    let (Rights_bits r1) = rights in
    let (Rights_bits rmask) = prv_all_rights in 

    if (rights = prv_all_rights) then 
        {
            prv_object = obj;
            prv_rights = Rights_bits (r1 land rmask);
            prv_random = tmp_port;
        }
    else
        {
            prv_object = obj;
            prv_rights = Rights_bits (r1 land rmask);
            prv_random = (
                           let pt0 = get_portbyte ~port:tmp_port ~byte:0 in
                           set_portbyte ~port:tmp_port 
                                        ~byte:0 
                                        ~value:(pt0 lxor r1);
                           one_way tmp_port
                         );
        }

(*
** Return the private object number form a private structure
*)

let prv_number prv =
    prv.prv_object

(*
** Return the private rights field.
*)

let prv_rights prv =
    let (Rights_bits r) = prv.prv_rights in
    let (Rights_bits mask) = prv_all_rights in
    (Rights_bits (r land mask))

(*
** Return a new random port.
**
** Warning: the quality of the random ports are strongly
** related with CaML's underlying random generator. Be warned!
*)

let uniqport () =
    let str = String.create port_SIZE in
    for i = 0 to (port_SIZE - 1)
    do
        String.set str i (char_of_int (Random.int 256));
    done;
    (Port str)


(*
** Derive a port from a string.
*)

let port_of_str name =
    let p = String.create port_SIZE in
    let n = String.length name in
    for i = 0 to port_SIZE - 1
    do
        p.[i] <- '\000';        
    done;

    for i = 0 to n-1
    do
        let k = i mod port_SIZE in
        p.[k] <- char_of_int (
                    (int_of_char p.[k] + int_of_char name.[i])
                    land 0xff);
    done;
    Port p

     

let _ =
    (*
    ** TODO: Very optimistic random initialization !!
    *)
    let init_val = int_of_float (Sys.clock ()) in 
    Random.init init_val

