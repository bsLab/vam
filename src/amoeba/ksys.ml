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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     ?
**    $VERSION:     1.03
**
**    $INFO:
**
** Kernel system requests
**
**    $ENDOFINFO
**
*)

open Amoeba
open Bytebuf
open Buf
open Cmdreg
open Stderr
open Stdcom
open Rpc
open Ar
open Dir


let sys_PRINTBUF    = Command (sys_FIRST_COM + 0)
let sys_KSTAT       = Command (sys_FIRST_COM + 1)
let sys_NETCONTROL  = Command (sys_FIRST_COM + 3)
let sys_BOOT        = Command (sys_FIRST_COM + 4)
let sys_CHGCAP      = Command (sys_FIRST_COM + 5)

let def_SYSSVRNAME  = "sys"

let printbuf_SIZE = 30000 

let syscap_lookup hostcap =
    dir_lookup ~root:hostcap
               ~name:def_SYSSVRNAME


(*
** Kernel message buffer readout. Either the kernel root capability
** or the system server capability must be specified.
*)

let sys_printbuf ~cap =

    try
    begin
        let printchar c =
            match c with
            | '\000' -> 
                false;
            | ' ' .. '~' 
            | '\n' | '\t' -> 
                true
            | _ -> 
                false
        in

        let syscap = ref nilcap in
        let stat,cap' = syscap_lookup cap in

        if (stat = std_OK) then
            syscap := cap'
        else
        begin
            let stat,str = std_info cap 100 in
            if (stat <> std_OK) then
                raise (Error stat);
            (*
            ** Could be the sys server.
            *)
            syscap := cap;            
        end;
        


        let hdr = { (header_new ()) with
                h_port = !syscap.cap_port;
                h_priv = !syscap.cap_priv;
                h_command = sys_PRINTBUF;
              } in

        let buf = buf_create printbuf_SIZE in
        let stat,n,hdr' = trans (hdr,nilbuf,0,
                                 buf,printbuf_SIZE) in


        if (stat <> std_OK) then
            raise (Error stat);

        if (hdr'.h_status <> std_OK) then
            raise (Error hdr'.h_status);

        let bstr = String.create n in
        let off = let off' = hdr'.h_offset in
                        if (off' = n) then
                            0
                        else 
                            off'
            in
            
        (*
        ** Some sanity checks 
        *)
        if (off > n || off < 0) then
            raise (Error std_SYSERR);

        (*
        ** Fix a wrap around of the buffer
        *)

        blit_bs ~src:buf
                        ~src_pos:off
                        ~dst:bstr
                        ~dst_pos:0
                        ~len:(n-off);
        blit_bs ~src:buf
                        ~src_pos:0
                        ~dst:bstr
                        ~dst_pos:(n-off)
                        ~len:off;
        (*
        ** Because there could be garbage in the message buffer,
        ** we must scan and fix this garbage. Arrgh.
        *)
        let valid = ref [] in
        let first = ref 0 in
        let last = ref 0 in

        for i = 0 to n-1
        do
            let isc = printchar bstr.[i] in
            if isc then
            begin
                if !first = (-1) then first := i;
                last := i;
            end
            else
            begin
                if !last > !first then valid := !valid @ [!first, !last];
                last := (-1);
                first := (-1);
            end;
        done;
        if !last > !first then valid := !valid @ [!first, !last];

        let bstr' = ref "" in
        List.iter (fun (f,l)  ->
                bstr' := !bstr' ^ (String.sub bstr f (l-f+1));
            ) !valid;
        std_OK,!bstr'
    end
    with
        | Error err -> err,""

(*
** Generate a kernel host capability from the MAC address of the NIC.
*)

let sys_hostcap mac =
    let getport = ar_toport mac in
    let putport = priv2pub getport in
    let kernel_rights = Rights_bits 63 in
    let putpriv = prv_encode ~obj:(Objnum 1) ~rights:kernel_rights
                             ~rand:getport in
    { cap_port = putport; cap_priv = putpriv } 


(*
** Boot remote a new kernel image 
**
** Args:
**
**      kernelcap:      capability for new kernel binary (AFS object)
**      syscap:         capability for kernel's sysserver
**      commandline:    kernel command line
**      flags:          bootflags
*)


(*
** maximal size of kernel command line 
*)

let max_COMMANDLINE = 128 


let sys_boot ~syscap
             ~kernelcap
             ~commandline
             ~flags     
    =
    let buf = buf_create (cap_SIZE + max_COMMANDLINE) in
    
    let pos = buf_put_cap ~buf:buf
                          ~pos:0
                          ~cap:kernelcap in
    let pos = buf_put_string ~buf:buf
                             ~pos:pos
                             ~str:commandline in

    let hdr = { (header_new ()) with
                h_port = syscap.cap_port;
                h_priv = syscap.cap_priv;
                h_command = sys_BOOT;
                h_extra = flags;
            } in
    let err,n,hdr' = trans (hdr,buf,pos,nilbuf,0) in
    if (err <> std_OK) then
        err
    else
        hdr'.h_status

(*
** Print kernel statistics. Either the kernel root capability
** or the system cap must be specified. The stats flags list
** contains string in  the form '-X'.
*)

let sys_kstat ~cap
              ~flags
    =
    try
    begin
        let syscap = ref nilcap in
        let stat,cap' = syscap_lookup cap in

        if (stat = std_OK) then
            syscap := cap'
        else
        begin
            let stat,str = std_info cap 100 in
            if (stat <> std_OK) then
                raise (Error stat);
            (*
            ** Could be the sys server.
            *)
            syscap := cap;            
        end;
        let str = ref "" in
        let buf = buf_create printbuf_SIZE in

        List.iter (fun a ->
            if (String.length a < 2 ||
                a.[0] <> '-') then
                raise (Error std_ARGBAD);

            let hdr = { (header_new ()) with
                    h_port = !syscap.cap_port;
                    h_priv = !syscap.cap_priv;
                    h_command = sys_KSTAT;
                    h_extra = int_of_char a.[1];
                  } in

            let stat,n,hdr' = trans (hdr,nilbuf,0,
                                     buf,printbuf_SIZE) in


            if (stat <> std_OK) then
                raise (Error stat);

            if (hdr'.h_status <> std_OK) then
                raise (Error hdr'.h_status);

            let bstr = buf_tostring buf 0 n in
            str := !str ^ bstr ^ "\n";
            ) flags;

        std_OK,!str
    end
    with
        | Error err -> err,""
