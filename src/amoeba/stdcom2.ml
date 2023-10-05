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
**    $VERSION:     1.14
**
**    $INFO:
**
**
** Implementation of Amoeba's std commands / Part 2
**
**    $ENDOFINFO
**
*)


open Amoeba
open Rpc
open Cmdreg
open Stdcom
open Stderr
open Bytebuf
open Buf


(*
** Standard restrict request. Returns the restricted capability
** of the object 'cap' owned by the server. 
*)

let std_restrict ~cap ~mask =
    let bufsize = cap_SIZE in
    let buf = buf_create bufsize in

    if (mask = prv_all_rights) then
        std_OK,cap
    else if (cap.cap_priv.prv_rights = prv_all_rights)then
    begin
        let obj = prv_number cap.cap_priv in
        let cap' = {
                    cap_port = cap.cap_port;
                    cap_priv = 
                        prv_encode ~obj:obj
                                   ~rights:mask
                                   ~rand:cap.cap_priv.prv_random;
                   } in
        std_OK,cap'
        
    end
    else
    begin 
        let Rights_bits mask = mask in
        let hdr_req = { (header_new ()) with
                h_port = cap.cap_port;
                h_priv = cap.cap_priv;
                h_command = std_RESTRICT;
                h_offset = mask;
                h_size = bufsize;
                } in

        let (err_stat,size,hdr_rep) = trans (hdr_req,nilbuf,0,buf,bufsize) in

    
        if(size > 0 && (hdr_rep.h_status = std_OK)) then
        begin
            let pos,cap_restr = buf_get_cap ~buf:buf ~pos:0 in
            (err_stat,cap_restr)
        end
        else if (hdr_rep.h_status = std_OK) then
        begin
            std_OK,{
                cap_port = hdr_rep.h_port;
                cap_priv = hdr_rep.h_priv;
            }
        end
        else
            (hdr_rep.h_status,nilcap)
    end   

(*
** Standard exec request. Execute a string list on a server. The
** reply is returned in a string, too.
*)

let std_exec ~srv ~args =
    let n = List.length args in
    let buf = buf_create max_TRANS in
    let pos = ref 0 in

    List.iter (fun a ->
        let pos' = buf_put_string ~buf:buf ~pos:!pos ~str:a in
        pos := pos';
        ) args;
    
    let hdr_req = { (header_new ()) with
                h_port = srv.cap_port;
                h_priv = srv.cap_priv;
                h_command = std_EXEC;
                h_size = !pos;
                h_extra = n;
                } in

    let (err_stat,size',hdr_rep) = trans (hdr_req,buf,!pos,
                                          buf,max_TRANS) in

    
    if (size' > 0 && 
        err_stat = std_OK &&
        hdr_rep.h_status = std_OK) then
    begin
        let pos,str = buf_get_string ~buf:buf ~pos:0 in
        err_stat,str
    end
    else if (err_stat = std_OK && 
             hdr_rep.h_status = std_OK) then
    begin
        std_OK,""
    end
    else if (err_stat <> std_OK) then
        err_stat,""
    else
        hdr_rep.h_status,""


(*
** Set parameters for server administration.
**
** Format of argument list: <name>,<value> 
*)

let std_set_params ~srv ~args =
    let hdr = header_new () in
    let bufsiz = 300 in
    let buf = buf_create bufsiz in
    try
    begin
        List.iter (fun a ->
            let name,valu=a in
            let pos = buf_put_string ~buf:buf ~pos:0 ~str:name in
            let pos = buf_put_string ~buf:buf ~pos:pos ~str:valu in

            hdr.h_port <- srv.cap_port;
            hdr.h_priv <- srv.cap_priv;
            hdr.h_command <- std_SETPARAMS;
            hdr.h_extra <- pos; 
            hdr.h_size <- 1;

            let stat,n,hdr' = trans(hdr,buf,bufsiz,nilbuf,0) in
            if (stat <> std_OK) then
                raise (Error stat);            
            if (hdr'.h_status <> std_OK) then
                raise (Error hdr'.h_status);            
        ) args;
        std_OK;
    end;
    with
        | Error stat -> stat

(*
** Get parameter list.
** Return string tuple format:
** <name>,<range and unit>,<desc>,<value>
*)


let std_get_params ~srv =
    let hdr = header_new () in
    let bufsiz = 30000 in
    let buf = buf_create bufsiz in
    hdr.h_port <- srv.cap_port;
    hdr.h_priv <- srv.cap_priv;
    hdr.h_command <- std_GETPARAMS;
    hdr.h_extra <- bufsiz; 

    let stat,n,hdr' = trans(hdr,nilbuf,0,buf,bufsiz) in
    if (stat <> std_OK) then
        stat,[]
    else if (hdr'.h_status <> std_OK) then
        hdr'.h_status,[]
    else
    begin
        let pos = ref 0 in
        let paramlen=hdr'.h_extra in
        let nparams=hdr'.h_size in

        (*
        ** extract the params from the buffer ...
        *)

        let pars = ref [] in
    
        for i = 1 to nparams
        do
            let pos',pname = buf_get_string ~buf:buf ~pos:!pos in
            let pos',prang = buf_get_string ~buf:buf ~pos:pos' in
            let pos',pdesc = buf_get_string ~buf:buf ~pos:pos' in
            let pos',pvalu = buf_get_string ~buf:buf ~pos:pos' in
            pars := !pars @ [pname,prang,pdesc,pvalu];
            pos := pos';
        done;
        std_OK,!pars
    end
