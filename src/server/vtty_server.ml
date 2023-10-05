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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     25.4.2005
**    $MODIFIED:    
**    $VERSION:     1.05
**
**    $INFO:
**
** Simple Amoeba Terminal server with TIOS support. Emulates an Amoeba
** terminal mapped to the local UNIX terminal. The input and output
** functions can be specified (default UNIX read/write to standard
** output channel).
**
**
**    $ENDOFINFO
**
*)


(* #define DEBUG   1  *)


open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Cmdreg
open Rpc
open Thread
open Ar
open Bytebuf
open Buf
open Name
open Unix
open Signals 
open Circbuf 
open Sema
open Cap_env
open Vtty_common
open Printf
open Syslog



(*
** The TTY server structure
*)
type tty_server = {
    mutable tty_sema: semaphore;
    mutable tty_nthr: int;          (* number of service threads *)
    mutable tty_bufsize: int;
    mutable tty_verbose: int;

    (*
    ** Server ports
    *)
    mutable tty_getport: port;
    mutable tty_putport: port;
    mutable tty_checkfield: port;

    mutable tty_tios: Unix.terminal_io;
    mutable tty_dying: bool;    
    mutable tty_io_init: bool;
    (*
    ** Input and output circular buffers
    *)
    mutable tty_cb_in: Circbuf.circular_buf;
    mutable tty_cb_out: Circbuf.circular_buf;

    (*
    ** Server output and input functions - default
    ** write and read. Can be overriden by user to redirect input
    ** and output.
    *)
    mutable tty_output : buffer -> int -> unit;
    mutable tty_input : buffer -> int -> int;

}




let info str =
    sys_log Sys_info "TTY: %s\n" str

(*
** Print TIOS structure
*)
let tty_print_tios tios =
                    (
                        let p = Printf.printf in
                        p "c_ignbrk %c\n" (if tios.c_ignbrk then '1' else '0');
                        p "c_brkint %c\n" (if tios.c_brkint then '1' else '0');
                        p "c_ignpar %c\n" (if tios.c_ignpar then '1' else '0');
                        p "c_parmrk %c\n" (if tios.c_parmrk then '1' else '0');
                        p "c_inpck %c\n" (if tios.c_inpck then '1' else '0');
                        p "c_istrip %c\n" (if tios.c_istrip then '1' else '0');
                        p "c_inlcr %c\n" (if tios.c_inlcr then '1' else '0');
                        p "c_igncr %c\n" (if tios.c_igncr then '1' else '0');
                        p "c_icrnl %c\n" (if tios.c_icrnl then '1' else '0');
                        p "c_ixon %c\n" (if tios.c_ixon then '1' else '0');
                        p "c_ixoff %c\n" (if tios.c_ixoff then '1' else '0');
                        p "c_opost %c\n" (if tios.c_opost then '1' else '0');
                        p "c_cread %c\n" (if tios.c_cread then '1' else '0');
                        p "c_parenb %c\n" (if tios.c_parenb then '1' else '0');
                        p "c_parodd %c\n" (if tios.c_parodd then '1' else '0');
                        p "c_hupcl %c\n" (if tios.c_hupcl then '1' else '0');
                        p "c_clocal %c\n" (if tios.c_clocal then '1' else '0');
                        p "c_isig %c\n" (if tios.c_isig then '1' else '0');
                        p "c_icanon %c\n" (if tios.c_icanon then '1' else '0');
                        p "c_noflsh %c\n" (if tios.c_noflsh then '1' else '0');
                        p "c_echo %c\n" (if tios.c_echo then '1' else '0');
                        p "c_echoe %c\n" (if tios.c_echoe then '1' else '0');
                        p "c_echok %c\n" (if tios.c_echok then '1' else '0');
                        p "c_echonl %c\n" (if tios.c_echonl then '1' else '0');

                        p "c_obaud %d\n" tios.c_obaud;
                        p "c_ibaud %d\n" tios.c_ibaud;
                        p "c_csize %d\n" tios.c_csize;
                        p "c_cstopb %d\n" tios.c_cstopb;
                        p "c_vmin %d\n" tios.c_vmin;
                        p "c_vtime %d\n" tios.c_vtime;


                        p "c_vintr <%d>\n" (int_of_char tios.c_vintr);
                        p "c_vquit <%d>\n" (int_of_char tios.c_vquit);
                        p "c_verase <%d>\n" (int_of_char tios.c_verase);
                        p "c_vkill <%d>\n" (int_of_char tios.c_vkill);
                        p "c_veof <%d>\n" (int_of_char tios.c_veof);
                        p "c_veol <%d>\n" (int_of_char tios.c_veol);
                        p "c_vstart <%d>\n" (int_of_char tios.c_vstart);
                        p "c_vstop <%d>\n" (int_of_char tios.c_vstop);

                    )



(*
** Marshal and unmarshal Termianl IO structures into/from buffers. 
** Target format: Amoeba TTY!
*)
let tty_tc_marshal ~tios ~buf =
    let c_iflag = (if (tios.c_ignbrk) then 0x1 else 0) lor
                  (if (tios.c_brkint) then 0x2 else 0) lor
                  (if (tios.c_ignpar) then 0x4 else 0) lor
                  (if (tios.c_parmrk) then 0x8 else 0) lor
                  (if (tios.c_inpck)  then 0x10 else 0) lor
                  (if (tios.c_istrip) then 0x20 else 0) lor
                  (if (tios.c_inlcr)  then 0x40 else 0) lor
                  (if (tios.c_igncr)  then 0x80 else 0) lor
                  (if (tios.c_icrnl)  then 0x100 else 0) lor
                  (if (tios.c_ixon)   then 0x400 else 0) lor
                  (if (tios.c_ixoff)  then 0x1000 else 0) 
        in
    let c_oflag = (if (tios.c_opost) then 0x1 else 0) 
        in
    let c_cflag = (if (tios.c_clocal) then 0x800 else 0) lor
                  (if (tios.c_cread) then 0x80 else 0) lor
                  (match tios.c_csize with 
                        | 5 -> 0x00;
                        | 6 -> 0x10;
                        | 7 -> 0x20;
                        | 8 -> 0x30;
                        | _ -> 0) lor
                  (if (tios.c_cstopb = 2) then 0x40 else 0) lor
                  (if (tios.c_hupcl)  then 0x400 else 0) lor
                  (if (tios.c_parenb) then 0x100 else 0) lor
                  (if (tios.c_parodd)  then 0x200 else 0) 
        in
    let c_lflag = (if (tios.c_echo) then 0x8 else 0) lor
                  (if (tios.c_echoe) then 0x10 else 0) lor
                  (if (tios.c_echok) then 0x20 else 0) lor
                  (if (tios.c_echonl) then 0x40 else 0) lor
                  (if (tios.c_icanon)  then 0x2 else 0) lor
                  (if (tios.c_isig)  then 0x1 else 0) lor
                  (if (tios.c_noflsh)  then 0x80 else 0) 
        in
    let p = buf_put_int16 ~buf:buf ~pos:0 ~int16:c_iflag in
    let p = buf_put_int16 ~buf:buf ~pos:p ~int16:c_oflag in
    let p = buf_put_int16 ~buf:buf ~pos:p ~int16:c_cflag in
    let p = buf_put_int16 ~buf:buf ~pos:p ~int16:c_lflag in

    (* Amoeba doesn't use the first one *)
    let p = buf_put_int8 ~buf:buf ~pos:p ~int8:0 in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_vintr) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_vquit) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_verase) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_vkill) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_veof) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_vstart) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(int_of_char tios.c_vstop) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(tios.c_vmin) in
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(0) in       (* VSUSP ? *)
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(0) in       (* ? *)
    let p = buf_put_int8 ~buf:buf ~pos:p 
                         ~int8:(0) in       (* ? *)
    p

let tty_tc_unmarshal ~buf =
    let p,c_iflag = buf_get_int16 ~buf:buf ~pos:0 in
    let p,c_oflag = buf_get_int16 ~buf:buf ~pos:p in
    let p,c_cflag = buf_get_int16 ~buf:buf ~pos:p in
    let p,c_lflag = buf_get_int16 ~buf:buf ~pos:p in

    (* Amoeba doesn't use the first one *)
    let p,_ = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vintr = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vquit = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_verase = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vkill = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_veof = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vstart = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vstop = buf_get_int8 ~buf:buf ~pos:p in
    let p,c_vmin = buf_get_int8 ~buf:buf ~pos:p in
    let p,_ = buf_get_int8 ~buf:buf ~pos:p in   (* VSUSP ? *)
    let p,_ = buf_get_int8 ~buf:buf ~pos:p in   (* ? *)
    let p,_ = buf_get_int8 ~buf:buf ~pos:p in   (* ? *)
    {   c_ignbrk = (c_iflag land 0x1 = 0x1);
        c_brkint = (c_iflag land 0x2 = 0x2);
        c_ignpar = (c_iflag land 0x4 = 0x4); 
        c_parmrk = (c_iflag land 0x8 = 0x8);
        c_inpck  = (c_iflag land 0x10 = 0x10);
        c_istrip = (c_iflag land 0x20 = 0x20);
        c_inlcr  = (c_iflag land 0x40 = 0x40);
        c_igncr  = (c_iflag land 0x80 = 0x80);
        c_icrnl  = (c_iflag land 0x100 = 0x100); 
        c_ixon   = (c_iflag land 0x400 = 0x400); 
        c_ixoff  = (c_iflag land 0x1000 = 0x1000); 
        c_opost  = (c_oflag land 0x1 = 0x1);
        c_obaud  = 38400; 
        c_ibaud  = 38400; 
        c_csize  = (match (c_cflag land 0x30) with
                    | 0x00 -> 5;
                    | 0x10 -> 6;
                    | 0x20 -> 7;
                    | 0x30 -> 8;
                    | _ -> 0); 
        c_cstopb = 1; 
        c_cread  = (c_cflag land 0x80 = 0x80);
        c_parenb = (c_cflag land 0x100 = 0x100); 
        c_parodd = (c_cflag land 0x200 = 0x200); 
        c_hupcl  = (c_cflag land 0x400 = 0x400); 
        c_clocal = (c_cflag land 0x800 = 0x800);
        c_isig   = (c_lflag land 0x1 = 0x1);  
        c_icanon = (c_lflag land 0x2 = 0x2); 
        c_noflsh = (c_lflag land 0x80 = 0x80); 
        c_echo   = (c_lflag land 0x8 = 0x8);
        c_echoe  = (c_lflag land 0x10 = 0x10); 
        c_echok  = (c_lflag land 0x20 = 0x20); 
        c_echonl = (c_lflag land 0x40 = 0x40); 
        c_vintr  = char_of_int c_vintr;
        c_vquit  = char_of_int c_vquit; 
        c_verase = char_of_int c_verase; 
        c_vkill  = char_of_int c_vkill; 
        c_veof   = char_of_int c_veof;
        c_veol   = '\255'; 
        c_vmin   = c_vmin; 
        c_vtime  = 0; 
        c_vstart = char_of_int c_vstart;
        c_vstop  = char_of_int c_vstop;
    }






(*
** Server IO threads.
*)
let tty_in server =
    let cb = server.tty_cb_in in
    let b = buf_create 100 in
    let stat = ref std_OK in
    while (cb_empty cb >= 0 && !stat = std_OK)
    do
        let n = server.tty_input b 100 in
        if (n>0) then
            ignore(cb_putbn cb b 0 n)
        else
            stat := std_INTR;
    done
    
let tty_out server =
    let cb = server.tty_cb_out in
    let b = buf_create 100 in
    let stat = ref std_OK in
    while (cb_full cb >= 0 && !stat = std_OK)
    do
        let n = cb_getbn cb b 0 1 100 in
        if (n > 0) then
        begin
            server.tty_output b n;
        end
        else
            stat := std_INTR;
        
    done



(*
** Server TTY write and read functions used by server loop.
*)

let tty_write ~cb ~buf ~size =
    let n = cb_putbn cb buf 0 size in
    if (n < 0) then
        std_INTR
    else
        std_OK

let tty_read ~cb ~buf ~size =
    let n = cb_getbn cb buf 0 1 size in
    n

(*
** Server loop
*)
let tty_server_loop ~server
    =

    if (server.tty_io_init = false) then
    begin
        (*
        ** Start IO threads
        *)
        server.tty_io_init <- true;
        ignore (thread_create tty_in server);
        ignore (thread_create tty_out server);
    end;

    if server.tty_verbose > 0 then
    begin
        sys_log Sys_info "TTY server thread started...\n";
    end;    

    (*
    ** Catch transaction interrupts...
    *)
    sig_catch sig_TRANS (fun i -> 
            if server.tty_verbose > 1 then
                info (sprintf "got sig_TRANS signal %d" i); 
            cb_close server.tty_cb_in;
        );

    server.tty_dying <- false;

    let initial = ref false in
    let on_exit cap =
            let stat = if (server.tty_dying = false) then
                       begin
                            server.tty_dying <- true;
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (server.tty_nthr-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up server.tty_sema;
                            done;
                            std_OK
                       end
                       else
                            std_OK
                       in
            stat
    in        
    try
    begin
        let ibuf = buf_create server.tty_bufsize in     (* request buffer *)
        let obuf = buf_create server.tty_bufsize in    (* reply buffer   *)
        
        let hdr_rep = header_new () in
        let replen = ref 0 in
        
        let getport = server.tty_getport in
        let putport = server.tty_putport in
        let checkfield = server.tty_checkfield in

        if (server.tty_verbose > 2) then
        begin
            info ("started with port "^(ar_port getport));
        end;

        while (true)
        do
        try
          begin
                        
            let stat,reqlen,hdr_req = getreq (getport,
                                              ibuf,
                                              server.tty_bufsize) in

            replen := 0;
            hdr_rep.h_size <- 0;
            hdr_rep.h_status <- std_OK;
            hdr_rep.h_priv <- priv_copy nilpriv;


            let priv =hdr_req.h_priv in

            (
                if server.tty_verbose > 1 then
                begin
                    info (sprintf "TTY request %s" (ar_priv hdr_req.h_priv));
                    info (sprintf "TTY command %d" (let Command com =
                                                    hdr_req.h_command in com));
                end;

                match hdr_req.h_command with

                | com when (com = tty_WRITE) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    let size = ref (min hdr_req.h_size server.tty_bufsize) in
                        
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                        if server.tty_verbose > 1 then
                            info (sprintf "tty_WRITE [size=%d]" !size);

                        stat := tty_write ~cb:server.tty_cb_out
                                          ~buf:ibuf
                                          ~size:!size;
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !size;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                | com when (com = tty_READ) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let size = ref (min hdr_req.h_size server.tty_bufsize) in
                        
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                        if server.tty_verbose > 1 then
                            info "tty_READ";

                        let n =  tty_read ~cb:server.tty_cb_in
                                          ~buf:obuf
                                          ~size:!size in
                        if (n<0) then
                        begin
                            stat := std_IOERR;
                            size := 0;
                        end
                        else
                        begin
                            stat := std_IOERR;
                            size := n;
                        end;
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !size;
                    hdr_rep.h_status <- !stat;
                    replen := !size;
                end;

                (*
                ** Terminal IO Structire control
                *)

                | com when (com = tios_GETATTR) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in

                    if server.tty_verbose > 1 then
                        info "tty_GETATTR";

                    let tios = Unix.tcgetattr Unix.stdin in

                    let p = tty_tc_marshal tios obuf in
                    
                    hdr_rep.h_size <- p;
                    hdr_rep.h_status <- std_OK;
                    replen := p;
                end;

                | com when (com = tios_SETATTR) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
                    if server.tty_verbose > 1 then
                        info (sprintf "tty_SETATTR reqlen=%d" reqlen);
                        

                    let stat = ref std_OK in
                    let tios = tty_tc_unmarshal ibuf in
                    Unix.tcsetattr Unix.stdin TCSANOW tios;
                end;

                (*
                ** STD_INFO request
                *)

                | com when (com = std_INFO) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in

        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"+ (terminal)"
                            in
                            pos := pos';
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                (*
                ** STD_STATUS request
                *)

                | com when (com = std_STATUS) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let pos = ref 0 in
                    if (prv_decode ~prv:priv ~rand:checkfield
                            = true) then
                    begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"Terminal Ok (TODO)"
                            in
                            pos := pos';
                    end
                    else
                        stat := std_DENIED;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
                end;

                (*
                ** Close the server thread.
                ** Only possible with the super capability.
                **
                *)
                | com when (com = std_EXIT) ->
                begin
                    initial := not (server.tty_dying);
                    let rights = prv_rights hdr_req.h_priv in
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (obj = 0 &&
                       (prv_decode ~prv:hdr_req.h_priv 
                                   ~rand:server.tty_checkfield)
                       ) = true then
                    begin 
                        let stat = on_exit { cap_port = hdr_req.h_port;
                                             cap_priv = priv} in

                        hdr_rep.h_status <- stat;
                        ignore( putrep (hdr_rep,obuf,!replen));
                        raise Exit;            
                    end
                    else
                    begin
                        hdr_rep.h_status <- std_DENIED;
                    end;
                end;

                | _ -> 
                begin
                    let Command com = hdr_req.h_command in
                    if server.tty_verbose > 1 then
                        info (sprintf "std_COMBAD com=%d" com);
                    hdr_rep.h_status <- std_COMBAD;
                end;

            );
            
            ignore( putrep (hdr_rep,obuf,!replen));
            if server.tty_verbose > 1 then
                info (sprintf "reply stat=%s" (err_why hdr_rep.h_status));

          end
          with
            | Buf_overflow ->
            begin
                replen := 0;
                hdr_rep.h_status <- std_ARGBAD;
                ignore( putrep (hdr_rep,obuf,!replen));
            end;
        done;
    end
    with
        | Exit -> 
                  info "TTY: Server thread exited normally."; 
                  if (!initial) then
                    Sema.sema_up server.tty_sema

(*
** Initialize and start the TTY server. The input and output functions
** are optional. Defaults are used if None is specified.
*)


let tty_init ~bufsize ~nthr ~input ~output =
    let getport = uniqport () in
    let putport = priv2pub getport in
    let checkfield = uniqport () in

    let srv = {
        tty_sema = sema_create 0;
        tty_nthr = nthr;
        tty_bufsize = bufsize;
        tty_verbose = 0;
        tty_getport = getport;
        tty_putport = putport;
        tty_checkfield = checkfield;

        (*
        ** Initial tios used for restore 
        *)
        tty_tios = Unix.tcgetattr Unix.stdin;   
        tty_dying = false;
        tty_io_init = false;
        tty_cb_in = cb_create (2*bufsize);
        tty_cb_out = cb_create (2*bufsize);
        tty_output = 
            (match output with
             | Some f -> f;
             | None -> (fun buf len ->
                            ignore(Unix.writeb Unix.stdin buf 0 len)));
        tty_input = 
            (match input with
             | Some f -> f;
             | None -> (fun buf size ->
                        Unix.readb Unix.stdin buf 0 size));

    } in

    for i = 1 to srv.tty_nthr
    do
        ignore(thread_create (fun () -> 
                tty_server_loop ~server:srv
               ) ());
    done;
    std_OK,srv



let tty_restore ~server =
    Unix.tcsetattr Unix.stdin TCSANOW server.tty_tios


let tty_cap ~server =
    let pubcap = {cap_port = server.tty_putport;
                  cap_priv = prv_encode ~obj:(Objnum 0)
                                        ~rights:prv_all_rights    
                                        ~rand:server.tty_checkfield
            } in
    pubcap


(*
** Publish TTY server capability
*)

let tty_publ_cap ~server ~path =
  try
  begin
    let cap = tty_cap server in

    let stat,_ = name_lookup path in
    if stat = std_OK then
    begin
        let stat = name_delete path in
        if stat <> std_OK then
        begin
            if server.tty_verbose > 2 
                then info "tty_publ_file: name_delete failed.";
            raise (Error stat);
        end;
    end;
    let stat = name_append path cap in
    if stat <> std_OK then
    begin
        if server.tty_verbose > 2
            then info "tty_publ_file: name_append failed.";
        raise (Error stat)
    end;
    std_OK;
  end
  with Error stat -> stat
    

let tty_exit ~server =
  try
  begin
    tty_restore server;
    let stat = std_exit (tty_cap server) in
    if (stat <> std_OK) then
    begin
        info "Can't terminate terminal server TTY!"; 
        raise (Error stat);
    end;
    for i = 1 to server.tty_nthr
    do
        Sema.sema_down server.tty_sema;
    done;
    info "server exited normally."; 
    std_OK
  end
  with Error stat -> stat

