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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     29.4.2005
**    $VERSION:     1.06
**
**    $INFO:
**
**  SYSLOG service - client part
**
** All system servers (and of cources user servers, too)
** can and ***should*** write their messages (info,
** warnings, errors) to this system unique log server.
** The syslog server keeps all incoming messages 
** in a local buffer of size SYSLOG_BUFSIZE.
** If the buffer overflows, the syslog server writes the buffer
** to disk with time and and date reference in the
** directory SYSLOG_PATH.
**
** Local, on the client side, all messages are buffered
** with a circular buffer. A different deliver thread put
** all buffered messages out of the circular buffer to the
** syslog server with the generic Amoeba rpc interface.
** After the SYSLOG_CHECK timeout is passed, the deliver thread
** try to connect to the syslog server or send buffered
** measseges.
** The reason for this procedure:
** - The client can start before the syslog server is ready/started
** - messages are collected and can be send all in one transaction
**
**
** The syslog services must be initialized using the syslog_init () 
** function! Otherwise, sys_log will print only the messages to
** the standard output channel.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Cmdreg
open Syslog_common
open Circbuf
open Thread
open Printf
open Unix (* for time/localtime *)
open Cap_env
open Stderr
open Stdcom
open Dir
open Buf
open Bytebuf
open Rpc

let __ = ignore 

(*
** The circular buffer, created on first sys_log call.
*)

let syslog_cb = ref nilcb
let syslog_inited = ref false
let syslog_silent = ref false
let syslog_lock = mu_create () 
let syslog_deliver_connected = ref false
let syslog_server = ref syslog_SERVERPATH
let syslog_chan = ref Pervasives.stdout
let syslog_dying = ref false
(*
** Print functions. Can be overriden.
*)
let syslog_print_str = ref (fun str -> output_string !syslog_chan str)
let syslog_print_char = ref (fun ch -> output_char !syslog_chan ch)

let syslog_level_string level =
    match level with
    | Sys_debug     -> "SYS_DEBUG";
    | Sys_info      -> "SYS_INFO";
    | Sys_notice    -> "SYS_NOTICE";
    | Sys_warn      -> "SYS_WARN";
    | Sys_err       -> "SYS_ERROR";
    | Sys_fatal     -> "SYS_FATAL";
    | Sys_print     -> "SYS_PRINT";
    | Sys_start     -> "SYS_STARTUP"

let partial_msg = ref ""

(*
** This threads tries to deliver the messages.
** The syslog server can be specified with:
**
**  1. SYSLOG environment capability
**  2. ROOT environment and the syslog_server path (either set with
**     syslog_init or default path)
*)

let syslog_deliver_thread () =
    (*
    ** Try to 'connect' to the syslog server.
    *)
    let server_cap = ref nilcap in

    let connect () =
      let stat,syslogcap = get_env_cap "SYSLOG" in
      if stat <> std_OK then
      begin
        (*
        ** get the root capability first
        *)
        let stat,rootcap = get_env_cap "ROOT" in
        let found = ref false in

        if stat = std_OK then
        while (not !found) 
        do
            let stat,cap = dir_lookup rootcap !syslog_server in
            if stat = std_OK then
            begin
                let stat,_ = std_info cap 10 in
                if stat = std_OK then
                begin
                    (*
                    ** We've found the server and it's up.
                    *)
                    server_cap := cap;
                    found := true;
                end
                else
                begin
                    server_cap := nilcap;
                    found := false;
                end;
            end
            else
            begin
                    server_cap := nilcap;
                    found := false;
            end;
            __(thread_delay syslog_CHECK SEC);
        done
        else
        begin
            print_string "syslog: connect: neither ROOT nor SYSLOG envcap specified! Giving up.";
            thread_exit ();
        end;
      end
      else
      begin
        let found = ref false in
        while (not !found) 
        do
            let stat,_ = std_info syslogcap 10 in
            if stat = std_OK then
            begin
                (*
                ** We've found the server and it's up.
                *)
                server_cap := syslogcap;
                found := true;
            end
            else
            begin
                server_cap := nilcap;
                found := false;
            end;
            __(thread_delay syslog_CHECK SEC);
        done
      end
      in

    connect ();
    syslog_deliver_connected := true;

    let buf = buf_create syslog_BUFSIZE in
    while (true)
    do
        let str = cb_gets !syslog_cb 1 syslog_BUFSIZE in
        let len = buf_put_string buf 0 str  in

        let transferred = ref false in

        while (not !transferred)
        do
            let hdr = {(header_new ()) with
                        h_port = !server_cap.cap_port;
                        h_priv = !server_cap.cap_priv;
                        h_command = syslog_WRITE;
                        h_size = len;
                      } in
            let stat,_,hdr' = trans (hdr,buf,len,nilbuf,0) in
            if stat <> std_OK then
            begin
                syslog_deliver_connected := false;
                connect ();
                syslog_deliver_connected := true;
            end
            else
                transferred := true;
        done;
    done



(*
** A system log message is both printed to the standard error channel
** of the current process, and collected and send to a specified system
** log server. 
*)

let sys_log level fmt =
    if !syslog_inited then
    begin
        (*
        ** extract the message 
        *)
        let message =   
            let fmt = (Obj.magic fmt : string) in
            let len = String.length fmt in
            let dest = Buffer.create (len + 16) in
            let rec doprn i =
            if i >= len then 
            begin
                let res = Buffer.contents dest in
                Buffer.clear dest;  (* just in case sprintf is partially applied *)
                Obj.magic res
            end 
            else
            match String.unsafe_get fmt i with
            | '%' -> scan_format fmt i cont_s cont_a cont_t
            |  c  -> Buffer.add_char dest c; doprn (succ i)
            and cont_s s i =
                Buffer.add_string dest s; doprn i
            and cont_a printer arg i =
                Buffer.add_string dest (printer () arg); doprn i
            and cont_t printer i =
                Buffer.add_string dest (printer ()); doprn i
          in doprn 0 in
    

        mu_lock syslog_lock;

        (*
        ** Original implementation used comment string from process server...
        *)
        let myname = if Sys.argv <> [||] then Sys.argv.(0) else "VAM?" in
        let tms = Unix.localtime (Unix.time ()) in
        let timestr = sprintf "%02d.%02d.%02d %02d:%02d:%02d" 
                           tms.tm_mday
                           (tms.tm_mon+1)
                           (if tms.tm_year > 99 
                                then (tms.tm_year-100)
                                else (tms.tm_year))
                            tms.tm_hour
                            tms.tm_min
                            tms.tm_sec in

        (*
        ** Maybe the current message is only partial (missing newline
        ** at end of string). In this case, we store the message
        ** in temporary buffer. If the rest of the message arrives,
        ** we kick the complete message into the cb!
        *)

        let msg_len = String.length message in
        let has_nl = message.[msg_len-1] = '\n' in

        if !partial_msg = "" && has_nl then
        begin
            (*
            ** A new and complete message.
            *)
            let str = sprintf "[ %s at %s by %s ]\n%s"
                            (syslog_level_string level)
                            timestr
                            myname 
                            message in
    
            let len = String.length str in
            if (cb_empty !syslog_cb) >= (len+1) then
            begin
                (*
                ** store the message and the message header
                *)
                __(cb_puts !syslog_cb str);
            end;           
        end
        else if !partial_msg <> "" && not has_nl then
        begin
            (*
            ** Some more parts of an incomplete message.
            *)
            partial_msg := !partial_msg ^ message
        end
        else if !partial_msg <> "" && has_nl then
        begin
            (*
            ** The message seems to be complete and has already
            ** a header.
            *)
            let str = !partial_msg ^ message in
            let len = String.length str in
            if (cb_empty !syslog_cb) >= (len+1) then
            begin
                (*
                ** save the message with additional informations
                *)
                __(cb_puts !syslog_cb str);
            end;           
             
        end
        else if !partial_msg = "" && not has_nl then
        begin
            (*
            ** The start of an incomplete message.
            *)
            partial_msg := sprintf "[ %s at %s by %s ]\n%s"
                            (syslog_level_string level)
                            timestr
                            myname 
                            message;
        end;
        mu_unlock syslog_lock;
    end;

    let print fmt =
        let fmt = (Obj.magic fmt : string) in
        let len = String.length fmt in
        let fl () = Pervasives.flush !syslog_chan in
        let rec doprn i =
            if i >= len then fl ();
            if i >= len then Obj.magic () else
            match String.unsafe_get fmt i with
            | '%'  -> scan_format fmt i cont_s cont_a cont_t
            | '\n' -> if not !syslog_silent then
                      begin   
                        !syslog_print_char '\n';
                        Pervasives.flush !syslog_chan;
                      end;
                      doprn (succ i);
            |  c   -> if not !syslog_silent then
                            !syslog_print_char c; 
                     doprn (succ i)
          and cont_s s i =
            if not !syslog_silent then !syslog_print_str s; doprn i
          and cont_a printer arg i =
            if not !syslog_silent then printer arg; doprn i
          and cont_t printer i =
            if not !syslog_silent then printer; doprn i
          in doprn 0
        in

    (*
    ** print the message to the (specified standard) output channel
    *)
    print fmt

(*
** Init syslog service. Without calling this function, the above sys_log 
** function only prints the messages to the (specified standard) 
** output channel!
*)

let syslog_init server =
    mu_lock syslog_lock;
    if server <> "" then
        syslog_server := server;

    if !syslog_inited = false then
    begin
        syslog_inited := true;
        syslog_cb := cb_create syslog_BUFSIZE;
        __(thread_create (fun () -> syslog_deliver_thread ()) ());            
        at_exit (fun () ->
                    mu_lock syslog_lock;
                    syslog_dying := true;
                    (*
                    ** Is there a partial message? Kick it out 
                    ** before it's too late.
                    *)
                    __(cb_puts !syslog_cb !partial_msg);
                    partial_msg := "";
                    mu_unlock syslog_lock;
                    cb_close !syslog_cb;
                    (*
                    ** Give the deliver thread a chance to send the data...
                    *)
                    __(thread_delay 500 MILLISEC);
                );
    end;
    mu_unlock syslog_lock

(*
** Close and flush the syslog cb!!!
*)
let syslog_close () =
    if !syslog_inited then
    begin
        mu_lock syslog_lock;
        syslog_dying := true;
        (*
        ** Is there a partial message? Kick it out before it's too late.
        *)
        __(cb_puts !syslog_cb !partial_msg);
        partial_msg := "";
        mu_unlock syslog_lock;

        cb_close !syslog_cb;
        (*
        ** Give the deliver thread a chance to send the data...
        *)
        __(thread_delay 500 MILLISEC);
    end

let syslog_mute on =
    syslog_silent := on

let syslog_chan chan =
    syslog_chan := chan

let syslog_output_str f =
    syslog_print_str := f

let syslog_output_char f =
    syslog_print_char := f

