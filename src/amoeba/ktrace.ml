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
**    $INITIAL:     (C) 2005
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.05
**
**    $INFO:
**
**  Kernel and network trace client interface.
**
**  Limited to maximal number of 32000 events.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Cmdreg
open Stderr
open Stdcom
open Rpc
open Bytebuf
open Buf
open Printf

let trace_INIT      = Command (trace_FIRST_COM)
let trace_DELETE    = Command (trace_FIRST_COM+1)
let trace_START     = Command (trace_FIRST_COM+2)
let trace_STOP      = Command (trace_FIRST_COM+3)
let trace_READ      = Command (trace_FIRST_COM+4)
let trace_RESET     = Command (trace_FIRST_COM+5)

let trace_NOMOREDATA = 65535

type trace_TYPE =
    | Trace_NETWORK         (* network packet monitoring *)
    | Trace_THREAD          (* thread/process monitoring *)

let trace_init ~cap ~kind ~size ~mask =
    match kind with
    |  Trace_NETWORK ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_INIT;
        hdr.h_size <- size;
        hdr.h_extra <- 0x10;
        hdr.h_offset <- mask;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end;
    | Trace_THREAD ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_INIT;
        hdr.h_size <- size;
        hdr.h_extra <- 0x20;
        hdr.h_offset <- mask;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end


let trace_delete ~cap ~kind =
    match kind with
    |  Trace_NETWORK ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_DELETE;
        hdr.h_extra <- 0x10;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end;
    | Trace_THREAD ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_DELETE;
        hdr.h_extra <- 0x20;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end

let trace_reset ~cap ~kind ~mask =
    match kind with
    |  Trace_NETWORK ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_RESET;
        hdr.h_extra <- 0x10;
        hdr.h_offset <- mask;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end;
    | Trace_THREAD ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_RESET;
        hdr.h_extra <- 0x20;
        hdr.h_offset <- mask;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end

let trace_start ~cap ~kind =
    match kind with
    |  Trace_NETWORK ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_START;
        hdr.h_extra <- 0x10;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end;
    | Trace_THREAD ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_START;
        hdr.h_extra <- 0x20;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end

let trace_stop ~cap ~kind =
    match kind with
    |  Trace_NETWORK ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_STOP;
        hdr.h_extra <- 0x10;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end;
    | Trace_THREAD ->
    begin
        let hdr = header_new () in
        hdr.h_port <- cap.cap_port;
        hdr.h_priv <- cap.cap_priv;
        hdr.h_command <- trace_STOP;
        hdr.h_extra <- 0x20;
        let stat,_,hdr' = trans (hdr,nilbuf,0,nilbuf,0) in
        if (stat <> std_OK) then 
            stat
        else
            hdr'.h_status
    end

let trace_print_str ~cap ~kind =
  try
  begin
    match kind with
    |  Trace_NETWORK ->
    begin
        let lastev = ref 0 in
        let more = ref 0 in
        let hdr = header_new () in
        let str = ref "" in
        let stat = ref std_OK in
        let overflow = ref false in
        
        let bufsize = 8020 in
        let buf = buf_create bufsize in

        str := (sprintf "%10s %23s %23s %s\n"
                       "Time" "net-srcaddr" "net-dstaddr" "Name"); 
        str := !str ^ (sprintf "           %23s %23s\n"
                       "flip-srcaddr" "flip-dstaddr"); 
        str := !str ^ (sprintf "    %16s %6s %6s %6s\n"
                        "FlType" "ActHop" "MaxHop" "Length" ); 
        str := !str ^ (sprintf "    %16s %6s\n"
                        "FrType" "FrCnt"); 

        (*
        ** FLIP fragment control
        *)
        let fc_type_str fct =
            match fct with
            | 0x0 -> "FC_DATA";
            | 0x1 -> "FC_REQCREDIT";
            | 0x2 -> "FC_CREDIT";
            | 0x3 -> "FC_MCREQ";
            | 0x4 -> "FC_MCACK";
            | 0x5 -> "FC_MCNAK";
            | 0x6 -> "FC_ABSCREDIT";
            | 0x7 -> "FC_FAST_UNIDATA";
            | 0x8 -> "FC_FAST_MULTIDATA";
            | _ -> sprintf "?%x" fct;
            in 

        (*
        ** FLIP message type
        *)
        let fh_type_str fht =
            match fht with
            | 0x1 -> "FL_T_LOCATE";
            | 0x2 -> "FL_T_HEREIS";
            | 0x3 -> "FL_T_UNIDATA";
            | 0x4 -> "FL_T_MULTIDATA";
            | 0x5 -> "FL_T_NOTHERE";
            | 0x6 -> "FL_T_UNTRUSTED";
            | 0x7 -> "FL_T_GONE";
            | x -> sprintf "?%x" fht;
            in 
    
        let lasttime = ref (-1) in
        let curtime  = ref (-1) in

        while (!stat = std_OK && !more != trace_NOMOREDATA)
        do
            hdr.h_port <- cap.cap_port;
            hdr.h_priv <- cap.cap_priv;
            hdr.h_command <- trace_READ;
            hdr.h_extra <- 0x10;
            hdr.h_offset <- !lastev;
            hdr.h_size <- bufsize;

            let stat',_,hdr' = trans (hdr,nilbuf,0,buf,bufsize) in
            if (stat' <> std_OK) then 
                stat := stat'
            else
                stat := hdr'.h_status;

            if (!stat = std_OVERFLOW) then
            begin
                (*
                ** Not really an error. The trace buffer limit
                ** was reached.
                *)
                overflow := true;
                stat := std_OK;
            end; 
            more := hdr'.h_extra;           
            (*
            ** Extract events from buffer...
            *)
            let events = hdr'.h_size in
            lastev := !lastev + events;
            let pos = ref 0 in
            for i = 1 to events
            do
                let pos',time = buf_get_int32 buf !pos in
                let pos',net_dstaddr = buf_get_string buf pos' in
                let pos',net_srcaddr = buf_get_string buf pos' in
                let pos',fc_type  = buf_get_int8 buf pos' in
                let pos',fc_cnt  = buf_get_int8 buf pos' in
                let pos',fh_version  = buf_get_int8 buf pos' in
                let pos',fh_type  = buf_get_int8 buf pos' in
                let pos',fh_flags  = buf_get_int8 buf pos' in
                let pos',fh_res  = buf_get_int8 buf pos' in
                let pos',fh_act_hop  = buf_get_int16 buf pos' in
                let pos',fh_max_hop  = buf_get_int16 buf pos' in
                let pos',fh_dstaddr = buf_get_string buf pos' in
                let pos',fh_srcaddr = buf_get_string buf pos' in
                let pos',fh_length = buf_get_int32 buf pos' in
                let pos',fh_offset = buf_get_int32 buf pos' in
                let pos',fh_total = buf_get_int32 buf pos' in

                let pos',name = buf_get_string buf pos' in
                let time = time land 0x3FFFFFFF in
                if (!lasttime = -1) then
                begin
                    lasttime := time;
                    curtime := 0;
                end
                else if (time < !lasttime) then
                begin
                    curtime := time + (0x3FFFFFFF - !lasttime);
                end
                else        
                    curtime := time - !lasttime;

                pos := pos';
                str := !str ^
                        (sprintf "%10d %23s %23s %s\n"
                                    (!curtime) net_srcaddr net_dstaddr name);
                str := !str ^
                        (sprintf "           %23s %23s\n"
                                    fh_srcaddr fh_dstaddr);
                str := !str ^
                        (sprintf "    %16s %6d %6d %6d\n"
                         (fh_type_str fh_type) fh_act_hop fh_max_hop fh_length);
                str := !str ^
                        (sprintf "    %16s %6d\n"
                         (fc_type_str fc_type) fc_cnt);
            done;

        done;
        !stat,!str
    end;
    | Trace_THREAD ->
    begin
        let more = ref 0 in
        let lastev = ref 0 in
        let hdr = header_new () in
        let str = ref "" in
        let stat = ref std_OK in
        let overflow = ref false in
        
        let bufsize = 8000 in
        let buf = buf_create bufsize in

        str := (sprintf "%12s %4s %4s %8s %s\n"
                       "Time" "Tid" "Pid" "Argument" "Name"); 

        let lasttime = ref (-1) in
        let curtime  = ref (-1) in

        while (!stat = std_OK && !more != trace_NOMOREDATA)
        do
            hdr.h_port <- cap.cap_port;
            hdr.h_priv <- cap.cap_priv;
            hdr.h_command <- trace_READ;
            hdr.h_extra <- 0x20;
            hdr.h_offset <- !lastev;
            hdr.h_size <- bufsize;


            let stat',_,hdr' = trans (hdr,nilbuf,0,buf,bufsize) in
            if (stat' <> std_OK) then 
                stat := stat'
            else
                stat := hdr'.h_status;

            if (!stat = std_OVERFLOW) then
            begin
                (*
                ** Not really an error. The trace buffer limit
                ** was reached.
                *)
                overflow := true;
                stat := std_OK;
            end; 
            more := hdr'.h_extra;           
            (*
            ** Extract events from buffer...
            *)
            let events = hdr'.h_size in
            lastev := !lastev + events;
            let pos = ref 0 in
            for i = 1 to events
            do
                let pos',time = buf_get_int32 buf !pos in
                let pos',tid  = buf_get_int16 buf pos' in
                let pos',pid  = buf_get_int16 buf pos' in
                let pos',flgs = buf_get_int32 buf pos' in
                let pos',arg  = buf_get_int32 buf pos' in
                let pos',name = buf_get_string buf pos' in
        
                pos := pos';

                let time = time land 0x3FFFFFFF in
                if (!lasttime = -1) then
                begin
                    lasttime := time;
                    curtime := 0;
                end
                else if (time < !lasttime) then
                begin
                    curtime := time + (0x3FFFFFFF - !lasttime);
                end
                else        
                    curtime := time - !lasttime;

                str := !str ^
                        (sprintf "%12d %4d %4d %8x %s\n"
                                    time tid pid arg name);
            done;

        done;
        !stat,!str
    end
  end
  with
    | Buf_overflow -> std_SYSERR,"Buffer overflow"
#if 0
    | _ -> std_SYSERR,"Unknown error"
#endif

let trace_print_file ~cap ~kind ~name =
  try
  begin
    let oc = open_out name in

    match kind with
    |  Trace_NETWORK ->
    begin
        let lastev = ref 0 in
        let more = ref 0 in
        let hdr = header_new () in
        let str = ref "" in
        let stat = ref std_OK in
        let overflow = ref false in
        
        let bufsize = 8020 in
        let buf = buf_create bufsize in

        str := (sprintf "%10s %23s %23s %s\n"
                       "Time" "net-srcaddr" "net-dstaddr" "Name"); 
        str := !str ^ (sprintf "           %23s %23s\n"
                       "flip-srcaddr" "flip-dstaddr"); 
        str := !str ^ (sprintf "    %16s %6s %6s %6s\n"
                        "FlType" "ActHop" "MaxHop" "Length" ); 
        str := !str ^ (sprintf "    %16s %6s\n"
                        "FrType" "FrCnt"); 

        output_string oc !str;
        let fc_type_str fct =
            match fct with
            | 0x0 -> "FC_DATA";
            | 0x1 -> "FC_REQCREDIT";
            | 0x2 -> "FC_CREDIT";
            | 0x3 -> "FC_MCREQ";
            | 0x4 -> "FC_MCACK";
            | 0x5 -> "FC_MCNAK";
            | 0x6 -> "FC_ABSCREDIT";
            | 0x7 -> "FC_FAST_UNIDATA";
            | 0x8 -> "FC_FAST_MULTIDATA";
            | x -> sprintf "?%x" fct;
            in 

        let fh_type_str fht =
            match fht with
            | 0x1 -> "FL_T_LOCATE";
            | 0x2 -> "FL_T_HEREIS";
            | 0x3 -> "FL_T_UNIDATA";
            | 0x4 -> "FL_T_MULTIDATA";
            | 0x5 -> "FL_T_NOTHERE";
            | 0x6 -> "FL_T_UNTRUSTED";
            | 0x7 -> "FL_T_GONE";
            | x -> sprintf "?%x" fht;
            in 
    
        let lasttime = ref (-1) in
        let curtime  = ref (-1) in

        while (!stat = std_OK && !more != trace_NOMOREDATA)
        do
            hdr.h_port <- cap.cap_port;
            hdr.h_priv <- cap.cap_priv;
            hdr.h_command <- trace_READ;
            hdr.h_extra <- 0x10;
            hdr.h_offset <- !lastev;
            hdr.h_size <- bufsize;

            let stat',_,hdr' = trans (hdr,nilbuf,0,buf,bufsize) in
            if (stat' <> std_OK) then 
                stat := stat'
            else
                stat := hdr'.h_status;

            if (!stat = std_OVERFLOW) then
            begin
                (*
                ** Not really an error. The trace buffer limit
                ** was reached.
                *)
                overflow := true;
                stat := std_OK;
            end; 
            more := hdr'.h_extra;           
            (*
            ** Extract events from buffer...
            *)
            let events = hdr'.h_size in



            lastev := !lastev + events;
            let pos = ref 0 in
            for i = 1 to events
            do
                let pos',time = buf_get_int32 buf !pos in
                let pos',net_dstaddr = buf_get_string buf pos' in
                let pos',net_srcaddr = buf_get_string buf pos' in
                let pos',fc_type  = buf_get_int8 buf pos' in
                let pos',fc_cnt  = buf_get_int8 buf pos' in
                let pos',fh_version  = buf_get_int8 buf pos' in
                let pos',fh_type  = buf_get_int8 buf pos' in
                let pos',fh_flags  = buf_get_int8 buf pos' in
                let pos',fh_res  = buf_get_int8 buf pos' in
                let pos',fh_act_hop  = buf_get_int16 buf pos' in
                let pos',fh_max_hop  = buf_get_int16 buf pos' in
                let pos',fh_dstaddr = buf_get_string buf pos' in
                let pos',fh_srcaddr = buf_get_string buf pos' in
                let pos',fh_length = buf_get_int32 buf pos' in
                let pos',fh_offset = buf_get_int32 buf pos' in
                let pos',fh_total = buf_get_int32 buf pos' in

                let pos',name = buf_get_string buf pos' in
                let time = time land 0x3FFFFFFF in
                if (!lasttime = -1) then
                begin
                    lasttime := time;
                    curtime := 0;
                end
                else if (time < !lasttime) then
                begin
                    curtime := time + (0x3FFFFFFF - !lasttime);
                end
                else        
                    curtime := time - !lasttime;

                pos := pos';
                str := 
                        (sprintf "%10d %23s %23s %s\n"
                                    (!curtime) net_srcaddr net_dstaddr name);
                str := !str ^
                        (sprintf "           %23s %23s\n"
                                    fh_srcaddr fh_dstaddr);
                str := !str ^
                        (sprintf "    %16s %6d %6d %6d\n"
                         (fh_type_str fh_type) fh_act_hop fh_max_hop fh_length);
                str := !str ^
                        (sprintf "    %16s %6d\n"
                         (fc_type_str fc_type) fc_cnt);
                output_string oc !str;
            done;

        done;
        close_out oc;
        !stat
    end;
    | Trace_THREAD ->
    begin
        let lastev = ref 0 in
        let more = ref 0 in
        let hdr = header_new () in
        let str = ref "" in
        let stat = ref std_OK in
        let overflow = ref false in
        
        let bufsize = 8000 in
        let buf = buf_create bufsize in

        str := (sprintf "%12s %4s %4s %8s %s\n"
                       "Time" "Tid" "Pid" "Argument" "Name"); 

        output_string oc !str;
        let lasttime = ref (-1) in
        let curtime  = ref (-1) in

        while (!stat = std_OK && !more != trace_NOMOREDATA)
        do
            hdr.h_port <- cap.cap_port;
            hdr.h_priv <- cap.cap_priv;
            hdr.h_command <- trace_READ;
            hdr.h_extra <- 0x20;
            hdr.h_offset <- !lastev;
            hdr.h_size <- bufsize;


            let stat',_,hdr' = trans (hdr,nilbuf,0,buf,bufsize) in
            if (stat' <> std_OK) then 
                stat := stat'
            else
                stat := hdr'.h_status;

            if (!stat = std_OVERFLOW) then
            begin
                (*
                ** Not really an error. The trace buffer limit
                ** was reached.
                *)
                overflow := true;
                stat := std_OK;
            end; 
            more := hdr'.h_extra;           
            (*
            ** Extract events from buffer...
            *)
            let events = hdr'.h_size in
            lastev := !lastev + events;
            let pos = ref 0 in
            for i = 1 to events
            do
                let pos',time = buf_get_int32 buf !pos in
                let pos',tid  = buf_get_int16 buf pos' in
                let pos',pid  = buf_get_int16 buf pos' in
                let pos',flgs = buf_get_int32 buf pos' in
                let pos',arg  = buf_get_int32 buf pos' in
                let pos',name = buf_get_string buf pos' in
        
                pos := pos';

                let time = time land 0x3FFFFFFF in
                if (!lasttime = -1) then
                begin
                    lasttime := time;
                    curtime := 0;
                end
                else if (time < !lasttime) then
                begin
                    curtime := time + (0x3FFFFFFF - !lasttime);
                end
                else        
                    curtime := time - !lasttime;

                str := 
                        (sprintf "%12d %4d %4d %8x %s\n"
                                    time tid pid arg name);
                output_string oc !str;
            done;

        done;
        close_out oc;
        !stat
    end
  end
  with
    | Buf_overflow -> std_SYSERR
#if 0
    | _ -> std_SYSERR
#endif
