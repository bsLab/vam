open Xtypes
open WX_types
open WX_tree
open Unix


open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Stdcom2
open Cap_env
open Thread
open Printf
open Sema
open Bstream

open Xnc_widget
open Xnc_prog

let add_log str =
                xw_main.xw_log.xw_log_str <-
                    xw_main.xw_log.xw_log_str ^ str;
                xw_main.xw_log.xw_log_text#set_lines
                    xw_main.xw_log.xw_log_str;
                xw_main.xw_log.xw_log_text#update;
                xw_main.xw_log.xw_log_ady#set_pos 1 1

let clear_log () =
                xw_main.xw_log.xw_log_str <- "";
                xw_main.xw_log.xw_log_text#set_lines
                    xw_main.xw_log.xw_log_str;
                xw_main.xw_log.xw_log_text#update;
                xw_main.xw_log.xw_log_ady#set_pos 1 1

(*
** CNC server capability
*)
let cnc_cap = ref nilcap

(*
** Encoder server cap (serial port)
*)
let enc_cap = ref nilcap

(*
** Coordinate viewer threads
*)

let cnc_status_sema = sema_create 1

let cnc_status () =
    sema_down cnc_status_sema;
    let milli v =
        abs (v/1000) in
    let micro v =
        let v1 = (v/1000)*1000 in
        abs ((v-v1)/10) in
    let sign v =
        if v >= 0 then ' ' else '-' in

    let stat = ref std_OK in
    while (!stat = std_OK) 
    do
        (*
        ** Get the status from the cnc server:
        ** Format: RUN:_ X:_ Y:_ Z:_
        *)
        let stat',str = std_status !cnc_cap 100 in
        stat := stat';
        if stat' = std_OK then
        begin
            let len = String.length str in
            let str = if str.[len-1] = '\n' 
                        then String.sub str 0 (len-1)
                        else str in
            let vals = Str.split (Str.regexp " ") str in
            if (List.length vals) >= 4 then
            begin
                let runs = Str.split (Str.regexp ":") (List.nth vals 0) in 
                let xs = Str.split (Str.regexp ":") (List.nth vals 1) in 
                let ys = Str.split (Str.regexp ":") (List.nth vals 2) in 
                let zs = Str.split (Str.regexp ":") (List.nth vals 3) in 
                if (List.length xs) = 2 &&
                   (List.length ys) = 2 &&
                   (List.length zs) = 2 then
                begin
                    let x = int_of_string (List.nth xs 1) in
                    let y = int_of_string (List.nth ys 1) in
                    let z = int_of_string (List.nth zs 1) in
                    let xw = xw_main.xw_cnc_coord in
                    xw.xw_t3lvb_value1#set_string 
                        (sprintf "%c%4d.%.2d" (sign x) (milli x) (micro x));
                    xw.xw_t3lvb_value2#set_string 
                        (sprintf "%c%4d.%.2d" (sign y) (milli y) (micro y));
                    xw.xw_t3lvb_value3#set_string 
                        (sprintf "%c%4d.%.2d" (sign z) (milli z) (micro z));
                    xw.xw_t3lvb_vbar#update;
                end;
            end;
            ignore(thread_delay 1 SEC);
        end;
    done;
    sema_up cnc_status_sema


let enc_sema = sema_create 1

let enc_status () =
    let fail str =
        clear_log ();
        add_log (str^"\n");
        sema_up enc_sema;
        raise Exit
        in
    let write str =
        let stat = stream_write !enc_cap str in
        if stat <> std_OK
            then fail "Can't write to serial port"
        in
    let read () =
        let stat,str = stream_read !enc_cap 100 in
        if stat <> std_OK
            then fail "Can't read from serial port";
        str
        in

    let milli v =
        abs (v/1000) in
    let micro v =
        let v1 = (v/1000)*1000 in
        abs ((v-v1)/10) in
    let sign v =
        if v >= 0 then ' ' else '-' in

    let stat = ref std_OK in
    while (!stat = std_OK) 
    do
        sema_down enc_sema;


        let stat',str = stream_read !enc_cap 100 in
        stat := stat';
        if stat' = std_OK then
        begin
            write "X";
            let strx = read () in
            write "Y";
            let stry = read () in
            write "Z";
            let strz = read () in

            if strx <> "" &&
               stry <> "" &&
               strz <> "" then
            begin
                let x = int_of_string strx in
                let y = int_of_string stry in
                let z = int_of_string strz in
                let xw = xw_main.xw_cnc_coord in
                xw.xw_t3lvb_value1#set_string 
                        (sprintf "%c%4d.%.2d" (sign x) (milli x) (micro x));
                xw.xw_t3lvb_value2#set_string 
                        (sprintf "%c%4d.%.2d" (sign y) (milli y) (micro y));
                xw.xw_t3lvb_value3#set_string 
                        (sprintf "%c%4d.%.2d" (sign z) (milli z) (micro z));
                xw.xw_t3lvb_vbar#update;
            end;

            sema_up enc_sema;
            ignore(thread_delay 1 SEC);
        end;
    done

let cnc_inited = ref false
let enc_inited = ref false

let init_cnc path =
    if not !cnc_inited then
    begin
        let stat,cap = name_lookup path in
        if stat = std_OK then
        begin
            let stat,str = std_info cap 100 in
            if stat = std_OK then
            begin
                cnc_cap := cap;
                cnc_inited := true;
                ignore(thread_create cnc_status ());
            end;
            stat
        end
        else
            stat
    end
    else
        std_EXISTS

let init_enc path =
    if not !enc_inited then
    begin
        let stat,cap = name_lookup path in
        if stat = std_OK then
        begin
            let stat,str = std_info cap 100 in
            if stat = std_OK then
            begin
                (*
                ** Configure serial port
                *)
                let stat = std_set_params cap ["baud","9600";
                                               "thr","16";
                                               "tmo","100";
                                               "enable","1"] in
                if stat = std_OK then
                begin
                    enc_cap := cap;
                    enc_inited := true;
                    ignore(thread_create enc_status ());
                end;
                stat
            end
            else
                stat
        end
        else
            stat
    end
    else    
        std_EXISTS

let cnc_run = ref false 
let cnc_stop = ref false
let cnc_lock = mu_create ()

(*
** Execute one string
*)
let cnc_eval str =

    (*
    ** Check first for already running execution
    *)
    mu_lock cnc_lock;
    if !cnc_run then 
    begin
        mu_unlock cnc_lock;
        raise Exit;
    end;

    clear_log ();

    if !cnc_cap = nilcap then
    begin
        add_log "CNC server not initialized.\n";
        mu_unlock cnc_lock;
        raise Exit;
    end;

    
    (*
    ** Replace ';' with newlines.
    *)
    let str = (Str.global_replace (Str.regexp ";") "\n" str)^"\n" in

    let thr () =
        add_log "Executing command...\n";
        let stat,res = std_exec !cnc_cap [str] in

        if stat <> std_OK then
        begin
            add_log (sprintf "%s\n" (err_why stat));
        end
        else if not !cnc_stop then
            add_log (sprintf "%s\n" res)
        else 
            add_log (sprintf "User break: %s\n" res);

        if !cnc_stop then
            cnc_stop := false;
        mu_lock cnc_lock;
        cnc_run := false;
        mu_unlock cnc_lock;        
        in

    ignore(thread_create thr ());

    cnc_run := true;
    mu_unlock cnc_lock;
    std_OK,"Starting CNC service thread..."

(*
** Load program (but not execute it).
*)
let cnc_load str =

    (*
    ** Check first for already running execution
    *)
    mu_lock cnc_lock;
    if !cnc_run then 
    begin
        mu_unlock cnc_lock;
        raise Exit;
    end;

    clear_log ();

    if !cnc_cap = nilcap then
    begin
        add_log "CNC server not initialized.\n";
        mu_unlock cnc_lock;
        raise Exit;
    end;

    

    let thr () =
        add_log "Loading program ...\n";
        let stat,res = std_exec !cnc_cap [str] in

        if stat <> std_OK then
        begin
            add_log (sprintf "%s\n" (err_why stat));
        end
        else if not !cnc_stop then
            add_log (sprintf "%s\n" res)
        else 
            add_log (sprintf "User break: %s\n" res);

        if !cnc_stop then
            cnc_stop := false;
        mu_lock cnc_lock;
        cnc_run := false;
        mu_unlock cnc_lock;        
        in

    ignore(thread_create thr ());

    cnc_run := true;
    mu_unlock cnc_lock;
    std_OK,"Starting CNC service thread..."


(*
** Execute cnc block list
*)

let cnc_exec bl =

    (*
    ** Check first for already running execution
    *)
    mu_lock cnc_lock;
    if !cnc_run then
    begin
        mu_unlock cnc_lock;
        raise Exit;
    end;


    if !cnc_cap = nilcap then
    begin
        add_log "CNC server not initialized.\n";
        mu_unlock cnc_lock;        
        raise Exit;
    end;
    if bl = [] then
    begin
        add_log "No blocks selected.\n";
        mu_unlock cnc_lock;
        raise Exit;
    end; 
    (*
    ** At least one stepper motor must be locked
    *)
    if not xw_main.xw_axis.xw_l4b_state1 &&
       not xw_main.xw_axis.xw_l4b_state2 &&
       not xw_main.xw_axis.xw_l4b_state3 then
    begin
        add_log "No axis stepper motor in on state!\n";
        mu_unlock cnc_lock;
        raise Exit;
    end;


    let stat = ref std_OK in

    let thr () =
      try
      begin
        cnc_run := true;
        let hd = List.hd bl in

        let header = 
            if hd.b_type <> Header then
            begin
                add_log "Warning: no header found!\n";
                ""
            end
            else
                hd.b_cont in
        
        let blocks = List.tl bl in
        if blocks <> [] then
          List.iter (fun b ->
                (*
                ** Each program block must be preceeded
                ** by the header block (if any).
                *)
                let exec_str = header ^ b.b_cont ^ "\nGO\n" in
                add_log (sprintf "Executing %s:\n" b.b_name);
                let stat,str' = std_exec !cnc_cap [exec_str] in

                if stat <> std_OK then
                begin
                    add_log (sprintf "Failure: %s\n" (err_why stat));
                    raise Exit;
                end
                else
                    add_log (str'^"\n");

                if !cnc_stop = true  then
                begin
                    add_log (sprintf "User break: %s\n" str');
                    cnc_stop := false;
                    raise Exit;
                end;
    
                (*
                ** Unselect this block
                *)
                let d = match b.b_desc with
                        | Some d -> d;
                        | None -> failwith "desc";
                    in
                d.pb_desc#configure [Foreground "Grey50"];
                d.pb_desc#refresh;
                d.pb_selected <- false;

            ) blocks
        else
        begin
                (*
                ** Each program block must be preceeded
                ** by the header block (if any).
                *)
                let exec_str = header ^ "\nGO\n" in
                let stat,str' = std_exec !cnc_cap [exec_str] in

                if stat <> std_OK then
                begin
                    add_log (sprintf "Failure: %s\n" (err_why stat));
                    raise Exit;
                end
                else
                    add_log (str'^"\n");

                if !cnc_stop = true  then
                begin
                    add_log (sprintf "User break: %s\n" str');
                    cnc_stop := false;
                    raise Exit;
                end;

        end;

        add_log "Result: All Done.\n";
        mu_lock cnc_lock;
        cnc_run := false;
        mu_unlock cnc_lock;
      end
      with 
        | Exit -> mu_lock cnc_lock;
                  cnc_run := false;
                  mu_unlock cnc_lock;
      in

    ignore (thread_create thr ());
    cnc_run := true;
    mu_unlock cnc_lock;
    std_OK,"Starting CNC service thread...\n"


(*
** Stop execution of current CNC program.
*)
let cnc_break () =
    if !cnc_run = false ||
       !cnc_cap = nilcap then
    begin
        add_log "CNC program not running or CNC server not initialized.\n";
        std_ARGBAD
    end
    else
    begin
        let stat = std_set_params !cnc_cap ["break","1"] in
        cnc_stop := true;
        stat
    end


(*
** Serial Port Encoder command evaluation
*)

let enc_eval str =
    sema_down enc_sema;
    let fail str =
        clear_log ();
        add_log (str^"\n");
        sema_up enc_sema;
        raise Exit
        in
    let write str =
        let stat = stream_write !enc_cap str in
        if stat <> std_OK
            then fail "Can't write to serial port"
        in
    let read () =
        let stat,str = stream_read !enc_cap 100 in
        if stat <> std_OK
            then fail "Can't read from serial port";
        str
        in
    if not !enc_inited then
    begin
        clear_log ();
        add_log "Encoder not initialized.\n";
        sema_up enc_sema;
        raise Exit; 
    end;
    write str;
    ignore (read ());
    sema_up enc_sema;
    add_log "Encoder command exectued.\n";
    ""
