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
**    $INITIAL:     (C) 2006 BSSLAB
**    $CREATED:     7.1.2006
**    $VERSION:     1.05
**
**    $INFO:
**
**  JTAG-TAP programming frontend.
**
**
**    $ENDOFINFO
**
*)


open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Myenv
open Bytebuf
open Machtype
open Buf
open Unix
open Thread
open Printf
open Name
open Sema
open Bstream
open Shell_exec
open Mutex

open VX_types
open VX_common
open VX_box
open VX_log
open VX_navigator


(*
** Device ID to Vendor/Name mapping (32 bit)
*)
let device_of_id str =
  let str' = ref "" in
  protect (str' := String.sub str 6 ((String.length str) - 6));
  match !str' with
  | "0614093" -> "Xilinx XC2S100";
  | "5034093" -> "Xilinx XC18V01";
  | "5023093" -> "Xilinx XC18V512";
  | _ -> (sprintf "Unknown device: <%s>" str)
  


let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
              [
                MinHeight 500; MinWidth 700;Background "white"
              ]
                     
let closewindow () = 
  Display.closeDisplay (top#display)

let last_line = ref ""
let com_event = thread_create_event ()
 
type com_param = {
  mutable com_enable : bool;
  mutable com_path: string;
  mutable com_cap : capability; 
  mutable com_sema : semaphore;
}

let com = {
  com_enable = false;
  com_path = "";
  com_cap = nilcap;
  com_sema = sema_create 0;
}

let log_fun = ref None
let log str =
  match !log_fun with
  | Some f -> f str;
  | None ->
    print_string str;
    print_newline ()
  

(*
** S1: Setup
*)
let s2_server = {
    inp_desc = 160,"Server path";
    inp_attr = [Border []];
    inp_buts = [];
    inp_str = "/server/jtag0";
    inp_vx = None;
    } 


let s2_host = {
    inp_desc = 160,"Controller Host";
    inp_attr = [Border []];
    inp_buts = [];
    inp_str = "/hosts/";
    inp_vx = None;
} 
                    
let s2_cont = {
    file_desc = "JTAG controller program binary (Amoeba)";
    file_attr = [];
    file_action = (fun str -> 
        print_string str; print_newline ();
        );
    file_path = Unix_path "/amoeba/Amcross/driver/i86_pc/jtag";
    file_top = false;
    file_edit = true;    
    file_vx = None;
} 
                    
let pdef1 = {
    exec_name = "JTAG server control";
    exec_src = Unix_src (Exec_path (
                "/unix"^(match s2_cont.file_path with
                         | Unix_path p -> p;
                         | _ -> progerr "")));
    exec_dst = Amoeba_dst (Exec_path (s2_host.inp_str^"/proc"));
    exec_args = [s2_host.inp_str;];
    exec_env = [
        Env_cappath ("TOD",(s2_host.inp_str^"/tod")); 
        Env_cappath ("RANDOM",(s2_host.inp_str^"/random")); 
    ];
    exec_ops = [
        Exec_coldstart;        
        Exec_poll ((Exec_path "/server/jtag0"),1);
        Exec_stop;
    ];
}
let update_pdef1 () =
    pdef1.exec_src <- 
                Unix_src (Exec_path (
                "/unix"^(match s2_cont.file_path with
                         | Unix_path p -> p;
                         | _ -> progerr "")));    
    pdef1.exec_args <- [s2_host.inp_str];
    pdef1.exec_dst <- Amoeba_dst (Exec_path (s2_host.inp_str^"/proc"));
    pdef1.exec_env <-
                [
                Env_cappath ("TOD",(s2_host.inp_str^"/tod")); 
                Env_cappath ("RANDOM",(s2_host.inp_str^"/random")); 
                ]
                                                                                                                                                       

let s2_proc1 = {
    pro_desc = "JTAG server process \\rightarrow  control";
    pro_attr = [OpadY 5];
    pro_def = pdef1;
    pro_log = None;
    pro_obj = None;
}

let s2_buts = {
    but_attr = [AdjustX false;];
    but_cols = 
    [|
      [|
      ("Enable" ,(fun () ->
          if not com.com_enable then
          begin
            com.com_path <- s2_server.inp_str;
            let stat,cap = name_lookup com.com_path in
            if stat <> std_OK then
              log (sprintf "name_lookup failed: %s" (err_why stat))
            else
            begin
              let stat,_ = std_info cap 10 in
              if stat = std_OK then
              begin
                com.com_cap <- cap;
                sema_up com.com_sema;
                com.com_enable <- true;
              end
              else
                log (sprintf "std_info failed: %s" (err_why stat))
            end;  
          end;
          ));
      ("Disable" ,(fun () ->
          if com.com_enable then
          begin
              com.com_enable <- false;
          end;
          ));
      |];
    |];
    but_group = true;
    but_vx = None;
    }

let s1_log = {
    logedit_attr = [Height 300;
                    Border [];
                    Background "grey80"];
    logedit_cols = 80;
    logedit_rows = 1000;
    logedit_add = None;
    logedit_clear = None;
    logedit_vx = None;
  }

let s1_xsvf = {
    file_desc = "XSVF file";
    file_attr = [];
    file_action = (fun str -> 
        print_string str; print_newline ();
        );
    file_path = Unix_path (Unix.getcwd ());
    file_edit = true;    
    file_top = false;
    file_vx = None;
} 

let load_lock = sema_create 1

let s1_buts = {
    but_attr = [AdjustX false;];
    but_cols = 
    [|
      [|
      ("Load" ,(fun () ->
        if com.com_enable then
        begin
          let feed file = 
            let buf = ref "" in
            let good = protects (
                match file with
                | Unix_path file -> 
                    let st = Unix.stat file in
                    log (sprintf "Loading %s (%d bytes)..." file st.st_size);
                    if st.st_kind = S_REG then
                    begin
                      let size = ref st.st_size in
                      let off = ref 0 in
                      buf := String.create !size;
                      let fd = Unix.openfile file [O_RDONLY] 0 in
                      while !size > 0
                      do
                          let m = min !size 512 in
                          let n = Unix.read fd !buf !off m in
                          size := !size - n;
                          off := !off + n;
                      done;
                      Unix.close fd;
                    end;
                | Amoeba_path _ -> raise Exit;
              ) in    
            if good then
            begin        
              let stat = stream_write com.com_cap "XSVF;" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));
              let stat = stream_write com.com_cap !buf in
              if stat <> std_OK then
                  log (sprintf "ERR: write of file failed: %s" (err_why stat));
              
            end
            else
              log "ERR: invalid XSVF file";
            sema_up load_lock;
            in
          if (sema_trydown load_lock 0) then
                __(thread_create feed s1_xsvf.file_path)
          else
                log "ERR: already loading!"; 
        end
        else
            log "ERR: Communication port not enabled";
            
          ));
      ("Enable" ,(fun () ->
          if com.com_enable then
          begin
              log "Enable...";
              let stat = stream_write com.com_cap "ENABLE 1;" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      ("Disable" ,(fun () ->
          if com.com_enable then
          begin
              log "Disable...";
              let stat = stream_write com.com_cap "ENABLE 0;" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      ("Reset" ,(fun () ->
          if com.com_enable then
          begin
              log "Reset...";
              let stat = stream_write com.com_cap "STATE RESET;" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      ("Identify" ,(fun () ->
          if com.com_enable then
          begin
              log "Identifying device...";
              let stat = stream_write com.com_cap "STATE RESET;" in
              __(thread_await com_event 500);
              let stat = stream_write com.com_cap "SDR 32 TDI 0;" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));
              (*
              ** Wait for response TDO
              *)
              __(thread_await com_event 1000);
              log (device_of_id !last_line);
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      ("Status" ,(fun () ->
          if com.com_enable then
          begin
              let stat,str = std_info com.com_cap 100 in
              if stat = std_OK then              
                  log (sprintf "JTAG status: %s" str)
              else
                  log (sprintf "JTAG status: failed: %s" 
                        (err_why stat));
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      ("FPGA done?" ,(fun () ->
          if com.com_enable then
          begin
              log "Checking Done Status of FPGA...";
              let stat = stream_write com.com_cap "STATE RESET;" in
              __(thread_await com_event 500);
              let stat = stream_write com.com_cap "SIR 05 TDI (1F) TDO (05) MASK (04);" in
              if stat <> std_OK then
                  log (sprintf "ERR: command write failed: %s" (err_why stat));

              (*
              ** Wait for response TDO
              *)
          end
          else
            log "ERR: Communication port not enabled";
            
          ));
      |];
    |];
    but_group = false;
    but_vx = None;
    }

let s1 = [
    Space 20;
    Logedit s1_log;
    Space 20;
    File s1_xsvf;
    Buttons s1_buts;
  ]

let s2 = [
    Space 10;
    Input s2_server;
    Input s2_host;
    File s2_cont;
    Space 10;
    Proc s2_proc1;
    Buttons s2_buts;
  ]

let desc = [
    S1 ("Main",s1);
    S1 ("Setup",s2);
    S1 ("Exit",[Action (fun () -> closewindow ();
                                  exit 0;)])
  ]
    

let nav = new VX_navigator.s1_view root
              top#container
              desc
              [
                ExpandX true;
                ExpandY true;
              ]

let main args =


    top#setWM_NAME "XTAP JTAG controller (C) 2006 BSSLAB Dr. Stefan Bosse";
    top#container_add nav#contained;
    top#show;

    
    (*
    ** Host name 
    *)
    let w = get_some s2_host.inp_vx in
    w#configure_col 0 1 [ActionSU (fun _ -> update_pdef1 ())];

    (*
    ** JTAG binary
    *)
    let w = get_some s2_cont.file_vx in
    w#set_action (fun _ -> update_pdef1 ();); 

    (*
    ** Log window management
    *)
    let log1 = get_some s1_log.logedit_vx in
    log1#add_lines ["Ready.";">> "];
    let cur_row = ref 1 in
    log1#set_cursor 3 !cur_row;
    log1#set_editrange 3 !cur_row !cur_row;
                       
    let last_in = ref 0 in
    log1#set_action_nl (fun row ->
        let line = log1#get_line row in

        log1#add_lines [">> "];
        cur_row := min (!cur_row + 1) 999;
        log1#set_cursor 3 !cur_row;
        log1#set_editrange 3 !cur_row !cur_row;

        if com.com_enable then
        begin
            let line = String.sub line 3 ((String.length line)-3) in
            let eol = "\n" in
            let stat = stream_write com.com_cap (line^eol) in
            if stat <> std_OK then
                log (sprintf "ERR: %s" (err_why stat));
        end
        else
        begin
            log "ERR: Communication port not enabled!";
        end;
        
      );
    log_fun := Some (fun str ->
        log1#set_line !cur_row str;
        log1#add_lines [">> "];
        cur_row := min (!cur_row + 1) 999;
        log1#set_cursor 3 !cur_row;
        log1#set_editrange 3 !cur_row !cur_row;
        );

    log "Starting receiver thread...";
    __(thread_create (fun () ->
        let curstr = ref "" in
        while true
        do
            (*
            ** wait untill communication port was enabled
            *)
            sema_down com.com_sema;
            while com.com_enable
            do
                let stat,str = stream_read com.com_cap 1000 in

                if stat = std_OK && str <> "" then
                begin
                    let strl = Str.split (Str.regexp "\n") str in
                    let n = List.length strl in
                    (
                      match strl with
                      | hd::[] -> log1#set_line !cur_row hd;
                                  last_line := hd;
                                  log1#add_lines [">> "];
                      | hd::tl -> log1#set_line !cur_row hd;
                                  log1#add_lines (tl @ [">> "]);
                                  List.iter (fun l -> last_line := l) tl;
                      | [] -> ();
                    );
                    cur_row := min (!cur_row + n) 999;
                    log1#set_cursor 3 !cur_row;
                    log1#set_editrange 3 !cur_row !cur_row;
                    for i = 1 to n
                    do
                      thread_switch ();
                      thread_wakeup com_event;
                      thread_switch ();
                    done;
                end;
            done;            
        done;
        ) ());

    try
      loop ();
    with Exit -> ()
              

  
  
            
            