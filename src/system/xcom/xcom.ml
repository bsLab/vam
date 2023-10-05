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
**    $CREATED:     5.1.2006
**    $VERSION:     1.06
**
**    $INFO:
**
**  Stream communication frontend.
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

open VX_types
open VX_common
open VX_box
open VX_log
open VX_navigator

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
              [
                MinHeight 500; MinWidth 700;Background "white"
              ]
                     
let closewindow () = 
  Display.closeDisplay (top#display)

type eol = 
  | Eol_no
  | Eol_nl
  | Eol_nlcr
  | Eol_cr
  | Eol_special of (int * int)          (* # chars, timeout msec *)
 
type com_param = {
  mutable com_enable : bool;
  mutable com_path: string;
  mutable com_cap : capability; 
  mutable com_baud: int;
  mutable com_bits: int;
  mutable com_stops: int;
  mutable com_parity: int;
  mutable com_out_eol : eol;
  mutable com_in_eol : eol;
  mutable com_sema : semaphore;
}

let com = {
  com_enable = false;
  com_path = "";
  com_cap = nilcap;
  com_baud = 9600;
  com_bits = 8;
  com_stops = 1;
  com_parity = 0;
  com_out_eol = Eol_nl;
  com_in_eol = Eol_nl;
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
let s2_host = {
    inp_desc = 160,"Host machine";
    inp_attr = [Border []];
    inp_buts = [];
    inp_str = "/hosts/";
    inp_vx = None;
    } 

let s2_comport = {
    inp_desc = 160,"Communication port";
    inp_attr = [Border []];
    inp_buts = [];
    inp_str = "ser:00";
    inp_vx = None;
    } 

let s2_baud = {
    sel_desc = "Baud Rate";
    sel_attr = [ExpandX true;
        Rows [| [Active true];[];[];[];[]; |];
      ];
    sel_choices = [|
      "9600";
      "19600";
      "38400";
      "57600";
      "115200";
      |];
    sel_sel = [0];
    sel_mutual = true;
    sel_action = (fun i -> 
        (match i with
          | 0 -> com.com_baud <- 9600;
          | 1 -> com.com_baud <- 19600;
          | 2 -> com.com_baud <- 38400;
          | 3 -> com.com_baud <- 57600;
          | 4 -> com.com_baud <- 115200;
          | _ -> progerr ""
        );
        if com.com_enable then
        begin
          let stat = std_set_params com.com_cap
                                    ["enable","0";
                                     "baud",string_of_int com.com_baud;
                                     "enable","1"] in
          if stat <> std_OK then
            log (sprintf "std_set_params failed: %s" (err_why stat));                                     
        end;
      );
    sel_vx = None;
    }

let s2_bits = {
    sel_desc = "Character bit size";
    sel_attr = [ExpandX true;
        Rows [| [];[Active true]; |];
      ];
    sel_choices = [|
      "7";
      "8";
      |];
    sel_sel = [1];
    sel_mutual = true;
    sel_action = (fun i -> 
        (match i with
          | 0 -> com.com_bits <- 7;
          | 1 -> com.com_bits <- 8;
          | _ -> progerr ""
        );
        if com.com_enable then
        begin
          let stat = std_set_params com.com_cap
                                    ["enable","0";
                                     "bits",string_of_int com.com_bits;
                                     "enable","1"] in
          if stat <> std_OK then
            log (sprintf "std_set_params failed: %s" (err_why stat));                                     
        end;
      );
    sel_vx = None;
    }

let s2_stops = {
    sel_desc = "Number of stop bits";
    sel_attr = [ExpandX true;
        Rows [| [Active true]; [] |];
      ];
    sel_choices = [|
      "1";
      "2";
      |];
    sel_sel = [0];
    sel_mutual = true;
    sel_action = (fun i -> 
        (match i with
          | 0 -> com.com_bits <- 1;
          | 1 -> com.com_bits <- 2;
          | _ -> progerr ""
        );
        if com.com_enable then
        begin
          let stat = std_set_params com.com_cap
                                    ["enable","0";
                                     "stops",string_of_int com.com_stops;
                                     "enable","1"] in
          if stat <> std_OK then
            log (sprintf "std_set_params failed: %s" (err_why stat));                                     
        end;
      );
    sel_vx = None;
    }

let s2_parity = {
    sel_desc = "Parity";
    sel_attr = [ExpandX true;
        Rows [| [Active true]; []; [] |];
      ];
    sel_choices = [|
      "Off";
      "even";
      "odd";
      |];
    sel_sel = [0];
    sel_mutual = true;
    sel_action = (fun i -> 
        (match i with
          | 0 -> com.com_parity <- 0;
          | 1 -> com.com_parity <- 1;
          | 2 -> com.com_parity <- 2;
          | _ -> progerr ""
        );
        if com.com_enable then
        begin
          let stat = std_set_params com.com_cap
                                    ["enable","0";
                                     "stops",string_of_int com.com_stops;
                                     "enable","1"] in
          if stat <> std_OK then
            log (sprintf "std_set_params failed: %s" (err_why stat));                                     
        end;
      );
    sel_vx = None;
    }


let s2_out = {
    sel_desc = "Output end of message marker";
    sel_attr = [ExpandX true;
        Rows [| [];[Active true];[];[]; |];    
      ];
    sel_choices = [|
      "-";
      "NL";
      "NL + CR";
      "CR";
      |];
    sel_sel = [1];
    sel_mutual = true;
    sel_action = (fun i -> 
        match i with
        | 0 -> com.com_out_eol <- Eol_no;
        | 1 -> com.com_out_eol <- Eol_nl;
        | 2 -> com.com_out_eol <- Eol_nlcr;
        | 3 -> com.com_out_eol <- Eol_cr;
        | _ -> progerr "");
    sel_vx = None;
    }

let s2_in = {
    sel_desc = "Input end of message marker";
    sel_attr = [ExpandX true;
        Rows [| [];[Active true];[];[]; |];    
      ];
    sel_choices = [|
      "Number of chars and timeout";
      "NL";
      "NL + CR";
      "CR";
      |];
    sel_sel = [1];
    sel_mutual = true;
    sel_action = (fun i -> 
        match i with
        | 0 -> com.com_in_eol <- Eol_special (0,0);
        | 1 -> com.com_in_eol <- Eol_nl;
        | 2 -> com.com_in_eol <- Eol_nlcr;
        | 3 -> com.com_in_eol <- Eol_cr;
        | _ -> progerr "");
    sel_vx = None;
    }

let s2_in_param = {
    tab_desc = "Receive message parameters";
    tab_attr = [IpadX 5;IpadY 2;];
    tab_rows = [||];
    tab_cols = [|
        [|
              "# Chars",[Background "grey90";
                         Border [Sides [B_left;B_bottom;B_top;]]];
              "0",
                 [But [
                 Sym S_OK;Sym S_ENTER;
                 ActionSSS (fun str st -> 
                        let failed = not (protects(
                            let v = int_of_string str in
                            match com.com_in_eol with
                            | Eol_special (num,tmo) ->
                                com.com_in_eol <- Eol_special (v,tmo);
                            | _ -> ();
                            )) in
                        if not failed then
                            St_Submitted
                        else
                            St_Failed
                     );
                 Frame ReliefRaised; Color "grey80";
                ];
                Width 100;Mutable true; 
                          Border [Sides [B_top;B_bottom;]]];
              "Timeout [ms]",[Background "grey90";
                              Border [Sides [B_bottom;B_top;]]];
              "0",
                 [But [
                 Sym S_OK;Sym S_ENTER;
                 ActionSSS (fun str st -> 
                        let failed = not (protects(
                            let v = int_of_string str in
                            match com.com_in_eol with
                            | Eol_special (num,tmo) ->
                                com.com_in_eol <- Eol_special (num,v);
                            | _ -> ();
                            )) in
                        if not failed then
                            St_Submitted
                        else
                            St_Failed
                     );
                 Frame ReliefRaised; Color "grey80";
                ];
                Width 100;Mutable true; Border [Sides [B_top;B_bottom;B_right]]];
        |];            
      |];
    tab_set = None;
    tab_get = None;
    tab_vx = None;
    }

let s2_buts = {
    but_attr = [AdjustX false;];
    but_cols = 
    [|
      [|
      ("Enable" ,(fun () ->
          if not com.com_enable then
          begin
            com.com_path <- s2_host.inp_str ^ "/" ^ s2_comport.inp_str;
            let stat,cap = name_lookup com.com_path in
            if stat <> std_OK then
              log (sprintf "name_lookup failed: %s" (err_why stat))
            else
            begin
              com.com_cap <- cap;
              let stat = std_set_params com.com_cap (
                                [
                                    "baud",string_of_int com.com_baud;
                                    "bits",string_of_int com.com_bits;
                                    "stop",string_of_int com.com_stops;
                                    "parity",string_of_int com.com_parity;
                                ] @ 
                                (
                                    match com.com_in_eol with
                                    | Eol_no ->
                                        [
                                            "tmo","0";
                                            "thr","1";
                                            "eol","0";
                                        ];
                                    | Eol_nl ->
                                        [
                                            "tmo","0";
                                            "thr","0";
                                            "eol","2";
                                        ];
                                    | Eol_cr
                                    | Eol_nlcr ->
                                        [
                                            "tmo","0";
                                            "thr","0";
                                            "eol","1";
                                        ];
                                    | Eol_special (num,tmo) ->
                                        [
                                            "tmo",string_of_int tmo;
                                            "thr",string_of_int num;
                                            "eol","0";
                                        ];
                                            
                                ) @
                                [ "enable","1" ])
                                in
              if stat <> std_OK then
                log (sprintf "std_set_params failed: %s" (err_why stat));
              sema_up com.com_sema;
              com.com_enable <- true;
            end;  
          end;
          ));
      ("Disable" ,(fun () ->
          if com.com_enable then
          begin
              let stat = std_set_params com.com_cap
                                        ["enable","0"] in
              if stat <> std_OK then
                log (sprintf "std_set_params failed: %s" (err_why stat));
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
                    Background "grey90"];
    logedit_cols = 80;
    logedit_rows = 1000;
    logedit_add = None;
    logedit_clear = None;
    logedit_vx = None;
  }


let s1 = [
    Space 20;
    Logedit s1_log;
  ]

let s2 = [
    Space 10;
    Input s2_host;
    Input s2_comport;
    Select s2_baud;
    Select s2_bits;
    Select s2_stops;
    Select s2_parity;
    Select s2_out;
    Select s2_in;
    Table s2_in_param;
    Space 10;
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


    top#setWM_NAME "XCOM Serial Controller (C) 2006 BSSLAB Dr. Stefan Bosse";
    top#container_add nav#contained;
    top#show;
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
            let eol = match com.com_out_eol with
                      | Eol_no -> "";
                      | Eol_nl -> "\n";
                      | Eol_cr -> "\r";
                      | Eol_nlcr -> "\n\r";
                      | _ -> "" in

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
        while true
        do
            (*
            ** wait untill communication port was enabled
            *)
            sema_down com.com_sema;
            while com.com_enable
            do
                let stat,str = stream_read com.com_cap 80 in
                if stat = std_OK && str <> "" then
                begin
                    let str' = Str.global_replace 
                                (Str.regexp "\r") "" str in
                    let str' = Str.global_replace 
                                (Str.regexp "\n") "" str' in
                    log str';
                end;
            done;            
        done;
        ) ());

    try
      loop ();
    with Exit -> ()
              

  
            
            