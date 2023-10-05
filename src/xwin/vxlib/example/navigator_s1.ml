open Amoeba
open Stderr
open Myenv 

open X
open VX_types
open VX_box
open VX_ps
open VX_navigator
open VX_common
open Thread
open Printf
open Shell_exec

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [
           MinHeight 300; MinWidth 500;Background "white"]

let closewindow () = 
        Display.closeDisplay (top#display)


let s111 = text "The goals of the project..."
let s112 = text "This section deals with settings."
let s113 = text (no_nl
"
Bash is an sh-compatible command language interpreter that
executes  commands  read from the standard input or from a
file.  Bash also incorporates  useful  features  from  the
Korn and C shells (ksh and csh).
")

let input1 = Input {
    inp_desc = 0,"Enter design name";
    inp_attr = [Border []];
    inp_buts = ["", ActionSS (fun str -> 
        print_string str; print_newline ();
        if str = "" then std_ARGBAD else std_OK
        )];
    inp_str = "<Empty>";
    inp_vx = None;
}

let input2 = Input (
    let rec ip = 
     {
        inp_desc = 0,"Enter source name";
        inp_attr = [Border []];
        inp_buts = ["Start", ActionUU (fun () -> 
                        print_string ip.inp_str; print_newline ();
                            );
                       "Stop", ActionUU (fun () ->
                        print_string ip.inp_str; print_newline ();
                            );];
        inp_str = "test.c";
        inp_vx = None;
     } in ip)

let but1 = Buttons {
    but_attr = [But [Width 120]];
    but_cols = [|
                    [| 
                        ("Start",(fun () -> ()));
                        ("Stop",(fun () -> ()));
                        ("Reset",(fun () -> ()));
                    |];
                    [| 
                        ("Start the dragon",(fun () -> ()));
                        ("Stop",(fun () -> ()));
                        ("Reset",(fun () -> ()));
                    |]
               |];
    but_vx = None;
}

let sel1 = Select {
    sel_desc = "Select Motion pattern";
    sel_attr = [OpadY 20;Border [];ExpandX true];
    sel_choices = [|
        "Triangle";
        "Sinwave half";
        "Sinwave full";
        "Random";
    |];
    sel_sel = [];
    sel_mutual = true;
    sel_action = (fun i -> 
        Printf.printf "selected %d" i;
        print_newline ();
        );
    sel_vx = None;
}
let fs = File {
    file_desc = "Program file";
    file_attr = [];
    file_action = (fun str -> 
        print_string str; print_newline ();
        );
    file_path = Unix_path "/";
    file_edit = true;    
    file_vx = None;
} 

let lines = [
    "let hbox = new VX_box.h vbox#container";
    "let log1 = new VX_text.log hbox#container 10";
    "let vbox = new VX_box.v top#container";
]

let log = {
    log_attr = [Height 100; Border []];
    log_rows = 100;
    log_add = None;
    log_clear = None;
    log_vx = None;
}

let pdef1 = {
    exec_name = "RPC client";
    exec_src = Unix_src (Exec_path "/unix/amoeba/Amcross/bin/i86_pc/Trpc_client");
    exec_dst = Amoeba_dst (Exec_path "/hosts/geo01/proc");
    exec_args = ["Trpc_client";"testport";"1000000";"100"];
    exec_env = [
        Env_cappath ("TOD","/hosts/geo01/tod"); 
    ];
    exec_ops = [
        Exec_coldstart;        
        Exec_stop;
    ];
}

let pdef2 = {
    exec_name = "RPC_server";
    exec_src = Unix_src (Exec_path "/unix/amoeba/Amcross/bin/i86_pc/Trpc_server");
    exec_dst = Amoeba_dst (Exec_path "/hosts/geo01/proc");
    exec_args = ["Trpc_server";"testport";"1000000";"0"];
    exec_env = [

    ];
    exec_ops = [
        Exec_coldstart;        
        Exec_stop;
    ];
}

let proc1 = {
    pro_desc = "Test RPC Client";
    pro_attr = [OpadY 5];
    pro_def = pdef1;
    pro_log = None;
    pro_obj = None;
}
let proc2 = {
    pro_desc = "Test RPC Server";
    pro_attr = [OpadY 5];
    pro_def = pdef2;
    pro_log = None;
    pro_obj = None;
}

let tab1 = {
    tab_desc = "Data table";
    tab_attr = [IpadX 5;IpadY 2; OpadY 10;];
    tab_rows = [||];
    tab_cols = [|
        [|
            "Voltage",[Background "grey90";Border [Sides [B_left;B_top;]]];
            "10",[Width 80;Mutable true; Border [Sides [B_top;]]];
            "V",[Border [Sides [B_right;B_top;]]];
        |];
        [|
            "Current",[Background "grey90";Border [Sides [B_left;B_top;B_bottom]]];
            "100",[Width 80; Border [Sides [B_top;B_bottom]]];
            "\\mu A",[Border [Sides [B_right;B_top;B_bottom]]];
        |];
    |];
    tab_set = None;
    tab_get = None;
    tab_vx = None;
}

let s1 = [s111 ; Log log ; input1 ; fs]
let s2 = [s111 ; but1 ; s112 ; sel1; Table tab1] 
let s3 = [s111 ; Proc proc2; Proc proc1; input2 ; s112 ; s113]


let desc = [S1 ("Intro",s1);
            S1 ("Main",s2);
            S1 ("Help",s3);
            S1 ("Exit",[Action
                           (fun () ->
                                closewindow ();
                                exit 0;
                            )])
            ]

let nav = new VX_navigator.s1 root 
                              top#container 
                desc
                [
                ExpandX true;
                ExpandY true; 
                ] 


let _ =
    top#setWM_NAME "VX test";
    top#container_add nav#contained;
    top#show;


    __(thread_create (fun () ->
        while true
        do
            __(thread_delay 2 SEC);
            (get_some log.log_add) [(sprintf "%f: time" (Unix.time ()))];
            (get_some tab1.tab_set) 1 1 
                        ((sprintf "%d" (Random.int 1000)),[]);
        done;
        ) ());

(*
    let printed = ref false in

    top#configure [Bindings [
        EnterWindow,(fun _ -> 
            if not !printed then
            begin
                printed := true;
                print_eps "box1.eps" 0.5 vbox#contained;
                print_ps "box1.ps" A4 vbox#contained;
            end;
            );
        ]];
*)
    try
      loop ();
    with Exit -> ();
