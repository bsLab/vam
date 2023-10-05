
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
open Cap_env
open Thread
open Printf
open Sema


open Xnc_exec
open Xnc_widget
open Xnc_file
open Xnc_prog

let catch func =
    try func () with
        | _ -> ()


let main () =
    let add_log str =
                xw_main.xw_log.xw_log_str <-
                    xw_main.xw_log.xw_log_str ^ str;
                xw_main.xw_log.xw_log_text#set_lines
                    xw_main.xw_log.xw_log_str;
                xw_main.xw_log.xw_log_text#update;
                xw_main.xw_log.xw_log_ady#set_pos 1 1;
        in
    let clear_log () =
                xw_main.xw_log.xw_log_str <- "";
                xw_main.xw_log.xw_log_text#set_lines
                    xw_main.xw_log.xw_log_str; 
                xw_main.xw_log.xw_log_text#update;
                xw_main.xw_log.xw_log_ady#set_pos 1 1;
        in

    let closewindow () = 
        Display.closeDisplay (xw_main.xw_top#display);
        in
    (*
    ** Configure top action
    *)
    xw_main.xw_quit#set_action (fun () ->
            closewindow ();
            exit 0;
        );

    (* INIT cnc *)
    xw_main.xw_cnc_server.xw_tib_button#set_action (fun () ->
            xw_main.xw_cnc_server.xw_tib_str <- 
                xw_main.xw_cnc_server.xw_tib_input#string;
            clear_log ();
            add_log "Initializing CNC server...\n";
            let stat = init_cnc xw_main.xw_cnc_server.xw_tib_str in
            add_log (sprintf "Result: %s\n" (err_why stat));
        );
    (* INIT enc *)
    xw_main.xw_enc_server.xw_tib_button#set_action (fun () ->
            xw_main.xw_enc_server.xw_tib_str <- 
                xw_main.xw_enc_server.xw_tib_input#string;
            clear_log ();
            add_log "Initializing ENC serial port ...\n";
            let stat = init_enc xw_main.xw_enc_server.xw_tib_str in
            add_log (sprintf "Result: %s\n" (err_why stat));
        );

    (*
    ** Configure axis button actions (motor locking...)
    *)

    (* X *)
    xw_main.xw_axis.xw_l4b_but1#set_external_switch true;
    xw_main.xw_axis.xw_l4b_but1#set_action (fun () ->
            if (xw_main.xw_axis.xw_l4b_state1) then
            begin
                ignore (cnc_eval "UL X;");
                xw_main.xw_axis.xw_l4b_but1#desactivate;
                xw_main.xw_axis.xw_l4b_state1 <- false;
            end
            else
            begin
                ignore (cnc_eval "LO X;");
                xw_main.xw_axis.xw_l4b_but1#activate;
                xw_main.xw_axis.xw_l4b_state1 <- true;
            end;
        );

    (* Y *)
    xw_main.xw_axis.xw_l4b_but2#set_external_switch true;
    xw_main.xw_axis.xw_l4b_but2#set_action (fun () ->
            if (xw_main.xw_axis.xw_l4b_state2) then
            begin
                ignore (cnc_eval "UL Y;");
                xw_main.xw_axis.xw_l4b_but2#desactivate;
                xw_main.xw_axis.xw_l4b_state2 <- false;
            end
            else
            begin
                ignore (cnc_eval "LO Y;");
                xw_main.xw_axis.xw_l4b_but2#activate;
                xw_main.xw_axis.xw_l4b_state2 <- true;
            end;
        );

    (* Z *)
    xw_main.xw_axis.xw_l4b_but3#set_external_switch true;
    xw_main.xw_axis.xw_l4b_but3#set_action (fun () ->
            if (xw_main.xw_axis.xw_l4b_state3) then
            begin
                ignore (cnc_eval "UL Z;");
                xw_main.xw_axis.xw_l4b_but3#desactivate;
                xw_main.xw_axis.xw_l4b_state3 <- false;
            end
            else
            begin
                ignore (cnc_eval "LO Z;");
                xw_main.xw_axis.xw_l4b_but3#activate;
                xw_main.xw_axis.xw_l4b_state3 <- true;
            end;
        );

    (* ALL *)
    xw_main.xw_axis.xw_l4b_but4#set_action (fun () ->
            if (xw_main.xw_axis.xw_l4b_state4) then
            begin
                ignore (cnc_eval "UA;");
                xw_main.xw_axis.xw_l4b_but1#desactivate;
                xw_main.xw_axis.xw_l4b_but2#desactivate;
                xw_main.xw_axis.xw_l4b_but3#desactivate;
                xw_main.xw_axis.xw_l4b_state1 <- false;
                xw_main.xw_axis.xw_l4b_state2 <- false;
                xw_main.xw_axis.xw_l4b_state3 <- false;
                xw_main.xw_axis.xw_l4b_state4 <- false;
            end
            else
            begin
                ignore (cnc_eval "LA;");
                xw_main.xw_axis.xw_l4b_but1#activate;
                xw_main.xw_axis.xw_l4b_but2#activate;
                xw_main.xw_axis.xw_l4b_but3#activate;
                xw_main.xw_axis.xw_l4b_state1 <- true;
                xw_main.xw_axis.xw_l4b_state2 <- true;
                xw_main.xw_axis.xw_l4b_state3 <- true;
                xw_main.xw_axis.xw_l4b_state4 <- true;
            end;
        );

    (*
    ** Init file and program (empty) menu
    *)
    file_init xw_main;
    prog_init xw_main;

    (*
    ** Main button bar actions
    *)

    (* LOAD *)
    xw_main.xw_buttons.xw_b9_but1#set_action (fun () ->
            clear_log ();
            if !cnc_program <> [] then
            begin
                let str = ref "" in
                List.iter (fun b ->
                        str := !str ^ b.b_cont ^"\n";
                    ) !cnc_program;

                catch (fun () -> 
                        let stat,res = cnc_load !str in
                        if stat <> std_OK then
                            add_log (sprintf "Result: %s\n" (err_why stat));
                        (*
                        ** The cnc program will be loaded
                        ** in a newely created thread environment!
                        *)
        
                      );
            end
            else
            begin
                add_log "No program loaded.\n";
            end;
        );

    (* RUN *)
    xw_main.xw_buttons.xw_b9_but2#set_action (fun () ->
            clear_log ();
            if !cnc_program <> [] then
            begin
                let bl = List.filter (fun b ->
                        let d = match b.b_desc with
                                | Some d -> d;
                                | None -> failwith "desc";
                            in
                        if d.pb_selected then true else false;
                     ) !cnc_program in
                catch (fun () -> 
                        add_log "Executing program:\n";
                        let stat,res = cnc_exec bl in
                        if stat <> std_OK then
                            add_log (sprintf "Result: %s\n" (err_why stat));
                        (*
                        ** The cnc program will be executed
                        ** in a newely created thread environment!
                        *)
        
                      );
            end
            else
            begin
                add_log "No program loaded.\n";
            end;
        );

    (* STOP *)
    xw_main.xw_buttons.xw_b9_but3#set_action (fun () ->
            let stat = cnc_break () in
            if stat <> std_OK then
                add_log "Can't stop CNC program.\n"
            else
                add_log "Stopping CNC execution...\n";
        );

    (* SHOW *)
    xw_main.xw_buttons.xw_b9_but7#set_action (fun () ->
            clear_log ();
            let str = ref "" in
            (*
            ** Show current program block section
            *)
            if !cnc_program <> [] then
            begin
                List.iter (fun b ->
                    let d = match b.b_desc with
                            | Some d -> d;
                            | None -> failwith "desc";
                        in
                    if d.pb_selected then
                        str := !str ^ ("<"^b.b_name^">\n"^
                                 b.b_cont^
                                 "<\\"^b.b_name^">\n\n");
                  ) !cnc_program;
                add_log !str;
            end
            else
            begin
                add_log "No program loaded.\n";
            end;
        );

    (* SEL+ *)    
    xw_main.xw_buttons.xw_b9_but5#set_action (fun () ->
            (*
            ** Select all program blocks
            *)
            if !cnc_program <> [] then
            begin
                List.iter (fun b ->
                    let d = match b.b_desc with
                            | Some d -> d;
                            | None -> failwith "desc";
                        in
                        d.pb_desc#configure [Foreground "black"];
                        d.pb_selected <- true;
                  ) !cnc_program;
            end
            else
            begin
                clear_log ();
                add_log "No program loaded.\n";
            end;
        );
    
    (* SEL- *)    
    xw_main.xw_buttons.xw_b9_but6#set_action (fun () ->
            (*
            ** Select all program blocks
            *)
            if !cnc_program <> [] then
            begin
                List.iter (fun b ->
                    let d = match b.b_desc with
                            | Some d -> d;
                            | None -> failwith "desc";
                        in
                        if (b.b_type <> Header) then
                        begin
                            d.pb_desc#configure [Foreground "Grey50"];
                            d.pb_selected <- false;
                        end;
                  ) !cnc_program;
            end
            else
            begin
                clear_log ();
                add_log "No program loaded.\n";
            end;
        );

    (*
    ** Solid button actions
    *)
    (* SN X *)
    xw_main.xw_solid.xw_t3lib_button1#set_action (fun () ->
            let str = xw_main.xw_solid.xw_t3lib_input1#string in
            xw_main.xw_solid.xw_t3lib_str1 <- str;
            let stat,str = cnc_eval (sprintf "SN X %s;" str) in
            ()
        );    
    (* SN Y *)
    xw_main.xw_solid.xw_t3lib_button2#set_action (fun () ->
            let str = xw_main.xw_solid.xw_t3lib_input2#string in
            xw_main.xw_solid.xw_t3lib_str2 <- str;
            let stat,str = cnc_eval (sprintf "SN Y %s;" str) in
            ()
        );    
    (* SN Z *)
    xw_main.xw_solid.xw_t3lib_button3#set_action (fun () ->
            let str = xw_main.xw_solid.xw_t3lib_input3#string in
            xw_main.xw_solid.xw_t3lib_str3 <- str;
            let stat,str = cnc_eval (sprintf "SN Z %s;" str) in
            ()
        );    

    (*
    ** Tool button actions
    *)
    (* TN ALL *)
    xw_main.xw_tool.xw_t4lib_button#set_action (fun () ->
            let str1 = xw_main.xw_tool.xw_t4lib_input1#string in
            let str2 = xw_main.xw_tool.xw_t4lib_input2#string in
            let str3 = xw_main.xw_tool.xw_t4lib_input3#string in
            let str4 = xw_main.xw_tool.xw_t4lib_input4#string in
            xw_main.xw_tool.xw_t4lib_str1 <- str1;
            xw_main.xw_tool.xw_t4lib_str2 <- str2;
            xw_main.xw_tool.xw_t4lib_str3 <- str3;
            xw_main.xw_tool.xw_t4lib_str4 <- str4;
            ignore(cnc_eval (sprintf "TN X %s; TN Y %s; TN Z %s; TD %s;" 
                    str1 str2 str3 str4));
        );    

    (*
    ** Position view actions (cnc/machine)
    *)
    (* X=0 *)
    xw_main.xw_machine_coord.xw_t3lvb_but1#set_action (fun () ->
            ignore(enc_eval "x");
        );
    (* Y=0 *)
    xw_main.xw_machine_coord.xw_t3lvb_but2#set_action (fun () ->
            ignore(enc_eval "y");
        );
    (* Z=0 *)
    xw_main.xw_machine_coord.xw_t3lvb_but3#set_action (fun () ->
            ignore(enc_eval "z");
        );

    (* X=0 *)
    xw_main.xw_cnc_coord.xw_t3lvb_but1#set_action (fun () ->
            ignore(cnc_eval "MN X;");
        );
    (* Y=0 *)
    xw_main.xw_cnc_coord.xw_t3lvb_but2#set_action (fun () ->
            ignore(cnc_eval "MN Y;");
        );
    (* Z=0 *)
    xw_main.xw_cnc_coord.xw_t3lvb_but3#set_action (fun () ->
            ignore(cnc_eval "MN Z;");
        );

    (*
    ** Single command line (P1..P8) actions
    *)
    List.iter (fun c ->
            c.xw_tib_button#set_action (fun () ->
                    clear_log ();
                    add_log "Evaluate single command:\n";
                    c.xw_tib_str <- c.xw_tib_input#string;
                    let stat,res = cnc_eval c.xw_tib_str in
                    if stat <> std_OK then
                        add_log (sprintf "Result: %s\n" (err_why stat))
                    else
                        add_log (sprintf "Result:\n %s\n" res);
                );
        )   xw_main.xw_comms;
    

    (*
    ** Configure scroll bar and input field actions
    *)

    (* LOG *)
    let ady = xw_main.xw_log.xw_log_ady in
    xw_main.xw_log.xw_log_hbar#configure [Bindings [
        Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
        Key(XK.xk_Next,0),(fun _ -> ady#page_down);
        Key(XK.xk_Down,0),(fun _ -> ady#down);
        Key(XK.xk_Up,0),(fun _ -> ady#up);
        ButtonPress, (fun _ -> 
                xw_main.xw_log.xw_log_hbar#focus);
    ]];

    List.iter (fun c ->
            c.xw_tib_hbar#configure [ 
                        Bindings 
                        [
                            ButtonPress, (fun _ -> 
                                c.xw_tib_input#focus);
                        ]
                ];
        ) xw_main.xw_comms;

    (* CNC input *)
    xw_main.xw_cnc_server.xw_tib_hbar#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                xw_main.xw_cnc_server.xw_tib_input#focus);
                        ]
                ];

    (* ENC input *)
    xw_main.xw_enc_server.xw_tib_hbar#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                xw_main.xw_enc_server.xw_tib_input#focus);
                        ]
                ];

    (* TOOL inputs *)
    let t = xw_main.xw_tool in
    t.xw_t4lib_input1#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                t.xw_t4lib_input1#focus);
                        ]
                ];
    t.xw_t4lib_input2#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                t.xw_t4lib_input2#focus);
                        ]
                ];
    t.xw_t4lib_input3#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                t.xw_t4lib_input3#focus);
                        ]
                ];
    t.xw_t4lib_input4#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                t.xw_t4lib_input4#focus);
                        ]
                ];

    (* SOLID inputs *)
    let s = xw_main.xw_solid in
    s.xw_t3lib_input1#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                s.xw_t3lib_input1#focus);
                        ]
                ];
    s.xw_t3lib_input2#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                s.xw_t3lib_input2#focus);
                        ]
                ];
    s.xw_t3lib_input3#configure [
                        Bindings 
                        [
                            ButtonPress, (fun _ ->
                                s.xw_t3lib_input3#focus);
                        ]
                ];

    (*
    ** Push it to the maximum...
    *)
    xw_main.xw_top#show;

    xw_main.xw_top#setWM_NAME "XNC CNC MACHINE CONTROLLER";
    try 
      loop ()
    with 
      Exit -> ()


let _ = 
  main ()  
