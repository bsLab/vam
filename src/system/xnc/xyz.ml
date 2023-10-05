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
**    $CREATED:     2003.0.0
**    $VERSION:     1.00
**
**    $INFO:
**
**  Simple graphical client application to get xyz coordinates
**  from an encoder via serial port.
**
**    $ENDINFO
*)

open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Bstream
open Name
open Printf
open Thread
open Xtypes
open WX_types
open Sema


let sema = sema_create 1

let serial = ref "/hosts/juki01/ser:00"
let cap = ref nilcap 


let display = new WX_display.t ""
let root = new WX_root.t display 0

(* The top widget *)
let top = new WX_wmtop.t root [MinWidth 100; MinHeight 100; MaxHeight 600]

(* Vertical and horizontal container widgets *)
let vbar1 = new WX_bar.v top#container [Background "black"]
let hbar1 = new WX_bar.h vbar1#container [IpadX 10; IpadY 10;
                        Background "black"] 
let hbar2 = new WX_bar.h vbar1#container 
                    [IpadX 10; IpadY 10;
                    Background "black"] 
let hbar3 = new WX_bar.h vbar1#container 
                    [IpadX 10; IpadY 10;
                    Background "black"]
            

(*
** Button table
*)
let cop_rows = 1
let cop_cols = 5

let buts = [|
    [|"QUIT";
      "INIT";
      "X0";
      "Y0";
      "Z0"|]
    
|]

let buttons =  new WX_table.button vbar1#container 
                cop_cols cop_rows 50 20 buts 
                [IpadX 5; IpadY 5] 

(*
** Path field
*)
let input = new WX_ledit.t vbar1#container !serial 
                        [ExpandX true;]
let input_label = new WX_label.t vbar1#container "Serial Port" 
                        []

(*
** Coordinate labels
*)

let v_cols = 3 
let v_rows = 2
let vals = [|
        [|"  X [mm]";"  Y [mm]";"  Z [mm]"|];
        [|"    0.00";"    0.00";"    0.00"|];
    |]
let vals = new WX_table.text vbar1#container
                v_cols v_rows 100 20 vals
                [IpadX 5; IpadY 5]

let catch func =
    try func () with
        | _ -> sema_up sema


let fail str =
    output_string Pervasives.stdout str;
    print_newline ();
    raise Exit


let write str =
    let stat = stream_write !cap str in
    if stat <> std_OK
        then fail "Can't write to serial port"

let read () =
    let stat,str = stream_read !cap 100 in
    if stat <> std_OK
        then fail "Can't read from serial port";
    str

let measure () =
    let milli v =
        abs (v/1000) in
    let micro v =
        let v1 = (v/1000)*1000 in
        abs ((v-v1)/10) in
    let sign v =
        if v >= 0 then ' ' else '-' in

    while (true)
    do
        sema_down sema;
        write "X";
        let str = read () in
        let x = int_of_string str in
        write "Y";
        let str = read () in
        let y = int_of_string str in
        write "Z";
        let str = read () in
        let z = int_of_string str in
        vals#set_text 0 1 (sprintf "%c%4d.%.2d" (sign x) (milli x) (micro x));
        vals#set_text 1 1 (sprintf "%c%4d.%.2d" (sign y) (milli y) (micro y));
        vals#set_text 2 1 (sprintf "%c%4d.%.2d" (sign z) (milli z) (micro z));
        sema_up sema;
        Unix.sleep 1;
    done

let inited = ref false

let _ =




    buttons#set_action 0 0 (fun () ->
                Display.closeDisplay (top#display);
                raise Exit;
            );
    buttons#set_action 2 0 (fun () ->
        catch (fun () ->
                sema_down sema;
                write "x";
                ignore(read ());
                sema_up sema;
            ));
    buttons#set_action 1 0 (fun () ->
        catch (fun () ->
            if not !inited then
            begin
                serial := input#string;
                let stat,cap' = name_lookup !serial in
                if stat <> std_OK 
                    then fail (sprintf "name_lookup of serial port %s failed"
                                        !serial);

                cap := cap';

                (*
                ** Set serial port parameters
                *)
                let stat = std_set_params !cap ["enable","0";
                                   "baud","9600";
                                   "thr","2";
                                   "enable","1";] in
                if stat <> std_OK
                    then fail "Can't set serial port params";
                ignore(thread_create measure ());
                inited := true;
            end;
            ));
    buttons#set_action 3 0 (fun () ->
         catch (fun () ->
                sema_down sema;
                write "y";
                ignore(read ());
                sema_up sema;
            ));
    buttons#set_action 4 0 (fun () ->
        catch (fun () ->
                sema_down sema;
                write "z";
                ignore(read ());
                sema_up sema;
            ));

    for i = 0 to 2
    do
        for j = 0 to 1
        do
            vals#set_font i j Fonts.Courier.Bold.s24;
        done;
    done;

    hbar1#container_add buttons#contained;
    hbar2#container_add vals#contained;
    hbar3#container_add_s [
            input_label#contained;
            input#contained;
        ];

    vbar1#container_add_s  [
                            hbar1#contained;
                            hbar3#contained;
                            hbar2#contained;
                         ];


    input#configure [ 
                        Bindings 
                        [
                            ButtonPress, (fun _ -> 
                                input#focus);
                        ]
        ];
    
    top#configure [ 
                        Bindings 
                        [
                          FocusIn,(fun _ ->
                                    if (X.getInputFocus top#display).gif_win 
                                         == top#window then
                                    begin
                                        top#focus
                                    end;
                            );

                          ButtonPress, (fun _ -> 
                                        top#focus);
                        ]
        ];

    top#container_add vbar1#contained;

    top#show;

    top#setWM_NAME "CNC coordinate viewer";
    try 
      loop ()
    with 
      Exit -> ()

    

   