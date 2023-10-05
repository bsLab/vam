(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              29/09/2002
**
** Changes:
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
*) 

(*
** Amoeba shell: builtin commands, like dir,get,put,mkhost,...
*)


let mod_shell_ver = 1.0

open Amoeba
open Stderr
open Soap
open Dir
open Format
open Stdcom
open Ar
open Rpc
open Name

type com_options = 
    (* dir *)
    Rights              |
    Info                |
    Cap                 |

    (* generic *)
    Help        
    
    
let com_out = ref Pervasives.stdout 

(*
** Show the content of a directory or one component.
*)


let dir ?(root=nilcap) ~path ~args =
    let help = ref false in
    let opt_info    = ref false in
    let opt_colri   = ref false in
    let opt_capdu   = ref false in
    let opt_help    = ref false in

    let stat        = ref std_OK in

    let rec parse_opt args =
        match args with
        | hd::tl -> parse_opt tl;
                    (
                      match hd with
                      | Help          -> opt_help := true;
                      | Info          -> opt_info := true;
                      | Rights        -> opt_colri := true;
                      | Cap           -> opt_capdu := true;    
                      | _             -> failwith "dir: Unknown option";
                    );
        | [] -> ();
    in

    parse_opt args;

    (* we can't display all at one time ! *)
    if (!opt_capdu = true & !opt_colri = true) then
        opt_colri := false;
    if (!opt_capdu = true & !opt_info = true) then
        opt_info := false;

    if (!opt_help = true) then
    begin
            output_string !com_out 
              "dir ?(root=nilcap) ~path [Help|Info|Rights|Cap]\n";
            flush !com_out;
    end
    else
    begin
      let err,cp = dir_lookup ~root:root ~name:path in

      if (err = std_OK) then
      begin


        let err,dirdesc = dir_open cp in


        if (err = std_OK) then  
        begin
            (*
            ** It's a directory, dump the content.
            *)

            set_formatter_out_channel !com_out;

            print_newline();

            printf "%20s" "Name"; 
            if (!opt_colri = true) then
                printf "%20s" "Column rights";
            if (!opt_info = true) then
                printf "%20s" "Info";
            if (!opt_capdu = true) then
                printf "%30s" "Capability";

            print_newline ();

            if (!opt_colri = true) then
            begin
                printf "%24s" "";
                Array.iter (fun ci -> 
                                    printf "%6s " ci;
                            ) dirdesc.dir_colnames;
                print_newline ();
            end;

            for i = 1 to 79 
            do
                printf "-";
            done;
            print_newline();

            for i = 0 to (dirdesc.dir_nrows - 1)
            do
                let (name,cols) = dir_next dirdesc in

                printf "%20s    " name; 
                if (!opt_colri = true) then
                begin
                    Array.iter (fun ci -> 
                                    printf "%6x " ci;
                                ) cols;
                end;              
                if (!opt_info = true & !opt_colri = true) then
                begin
                    printf "%4s" "";
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        let err,info = std_info ~cap:cap ~bufsize:28 in
                        if (err = std_OK) then
                        begin
                            printf "%28s" info;
                        end
                        else
                            printf "%28s" (err_why err);
                    end;
                end
                else if (!opt_info = true & 
                         !opt_colri = false) then
                begin
                    printf "%4s" "";
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        let err,info = std_info ~cap:cap ~bufsize:40 in
                        if (err = std_OK) then
                        begin
                            printf "%s" info;
                        end
                        else
                            printf "%s" (err_why err);
                    end;
                end
                else if (!opt_capdu = true) then
                begin
                    printf "%4s" "";
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        printf "%s" (ar_cap cap);
                    end;
                end;  
                print_newline ();
            done;      
            print_newline();

            dir_close dirdesc; 
        end
        else if (err = std_COMBAD) then
        begin
            (*
            ** It's not a directory. Get the parent directory instead.
            *)
            let fname = Filename.basename path in
            let err,cap = dir_lookup ~root:root ~name:(Filename.dirname path) in

            if (err = std_OK) then
            begin
                let err,dd = dir_open cap in
                if (err = std_OK) then
                begin
                    (*
                    ** Find the entry.
                    *)
                    let dei = ref (-1) in
    
                    try 
                        for i = 0 to (dd.dir_nrows)
                        do
                            let name,cols = dd.dir_rows.(i) in
                            if (name=fname) then
                            begin
                                dei:=i;
                                raise Exit;
                            end;
                        done;                
                    with    
                        Exit -> ();

                    if (!dei <> -1) then
                    begin
                        let name,cols = dd.dir_rows.(!dei) in
    
                        set_formatter_out_channel !com_out;

                        print_newline();


                        printf "%20s" "Name"; 
                        if (!opt_colri = true) then
                        begin
                            printf "%20s" "Column rights";
                        end;
                        if (!opt_info = true) then
                        begin
                            printf "%20s" "Info";
                        end;
                        if (!opt_capdu = true) then
                        begin
                            printf "%30s" "Capability";
                        end;
                        print_newline ();

                        if (!opt_colri = true) then
                        begin
                            printf "%24s" "";
                            Array.iter (fun ci -> 
                                        printf "%6s " ci;
                                    ) dd.dir_colnames;
                            print_newline ();
                        end;

                        for i = 1 to 79 
                        do
                            printf "-";
                        done;
                        print_newline();


                        printf "%20s    " path; 
                        if (!opt_colri = true) then
                        begin
                            Array.iter (fun ci -> 
                                            printf "%6x " ci;
                                        ) cols;
                        end;              
                        if (!opt_info = true & !opt_colri = true) then
                        begin
                            printf "%4s" "";
                            let err,info = std_info ~cap:cp ~bufsize:28 in
                            if (err = std_OK) then
                            begin
                                printf "%28s" info;
                            end
                            else
                                printf "%28s" (err_why err);
                        end 
                        else if (!opt_info = true & !opt_colri = false) then
                        begin
                            printf "%4s" "";
                            let err,info = std_info ~cap:cp ~bufsize:40 in
                            if (err = std_OK) then
                            begin
                                printf "%s" info;
                            end
                            else
                                printf "%s" (err_why err);
                        end
                        else if (!opt_capdu = true) then
                        begin
                            printf "%s" (ar_cap cp);
                        end;
                        print_newline ();
                    end;

                    print_newline();

                    dir_close dd;                                     
                end
                else
                begin
                    set_formatter_out_channel !com_out;
                    print_string ( 
                                   "dir: Can't lookup parent ("^
                                   (Filename.dirname path)^") :"^
                                   (err_why err)
                                 );
                    print_newline ();
                end;

            end
          end
        end
        else 
        begin
            set_formatter_out_channel !com_out;
            print_string ("dir: Directory lookup failed: "^(err_why err));
            print_newline ();
        end;
    end


(*
** Print the status from a server 'name'
*)

let std_status ?(root=nilcap) ~name ~args =
    let help = ref false in
    let opt_help    = ref false in

    let stat        = ref std_OK in

    let rec parse_opt args =
        match args with
        | hd::tl -> parse_opt tl;
                    (
                      match hd with
                      | Help          -> opt_help := true;
                      | _             -> failwith "std_status: Unknown option";
                    );
        | [] -> ();
    in

    parse_opt args;


    if (!opt_help = true) then
    begin
            output_string !com_out 
              "std_status ?(root=nilcap) ~name [Help]\n";
            flush !com_out;
    end
    else
    begin
        let err,cap = dir_lookup ~root:root ~name:name in
        if (err = std_OK) then
        begin
            let err,str = std_status ~cap:cap ~bufsize:max_TRANS in
            if (err=std_OK) then
            begin
                output_string !com_out str;
                print_newline ();
                flush !com_out;
            end
        end
    end

(*
** Print the server id name (info) from a server 'name'
*)

let std_info ?(root=nilcap) ~name ~args =
    let help = ref false in
    let opt_help    = ref false in

    let stat        = ref std_OK in

    let rec parse_opt args =
        match args with
        | hd::tl -> parse_opt tl;
                    (
                      match hd with
                      | Help          -> opt_help := true;
                      | _             -> failwith "std_info: Unknown option";
                    );
        | [] -> ();
    in

    parse_opt args;


    if (!opt_help = true) then
    begin
            output_string !com_out 
              "std_info ?(root=nilcap) ~name [Help]\n";
            flush !com_out;
    end
    else
    begin
        let err,cap = dir_lookup ~root:root ~name:name in
        if (err = std_OK) then
        begin
            let err,str = std_info ~cap:cap ~bufsize:max_TRANS in
            if (err=std_OK) then
            begin
                output_string !com_out str;
                print_newline ();
                flush !com_out;
            end
        end
    end

(*
** Send the server specified with cap 'name' the STD_EXIT command.
*)

let std_exit ?(root=nilcap) ~name ~args =
    let help = ref false in
    let opt_help    = ref false in

    let stat        = ref std_OK in

    let rec parse_opt args =
        match args with
        | hd::tl -> parse_opt tl;
                    (
                      match hd with
                      | Help          -> opt_help := true;
                      | _             -> failwith "std_exit: Unknown option";
                    );
        | [] -> ();
    in

    parse_opt args;


    if (!opt_help = true) then
    begin
            output_string !com_out 
              "std_exit ?(root=nilcap) ~name [Help]\n";
            flush !com_out;
    end
    else
    begin
        let err,cap = dir_lookup ~root:root ~name:name in
        if (err = std_OK) then
        begin
            let err = std_exit ~cap:cap in
                ()
        end
    end


(*
** Make a new directory
*)

let mkdir ?(root=nilcap) ~name ~args =

    let help = ref false in
    let opt_help    = ref false in

    let stat        = ref std_OK in

    let rec parse_opt args =
        match args with
        | hd::tl -> parse_opt tl;
                    (
                      match hd with
                      | Help          -> opt_help := true;
                      | _             -> failwith "mkdir: Unknown option";
                    );
        | [] -> ();
    in

    parse_opt args;

    if (!opt_help = true) then
    begin
            output_string !com_out 
              "mkdir ?(root=nilcap) ~path [Help]\n";
            flush !com_out;
    end
    else
    begin
        let err,cap = dir_create ~server:root in
        if (err=std_OK) then
        begin
            let err = dir_append ~root:root ~name:name ~obj:cap in
            if (err<>std_OK) then
            begin
                output_string !com_out
                (Printf.sprintf "mkdir: failed to create directory %s: %s\n"
                                name (err_why err));
                flush !com_out;
            end
        end
    end
