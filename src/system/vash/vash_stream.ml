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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     
**    $VERSION:     1.02
**
**    $INFO:
**
**  VASH simple stream read/write interface
**
**    $ENDOFINFO
**
*)




open Amoeba
open Stderr
open Stdcom
open Dir
open Ar
open Unix
open Name
open Stdcom
open Stdcom2
open Bstream

open Vash_io
open Vash_env
open Cap_env


let stream_read args =
    let usage err =
            out "usage: stream_read [-h -f <file>] <serverpath> <size>"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
    let file = ref "" in
    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-f" -> (match tl with
                       | hd'::tl' -> file := hd'; iter tl';
                       | [] -> usage std_ARGBAD;);
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                let path =
                    if hd.[0] <> '/' then
                    begin
                       match !work_dir with
                        | Amoeba_path wd ->
                            wd^"/"^hd;
                        | Unix_path wd ->
                            "/unix/"^wd^"/"^hd;
                    end
                    else
                       hd in
                let path = path_resolve path in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                        let stat,cap = dir_pathlookup path in
                        if (stat = std_OK) then
                        begin
                            match tl with
                            | [] -> usage std_ARGBAD;
                            | str::tl ->
                                let size = int_of_string str in
                                let stat,str = stream_read cap size in
                                if (stat != std_OK) then
                                begin
                                    out ("'"^path^"': stream_read failed: "^
                                         (err_why stat));
                                    nl ();
                                end
                                else
                                begin
                                    out str; nl ();
                                end; 
                        end
                        else
                        begin
                            out ("'"^path^"': dir_lookup failed: "^
                                 (err_why stat));
                            nl ();
                        end;
                    end;
                    | Unix_path path ->
                    begin   
                    end;
                end;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK


(*
** Stream read
*)

let stream_write args =
    let usage err =
            out "usage: stream_read [-h -f <file>] <serverpath> <str>"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in

    /*
    ** Resolve escape sequences
    */
    let resolve_esc str =
        let str' = ref "" in
        let len = String.length str in
        let i = ref 0 in
        while !i < len
        do
            if str.[!i] <> '\\' then
                str' := !str' ^ (let s = " " in s.[0] <- str.[!i]; s)
            else if (!i + 1) < len then
            begin
                str' := !str' ^ (
                    let e = str.[!i+1] in
                    let e' =
                        match e with
                        | 'r' -> '\r';
                        | 'n' -> '\n';
                        | 't' -> '\t';
                        | '\\' -> '\\';
                        | _ -> raise (Error std_ARGBAD);
                        in
                    incr i;
                    let s = " " in s.[0] <- e'; s
                    )
            end;
            incr i;
        done;
        !str'
        in  

    /*
    ** Resolve escape sequences vice versa
    */
    let resolve_esc_versa str =
        let str' = ref "" in
        let len = String.length str in
        for i = 0 to  len-1
        do
            match str.[i] with
            | '\n'  -> str' := !str' ^ "\n";
            | ' ' .. '~'  -> str' := !str' ^ 
                                (let s = " " in s.[0] <- str.[i]; s); 
            | c -> str' := !str' ^ (Printf.sprintf "\\%03d" 
                                    (int_of_char c));
        done;
        !str'
        in  
  
    let file = ref "" in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-f" -> (match tl with
                       | hd'::tl' -> file := hd'; iter tl';
                       | [] -> usage std_ARGBAD;);
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                let path =
                    if hd.[0] <> '/' then
                    begin
                       match !work_dir with
                        | Amoeba_path wd ->
                            wd^"/"^hd;
                        | Unix_path wd ->
                            "/unix/"^wd^"/"^hd;
                    end
                    else
                       hd in
                let path = path_resolve path in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                        let stat,cap = dir_pathlookup path in
                        if (stat = std_OK) then
                        begin
                            let str = 
                              match tl with
                              | [] -> if !file <> "" then "" else 
                                        usage std_ARGBAD;
                              | str::_ -> str in
                                                  
                            let str = 
                                let head = resolve_esc str in
                                if !file = "" then head else
                                begin
                                  let fpath =
                                    if !file.[0] <> '/' then
                                    begin
                                      match !work_dir with
                                      | Amoeba_path wd ->
                                        Amoeba_path (wd^"/"^ !file);
                                      | Unix_path wd ->
                                        Unix_path ("/unix/"^wd^"/"^ !file);
                                    end
                                    else
                                      path_resolve !file in
                                  match fpath with
                                  | Unix_path file ->
                                      (*
                                      ** Read unix file...
                                      *)
                                      let st = Unix.stat file in
                                      let size = ref st.st_size in
                                      let off = ref 0 in
                                      let str = String.create !size in
                                      let fd = Unix.openfile file 
                                                [Unix.O_RDONLY] 0 in
                                      while (!size > 0)
                                      do
                                          let this_size = min !size 4096 in
                                          let n = Unix.read fd
                                                            str
                                                            !off
                                                            this_size in
                                          size := !size - n;
                                          off := !off + n;
                                      done;
                                      (head^str)
                                  | Amoeba_path file ->
                                      "" 
                                end in
                            
                            let stat = stream_write cap str in
                            if (stat != std_OK) then
                            begin
                                    out ("'"^path^"': stream_write failed: "^
                                         (err_why stat));
                                    nl ();
                            end;
                        end
                        else
                        begin
                            out ("'"^path^"': dir_lookup failed: "^
                                 (err_why stat));
                            nl ();
                        end;
                    end;
                    | Unix_path path ->
                    begin   
                    end;
                end;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK


