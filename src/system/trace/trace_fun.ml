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
**    $CREATED:     ??
**    $VERSION:     1.01
**
**    $INFO:
**
**
**    $ENDOFINFO
**
*)


open Amoeba
open Buf
open Unix
open Stderr
open Stdcom
open Stdcom2
open Ar
open Cap_env
open Dir
open Name
open Ktrace

open Io

let dir_pathlookup path =
    if (path <> "" &&
        path.[0] = '/') then
        dir_lookup ~root:!root_cap ~name:path
    else
        dir_lookup ~root:!work_cap ~name:path


let f_init server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
        let size = int_of_string args.(0) in
        let mask = int_of_string args.(1) in
    
        let path = path_resolve path' in
        begin
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_init cap trtype size mask in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_init cap trtype size mask in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
        end;
    end
    with
        | Error err -> err


let f_delete server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
    
        let path = path_resolve path' in
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_delete cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_delete cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
    end
    with
        | Error err -> err

let f_start server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
    
        let path = path_resolve path' in
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_start cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_start cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
    end
    with
        | Error err -> err

let f_stop server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
    
        let path = path_resolve path' in
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_stop cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_stop cap trtype in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
    end
    with
        | Error err -> err

let f_reset server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
        let mask = int_of_string args.(0) in    
        let path = path_resolve path' in
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_reset cap trtype mask in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_reset cap trtype mask in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
    end
    with
        | Error err -> err

let f_print server ttype args =
    try
    begin
        let path' = server in
        let trtype = match ttype with
                     | "net" -> Trace_NETWORK;
                     | "threads" -> Trace_THREAD;
                     | _ -> failwith "unknown type";
            in
    
        let name = args.(0) in
        let path = path_resolve path' in
        match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_print_file cap trtype name in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = trace_print_file cap trtype name in
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK;
            end;
    end
    with
        | Error err -> err
