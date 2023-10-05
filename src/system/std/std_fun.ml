

let _version = 1.01

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

open Io

let dir_pathlookup path =
    if (path <> "" &&
        path.[0] = '/') then
        dir_lookup ~root:!root_cap ~name:path
    else
        dir_lookup ~root:!work_cap ~name:path


let f_exit args =
    try
    begin
        List.iter (fun n ->
            let path = path_resolve n in
            match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = std_exit cap in
                if (stat <> std_OK) then
                    raise (Error stat);
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat = std_exit cap in
                if (stat <> std_OK) then
                    raise (Error stat);
            end;
            ) args;
        std_OK
    end
    with
        | Error err -> err

let f_info args =
    try
    begin
        List.iter (fun n ->
            let path = path_resolve n in
            match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat,str = std_info cap 30000 in
                if (stat <> std_OK) then
                    raise (Error stat);
                out str; nl ();
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat,str = std_info cap 30000 in
                if (stat <> std_OK) then
                    raise (Error stat);
                out str; nl ();
            end;
            ) args;
        std_OK
    end
    with
        | Error err -> err

let f_status args =
    try
    begin
        List.iter (fun n ->
            let path = path_resolve n in
            match path with
            | Amoeba_path p ->
            begin
                let stat,cap = name_lookup p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat,str = std_status cap 30000 in
                if (stat <> std_OK) then
                    raise (Error stat);
                out str; nl ();
            end;
            | Unix_path p ->
            begin
                let stat,cap = read_cap p in
                if (stat <> std_OK) then
                    raise (Error stat);
                let stat,str = std_status cap 30000 in
                if (stat <> std_OK) then
                    raise (Error stat);
                out str; nl ();
            end;
            ) args;
        std_OK
    end
    with
        | Error err -> err

