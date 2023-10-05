
open Amoeba
open Unix
open Io
open Std
open Stderr

let _ =
    try
    begin
        let comline = Array.to_list Sys.argv in
        let com,args =
            match comline with
            | hd::tl ->
            begin
                match tl with
                | hd::tl -> hd,tl;
                | [] -> usage std_ARGBAD;
            end;
            | [] ->
            begin
                usage std_ARGBAD;
            end;
            in

        let com = to_command com args in
        let stat = exec_com com in
        if (stat <> std_OK) then
            raise (Error stat); 
    end
    with
        | Error err -> out ("failed: "^(err_why err)); nl()
