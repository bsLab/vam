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
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     0.62
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*)




open Amoeba
open Buf
open Stderr
open Vash_io
open Cap_env
open Ar
open Name
open Dir


(*
** Root and working directories
*)

let root_cap = ref (let _,cap = get_env_cap "ROOTCAP" in cap)
let file_cap = ref (let _,cap = get_env_cap "AFSCAP" in cap)
let work_dir = ref (Amoeba_path "/")
let work_cap = ref (!root_cap)
let tty_cap  = ref (let _,cap = get_env_cap "TTY" in cap)
let stdin_cap  = ref (let _,cap = get_env_cap "STDIN" in cap)
let stdout_cap  = ref (let _,cap = get_env_cap "STDOUT" in cap)
let stderr_cap  = ref (let _,cap = get_env_cap "STDERR" in cap)
let random_cap = ref (let _,cap = get_env_cap "RANDOM" in cap)
let tod_cap = ref (let _,cap = get_env_cap "TOD" in cap)


let update_cap name cap =
    match name with
    | "ROOT"    -> root_cap := cap;
    | "AFS"     -> file_cap := cap;
    | "TTY"     -> tty_cap  := cap;
    | "STDIN"   -> stdin_cap := cap;
    | "STDOUT"  -> stdout_cap := cap;
    | "STDERR"  -> stderr_cap := cap;
    | "WORK"    -> work_cap := cap;
    | "TOD"     -> tod_cap := cap;
    | "RANDOM"  -> random_cap := cap;
    | _ -> ()

let dir_pathlookup path =
    if (path <> "" &&
        path.[0] = '/') then
        dir_lookup ~root:!root_cap ~name:path
    else 
        dir_lookup ~root:!work_cap ~name:("/"^path)

let print_env args =
    let usage err =
            out "usage: [-h -a] print_env <envname1> [<envname2> ...]"; nl ();
            out "       -a : print all environment caps"; nl();
            raise (Error err);
            in
    let envnames = ref [] in
    let all = ref false in
    let rec iter args =
            match args with
            | hd::tl -> 
            begin
                match hd with
                | "-h" -> usage std_OK;
                | "-a" -> all := true;
                | _ -> 
                begin
                    if hd.[0] = '-' then
                    begin
                        out ("unknown option: "^hd); nl ();
                        usage std_ARGBAD; 
                    end;
                    let stat,cap = get_env_cap hd in
                    if (stat = std_OK) then
                    begin
                        out ("Environment capability '"^(hd)^"': "^
                                      (ar_cap cap)); nl ();   
                    end
                    else
                    begin
                        out ("Environment capability '"^(hd)^"': "^
                                      (err_why stat)); nl ();   
                    end;
                    iter tl;
                end;
            end;
            | [] -> ()
            in
    iter args;
    if (!all = true) then
    begin
        let evs = get_env_caps () in
        out "Capability environment: "; nl();
        List.iter (fun nc ->
                    let name,cap = nc in
                    out ("  '"^(name)^"': "^
                                      (ar_cap cap)); nl ();
            ) evs;
    end;
    std_OK

let get_env args =
  try
  begin
    let usage err =
            out "<environment variable> := <path cap> | <cap string>"; nl ();
            out "usage: [-h -c] get_env <envname> <path or cap string>"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
    let use_cap = ref false in
    let envname = ref "" in
    let capname = ref "" in

    let rec iter args =
            match args with
            | hd::tl -> 
            begin
                match hd with
                | "-h" -> usage std_OK;
                | "-c" -> use_cap := true;
                | _ -> 
                begin
                    if hd.[0] = '-' then
                    begin
                        out ("unknown option: "^hd); nl ();
                        usage std_ARGBAD; 
                    end;
                    if (!envname = "") then
                        envname := hd
                    else if (!capname = "") then
                        capname := hd
                    else
                        usage std_ARGBAD;
                    iter tl;
                end                        
            end;          
            | [] -> ()
            in
    iter args;
    if (!capname = "" || !envname = "") then
        usage std_ARGBAD;


    if (!use_cap = true) then
    begin
        let cap = ar_tocap !capname in
        update_cap !envname cap;
        put_env_cap !envname cap;
    end
    else
    begin
      let file =
        if (!capname.[0] = '/') then
        begin
            path_resolve !capname 
        end
        else
        begin
            match !work_dir with
            | Amoeba_path wd ->
                Amoeba_path (wd^"/"^(!capname));
            | Unix_path wd ->
                Unix_path (wd^"/"^(!capname));
        end; 
        in
        match file with
        | Amoeba_path file ->
        begin
            let stat,cap = name_lookup file in
            if (stat <> std_OK) then
                raise (Error stat);
            update_cap !envname cap;
            put_env_cap !envname cap;
        end;
        | Unix_path file ->
        begin
            let stat,cap = read_cap file in 
            if (stat <> std_OK) then
                raise (Error stat);
            update_cap !envname cap;
            put_env_cap !envname cap;
        end;
    end;    
  end
  with
        | Error err -> err

let put_env args =
  try
  begin
    let usage err =
            out "<path cap> := <environment variable>"; nl ();
            out "usage: [-h] put_env <envname> <path string>"; nl ();
            raise (Error err);
            in
    let envname = ref "" in
    let capname = ref "" in

    let rec iter args =
            match args with
            | hd::tl -> 
            begin
                match hd with
                | "-h" -> usage std_OK;
                | _ -> 
                begin
                    if hd.[0] = '-' then
                    begin
                        out ("unknown option: "^hd); nl ();
                        usage std_ARGBAD; 
                    end;
                    if (!envname = "") then
                        envname := hd
                    else if (!capname = "") then
                        capname := hd
                    else
                        usage std_ARGBAD;
                    iter tl;
                end                        
            end;          
            | [] -> ()
            in
    iter args;
    if (!capname = "" || !envname = "") then
        usage std_ARGBAD;


    let file =
        if (!capname.[0] = '/') then
        begin
            path_resolve !capname 
        end
        else
        begin
            match !work_dir with
            | Amoeba_path wd ->
                Amoeba_path (wd^"/"^(!capname));
            | Unix_path wd ->
                Unix_path (wd^"/"^(!capname));
        end; 
        in
     match file with
        | Amoeba_path file ->
        begin
            let stat,cap = get_env_cap !envname in
            if (stat <> std_OK) then
                raise (Error stat);
            
            let parent = Filename.dirname file in
            let name = Filename.basename file in

            let stat,dir = dir_lookup ~root:!root_cap ~name:parent in
            if (stat <> std_OK) then
                raise (Error stat);
            let stat = dir_append ~root:dir ~name:name ~obj:cap in
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end;
        | Unix_path file ->
        begin
            let stat,cap = get_env_cap !envname in
            if (stat <> std_OK) then
                raise (Error stat);
            
            let stat = write_cap file cap in 
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end;
  end
  with
        | Error err -> err

