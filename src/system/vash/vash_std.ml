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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:     ??
**    $VERSION:     1.01
**
**    $INFO:
**
**
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
open Vash_io
open Vash_env
open Cap_env
open Name
open Stdcom
open Stdcom2



let info args =
    let usage err =
            out "usage: std_info <path1> [<path2> ...]"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
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
                            let stat,str = std_info cap 1000 in
                            if (stat = std_OK) then
                            begin
                                out (str); nl ();
                            end
                            else
                            begin
                                out ("'"^path^
                                     "': std_info failed: "^(err_why stat));
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
                        try
                        begin
                            let stats = Unix.stat path in
                            let kind = 
                                match stats.st_kind with
                                | S_REG -> "Regular file";
                                | S_DIR -> "Directory";
                                | S_CHR -> "Character device";
                                | S_BLK -> "Block device";
                                | S_LNK -> "Symbolic link";
                                | S_FIFO -> "Named pipe";
                                | S_SOCK -> "Socket";
                            in
                            out ("Unix "^kind); nl ();
                        end
                        with
                            | _ -> out ("'"^path^
                                        "': Unix file/directory not found"); 
                                   nl ();
                    end;
                end;

                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK


let status args =
    let usage err =
            out "usage: std_status <path1> [<path2> ...]"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
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
                            let stat,str = std_status cap 30000 in
                            if (stat = std_OK) then
                            begin
                                out (str); nl ();
                            end
                            else
                            begin
                                out ("'"^path^"': std_status failed: "^
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
                        out ("- Unix file/directory"); nl ();
                    end;
                end;

                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK


let exit args =
    let usage err =
            out "usage: std_exit <server path1> [<server path2> ...]"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
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
                let src = 
                        if (hd.[0] = '/') then
                            path_resolve hd
                        else
                        match !work_dir with
                        | Amoeba_path wd ->
                        begin
                            Amoeba_path (wd^"/"^(hd));
                        end;
                        | Unix_path wd ->
                        begin
                            Unix_path (wd^"/"^(hd));
                        end;
                    in

                let path = src in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                        let stat,cap = dir_pathlookup path in
                        if (stat = std_OK) then
                        begin
                            let stat = std_exit cap in
                            if (stat <> std_OK) then
                            begin
                                out ("'"^path^"': std_exit failed: "^(err_why stat));
                                nl ();
                            end;
                        end
                        else
                        begin
                            out ("'"^path^"': dir_lookup failed: "^(err_why stat));
                            nl ();
                        end;
                    end;
                    | Unix_path path ->
                    begin
                        out ("'"^path^"': Not supported on Unix file/directory"); 
                        nl ();
                    end;
                end;

                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK


let exec args =
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
  
    let usage err =
            out "usage: std_exec [-h -f] <serverpath> exec arguments"; nl ();
            out "       -f: load content from generic file (filename== arg 2..N!)"; nl(); 
            out "       -F: load content from 4th source file (filename== arg 2..N!)"; nl(); 
            raise (Error err);
            in
    let eargs = ref [] in
    let serverpath = ref "" in
    let filemode = ref false in
    let vm4mode = ref false in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-f" -> if !vm4mode then usage std_ARGBAD; filemode := true; iter tl;
            | "-F" -> if !filemode then usage std_ARGBAD; vm4mode := true; iter tl;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!serverpath = "") then
                    serverpath := hd
                else
                begin
                    if (!filemode = false && !vm4mode = false || 
                        !eargs = []) then
                    begin
                        eargs := !eargs @ [resolve_esc hd];
                    end
                    else if (!filemode = true) then
                    begin
                        (*
                        ** Load content of file and append it
                        ** to the argument list.
                        *)
                        let src = 
                        if (hd.[0] = '/') then
                            path_resolve hd
                        else
                        match !work_dir with
                        | Amoeba_path wd ->
                        begin
                            Amoeba_path (wd^"/"^(hd));
                        end;
                        | Unix_path wd ->
                        begin
                            Unix_path (wd^"/"^(hd));
                        end;
                        in

                        let path = src in
                        match path with
                        | Amoeba_path path ->
                        begin
                            out ("'"^path^"': Currently not supported on Amoeba files"); 
                            nl ();
                        end;
                        | Unix_path path ->
                        try
                          begin
                            (*
                            ** First determine file size...
                            *)
                            let fds  = Unix.stat path in
                            let size = fds.st_size in
                            let str = String.create size in

                            let fdin = Unix.openfile path [O_RDONLY] 0
                                       in 
                            let n = Unix.read fdin str 0 size in
                            Unix.close fdin;
                            if (n <> size) then
                                raise (Error std_IOERR);
                            eargs := !eargs @ [str];
                        end;
                        with | Unix_error (_,_,_) -> 
                                raise (Error std_NOTFOUND);
                    end
                    else 
                    begin
                        (*
                        ** Load 4th source content from file and append it
                        ** to the argument list. Resolve needs!
                        *)
                        let src = 
                        if (hd.[0] = '/') then
                            path_resolve hd
                        else
                        match !work_dir with
                        | Amoeba_path wd ->
                        begin
                            Amoeba_path (wd^"/"^(hd));
                        end;
                        | Unix_path wd ->
                        begin
                            Unix_path (wd^"/"^(hd));
                        end;
                        in

                        let path = src in
                        match path with
                        | Amoeba_path path ->
                        begin
                            out ("'"^path^"': Currently not supported on Amoeba files"); 
                            nl ();
                        end;
                        | Unix_path path ->
                        begin
                            (*
                            ** First determine file size...
                            *)
                            let read_file name =
                              try
                              begin
                                let fdin = Unix.openfile name [O_RDONLY] 0
                                           in 
                                let fds  = Unix.stat name in
                                let size = fds.st_size in
                                let str = String.create size in
                                let n = Unix.read fdin str 0 size in
                                Unix.close fdin;
                                if (n <> size) then
                                    raise (Error std_IOERR);
                                str
                              end
                              with | Unix_error (_, _, _)->
                                        raise (Error std_NOTFOUND);     
                                in

                            (*
                            ** Now search the sources for [needs] words.
                            ** If found, search first in the directory
                            ** from where the source comes from, then
                            ** the generic vm4 environment directory
                            ** ENV_VM4 for the desired file.
                            *)
                            let env4th = try 
                                            Unix.getenv "ENV_VM4" 
                                         with
                                            | Not_found -> ""
                                in

                            let rec resolve name =
                                let str = "\n" ^ (
                                  try
                                    read_file name
                                  with
                                    | _ -> try read_file
                                               (env4th^"/"^name) with
                                    | _ -> out ("std_exec: can't find "^name);
                                           nl (); raise (Error std_ARGBAD);
                                    ) 
                                    in

                                let str' = ref "" in
                                let strl = Str.split (Str.regexp "\[needs ")
                                                     str
                                    in
                                if (List.length strl = 1) then
                                    str
                                else
                                begin
                                    let str' = ref "" in
                                    List.iter (fun str ->
                                        if (!str' = "") then
                                        begin
                                            str' := !str' ^ str;
                                        end
                                        else
                                        begin
                                            let strl' = Str.split 
                                                        (Str.regexp "\]$")
                                                        str
                                                in
                                            if (List.length strl' = 1) then
                                                raise (Error std_ARGBAD);
                                            str' := !str' ^
                                                    (resolve (List.hd strl')) ^
                                                    (List.nth strl' 1);
                                        end;
                                        ) strl;
                                    !str';
                                end;
                                in
                            let str = try 
                                        resolve path 
                                      with _ -> raise (Error std_ARGBAD)
                                in    
                            eargs := !eargs @ [str];
                        end;
                    end;
                end;
                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    if (!eargs = [] || !serverpath = "") then
        usage std_ARGBAD;


    let path = path_resolve !serverpath in
    match path with
        | Amoeba_path path ->
        begin
            let stat,cap = dir_pathlookup path in
            if (stat <> std_OK) then
            begin
                out ("dir_lookup failed: "^path); nl ();
                raise (Error stat);
            end;
            let stat,str = std_exec cap !eargs in
            if (stat <> std_OK) then
            begin
                out "std_exec failed"; nl ();
                raise (Error stat);
            end;
            out (resolve_esc_versa str); nl ();
            std_OK
        end;
        | Unix_path path ->
        begin
            out ("'"^path^"': Not supported on Unix file/directory"); 
            nl ();
            std_ARGBAD
        end


let params args =
  
    let usage err =
            out "usage: std_params [-h -v] <serverpath> [-s name:val]"; nl ();
            out "       -s: set parameter"; nl(); 
            out "       -v: show full parameter description"; nl(); 
            raise (Error err);
            in
    let sargs = ref [] in
    let serverpath = ref "" in
    let verb = ref false in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-v" -> verb := true; iter tl;
            | "-s" ->
            begin
                match tl with
                | hd::tl ->
                begin
                    let sa = Str.split (Str.regexp ":") hd in
                    match sa with
                    | name::vall -> 
                    begin
                        match vall with
                        | valu::_ -> sargs := !sargs @ [name,valu]; iter tl;
                        | [] -> usage std_ARGBAD;
                    end;
                    | [] -> usage std_ARGBAD;
                end;
                | [] -> usage std_ARGBAD;
            end;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if (!serverpath = "") then
                    serverpath := hd;
                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    if (!serverpath = "") then
        usage std_ARGBAD;

    let serverpath =
        if !serverpath.[0] <> '/' then
        begin
            match !work_dir with
            | Amoeba_path wd ->
                wd^"/"^ !serverpath;
            | Unix_path wd ->
                "/unix/"^wd^"/"^ !serverpath;
        end
        else
            !serverpath in

    let path = path_resolve serverpath in
    match path with
        | Amoeba_path path ->
        begin
            let stat,cap = dir_pathlookup path in
            if (stat <> std_OK) then
            begin
                out ("dir_lookup failed: "^path); nl ();
                raise (Error stat);
            end;
            if (!sargs = []) then
            begin
                let stat,params = std_get_params cap in
                if (stat <> std_OK) then
                begin
                    out "std_params failed"; nl ();
                    raise (Error stat);
                end;
                List.iter (fun p ->
                        let name,range,desc,valu=p in
                        let str = Printf.sprintf 
                                    "%s: %s [%s]\n    {%s}"
                                    name valu range desc in
                        out str; nl ();
                    ) params;
                std_OK
            end
            else
            begin
                let stat = std_set_params cap !sargs in
                if (stat <> std_OK) then
                begin
                    out "std_params failed"; nl ();
                    raise (Error stat);
                end;
                std_OK;
            end;
        end;
        | Unix_path path ->
        begin
            out ("'"^path^"': Not supported on Unix file/directory"); 
            nl ();
            std_ARGBAD
        end


let touch args =
    let usage err =
            out "usage: std_touch <path1> [<path2> ...]"; nl ();
            raise (Error err);
            in
    let envnames = ref [] in
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
                            let stat = std_touch cap in
                            if stat <> std_OK then
                            begin
                                out ("'"^path^
                                     "': std_info failed: "^(err_why stat));
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
                        raise (Error std_NOTNOW);
                    end;
                end;

                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK

    