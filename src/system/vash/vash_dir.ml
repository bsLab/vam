/*
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
**    $MODIFIED:    
**    $VERSION:     1.03
**
**    $INFO:
**
**  Directory handling
**
**    $ENDOFINFO
**
*/



open Unix

open Amoeba
open Buf
open Stderr
open Stdcom
open Dir
open Printf
open Ar
open Cap_env
open Name


open Vash_io
open Vash_env



let dir_pathlookup path =
    if (path <> "" &&
        path.[0] = '/') then
        dir_lookup ~root:!root_cap ~name:path
    else
        dir_lookup ~root:!work_cap ~name:("/"^path)



let dir args =
    let rec line1 i = 
        if (i > 0) then
            "-"^(line1 (i-1))
        else
            ""
    in

    let opt_info    = ref false in
    let opt_colri   = ref false in
    let opt_capdu   = ref false in
    let opt_help    = ref false in
    let opt_long    = ref false in
    let paths       = ref [] in

    let print_amdir root path =
        let lpos = ref 0 in
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then   out (line1 79);
        let stat,cp = dir_lookup ~root:root ~name:path in
        if (stat <> std_OK) then
            raise (Error stat);

        let stat,dirdesc = dir_open cp in

        if (stat = std_OK) then
        begin            
            if (!opt_long = true) ||
               (!opt_info = true) || 
                   (!opt_colri = true && !opt_capdu = false) ||
                   (!opt_capdu = true && !opt_colri = false) then
            begin            
                out (sprintf "%20s" "Name"); 
                if (!opt_colri = false && 
                    !opt_capdu = false &&
                    !opt_info = false ) then
                    out (sprintf "%20s" "Creation Time");
            end
            else if (!opt_colri = true && !opt_capdu = true) then
            begin
                out (sprintf "%10s" "Name"); 
            end; 

            if (!opt_colri = true) then
                out (sprintf "%20s" "Column rights");
            if (!opt_info = true) then
                out (sprintf "%20s" "Info");
            if (!opt_capdu = true) then
                out (sprintf "%30s" "Capability");
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then nl ();                        

            if (!opt_colri = true) then
            begin
                if (!opt_capdu = false) then
                    out (sprintf "%24s" "")
                else
                    out (sprintf "%14s" "");
                Array.iter (fun ci -> 
                        out (sprintf "%s " ci);
                    ) dirdesc.dir_colnames;
                nl ();
            end;
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then 
            begin
                out (line1 79); nl ();
            end;

            for i = 0 to (dirdesc.dir_nrows - 1)
            do
                let dr = dir_next dirdesc in
                let name = dr.dr_name in
                let cols = dr.dr_cols in
                let time = dr.dr_time in

                if (!opt_long = true) ||
                   (!opt_info = true) ||
                   (!opt_colri = true && !opt_capdu = false) ||
                   (!opt_capdu = true && !opt_colri = false) then
                begin            
                    let len = (String.length name) in
                    out (sprintf "%20s  " (if len > 20 then 
                                            String.sub name 0 20 
                                           else
                                            name));
                end
                else if (!opt_colri = true && !opt_capdu = true) then
                begin
                    let len = (String.length name) in
                    out (sprintf "%10s  " (if len > 10 then 
                                            String.sub name 0 10 
                                           else
                                            name));
                end 
                else
                begin
                    let len = (String.length name) + 1 in
                    if (!lpos + len >= 79) then
                    begin
                        lpos := 0;
                        nl();
                    end;
                    out (name^" ");
                    lpos := !lpos + len;
                end;

                if (!opt_long = true) then
                begin
                    (*
                    ** Hack:
                    ** New servers count time in 10s units,
                    ** old one in 1s units!
                    *)
                    let tm = if (time > 800000000) then
                                localtime (float_of_int time)
                             else
                                 localtime ((float_of_int time)*.10.0) in
                    out (sprintf "   %2.2d.%2.2d.%4.4d %2.2d:%2.2d"
                            tm.tm_mday
                            (tm.tm_mon+1)
                            (tm.tm_year+1900)
                            tm.tm_hour
                            tm.tm_min);
                end;

                if (!opt_colri = true) then
                begin
                    out "  ";
                    Array.iter (fun ci -> 
                                    out (sprintf "0x%2.2x  " ci);
                                ) cols;
                end;              
                if (!opt_info = true & !opt_colri = true) then
                begin
                    out (sprintf "%4s" "");
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        let err,info = std_info ~cap:cap ~bufsize:28 in
                        if (err = std_OK) then
                        begin
                            out (sprintf "%28s" info);
                        end
                        else
                            out (sprintf "%28s" (err_why err));
                    end;
                end
                else if (!opt_info = true & 
                         !opt_colri = false) then
                begin
                    out (sprintf "%4s" "");
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        let err,info = std_info ~cap:cap ~bufsize:40 in
                        if (err = std_OK) then
                        begin
                            out (sprintf "%s" info);
                        end
                        else
                            out (sprintf "%s" (err_why err));
                    end
                    else
                        out (sprintf "dir_lookup failed: %s" (err_why err));
                end
                else if (!opt_capdu = true) then
                begin
                    out (sprintf "%4s" "");
                    let err,cap = dir_lookup ~root:cp ~name:name in
                    if (err = std_OK) then
                    begin
                        out (sprintf "%s" (ar_cap cap));
                    end;
                end;  
                if (!opt_long || 
                    !opt_colri || 
                    !opt_info || 
                    !opt_capdu = true) then  nl ();
            done;      
            nl ();
            dir_close dirdesc; 
        end
        else if (stat = std_COMBAD) then
        begin
            (*
            ** It's not a directory. Get the parent directory instead.
            *)
            let fname = Filename.basename path in
            let err,cap = dir_lookup ~root:root 
                                     ~name:(Filename.dirname path) in

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
                            let dr = dd.dir_rows.(i) in
                            let name = dr.dr_name in
                            let cols = dr.dr_cols in
                            let time = dr.dr_time in

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
                        let dr = dd.dir_rows.(!dei) in
                        let name = dr.dr_name in
                        let cols = dr.dr_cols in
                        let time = dr.dr_time in

                        if (!opt_long || 
                            !opt_colri || 
                            !opt_info || 
                            !opt_capdu = true) then                         
                        begin
                            nl (); out (sprintf "%20s" "Name"); 
                        end;
                        if (!opt_colri = true) then
                        begin
                            out (sprintf "%20s" "Column rights");
                        end;
                        if (!opt_info = true) then
                        begin
                            out (sprintf "%20s" "Info");
                        end;
                        if (!opt_capdu = true) then
                        begin
                            out (sprintf "%30s" "Capability");
                        end;
                        if (!opt_long || 
                            !opt_colri || 
                            !opt_info || 
                            !opt_capdu = true) then 
                            nl ();

                        if (!opt_colri = true) then
                        begin
                            out (sprintf "%24s" "");
                            Array.iter (fun ci -> 
                                        out (sprintf "%6s " ci);
                                    ) dd.dir_colnames;
                            nl ();
                        end;

                        if (!opt_long || 
                            !opt_colri || 
                            !opt_info || 
                            !opt_capdu = true) then                         
                        begin
                            out (line1 79); nl ();
                        end;
                        out (sprintf "%20s    " path); 
                        if (!opt_colri = true) then
                        begin
                            Array.iter (fun ci -> 
                                            out (sprintf "%6x " ci);
                                        ) cols;
                        end;              
                        if (!opt_info = true & !opt_colri = true) then
                        begin
                            out (sprintf "%4s" "");
                            let err,info = std_info ~cap:cp ~bufsize:28 in
                            if (err = std_OK) then
                            begin
                                out (sprintf "%28s" info);
                            end
                            else
                                out (sprintf "%28s" (err_why err));
                        end 
                        else if (!opt_info = true & !opt_colri = false) then
                        begin
                            out (sprintf "%4s" "");
                            let err,info = std_info ~cap:cp ~bufsize:40 in
                            if (err = std_OK) then
                            begin
                                out (sprintf "%s" info);
                            end
                            else
                                out (sprintf "%s" (err_why err));
                        end
                        else if (!opt_capdu = true) then
                        begin
                            out (sprintf "%s" (ar_cap cp));
                        end;
                        if (!opt_long || 
                            !opt_colri || 
                            !opt_info || 
                            !opt_capdu = true) then                         
                            nl ();
                    end;

                    nl();

                    dir_close dd;                                     
                end
                else
                begin
                    out ( 
                                   "dir: Can't lookup parent ("^
                                   (Filename.dirname path)^") :"^
                                   (err_why err)
                                 );
                    nl ();
                end;

            end
        end
        else 
            raise (Error stat);
    in

    let print_unixdir path =
        let unix_cols = [|"owner";"group";"other"|] in
        try
        begin
            let dir = opendir path in
            let direntr = ref [] in
            begin
                try while (true) 
                do
                    let de = readdir dir in
                    direntr := !direntr @ [de];
                done
                with
                    | End_of_file -> ();
            end;
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then   out (line1 79);

            if (!opt_long = true) ||
               (!opt_info = true) || 
                   (!opt_colri = true && !opt_capdu = false) ||
                   (!opt_capdu = true && !opt_colri = false) then
            begin            
                out (sprintf "%20s" "Name"); 
                if (!opt_colri = false && 
                    !opt_capdu = false &&
                    !opt_info = false ) then
                    out (sprintf "%20s" "Modification Time");
            end
            else if (!opt_colri = true && !opt_capdu = true) then
            begin
                out (sprintf "%10s" "Name"); 
            end; 

            if (!opt_colri = true) then
                out (sprintf "%20s" "Column rights");
            if (!opt_info = true) then
                out (sprintf "  %20s        %20s" "Info" "Size [bytes]");
            if (!opt_capdu = true) then
                out (sprintf "%30s" "Capability");
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then nl ();                        

            if (!opt_colri = true) then
            begin
                if (!opt_capdu = false) then
                    out (sprintf "%24s" "")
                else
                    out (sprintf "%14s" "");
                Array.iter (fun ci -> 
                        out (sprintf "%s " ci);
                    ) unix_cols;
                nl ();
            end;
            if (!opt_long || 
                !opt_colri || 
                !opt_info || 
                !opt_capdu = true) then 
            begin
                out (line1 79); nl ();
            end;

            if (!opt_long = false &&
                !opt_colri = false &&
                !opt_info = false) then
            begin
                let lpos = ref 0 in
                List.iter (fun name ->
                    let len = (String.length name) + 1 in
                    if (!lpos + len >= 79) then
                    begin
                        lpos := 0;
                        nl();
                    end;
                    out (name^" ");
                    lpos := !lpos + len;
                    ) !direntr;
                nl ();
            end
            else
            if (!opt_long = true &&
                !opt_colri = false &&
                !opt_info = false) then
            begin
                let lpos = ref 0 in
                List.iter (fun name ->
                    let len = (String.length name) + 1 in
                    let name =
                        if (len >= 20) then
                            String.sub name 0 20 
                        else
                            name
                        in
                    out (sprintf "%20s" name);
                    let stat = Unix.stat (path^"/"^name) in
                    let tm = localtime stat.st_mtime in
                    out (sprintf "   %2.2d.%2.2d.%4.4d %2.2d:%2.2d"
                            tm.tm_mday
                            (tm.tm_mon+1)
                            (tm.tm_year+1900)
                            tm.tm_hour
                            tm.tm_min);

                    nl ();
                    ) !direntr;
            end
            else if (!opt_colri = true) then
            begin
                let lpos = ref 0 in
                List.iter (fun name ->
                    let len = (String.length name) + 1 in
                    let name =
                        if (len >= 20) then
                            String.sub name 0 20 
                        else
                            name
                        in
                    out (sprintf "%20s" name);
                    let stat = Unix.stat (path^"/"^name) in
                    let r = stat.st_perm in

                    out "     ";                    
                    out (if (r land 64 = 64) then "X" else "-");
                    out (if (r land 128 = 128) then "W" else "-");
                    out (if (r land 256 = 256) then "R" else "-");
                    out "   ";                    
                    out (if (r land 8 = 8) then "X" else "-");
                    out (if (r land 16 = 16) then "W" else "-");
                    out (if (r land 32 = 32) then "R" else "-");
                    out "   ";                    
                    out (if (r land 1 = 1) then "X" else "-");
                    out (if (r land 2 = 2) then "W" else "-");
                    out (if (r land 4 = 4) then "R" else "-");

                    nl ();
                    ) !direntr;
            end
            else if (!opt_info = true) then
            begin
                let lpos = ref 0 in
                List.iter (fun name ->
                    let len = (String.length name) + 1 in
                    let name =
                        if (len >= 20) then
                            String.sub name 0 20 
                        else
                            name
                        in
                    out (sprintf "%20s" name);
                    let stat = Unix.stat (path^"/"^name) in
                    out (sprintf "   %20s" (
                            match stat.st_kind with
                            | S_REG -> "Regular file";
                            | S_DIR -> "Directory";
                            | S_CHR -> "Character device";
                            | S_BLK -> "Block device";
                            | S_LNK -> "Symbolic link";
                            | S_FIFO -> "Named pipe";
                            | S_SOCK -> "Socket";
                        ));
                    out (sprintf "%20d" stat.st_size);
                    nl ();
                    ) !direntr;
            end;
        end
        with
            | Unix_error (ENOTDIR,_,_) ->
            begin
                ()
            end;
            | _ -> out (path^": not found!"); nl ();
        in

    let usage err =
            out "usage: dir [-l -h -i -r -c] <path1> [<path2> ...]"; nl ();
            raise (Error err);
        in

    let envnames = ref [] in
    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-i" -> opt_info := true; iter tl;
            | "-r" -> opt_colri := true; iter tl;
            | "-c" -> opt_capdu := true; iter tl; 
            | "-l" -> opt_long := true; iter tl;
            | _ -> 
            begin
                if hd.[0] = '-' then
                begin
                    out ("unknown option: "^hd); nl ();
                    usage std_ARGBAD; 
                end;
                if hd.[0] = '/' then
                    paths := !paths @ [path_resolve hd]
                else
                begin
                    match !work_dir with
                    | Amoeba_path wd ->
                        paths := !paths @ [Amoeba_path (wd^"/"^hd)];
                    | Unix_path wd ->
                        paths := !paths @ [Unix_path (wd^"/"^hd)];
                end;
            end;
            iter tl;
        end;
        | [] -> ()
        in
    iter args;

    if (!paths = []) then
        paths := [!work_dir];

    List.iter (fun hd ->
            let path = hd in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                      try
                      begin
                        print_amdir !root_cap path;
                      end
                      with
                        | Error err -> out ("'"^path^"': failed:"^
                                            (err_why err));
                                       nl ();
                        | Failure str -> 
                                       out ("failed:"^str); 
                                       nl (); 
                    end;
                    | Unix_path path ->
                    begin   
                        print_unixdir path;
                    end;
                end;
        ) !paths;
    std_OK

let last_workdir = ref (!work_dir)

let cd args =
    try
    begin
        match args with
        | hd::tl ->
        begin
            if (tl <> []) then
                raise (Error std_ARGBAD);

            match hd with

            | ".." -> 
            begin
                match !work_dir with
                | Amoeba_path wd ->
                begin
                    last_workdir := !work_dir;
                    let parent = Filename.dirname wd in
                    let stat,cap = name_lookup parent in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    let stat,str = std_info cap 10 in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    if (str <> "" && 
                        str.[0] <> '/' &&
                        str.[0] <> '%') then
                        raise (Error std_WRONGSRV);

                    work_dir := (Amoeba_path parent);
                    work_cap := cap;
                    std_OK;
                end;
                | Unix_path wd ->
                begin
                    let tmp = !work_dir in
                    let parent = Filename.dirname wd in
                    if (parent = "/" && 
                        !work_dir = Unix_path "/") then    (* !!! *)
                    begin
                        work_dir := Amoeba_path "/";
                        let stat,cap = name_lookup parent in
                        work_cap := cap;
                        if (stat <> std_OK) then
                            raise (Error stat);
                        last_workdir := tmp;
                        std_OK;
                    end
                    else
                    begin
                        (*
                        ** Check for a valid directory.
                        *)
                        try
                        begin
                            let dir = opendir parent in
                            closedir dir;
                            work_dir := Unix_path parent;
                            work_cap := nilcap;
                            last_workdir := tmp;
                            std_OK;
                        end
                        with
                            | Unix_error _ -> std_NOTFOUND;
                    end;
                end;
            end;

            | "-" ->
            begin
                let tmp = !work_dir in
                match !work_dir with
                | Amoeba_path wd ->
                begin
                    let stat,cap = name_lookup wd in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    let stat,str = std_info cap 10 in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    if (str <> "" && 
                        str.[0] <> '/' &&
                        str.[0] <> '%') then
                        raise (Error std_WRONGSRV);
                    work_dir := !last_workdir;
                    last_workdir := tmp;
                    work_cap := cap;
                    std_OK;
                end;
                | Unix_path wd ->
                begin
                    work_dir := !last_workdir;
                    last_workdir := tmp;
                    work_cap := nilcap;
                    std_OK; 
                end;
                
            end;

            | _ ->
            begin
                let tmp = !work_dir in
                if (hd.[0] = '/') then
                begin
                    (*
                    ** Absolute path
                    *)
                    let strlen = String.length hd in
                    if (strlen >= 5 && (String.sub hd 0 5) = "/unix") then
                    begin
                        (*
                        ** Check for a valid directory.
                        *)
                        try
                        begin
                            let path = match (path_resolve hd) with
                                       | Unix_path p -> p;
                                       | _ -> failwith "progerr";
                                in
                            let dir = opendir path in
                            closedir dir;
                            work_dir := Unix_path path;
                            work_cap := nilcap;
                            last_workdir := tmp;
                            std_OK;
                        end
                        with
                            | Unix_error _ -> std_NOTFOUND;
                    end
                    else
                    begin
                        let stat,cap = name_lookup hd in
                        if (stat <> std_OK) then
                        begin
                            raise (Error stat);
                        end;
                        let stat,str = std_info cap 10 in
                        if (stat <> std_OK) then
                            raise (Error stat);
                        if (str <> "" && 
                            str.[0] <> '/' &&
                            str.[0] <> '%') then
                            raise (Error std_WRONGSRV);

                        work_dir := (Amoeba_path hd);
                        last_workdir := tmp;
                        work_cap  := cap;
                        std_OK;
                    end;
                end
                else
                begin
                    (*
                    ** Relative to working directory path
                    *)
                    match !work_dir with
                    | Amoeba_path wd ->
                    begin
                        let strlen = String.length hd in
                        if (wd = "/" && strlen >= 4 &&
                            (String.sub hd 0 4) = "unix") then
                        begin
                            (*
                            ** Check for a valid directory.
                            *)
                            try
                            begin
                                let path = match (path_resolve ("/"^hd)) with
                                           | Unix_path p -> p;
                                           | _ -> failwith "progerr";
                                    in
                                let dir = opendir path in
                                closedir dir;
                                work_dir := Unix_path path;
                                work_cap := nilcap;
                                last_workdir := tmp;
                                std_OK;
                            end
                            with
                                | Unix_error _ -> std_NOTFOUND;
                        end
                        else
                        begin
                            let path = wd^(if wd = "/" then "" else "/")^hd
                            in
                            let stat,cap = name_lookup path in
                            if (stat <> std_OK) then
                            begin
                                raise (Error stat);
                            end;
                            let stat,str = std_info cap 10 in
                            if (stat <> std_OK) then
                                raise (Error stat);
                            if (str <> "" && 
                                str.[0] <> '/' &&
                                str.[0] <> '%') then
                                raise (Error std_WRONGSRV);
                            work_dir := Amoeba_path path;
                            work_cap  := cap;
                            last_workdir := tmp;
                            std_OK;
                        end;
                    end;
                    | Unix_path wd ->
                    begin
                        (*
                        ** Check for a valid directory.
                        *)
                        try
                        begin
                            let path = (wd^(if wd = "/" then "" else "/")^hd)
                                in
                            let dir = opendir path in
                            closedir dir;
                            work_dir := Unix_path path;
                            work_cap := nilcap;
                            last_workdir := tmp;
                            std_OK;
                        end
                        with
                            | Unix_error _ -> std_NOTFOUND;
                    end;
                end;
            end;
        end;
        | [] -> raise (Error std_ARGBAD);
    end
    with
        | Error err -> err
        | Failure str -> out ("failed:"^str); nl (); std_SYSERR   


let mkdir args =
  try
  begin
    let usage err =
            out "usage: mkdir [-h] <path1> [<path2> ...]"; nl ();
            raise (Error err);
            in
    let paths = ref [] in
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

                paths := !paths @ [path];
            end;
            iter tl;
        end;
        | [] -> ()
        in
    iter args;
    List.iter ( fun p ->
            let path = path_resolve p in
            begin
                match path with
                | Amoeba_path path ->
                begin
                    let stat,parent = if (path.[0] = '/') then
                                        dir_lookup ~root:!root_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                                      else 
                                      if (Filename.dirname path = ".") 
                                        then
                                        std_OK,!work_cap
                                      else
                                        dir_lookup ~root:!work_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                            in
                    if (stat <> std_OK) then
                        raise (Error stat);

                    let stat,newdir = dir_create ~server:parent in

                    if (stat <> std_OK) then
                        raise (Error stat);

                    let stat = dir_append ~root:parent
                                          ~name:(Filename.basename path)
                                          ~obj:newdir
                    in
                    if (stat <> std_OK) then
                        raise (Error stat);
                end;
                | Unix_path path ->
                begin   

                end;
            end
        ) !paths;
    std_OK
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR   

let del args =
  try
  begin
    let usage err =
            out "usage: del [-h -d -f] <path1> [<path2> ...]"; nl ();
            raise (Error err);
            in
    let paths = ref [] in
    let delforced = ref false in
    let destroy = ref false in

    let rec iter args =
        match args with
        | hd::tl -> 
        begin
            match hd with
            | "-h" -> usage std_OK;
            | "-d" -> delforced := true; iter tl;
            | "-f" -> destroy := true; iter tl;
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
                paths := !paths @ [path];
            end;
            iter tl;
        end;
        | [] -> ()
        in
    iter args;
    List.iter ( fun p ->
            let path = path_resolve p in
            begin
                match path with
                | Amoeba_path path ->
                begin
                    let stat,parent = if (path.[0] = '/') then
                                        dir_lookup ~root:!root_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                                      else 
                                      if (Filename.dirname path = ".") 
                                        then
                                        std_OK,!work_cap
                                      else
                                        dir_lookup ~root:!work_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                    in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    if (!delforced = false) then
                    begin
                        let stat,cap = dir_lookup ~root:parent
                                                  ~name:(
                                            Filename.basename path
                                            ) in
                        if (stat <> std_OK) then
                        begin
                            raise (Error stat);
                        end;
                        let stat,info = std_info cap 10 in

                        if (stat <> std_OK) then
                            raise (Error stat);

                        if (info.[0] = '/' ||
                            info.[0] = '%') then
                        begin
                            out "- is a directory (use -d to override)";
                            nl();
                            raise (Error std_ARGBAD);
                        end;
                        if (!destroy) then  
                            ignore(std_destroy cap);
        
                    end;                                 
                    let stat = dir_delete ~root:parent
                                          ~name:(Filename.basename path)
                    in
                    if (stat <> std_OK) then
                        raise (Error stat);

                end;
                | Unix_path path ->
                begin   
                end;
            end
        ) !paths;
    std_OK
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR   


let a2c args =
  try
  begin
    let usage err =
            out "usage: a2c [-h] \"0:0:0...CAP-format\" <filename>"; nl ();
            raise (Error err);
            in
    let cap = ref "" in
    let file = ref "" in

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
                if (!cap = "") then
                    cap := hd
                else if (!file = "") then
                    file := hd
                else
                    usage std_ARGBAD;
            end;
            iter tl;
        end;
        | [] -> ()
        in
    iter args;

    if (!cap = "" || !file = "") then
        usage std_ARGBAD;

    let filepath =
      if !file.[0] <> '/' then
      begin
        match !work_dir with
        | Amoeba_path wd ->
           wd^"/"^ !file;
        | Unix_path wd ->
           "/unix/"^wd^"/"^ !file;
        end
        else
          !file in
    
    let path = path_resolve filepath in
    begin
        match path with
        | Amoeba_path path ->
        begin
            let stat,parent = if (path.[0] = '/') then
                                        dir_lookup ~root:!root_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                                      else 
                                      if (Filename.dirname path = ".") 
                                        then
                                        std_OK,!work_cap
                                      else
                                        dir_lookup ~root:!work_cap
                                                   ~name:(
                                            Filename.dirname path
                                        )
                in
            if (stat <> std_OK) then
                raise (Error stat);
            let newcap = ar_tocap !cap in
            let stat = dir_append ~root:parent
                                  ~name:(Filename.basename path)
                                  ~obj:newcap
                in
            if (stat <> std_OK) then
                raise (Error stat);
        end;
        | Unix_path path ->
        begin   
        end;
    end;
    std_OK
  end
  with
    | Error err -> err
    | Failure str -> out ("failed:"^str); nl (); std_SYSERR   


let c2a args =
    let usage err =
            out "usage: c2a [-h] <path1> [<path2> ...]"; nl ();
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
                let path = if (hd <> "" && hd.[0] = '/') then
                                path_resolve hd 
                           else
                           begin
                                match !work_dir with
                                | Amoeba_path wd ->
                                    Amoeba_path (wd^"/"^hd);
                                | Unix_path wd ->
                                    Unix_path (wd^"/"^hd);
                           end
                    in
                begin
                    match path with
                    | Amoeba_path path ->
                    begin
                        let stat,cap = dir_pathlookup path in
                        if (stat = std_OK) then
                        begin
                            out (ar_cap cap); nl ();
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
                        let stat,cap = read_cap path in
                        if (stat <> std_OK) then
                            raise (Error stat);
                        out (sprintf "%30s" (ar_cap cap)); nl ();                     
                    end;
                end;
                iter tl;
            end;
        end;
        | [] -> ()
        in
    iter args;
    std_OK
