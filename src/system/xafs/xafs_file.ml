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
**    $INITIAL:     (C) BSSLAB 2003-2005
**    $CREATED:     2003.0.0
**    $VERSION:     1.10
**
**    $INFO:
**
** Amoeba-Unix filesystem  
**
**    $ENDOFINFO
**
*)

open Xtypes
open WX_types
open WX_tree

open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Cap_env
open Thread
open Bytebuf
open Sema

open Afs_common
open Afs_client
open Dns_client


open Xafs_widget
open Unix
open Printf
open Capset


exception AmError of status

(*
** Directory entry (branches)
*)

type dir_desc = {
    dir_name : string;
    dir_path : string;
    dir_tree : WX_tree.t;
    dir_label : WX_label.t;
    dir_branch : WX_tree.node;
    mutable dir_selected : bool;
    mutable dir_open : bool;
}

(*
** File entry
*)

type file_desc = {
    file_name : string;
    file_path : string;
    file_label : WX_label.t;
    file_leaf : WX_tree.node;
    mutable file_selected : bool;
}

(*
** List of all selected afs files and directories
*)

let sel_objs_afs = ref []
let sel_dirs_afs = ref []

(*
** List of all selected unix files and directories
*)

let sel_files_unix = ref []
let sel_dirs_unix = ref []

let verbose = ref false
let copy_buf = ref (nilcap,"")

(*
** File copy flags
*)

type copy_flags = {
        mutable copy_overwrite: bool;
        mutable copy_destroy: bool;
    }

let copy_flags = {copy_overwrite=false; copy_destroy=false}


(*
** Delete flags
*)

type del_flags = {
        mutable del_destroy: bool;
    }

let del_flags = {del_destroy=false}

let dying = ref false


(*
** Service thread. The background worker for std_info et al.
*)

let service_sema = sema_create 0 
let service_fun = ref (fun () -> ())

let service_thr () =
    while (!dying = false)
    do
        sema_down service_sema;
        let func = !service_fun in
        func ();
    done

(*
** Unselect all after an operation (copy, delete, ...)
*)
let unsel_all () =
    let dl = sel_dirs_unix in
    List.iter (fun dir ->
        dir.dir_label#configure [Background "lightblue"];
        dir.dir_label#refresh;
        dir.dir_selected <- false;
        ) !dl;
    dl := [];
    let fl = sel_files_unix in
    List.iter (fun file ->
        file.file_label#configure [Background "lightblue"];
        file.file_label#refresh;
        file.file_selected <- false;
        ) !fl;  
    fl := [];
    let dl = sel_dirs_afs in
    List.iter (fun dir ->
        dir.dir_label#configure [Background "lightblue"];
        dir.dir_label#refresh;
        dir.dir_selected <- false;
        ) !dl;
    dl := [];
    let fl = sel_objs_afs in
    List.iter (fun file ->
        file.file_label#configure [Background "lightblue"];
        file.file_label#refresh;
        file.file_selected <- false;
        ) !fl;  
    fl := [];
    cur_path := "";
    cur_sel := "";
    input_path#set_string "";
    input_sel#set_string ""

    
    
let file_cap = ref nilcap

(*
** Get default file server capability for current parent directory dir!
*)
let init_file_cap parentdir =
    let stat,dircap = name_lookup parentdir in
    if stat = std_OK then
    begin
        let dircs = cs_singleton dircap in
        let stat,cap = dns_getdefafs dircs in
        if stat = std_OK then
            file_cap := cap
    end
    else if (!file_cap = nilcap) then
    begin
        let stat,cap = get_env_cap "AFS" in 
        if (stat <> std_OK) then
        begin
            let stat,cap = name_lookup "/server/afs" in
            if (stat = std_OK) then
                file_cap := cap
            else
            begin
                let stat,cap = name_lookup "/profile/cap/afs/server" in
                if (stat = std_OK) then
                    file_cap := cap
                else
                begin
                    print "no way to get the fileserver cap!\n"; 
                    raise (AmError std_NOTFOUND); (* no idea anymore ! *)
                end;
            end; 
        end
        else
            file_cap := cap;
    end
    

(*
** Load a unix directory. Return sub dir and file list.
*)

let load_unix_dir dirname =
    Db.Pr.ss 10 "load_unix_dir" dirname;

    let filenames' = Utils.list_dir dirname in
    let subdirs = List.fold_left (fun files filename ->
          if filename <> "." && filename <> ".." then
            let fullname = Filename.concat dirname filename in
            let stats = Unix.lstat fullname in
            if stats.st_kind = S_DIR then filename::files else
              files
          else files
      ) [] filenames' in
    let filenames = List.filter ( fun filename ->
                    if filename <> "." && filename <> ".."  &&
                    ( 
                        let fullname = Filename.concat dirname filename in
                        (Unix.lstat fullname).st_kind <> S_DIR
                    )
                    then
                        true
                    else
                        false
                    ) filenames' in
    (Sort.list (fun a b ->
                    let na,_ = a in
                    let nb,_ = b in
                    na <= nb) (List.map (fun d -> d,std_OK)
                                subdirs),
     Sort.list (fun a b ->
                   let na,_ = a in
                   let nb,_ = b in
                   na <= nb) (List.map (fun f -> f,std_OK) filenames))

(*
** Load an afs directory. Return sub dir and file list.
*)

let load_afs_dir dirname =
  try
  begin
    Db.Pr.ss 10 "load_afs_dir" dirname;

    let err,dir = dir_lookup ~root:(!dns_root) ~name:dirname in

    Db.Pr.ss 10 "load_afs_dir: dir_lookup" (err_why err);

    if (err <> std_OK) then
        raise (AmError err);

    let err,dircont = dir_open dir in

    Db.Pr.ss 10 "load_afs_dir: dir_open" (err_why err);

    if (err <> std_OK) then
        raise (AmError err);
    let dira = dircont.dir_rows in

    let subdirs = ref [] in
    let filenames = ref [] in

    Array.iter (fun de ->
                let name = de.dr_name in
                let err,cap = dir_lookup ~root:dir ~name:name in
                if (err = std_OK) then
                begin
                    Db.Pr.ss 10 "load_afs_dir: " name;
                    doing ("std_info: "^name);
                    
                    let old = Rpc.timeout 1000 in
                    let err,info = std_info ~cap:cap ~bufsize:40 in
                    __(Rpc.timeout old);
                    thread_switch();
                    if (err = std_OK) then
                    begin
                        if (info.[0] = '/' || info.[0] = '%') then
                             subdirs := !subdirs @ [name,err]
                        else
                            filenames := !filenames @ [name,err];
                    end
                    else
                        filenames := !filenames @ [name,err];
                end
                else
                    filenames := !filenames @ [name,err];
            ) dira;
    Db.Pr.sdd 10 "load_afs_dir: subdirs filenames" (List.length !subdirs)
                                                  (List.length !filenames);

    doing "Ready.";

    (Sort.list (fun a b ->
                    let na,_ = a in
                    let nb,_ = b in
                    na <= nb) (!subdirs),
     Sort.list (fun a b ->
                   let na,_ = a in
                   let nb,_ = b in
                   na <= nb) (!filenames))
    
  end
  with | AmError stat -> doing "failed"; ([],[])


(*
** Copy a file from AFS to UNIX with flags specified.
*)

let copy_fromafs ~src ~dst =
    doing ("std_info "^src);
    let err,fcap  = dir_lookup ~root:(!dns_root)
                               ~name:src
                    in
    if (err = std_OK) then
    begin
        let err,info = std_info ~cap:fcap ~bufsize:100 
            in

        if (err = std_OK && info.[0] = '-') then
        begin
            if !verbose then print ("Copying file "^src^" to "^dst^"\n");

            let stat,fsize = afs_size fcap in

            if (stat <> std_OK) then raise (AmError stat);
            (*  
            ** Open unix file for writing
            *)
            let fd = Unix.openfile dst
                                   ([O_RDWR;O_CREAT]@
                                        (if copy_flags.copy_overwrite then
                                            []
                                         else 
                                            [O_EXCL]))
                                    (256+128) in
            
            let buf_size = if fsize > 10000 then 10000 else fsize in
            let buf = buf_create buf_size in
            
            let frags = fsize/buf_size in
            let off = ref 0 in
            let size = ref (if (frags = 0) then
                                 fsize
                             else
                                 buf_size) 
                in

            for i = 0 to frags 
            do
                (*
                ** read from afs server
                *)
                let stat,n = afs_read ~cap:fcap
                                      ~offset:!off
                                      ~buf:buf
                                      ~size:!size
                in
                if (stat <> std_OK) then
                begin
                    print "afs_read of src failed\n"; 
                    raise (AmError stat);
                end;

                (*
                ** and write to unix file
                *)

                let n = Unix.writeb fd buf 0 !size in
                
                if (n <> !size) then
                    raise (AmError std_IOERR);
                off := !off + !size;
                size := min buf_size (fsize - !off);
            done;
            Unix.close fd;
            
            
            doing "Ready.";
            std_OK
        end
        else
        begin
            print ("not a file or server down: "^src^"\n");
            doing "Ready.";
            std_NOTFOUND;
        end;            
    end
    else
    begin
        print ("dir_lookup "^src^" failed!\n");
        doing "Ready.";
        std_NOTFOUND;
    end


let copy_fromunix ~src ~dst =
    (*
    ** Lookup the dns parent object 
    *)
    let parent = Filename.dirname dst in
    let dstname = Filename.basename dst in
    let srcname = Filename.basename src in

    init_file_cap parent;

    let stat,dir = name_lookup parent in
    if (stat <> std_OK) then
    begin
        print "name_lookup of dst failed\n"; 
        raise (AmError stat);
    end;
    (*
    ** determine unix file size 
    *)
    let fstat = Unix.stat src in
    let size = fstat.st_size in

    if !verbose then print ("Copying file "^src^" to "^dst^"\n");

    (*
    ** Open unix file for reading 
    *)
    let fd = Unix.openfile src
                  [O_RDONLY]
                  (256+128) in
            

    if (copy_flags.copy_overwrite = false) then
    begin
        let stat,cap = dir_lookup ~root:dir
                                  ~name:dstname
            in
        if (stat = std_OK) then
            raise (AmError std_EXISTS);
    end;

    let buf_size = if size > 10000 then 10000 else (max size 1) in
    let buf = buf_create buf_size in
    (*
    ** Create the afs file 
    *)

    let stat,cap2 = afs_create ~cap:!file_cap
                               ~buf:nilbuf
                               ~size:0
                               ~commit:0
        in
    if (stat <> std_OK) then
    begin
        print "afs_create of dst failed\n"; 
        raise (AmError stat);
    end;

    let frags = size/buf_size in
    let off = ref 0 in
    let fsize = ref (if (frags = 0) then
                        size
                     else
                        buf_size) 
        in

    for i = 0 to frags 
    do
        (*
        ** read from unix file
        *)
        let n = Unix.readb fd buf 0 !fsize in
        if (n <> !fsize) then
            raise (AmError std_IOERR);

        (*
        ** modify afs file
        *)

        let stat,_ = afs_modify ~cap:cap2 
                                ~offset:!off
                                ~buf:buf
                                ~size:!fsize
                                ~commit:0
            in
        if (stat <> std_OK) then
        begin
            print "afs_modify of dst failed\n"; 
            raise (AmError stat);
        end;

        off := !off + !fsize;
        fsize := min buf_size (size - !off);
    done;
    Unix.close fd;
    let stat,_ =    afs_modify ~cap:cap2
                               ~offset:0
                               ~buf:nilbuf
                               ~size:0
                               ~commit:afs_COMMIT
        in
    if (stat <> std_OK) then
    begin
        print "afs_modify of dst failed\n"; 
        raise (AmError stat);
    end;
    if (copy_flags.copy_overwrite = true) then
    begin
        let stat,cap = dir_lookup ~root:dir
                                  ~name:dstname
            in
        if (stat = std_OK) then
        begin
            let stat = dir_delete ~root:dir
                                  ~name:dstname
                in
            if (stat <> std_OK) then
            begin
                print "dir_delete failed.\n"; 
                raise (AmError stat);
            end;
            if (copy_flags.copy_destroy = true) then
                ignore(std_destroy cap);
        end;
    end;
    (*
    ** Append the new file cap to the directory
    *)

    let stat = dir_append ~root:dir
                          ~name:dstname
                          ~obj:cap2
        in
    if (stat <> std_OK) then
    begin
        print "dir_append of dst failed\n"; 
        ignore(std_destroy cap2);
        raise (AmError stat);
    end;
    std_OK


(*
** Copy selected files and directories to selected Unix directory.
*)

let copy_from_afs () =
    doing "Copy from AFS...";
    if (!sel_dirs_unix = [] && !sel_files_unix = [] ||
        (List.length !sel_dirs_unix) > 1 ||
        (List.length !sel_files_unix) > 1 
       ) then
    begin
        doing "One target directory or file must be selected!";
    end
    else
    begin
        let rec copy_dir src dst = 
            ( try 
                ignore(Unix.stat dst)
            with
            | _ ->
            begin
                (*
                ** destination directory doesn't exist. create it
                *)
                if !verbose then print ("Creating dir "^dst^"\n"); 
                Unix.mkdir dst (256+128+64);
            end );

            let subdirs,files = load_afs_dir src in
            (*
            ** Copy files
            *)
            List.iter (fun fs ->
                    let fname,stat = fs in
                    let dpath = dst^"/"^fname in
                    let spath = src^"/"^fname in
                    let stat = copy_fromafs 
                                         ~src:spath
                                         ~dst:dpath
                                         in
                    if (stat <> std_OK) then
                    begin
                        print ("copy_fromafs failed: "^src^"->"^dst^"\n");
                        raise (AmError stat);                        
                    end;
                ) files;
            (*
            ** and the subdirectories
            *)
            List.iter (fun ds ->
                    let dname,stat = ds in
                    let dpath = dst^"/"^dname in
                    let spath = src^"/"^dname in
                    if (stat = std_OK) then
                        copy_dir spath dpath;                    
                ) subdirs;
            in
         
        let doit () =
          try
          begin
            (*
            ** Copy files first.
            *)

            begin
                match !sel_dirs_unix with
                | td::tl -> 
                begin
                
                  List.iter ( fun fd ->
                    let spath = if fd.file_path = "/" 
                                then 
                                    "/"^fd.file_name 
                                else
                                    fd.file_path^"/"^fd.file_name 
                        in
                    let tpath = if td.dir_path = "/" 
                                then 
                                    "/"^td.dir_name^"/"^fd.file_name 
                                else
                                    td.dir_path^"/"^td.dir_name^"/"^fd.file_name 
                        in
                    let stat = copy_fromafs 
                                         ~src:spath
                                         ~dst:tpath
                        in
                    if (stat <> std_OK) then
                        raise (AmError stat);
                    ) !sel_objs_afs;
                  td.dir_label#configure [Foreground "yellow"];
                  td.dir_label#refresh;
                end
                | [] -> 
                begin
                  let td = List.nth !sel_files_unix 0 in
                  let fd = List.nth !sel_objs_afs 0 in
                  let spath = if fd.file_path = "/" 
                                then 
                                    "/"^fd.file_name 
                                else
                                    fd.file_path^"/"^fd.file_name 
                        in
                  let tpath = if td.file_path = "/" 
                                then 
                                    "/"^td.file_name 
                                else
                                    td.file_path^"/"^td.file_name 
                        in
                  let stat = copy_fromafs 
                                         ~src:spath
                                         ~dst:tpath
                        in
                  if (stat <> std_OK) then
                      raise (AmError stat);
              end;
            end;

            if (!sel_dirs_unix <> []) then
            begin
                (*
                ** Now the unix directories with
                ** subdirectories. Missing subdirectories must be created!
                *)
                let td = List.nth !sel_dirs_unix 0 in

                List.iter ( fun dd ->
                    let srcroot = if dd.dir_path = "/" then
                                "/"^dd.dir_name 
                              else
                                dd.dir_path^"/"^dd.dir_name
                    in
                    let dstroot = if td.dir_path = "/" then
                                "/"^td.dir_name 
                              else
                                td.dir_path^"/"^td.dir_name
                    in
                    copy_dir srcroot dstroot;
                ) !sel_dirs_afs;
            end;
            doing "Ready.";
            unsel_all();
          end
          with 
            | AmError stat -> unsel_all();
                            print ("copy_from_afs failed: "^
                                    (err_why stat)^"\n");
                            doing "Ready.";
            | Unix_error (e,s1,s2) -> 
                     unsel_all ();
                     print ("copy_from_afs failed: "^(error_message e)^"\n");
            | _ -> unsel_all ();
                   print "copy_from_afs failed!\n"; 
                   doing "Ready."

        in

    service_fun := doit;
    sema_up service_sema;
    end
   
(*
** Copy files and directories from UNIX to AFS
*)

let copy_from_unix () =
    doing "Copy from Unix...";
    if ((!sel_dirs_afs = [] && !sel_objs_afs = []) ||
        (List.length !sel_dirs_afs) > 1 ||
        (List.length !sel_objs_afs) > 1
       ) then
    begin
        doing "One target directory or one target object must be selected!";
    end
    else if (!sel_dirs_unix = [] && !sel_files_unix = []) then
    begin
        doing "No sources selected!";
    end
    else
    begin
        let rec copy_dir src dst = 
            let stat,_ = dir_lookup !dns_root dst in
            if (stat <> std_OK) then
            begin
                (*
                ** destination directory doesn't exist. create it
                *)
                if !verbose then print ("Creating dir "^dst^"\n"); 

                let stat,parent = dir_lookup ~root:!dns_root
                                             ~name:(
                                                Filename.dirname dst
                                              )
                    in
                if (stat <> std_OK) then
                begin
                    print ("dir_lookup failed: "^(Filename.dirname dst)^"\n"); 
                    raise (AmError stat);
                end;

                let stat,newdir = dir_create ~server:parent in
                if (stat <> std_OK) then
                begin
                    print ("dir_create failed: "^dst^"\n"); 
                    raise (AmError stat);
                end;

                let stat = dir_append ~root:parent
                                      ~name:(Filename.basename dst)
                                      ~obj:newdir
                    in
                if (stat <> std_OK) then
                begin
                    print ("dir_append failed: "^(Filename.basename dst)^"\n");                     
                    raise (AmError stat);
                end;
            end;

            let subdirs,files = load_unix_dir src in
            (*
            ** Copy files
            *)
            List.iter (fun fs ->
                    let fname,stat = fs in
                    let dpath = dst^"/"^fname in
                    let spath = src^"/"^fname in
                    let stat = copy_fromunix 
                                         ~src:spath
                                         ~dst:dpath
                                         in
                    if (stat <> std_OK) then
                    begin
                        print ("copy_fromunix failed: "^src^"->"^dst^"\n");
                        raise (AmError stat);                        
                    end;
                ) files;
            (*
            ** and the subdirectories
            *)
            List.iter (fun ds ->
                    let dname,stat = ds in
                    let dpath = dst^"/"^dname in
                    let spath = src^"/"^dname in
                    if (stat = std_OK) then
                        copy_dir spath dpath;                    
                ) subdirs;
            in
         
        let doit () =
          try
          begin
            (*
            ** Copy files first.
            *)
            begin
                match !sel_dirs_afs with
                | td::tl -> 
                begin
                  List.iter ( fun fd ->
                    let spath = if fd.file_path = "/" 
                                then 
                                    "/"^fd.file_name 
                                else
                                    fd.file_path^"/"^fd.file_name 
                        in
                    let tpath = if td.dir_path = "/" 
                                then 
                                    "/"^td.dir_name^"/"^fd.file_name 
                                else
                                    td.dir_path^"/"^td.dir_name^"/"^fd.file_name 
                        in
                    let stat = copy_fromunix 
                                         ~src:spath
                                         ~dst:tpath
                        in
                    if (stat <> std_OK) then
                        raise (AmError stat);
                  ) !sel_files_unix;
                  td.dir_label#configure [Foreground "yellow"];
                  td.dir_label#refresh;
                end
                | [] ->
                begin
                    let td = List.nth !sel_objs_afs 0 in
                    let fd = List.nth !sel_files_unix 0 in
                    let spath = if fd.file_path = "/" 
                                then 
                                    "/"^fd.file_name 
                                else
                                    fd.file_path^"/"^fd.file_name 
                        in
                    let tpath = if td.file_path = "/" 
                                then 
                                    "/"^td.file_name 
                                else
                                    td.file_path^"/"^td.file_name 
                        in
                    let stat = copy_fromunix 
                                         ~src:spath
                                         ~dst:tpath
                        in
                    if (stat <> std_OK) then
                        raise (AmError stat);
                end
            end;
            if (!sel_dirs_afs <> []) then
            begin
                (*
                ** Now the unix directories with
                ** subdirectories. Missing subdirectories must be created!
                *)
                let td = List.nth !sel_dirs_afs 0 in
                List.iter ( fun dd ->
                    let srcroot = if dd.dir_path = "/" then
                                "/"^dd.dir_name 
                              else
                                dd.dir_path^"/"^dd.dir_name
                    in
                    let dstroot' = if td.dir_path = "/" then
                                "/"^td.dir_name 
                              else
                                td.dir_path^"/"^td.dir_name
                    in
                    let dstroot = if (Filename.basename srcroot) <> ""
                                  then dstroot'^"/"^
                                        (Filename.basename srcroot)
                                  else
                                       dstroot'
                    in 
                    copy_dir srcroot dstroot;
                ) !sel_dirs_unix;
            end;
            unsel_all ();
            doing "Ready.";
          end
          with 
            | AmError stat -> unsel_all();
                            print ("copy_from_unix failed: "^
                                    (err_why stat)^"\n");
                            doing "Ready.";
            | Unix_error (e,s1,s2) -> 
                     unsel_all ();
                     print ("copy_from_unix failed: "^(error_message e)^"\n");
                     doing "Ready.";
            | _ -> unsel_all();
                   print "copy_from_unix failed!\n";
                   doing "Ready."

        in

        service_fun := doit;
        sema_up service_sema;
    end

let show_info () =
    let doit () =
      let msg = ref "" in
      if (List.length !sel_dirs_afs > 0) then
      begin
        List.iter ( fun d ->
                    let spath = if d.dir_path = "/"  
                                then 
                                    "/"^d.dir_name 
                                else
                                    d.dir_path^"/"^d.dir_name 
                    in
                    let err,dircap = dir_lookup ~root:(!dns_root) 
                                                ~name:spath 
                        in
                    if (err = std_OK) then
                    begin
                        doing ("std_info: "^spath);
                        let err,info = std_info ~cap:dircap ~bufsize:100 in

                        if (err = std_OK) then
                        begin
                            msg := !msg ^ 
                            (sprintf "std_info %30s -> %30s\n" 
                                     spath info);
                            d.dir_label#configure [Foreground "black"];
                            d.dir_label#refresh;
                        end
                        else
                        begin
                            msg := !msg ^ 
                            (sprintf "std_info %30s -> %30s\n" 
                                     spath (err_why err));
                            d.dir_label#configure [Foreground "blue"];
                            d.dir_label#refresh;
                        end;
                    end
                    else
                    begin
                            msg := !msg ^ 
                            (sprintf "std_info %30s -> %30s\n" 
                                     spath (err_why err));
                            d.dir_label#configure [Foreground "blue"];
                            d.dir_label#refresh;
                    end;
               ) !sel_dirs_afs;
      end;
      if (List.length !sel_objs_afs > 0) then
      begin
        List.iter ( fun o ->
                    let spath = if o.file_path = "/"  
                                then 
                                    "/"^o.file_name 
                                else
                                    o.file_path^"/"^o.file_name 
                    in
                    
                    let err,dircap = dir_lookup ~root:(!dns_root) 
                                                ~name:spath 
                        in
                    if (err = std_OK) then
                    begin
                        doing ("std_info: "^spath);
                        let err,info = std_info ~cap:dircap ~bufsize:100 in

                        if (err = std_OK) then
                        begin
                            msg := !msg ^ (sprintf 
                                        "std_info %30s -> %30s\n" 
                                        spath info);
                            o.file_label#configure [Foreground "black"];
                            o.file_label#refresh;
                        end
                        else
                        begin
                            msg := !msg ^ (sprintf 
                                        "std_info %30s -> %30s\n" 
                                        spath (err_why err));
                            o.file_label#configure [Foreground "blue"];
                            o.file_label#refresh;
                        end;
                    end
                    else
                    begin
                        msg := !msg ^ (sprintf "std_info %30s -> %30s\n" 
                                        spath (err_why err));
                        o.file_label#configure [Foreground "blue"];
                        o.file_label#refresh;
                    end;

               ) !sel_objs_afs;
      end;
      doing "Ready.";
      print !msg
    in
    service_fun := doit;
    sema_up service_sema

let show_status () =
    let doit () =
      let msg = ref "" in
      if (List.length !sel_dirs_afs > 0) then
      begin
        List.iter ( fun d ->
                    let spath = if d.dir_path = "/"  
                                then 
                                    "/"^d.dir_name 
                                else
                                    d.dir_path^"/"^d.dir_name 
                    in
                    let err,dircap = dir_lookup ~root:(!dns_root) 
                                                ~name:spath 
                        in
                    if (err = std_OK) then
                    begin
                        doing ("std_info: "^spath);
                        let err,info = std_status ~cap:dircap ~bufsize:8000 in

                        if (err = std_OK) then
                        begin
                            msg := !msg ^ 
                            (sprintf "std_status %30s:\n%s\n\n" 
                                     spath info);
                            d.dir_label#configure [Foreground "black"];
                            d.dir_label#refresh;
                        end
                        else
                        begin
                            msg := !msg ^ 
                            (sprintf "std_status %30s:\n%s\n\n" 
                                     spath (err_why err));
                            d.dir_label#configure [Foreground "blue"];
                            d.dir_label#refresh;
                        end;
                    end
                    else
                    begin
                            msg := !msg ^ 
                            (sprintf "std_status %30s:\n%s\n\n" 
                                     spath (err_why err));
                            d.dir_label#configure [Foreground "blue"];
                            d.dir_label#refresh;
                    end;
               ) !sel_dirs_afs;
      end;
      if (List.length !sel_objs_afs > 0) then
      begin
        List.iter ( fun o ->
                    let spath = if o.file_path = "/"  
                                then 
                                    "/"^o.file_name 
                                else
                                    o.file_path^"/"^o.file_name 
                    in
                    
                    let err,dircap = dir_lookup ~root:(!dns_root) 
                                                ~name:spath 
                        in
                    if (err = std_OK) then
                    begin
                        doing ("std_info: "^spath);
                        let err,info = std_status ~cap:dircap ~bufsize:8000 in

                        if (err = std_OK) then
                        begin
                            msg := !msg ^ (sprintf 
                                        "std_status %30s:\n%s\n\n" 
                                        spath info);
                            o.file_label#configure [Foreground "black"];
                            o.file_label#refresh;
                        end
                        else
                        begin
                            msg := !msg ^ (sprintf 
                                        "std_status %30s:\n%s\n\n" 
                                        spath (err_why err));
                            o.file_label#configure [Foreground "blue"];
                            o.file_label#refresh;
                        end;
                    end
                    else
                    begin
                        msg := !msg ^ (sprintf "std_status %30s:\n%s\n\n" 
                                        spath (err_why err));
                        o.file_label#configure [Foreground "blue"];
                        o.file_label#refresh;
                    end;

               ) !sel_objs_afs;
      end;
      doing "Ready.";
      print !msg
    in
    service_fun := doit;
    sema_up service_sema



let mkdir () =
  cur_path := input_path#string;
  doing ("newdir "^(!cur_path));
  try
  begin
    if (!cur_path <> "" && !cur_path.[0] = '/') then
    begin
        let stat,parent = 
                    dir_lookup ~root:!dns_root
                               ~name:(
                                      Filename.dirname !cur_path
                                     )
            in
        if (stat <> std_OK) then
            raise (AmError stat);

        let stat,newdir = dir_create ~server:parent in
        if (stat <> std_OK) then
            raise (AmError stat);

        let stat = dir_append ~root:parent
                              ~name:(Filename.basename !cur_path)
                              ~obj:newdir
            in
        if (stat <> std_OK) then
            raise (AmError stat);
        if !verbose then print ("Created: "^(!cur_path)^"\n");                  
        if (!sel_dirs_afs <> []) then
        begin
            let dd = List.nth !sel_dirs_afs 0 in
            dd.dir_label#configure [Foreground "yellow"];
            dd.dir_label#refresh;
            unsel_all ();
        end;
    end
    else
        doing "newdir: Invalid path"
  end
  with
     | AmError stat -> unsel_all();
                       print ("newdir failed: "^(err_why stat)^"\n");
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("newdir failed: "^(error_message e)^"\n");
     | _ -> unsel_all ();
            print ("newdir failed: "^(err_why std_SYSERR)^"\n")



let rename () =
  try
  begin
    cur_sel := input_sel#string;
      if ((!sel_objs_afs <> [] &&
           !sel_dirs_afs <> []) ||
          (List.length !sel_objs_afs > 1) ||
          (List.length !sel_dirs_afs > 1)) then
    begin
        doing "rename: only one target may be selected!";
        raise (AmError std_ARGBAD);
    end;
    doing ("rename "^(!cur_sel));
    if (!cur_sel <> "" && !cur_sel.[0] = '/') then
    begin
        let oldpath = match !sel_dirs_afs with
                      | dd::_ -> 
                      begin
                        dd.dir_label#configure [Foreground "yellow"];
                        dd.dir_label#refresh;
                        if dd.dir_path = "/" 
                        then
                            "/"^dd.dir_name
                        else
                            dd.dir_path^"/"^dd.dir_name
                      end;
                      | [] -> 
                      match !sel_objs_afs with
                      | fd::_ -> 
                      begin
                            fd.file_label#configure [Foreground "yellow"];
                            fd.file_label#refresh;
                            if fd.file_path = "/"
                            then 
                                "/"^fd.file_name
                            else
                                fd.file_path^"/"^fd.file_name
                      end;      
                      | [] -> failwith "prog.error";
            in
        let newpath = !cur_sel in

        if (Filename.dirname oldpath) <> (Filename.dirname newpath) then
        begin
            doing "rename: Invalid new name";
            raise (AmError std_ARGBAD);
        end;
        let stat,parent = 
                    dir_lookup ~root:!dns_root
                               ~name:(
                                      Filename.dirname oldpath
                                     )
            in
        if (stat <> std_OK) then
            raise (AmError stat);
        let stat = dir_rename ~dir:parent
                              ~oldname:(Filename.basename oldpath)
                              ~newname:(Filename.basename newpath)
            in
        if (stat <> std_OK) then
            raise (AmError stat);
        if !verbose then print ("Renamed: "^oldpath^"->"^newpath^"\n");                  
        unsel_all ();
    end
    else
        doing "rename: Invalid path";
  end
  with
     | AmError stat -> unsel_all();
                       print ("rename failed: "^(err_why stat)^"\n");
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("rename failed: "^(error_message e)^"\n");
     | _ -> unsel_all();
            print ("rename failed: "^(err_why std_SYSERR)^"\n")


let dodelfile path =
        if !verbose then print ("Deleting file "^path^"\n");
        let stat,cap = dir_lookup ~root:!dns_root
                                  ~name:path in 
        let stat,info = std_info cap 10 in 
        if (info.[0] <> '-') then 
        begin
            print ("delfile: "^path^" not a file!\n");
            raise (AmError std_ARGBAD);
        end;
        let stat,parent = dir_lookup ~root:!dns_root
                                     ~name:(
                                      Filename.dirname path
                                     )
            in
        if (stat <> std_OK) then
            raise (AmError stat);

        if (del_flags.del_destroy = true) then
        begin
            (*
            ** Destroy object first
            *)
            let stat,cap = dir_lookup ~root:parent
                                      ~name:(
                                              Filename.basename path
                                            ) in
            if (stat <> std_OK) then
                raise (AmError stat);
            if !verbose then print ("Destroying file "^path^"\n");

            ignore(std_destroy cap);            
        end;
        let stat = dir_delete ~root:parent
                              ~name:(Filename.basename path)
            in
        if (stat <> std_OK) then
            raise (AmError stat)

let dodeldir path =
        if !verbose then print ("Deleting dir "^path^"\n");
        let stat,parent = dir_lookup ~root:!dns_root
                                     ~name:(
                                      Filename.dirname path
                                     )
            in
        if (stat <> std_OK) then
            raise (AmError stat);

        if (del_flags.del_destroy = true) then
        begin
            (*
            ** Destroy object first
            *)
            let stat,cap = dir_lookup ~root:parent
                                      ~name:(
                                              Filename.basename path
                                            ) in
            if (stat <> std_OK) then
                raise (AmError stat);

            ignore(std_destroy cap);            
        end;
        let stat = dir_delete ~root:parent
                              ~name:(Filename.basename path)
            in
        if (stat <> std_OK) then
            raise (AmError stat)


let delfile () =
  doing "Delete files...";
  try
  begin
    if (!sel_objs_afs = []) then
    begin
        doing ("No files selected!");
        raise (AmError std_ARGBAD);
    end;
    List.iter (fun fd ->
        let path =  if fd.file_path = "/" 
                   then 
                        "/"^fd.file_name 
                   else
                        fd.file_path^"/"^fd.file_name 
           in
        dodelfile path;
        fd.file_label#configure [Foreground "lightblue"];
        fd.file_label#refresh;
    ) !sel_objs_afs;
    doing "Ready.";
    unsel_all();
  end
  with
     | AmError stat -> unsel_all ();
                       print ("delfile failed: "^(err_why stat)^"\n");
                       doing "Ready.";
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("delfile failed: "^(error_message e)^"\n");
            doing "Ready."
     | _ -> unsel_all ();
            print ("delfile failed: "^(err_why std_SYSERR)^"\n");
            doing "Ready."

let deldir () =
  doing "Delete dirs...";
  try
  begin
    (*
    ** Delete recursive all subentries in the directory, and finally
    ** the directory itself.
    *)
    List.iter (fun dd ->
        let path =  if dd.dir_path = "/" 
                   then 
                        "/"^dd.dir_name 
                   else
                        dd.dir_path^"/"^dd.dir_name 
           in

        let rec enter path =
            let subdirs,files = load_afs_dir path in

            (*
            ** First delete files.
            *)
            List.iter (fun fs ->
                let fname,stat = fs in
                let fpath = path^"/"^fname in
                dodelfile fpath;
            ) files;

            (*
            ** Now enter the subdirectory tree.
            *)
            List.iter (fun ds ->
                let dname,stat = ds in
                let dpath = path^"/"^dname in
                enter dpath;
            ) subdirs;
            dodeldir path;
          in

        enter path;
        dd.dir_label#configure [Foreground "lightblue"];
        dd.dir_label#refresh;
    ) !sel_dirs_afs;
    unsel_all ();
    doing "Ready.";
  end
  with
     | AmError stat -> unsel_all ();
                       print ("deldir failed: "^(err_why stat)^"\n");
                       doing "Ready.";
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("deldir failed: "^(error_message e)^"\n");
            doing "Ready."
     | _ -> unsel_all ();
            print ("deldir failed: "^(err_why std_SYSERR)^"\n");
            doing "Ready."
    

(*
** Delete (= unlink from parent dir tree) objects no matter 
** if they are files or directories or
** what kind of object they ever be. No object destruction will
** be performed.
*)

let delobj () =
  doing "Deleting objs...";
  try
  begin
    List.iter (fun fd ->
        let path =  if fd.file_path = "/" 
                   then 
                        "/"^fd.file_name 
                   else
                        fd.file_path^"/"^fd.file_name 
           in
        let stat,parent = dir_lookup ~root:!dns_root
                                     ~name:(
                                      Filename.dirname path
                                     )
            in
        if (stat <> std_OK) then
            raise (AmError stat);

        let stat = dir_delete ~root:parent
                              ~name:(Filename.basename path)
            in
        if (stat <> std_OK) then
            raise (AmError stat);
        fd.file_label#configure [Foreground "lightblue"];
        fd.file_label#refresh;
    ) !sel_objs_afs;

    List.iter (fun dd ->
       let path =  if dd.dir_path = "/" 
                   then 
                        "/"^dd.dir_name 
                   else
                        dd.dir_path^"/"^dd.dir_name 
           in
        dodeldir path;
        dd.dir_label#configure [Foreground "lightblue"];
        dd.dir_label#refresh;
    ) !sel_dirs_afs;
    doing "Ready.";
    unsel_all();
  end
  with
     | AmError stat -> unsel_all ();
                       print ("delobj failed: "^(err_why stat)^"\n");
                       doing "Ready.";
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("delobj failed: "^(error_message e)^"\n");
            doing "Ready."
     | _ -> unsel_all ();
            print ("delobj failed: "^(err_why std_SYSERR)^"\n");
            doing "Ready."

let copy () =
  try
  begin
    if ((!sel_objs_afs <> [] &&
           !sel_dirs_afs <> []) ||
        (!sel_files_unix <> [] &&  
           !sel_dirs_unix <> []) ||
          (List.length !sel_files_unix > 1) ||
          (List.length !sel_dirs_unix > 0) ||
          (List.length !sel_objs_afs > 1) ||
          (List.length !sel_dirs_afs > 1)) then
    begin
        print "copy: only one target may be selected!\n";
        raise (AmError std_ARGBAD);
    end;
    match !sel_objs_afs with
    | fd::_ ->
    begin
        let path =  if fd.file_path = "/" 
                    then 
                        "/"^fd.file_name 
                    else
                        fd.file_path^"/"^fd.file_name 
            in
        let stat,cap = dir_lookup !dns_root path in
        if (stat <> std_OK) then
            raise (AmError stat);
        print (sprintf "Copy buffer: %s with cap %s\n" path (ar_cap cap));
        copy_buf := cap,path;
        unsel_all();
    end;
    | [] ->
        match !sel_dirs_afs with
        | dd::_ ->
        begin
            let path =  if dd.dir_path = "/" 
                   then 
                        "/"^dd.dir_name 
                   else
                        dd.dir_path^"/"^dd.dir_name 
               in
            let stat,cap = dir_lookup !dns_root path in
            if (stat <> std_OK) then
                raise (AmError stat);
            print (sprintf "Copy buffer: %s with cap %s\n" path (ar_cap cap));
            copy_buf := cap,path;
            unsel_all();
        end;
        | [] -> 
            match !sel_files_unix with
            | fd::_ ->
            begin
                let path =  if fd.file_path = "/" 
                       then 
                            "/"^fd.file_name 
                       else
                            fd.file_path^"/"^fd.file_name 
                   in
                let stat,cap = Buf.read_cap path in
                if (stat <> std_OK) then
                    raise (AmError stat);
                print (sprintf "Copy buffer: %s with cap %s\n" 
                        path (ar_cap cap));
                copy_buf := cap,path;
                unsel_all();
            end;
            | [] -> failwith "prog.error"
  end
  with
     | AmError stat -> unsel_all ();
                       print ("copy failed: "^(err_why stat)^"\n");
                       doing "Ready.";
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("copy failed: "^(error_message e)^"\n");
            doing "Ready."
     | _ -> unsel_all ();
            print ("copy failed: "^(err_why std_SYSERR)^"\n");
            doing "Ready."
let paste () =
  try
  begin
    if (!copy_buf = (nilcap,"")) then
    begin
        print "paste: copy buffer empty!\n";
        raise (AmError std_ARGBAD);
    end;
    if(
        (!sel_dirs_afs <> [] &&  
           !sel_dirs_unix <> []) ||
          (List.length !sel_files_unix > 0) ||
          (List.length !sel_dirs_unix > 1) ||
          (List.length !sel_objs_afs > 0) ||
          (List.length !sel_dirs_afs > 1)) then
    begin
        print "paste: only one target may be selected!\n";
        raise (AmError std_ARGBAD);
    end;
    match !sel_dirs_afs with
        | dd::_ ->
        begin
            let path' =  if dd.dir_path = "/" 
                   then 
                        "/"^dd.dir_name 
                   else
                        dd.dir_path^"/"^dd.dir_name 
               in
            let cap,name = !copy_buf in
            let path = path'^"/"^(Filename.basename name) in
            let stat,parent = dir_lookup !dns_root path' in
            if (stat <> std_OK) then
                raise (AmError stat);
            let stat = dir_append parent (Filename.basename name)
                                  cap 
                in
            if (stat <> std_OK) then
                raise (AmError stat);
            dd.dir_label#configure [Foreground "yellow"];            
            unsel_all();
            copy_buf := (nilcap,"");
        end;
        | [] -> 
            match !sel_dirs_unix with
            | dd::_ ->
            begin
                let path' =  if dd.dir_path = "/" 
                       then 
                            "/"^dd.dir_name 
                       else
                            dd.dir_path^"/"^dd.dir_name 
                   in
                let cap,name = !copy_buf in
                let path = path'^"/"^(Filename.basename name) in

                let stat = Buf.write_cap path cap in
                if (stat <> std_OK) then
                    raise (AmError stat);
                dd.dir_label#configure [Foreground "yellow"];            
                unsel_all();
                copy_buf := (nilcap,"");
            end;
            | [] -> failwith "prog.error"
  end
  with
     | AmError stat -> unsel_all ();
                       print ("paste failed: "^(err_why stat)^"\n");
                       doing "Ready.";
     | Unix_error (e,s1,s2) -> 
            unsel_all ();
            print ("paste failed: "^(error_message e)^"\n");
            doing "Ready."
     | _ -> unsel_all ();
            print ("paste failed: "^(err_why std_SYSERR)^"\n");
            doing "Ready."
