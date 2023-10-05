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
**    $INITIAL:     (C) BSSLAB 2003
**    $CREATED:     
**    $VERSION:     1.23
**
**    $INFO:
**
** AFS: Atomic Filesystem Service
** Device independent server implementation.
**
** Basic File Parameters:
**
**      File Sizes:     Logical, in bytes
**      Disk addresses: Physical, in blocks [super.afs_bock_size]
**      File offsets:   Logical, in bytes
**
**      Free/Used
**      Clusters:       Physical (both addr and size!), in blocks
**
**      A File always occupy full blocks.
**
**
**
**    $ENDOFINFO
**
*)





open StdLabels

open Amoeba
open Bytebuf
open Cmdreg
open Stderr
open Stdcom
open Stdcom2
open Capset
open Sys
open Rpc
open Buf
open Mutex
open Dblist
open Thread
open Syslog

module Str = StrLabels
open Str


open Afs_common

exception Deny
let debug = 1

(*
** Files have live time between MAXLIVE and 0.
** Zero live time means: destroy the file after an age request.
*)

let afs_MAXLIVE = 7



(*
** One file entry.
*)

(*
** Inode/File state
**
** Note: The FF_commit status is changed to FF_lcoked after
**       all blocks are written (synced) to disk.
*)

type afs_file_state = 
    | FF_invalid        (* Inode not used                               *)
    | FF_unlocked       (* New file created                             *)
    | FF_commit         (* File committed, but not synced /afs_COMMIT/  *)
    | FF_locked         (* File committed, and synced /afs_SAFETY/      *)


(*
** Internal Inode informations (Physical disk offset of file and
** the inode itself, and perhaps reserved space for unlocked
** files).
*)

type afs_inode = {
    mutable fi_daddr: int;  (* File disk address [blocks]               *)
    mutable fi_ioff: int;   (* File inode logical offset [bytes]        *)
    mutable fi_res: int;    (* Reserved disk space for unlocked files
                            ** [bytes]
                            *)
}

type afs_file = {
    mutable ff_lock: Mutex.t;
    mutable ff_objnum: int;                 (* The directory index number   *)
    mutable ff_random: port;                (* Random check number          *)
    mutable ff_time: int;                   (* Time stamp in 10s units      *)
    mutable ff_live: int;                   (* Live time [0..MAXLIVE]       *)
    mutable ff_state: afs_file_state;       (* The status of a file         *)
    mutable ff_size: int;                   (* Size of the file [bytes]     *)
    mutable ff_inode: afs_inode;            (* Inode from higher layers     *)
    mutable ff_modified: bool;              (* modified afs_file ?          *)
}

let nilafsfile = { ff_lock = mu_create (); ff_objnum = -1;
                   ff_random = nilport; ff_time = -1;
                   ff_live = -1; ff_state = FF_invalid;
                   ff_size = -1; 
                   ff_inode = {fi_daddr = -1; fi_ioff = -1; fi_res = 0;};
                   ff_modified = false;
}
                    

(*
** The main AFS structure: the all known super structure with basic
** informations about the file system. This structure is generated
** by the server with fixed informations from the super block
** of the filesystem (name, nfiles, ports, size), and dynamically
** from the inode table (nfiles, nused, freeobj, nextfree).
*)

type afs_super = {
    mutable afs_lock: Mutex.t;           (* Super protection lock *)
    mutable afs_name : string;           (* The server name      *)

    mutable afs_nfiles: int;             (* Number of total table entries *)
    mutable afs_nused: int;              (* Number of used table entries  *)
    mutable afs_freeobjnums: int list;   (* Free slots list               *)
    mutable afs_nextfree: int;           (* Next free slot                *)


    mutable afs_getport : port;                     (* Private getport      *)
    mutable afs_putport : port;                     (* Public putport       *)
    mutable afs_checkfield : port;                  (* Private checkfield   *)

    (*
    ** Not used by this module
    *)

    mutable afs_block_size: int;            (* Data Blocksize [bytes]       *)

    mutable afs_nblocks: int;               (* Number of total data blocks  *)

}


let nilafssuper = {
    afs_lock = mu_create ();
    afs_name="";afs_nfiles=0;afs_nused=0; afs_freeobjnums=[];
    afs_nextfree=0;afs_getport=nilport;afs_putport=nilport;
    afs_checkfield=nilport;afs_block_size=0;afs_nblocks=0
    }

(*
** The server structure. Must be filled by the main (user
** supplied) server module.
*)

type afs_server = {

    (*
    ** The super structure
    *)

    mutable afs_super : afs_super;

    (*
    ** Server function to read and write files and the super
    ** table structure. 
    ** The index number is the file number (= object number).
    ** The server is responsible for managing the file table, for
    ** example caching, reading and writing of file changes.
    ** This is not part of this module!
    *)
    
    (*
    ** Read a file specified with his objnum (index) number.
    ** Physical reads only if not cached already.
    **
    **  off: Logical File offset [bytes]
    **  size: [bytes] 
    *)
    
    mutable afs_read_file : file:afs_file ->
                            off:int ->
                            size:int ->
                            buf:buffer -> 
                            status; 

    (*
    ** Modify data of a file. In the case, the (offset+size)
    ** fragment exceeds the current filesize, the filesize
    ** must be increased with afs_modify_size first.
    **
    **  off: Logical File offset [bytes]
    **  size: [bytes] 
    *)

    mutable afs_modify_file: file:afs_file ->
                             off:int ->
                             size:int ->
                             buf:buffer -> 
                             status; 

    (*
    **  newsize: [bytes]
    *)
    mutable afs_modify_size: file:afs_file ->
                             newsize:int ->
                             status; 

    
    (*
    ** Commit a file to the disk. 
    ** The flag argument specifies the way: immediately
    ** (afs_SAFETY) or later (afs_COMMIT) by the cache module if any.
    *)
    
    mutable afs_commit_file : file:afs_file -> 
                              flag:int ->
                              status;

    (*
    ** Read an inode of a file.
    *)
    mutable afs_read_inode: obj:int ->
                            status*afs_file;


 
    (*
    ** Create a new inode with inital afs_file.
    ** A true final flag indicates that the file size
    ** is final and not initial (afs_req_create with
    ** afs_COMMIT/SAFETY flag set).
    *)
    
    mutable afs_create_inode : file:afs_file -> 
                               final:bool ->
                               status;

    (*
    ** Delete an inode (file); free used disk space, if any.
    *)
    
    mutable afs_delete_inode : file:afs_file -> 
                              status;

    (*
    ** Modify an inode, for example the ff_live field was changed.
    *)
    
    mutable afs_modify_inode : file:afs_file -> 
                              status;

    (*
    ** Read the super structure.
    *)
    
    mutable afs_read_super : unit -> afs_super * status;  


    (*
    ** Flush the caches (if any).
    *)
    
    mutable afs_sync : unit -> status;

    (*
    ** Create statistics informations.
    *)

    mutable afs_stat: obj:int -> status * string;

    (*
    ** Age a file. Return true if inode is used, and the new live time.
    *)

    mutable afs_age: obj:int -> bool*int;

    (*
    ** Touch a file.
    *)

    mutable afs_touch: file:afs_file -> unit;

    (*
    ** Something to do on exit ?
    *)
    mutable afs_exit: unit -> status;

    (*
    ** Get the current system time in 10s units
    *)
    mutable afs_time: unit -> int    
}

    
(*
** Acquire and lock a file with object number 'obj'. A
** release_file call must follow this operation.
*)

let acquire_file  ~server ~obj  =
    let super = server.afs_super in
    try 
    begin
        mu_lock super.afs_lock;
    
        if (obj >= super.afs_nfiles) then
            raise (Error std_ARGBAD);

        let stat,file = server.afs_read_inode ~obj:obj in

        if (stat <> std_OK) then
        begin
            sys_log Sys_warn
                    "AFS: Warning: acquire_file: read_inode failed for inode %d : %s\n"
                    obj (err_why stat);
            raise (Error stat);
        end;
        if (file.ff_state = FF_invalid) then
            raise (Error std_NOTFOUND);

        mu_lock file.ff_lock;
        mu_unlock super.afs_lock;
        (std_OK,file)
    end
    with
        | Error err -> mu_unlock super.afs_lock; 
                       err,nilafsfile

(*
** Release an acquired file. 
** Commit the file with specified flag argument - 
** the way to commit the file. A zero value shows no
** modifications.
*)

let release_file ~server ~file ~flag =
    let super = server.afs_super in

    try
    begin
        if (file = nilafsfile) then
            raise (Error std_SYSERR);


        if (flag land (afs_COMMIT lor afs_SAFETY) > 0) then
        begin
            file.ff_state <- if (flag land afs_SAFETY = afs_SAFETY) then
                                FF_locked
                             else
                                FF_commit;
            file.ff_modified <- true;
            (*
            ** Commit the file.
            *)
            let stat = server.afs_commit_file ~file:file ~flag:flag in
            
            if (stat <> std_OK) then 
            begin
                sys_log Sys_err "AFS: release_file: commit_file failed: %s\n"
                                (err_why stat);
                mu_unlock file.ff_lock;
                raise (Error stat);
            end;
        end;

        let stat = 
            if (file.ff_modified = true) then
                server.afs_modify_inode ~file:file 
            else
                std_OK
            in
    
        mu_unlock file.ff_lock;
        stat
    end
    with
        | Error err -> err


    
(*
** Return a free objnum in the AFS table (index). 
** Note: protect this function with the server lock untill
** the file creation is finished.
*)

let get_freeobjnum super =
    (*
    ** First check the free objnums list. If empty, use nextfree at the
    ** end of the directory table.
    *)
    super.afs_nused <- super.afs_nused + 1;
        
    match super.afs_freeobjnums with
        | hd::tl -> super.afs_freeobjnums <- tl;
                    hd;
        | [] -> 
            let objnum = super.afs_nextfree in

            if (objnum + 1 = super.afs_nfiles) then
                failwith "AFS: TODO: out of filetable slots";

            super.afs_nextfree <- objnum + 1;
            objnum

(*
** AFS server size request.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**
** Return:
**  status
**  size [bytes]
*)

let afs_req_size
        ~server
        ~priv
    =
   
    let super = server.afs_super in
    try
    begin

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in
        let stat,af = acquire_file ~server:server ~obj:obj in 

            
        if (stat <> std_OK) then
            raise (Error stat);

        if ( (prv_decode ~prv:priv ~rand:af.ff_random)  = false) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED)
        end;
        let size = af.ff_size in
        ignore(release_file ~server:server ~file:af ~flag:0);
        std_OK,size
    end
    with
        | Error err -> (err,-1)



(*
** AFS server create request.
** Create a new file.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**  buf:    the write buffer
**  size:   the inital  file size [bytes] (can be 0: only object creation)
**  commit: the commit flag
**
** Return:
**  status
**  newcap
**
*)

let afs_req_create 
        ~server
        ~priv
        ~buf
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the 'parent' directory if any.
        *)

        let Objnum obj' = prv_number priv in

#ifdef DEBUG
        Db.Pr.sddd 2 "afs_server:afs_req_create [obj,size,commit]"
                 obj' size commit;
#endif

        if (obj' > 1) then
        begin
            (*
            ** It's a file.
            *)
            let stat,af = acquire_file ~server:server ~obj:obj' in 

            if (stat <> std_OK) then
                raise (Error stat);

            if ((prv_decode ~prv:priv
                            ~rand:af.ff_random) = false ||
                (rights_req rights [afs_RGT_CREATE]) = false ) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error std_DENIED);
            end;
            ignore(release_file ~server:server ~file:af ~flag:0);
        end
        else
        begin
            
            if ((prv_decode ~prv:priv 
                            ~rand:super.afs_checkfield) = false  ||
                (rights_req rights [afs_RGT_CREATE]) = false) then
            begin
                raise (Error std_DENIED);
            end; 
        end;

        (*
        ** First get a new inode==object number
        *)
        let obj = get_freeobjnum super in

        let af = {
            ff_lock = mu_create ();
            ff_objnum = obj;
            ff_random = uniqport ();
            ff_time = server.afs_time ();                    
            ff_live = afs_MAXLIVE;
            ff_state = FF_unlocked;
            ff_size = size;                 (* Initial size ! *)
            ff_inode = { fi_daddr = -1; fi_ioff = -1; fi_res = 0; };
            ff_modified = true;
        } in
        
        mu_lock af.ff_lock;

        let final = if commit > 0 then true else false in
        let stat = server.afs_create_inode ~file:af ~final:final in

        if (stat <> std_OK) then
        begin
            mu_unlock af.ff_lock;
            raise (Error stat);
        end;

        let cap = { cap_port = super.afs_putport;
                    cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
        } in
        
        server.afs_touch af;

        let stat =
            if (size > 0) then
            begin 
                server.afs_modify_file ~file:af
                               ~off:0
                               ~size:size
                               ~buf:buf
            end
            else
                std_OK
            in                     

        if (stat <> std_OK) then
        begin
            mu_unlock af.ff_lock;
            raise (Error stat);
        end;

        let stat = release_file ~server:server ~file:af ~flag:commit;
            in

        if (stat <> std_OK) then
        begin
            mu_unlock af.ff_lock;
            raise (Error stat);
        end;

        (std_OK,cap)
    end
    with
        | Error err -> (err,nilcap)


(*
** AFS server read request.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**  buf:    the read buffer
**  size:   the requested size (<= file size)   [bytes]  
**  off:    the file offset (<= file size)  [bytes]
**
** Return:
**  status
*)

let afs_req_read 
        ~server
        ~priv
        ~buf
        ~off
        ~size
    =
    try
    begin

        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sddd 2 "afs_server:afs_req_read [obj,off,size]"
                 obj off size;
#endif

        let stat,af = acquire_file ~server:server ~obj:obj in 

        if (stat <> std_OK) then
            raise (Error stat);

        if (off > af.ff_size) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_ARGBAD);
        end;

        (*
        ** Huuh, some clients may be request more bytes than
        ** the file has. Fix it here.
        *)
        let size =
            if (size+off > af.ff_size) then (af.ff_size-off) else size in

            
        if   ((prv_decode ~prv:priv ~rand:af.ff_random) = false ||
              (rights_req rights [afs_RGT_READ])= false) then
        begin    
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED);                                        
        end;
        let stat =
                server.afs_read_file ~file:af
                               ~off:off
                               ~size:size
                               ~buf:buf
            in
        if (stat <> std_OK) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error stat);                            
        end;
        ignore(release_file ~server:server ~file:af ~flag:0);
        std_OK,size;
    end
    with
        | Error err -> err,0


(*
**
**  Distinguish this two cases:
**
**  1. The file state = FF_unlocked -> Modification is uncritical.
**
**  2. The file state = FF_locked|FF_commit ->
**
**      I. Create a new file.
**      II. Copy the original content and do the modifications in the
**          newly created file.
**
*)

(*
** AFS server Modify request.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**  buf:    the write buffer
**  size:   the requested size (<= file size)   [bytes]
**  off:    the file offset (<= file size)  [bytes]
**  commit: the commit flag
**
** Return:
**  status
**  newcap      (FF_unlocked -> oldcap)
*)

let afs_req_modify
        ~server
        ~priv
        ~buf
        ~off
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sdddd 2 "afs_server:afs_req_modify [obj,off,size,commit]"
                 obj off size commit;
#endif


        let stat,af = acquire_file ~server:server ~obj:obj in 
        if (stat <> std_OK) then
            raise (Error stat);

        if (size < 0 ||
            off < 0) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_ARGBAD);                            
        end;

        (*
        ** Four different cases:
        **
        **  1. Request authorized/not authorized
        **  2. stat = FF_locked/FF_commit, FF_unlocked
        **  3. Invalid arguments (off,size)
        **  4. size = 0 (only commit)
        **
        *)

        if ((prv_decode ~prv:priv ~rand:af.ff_random) = false ||
            (rights_req rights [afs_RGT_MODIFY]) = false) then
        begin    
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED);                            
        end;

        if (size = 0 && af.ff_state = FF_unlocked) then
        begin
            (*
            ** Only commit the file.
            *)
            let stat = release_file ~server:server ~file:af 
                                 ~flag:commit
            in
            if (stat <> std_OK) then
            begin
                raise (Error stat);
            end;

            let cap = { cap_port = super.afs_putport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
                } in

            std_OK,cap
        end         
        else if (size = 0 && (af.ff_state = FF_locked ||
                              af.ff_state = FF_commit)) then
        begin
            (*
            ** Only unlocked files can be committed!
            ** Or must we create a copy here?
            *)
            ignore(release_file ~server:server ~file:af ~flag:0);
            std_ARGBAD,nilcap
        end 
        else 
        begin
#ifdef DEBUG
            Db.Pr.s 5 "afs_server:afs_req_modify: auth";
#endif
            (*
            ** The file exists, and the request is authorized.
            *)

            if (af.ff_state = FF_unlocked) then
            begin

                (*
                ** New data beyond the current file size ?
                *)

                let stat =
                    if (af.ff_size < (off+size)) then
                        server.afs_modify_size ~file:af
                                               ~newsize:(off+size)
                    else
                        std_OK
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;
              
                let stat =
                    server.afs_modify_file ~file:af
                               ~off:off
                               ~size:size
                               ~buf:buf
                                
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;

                let stat = release_file ~server:server 
                                        ~file:af ~flag:commit
                    in

                if (stat <> std_OK) then
                begin
                    raise (Error stat);
                end;

                let cap = { cap_port = super.afs_putport;
                            cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
                } in

                std_OK,cap
            end
            else
            begin
                (*
                ** Create a new file. Copy the content of the
                ** original one. Modify the new one.
                *)
#ifdef DEBUG
                Db.Pr.s 2 "afs_server: afs_req_modify: copy";
#endif

                (*
                ** First get a new inode==object number
                *)
                let obj' = get_freeobjnum super in

                let af' = {
                    ff_lock = mu_create ();
                    ff_objnum = obj';
                    ff_random = uniqport ();
                    ff_time = server.afs_time();                   
                    ff_live = afs_MAXLIVE;
                    ff_state = FF_unlocked;
                    ff_size = if (af.ff_size < off+size) then
                                (off+size)
                              else
                                af.ff_size;
                    ff_inode = { fi_daddr = -1; fi_ioff = -1; fi_res = 0; };
                    ff_modified = true;
                } in
        
                mu_lock af'.ff_lock;

                let stat = server.afs_create_inode ~file:af' 
                                                   ~final:false
                    in
#ifdef DEBUG
                Db.Pr.ss 2  "afs_server: afs_req_modify: create_inode="
                            (err_why stat);
#endif

                if (stat <> std_OK) then
                begin       
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;

                let cap' = { cap_port = super.afs_putport;
                             cap_priv = prv_encode ~obj:(Objnum obj')
                                          ~rights:prv_all_rights
                                          ~rand:af'.ff_random; 
                } in

                (*
                ** Copy the original data
                *)

                let buf' = buf_create 1000 in
                let cs = ref (min af.ff_size 1000) in
                let co = ref 0 in
                
                while (!cs > 0) 
                do
                    let stat = server.afs_read_file ~file:af
                                        ~off:!co
                                        ~size:!cs
                                        ~buf:buf'
                    in
#ifdef DEBUG
                    Db.Pr.ss 2  "afs_server: afs_req_modify: read_="
                                (err_why stat);
#endif
    
                    if (stat <> std_OK) then
                    begin
                        ignore(release_file ~server:server ~file:af ~flag:0);
                        mu_unlock af'.ff_lock;
                        raise (Error stat);
                    end;

                    let stat =
                            server.afs_modify_file ~file:af'
                                        ~off:!co
                                        ~size:!cs
                                        ~buf:buf'
                        in
#ifdef DEBUG
                    Db.Pr.ss 2  "afs_server: afs_req_modify: write_="
                                (err_why stat);
#endif
    
                    if (stat <> std_OK) then
                    begin
                        ignore(release_file ~server:server ~file:af ~flag:0);
                        mu_unlock af'.ff_lock;
                        raise (Error stat);
                    end;
                    
                    co := min (!co + !cs) af.ff_size;
                    cs := min (af.ff_size - !co) 1000;
                done;

                let stat =
                        server.afs_modify_file ~file:af'
                               ~off:off
                               ~size:size
                               ~buf:buf
                    in                     
#ifdef DEBUG
                    Db.Pr.ss 2  "afs_server: afs_req_modify: mod_="
                                (err_why stat);
#endif

    
                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;
                              
                ignore(release_file ~server:server ~file:af ~flag:0);

                let stat = release_file ~server:server 
                                        ~file:af' ~flag:commit
                    in

                if (stat <> std_OK) then
                begin
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;

                std_OK,cap'
            end;
        end
    end
    with
        | Error err -> err,nilcap


(*
** AFS server Insert request.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**  buf:    the write buffer
**  size:   the requested size (<= file size)   [bytes]
**  off:    the file offset (<= file size)  [bytes]
**  commit: the commit flag
**
** Return:
**  status
**  newcap      (FF_unlocked -> oldcap)
*)

let afs_req_insert
        ~server
        ~priv
        ~buf
        ~off
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sdddd 2 "afs_server:afs_req_insert [obj,off,size,commit]"
                 obj off size commit;
#endif


        let stat,af = acquire_file ~server:server ~obj:obj in 


        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
        
        if (size > af.ff_size ||
            off >= af.ff_size) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_ARGBAD);
        end;

        (*
        ** There are four different cases:
        **
        **  1. Request authorized/not authorized
        **  2. stat = FF_locked/FF_commit, FF_unlocked
        **  3. Invalid arguments (off,size)
        **  4. size = 0 (only commit)
        **
        *)
            
        if ((prv_decode ~prv:priv ~rand:af.ff_random) = false ||
            (rights_req rights [afs_RGT_MODIFY]) = false) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED);
        end;

        if (size = 0 && af.ff_state = FF_unlocked) then
        begin
            (*
            ** Only commit the file.
            *)
            let stat = release_file ~server:server ~file:af 
                                 ~flag:commit
            in
            if (stat <> std_OK) then
                raise (Error stat);
 
            let cap = { cap_port = super.afs_putport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
                } in

            std_OK,cap
        end         
        else if (size = 0 && (af.ff_state = FF_locked ||
                              af.ff_state = FF_commit)) then
        begin
            (*
            ** Only unlocked files can be committed!
            ** Or must we create a copy here?
            *)
            ignore(release_file ~server:server ~file:af ~flag:0);
            std_ARGBAD,nilcap
        end
        else 
        begin
          (*
          ** The file exists, and the request is authorized.
          *)
        
          if (af.ff_state = FF_unlocked) then
          begin
            (*
            ** New data beyond the current file size ?
            *)
            let stat = 
                if (af.ff_size < (off+size)) then
                    server.afs_modify_size ~file:af
                                           ~newsize:(off+size)
                else
                    std_OK
            in
            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;

            let cap = { cap_port = super.afs_putport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
                } in

            if (off < af.ff_size) then
            begin

                (*
                ** The data should be inserted in the middle
                ** or at the beginning of the file. Therefore, we must
                ** shift the data between this offset (off) and 
                ** (off+size) to create the needed space.
                *)

                let buf' = buf_create 1000 in

                let size' = ref (min 1000 size) in
                let off'  = ref (af.ff_size - !size') in
            
                (*
                ** Start at the current end of the file position and
                ** work down to the specified insert offset (off).
                *)

                while ( !off' >= (off+size)) 
                do
                            
                    (*
                    ** First the chunk to read
                    *)
                
                    let stat =
                        server.afs_read_file ~file:af
                               ~off:!off'
                               ~size:!size'
                               ~buf:buf'
                                
                        in
                    if (stat <> std_OK) then
                    begin
                        ignore(release_file ~server:server ~file:af ~flag:0);
                        raise (Error stat);
                    end;

                    (*
                    ** Now write the buffer to the shifted
                    ** position.
                    *)

                    let stat =
                            server.afs_modify_file ~file:af
                               ~off:(!off'+size)
                               ~size:!size'
                               ~buf:buf'
                                            
                        in
                    if (stat <> std_OK) then
                    begin
                        ignore(release_file ~server:server ~file:af ~flag:0);
                        raise (Error stat);
                    end;

                    size' := min (!off' - off) 1000;
                    off' := max (!off' - !size') off;
                done;
            end;
            
            (*
            ** Now write the data to be inserted.
            *)

            let stat =
                    server.afs_modify_file ~file:af
                               ~off:off
                               ~size:size
                               ~buf:buf
                                
                in

            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;

            let stat = release_file ~server:server ~file:af ~flag:commit
                in
            if (stat <> std_OK) then
            begin
                raise (Error stat);
            end;

            std_OK,cap
          end
          else
          begin
            (*
            ** Create a new file, copy the content of the
            ** original one, but shift the part from (off) 
            ** to (off+size).
            *)

            (*
            ** First get a new inode==object number
            *)
            let obj' = get_freeobjnum super in

            let af' = {
                    ff_lock = mu_create ();
                    ff_objnum = obj';
                    ff_random = uniqport ();
                    ff_time = server.afs_time ();                   
                    ff_live = afs_MAXLIVE;
                    ff_state = FF_unlocked;
                    ff_size = if (af.ff_size < off+size) then
                                (off+size)
                              else
                                af.ff_size;
                    ff_inode = { fi_daddr = -1; fi_ioff = -1; fi_res = 0; };
                    ff_modified = true;
                } in
        
            mu_lock af'.ff_lock;

            let stat = server.afs_create_inode ~file:af' 
                                                ~final:false
                in
            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                mu_unlock af'.ff_lock;
                raise (Error stat);
            end;

            let cap' = { cap_port = super.afs_putport;
                             cap_priv = prv_encode ~obj:(Objnum obj')
                                          ~rights:prv_all_rights
                                          ~rand:af'.ff_random; 
                } in

            (*
            ** Copy the original data
            *)

            let buf' = buf_create 1000 in
                
            let co  = ref 0 in
            let cs  = ref (min off 1000) in
        
            (*
            ** First the part upto the off position.
            ** (Not shifted).
            *)
            while (!cs > 0)
            do
                (*
                ** First the chunk to read
                *)
                
                let stat =
                        server.afs_read_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                
                    in
                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;

                (*
                ** Now write the buffer 
                *)

                let stat =
                            server.afs_modify_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                        
                    in
                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;

                    
                co := !co + !cs;
                cs := min (off - !co) 1000;        
            done;

            (*
            ** Now the shifted part ipto the end of the file.
            *)

            cs := min (af.ff_size - !co) 1000;

            while (!cs > 0)
            do
                (*
                ** First the chunk to read
                *)
                
                let stat =
                        server.afs_read_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;

                (*
                ** Now write the buffer to the shifted position.
                *)

                let stat =
                            server.afs_modify_file ~file:af
                               ~off:(!co + off + size)
                               ~size:!cs
                               ~buf:buf'
                                        
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    mu_unlock af'.ff_lock;
                    raise (Error stat);
                end;
                    
                co := !co + !cs;
                cs := min (af.ff_size - !co) 1000;        
            done;

            (*
            ** Now write the data to be inserted.
            *)

            let stat =
                        server.afs_modify_file ~file:af'
                               ~off:off
                               ~size:size
                               ~buf:buf
                                
                in


            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                mu_unlock af'.ff_lock;
                raise (Error stat);
            end;

            let stat = release_file ~server:server ~file:af' ~flag:commit
                in

            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;

            ignore(release_file ~server:server ~file:af ~flag:0);

            std_OK,cap'
          end
        end
    end
    with
        | Error err -> err,nilcap


(*
** AFS server Delete request.
**
** Args:
**  server: the server structure
**  priv:   the request private field
**  size:   the requested size (<= file size)   [bytes]
**  off:    the file offset (<= file size)  [bytes]
**  commit: the commit flag
**
** Return:
**  status
**  newcap      (FF_unlocked -> oldcap)
*)

let afs_req_delete
        ~server
        ~priv
        ~off
        ~size
        ~commit
    =
    try 
    begin
        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sdddd 2 "afs_server:afs_req_delete [obj,off,size,commit]"
                 obj off size commit;
#endif


        let stat,af = acquire_file ~server:server ~obj:obj in 

        if (stat <> std_OK) then
            raise (Error stat);

        if (off+ size > af.ff_size) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_ARGBAD);
        end;
            
        if ((prv_decode ~prv:priv ~rand:af.ff_random) = false ||  
            (rights_req rights [afs_RGT_MODIFY]) = false) then
        begin    
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED);
        end;

        if (af.ff_state = FF_unlocked) then
        begin
            let cap = { cap_port = super.afs_putport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.ff_random; 
                } in
            
            let buf' = buf_create 1000 in

            let size' = ref (min (af.ff_size - off + size) 1000) in
            let off'  = ref (off+size) in


            while ( !off' < af.ff_size) 
            do
                    (*
                    ** First the chunk to read
                    *)
                
                let stat =
                        server.afs_read_file ~file:af
                               ~off:!off'
                               ~size:!size'
                               ~buf:buf'
                                
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;

                (*
                ** Now write the buffer to the shifted
                ** position.
                *)

                let stat =
                            server.afs_modify_file ~file:af
                               ~off:(!off'-size)
                               ~size:!size'
                               ~buf:buf'
                    in

                if (stat <> std_OK) then
                begin
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;

                off'  := !off' + !size';
                size' := min (af.ff_size - !off') 1000;
            done;

            (*
            ** Adjust the new size.
            *)

            ignore(server.afs_modify_size ~file:af
                                          ~newsize:(af.ff_size - size));

            let stat = release_file ~server:server ~file:af ~flag:commit
                in

            if (stat <> std_OK) then
            begin
                raise (Error stat);
            end;

            std_OK,cap

        end
        else
        begin
            (*
            ** Create a new file, copy the content of the
            ** original one, but shift the part from (off+size) 
            ** to (off).
            *)

            (*
            ** First get a new inode==object number
            *)
            let obj' = get_freeobjnum super in

            let af' = {
                    ff_lock = mu_create ();
                    ff_objnum = obj';
                    ff_random = uniqport ();
                    ff_time = server.afs_time();                    
                    ff_live = afs_MAXLIVE;
                    ff_state = FF_unlocked;
                    ff_size = af.ff_size - size;
                    ff_inode = { fi_daddr = -1; fi_ioff = -1; fi_res = 0; };
                    ff_modified = true;
                } in
        
            mu_lock af'.ff_lock;

            let stat = server.afs_create_inode ~file:af' 
                                                ~final:false 
                in
            if (stat <> std_OK) then
            begin
                mu_unlock af'.ff_lock;
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;

            let cap' = { cap_port = super.afs_putport;
                             cap_priv = prv_encode ~obj:(Objnum obj')
                                          ~rights:prv_all_rights
                                          ~rand:af'.ff_random; 
                } in

            (*
            ** Copy the original data
            *)
            let buf' = buf_create 1000 in
                
            let co  = ref 0 in
            let cs  = ref (min off 1000) in
        
            (*
            ** First the part upto the off position.
            ** (Not shifted).
            *)
            while (!cs > 0)
            do
                (*
                ** First the chunk to read
                *)
                
                let stat =
                        server.afs_read_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                
                    in
                if (stat <> std_OK) then
                begin
                    mu_unlock af'.ff_lock;
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;

                (*
                ** Now write the buffer 
                *)

                let stat =
                            server.afs_modify_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                        
                    in

                if (stat <> std_OK) then
                begin
                    mu_unlock af'.ff_lock;
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;
                    
                co := !co + !cs;
                cs := min (off - !co) 1000;        
            done;

            (*
            ** Now the shifted part upto the end of the file.
            *)

            cs := min (af'.ff_size - !co) 1000;

            while (!co < af'.ff_size)
            do
                (*
                ** First the chunk to read
                *)
                
                let stat =
                        server.afs_read_file ~file:af
                               ~off:(!co+size)
                               ~size:!cs
                               ~buf:buf'
                                
                    in

                (*
                ** Now write the buffer to the shifted position.
                *)

                let stat =
                            server.afs_modify_file ~file:af
                               ~off:!co
                               ~size:!cs
                               ~buf:buf'
                                        
                    in

                if (stat <> std_OK) then
                begin
                    mu_unlock af'.ff_lock;
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;
                    
                co := !co + !cs;
                cs := min (af'.ff_size - !co) 1000;        
            done;

            let stat = release_file ~server:server ~file:af' ~flag:commit
                in

            if (stat <> std_OK) then
            begin
                mu_unlock af'.ff_lock;
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;

            ignore(release_file ~server:server ~file:af ~flag:0);

            std_OK,cap'
        end
    end
    with
        | Error err -> err,nilcap


(*
** Destroy a file object.
*)

let afs_req_destroy
        ~server
        ~priv
    =
    try
    begin
        let rights = prv_rights priv in
        let super = server.afs_super in

        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sd 2 "afs_server:afs_req_destroy [obj]" obj;
#endif

        let stat,af = acquire_file ~server:server ~obj:obj in 

        if (stat <> std_OK) then
            raise (Error stat);

        if ((prv_decode ~prv:priv ~rand:af.ff_random) = false ||  
            (rights_req rights [afs_RGT_DESTROY]) = false) then
        begin    
            ignore(release_file ~server:server ~file:af ~flag:0);            
            raise (Error std_DENIED);
        end;
        if (af.ff_state = FF_unlocked) then
        begin
            let stat = server.afs_delete_inode ~file:af in
            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;
            std_OK
        end
        else
        begin
            let stat = server.afs_delete_inode ~file:af in
            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;
            ignore(release_file ~server:server ~file:af ~flag:0);
            std_OK
        end;
    end
    with
        | Error err -> err

(*
** Statistic request.
*)

open Printf

let afs_req_stat ~server
                 ~priv
    =
    try
    begin
        let Objnum obj = prv_number priv in
        let stat,af = acquire_file ~server:server ~obj:obj in 

        if (stat <> std_OK) then
            raise (Error stat);

        if ((prv_decode ~prv:priv ~rand:af.ff_random) = false) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error std_DENIED)
        end;

        let stat,str = server.afs_stat ~obj:obj in
        if (stat <> std_OK) then
        begin
            ignore(release_file ~server:server ~file:af ~flag:0);
            raise (Error stat);
        end;
        ignore(release_file ~server:server ~file:af ~flag:0);
        std_OK,str
    end
    with
        | Error err -> err,""

(*
** Sync the disk, flush all caches.
*)

let afs_req_sync ~server
                 ~priv
    =
    try
    begin
        let Objnum obj = prv_number priv in

        if (obj > 0) then
        begin
            let stat,af = acquire_file ~server:server ~obj:obj in 
        
            if (stat <> std_OK) then
                raise (Error stat);
            if ((prv_decode ~prv:priv ~rand:af.ff_random) = false) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error std_DENIED);
            end;

            let stat = server.afs_sync () in
            if (stat <> std_OK) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error stat);
            end;
            ignore(release_file ~server:server ~file:af ~flag:0);
            std_OK
        end
        else
        begin
            if (prv_decode ~prv:priv ~rand:server.afs_super.afs_checkfield) 
                = false then
            begin
                raise (Error std_DENIED);
            end;
            let stat = server.afs_sync () in
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end
    end
    with
        | Error err -> err

(*
** Touch request
*)

let afs_req_touch ~server
                  ~priv
    =
    try
    begin
        let Objnum obj = prv_number priv in
        if (obj > 0) then
        begin
            let stat,af = acquire_file ~server:server ~obj:obj in 
    
            if (stat <> std_OK) then
                raise (Error stat);

            if ((prv_decode ~prv:priv ~rand:af.ff_random) = false) then
            begin
                ignore(release_file ~server:server ~file:af ~flag:0);
                raise (Error std_DENIED);
            end;

            server.afs_touch af;
            ignore(release_file ~server:server ~file:af ~flag:0);
            std_OK
        end
        else
            std_OK
    end
    with
        | Error err -> err


let afs_req_age ~server
                ~priv
    =
    try
    begin
        let gone = ref 0 in
        let rights = prv_rights priv in
        let super = server.afs_super in
        let Objnum obj = prv_number priv in

        if (obj <> 0 ||
            (prv_decode ~prv:priv
                        ~rand:super.afs_checkfield) = false ||
            (rights = afs_RGT_ALL) = false) then
        begin
                raise (Error std_DENIED);
        end;

        (*
        ** Age all objects (used and unused inodes). Destroy files
        ** with live time equal zero (only valid files).
        *)

        for i = 1 to super.afs_nfiles-1
        do
            let used,time = server.afs_age ~obj:i in
            if (used = true && time = 0) then
            begin
                let stat,af = acquire_file ~server:server ~obj:i in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err "AFS: afs_req_age: acq failed: %d\n" i; 
                    raise (Error stat);
                end;
                let stat = server.afs_delete_inode ~file:af in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err "AFS: afs_req_age: del failed: %d\n" i; 
                    ignore(release_file ~server:server ~file:af ~flag:0);
                    raise (Error stat);
                end;
                ignore(release_file ~server:server ~file:af ~flag:0);
                incr gone;
            end;     
        done;  
        sys_log Sys_info "AFS: afs_req_age: %d objects destroyed!\n" (!gone);
        std_OK
    end
    with
        | Error err -> err

