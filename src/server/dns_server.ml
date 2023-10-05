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
**    $CREATED:     2003.
**    $VERSION:     1.52
**
**    $INFO:
**
** DNS: Directory and Name Service
** Server implementation.
**
**
**    $ENDOFINFO
**
*)


open StdLabels

open Amoeba
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
open Ar
open Syslog

module Str = StrLabels
open Str


open Dns_common

exception Deny
let debug = 1

(*
** Directory have live time between MAXLIVE and 0.
** Zero live time means: destroy the directory after an age request.
*)

let dns_MAXLIVE = 8 

(*
** One rows of a directory (= directory entry).
*)

type dns_row = {
    mutable dr_name: string;                (* The row name         *)
    mutable dr_time : int;                  (* Time stamp           *)
    mutable dr_columns: rights_bits array;  (* The rights mask      *)  
    mutable dr_capset: capset;              (* Row capability set   *)
} 

let nildnsrow = {dr_name="";dr_time=0;dr_columns=[||];
                  dr_capset=Capset.nilcapset}

(*
** One DNS table entry (= directory). All rows are stored in a double
** linked list.
*)

type dns_dir_state = 
    (*
    ** Not used
    *)
    | DD_invalid        (* not used *)
    (*
    ** new directory; not written to disk - can be modified
    *)
    | DD_unlocked       
    (*
    ** Special case:
    ** Previously locked directory - now modified   
    **  -> a new directory AFS object must be created!
    *)
    | DD_modified       
    (*
    ** Read only directory (standard case)
    *)
    | DD_locked         
    

type dns_dir = {
    mutable dd_lock: Mutex.t;
    mutable dd_objnum: int;                 (* The directory index number   *)
    mutable dd_ncols: int;                  (* Number of columns            *)
    mutable dd_nrows: int;                  (* Number of rows in this dir   *)
    mutable dd_colnames: string array;      (* The column names             *)
    mutable dd_random: port;                (* Random check number          *)
    mutable dd_rows: dns_row dblist;        (* All the rows               *) 
    mutable dd_state: dns_dir_state;        (* Status of the directory      *)
    mutable dd_time: int;                   (* Time stamp                   *)
    mutable dd_live: int;                   (* Live time [0..MAXLIVE]   *)
}

let nildnsdir = { dd_lock = mu_create (); dd_objnum = 0; dd_ncols = 0;
                  dd_nrows = 0; dd_colnames = [||]; dd_random = nilport;
                  dd_rows = Dblist.create (); dd_state = DD_invalid; dd_time = 0;
                  dd_live = 0;
        }

(*
** DNS operating modes
*)

type dns_mode =
    | Dnsmode_ONECOPY       (* Default file server number = 0 *)
    | Dnsmode_TWOCOPY       (* Duplicated file server mode    *)


(*
** The filesystem server informations we need to store the directory files
** and to tell clients about the default fileserver. We can use up
** to two file servers.
*)

type fs_state = 
    | FS_down 
    | FS_up   
    | FS_unknown
        

type fs_server = {
    mutable fs_cap      : capability array;     (* FS server ports *)
    mutable fs_state    : fs_state array;       (* FS server state *)
    mutable fs_default  : int;                  (* Cuurent default *)          
    mutable dns_mode    : dns_mode;
}


(*
** The main DNS structure: the all known super structure with basic
** informations about the file system.
*)

type dns_super = {
    mutable dns_lock: Mutex.t;
    mutable dns_name : string;           (* The server name      *)

    mutable dns_ndirs: int;              (* Number of total table entries *)
    mutable dns_nused: int;              (* Number of used table entries  *)
    mutable dns_freeobjnums: int list;   (* Free slots list               *)
    mutable dns_nextfree: int;           (* Next free slot                *)


    mutable dns_getport : port;                     (* Private getport      *)
    mutable dns_putport : port;                     (* Public putport       *)
    mutable dns_checkfield : port;                  (* Private checkfield   *)

    mutable dns_ncols : int;                        (* Number of columns    *)
    mutable dns_colnames : string array;            (* Column names         *)
    mutable dns_generic_colmask : rights_bits array;   (* Column mask  *)

     
    mutable dns_fs_server : fs_server;              (* File server *)

    (*
    ** Not used by this module
    *)

    mutable dns_block_size: int;            (* Inode Disk Blocksize [bytes] *)

}

let nildnssuper = {dns_lock = mu_create ();
                   dns_name="";dns_ndirs=0;dns_nused=0;
                   dns_freeobjnums=[];dns_nextfree=0;
                   dns_getport=nilport;dns_putport=nilport;
                   dns_checkfield=nilport;dns_ncols=0;
                   dns_colnames=[|""|];
                   dns_generic_colmask=[|Rights_bits 0|];
                   dns_block_size=0;
                   dns_fs_server={ fs_cap = [|nilcap|];
                                   fs_state = [|FS_unknown|];
                                   fs_default = 0;
                                   dns_mode = Dnsmode_ONECOPY;
                   };
                    }

(*
** The server structure.
*)

type dns_server = {

    (*
    ** The super structure
    *)

    mutable dns_super : dns_super;

    (*
    ** Server function to read and write directories and the super
    ** table structure. 
    ** The index number is the directory number (= object number).
    ** The server is responsible for managing the directory table, for
    ** example caching, reading and writing of directory changes.
    ** This is not part of this module!
    *)
    
    (*
    ** Read a directory specified with his objnum (index) number.
    *)
    
    mutable dns_read_dir : obj:int -> dns_dir*status; 

    (*
    ** Write an existing directory (only if modified).
    *)
    
    mutable dns_modify_dir : dir:dns_dir -> status;

    (*
    ** Insert a new created directory into the DNS table.
    *)
    
    mutable dns_create_dir : dir:dns_dir -> status;

    (*
    ** Delete a directory. Destroy associated AFS objects.
    *)
    
    mutable dns_delete_dir : dir:dns_dir -> status;

    (*
    ** Delete an inode without destroying AFS objects.
    *)
    
    mutable dns_delete_inode : obj:int -> status;

    (*
    ** Read the super structure.
    *)
    
    mutable dns_read_super : unit -> dns_super*status;  


    (*
    ** Flush the caches (if any).
    *)
    
    mutable dns_sync : unit -> status;

    (*
    ** Create generic server statistics informations.
    *)

    mutable dns_stat: unit -> status * string;

    (*
    ** Touch and age directory objects
    *)

    mutable dns_touch: dir:dns_dir -> unit;
    (*
    ** Returns (used,livetime) tuple. This functions first probes for an
    ** used inode, and in the case of an used inode, it ages the live time.
    ** If live time zero is reached, the object must be destroyed.
    ** (by this module).
    *)
    mutable dns_age: obj:int -> bool*int;

    (*
    ** Something to do on exit ?
    *)
    mutable dns_exit: unit -> status;

    (*
    ** get current system time in 10s units
    *)
    mutable dns_time: unit -> int;
}

(*
** The right bit for each column. Indead, only the first
** dns_MAXCOLUMNS array elements are used.
*)

let dns_col_bits = [| Rights_bits 0x1;
                      Rights_bits 0x2;
                      Rights_bits 0x4;
                      Rights_bits 0x8;
                      Rights_bits 0x10;
                      Rights_bits 0x20;
                      Rights_bits 0x40;
                      Rights_bits 0x80 |]

(*
** Util functions
*)

(*
** Acquire and lock a directory with object number 'obj'. A
** release_dir call must follow this operation.
*)


let acquire_dir  ~server ~obj  =
    let super = server.dns_super in
    mu_lock super.dns_lock;
    
    let dir,stat = server.dns_read_dir ~obj:obj in

#ifdef DEBUG
    Db.Pr.sd debug "acquire_dir" dir.dd_objnum;
#endif
    if (stat = std_OK) then
        mu_lock dir.dd_lock;

    mu_unlock super.dns_lock;

    (stat,dir)
    

(*
** Release an acquired directory. Flush all pending writes - the
** super structure and the inode table, and the directory itself
** if modified. Return the status of the operation.
*)

let release_dir ~server ~dir =
    if (dir <> nildnsdir) then
    begin
#ifdef DEBUG
        Db.Pr.sd debug "release_dir" dir.dd_objnum;
#endif
        let stat = if (dir.dd_state = DD_unlocked ||
                       dir.dd_state = DD_modified) then
                        server.dns_modify_dir ~dir:dir 
                   else
                        std_OK in
        mu_unlock dir.dd_lock;
        stat
    end
    else
        std_ARGBAD



(*
** Find a row 'name' in directory 'dir'.
*)

let dns_search_row ~dir ~name =
        try
        begin
            Some (
                    Dblist.find_data 
                           ~f:(fun r ->
                                    if (r.dr_name = name) then
                                        true
                                    else
                                        false;
                              ) ~dl:dir.dd_rows;
                 )
        end
        with
            | Not_found -> None;
            | List_empty -> None
            

                            
(*
** Return a free objnum in the directory table (index). 
** Note: protect this function with the server lock untill
** the directory creation is finished.
*)

let get_freeobjnum super =
    (*
    ** First check the free objnums list. If empty, use nextfree at the
    ** end of the directory table.
    *)
    super.dns_nused <- super.dns_nused + 1;
        
    match super.dns_freeobjnums with
        | hd::tl -> super.dns_freeobjnums <- tl;
                    hd;
        | [] -> 
            let objnum = super.dns_nextfree in

            if (objnum + 1 = super.dns_ndirs) then
                failwith "DNS: TODO: out of directory slots";

            super.dns_nextfree <- objnum + 1;
            objnum

(*
** Create a new directory. Return the directory structure and
** the status returned by the server create function. The super structure
** is already modified by this function. The new directory remains
** locked.
*)

let dns_create_dir ~server 
                   ~ncols 
                   ~colnames =
    let super = server.dns_super in
    
    mu_lock super.dns_lock;
    let objnum = get_freeobjnum super in        
    
    let new_dir = {
            dd_lock = mu_create ();
            dd_objnum = objnum;
            dd_ncols = ncols;
            dd_nrows = 0;
            dd_colnames = colnames;
            dd_rows = Dblist.create ();                     (* Still empty *)
            dd_state = DD_unlocked;
            dd_time = server.dns_time ();                       
            dd_live = dns_MAXLIVE;
            dd_random = uniqport ();
        } in


    mu_lock new_dir.dd_lock;
    mu_unlock super.dns_lock;
    
    let stat = server.dns_create_dir  ~dir:new_dir in
    
    (stat,new_dir)
    

(*
** Convert a directory structure into a capset with maybe
** restricted rights.
*)

let capset_of_dir ~super ~dir ~rights =
    let cap = {
        cap_port = super.dns_putport;
        cap_priv = prv_encode ~obj:(Objnum dir.dd_objnum)
                              ~rights:rights
                              ~rand:dir.dd_random;
                              
    } in   

#ifdef DEBUG
    Db.Pr.ss debug "capset_of_dir" (Ar.ar_cap cap);
#endif

    cs_singleton cap
    
(*
** Create the super block.
*)

let dns_create_super 
            ~name 
            ~ndirs
            ~ncols
            ~colnames
            ~colmask
            =
            
            let getport = uniqport () in
            let checkfield = uniqport () in
            let putport = priv2pub getport in
            {
                dns_lock = mu_create ();
                dns_name = name;
                dns_nused = 0;
                dns_ndirs = ndirs;
                dns_freeobjnums = [];
                dns_nextfree = 1;
                dns_getport = getport;
                dns_putport = putport;
                dns_checkfield = checkfield;
                dns_ncols = ncols;
                dns_colnames = colnames;
                dns_generic_colmask = colmask;
                dns_fs_server = { fs_cap = [||]; fs_state = [||];
                                  fs_default = 0; dns_mode = Dnsmode_ONECOPY };
                dns_block_size = 0;
            }
            

(*
** Create the root directory (Object number 1) and return it and the status
** of the server create function. All pending writes are already done here.
**
** Note: the root directory can't be deleted. Therefore it's not
** garbage collected.
*)

let dns_create_root ~server =
    try
    begin
        let super = server.dns_super in
    
        mu_lock super.dns_lock;

        if (super.dns_nextfree <> 1) then
        begin
            mu_unlock super.dns_lock;
            raise (Error std_EXISTS);
        end;    
        super.dns_nextfree <- 2;
    
        let root_dir = {
            dd_lock = mu_create ();
            dd_objnum = 1;
            dd_ncols = super.dns_ncols;
            dd_nrows = 0;
            dd_colnames = super.dns_colnames;
            dd_rows = Dblist.create ();                     (* Still empty *)
            dd_state = DD_unlocked;
            dd_time = server.dns_time ();
            dd_live = dns_MAXLIVE;  
            dd_random = uniqport ();
        } in
    
        mu_lock root_dir.dd_lock;
        mu_unlock super.dns_lock;

        let stat = server.dns_create_dir  ~dir:root_dir in
        if (stat <> std_OK) then
        begin
            mu_unlock root_dir.dd_lock;
            raise (Error stat);
        end;

        let stat = server.dns_modify_dir  ~dir:root_dir in
        if (stat <> std_OK) then
        begin
            mu_unlock root_dir.dd_lock;
            raise (Error stat);
        end;
        mu_unlock root_dir.dd_lock;
    
        std_OK, root_dir 
    end
    with
        | Error err -> err,nildnsdir

(*
** Remove an empty directory. Don't call release_dir after this
** function!
*)
    
let dns_delete_dir ~server ~dir =
    try
    begin
        let super = server.dns_super in

        mu_lock super.dns_lock;
    
        if (dir.dd_nrows <> 0) then
        begin
            mu_unlock super.dns_lock;                
            raise (Error dns_NOTEMPTY);
        end;

        let objnum = dir.dd_objnum in
        super.dns_freeobjnums <- super.dns_freeobjnums @ [objnum];

        let stat = server.dns_delete_dir ~dir:dir in
        if (stat <> std_OK) then
        begin
            mu_unlock dir.dd_lock;
            mu_unlock super.dns_lock;                
            raise (Error stat);
        end;
        mu_unlock dir.dd_lock;
        mu_unlock super.dns_lock;                
        std_OK
    end
    with
        | Error err -> err

(*
** Destroy a directory, either empty or not. Don't call release_dir
** after this function!
*)
    
let dns_destroy_dir ~server ~dir =
    let super = server.dns_super in

    mu_lock super.dns_lock;

    let objnum = dir.dd_objnum in
    super.dns_freeobjnums <- super.dns_freeobjnums @ [objnum];

    let stat = server.dns_delete_dir ~dir:dir in
    mu_unlock dir.dd_lock;
    mu_unlock super.dns_lock;

    stat
    

(*
** Create a new directory row. Simple.
*)

let dns_create_row 
        ~name
        ~cols
        ~cs     =
        {
            dr_name = name;
            dr_time = 0;       
            dr_columns = cols;
            dr_capset = cs;
        }  
        
let dir_modified dir =
    if (dir.dd_state = DD_locked) then
        dir.dd_state <- DD_modified
        
(*
** Append a row to a directory.
*)

let dns_append_row ~server ~dir ~row =
    Dblist.insert_tail ~dblist:dir.dd_rows ~node:row;
    dir.dd_nrows <- dir.dd_nrows + 1;
    dir_modified dir

            
    
(*
** Remove a row from a directory.
*)

let dns_delete_row ~server ~dir ~row =
    Dblist.remove_data ~dl:dir.dd_rows ~node:row;
    dir.dd_nrows <- dir.dd_nrows - 1;
    dir_modified dir



    
(*
** A client request arrived.  Enter the critical section.  Check the capability.
** If correct, check whether the required rights are present or not.
** Return the directory structure, but only if the required rights are present.
*)

let request_dir ~server ~priv ~req =

    try
    begin
        let Objnum obj = prv_number priv in

        let stat,dir = acquire_dir ~server:server ~obj:obj in

        if (stat <> std_OK) then
            raise (Error stat);

        (*
        ** Validate the private field.
        *)

#ifdef DEBUG
        Db.Pr.ss debug "acquire: random" (Ar.ar_port dir.dd_random);
#endif
    
        if ((prv_decode ~prv:priv
                        ~rand:dir.dd_random) = false) then
        begin
            ignore(release_dir ~server:server ~dir:dir); 
            raise (Error std_DENIED);
        end;                        

    
        let ncols = dir.dd_ncols in
        
        (* 
        ** When checking the presence of column rights, only take the
        ** *actual*
        ** columns present into acount (i.e., do not use dns_COLMASK here)
        *) 

        let Rights_bits colbits = dns_col_bits.(ncols) in
        let Rights_bits rights  = (prv_rights priv) in     
        let Rights_bits req     = req in

        
        let colmask = colbits - 1 in

#ifdef DEBUG
        Db.Pr.sd debug "acquire: req" req;
        Db.Pr.sd debug "acquire: rights" rights;
        Db.Pr.sd debug "acquire: colmask" colmask;
#endif
 
        if ((rights land colmask = 0) ||
            (rights land req <> req)) then
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_DENIED);
        end;
        (std_OK,dir)
    end
    with
        | Error err -> err,nildnsdir


(*
** With a given directory capset check if this directory belongs
** to this server. 
*)
 
let get_dir ~server ~dir_cs =
    
    let super = server.dns_super in
    let stat  = ref std_CAPBAD in
    let priv  = ref nilpriv in
    
    try
    (
        for i = 0 to dir_cs.cs_final - 1 
        do
            let s = dir_cs.cs_suite.(i) in
            if (s.s_current = true) then
            begin
                let cap = s.s_object in
                if ((portcmp cap.cap_port super.dns_putport) = true &&
                    (prv_number cap.cap_priv) <> Objnum 0) then
                begin
                    (*
                    ** A capability for me. Check it.
                    *)
                    priv := cap.cap_priv;
                    raise Exit;
                end
            end
        done;
        (!stat,!priv)        

    )
    with
        | Exit ->   (std_OK,!priv)


(*
** Restrict a capability set.
** TODO: Caching.
*)

let dns_restrict ~server ~cs ~mask =
    try
    begin
        let super = server.dns_super in
    
        let stat = ref std_OK in
        let cs_restr = cs_copy cs in

        if (cs_restr.cs_final = 0) then
            raise (Error dns_UNREACH);

        for i = 0 to cs_restr.cs_final - 1
        do
            let s = cs_restr.cs_suite.(i) in

            if (s.s_current = true) then
            begin
                let c = s.s_object in 

#ifdef DEBUG
                Db.Pr.ss debug "dns_restrict cap" (Ar.ar_cap c);
#endif

                if ((portcmp c.cap_port super.dns_putport) = true &&
                    (prv_number c.cap_priv) <> Objnum 0) then
                begin
                    (*
                    ** An object from this DNS server. Simple.
                    *)
                    let stat,dir = request_dir ~server:server
                                               ~priv:c.cap_priv
                                               ~req:(Rights_bits 0)
                        in
                    if (stat <> std_OK) then
                        raise (Error stat);

#ifdef DEBUG
                    Db.Pr.ss debug "dns_restrict: acquire" "OK";
#endif                        
    
                    let rights = rights_and (prv_rights c.cap_priv) mask in
                    c.cap_priv <- prv_encode ~obj:(prv_number c.cap_priv) 
                                             ~rights:rights
                                             ~rand:dir.dd_random;

                    ignore(release_dir ~server:server ~dir:dir);
                end else
                if ((portcmp c.cap_port super.dns_putport) = true &&
                    (prv_number c.cap_priv) <> Objnum 0) then
                begin
                    let rights = rights_and (prv_rights c.cap_priv) mask in
                    c.cap_priv <- prv_encode ~obj:(prv_number c.cap_priv) 
                                             ~rights:rights
                                             ~rand:super.dns_checkfield;

                end else
                begin
                    (*
                    ** Not owned by this server.
                    *)

                    let stat,rc = std_restrict ~cap:c 
                                               ~mask:mask in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    s.s_object <- rc;
                end
            end
            else
            begin
                (*
                ** Not current. Generate a fake capability with the correct
                ** port.
                *)
                let cap = {
                     cap_port = s.s_object.cap_port;
                    cap_priv = priv_copy nilpriv;
                } in
                s.s_object <- cap;
            end
        done;
        std_OK,cs_restr
    end
    with
        | Error err -> err,nilcapset

(************************************ EXREV ******************************)    
    
(*
** Request handlers.
*)


(*
** dns_LOOKUP: 
** Traverse a path as far as possible, and return the resulting capability
** set and the rest of the path. 
*)

let dns_req_lookup 
            ~server
            ~priv        (* hdr.cap_priv from client request *) 
            ~path        (* path name to lookup *)
            =
    try
    begin
        let super = server.dns_super in
    
        let rights = prv_rights priv in
        let stat,dir = request_dir ~server:server
                               ~priv:priv
                               ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let Rights_bits rights = rights in

        let path = path_normalize path in
        let path_rel = Filename.is_relative path in
        let pathl = Str.split (Str.regexp "/") path in           

        let path_glue pl =
            let rec iter pl =
                match pl with
                | hd::tl -> "/"^hd^(iter tl); 
                | [] -> "";
            in
            let ps = iter pl in

            if (path_rel = false && ps <> "") then
                String.sub ps ~pos:1 ~len:((String.length ps)-1)
            else 
                ps
        in

#ifdef DEBUG        
        Db.Pr.sd 1 "dns_LOOKUP,rights" rights;
#endif
        
        let Rights_bits all_rights = prv_all_rights in
        let colmask = rights land all_rights in

        (*
        ** During we iterate over the path compononets, this
        ** rights value will be adjusted with the rights from the
        ** path components and used finally to restrict the
        ** capability set of the last path component
        ** resolved by this server.
        *)
        
        let have_rights = ref all_rights in        
        
        let path_rest = ref [""] in
        let dir_last = ref dir in
        let row_last = ref nildnsrow in
        
        let rec iter_path pl dir mask =
            match pl with
            | hd::tl ->
            begin
                path_rest := tl;
                dir_last  := dir;
#ifdef DEBUG
                Db.Pr.ss 1 "dns_LOOKUP,path" hd;
#endif                

                (* 
                ** When checking the presence of column rights, only take
                ** the *actual* columns present into acount, 
                ** so do not use dns_COLMASK here.
                *)
                         
                let ncols = dir.dd_ncols in
                let Rights_bits colbits = dns_col_bits.(ncols) in
                let colrights = colbits - 1 in        

                (*
                ** The columns mask for the calculation for the
                ** next lookup, if any,
                ** and the permission for the row lookup in the current 
                ** directory.
                ** The current have_rights are logical
                ** anded with the current mask (limited to
                ** ncol bits).
                *)
                
                let colmask = mask land !have_rights land colrights in

#ifdef DEBUG
                Db.Pr.sd 1 "dns_LOOKUP,colmask" colmask;
#endif                
                
                if (colmask = 0) then
                begin
                    raise Deny;     (* No permission to read the directory *)
                end;
                    
                (*
                ** Lookup the desired row in this directory.
                *)
                
                let row = 
                    (
                        let row' = dns_search_row ~dir:dir ~name:hd in
                        match row' with 
                        | Some row  -> row;
                        | None      -> raise Not_found;
                    ) in 
                    
                row_last := row;
                
                (*
                ** Calculate the new have_rights value.
                ** All the columns rights from the current row
                ** are logical ored,if, and only if the i-th
                ** bit in the current colmask is set. 
                ** The i-th bit corresponds to the i-th rights column
                ** in the current row.
                *)

                let colmask' = ref 0 in
                let cols = row.dr_columns in

                for i = 0 to ncols - 1 
                do
                    let Rights_bits coli = cols.(i) in 
                    let Rights_bits colbits = dns_col_bits.(i) in
                    
                    (*
                    ** If the i-th bit in colmask is set,
                    ** add the i-th column of the row to
                    ** the new colmask'.
                    *)
                     
                    if (colmask land colbits = colbits) then
                        colmask' := !colmask' lor coli;
                done;
                
                have_rights := !colmask';

#ifdef DEBUG                
                Db.Pr.sd 1 "dns_LOOKUP,have_rights" (!have_rights);
#endif

                (*
                ** Get and acquire the next directory. If the next
                ** object belongs not to this server, exit.
                *)
#ifdef DEBUG
                Db.Pr.ss 1 "dns_LOOKUP: caps" (ar_cs row.dr_capset);
#endif
                let stat,priv = get_dir ~server:server
                                        ~dir_cs:row.dr_capset 
                in
                
                if (stat = std_OK) then
                begin
                    let stat,dir = request_dir ~server:server
                                               ~priv:priv
                                               ~req:(Rights_bits 0)
                                               in 
                    if (stat = std_OK) then
                    begin
                        (*
                        ** The current colmask must be finally
                        ** logical anded with the rights field
                        ** of the next directory.
                        *)
                        
                        let Rights_bits rights = prv_rights priv in
                        let colmask = colmask land rights in
                        
                        ignore(release_dir ~server:server  ~dir:!dir_last);
                        iter_path tl dir colmask;
                    end
                    else
                        raise Exit;
                end
                else
                begin
                    raise Exit;                                       
                end;
            end;
            
            | [] ->
            begin  
                dir_last  := dir;
            end;
        in 

        try
            begin
                iter_path pathl dir colmask;

                ignore(release_dir ~server:server ~dir:!dir_last);
                
                (*
                ** Build the restricted object capability set 
                *)
                let stat,cs_restr = dns_restrict ~server:server
                                             ~cs:!row_last.dr_capset 
                                             ~mask:(Rights_bits !have_rights)
                in

#ifdef DEBUG
                if (stat = std_OK) then
                    Db.Pr.ss debug "dns_LOOKUP,cs_restr" (
                        Ar.ar_cap (cs_restr.cs_suite.(0).s_object)
                    )
                else
                    Db.Pr.ss debug "LOOKUP restr" (err_why stat);
                Db.Pr.ss 1 "dns_LOOKUP,path_rest" (path_glue !path_rest);
#endif

                stat,cs_restr,(path_glue !path_rest);
            end    
        with
            | Not_found -> 
            begin
                ignore(release_dir ~server:server ~dir:!dir_last);
                std_NOTFOUND,nilcapset,(path_glue !path_rest);
            end;                

            | Deny -> 
            begin
                ignore(release_dir ~server:server ~dir:!dir_last);
                std_DENIED,nilcapset,(path_glue !path_rest);
            end;
            | Exit ->
            begin
                (*
                ** Build the restricted object capability set 
                *)
                let stat,cs_restr = dns_restrict ~server:server
                                             ~cs:!row_last.dr_capset 
                                             ~mask:(Rights_bits !have_rights)
                in

#ifdef DEBUG                
                if (stat = std_OK) then
                    Db.Pr.ss debug "dns_LOOKUP,cs_restr" (
                        Ar.ar_cap (cs_restr.cs_suite.(0).s_object)
                    )
                else
                    Db.Pr.ss debug "dns_LOOKUP restr:" (err_why stat);
                Db.Pr.ss 1 "dns_LOOKUP,path_rest" (path_glue !path_rest);
#endif


                ignore(release_dir ~server:server ~dir:!dir_last);
                stat,cs_restr,(path_glue !path_rest);
            end;         
    end
    with 
        | Error err -> err,nilcapset,""
    

(*
** List a directory.  Returns a flattened representation of the number of
** columns, the number of rows, the names of the columns, the names of the
** rows and the right masks. 
** Return status,
**        the number of total rows and columns, 
**        the col names list,
**        the (dr_name,dr_columns) list starting with firstrow.
*)

let dns_req_list ~server
                 ~priv
                 ~firstrow
                 =
    try
    begin
        let super = server.dns_super in
    
        let rights = prv_rights priv in
        let stat,dir = request_dir ~server:server
                               ~priv:priv
                               ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let ncols = dir.dd_ncols in
        let nrows = dir.dd_nrows in
        let colnames = dir.dd_colnames in
        
        (*
        ** Build an row list starting with 'firstrow'.
        *)

#ifdef DEBUG
        Db.Pr.sd debug "LIST: gotit (rows)" nrows;
#endif
        
        let rowlist = ref [] in
        
        let rows = Dblist.to_array dir.dd_rows in

        let rowlist = ref [] in
        
        if (nrows > 0) then
        for i = firstrow to nrows-1
        do

#ifdef DEBUG
            Db.Pr.ss debug "LIST rowi" ((rows.(i)).dr_name);
#endif
            rowlist := !rowlist @ [(rows.(i)).dr_name,
                                   (rows.(i)).dr_columns];    
        done;

        ignore(release_dir ~server:server ~dir:dir);
        
        (std_OK,nrows,ncols,colnames,!rowlist)
                 
    end
    with
        | Error err -> (err,0,0,[||],[])

(*
** Append a row to a directory. The name, right masks (cols), and initial
** capability must be specified.
*)
        
let dns_req_append  ~server
                    ~priv
                    ~name
                    ~cols
                    ~capset
                    =
    try
    begin                        
        let super = server.dns_super in

        let stat,dir = request_dir ~server:server
                               ~priv:priv
                               ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        if (name = "") then
        begin
            raise (Error std_ARGBAD);
        end;

        (*
        ** Check that the entry doesn't exists alreay
        *)
        match (dns_search_row ~dir:dir ~name:name) with
          | Some _ -> 
          begin
            ignore(release_dir ~server:server ~dir:dir);
            std_EXISTS;
          end;
          | None ->
          begin
            let new_row = dns_create_row ~name:name
                                         ~cols:cols
                                         ~cs:capset
            in
            new_row.dr_time <- server.dns_time ();
            dns_append_row ~server:server
                                      ~dir:dir
                                      ~row:new_row;
                                      
            let stat = release_dir ~server:server ~dir:dir in
            if (stat <> std_OK) then
                raise (Error stat);

            std_OK
          end;
    end
    with
        | Error err -> err


(*
** Create a new directory table entry.
*)

let dns_req_create ~server
                   ~priv
                   ~colnames
                   =

    try
    begin
        let super = server.dns_super in
        let ncols = Array.length colnames in
    
        if ncols > dns_MAXCOLUMNS then
            raise (Error std_ARGBAD);

        (*
        ** Private is only needed to check for a valid capability.
        ** The new directory belongs currently to no other directory object!
        *)
    

        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits 0) in
    
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        ignore(release_dir ~server:server ~dir:dir);
        
        let stat,new_dir = dns_create_dir ~server:server
                                           ~ncols:ncols
                                           ~colnames:colnames
        in
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
        let new_cs = capset_of_dir ~super:super
                                   ~dir:new_dir
                                   ~rights:prv_all_rights
            in
        let stat = release_dir ~server:server ~dir:new_dir in
        if (stat <> std_OK) then
            raise (Error stat);

        std_OK,new_cs
    end
    with
        Error err -> err,nilcapset


(*
** Remove a directory.  Simple.  Only allow this if the directory is empty.
*)

let dns_req_discard ~server
                   ~priv
                   =
    try
    begin
        let super = server.dns_super in

        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_DEL) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let stat = dns_delete_dir ~server:server
                                  ~dir:dir
            in
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
        (*
        ** dir is deleted therefore no realse is needed
        *)
        std_OK
    end
    with
        | Error err -> err
        

(*
** Destroy a directory - either empty or not.
*)

let dns_req_destroy ~server
                    ~priv
                    =
    try
    begin
        let super = server.dns_super in

        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_DEL) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let stat = dns_destroy_dir ~server:server
                                   ~dir:dir
            in 
        (*
        ** dir is deleted therefore no realse is needed
        *)
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
        std_OK                                  
    end
    with
        | Error err -> err
        

(*
** Change the rights masks in a row.                                   
*)

let dns_req_chmod ~server
                  ~priv
                  ~cols
                  ~name
                  =

    try
    begin
        let super = server.dns_super in
        let ncols = Array.length cols in
    
        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;


        if (ncols = 0) then
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_ARGBAD);
        end;

        let ncolsmax = dir.dd_ncols in
        if (ncols > ncolsmax) then
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_ARGBAD);
        end;
        (*
        ** Lookup the desired row in this directory.
        *)
                        
        match (dns_search_row ~dir:dir ~name:name) with 
            | Some row  -> 
            begin
                for i = 0 to (ncols-1)
                do
                    row.dr_columns.(i) <- cols.(i);
                done;

                dir_modified dir;
                let stat = server.dns_modify_dir ~dir:dir in
                if (stat <> std_OK) then
                begin
                    ignore(release_dir ~server:server ~dir:dir);
                    raise (Error stat);
                end;
                let stat = release_dir ~server:server ~dir:dir in
                if (stat <> std_OK) then
                begin
                    raise (Error stat);
                end;
                std_OK
            end;
            | None      -> 
            begin
                ignore(release_dir ~server:server ~dir:dir);
                raise (Error std_NOTFOUND);
            end;
    end                                  
    with
        | Error err -> err
        
(*
** Delete a row within a directory.
*)

let dns_req_delete ~server
                   ~priv
                   ~name
                   =

    try 
    begin
        let super = server.dns_super in
    
        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;


#ifdef DEBUG
        Db.Pr.ss debug "dns_DELETE:" "Ok";
#endif        

        match (dns_search_row ~dir:dir ~name:name) with 
        | Some row  -> 
        begin
            dns_delete_row ~server:server
                                      ~dir:dir 
                                      ~row:row;
            let stat = release_dir ~server:server ~dir:dir in
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end;
        | None      -> 
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err


(*
** Replace a capability set.  The name and new capability set is specified.
*)

let dns_req_replace ~server
                ~priv
                ~name
                ~newcs
                =
    try
    begin
        let super = server.dns_super in
    
        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;


        match (dns_search_row ~dir:dir ~name:name) with 
        | Some row  -> 
        begin
            row.dr_capset <- newcs;

            dir_modified dir;
            let stat = server.dns_modify_dir ~dir:dir in
            if (stat <> std_OK) then
            begin
                ignore(release_dir ~server:server ~dir:dir);
                raise (Error stat);
            end;
            let stat = release_dir ~server:server ~dir:dir in
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end;
        | None      -> 
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err
(*
** Rename a directory entry.
*)

let dns_req_rename ~server
                ~priv
                ~oldname
                ~newname
                =
    try
    begin
        let super = server.dns_super in
    
        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;


        match (dns_search_row ~dir:dir ~name:oldname) with 
        | Some row  -> 
        begin
            row.dr_name <- newname;

            dir_modified dir;
            let stat = server.dns_modify_dir ~dir:dir in
            if (stat <> std_OK) then
            begin
                ignore(release_dir ~server:server ~dir:dir);
                raise (Error stat);
            end;
            let stat = release_dir ~server:server ~dir:dir in
            if (stat <> std_OK) then
                raise (Error stat);
            std_OK
        end;
        | None      -> 
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err

(*
** Set the live time of a directory to the maximal value.
*)


let dns_req_touch ~server
                  ~priv
                  =
    let super = server.dns_super in
    let stat,dir = request_dir ~server:server
                               ~priv:priv
                               ~req:(Rights_bits dns_RGT_MOD) in
    
    if (stat = std_OK) then
    begin
        server.dns_touch dir;
        let stat = release_dir ~server:server ~dir:dir in
        stat
    end
    else
        stat                

(*
** Age all objects from this server. Destroy objects with live time 
** = 0.
** Only allowed with the super capability (obj = 0).
**
*)

let dns_req_age ~server
                ~priv
    =
    let super = server.dns_super in
    let gone = ref 0 in

    try
    begin

        let rights = prv_rights priv in
        let Objnum obj = prv_number priv in

        if (obj <> 0 ||
            (prv_decode ~prv:priv
                        ~rand:super.dns_checkfield) = false ||
            (rights = prv_all_rights) = false) then
        begin
            raise (Error std_DENIED);
        end;

        (*
        ** Age all objects (used and unused inodes). Destroy directories
        ** with live time equal zero (only valid files). Remember that
        ** teh root dirctory (obj=1) can't be aged or deleted.
        *)


        for i = 2 to super.dns_ndirs-1
        do
            let used,time = server.dns_age ~obj:i in
            if (used = true && time = 0) then
            begin
                let stat,dir = acquire_dir ~server:server ~obj:i in
                if (stat <> std_OK) then
                begin
                    (*
                    ** Maybe already deleted AFS object.
                    *)
                    sys_log Sys_warn 
                            "DNS: Warning: dns_req_age: acquire failed for inode %d: %s\n" 
                            i (err_why stat); 
                    let stat = server.dns_delete_inode ~obj:i in
                    if (stat <> std_OK) then
                        raise (Error stat);
                    incr gone;
                end
                else
                begin
                    let stat = server.dns_delete_dir ~dir:dir in
                    if (stat <> std_OK) then
                    begin
                        sys_log Sys_err
                                "DNS: dns_req_age: del failed: %d\n" i;
                        ignore(release_dir ~server:server ~dir:dir);
                    end;
                    ignore(release_dir ~server:server ~dir:dir);
                    incr gone;
                end;                
            end;
        done; 
        sys_log Sys_info "DNS: dns_req_age: %d objects destroyed!\n" (!gone);
        std_OK
    end
    with
        | Error err -> err

(*
** Return the rights masks in a row.
*)


let dns_req_getmasks ~server
                     ~priv
                     ~name
                     =
    try
    begin                     
        let super = server.dns_super in
    
        let stat,dir = request_dir ~server:server
                                   ~priv:priv
                                   ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        match (dns_search_row ~dir:dir ~name:name) with 
        | Some row  -> 
        begin
            ignore(release_dir ~server:server ~dir:dir);
            std_OK,row.dr_columns
        end;
        | None      -> 
        begin
            ignore(release_dir ~server:server ~dir:dir);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err,[||]
                     

(*
** Lookup rownames in a set of directories. The 'dirs' argument
** is a list of (dir_cs,rowname) tuples. Return the resolved rows list with
** (status,time,capset). Always, in the case of failed partial lookups, too,
** all directory entries must be lookuped.
*)

let dns_req_setlookup ~server
                      ~dirs
                      =
    let super = server.dns_super in
    
    let stat = ref std_OK in
    let rows = ref [] in 
    
    let rec lookup dl =
        match dl with 
        | hd::tl ->
        begin
            let dir_cs,name = hd in
            let stat',priv = get_dir ~server:server
                                     ~dir_cs:dir_cs
                                     in
            stat := stat';

            if (stat' = std_OK) then
            begin
#ifdef DEBUG
                Db.Pr.ss debug "SETLOOKUPi" "Ok1";
#endif                
                let stat',dir = request_dir ~server:server
                                            ~priv:priv
                                            ~req:(Rights_bits 0)
                                            in
                let Rights_bits colmask = prv_rights priv in
                
                stat := stat';
                
                if (stat' = std_OK) then
                begin
#ifdef DEBUG
                Db.Pr.ss debug "SETLOOKUPi" "Ok2";
#endif
                  (*
                  ** Find the row
                  *)
                                                          
                  match (dns_search_row ~dir:dir ~name:name) with 
                  | Some row ->
                  begin
#ifdef DEBUG
                    Db.Pr.ss debug "SETLOOKUPi" "Ok3";
#endif

                    (*
                    ** calculate the rights mask
                    *)
                    let have_rights = ref 0 in
                    let ncols = dir.dd_ncols in
                    let cols = row.dr_columns in
                    
                    for i = 0 to ncols-1
                    do
                        let Rights_bits coli = cols.(i) in 
                        let Rights_bits colbits = dns_col_bits.(i) in

                        if (colmask land colbits = colbits) then
                            have_rights := !have_rights lor coli;
                    done;
                    stat := std_OK;

#ifdef DEBUG
                    Db.Pr.sd debug "SETLOOKUPi have" (!have_rights);
#endif                    
                    let time = row.dr_time in
                    let stat',cs_restr = dns_restrict 
                                                ~server:server
                                                ~cs:row.dr_capset 
                                                ~mask:(Rights_bits
                                                        !have_rights)
                                                in
                                                                             
                    ignore(release_dir ~server:server ~dir:dir);

                    stat := stat';
                    if (stat' = std_OK) then
                        rows := !rows @ [ !stat,
                                          time,
                                          cs_restr
                                        ]
                    else
                        rows := !rows @ [!stat,0,nilcapset];
                                                             
                  end;

                  | None -> 
                  begin
                    ignore(release_dir ~server:server ~dir:dir);
                    stat := std_NOTFOUND;
                    rows := !rows @ [!stat,0,nilcapset];
                  end;
                end
                else
                begin
                    ignore(release_dir ~server:server ~dir:dir);
                    rows := !rows @ [!stat,0,nilcapset];
                end;
            end
            else
                rows := !rows @ [!stat,0,nilcapset];

            lookup tl;                    
        end;
        | [] -> stat := std_OK;
    in                      
    lookup dirs;
    !rows


    
(*
** TODO: 
**       dns_INSTALL
**       dns_GETSEQNR
*)




