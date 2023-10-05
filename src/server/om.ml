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
**    $CREATED:     2003.11.23
**    $VERSION:     0.31
**
**    $INFO:
**
**  Object manager server -> Garbage collection
**
**    $ENDOFINFO
**
*)

open Amoeba
open Stderr
open Stdcom
open Stdcom2
open Dir
open Printf
open Name
open Syslog




(*
** Setup configuration
*)
type om_config =
    | Om_root   of string               (* root directory path          *)
    | Om_root_cap   of string           (* same, but cap. specified     *)
    | Om_ignore of string               (* ignore this path             *)
    | Om_ignore_cap of string           (* same, but cap. specified     *)
    | Om_age of string                  (* age server spec. with path   *)
    | Om_age_cap of string              (* same, but cap. specified     *)
    | Om_mintouch of int                (* 
                                        ** min. successfull touched
                                        ** objects required 
                                        ** before aging is started 
                                        *)
    | Om_passnum of int


type om = {
    mutable om_root: capability;            (* start root dir           *)
    mutable om_ignore: capability list;     (* dirs to ignore           *)
    mutable om_age: capability list;        (* age this servers         *)
    mutable om_mintouch: int;               (* min touched to start age *)
    mutable om_touched: int;                (* touched objects          *)
    mutable om_failed: int;                 (* not rechaed objects      *)
    mutable om_report: string;              (* report string            *)
    mutable om_passnow: int;   
    mutable om_passnum: int;             
}

(*
** Initialize om from configuration data.
*)

let om_init defs =
    let om = {
            om_root=nilcap;
            om_ignore=[];
            om_age=[];
            om_mintouch=1000;
            om_touched=0;
            om_failed=0;
            om_report="";
            om_passnow=0;
            om_passnum=1;
        } in
    List.iter (fun de ->
        match de with
        | Om_root p ->
            let stat,cap=name_lookup p in
            if (stat = std_OK) then
                om.om_root <- cap
            else
                sys_log Sys_info "OM: om_init (Om_root): name_lookup %s failed: %s\n"
                        p (err_why stat);
        | Om_root_cap p ->
            om.om_root <- Ar.ar_tocap p;
        | Om_ignore p ->
            let stat,cap=name_lookup p in
            if (stat = std_OK) then
                om.om_ignore <- om.om_ignore @ [cap]
            else
                sys_log Sys_info "OM: om_init (Om_ignore): name_lookup %s failed: %s\n"
                        p (err_why stat);
            
        | Om_ignore_cap p ->
            om.om_ignore <- om.om_ignore @ [Ar.ar_tocap p];
        | Om_age p ->
            let stat,cap=name_lookup p in
            if (stat = std_OK) then
                om.om_age <- om.om_age @ [cap]
            else
                sys_log Sys_info "OM: om_init (Om_age): name_lookup %s failed: %s\n"
                        p (err_why stat);
            
        | Om_age_cap p ->
            om.om_age <- om.om_age @ [Ar.ar_tocap p];
        | Om_mintouch i -> om.om_mintouch <- i;
        | Om_passnum i -> om.om_passnum <- i;
        ) defs;
    if (om.om_root=nilcap) then
        failwith "om_init: no root specified!";
    om



(*
** Walk a directory tree and touch all rechable objects.
** We must be aware of recursive directory structures!
*)

let om_walk_dir om =

    om.om_touched <- 0;
    om.om_failed <- 0;
    om.om_passnow <- om.om_passnow + 1;
    (*
    ** All directories already visited (to avoid directory recursion)
    ** are stored in a hash to lookup quickly entries. The key is the 
    ** directory capability, and the stored data is filled with
    ** the std_info status returned from the server (if any).
    ** In the case we don't get the info string from the server, we can't
    ** determine the object type. All these objects are stored in
    ** the directory hash, too. They are all ignored to speed up 
    ** directory walking.
    *) 

    let dirs = Hashtbl.create 100 in

    (*
    ** touch one object
    *)
    let touch cap =
        let stat = std_touch cap in
        if (stat <> std_OK) then
        begin
            sys_log Sys_info "OM: om_walk_dir: touch failed: %s (%s)\n"
                          (Ar.ar_cap cap) (err_why stat); 
            om.om_failed <- om.om_failed+1;
        end
        else
        begin
            om.om_touched <- om.om_touched+1;
        end;
        in
    (*
    ** get info for one object (dirctory or not?)
    *)
    let info cap =
        let stat,str = std_info cap 10 in
        let isdir = if (stat <> std_OK || 
                        stat = std_OK && str.[0] <> '/') then
                        false
                    else
                        true
            in
        if (stat <> std_OK) then
            om.om_failed <- om.om_failed+1;

        stat,isdir
        in

    (*
    ** Add a stripped down directory or unknown cap to the directory cap 
    ** hash.
    *)
    let dir_add cap stat =
        (* 
        ** Save only port and object number!
        *)
        let cap' =  {
                        cap_port = cap.cap_port;
                        cap_priv = {
                                      prv_object = cap.cap_priv.prv_object;
                                      prv_rights = Rights_bits 0;
                                      prv_random = nilport;
                                   }
                    }
            in
        Hashtbl.add dirs cap' stat
        in


    (*
    ** directory object already served ?
    *)
    let dir_done cap =
        let cap' =  {
                        cap_port = cap.cap_port;
                        cap_priv = {
                                      prv_object = cap.cap_priv.prv_object;
                                      prv_rights = Rights_bits 0;
                                      prv_random = nilport;
                                   }
                    }
            in
        (Hashtbl.mem dirs cap')
        in

    let rec walk dir =
        (*
        ** Walk down a dirctory ... 
        *)
        let stat,ds = dir_open dir in
            
        if (stat = std_OK && ds.dir_nrows > 0) then
            Array.iter (fun de ->
                    let stat,dc = dir_lookup dir de.dr_name in
                    if (stat = std_OK) then
                    begin
                        let stat,isdir = info dc in
                        if (stat = std_OK) then
                            touch dc;
                        (*
                        ** Server not reachable ?
                        *)
                        if (stat<>std_OK) then  
                            dir_add dc stat;

                        
                        if (isdir = true && (dir_done dc)=false) then
                        begin
                            dir_add dc stat;
                            walk dc;
                        end 
                        else if (isdir) then
                        begin
                            sys_log Sys_info "OM: om_walk_dir: ignoring %s\n"
                                    (Ar.ar_cap dc); 
                        end;
                    end
                    else
                    begin
                        sys_log Sys_info "OM: om_walk_dir: dir_lookup of %s failed: %s (%s)\n"
                                  de.dr_name
                                  (Ar.ar_cap dc) (err_why stat); 
                    end;    
                ) ds.dir_rows
        else
        begin
            sys_log Sys_info "OM: om_walk_dir: dir_lookup failed: %s (%s)\n"
                     (Ar.ar_cap dir) (err_why stat); 
        end;    
        in

    let report () =
            sys_log Sys_info 
            "-------------------------------- OM ------------------------------------\n"; 
            sys_log Sys_info "End of Pass %d:\n" om.om_passnow; 
            sys_log Sys_info "Touched: %8d    Not reached: %8d\n" (om.om_touched) 
                                                         (om.om_failed); 
        in

    (*
    ** Add exception capability list to dir hash (they should not be
    ** served).
    *)
    if (om.om_ignore <> []) then
        List.iter (fun dc ->        
            dir_add dc std_OK;
        ) om.om_ignore;
    try
    begin
        let stat,isdir= info om.om_root in
        if (stat = std_OK && isdir=true) then
        begin
            sys_log Sys_info
            "================================ OM ====================================\n"; 
            sys_log Sys_info "Touching:\n";
            dir_add om.om_root stat;
            touch om.om_root;
            walk om.om_root;
            report ();
            sys_log Sys_info
            "========================================================================\n"; 
            std_OK
        end
        else
        begin
            report ();
            stat
        end;
    end
    with
        | Error stat -> 
        begin
            report ();
            stat
        end

(*
** Start aging of specified servers
*)

let om_age om =
    sys_log Sys_info
    "\n================================ OM ====================================\n"; 
    sys_log Sys_info "Aging:\n"; 
    if (om.om_touched < om.om_mintouch) then
    begin
        sys_log Sys_info "om_age: no aging done (to less touched objects %d (min. %d))\n\n"
                om.om_touched  om.om_mintouch; 
        std_SYSERR
    end
    else if (om.om_age <> []) then
    begin
        List.iter (fun s ->
                sys_log Sys_info "om_age: Doing aging on %s ...\n" (Ar.ar_cap s); 
                let stat = std_age s in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_info "om_age: aging for %s failed: %s\n\n"
                        (Ar.ar_cap s) (err_why stat);
                end;                
            ) om.om_age;
        sys_log Sys_info 
        "========================================================================\n\n"; 
        std_OK
    end
    else
    begin
        sys_log Sys_info "om_age: no aging done (no server specified)\n\n";
        sys_log Sys_info
        "========================================================================\n\n"; 
        std_OK
    end

(*
** Om main loop
*)

let om_loop om =
    let dying = ref false in

    try
    begin
        if (om.om_passnum > 0) then
        for i = 1 to om.om_passnum
        do
            if (!dying = true) then
                raise Exit;
            ignore(om_walk_dir om);
            if (!dying = true) then
                raise Exit;
            ignore(om_age om);
        done
        else
        while (!dying = false) 
        do
            ignore(om_walk_dir om);
            if (!dying = true) then
                raise Exit;
            ignore(om_age om);
        done;
        std_OK
    end
    with | Exit -> std_OK
