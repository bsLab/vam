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
**    $INITIAL:     (C) 2004
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.10
**
**    $INFO:
**
**  Kernel Boot directory and disk module server implementation.
**  This server module emulates a DNS/AFS interface for the kernel
**  boot partition holding kernels, binaries needed for bootstrap
**  purposes and configuation files with a very simple
**  filesystem.
**
**    $ENDOFINFO
**
*)

(* #define DEBUG   *)

open Amoeba
open Bytebuf
open Stderr
open Disk_common
open Disk_client
open Afs_common
open Dns_common
open Bootdir
open Bootdisk_common
open Mutex
open Thread
open Machtype 
open Capset


(*
***************************************************************************
** Object AFS interface
***************************************************************************
*)

(*
** Acquire and lock a file with object number 'obj'. A
** release_file call must follow this operation.
*)

let bd_acquire_file ~disk ~obj =
    
    try 
    begin
        mu_lock disk.bdisk_lock;
    
        (*
        ** Find the object number
        *)
        let rec iter fl =
            match fl with
            | file::tl ->
                let Objnum obj' = prv_number file.boot_obj in
                if obj' = obj then
                begin
                    mu_lock file.boot_lock;
                    mu_unlock disk.bdisk_lock;
                    (std_OK,file);
                end
                else
                    iter tl;
            | [] -> 
                mu_unlock disk.bdisk_lock;
                (std_NOTFOUND,nilboot);
            in
        iter disk.bdisk_table;
    end
    with
        | Error err -> mu_unlock disk.bdisk_lock; 
                       err,nilboot
        
(*
** Release an acquired file. 
*)

let bd_release_file ~disk ~file =
    try
    begin
        if (file = nilboot) then
            raise (Error std_SYSERR);
        mu_unlock file.boot_lock;        
        std_OK
    end
    with
        | Error err -> err

(*
** Return a free objnum in the AFS table (index). 
** Note: protect this function with the server lock untill
** the file creation is finished.
*)

let bd_get_freeobjnum disk =
    (*
    ** Find highest object number
    *)
    let objmax = ref 0 in
    List.iter (fun file -> 
                let Objnum obj = prv_number file.boot_obj in
                objmax  := max !objmax obj;
        ) disk.bdisk_table;
    (!objmax + 1)
(*
** Free block region management.
** Free block region are sorted with increasing start address.
*)

(*
** Return a new free block region of size blocks from freeblocklist.
*)
let bd_get_freeblock disk size =
    (*
    ** Find the largest one.
    *)

    let maxfb = ref (0,0) in
    List.iter (fun fb ->
            let start',size' = fb in
            let start'',size'' = !maxfb in
            if size' > size'' then maxfb := fb;
        ) disk.bdisk_freeblocks;
    let start',size' = !maxfb in

#ifdef DEBUG
    Db.Pr.sddd 1 "bd_get_freeblock: maxfb size,bstart,bsize" size start' size';
#endif

    if size' < size then std_NOSPACE,0
    else
    begin
        let newfreel = ref [] in
        List.iter (fun fb ->
            if fb = !maxfb then
            begin
                let start'',size'' = fb in
                if (size''-size) > 0 then
                begin
#ifdef DEBUG
                Db.Pr.sdd 1 "bd_get_freeblock: found maxfb,start''+size,size''-size"
                (start''+size) (size''-size);
#endif
                    
                    newfreel := !newfreel @ [start''+size,size''-size];
                end;
            end
            else
                newfreel := !newfreel @ [fb];
            ) disk.bdisk_freeblocks;
        disk.bdisk_freeblocks <- !newfreel;
        std_OK,start'
    end

(*
** Try to extend an already used block region on disk
*)
let bd_extend_block disk start oldsize newsize =

#ifdef DEBUG
    Db.Pr.sddd 1 "bd_extend_block: start oldsize newsize" 
    start oldsize newsize;
#endif

    let tmpfl = ref [] in
    let rec iter fl =
        match fl with
        | fb::tl ->
        begin
            let start',size' = fb in
            if start' = (start+oldsize) then
            begin
                (*
                ** Check size of free block:
                *)
                if size' >= (newsize-oldsize) then
                begin
                    (*
                    ** Perfect, like jens said...
                    *)
                    let fb' = start'+(newsize-oldsize),
                              size'-(newsize-oldsize) in
                    disk.bdisk_freeblocks <- !tmpfl @ [fb'] @ tl;
                    std_OK
                end
                else
                    std_NOSPACE;
            end
            else
            begin
                tmpfl := !tmpfl @ [fb];
                iter tl;
            end;
        end;
        | [] -> std_NOSPACE;
        in
    if newsize > oldsize then
        iter disk.bdisk_freeblocks
    else
        std_OK

(*
** Add block region to freeblock list. Try to merge block regions.
*)

let bd_add_freeblock disk start size =
    let newfreel = ref [] in
    let rec insert fl =
            match fl with
            | fb::tl ->
                let start',size' = fb in
                if start >= (start'+size') then
                    newfreel := !newfreel @ [fb;(start,size)] @ tl
                else
                    insert tl;
            | [] -> (*
                    ** We must insert free block at head of list!
                    *)
                    newfreel := [(start,size)] @ disk.bdisk_freeblocks;
        in

    insert disk.bdisk_freeblocks;

    let newfreel' = ref [] in
    let last = ref (0,0) in
    let rec merge fl =
        match fl with
        | fb::tl ->
            let start',size' = fb in
            let start'',size'' = !last in
            if !last <> (0,0) &&
               (start''+size'') = start' then
            begin
                last := (start'',size'+size'');
                merge tl;
            end
            else if !last <> (0,0) then
            begin
                newfreel' := !newfreel' @ [!last];
                last := fb;
                merge tl;
            end
            else
            begin
                last := fb;
                merge tl;
            end;
        | [] -> if !last <> (0,0) then
                    newfreel' := !newfreel' @ [!last];
        in

    merge !newfreel;
    disk.bdisk_freeblocks <- !newfreel'

(*
** Build freeblock list of bootdisk
*)

let bd_freeblock_list disk =
    let bl = ref [] in
    (*
    ** Build first used block list and sort it with respect to
    ** start block address.
    *)
    List.iter (fun file ->
            bl := !bl @ [file.boot_start,file.boot_size];
        ) disk.bdisk_table;
    bl := List.sort (fun a b -> let st1,si1 = a in
                                let st2,si2 = b in
                                if st1 > st2 then 1 
                                else if st1 = st2 then 0
                                else (-1);
                    ) !bl;

    (*
    ** Not: block 0 is reserverd for our bootdisk table!
    *)

    let last = ref (1,0) in
    let fbl = ref [] in
    List.iter ( fun b ->
            let bst,bsi = b in
            let lst,lsi = !last in
            if (bst-(lst+lsi)) > 0 then 
                fbl := !fbl @ [lst+lsi,bst-(lst+lsi)];
            last := b;
        ) !bl;
    (*
    ** Get the size of the virtual disk used for bootdisk
    *)
    let stat,infol = disk_info disk.bdisk_vcap in
    if stat <> std_OK 
        then stat
    else
    begin
        let lst,lsi = !last in
        let info = List.hd infol in
        let size = to_int info.disk_numblks in
        if (size-(lst+lsi)) > 0 then
            fbl := !fbl @ [lst+lsi,size-(lst+lsi)];
        disk.bdisk_freeblocks <- !fbl;
        std_OK
    end

(*
** return order number for a new created file object. must be
** (current highest order # + 1)
*)
let bd_order disk =
    let omax = ref 0 in
    List.iter (fun f -> omax := max !omax f.boot_order) disk.bdisk_table;
    (!omax + 1)

(*
** AFS server size request.
**
** Args:
**  disk: the disk server structure
**  priv:   the request private field
**
** Return:
**  status
**  size [bytes]
*)

let bd_file_size
        ~disk
        ~priv
    =
    try
    begin
        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in
        let rights = prv_rights priv in

        let stat,af = bd_acquire_file ~disk:disk ~obj:obj in 

        if stat <> std_OK then raise (Error stat);

#ifdef DEBUG
        Db.Pr.sdd 2 "bd_file_size,bsize,fsize " af.boot_size af.boot_file_size;
#endif
        
        let size = if af.boot_file_size <> 0  
                    then af.boot_file_size (* only available if fresh created *)
                    else af.boot_size * disk.bdisk_blksize  (* blk rouned *) in

        ignore(bd_release_file ~disk:disk ~file:af);
        std_OK,size;
    end
    with
        | Error err -> err,0

    
(*
** AFS server read request.
**
** Args:
**  disk:   the server structure
**  priv:   the request private field
**  buf:    the read buffer
**  size:   the requested size (<= file size)   [bytes]  
**  off:    the file offset (<= file size)  [bytes]
**
** Return:
**  status
*)

let bd_file_read ~disk
                ~priv
                ~buf
                ~size
                ~off =
    let blksize = disk.bdisk_blksize in

    try
    begin
        (*
        ** Acquire the file.
        *)

        let Objnum obj = prv_number priv in
        let rights = prv_rights priv in

#ifdef DEBUG
        Db.Pr.sddd 2 "bd_file_read [obj,off,size]"
                 obj off size;
#endif
        let stat,af = bd_acquire_file ~disk:disk ~obj:obj in 

        if stat <> std_OK then raise (Error stat);

        let filesize = if af.boot_file_size > 0 
                            then af.boot_file_size
                            else af.boot_size*blksize in

        if (off > filesize) then
        begin
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_ARGBAD);
        end;

        (*
        ** Huuh, some clients may be request more bytes than
        ** the file has. Fix it here.
        *)
        let size =
            if (size+off > filesize) then (filesize-off) else size in


        if   ((prv_decode ~prv:priv ~rand:af.boot_obj.prv_random) = false ||
              (rights_req rights [afs_RGT_READ])= false) then
        begin    
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_DENIED);                                        
        end;

        (*
        ** Okay, do the read operation. 
        *)

        let stat = disk.bdisk_read ~disk:disk ~file:af
                             ~foff:off
                             ~size:size
                             ~buf:buf
                             ~boff:0 in
        if (stat <> std_OK) then
        begin
                ignore(bd_release_file ~disk:disk ~file:af);
                raise (Error stat);                            
        end;
        ignore(bd_release_file ~disk:disk ~file:af);
        std_OK,size;
    end
    with
        | Error err -> err,0


(*
** AFS server create request.
** Create a new file.
**
** Args:
**  disk: the disk
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

let bd_create 
        ~disk
        ~priv
        ~buf
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in

        (*
        ** Acquire the 'parent' if any.
        *)

        let Objnum obj' = prv_number priv in

#ifdef DEBUG
        Db.Pr.sddd 2 "bd_create [obj,size,commit]"
                 obj' size commit;
#endif

        if (obj' > 1) then
        begin
            (*
            ** It's a file.
            *)
            let stat,af = bd_acquire_file ~disk:disk ~obj:obj' in 

            if (stat <> std_OK) then
                raise (Error stat);

            if ((prv_decode ~prv:priv
                            ~rand:af.boot_obj.prv_random) = false ||
                (rights_req rights [afs_RGT_CREATE]) = false ) then
            begin
                ignore(bd_release_file ~disk:disk ~file:af);
                raise (Error std_DENIED);
            end;
            ignore(bd_release_file ~disk:disk ~file:af);
        end
        else
        begin
            
            if ((prv_decode ~prv:priv 
                            ~rand:disk.bdisk_file_chkport) = false  ||
                (rights_req rights [afs_RGT_CREATE]) = false) then
            begin
                raise (Error std_DENIED);
            end; 
        end;

        (*
        ** Check limits of directory table.
        *)
        if (List.length disk.bdisk_table) = disk.bdisk_nfiles
            then raise (Error std_NOSPACE);

        (*
        ** First get a new object number
        *)
        let obj = bd_get_freeobjnum disk in

        (*
        ** up rounded size in blocks - we must reserve at least one block!
        *)
        let bsize = if size = 0 then 1
                    else (size+disk.bdisk_blksize-1) / disk.bdisk_blksize in

        let stat,bstart = bd_get_freeblock disk bsize in

#ifdef DEBUG
        Db.Pr.sdd 1 "bd_create,bstart,bsize" bstart bsize;
#endif

        if stat <> std_OK then raise (Error stat);

        let af = {
                boot_lock = mu_create ();
                boot_name = "";
                boot_order = bd_order disk;
                boot_obj = prv_encode ~obj:(Objnum obj)
                                      ~rights:prv_all_rights
                                      ~rand:disk.bdisk_file_chkport;
                boot_start = bstart;
                boot_size = bsize;
                boot_file_size = size;
                boot_cols = [|prv_all_rights;
                              prv_all_rights;
                              prv_all_rights|];
                boot_flag = if commit > 0 
                                then Boot_locked 
                                else Boot_unlocked;
                
                boot_live = bootdisk_MAXLIVE;
            } in

        mu_lock af.boot_lock;
        (*
        ** Append new file at end of disk table. 
        *)
        disk.bdisk_table <- disk.bdisk_table @ [af];


        let stat =
            if (size > 0) then
            begin 
                disk.bdisk_write  ~disk:disk
                                  ~file:af
                                  ~foff:0
                                  ~size:size
                                  ~buf:buf
                                  ~boff:0;
            end
            else
                std_OK
            in                     

        if (stat <> std_OK) then
        begin
            mu_unlock af.boot_lock;
            raise (Error stat);
        end;

        let stat = bd_release_file ~disk:disk ~file:af in

        if (stat <> std_OK) then
        begin
            mu_unlock af.boot_lock;
            raise (Error stat);
        end;

        let cap = {cap_port = disk.bdisk_file_pubport;
                   cap_priv = af.boot_obj;} in
        (std_OK,cap)
    end
    with
        | Error err -> (err,nilcap)
        

(*
** AFS server Modify request.
**
** Args:
**  disk: the disk structure
**  priv:   the request private field
**  buf:    the write buffer
**  size:   the requested size    [bytes]
**  off:    the file offset   [bytes]
**  commit: the commit flag
**
** Return:
**  status
**  newcap      (Boot_unlocked -> oldcap)
*)

let bd_modify
        ~disk
        ~priv
        ~buf
        ~off
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in

        (*
        ** Acquire the file.
        *)
        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sdddd 2 "bd_modify [obj,off,size,commit]"
                 obj off size commit;
        Db.Pr.ss 1 "bd_modify: prv" (Ar.ar_priv priv);
#endif


        let stat,af = bd_acquire_file ~disk:disk ~obj:obj in 
        if (stat <> std_OK) then
            raise (Error stat);

        if (size < 0 ||
            off < 0) then
        begin
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_ARGBAD);                            
        end;
        af.boot_live <- bootdisk_MAXLIVE;
        (*
        ** Four different cases:
        **
        **  1. Request authorized/not authorized
        **  2. stat = Boot_locked, Boot_unlocked
        **  3. Invalid arguments (off,size)
        **  4. size = 0 (only commit)
        **
        *)
        if ((prv_decode ~prv:priv ~rand:af.boot_obj.prv_random) = false ||
            (rights_req rights [afs_RGT_MODIFY]) = false) then
        begin    
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_DENIED);                            
        end;

        if (size = 0 && (af.boot_flag = Boot_locked ||
                         af.boot_flag = Boot_named)) then
        begin
            (*
            ** Only unlocked files can be committed!
            ** Or must we create a copy here?
            *)
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_ARGBAD);
        end;


        if (size = 0 && af.boot_flag = Boot_unlocked) then
        begin
            
            if commit > 0 then
            begin
                (*
                ** Only commit the file. Simple.
                *)
                af.boot_flag <- Boot_locked;
            end;

            let cap = { cap_port = disk.bdisk_file_pubport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.boot_obj.prv_random; 
                } in
            ignore(bd_release_file ~disk:disk ~file:af);
            std_OK,cap
        end
        else 
        begin
#ifdef DEBUG
            Db.Pr.s 5 "bd_modify: auth ok & do modify";
#endif
            (*
            ** The file exists, and the request is authorized.
            *)

            (*
            ** Convert size and file offset to block units (up rounded)
            *)
            let blksize = disk.bdisk_blksize in
            let fbsize = (size+blksize-1)/blksize in
            let fboff = (off+blksize-1)/blksize in


            if (af.boot_flag = Boot_unlocked) then
            begin
                (*
                ** New data beyond the current file size ?
                *)
#ifdef DEBUG
                Db.Pr.s 5 "bd_modify: Boot_unlocked";
#endif

                let cap = {cap_port = disk.bdisk_file_pubport;
                           cap_priv = af.boot_obj;} in

                if af.boot_size < (fboff+fbsize)  then
                begin
                    (*
                    ** Try to extend current occupied block
                    ** fragment.
                    *)
                    let newsize = fboff+fbsize in
                    let stat = bd_extend_block disk af.boot_start
                                                    af.boot_size
                                                    newsize in
                    if stat = std_NOSPACE then
                    begin
                        (*
                        ** We must copy old content to new larger
                        ** block on disk.
                        *)
#ifdef DEBUG
                        Db.Pr.sd 1 "bd_modify: extension failed: newsize" 
                            newsize;
#endif
                        ignore(bd_release_file ~disk:disk ~file:af);
                        raise (Error std_NOTNOW);
                    end
                    else
                    begin
                        af.boot_size <- newsize;
                        af.boot_file_size <- off+size;
#ifdef DEBUG
                        Db.Pr.sddd 0 "bd_modify: bstart,new bsize,fsize" 
                            af.boot_start
                            af.boot_size
                            af.boot_file_size;
#endif
                    end;
                end;
                if af.boot_file_size < (off+size)  
                    then af.boot_file_size <- off+size;


                (*
                ** Copy user data.
                *)
                let stat = disk.bdisk_write ~disk:disk ~file:af 
                                            ~foff:off
                                            ~size:size
                                            ~buf:buf
                                            ~boff:0 in

                ignore(bd_release_file ~disk:disk ~file:af);
#ifdef DEBUG
                Db.Pr.s 5 "bd_modify: done";
#endif

                stat,(if stat = std_OK then cap else nilcap)
            end
            else
            begin
                (*
                ** Create a new file. Copy the content of the
                ** original one. Modify the new one.
                *)
                (*
                ** Not supported currently.
                *)
                ignore(bd_release_file ~disk:disk ~file:af);
                raise (Error std_NOTNOW);
            end;
        end;        
    end
    with
        | Error err -> 
#ifdef DEBUG
                Db.Pr.ss 5 "bd_modify: Error" (err_why err);
#endif
                err,nilcap

(*
** AFS server Insert request.
**
**
** Args:
**  disk: the disk struct
**  priv:   the request private field
**  buf:    the write buffer
**  size:   the requested size (<= file size)   [bytes]
**  off:    the file offset (<= file size)  [bytes]
**  commit: the commit flag
**
** Return:
**  status
**  newcap      (Boot_unlocked -> oldcap)
*)
let bd_insert
        ~disk
        ~priv
        ~buf
        ~off
        ~size
        ~commit
    =
    try
    begin
        let rights = prv_rights priv in

        (*
        ** Acquire the file.
        *)
        let Objnum obj = prv_number priv in

#ifdef DEBUG
        Db.Pr.sdddd 2 "bd_insert [obj,off,size,commit]"
                 obj off size commit;
#endif


        let stat,af = bd_acquire_file ~disk:disk ~obj:obj in 


        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let blksize = disk.bdisk_blksize in

        let filesize = if af.boot_file_size > 0
                        then af.boot_file_size
                        else af.boot_size*blksize in

        if (off > filesize) then
        begin
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_ARGBAD);
        end;

        (*
        ** There are four different cases:
        **
        **  1. Request authorized/not authorized
        **  2. stat = Boot_locked/_named, Boot_unlocked
        **  3. Invalid arguments (off,size)
        **  4. size = 0 (only commit)
        **
        *)
            
        if ((prv_decode ~prv:priv ~rand:af.boot_obj.prv_random) = false ||
            (rights_req rights [afs_RGT_MODIFY]) = false) then
        begin
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_DENIED);
        end;

        if (size = 0 && (af.boot_flag = Boot_locked ||
                         af.boot_flag = Boot_named) && commit > 0) then
        begin
            (*
            ** Only unlocked files can be committed!
            ** Or must we create a copy here?
            *)
            ignore(bd_release_file ~disk:disk ~file:af);
            raise (Error std_ARGBAD);
        end;

        if (size = 0 && af.boot_flag = Boot_unlocked) then
        begin
            if (commit > 0 ) then
            begin
                (*
                ** Only commit the file. Simple.
                *)
                af.boot_flag <- Boot_locked;
            end;

            let cap = { cap_port = disk.bdisk_file_pubport;
                        cap_priv = prv_encode ~obj:(Objnum obj)
                                          ~rights:prv_all_rights
                                          ~rand:af.boot_obj.prv_random; 
                } in
            ignore(bd_release_file ~disk:disk ~file:af);
            std_OK,cap
        end
        else 
        begin
#ifdef DEBUG
            Db.Pr.s 5 "bd_insert: auth ok & do insert";
#endif
            (*
            ** The file exists, and the request is authorized.
            *)

            let fboff = (off+blksize-1) / blksize in
            let fbsize = (size+blksize-1)/blksize in

            if (af.boot_flag = Boot_unlocked) then
            begin
                (*
                ** New data beyond the current file size ?
                *)
#ifdef DEBUG
                Db.Pr.s 5 "bd_insert: Boot_unlocked";
#endif

                let cap = {cap_port = disk.bdisk_file_pubport;
                           cap_priv = af.boot_obj;} in

                if af.boot_size < (fboff+fbsize)  then
                begin
                    (*
                    ** Try to extend current occupied block
                    ** fragment.
                    *)
                    let newsize = fboff+fbsize in
                    let stat = bd_extend_block disk af.boot_start
                                                    af.boot_size
                                                    newsize in
                    if stat = std_NOSPACE then
                    begin
                        (*
                        ** We must copy old content to new larger
                        ** block on disk.
                        *)
                        ignore(bd_release_file ~disk:disk ~file:af);
                        raise (Error std_NOTNOW);
                    end
                    else
                    begin
                        af.boot_size <- newsize;
                        af.boot_file_size <- off+size;
                    end;
                end;

            
                if (off < af.boot_file_size) then
                begin

                    (*
                    ** The data should be inserted in the middle
                    ** or at the beginning of the file. Therefore, we must
                    ** shift the data between this offset (off) and 
                    ** (off+size) to create the needed space.
                    *)
                    ignore(bd_release_file ~disk:disk ~file:af);
                    raise (Error std_NOTNOW);
                end;

                (*
                ** Now write the data to be inserted.
                *)
                let stat = disk.bdisk_write ~disk:disk
                                            ~file:af
                                            ~foff:off
                                            ~size:size
                                            ~buf:buf
                                            ~boff:0 in

                ignore(bd_release_file ~disk:disk ~file:af);

                stat,(if stat = std_OK then cap else nilcap)
            end
            else
            begin
                (*
                ** Create a new file. Copy the content of the
                ** original one. Modify the new one.
                *)
                (*
                ** Not supported currently.
                *)
                ignore(bd_release_file ~disk:disk ~file:af);
                raise (Error std_NOTNOW);
            end;
        end;        
    end
    with
        | Error err -> err,nilcap


(*
************************************************************************
** DNS part
************************************************************************
*)

let col_bits = [| Rights_bits 0x1;
                      Rights_bits 0x2;
                      Rights_bits 0x4;
                      Rights_bits 0x8;
                      Rights_bits 0x10;
                      Rights_bits 0x20;
                      Rights_bits 0x40;
                      Rights_bits 0x80 |]

let col_names = [|"owner";"group";"others"|]

(*
** Acquire and lock the root directory with object number 1. A
** release_dir call must follow this operation.
*)


let bd_acquire_dir ~disk ~obj =
    if obj <> 1 then std_CAPBAD 
    else
    begin
        mu_lock disk.bdisk_lock;
        std_OK
    end

(*
** Release an acquired directory. Flush all pending writes 
*)

let bd_release_dir ~disk =
    if disk.bdisk_sync then
    begin
        let blksize = disk.bdisk_blksize in
        let bdes = Array.create (List.length disk.bdisk_table)
                                {bde_start=word32 0;
                                 bde_size =word32 0;
                                 bde_name = String.create bde_NAMELEN} in
        (*
        ** Sort bootdisk table with respect to the
        ** order number of a file object.
        *)
        let sortl = List.sort (fun f1 f2 ->
                                if f1.boot_order > f2.boot_order 
                                    then 1
                                else if f1.boot_order < f2.boot_order
                                    then (-1)
                                else 0) disk.bdisk_table in
        
        let i = ref 0 in 
        List.iter (fun bd ->
                bdes.(!i) <- {
                            bde_start = word32 bd.boot_start;
                            bde_size  = word32 bd.boot_size;
                            bde_name  = bde_toname bd.boot_name;
                            };
                incr i;
            ) sortl;

        let bd = {
                    bd_magic = bd_MAGIC;
                    bd_entries = bdes;
                    bd_unused = word32 (bd_NENTRIES - 
                                            (List.length disk.bdisk_table));
                } in
#ifdef DEBUG
        Db.Pr.sdd 1 "bd_relase_dir: sync,#bdisk_table,bd_unused"
                    (List.length disk.bdisk_table)
                    (to_int bd.bd_unused);
#endif

        let pos = buf_put_bd disk.bdisk_buf 0 bd in
        let stat = disk_write disk.bdisk_vcap
                              ~start:0
                              ~num:1
                              ~blksize:blksize
                              ~buf:disk.bdisk_buf
                              ~pos:0 in
        disk.bdisk_sync <- false;
        mu_unlock disk.bdisk_lock;
        stat
    end
    else
    begin
        mu_unlock disk.bdisk_lock;
        std_OK
    end


(*
** Find a row 'name' in the boot directory.
*)

let bd_search_row ~disk ~name =
    let rec iter fl =
        match fl with
        | file::tl  -> if file.boot_name = name 
                            then Some file
                            else iter tl;
        | [] -> None 
        in
    iter disk.bdisk_table            

(*
** Remove a row from a directory.
*)

let bd_delete_row ~disk ~row =
    let fl = ref [] in
    List.iter (fun file ->
            if file = row then
            begin
                bd_add_freeblock disk file.boot_start
                                       file.boot_size;
            end
            else
                fl := !fl @ [file];
        ) disk.bdisk_table;
    disk.bdisk_table <- !fl;
    disk.bdisk_sync <- true


(*
** A client request arrived.  Enter the critical section.  Check the capability.
** If correct, check whether the required rights are present or not.
** Return the directory structure, but only if the required rights are present.
*)

let bd_request_dir ~disk ~priv ~req =
    try
    begin
        let Objnum obj = prv_number priv in

        let stat = bd_acquire_dir ~disk:disk ~obj:obj in

        if (stat <> std_OK) then
            raise (Error stat);
        (*
        ** Validate the private field.
        *)

#ifdef DEBUG
        Db.Pr.ss 1 "acquire: random" (Ar.ar_port disk.bdisk_chkport);
#endif
    
        if ((prv_decode ~prv:priv
                        ~rand:disk.bdisk_chkport) = false) then
        begin
            ignore(bd_release_dir ~disk:disk); 
            raise (Error std_DENIED);
        end;          

        (* 
        ** When checking the presence of column rights, only take the
        ** *actual*
        ** columns present into acount (i.e., do not use dns_COLMASK here)
        *) 
        let ncols = 3 in
        let Rights_bits colbits = col_bits.(ncols) in
        let Rights_bits rights  = (prv_rights priv) in     
        let Rights_bits req     = req in
        let colmask = colbits - 1 in
                      
        if ((rights land colmask = 0) ||
            (rights land req <> req)) then
        begin
            ignore(bd_release_dir ~disk:disk);
            raise (Error std_DENIED);
        end;

        std_OK
    end
    with
        | Error err -> err


let bd_dns_restrict ~disk ~mask =
    (*
    ** Simple. There is only one directory managed by this server!
    ** The root.
    *)
    let rights = rights_and prv_all_rights mask in
    let cap = 
        {
        cap_port = disk.bdisk_pubport;
        cap_priv = prv_encode ~obj:(Objnum 1) 
                             ~rights:rights
                             ~rand:disk.bdisk_chkport;
        } in
    std_OK,(cs_singleton cap)

let bd_afs_restrict ~disk ~row ~mask =
    (*
    ** Simple. There are only files managed by this server!
    *)
    let rights = rights_and prv_all_rights mask in
    let obj = prv_number row.boot_obj in
    let rand = row.boot_obj.prv_random in
    let cap = 
        {
        cap_port = disk.bdisk_file_pubport;
        cap_priv = prv_encode ~obj:obj
                             ~rights:rights
                             ~rand:rand;
        } in
    std_OK,(cs_singleton cap)


(*
** dns_LOOKUP: 
** Traverse a path as far as possible, and return the resulting capability
** set and the rest of the path. 
*)

let bd_lookup 
            ~disk
            ~priv        (* hdr.cap_priv from client request *) 
            ~path        (* path name to lookup *)
            =
    try
    begin

#ifdef DEBUG
        Db.Pr.ss 1 "bd_lookup: path" path;
#endif
    
        let rights = prv_rights priv in
        let stat = bd_request_dir ~disk:disk
                               ~priv:priv
                               ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        (*
        ** User rights for this boot directory
        *)
        let Rights_bits rights = rights in
        let Rights_bits all_rights = prv_all_rights in
        let mask = rights land all_rights in

        let path = path_normalize path in
        let path_rel = Filename.is_relative path in
        let pathl = Str.split (Str.regexp "/") path in           
        let path_chunks = List.length pathl in

        (*
        ** Only "/bootobj" or "bootobj" paths are possible.
        *)
        if path_chunks > 1 then raise (Error std_ARGBAD);

        let rowname = List.nth pathl 0 in
        let file = 
            match (bd_search_row disk rowname) with
            | Some file -> file;
            | None -> ignore(bd_release_dir disk);
                      raise (Error std_NOTFOUND);
             in

        if file.boot_flag <> Boot_named 
            then raise (Error std_NOTFOUND);

        (*
        ** Build the restricted object capability set 
        *)

        let Rights_bits all_rights = prv_all_rights in

        (* 
        ** When checking the presence of column rights, only take
        ** the *actual* columns present into acount, 
        ** so do not use dns_COLMASK here.
        *)

        let ncols = 3 in
        let Rights_bits colbits = col_bits.(ncols) in
        let colrights = colbits - 1 in        
        let colmask = mask land colrights in

        (*
        ** Calculate the new have_rights value.
        ** All the columns rights from the current row
        ** are logical ored,if, and only if the i-th
        ** bit in the current colmask is set. 
        ** The i-th bit corresponds to the i-th rights column
        ** in the current row.
        *)

        let colmask' = ref 0 in
        let cols = 3 in

        for i = 0 to ncols - 1 
        do
            let Rights_bits coli = file.boot_cols.(i) in 
            let Rights_bits colbits = col_bits.(i) in
            (*
            ** If the i-th bit in colmask is set,
            ** add the i-th column of the row to
            ** the new colmask'.
            *)
                     
            if (colmask land colbits = colbits) then
                colmask' := !colmask' lor coli;
        done;


        let stat,cs_restr = bd_afs_restrict  ~disk:disk
                                        ~row:file
                                         ~mask:(Rights_bits !colmask') in

#ifdef DEBUG
        if (stat = std_OK) then
                    Db.Pr.ss 1 "bd_lookup,cs_restr" (
                        Ar.ar_cap (cs_restr.cs_suite.(0).s_object)
                    )
        else
                Db.Pr.ss 1 "bd_lookup restr " (err_why stat);
#endif

        ignore(bd_release_dir disk);
        std_OK,cs_restr,""
    end
    with 
        | Error err -> err,nilcapset,""

(*
** With a given directory capset check if this directory belongs
** to this server. 
*)
 
let get_dir ~disk ~dir_cs =
    let stat  = ref std_CAPBAD in
    let priv  = ref nilpriv in
    let putport = disk.bdisk_pubport in

    try
    (
        for i = 0 to dir_cs.cs_final - 1 
        do
            let s = dir_cs.cs_suite.(i) in
            if (s.s_current = true) then
            begin
                let cap = s.s_object in
                if ((portcmp cap.cap_port putport) = true &&
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
** Lookup rownames in a set of directories. The 'dirs' argument
** is a list of (dir_cs,rowname) tuples. Return the resolved rows list with
** (status,time,capset). Always, in the case of failed partial lookups, too,
** all directory entries must be lookuped.
*)
let bd_setlookup ~disk
                 ~dirs
                 =
    
    let stat = ref std_OK in
    let rows = ref [] in 
    
    let rec lookup dl =

        match dl with 
        | hd::tl ->
        begin
            let dir_cs,name = hd in
            let stat',priv = get_dir ~disk:disk
                                     ~dir_cs:dir_cs
                                     in
            stat := stat';

            if (stat' = std_OK) then
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "SETLOOKUPi" "Ok1";
#endif                
                let stat' = bd_request_dir ~disk:disk
                                            ~priv:priv
                                            ~req:(Rights_bits 0)
                                            in
                let Rights_bits colmask = prv_rights priv in
                
                stat := stat';
                
                if (stat' = std_OK) then
                begin
#ifdef DEBUG
                Db.Pr.ss 1 "SETLOOKUPi" "Ok2";
#endif
                  (*
                  ** Find the row
                  *)
                                                          
                  match (bd_search_row ~disk:disk ~name:name) with 
                  | Some row ->
                  begin
#ifdef DEBUG
                    Db.Pr.ss 1 "SETLOOKUPi" "Ok3";
#endif
                    if row.boot_flag = Boot_named then
                    begin

                        (*
                        ** calculate the rights mask
                        *)
                        let have_rights = ref 0 in
                        let ncols = 3 in
                        let cols = row.boot_cols in
                    
                        for i = 0 to ncols-1
                        do
                            let Rights_bits coli = cols.(i) in 
                            let Rights_bits colbits = col_bits.(i) in
    
                            if (colmask land colbits = colbits) then
                                have_rights := !have_rights lor coli;
                        done;
                        stat := std_OK;

#ifdef DEBUG
                        Db.Pr.sd 1 "SETLOOKUPi have" (!have_rights);
#endif                    
                        let time = 0 in
                        let stat',cs_restr = bd_afs_restrict 
                                                ~disk:disk
                                                ~row:row
                                                ~mask:(Rights_bits
                                                        !have_rights)
                                                in
                                                                             
                        ignore(bd_release_dir ~disk:disk);

                        stat := stat';
                        if (stat' = std_OK) then
                            rows := !rows @ [ !stat,
                                              time,
                                              cs_restr
                                            ]
                        else
                            rows := !rows @ [!stat,0,nilcapset];
                                                             
                    end
                    else
                    begin
                        ignore(bd_release_dir ~disk:disk);
                        stat := std_NOTFOUND;
                        rows := !rows @ [!stat,0,nilcapset];
                    end;
                  end;
                  | None -> 
                  begin
                    ignore(bd_release_dir ~disk:disk);
                    stat := std_NOTFOUND;
                    rows := !rows @ [!stat,0,nilcapset];
                  end;
                end
                else
                begin
                    ignore(bd_release_dir ~disk:disk);
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
** List a directory.  Returns a flattened representation of the number of
** columns, the number of rows, the names of the columns, the names of the
** rows and the right masks. 
** Return status,
**        the number of total rows and columns, 
**        the col names list,
**        the (dr_name,dr_columns) list starting with firstrow.
*)

let bd_list     ~disk
                 ~priv
                 ~firstrow
                 =
    try
    begin

#ifdef DEBUG
        Db.Pr.ss 1 "bd_list,obj" (Ar.ar_priv priv);
#endif

        let rights = prv_rights priv in
        let stat = bd_request_dir ~disk:disk
                               ~priv:priv
                               ~req:(Rights_bits 0) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;

        let ncols = 3 in
        let nrows = List.length disk.bdisk_table in
        let colnames = col_names in
        
        (*
        ** Build an row list starting with 'firstrow'.
        *)
        let rowlist = ref [] in
        
        (*
        ** Sort bootdisk table with respect to the
        ** order number of a file object.
        *)
        let sortl = List.sort (fun f1 f2 ->
                                if f1.boot_order > f2.boot_order
                                    then 1
                                else if f1.boot_order < f2.boot_order
                                    then (-1)
                                else 0) disk.bdisk_table in

        let rows = Array.of_list sortl in

        let rowlist = ref [] in
        
        if (nrows > 0) then
        for i = firstrow to nrows-1
        do

            if rows.(i).boot_flag = Boot_named then
                rowlist := !rowlist @ [(rows.(i)).boot_name,
                                       (rows.(i)).boot_cols];    
        done;

        ignore(bd_release_dir ~disk:disk);

#ifdef DEBUG
        Db.Pr.sdd 1 "bd_list,firstrow,nrows" firstrow nrows;
#endif

        (std_OK,(List.length !rowlist),ncols,colnames,!rowlist)
                 
    end
    with
        | Error err -> (err,0,0,[||],[])

(*
** Append a row to a directory. The name, right masks (cols), and initial
** capability must be specified.
*)
        
let bd_append  ~disk
                    ~priv
                    ~name
                    ~cols
                    ~capset
                    =
    try
    begin                        

        let stat = bd_request_dir ~disk:disk
                               ~priv:priv
                               ~req:(Rights_bits 0) in
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
        (*
        ** Only one cap of capset is handled here.
        *)
        let stat,cap = cs_to_cap capset in
        if (stat <> std_OK) then
        begin
            ignore(bd_release_dir ~disk:disk);
            raise (Error stat);
        end;
        (*
        ** Search the capability in our disk table.
        ** It must be already there, locked but not named.
        *)
        let capcmp known unknown =
            let c1 = known in
            let c2 = unknown in
            (portcmp c1.cap_port c2.cap_port) &&
            (prv_number c1.cap_priv) = (prv_number c2.cap_priv) &&
            (prv_decode c2.cap_priv c1.cap_priv.prv_random)
            in

        let rec search fl =
            match fl with
            | file::tl ->
            begin
                if (capcmp {cap_port = disk.bdisk_file_pubport;
                           cap_priv = file.boot_obj} cap) then
                begin
                    (*
                    ** The file must  be already locked. Why ?
                    *)
                    if file.boot_flag = Boot_unlocked then
                        raise (Error std_DENIED);

                    file.boot_name <- name;
                    file.boot_flag <- Boot_named;
                    disk.bdisk_sync <- true;
                    ignore(bd_release_dir ~disk:disk);
                    std_OK
                end
                else
                    search tl;
            end;
            | [] -> ignore(bd_release_dir ~disk:disk);
                    std_NOTFOUND;
            in
        search disk.bdisk_table;
    end
    with
        | Error err -> err


(*
** Delete a row within a directory.
*)

let bd_delete ~disk
                   ~priv
                   ~name
                   =
    try 
    begin
    
        let stat = bd_request_dir ~disk:disk
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
    
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;
#ifdef DEBUG
        Db.Pr.ss 1 "dns_DELETE:" "Ok";
#endif        

        match (bd_search_row ~disk:disk ~name:name) with 
        | Some row  -> 
        begin
            bd_delete_row ~disk:disk ~row:row;
            ignore(bd_release_dir ~disk);
            std_OK
        end;
        | None      -> 
        begin
            ignore(bd_release_dir disk);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err


(*
** Rename a directory entry.
*)

let bd_rename ~disk
                ~priv
                ~oldname
                ~newname
                =
    try
    begin
    
        let stat = bd_request_dir ~disk:disk
                                   ~priv:priv
                                   ~req:(Rights_bits dns_RGT_MOD) in
        if (stat <> std_OK) then
        begin
            raise (Error stat);
        end;


        match (bd_search_row ~disk:disk ~name:oldname) with 
        | Some row  -> 
        begin
            row.boot_name <- newname;
            disk.bdisk_sync <- true;
            bd_release_dir disk;        
        end;
        | None      -> 
        begin
            ignore(bd_release_dir disk);
            raise (Error std_NOTFOUND);
        end;
    end                                  
    with
        | Error err -> err


(*
** Garbage collector: remove outtimed files without flag Boot_named!
*)

let bootdisk_gc ~disk ~interval =
    while (true)
    do
        ignore(thread_delay interval SEC);
        let newfl = ref [] in
        mu_lock disk.bdisk_lock;
        List.iter (fun file ->
                mu_lock file.boot_lock;
                if file.boot_flag <> Boot_named 
                    then file.boot_live <- file.boot_live - 1;
                if file.boot_live = 0 then
                begin
                    (*
                    ** Timed out. Remove the file and return acquired blocks
                    ** to freeblock list.
                    *)                    
#ifdef DEBUG
                    Db.Pr.ss 1 "bootdisk_gc: file timed out" 
                                (Ar.ar_priv file.boot_obj);
#endif
                    bd_add_freeblock disk file.boot_start
                                          file.boot_size;

                    file.boot_file_size <- 0;
                    file.boot_flag <- Boot_invalid;
                end
                else
                    newfl := !newfl @ [file];
                mu_unlock file.boot_lock;
            ) disk.bdisk_table;
        disk.bdisk_table <- !newfl;
        mu_unlock disk.bdisk_lock;
    done    