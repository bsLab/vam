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
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:
**    $MODIFIED:
**    $VERSION:     1.12
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

(* #define DEBUG *)

open Amoeba
open Bytebuf
open Stderr 
open Disk_common
open Disk_client
open Afs_common 
open Dns_common 
open Bootdir    
open Bootdisk_common
open Bootdisk_server
open Bootdisk_server_rpc
open Mutex
open Thread
open Machtype
open Capset  
open Name
open Printf
open Thread
open Sema
open Stdcom
open Stdcom2

let fail str =
    print_string (sprintf "BDISK: %s" str);
    print_newline ();
    exit 1

let nl = print_newline 
let info str =
    print_string ("BDISK: "^str); nl ()

    
let init blocksize vdiskpath hostpath bootdiskname verbose =
    (*
    ** get the virtual disk capability 
    *)
    let stat,vcap = name_lookup vdiskpath in
    if stat <> std_OK then fail "can't lookup virtual disk";

    (*
    ** read the boot directory table
    *)
    let buf = buf_create blocksize in
    let stat = disk_read vcap 
                         ~start:0
                         ~num:1
                         ~blksize:blocksize
                         ~buf:buf
                         ~pos:0 in
    if stat <> std_OK then fail "can't read boot directory block of vdisk";
    let pos,bd = buf_get_bd buf 0 in
    if bd.bd_magic <> bd_MAGIC 
        then fail "vdisk contains no valid boot directory";

    let bdes = ref [] in
    let order = ref 0 in

    for i = 0 to (bd_NENTRIES-(to_int bd.bd_unused))-1
    do
        let bdi = bd.bd_entries.(i) in
        let obj = prv_encode ~obj:(Objnum (i+1))
                             ~rights:prv_all_rights
                             ~rand:(uniqport ()) in
        bdes := !bdes @ [{
                boot_lock = mu_create ();
                boot_name = bde_fromname bdi.bde_name;
                boot_order = !order;
                boot_obj = obj;
                boot_start = to_int bdi.bde_start;
                boot_size = to_int bdi.bde_size;    (* 512 bytes chunks! *)
                boot_file_size = 0;
                boot_cols = [|prv_all_rights;
                              prv_all_rights;
                              prv_all_rights|];
                boot_flag = Boot_named;
                boot_live = bootdisk_MAXLIVE;
            }];
        incr order;
    done;


    (*
    ** byte unit read/write functions
    ** 
    ** foff: file offset (relativ to boot_start) in bytes
    ** size: requested size in bytes
    ** boff: relative buffer offset in bytes
    **
    *)


    let vread ~disk ~file ~foff ~size ~buf ~boff =
      try
      begin
        let blksize = disk.bdisk_blksize in
        let buf' = disk.bdisk_buf in    (* tmp buffer *)
        let bstart = file.boot_start in
        let fsize = foff+size in
        let boff = ref boff in

        let bsize = (size+blksize-1) / blksize in
        let bf = foff / blksize in      (* first block *)
        let bl = fsize / blksize in     (* last block  *)
        let bm1 = bf + 1 in             (* first full sized block *)
        let bm2 = bl - 1 in             (* last full sized block  *)

#ifdef DEBUG
            Db.Pr.sdd 1 "vread foff size" foff size;
            Db.Pr.sdd 1 "      bf bl" bf bl;
            Db.Pr.sdd 1 "      bm1 bm2" bm1 bm2;
#endif

        (*
        ** Read first block - maybe actuall size differ from blksize 
        *)
        let stat = disk_read disk.bdisk_vcap
                                        ~start:(bstart + bf)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
        if stat <> std_OK then   
        begin
            raise (Error stat);
        end;
        let cursize = min (blksize - (foff mod blksize))
                          size in

        blit_bb ~src:buf'
                ~src_pos:(foff mod blksize)
                ~dst:buf
                ~dst_pos:!boff
                ~len:cursize;
        boff := !boff + cursize;

        (*
        ** Now all full sized blocks (blksize) in one chunk - if any.
        *)
        if bm1 < bl && (bm2-bm1) >= 0 then
        begin
            let stat = disk_read disk.bdisk_vcap
                                        ~start:(bstart + bm1)
                                        ~num:(bm2-bm1+1)
                                        ~blksize:blksize
                                        ~buf:buf
                                        ~pos:!boff in
            if stat <> std_OK then   
            begin
                raise (Error stat);
            end;
            boff := !boff + (blksize * (bm2-bm1+1));
        end;
        (*
        ** And finally the last block - maybe actuall size differ from blksize 
        *)
        let cursize = (fsize mod blksize) in
        if (cursize > 0) then
        begin

            let stat = disk_read disk.bdisk_vcap
                                        ~start:(bstart + bl)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
            if stat <> std_OK then   
            begin
                raise (Error stat);
            end;
            blit_bb ~src:buf'
                    ~src_pos:0
                    ~dst:buf
                    ~dst_pos:!boff
                    ~len:cursize;
        end;
        std_OK
      end
      with Error err -> err
        in
        

    let vwrite ~disk ~file ~foff ~size ~buf ~boff =
      try
      begin
        let blksize = disk.bdisk_blksize in
        let buf' = disk.bdisk_buf in    (* tmp buffer *)
        let bstart = file.boot_start in
        let fsize = foff+size in
        let boff = ref boff in

        let bsize = (size+blksize-1) / blksize in
        let bf = foff / blksize in      (* first block *)
        let bl = fsize / blksize in     (* last block  *)
        let bm1 = bf + 1 in             (* first full sized block *)
        let bm2 = bl - 1 in             (* last full sized block  *)

#ifdef DEBUG
            Db.Pr.sdd 1 "vwrite foff size" foff size;
            Db.Pr.sdd 1 "      bf bl" bf bl;
            Db.Pr.sdd 1 "      bm1 bm2" bm1 bm2;
#endif

        (*
        ** If we modify the last block of a file,
        ** we must pad the area between the last file byte and
        ** the end of the last block with white spaces
        ** to ensure valid text and bytecode files.
        *)
        let pad buf blksize block len =
            if ((block+1)*blksize) > file.boot_file_size then
            begin
#ifdef DEBUG
                Db.Pr.sdd 1 "padding: start,size" len (blksize-len);
#endif
                fill buf
                     len
                     (blksize-len)
                     32;
            end;
            in


        (*
        ** Read first block - maybe actuall size differ from blksize 
        *)
#ifdef DEBUG
        Db.Pr.sdd 1 "bf start,boff" (bstart + bf) !boff;
#endif
        let stat = disk_read disk.bdisk_vcap
                                        ~start:(bstart + bf)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
        if stat <> std_OK then   
        begin
            raise (Error stat);
        end;


        let cursize = min (blksize - (foff mod blksize))
                          size in


#ifdef DEBUG
        Db.Pr.sdd 1 "bf cursize,boff" cursize !boff;
#endif
        (*
        ** Copy only desired region
        *)
        blit_bb ~dst:buf'
                ~dst_pos:(foff mod blksize)
                ~src:buf
                ~src_pos:!boff
                ~len:cursize;

        pad buf' blksize bf cursize;

        (*
        ** Now write this block to disk again 
        *)
        let stat = disk_write disk.bdisk_vcap
                                        ~start:(bstart + bf)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
        if stat <> std_OK then   
        begin
            raise (Error stat);
        end;

        boff := !boff + cursize;

        (*
        ** Now all full sized blocks (blksize) in one chunk - if any.
        *)
        if bm1 < bl && (bm2-bm1) >= 0 then
        begin
#ifdef DEBUG
            Db.Pr.sddd 1 "bm start,len,boff" (bstart + bm1) (bm2-bm1+1) !boff;
#endif
            let stat = disk_write disk.bdisk_vcap
                                        ~start:(bstart + bm1)
                                        ~num:(bm2-bm1+1)
                                        ~blksize:blksize
                                        ~buf:buf
                                        ~pos:!boff in
            if stat <> std_OK then   
            begin
                raise (Error stat);
            end;
            boff := !boff + (blksize * (bm2-bm1+1));
        end;

        let cursize = (fsize mod blksize) in
        if (cursize > 0 && bl <> bf) then
        begin
#ifdef DEBUG
            Db.Pr.sddd 1 "bl start,cursize,boff" (bstart + bl) cursize !boff;
#endif
            (*
            ** And finally the last block - maybe actuall size differ from blksize 
            *)
            let stat = disk_read disk.bdisk_vcap
                                        ~start:(bstart + bl)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
            if stat <> std_OK then   
            begin
                raise (Error stat);
            end;
            blit_bb ~dst:buf'
                    ~dst_pos:0
                    ~src:buf
                    ~src_pos:!boff
                    ~len:cursize;

            pad buf' blksize bl cursize;
            
            let stat = disk_write disk.bdisk_vcap
                                        ~start:(bstart + bl)
                                        ~num:1
                                        ~blksize:blksize
                                        ~buf:buf'
                                        ~pos:0 in
            if stat <> std_OK then   
            begin
                raise (Error stat);
            end;
        end;
        std_OK
      end
      with Error err -> err
        in


    let p1 = uniqport () in
    let p2 = uniqport () in
    let disk = {
            bdisk_blksize = blocksize;
            bdisk_lock = mu_create ();
            bdisk_vcap = vcap;
            bdisk_prvport = p1;
            bdisk_chkport = uniqport ();
            bdisk_pubport = priv2pub p1;
            bdisk_file_prvport = p2;
            bdisk_file_chkport = uniqport ();
            bdisk_file_pubport = priv2pub p2;
            bdisk_name = bootdiskname;
            bdisk_path = hostpath;
            bdisk_table = !bdes;
            bdisk_nfiles = bd_NENTRIES;
            bdisk_sync = false;
            bdisk_buf = buf;
            bdisk_freeblocks = [];
            bdisk_read = vread;
            bdisk_write = vwrite;
        } in
    let stat = bd_freeblock_list disk in
    if stat <> std_OK 
        then fail (sprintf "can't build freeblock list of bootdisk: %s"
                        (err_why stat));


    if verbose then
    begin
        info "Freeblock list:";
        List.iter (fun fb ->
                let st,si = fb in
                info (sprintf "  start=%d size=%d" st si); 
            ) disk.bdisk_freeblocks;
    end;
    disk

   
let start nthr disk verbose =
    let hostpath = disk.bdisk_path in
    (*
    ** Append our DNS root cap = boot directory
    *)
    let pubport = priv2pub disk.bdisk_prvport in
    let prv = prv_encode ~obj:(Objnum 1)
                         ~rights: prv_all_rights
                         ~rand:disk.bdisk_chkport in
    let rootcap = {cap_port = pubport;
                   cap_priv = prv} in

    (*
    ** Lookup host directory
    *)
    let stat,dircap = name_lookup hostpath in
    if stat <> std_OK then fail "can't lookup host directory";
    (*
    ** Old remains ?
    *)
    let diskpath = (hostpath^"/"^disk.bdisk_name) in
    let stat,cap =  name_lookup diskpath in
    if stat = std_OK then
    begin
        let stat = name_delete diskpath in
        if stat <> std_OK then 
            fail (sprintf "can't delete old server cap in host directory:%s"
                    (err_why stat));
    end;
    let stat = name_append diskpath rootcap in
    if stat <> std_OK 
        then fail (sprintf "can't append our server cap to host directory:%s"
                    (err_why stat));

    let sema = sema_create 0 in
    (*
    ** Start server threads
    *)
    info (sprintf "Starting %d DNS server threads..." nthr);

    for i = 1 to nthr
    do
        ignore(thread_create (fun () -> server_dns_loop 
                                            ~disk:disk
                                            ~sema:sema
                                            ~nthreads:nthr
                                            ~inbuf_size:disk_REQBUFSZ
                                            ~outbuf_size:disk_REQBUFSZ)
                            ());
    done;
    info (sprintf "Starting %d AFS server threads..." nthr);

    for i = 1 to nthr
    do
        ignore(thread_create (fun () -> server_afs_loop 
                                            ~disk:disk
                                            ~sema:sema
                                            ~nthreads:nthr
                                            ~inbuf_size:disk_REQBUFSZ
                                            ~outbuf_size:disk_REQBUFSZ)
                            ());
    done;
    (*
    ** Start garbage collector
    *)

    ignore(thread_create (fun () -> bootdisk_gc disk 1) ());

    (*
    ** Wait for server exit...
    *)
    for i = 1 to nthr
    do
        sema_down sema;        
    done;
    (*
    ** Shutdown AFS server threads...
    *)
    let cap = {cap_port = priv2pub disk.bdisk_file_prvport;
               cap_priv = prv_encode (Objnum 0)
                                     prv_all_rights
                                     disk.bdisk_file_chkport;
               } in
    ignore(std_exit cap)


         

let stop disk = 
    let hostpath = disk.bdisk_path in
    let diskpath = (hostpath^"/"^disk.bdisk_name) in
    let stat,cap =  name_lookup diskpath in
    if stat = std_OK then
    begin
        let stat = name_delete diskpath in
        if stat <> std_OK then 
            fail (sprintf "can't delete current server cap in host directory:%s"
                    (err_why stat));
    end;
    ()

