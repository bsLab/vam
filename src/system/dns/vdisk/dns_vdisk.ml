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
**    $VERSION:     1.05
**
**    $INFO:
**
**  Amoeba VDISK implementation of the Directory and Name Amoeba server.
**
**    $ENDOFINFO
**
*)

(* #define DEBUG  *)


open Amoeba
open Bytebuf
open Buf
open Stdcom
open Stderr
open Thread
open Rpc
open Capset
open Ar
open Syslog

open Afs_common
open Afs_client
open Afs_server
open Afs_cache          (* We use the AFS cache here, too *)
open Dns_common
open Dns_server
open Dns_server_rpc
open Disk_client
open Name

open Capset
open Printf


let server_version = "1.02"



(*
** Disk layout
**
**  Directory Table Partition:
**
**  -------------------------
**  512 bytes: 
**      32 bytes    : magic header
**      480 bytes   : super structure
**  -------------------------
**  Directory (inode) table
**
**  
**  -------------------------
**  Live  table
**  -------------------------
** 
**   
** The directory table contains the capability set of a directory (aka.
** directory object number to capset mapping).
**
*)


(*
** Default filesystem (data) block size. This should be
** the smallest value supported by the underlying OS hardware
** layer!
*)

let def_block_size  = 512

(*
** Raw disk structures (Directory table).
*)

let sizeof_int32 = 4
let sizeof_int16 = 2 

(*
** Directory object number to AFS capability set mapping (= inode).
**
** Each directory is saved in an AFS file. The directory object
** number to AFS object mapping is saved in the DNS disk partition.
*)

(*
** Convert the AFS file state to an integer value
*)

let of_state f =
    match f with    
    | FF_invalid -> 0;
    | FF_unlocked  -> 0x10;
    | FF_commit -> 0x40;
    | FF_locked -> 0x80
        
let to_state f =
    match f with    
    | 0 -> FF_invalid;
    | 0x10 -> FF_unlocked;
    | 0x40 -> FF_commit;
    | 0x80 -> FF_locked;
    | _ -> raise (Error std_SYSERR)


type inode = {
    mutable i_objnum:      int;     (* Directory object number      *)
    mutable i_caps:        capability array;  
                                    (* AFS capabilities             *)
    mutable i_size:        int;     (* Size of dir AFS file [bytes] *)
    mutable i_state:       int;     (* Used or not                  *)
}

let nilinode = {i_objnum=0;i_caps=[||];i_size=0;i_state=0}

(*
** Buffersize for each directory mapping (== sizeof(inode)).
** Rounded up to fit mutiple times in one 512 byte block. 
*)

let inode_SIZE = 
    let size = 2 * sizeof_int32 + sizeof_int16 +
               2 * cap_SIZE  
    in
    if size <= 64 then
        64
    else if size <= 128 then
        128
    else if size <= 256 then
        256
    else
        failwith "dns_unix: inode_SIZE: out of range"

(*
** Store and extract directories, saved by the AFS server,
** in a machine independent way, and the table entries, too.
*)

let buf_get_row ~buf ~pos ~ncols =
    let pos,r1 = buf_get_string ~buf:buf
                                ~pos:pos in
    let pos,r2 = buf_get_int32 ~buf:buf
                               ~pos:pos in
    let pos = ref pos in

    let r3 = Array.create ncols prv_all_rights in
    for i = 0 to ncols - 1
    do
#ifdef DEBUG
        Db.Pr.s 1 "buf_get_row: col rights";
#endif
        let pos',rg = buf_get_rights_bits  ~buf:buf
                                           ~pos:!pos in
        r3.(i) <- rg;
        pos := pos';                   
    done;

    let pos,r4 = buf_get_capset ~buf:buf
                                ~pos:!pos in
#ifdef DEBUG
    Db.Pr.ss 1 "buf_get_row: cs" (ar_cs r4);
#endif

    pos, {
        dr_name = r1;
        dr_time = r2;
        dr_columns = r3;
        dr_capset = r4;
    }

let buf_get_dir ~buf ~pos =
    let pos,d1 = buf_get_int32 ~buf:buf
                               ~pos:pos in
    let pos,ncols = buf_get_int32 ~buf:buf
                                  ~pos:pos in
    let pos,nrows = buf_get_int32 ~buf:buf
                                  ~pos:pos in
    let pos = ref pos in

    let d4 = Array.create ncols "" in

    for i = 0 to ncols - 1 
    do
        let pos',cn = buf_get_string ~buf:buf
                                     ~pos:!pos in
        d4.(i) <- cn;
        pos := pos';                   
    done;

    let pos,d5 = buf_get_port ~buf:buf
                              ~pos:!pos in

    let pos = ref pos in
    let rows = Dblist.create () in
    for i = 1 to nrows 
    do
            let pos',row = buf_get_row ~buf:buf
                                       ~pos:!pos
                                       ~ncols:ncols in
            Dblist.insert_tail ~dblist:rows 
                               ~node:row;
            pos := pos';
    done;            
    
    let pos,d7 = buf_get_int32 ~buf:buf
                               ~pos:!pos in
    pos, {
        dd_lock = mu_create ();
        dd_objnum = d1;
        dd_ncols = ncols;
        dd_nrows = nrows;
        dd_colnames = d4;
        dd_random = d5;
        dd_rows = rows;
        dd_state = DD_locked;
        dd_time = d7;
        dd_live = 0;
    }

let buf_get_inode ~buf ~pos =
    let start = pos in
    let pos,i1 = buf_get_int32 ~buf:buf
                            ~pos:pos
                            in
    let pos,i2a = buf_get_cap ~buf:buf
                                 ~pos:pos 
                                 in
    let pos,i2b = buf_get_cap ~buf:buf
                                 ~pos:pos 
                                 in
    let pos,i3 = buf_get_int32 ~buf:buf
                                ~pos:pos 
                                in
    let pos,i4 = buf_get_int16 ~buf:buf
                                ~pos:pos 
                                in

    (start+inode_SIZE), {
        i_objnum = i1;
        i_caps   = [|i2a;i2b|];
        i_size   = i3;
        i_state  = i4;
    }               


let buf_put_row ~buf ~pos ~row ~ncols =
    let pos = buf_put_string ~buf:buf
                             ~pos:pos
                             ~str:row.dr_name in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:row.dr_time in
    let pos = ref pos in
    for i = 0 to ncols-1
    do 
#ifdef DEBUG
        Db.Pr.s 1 "buf_put_row: col rights";
#endif
        let pos' = buf_put_rights_bits  ~buf:buf
                                        ~pos:!pos
                                        ~rights:row.dr_columns.(i) in
        pos := pos';                   
    done;

    let pos = buf_put_capset ~buf:buf
                             ~pos:!pos
                             ~cs:row.dr_capset
    in    
#ifdef DEBUG
    Db.Pr.ss 1 "buf_put_row: cs" (ar_cs row.dr_capset);
#endif

    pos

(*
** Write a dns_dir structure
*)

let buf_put_dir ~buf ~pos ~dir =
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:dir.dd_objnum in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:dir.dd_ncols in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:dir.dd_nrows in
    let pos = ref pos in
    Array.iter (fun cstr ->

        let pos' = buf_put_string ~buf:buf
                                  ~pos:!pos
                                  ~str:cstr in
        pos := pos';                   
        ) dir.dd_colnames;

    let pos = buf_put_port ~buf:buf
                            ~pos:!pos
                            ~port:dir.dd_random in

    let pos = ref pos in
    Dblist.iter (fun r ->
            let pos' = buf_put_row ~buf:buf
                                   ~pos:!pos
                                   ~row:r 
                                   ~ncols:dir.dd_ncols in
            pos := pos';
            
        ) dir.dd_rows;
    
    let pos = buf_put_int32 ~buf:buf
                            ~pos:!pos
                            ~int32:dir.dd_time in
    pos 
    

(*
** Write the dir inode structure.
*)

let buf_put_inode ~buf ~pos ~inode =
    let start = pos in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_objnum in
    let pos = buf_put_cap ~buf:buf
                             ~pos:pos
                             ~cap:inode.i_caps.(0) in
    let pos = buf_put_cap ~buf:buf
                             ~pos:pos
                             ~cap:inode.i_caps.(1) in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_size in
    let pos = buf_put_int16 ~buf:buf
                            ~pos:pos
                            ~int16:inode.i_state in
    (start+inode_SIZE)


(*
** Calculate the size of a directory structure.
*)

let dir_size dir =
    4 * sizeof_int32 +  (* dd_objnum,dd_ncols,dd_nrows,dd_time *)
    port_SIZE +         (* dd_random *)
    (
        (*
        ** colnames 
        *)
        let size = ref 0 in
        Array.iter (fun cstr ->
            size := !size + (String.length cstr) + 1;
        ) dir.dd_colnames;
        !size  
    ) +
    (
        (*
        ** Rows
        *)
        let size = ref 0 in
        Dblist.iter (fun r ->
            size := !size + sizeof_int32                (* dr_time *)
                          + (String.length r.dr_name) + 1
                          + capset_SIZE                 (* dr_capset *)
                          + (Array.length r.dr_columns) * 
                            sizeof_int32;
               
        ) dir.dd_rows;
        !size;
    )   




(*
** Cache parameters
*)

(*
** Default Number of cache entries for the inode cache
*)

let def_inode_ENTRIES = 500

let def_inode_SIZE = 1                  (* one block !!! *)

(*
** Default Number of cache entries for the dir cache
*)

let def_dir_ENTRIES = 200

(*
** Default dir buffer size (multiple of block_size)
*)

let def_dir_BUFSIZE = 1




(*
** Age periodically the inode cache and remove timeouted 
** cache objects [sec].
*)

let cache_GC_TIME = 10

(*
** Default Partition names and paths (UNIX files)
*)

let def_vpath = "/"
let def_vdisk_inode = "vdisk:03"

(*
** Disk Magic Label 
*)

let magic_str = "AMOEBA::DNS::INODE::032" 

(*
** Cache parameters
*)

type cache_param = {
    mutable c_inode_buffers: int; (* Number of inode cache buffers *)
    mutable c_inode_size: int;    (* Size of one inode cache buffer [blocks] *)
    mutable c_dir_buffers: int;   (* Number of dir cache buffers *)
}

(*
** Calculate the logical partition offset for an inode
*)

let off_of_inode inode =
    inode * inode_SIZE


(*
** Convert lbock to byte units and vice versa
*)
let to_block x block_size =
    x/block_size

let of_block x block_size =
    x*block_size


(*
** Round up the value x to nearest block_size -> block & byte units
*)
  
let ceil_block x block_size =
    ((x+block_size-1)/block_size)

let ceil_block_bytes x block_size =
    ((x+block_size-1)/block_size)*block_size

(*
** Round down the value x to nearest block_size -> block & byte units
*)
  
let floor_block x block_size =
    (x/block_size)

let floor_block_bytes x block_size =
    (x/block_size)*block_size



(*
** Create the filesystem (one partition = UNIX file). 
**
** Args:
**  label:      the filesystem label string [max 256 chars]
**  ninodes:    number of inodes ( = maximal number of directories)
**  cols:       Column name array (dns_colnames)
**  colmasks:   Column mask array (dns_generic_colmask)
**  vdisk_inode:  Path and name for the inode vdisk
**  fs_server:  AFS server(s). All directories are saved there. Up to
**              two file servers can be specified. The first is the default.
**  
** Return:
**  status
**  dns_super 
*)


let create_vdisk_fs
        ~label 
        ~ninodes
        ~cols
        ~colmasks
        ~vdisk_inode
        ~fs_server
        ~overwrite
    =
    let blocksize = def_block_size in

    try
    begin
        if (fs_server.(0) = nilcap) then
        begin
            sys_log Sys_fatal "DNS: no default (0) fileserver specified.\n";
            raise (Error std_ARGBAD); 
        end;
        sys_log Sys_info "DNS: Creating DNS tree...\n";
        sys_log Sys_info "DNS: Blocksize: %8d [bytes]\n" blocksize;
        sys_log Sys_info "DNS: Number of total inodes (dirs): %8d\n" ninodes;

        (*
        ** Get the virtual disk caps
        *)
        sys_log Sys_info "DNS: inode part -> %s\n" vdisk_inode;

        let stat1,vdisk_inode_cap=name_lookup vdisk_inode in

        if stat1 <> std_OK then raise (Error stat1);
    

        (*
        ** First create a server port and derive the public port.
        *)
        let priv_port = uniqport () in
        let pub_port  = priv2pub priv_port in
        let checkfield = uniqport () in



        (*
        ** The disk super structure.
        *)

        let super = buf_create def_block_size in

        (*
        ** The partition magic headers. After the magic string,
        ** the value 0xaa is written. 
        *)

    
        Bytebuf.fill super ~pos:0 ~len:32 0xaa;
        ignore (buf_put_string ~buf:super
                               ~pos:0
                               ~str:magic_str);

        let soff = 32 in        (* offset due to magic header *)

        Bytebuf.fill super ~pos:soff ~len:480 0xaa;



        (* dns_name *)
        let pos = buf_put_string ~buf:super
                                 ~pos:soff ~str:label in
        (* dns_ndirs *)
        let pos = buf_put_int32  ~buf:super
                                 ~pos:(256+soff) ~int32:ninodes in
        (* dns_block_size *)
        let pos = buf_put_int32  ~buf:super
                                 ~pos:pos ~int32:blocksize in
        (* dns_getport *)
        let pos = buf_put_port   ~buf:super
                                 ~pos:pos ~port:priv_port in
        (* dns_putport *)
        let pos = buf_put_port   ~buf:super
                                 ~pos:pos ~port:pub_port in
        (* dns_checkfield *)
        let pos = buf_put_port   ~buf:super
                                 ~pos:pos ~port:checkfield in

        (* dns_ncols *)
        let ncols = Array.length cols in
        let pos = buf_put_int32  ~buf:super
                                 ~pos:pos ~int32:ncols in
        (* dns_colnames *)
        let pos = ref pos in
        for i = 0 to ncols-1
        do
            let pos' = buf_put_string ~buf:super
                                      ~pos:!pos
                                      ~str:cols.(i) in
            pos := pos';
        done;

        (* dns_generic_colmask *)
        for i = 0 to ncols-1
        do
            let pos' = buf_put_rights_bits ~buf:super
                                      ~pos:!pos
                                      ~rights:colmasks.(i) in
            pos := pos';
        done;

        (* dns_fs_server *)
        for i = 0 to 1
        do
            let pos' = buf_put_cap ~buf:super
                                   ~pos:!pos
                                   ~cap:fs_server.(i) in
            pos := pos';
        done;
    

        sys_log Sys_info "DNS: Writing super structure... "; 

        let stat = disk_write vdisk_inode_cap ~start:0
                                         ~num:1
                                         ~blksize:def_block_size
                                         ~buf:super
                                         ~pos:0 in      
        if (stat <> std_OK) then
        begin
            sys_log Sys_info "failed.\n";
            raise (Error stat); 
        end;
        
        sys_log Sys_info  "Done.\n"; 



        (*
        ** Write the inodes.
        *)

        sys_log Sys_info  "DNS: Writing inode table... ";

        let buf = buf_create inode_SIZE in

        let inode = {
            i_objnum = 0;
            i_caps = [|nilcap;nilcap|];
            i_size = 0;
            i_state = of_state FF_invalid;
        } in

        let ninodes_block = def_block_size / inode_SIZE in
        let inode_buf = buf_create def_block_size in
        let ninodes' = ninodes / ninodes_block in   

        (*
        ** Inodes start on second vdisk block
        *) 
        let disk_off = ref 1 in


        for i = 0 to (ninodes'-1)
        do
            let pos = ref 0 in
            for j = 0 to (ninodes_block-1)
            do
                inode.i_objnum <- j + i*ninodes_block;
                pos := buf_put_inode ~buf:inode_buf ~pos:!pos
                                     ~inode:inode;

            done;
            let stat = disk_write vdisk_inode_cap
                                         ~start: !disk_off
                                         ~num:1
                                         ~blksize:def_block_size
                                         ~buf:inode_buf    
                                         ~pos:0 in
            incr disk_off;
            if (stat <> std_OK) then
            begin
                sys_log Sys_fatal "failed: %s\n" (err_why stat); 
                raise (Error stat); 
            end;
        done;            

        sys_log Sys_info  "Done.\n"; 

        let super =
        {
            dns_lock = mu_create ();
            dns_name = label;
            dns_ndirs = ninodes;
            dns_nused = 0;
            dns_freeobjnums = [];
            dns_nextfree = 1;
            dns_getport = priv_port;
            dns_putport = pub_port;
            dns_checkfield = checkfield;
            dns_ncols = ncols;
            dns_colnames = cols;
            dns_generic_colmask = colmasks;
            dns_block_size = blocksize;
            dns_fs_server = { fs_cap = fs_server;
                              fs_state = [|FS_unknown;FS_unknown|];
                              fs_default = 0; dns_mode = Dnsmode_ONECOPY };
        }
        in

        (*
        ** The livtime table. It's a fixed size bitfield table build with
        ** a string of sufficient size. 
        **
        ** Assumption: Maximale Live time value < 128, therefore  
        **             7 bit are used for each object. The MSB is
        **             the used flag (=1 -> inode used).
        **
        ** The first entry (obj=0) is used for the lock status
        ** of the live table.
        **
        *)

        let live_size = ninodes + 1 in
        let live_table = buf_create (ceil_block_bytes live_size 
                                     def_block_size) in


        (*
        ** the first block of the livetime table and his size in
        ** blocks
        *)
        let live_block_off = 1 + (ceil_block (ninodes*inode_SIZE) def_block_size)
            in
        let live_block_size = ceil_block live_size def_block_size in

        
        let live_set lvt ~obj ~time ~flag =
            buf_set lvt obj ((time land 0x7f) lor
                                ((flag lsl 7) land 0x80))
        in

        let live_write lv =
            (*
            ** Unlock the live table
            *)
            live_set lv ~obj:0 ~time:0 ~flag:1;
#ifdef DEBUG
            Db.Pr.sdd 1 "writing live table of size at starting block"
                       live_block_size live_block_off;
#endif

            disk_write vdisk_inode_cap ~start:live_block_off
                                  ~num:live_block_size
                                  ~blksize:def_block_size
                                  ~buf:lv
                                  ~pos:0;
        in
    

        for i = 1 to ninodes-1
        do
            live_set live_table ~obj:i ~time:afs_MAXLIVE ~flag:0;
        done;

        sys_log Sys_info  "DNS: Writing live table... ";

        let stat = live_write live_table in
        if (stat <> std_OK) then
        begin
            sys_log Sys_fatal "failed.\n";
            raise (Error stat);
        end;
        sys_log Sys_info "Done.\n"; 

        (*
        ** Now the last part: create an initial root directory (empty).
        ** We must setup a small server environment to do this!
        *)  
        let rootinode = {
                i_objnum = 1;
                i_caps = [|nilcap;nilcap|];
                i_size = 0;
                i_state = of_state FF_invalid;
            } in
        let rootcap = ref nilcap in


        let write_inode ~obj
                        ~data
                        ~size   
            =
            try 
            begin
                (*
                ** Read and write full blocks!
                *)
                let buf = buf_create blocksize in
                let off = 512 + (off_of_inode obj) in
                let off' = floor_block off blocksize in
                let off'' = off mod blocksize in

#ifdef DEBUG
                Db.Pr.sddd 1 "write_inode [size,off,off']" size off off';
#endif

                let stat = disk_read vdisk_inode_cap 
                                  ~start:off'
                                  ~num:1
                                  ~blksize:def_block_size
                                  ~buf:buf
                                  ~pos:0 in
                if stat <> std_OK then raise (Error stat);


                blit_bb ~src:data ~src_pos:0 
                        ~dst:buf ~dst_pos:off'' 
                        ~len:size;

                let stat = disk_write vdisk_inode_cap 
                                  ~start:off'
                                  ~num:1
                                  ~blksize:def_block_size
                                  ~buf:buf
                                  ~pos:0 in
                stat
            end
            with Error stat -> stat
        in

        let dns_create_dir ~dir =
          try
          begin
            let obj = dir.dd_objnum in
            let fs = fs_server in
            let fs_default = 0 in
            let stat,cap = afs_create ~cap:fs.(fs_default)
                                      ~buf:nilbuf
                                      ~size:0
                                      ~commit:afs_UNCOMMIT
            in
            if (stat = std_OK) then
            begin
                dir.dd_state <- DD_unlocked;
                let inodeb = buf_create inode_SIZE in
                rootinode.i_objnum <- obj;
                rootinode.i_caps <- (  let ca =
                                                Array.create 2 nilcap in
                                          ca.(fs_default) <- cap;
                                          ca
                                       );   
                rootinode.i_size <- 0;   
                rootinode.i_state <- of_state FF_unlocked;

                let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:rootinode in

                let stat = write_inode ~obj:obj
                                       ~data:inodeb
                                       ~size:inode_SIZE
                                       in
                stat
            end
            else
            begin
                sys_log Sys_fatal 
                        "DNS: dns_create_dir: afs_create failed: %s\n"
                        (err_why stat);
                stat
            end;
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal
                        "DNS: dns_create_dir: buffer oveflow. Fatal\n";
                std_OVERFLOW;
            end
        in

        let dns_modify_dir ~dir =
          try
          begin
            let obj = dir.dd_objnum in
            let fs_default = 0 in
            let dirsize = dir_size dir in
            let dirbuf = buf_create dirsize in
            let _ = buf_put_dir ~buf:dirbuf
                                    ~pos:0
                                    ~dir:dir in 
            let stat,_ = afs_modify ~cap:rootinode.i_caps.(fs_default)
                                      ~buf:dirbuf
                                      ~offset:0
                                      ~size:dirsize
                                      ~commit:afs_SAFETY
            in
            if (stat = std_OK) then
            begin
                    dir.dd_state <- DD_locked;
                    let inodeb = buf_create inode_SIZE in
                    rootinode.i_size <- dirsize;
                    rootinode.i_state <- of_state FF_locked;

                    rootcap := {
                            cap_port = super.dns_putport;
                            cap_priv = prv_encode 
                                            ~obj:(Objnum rootinode.i_objnum)
                                            ~rights:prv_all_rights
                                            ~rand:dir.dd_random;
                        };
                    let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:rootinode in
                
                    let stat = write_inode ~obj:obj
                                       ~data:inodeb
                                       ~size:inode_SIZE
                                       in
                    stat
            end
            else
            begin
                sys_log Sys_fatal 
                        "DNS: dns_modify_dir: afs_modify failed: %s\n"
                        (err_why stat);
                stat
            end;
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal
                        "DNS: dns_modify_dir: buffer oveflow. Fatal\n";
                std_OVERFLOW;
            end
                        
        in

        let server =
            {
                    dns_super = super;
                    dns_read_dir = (fun ~obj -> nildnsdir,std_SYSERR);
                    dns_modify_dir = dns_modify_dir;
                    dns_create_dir = dns_create_dir;
                    dns_delete_dir = (fun ~dir -> std_SYSERR);
                    dns_delete_inode = (fun ~obj -> std_SYSERR);
                    dns_read_super = (fun () -> super,std_OK);
                    dns_sync = (fun () -> std_OK);
                    dns_stat = (fun () -> std_OK,"");
                    dns_touch = (fun ~dir -> ()); 
                    dns_age = (fun ~obj -> false,0);
                    dns_exit = (fun () -> std_OK);
                    dns_time = (fun () -> int_of_float((Sys.clock())/.10.0));
             } in        

        sys_log Sys_info  "DNS: Creating root directory...";

        let stat,rootdir = dns_create_root ~server:server in

        if stat <> std_OK then
        begin
            sys_log Sys_fatal "failed.\n"; 
            raise (Error stat);
        end;

        sys_log Sys_info "Ok.\n";
        sys_log Sys_info  "DNS: Finished.\n";


        stat,super,!rootcap
    end
    with
        | Error err -> 
               sys_log Sys_fatal "\nDNS: Abort: %s\n" (err_why err);
               err,nildnssuper,nilcap
            
        | Buf_overflow -> 
               sys_log Sys_fatal "\nDNS: Abort: Buffer Overflow.\n";
               std_SYSERR,nildnssuper,nilcap

        | _ -> 
               sys_log Sys_fatal "\nDNS: Abort: Input-Output Error.\n";
               std_IOERR,nildnssuper,nilcap


(*
** Server statistics
*)
type dns_stats = {
    mutable op_touch: int;
    mutable op_age: int;
    mutable op_destroy: int;
    mutable op_read: int;
    mutable op_modify: int;
    mutable op_create: int;
    mutable op_commit: int;
    mutable op_sync: int;
}


(*
** Start the DNS server (one partition, UNIX file)
**
**  vdisk_inode:  Path and name for the inode vdisk
**  cache:      Cache parameters
**  mode:       dns_mode
**  
** Return:
**  status
**  dns_server
*)

let start_vdisk_fs 
        ~vdisk_inode
        ~cache
        ~mode
    =    
    let stats = {
            op_touch=0;
            op_age=0;
            op_destroy=0;
            op_read=0;
            op_modify=0;
            op_create=0;
            op_sync=0;
            op_commit=0;
        } in
                
    let nildnsserver =
            {
                    dns_super = nildnssuper;
                    dns_read_dir = (fun ~obj -> nildnsdir,std_SYSERR);
                    dns_modify_dir = (fun ~dir -> std_SYSERR);
                    dns_create_dir = (fun ~dir -> std_SYSERR);
                    dns_delete_dir = (fun ~dir -> std_SYSERR);
                    dns_delete_inode = (fun ~obj -> std_SYSERR);
                    dns_read_super = (fun () -> nildnssuper,std_SYSERR);
                    dns_sync = (fun () -> std_SYSERR);
                    dns_stat = (fun () -> std_SYSERR,"");
                    dns_touch = (fun ~dir -> ());
                    dns_age = (fun ~obj -> false,0);
                    dns_exit = (fun () -> std_SYSERR);
                    dns_time = (fun () -> 0);
             } in

    try
    begin
        sys_log Sys_start "DNS: Directory and Name Server, Ver. %s\n%s\n"
                      (server_version)
                      "     (C) 2003-2005 BSSLAB Dr. Stefan Bosse";
        sys_log Sys_start "DNS: Initializing normal mode...\n";

        (*
        ** Either the filesystem is read only or we have an 
        ** inconsistent filesystem. In that case, only read
        ** requests are allowed.
        *)

        let readonly = ref false in


        (*
        ** Here we store the inodes in one virtual disk.
        *)

        sys_log Sys_start "DNS: Checking virtual disk...\n"; 
        sys_log Sys_start "DNS: Inode part -> %s\n" vdisk_inode;


        (*
        ** Get the virtual disk caps
        *)

        let stat1,vdisk_inode_cap=name_lookup vdisk_inode in

        if stat1 <> std_OK then raise (Error stat1);

        (*
        ** Read the super structrure and the magic header block.
        *)

        sys_log Sys_start  "DNS: Reading the Superblock... ";

        let superb = buf_create 512 in

        let stat =
            disk_read vdisk_inode_cap
                                  ~start:0
                                  ~num:1  
                                  ~blksize:def_block_size
                                  ~buf:superb
                                  ~pos:0;
            in

        if (stat <> std_OK) then
        begin
            sys_log Sys_fatal "failed.\n"; 
            raise (Error stat);
        end;

        sys_log Sys_start "Done.\n" ;
        sys_log Sys_start "DNS: Checking the magic Header... ";
        
        let pos,mg = buf_get_string ~buf:superb ~pos:0 in
         
        if (mg <> magic_str) then
        begin
            sys_log Sys_fatal "failed.\n";
            raise (Error std_NOTFOUND);
        end;

        sys_log Sys_start  "OK.\n";  

        let soff = 32 in

        let super = 
            let pos,label = buf_get_string  ~buf:superb
                                            ~pos:soff
                            in
            let pos,ndirs = buf_get_int32 ~buf:superb
                                          ~pos:(256+soff)
                            in
        
            let pos,blocksize = buf_get_int32   ~buf:superb
                                                ~pos:pos
                            in

        
            let pos,priv_port = buf_get_port ~buf:superb
                                             ~pos:pos
                            in
            let pos,pub_port = buf_get_port ~buf:superb
                                             ~pos:pos
                            in
                
            let pos,checkfield = buf_get_port ~buf:superb
                                              ~pos:pos
                            in
            let pos,ncols = buf_get_int32 ~buf:superb
                                          ~pos:pos
                            in
            (*
            ** colnames 
            *)
            let pos = ref pos in
            let cols = Array.create ncols "" in
            for i = 0 to ncols-1
            do
                let pos',cn = buf_get_string ~buf:superb
                                             ~pos:!pos in
                pos := pos';
                cols.(i) <- cn;
            done;

            (* 
            ** generic_colmask 
            *)
            let colmasks = Array.create ncols prv_all_rights in
            for i = 0 to ncols-1
            do
                let pos',cm = buf_get_rights_bits ~buf:superb
                                                  ~pos:!pos in
                pos := pos';
                colmasks.(i) <- cm;
            done;
                
            (*
            ** fs_server 
            *)
            let fscaps = Array.create 2 nilcap in
            for i = 0 to 1
            do
                let pos',cap = buf_get_cap ~buf:superb
                                           ~pos:!pos in
                pos := pos';
                fscaps.(i) <- cap;
            done;
    
            {
                dns_lock = mu_create ();
                dns_name = label;
                dns_ndirs = ndirs;
                dns_nused = 0;              (* calculated below *)
                dns_freeobjnums = [];       (* calculated below *)
                dns_nextfree = 0;           (* calculated below *)
                dns_getport = priv_port;
                dns_putport = pub_port;
                dns_checkfield = checkfield;
                dns_ncols = ncols;
                dns_colnames = cols;
                dns_generic_colmask = colmasks;
                dns_fs_server = {
                        fs_cap = fscaps;
                        fs_state = [|FS_unknown;FS_unknown|];
                        fs_default = 0;
                        dns_mode = Dnsmode_ONECOPY;
                    };
                dns_block_size = blocksize;
            } in

        (*
        ** The default file server. Must have the state FF_up.
        ** This value specifies the capability used from a capset and 
        ** the server for new created directory objects. 
        *)


        let fs_default = ref 0 in

        (*
        ** Check the file server(s) and determine the default
        ** (primary) file server (the first one responding to a
        ** STD_INFO request). If the two copy mode was preferred, but
        ** only one file server is reachable, fall back to one
        ** copy mode.
        *)
        let mode =
            let fs = super.dns_fs_server in
            match mode with
            | Dnsmode_ONECOPY -> 
            begin
                sys_log Sys_start "DNS: entering one copy mode.\n";
                sys_log Sys_start "DNS: checking file servers... ";
                let goodcap = ref nilcap in
                for i = 0 to 1
                do                
                    let cap = fs.fs_cap.(i) in
                    if (cap <> nilcap &&
                        !goodcap = nilcap) then
                    begin
                        let stat,_ = std_info ~cap:cap ~bufsize:0 in
                        if (stat = std_OK) then
                        begin
                            goodcap := cap;
                            fs_default := i;
                            fs.fs_default <- i;
                            fs.fs_state.(i) <- FS_up;
                        end
                        else
                        begin
                            sys_log Sys_start "DNS: file server status: %s\n"
                                              (err_why stat);
                            fs.fs_state.(i) <- FS_down;
                        end;
                    end;                     
                done;
                if (!goodcap = nilcap) then
                begin
                    sys_log Sys_fatal "... No file server found. Abort.\n";
                    raise (Error std_NOTFOUND);
                end
                else
                begin
                    sys_log Sys_start  "OK.\n"; 
                end;
                Dnsmode_ONECOPY;
            end;
            | Dnsmode_TWOCOPY -> 
            begin
                sys_log Sys_start "DNS: entering two copy mode.\n";
                sys_log Sys_start "DNS: checking file servers... ";

                let goodcap = ref nilcap in
                let goods = ref 0 in
                for i = 0 to 1
                do                
                    let cap = fs.fs_cap.(i) in
                    if (cap <> nilcap) then
                    begin
                        let stat,_ = std_info ~cap:cap ~bufsize:0 in
                        if (stat = std_OK &&
                            !goodcap <> nilcap) then
                        begin
                            goodcap := cap;
                            fs_default := i;
                            fs.fs_default <- i;
                        end;
                        if (stat = std_OK) then
                        begin
                            fs.fs_state.(i) <- FS_up;
                            incr goods;
                        end;
                    end;
                done;
                if (!goodcap = nilcap) then
                begin
                    sys_log Sys_fatal "... No file server found. Abort.\n";
                    raise (Error std_NOTFOUND);
                end
                else if (!goods = 2) then
                begin
                    sys_log Sys_start  "OK.\n";
                    Dnsmode_TWOCOPY;
                end 
                else
                begin
                    sys_log Sys_start  "... only one file server alive.\n";
                    sys_log Sys_start  "DNS: falling back to one copy mode.\n";
                    Dnsmode_ONECOPY;
                end;
            end;
        in

        super.dns_fs_server.dns_mode <- mode;

        let block_size = super.dns_block_size in


        sys_log Sys_start "DNS: Label = \"%s\"\n" super.dns_name;
        sys_log Sys_start "DNS: Maximal number of files (inodes) = %d\n"
                          super.dns_ndirs;
        sys_log Sys_start "DNS: Blocksize = %d bytes\n"
                          block_size;

        (*
        ** The livetime table. It's a fixed size bitfield table build with
        ** a string of sufficient size. 
        **
        ** Assumption: Maximale Live time value < 128, therefore
        **             7 bit are used for each object. The MSB is
        **             the used flag (=1 -> inode used). 
        **             
        ** The first entry (obj=0) is used for the lock status
        ** of the live table.  
        *)

        let live_size = super.dns_ndirs in
        let live_table = buf_create 
                    (ceil_block_bytes live_size def_block_size) in

        let live_block_off = ceil_block (512 + (super.dns_ndirs*inode_SIZE))
                                   def_block_size in
        let live_block_size = ceil_block live_size def_block_size in


        let live_set lvt ~obj ~time ~flag =
#ifdef DEBUG
            Db.Pr.sddd 200 "live_set obj,val,flag" obj time flag;
#endif
            buf_set lvt obj ((time land 0x7f) lor
                                ((flag lsl 7) land 0x80))
        in             

        (*
        ** Returns time,flag tuple.
        *)
        let live_get lvt ~obj =
            let v = buf_get lvt obj in
            (v land 0x7f),((v land 0x80) lsr 7)
        in


        let live_read lv =
            let stat =    
                disk_read vdisk_inode_cap
                                  ~start:live_block_off
                                  ~num:live_block_size 
                                  ~blksize:def_block_size
                                  ~buf:lv
                                  ~pos:0;
                in


            if (stat <> std_OK) then
                stat
            else if ((live_get lv ~obj:0) = (0,1)) then
            begin   
                (*  
                ** Now lock the live table. After a server crash,
                ** the restarted server will find the lock and must
                ** discard the live table. All server objects will get
                ** the maximal livetime!
                *)
                live_set lv ~obj:0 ~time:1 ~flag:1;

                let stat =
                   disk_write vdisk_inode_cap
                                  ~start:live_block_off
                                  ~num:live_block_size 
                                  ~blksize:def_block_size
                                  ~buf:lv
                                  ~pos:0;
                    in
                stat  
            end
            else
            begin
                std_ARGBAD;
            end;
        in


        let live_write lv =
            (*
            ** Unlock the live table
            *)
            live_set lv ~obj:0 ~time:0 ~flag:1;

            let stat =
                   disk_write vdisk_inode_cap
                                  ~start:live_block_off
                                  ~num:live_block_size 
                                  ~blksize:def_block_size
                                  ~buf:lv
                                  ~pos:0;
                    in
            stat
        in


    

        (*
        ** Read the live table
        *)
        sys_log Sys_start  "DNS: Reading the livetime table..."; 

        let stat = live_read live_table in

        if ( stat = std_ARGBAD) then
        begin
            (*
            ** Found locked live table. Discard it and reinitialize.
            *)
            sys_log Sys_start  "found locked live table: Reinitialize";
            for i = 1 to super.dns_ndirs-1
            do
                live_set live_table ~obj:i ~time:afs_MAXLIVE ~flag:0;
            done;            
        end else if (stat = std_IOERR) then
        begin
            sys_log Sys_fatal " failed.\n"; 
            raise (Error stat);
        end;

        sys_log Sys_start " OK.\n"; 


        (*
        ** Raw disk Read and Write functions for the cache module.
        **
        ** Units:
        **  addr: blocks
        **  size: bytes
        *)

        (*
        ** Inode objnumber <-> capability mappings, simple.
        *)

        let read_inode  ~obj
                        ~addr 
                        ~data
                        ~size
            =
            let bsize =  ceil_block size def_block_size in

#ifdef DEBUG
            Db.Pr.sddd 200 "server: read_inode: [baddr,bsize,size]"
                    addr bsize size;
#endif
            let stat =
                disk_read vdisk_inode_cap
                                  ~start:addr
                                  ~num:bsize
                                  ~blksize:def_block_size
                                  ~buf:data
                                  ~pos:0;

                in
            stat  
        in




        let write_inode ~obj
                        ~addr
                        ~data
                        ~size
            =
            let bsize =  ceil_block size def_block_size in

#ifdef DEBUG
            Db.Pr.sdd 2 "server: write_inode: [baddr,bsize]" addr bsize;
#endif
            let stat =
                disk_write vdisk_inode_cap
                                  ~start:addr
                                  ~num:bsize 
                                  ~blksize:def_block_size
                                  ~buf:data
                                  ~pos:0;  
                in
            stat  
        in



        sys_log Sys_start "DNS: Creating inode cache... \n";

        (*
        ** NOP
        *)
        let sync_inode ~obj = () in

        (*
        ** We need two caches; one for the inodes,
        ** and the second for the directory entry data.
        **
        ** Note: All inodes are handled within ONE cache object of
        ** size = ninodes * size_INODE!
        *)

        let stat,cache_inode = cache_create 
                                   ~nbufs:cache.c_inode_buffers
                                   ~blocksize:block_size
                                   ~bufsize:cache.c_inode_size   
                                   ~read:read_inode
                                   ~write:write_inode
                                   ~sync:sync_inode
                                   ~mode:Cache_R
        in
        if (stat <> std_OK) then
        begin
            sys_log Sys_fatal "can't create inode cache: %s\n" (err_why stat);
            raise (Error stat);
        end;

        (*
        ** The inode cache object. One object for all inodes!
        ** Disk address = obj = 1 [blocks]
        *)

        let _,inode_fse = cache_lookup ~cache:cache_inode
                                     ~obj:1     (* phys offset = 512 bytes ! *)      
                                     ~addr:1
                                     ~size:(off_of_inode super.dns_ndirs)
                                     ~state:FF_unlocked
        in

        (*
        ** Some inode utils
        *)

        let inode_of_obj obj =
          try
          begin
            (*
            ** Get the AFS capability.
            *)
            let inodeb = buf_create inode_SIZE in
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in
            let stat,inode = if (stat <> std_OK) then
                                stat,nilinode
                             else
                             begin
                                let stat,inode = 
                                    try
                                    begin
                                        let _,inode = 
                                                buf_get_inode ~buf:inodeb
                                                              ~pos:0 
                                        in
                                        std_OK,inode
                                    end
                                    with
                                        _ -> std_SYSERR,nilinode
                                in
                                stat,inode
                             end in
            if ((to_state inode.i_state) <> FF_invalid) then
                stat,inode
            else
                std_NOTFOUND,nilinode
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: inode_of_obj: buffer overflow. Fatal.\n";
                std_SYSERR,nilinode
            end;
        in

        let cap_of_obj obj =
            let _,inode = inode_of_obj obj in
            
            let cap = inode.i_caps.(!fs_default) in
            let ok = let istate = to_state inode.i_state in
                        match istate with
                        | FF_locked | FF_unlocked -> std_OK;
                        | _ -> std_SYSERR;
                    in
            (if ok = std_OK then stat else ok),cap,inode
        in

        let update_inode inode =
          try
          begin
            if (inode.i_objnum = 0) then
                failwith "update_inode: inode with object 0";

            let inodeb = buf_create inode_SIZE in
            let _ = buf_put_inode ~buf:inodeb
                                  ~pos:0
                                  ~inode:inode in

            let stat = cache_write ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode inode.i_objnum)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in
            stat
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: update_inode: buffer overflow. Fatal.\n";                
                std_SYSERR
            end;

        in



        let cache_data = Cache.create ~size:cache.c_dir_buffers in

        sys_log Sys_start "DNS: Inode Cache -> %8d buffers of size %8d bytes\n"
                      cache.c_inode_buffers
                      (block_size*cache.c_inode_size);
        sys_log Sys_start "DNS: Dir Cache   -> %8d entries\n"
                          cache.c_dir_buffers;
        (*
        ** Round up the value x to block_size
        *)

        let block x = ceil_block x block_size in

    

        (*
        ** Read the inode table.
        ** Build a list with free inode object numbers below the
        ** nextfree boundary (above nextfree there are all
        ** free inodes, if any).
        *)

        sys_log Sys_start "DNS: Reading the Inode Table... \n";
    
        let freeino = ref [] in

        let firstfree = ref (-1) in
        let nextfree = ref (-1) in
        let nused = ref 0 in

        let inodeb = buf_create inode_SIZE in

        for i = 1 to (super.dns_ndirs-1)
        do
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in

            if (stat <> std_OK) then
                raise (Error std_IOERR);
            
            let pos,inode = buf_get_inode ~buf:inodeb ~pos:0 in
            let istate = to_state inode.i_state in

            (*
            ** Some sanity checks first
            *)

            if (inode.i_objnum <> i) then
            begin
                sys_log Sys_fatal "\nDNS: Invalid Inode entry. Abort\n";
                sys_log Sys_fatal "DNS: inode number expected %d, but got %d.\n"
                              i inode.i_objnum;
                raise (Error std_SYSERR);
            end;

            if (istate = FF_invalid ||
                istate = FF_unlocked) then
            begin

                live_set live_table ~obj:i ~time:0 ~flag:0;

                if (!nextfree <> -1 && !nextfree <> i-1) then
                begin
                    for j = !firstfree to !nextfree
                    do
                        freeino := !freeino @ [j];
                    done;
                    firstfree := i;
                    nextfree := i;
                end
                else 
                begin
                    nextfree := i;
                    if (!firstfree = -1) then
                        firstfree := i;
                end;

                if (istate = FF_unlocked) then
                begin
                    sys_log Sys_start 
                            "DNS: Unlocked directory found. Destroy it: %d\n"   
                            i;

                    inode.i_state <- of_state FF_invalid;
                    let pos = buf_put_inode ~buf:inodeb ~pos:0 
                                            ~inode:inode
                    in
                    ignore (cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE);

                end;
            end
            else
            begin
                let time,_ = live_get live_table ~obj:i in
                live_set live_table ~obj:i ~time:time ~flag:1;
                incr nused;
            end;    
        done;

        if (!nextfree <> -1 && !nextfree < super.dns_ndirs-1 ) then
        begin
            (*
            ** There are only used inodes at the end of the inodetable.
            *)
            for j = !firstfree to !nextfree
            do
                freeino := !freeino @ [j];
            done;
            nextfree := -1;
        end
        else if (!firstfree = 0 && !nextfree = super.dns_ndirs-1) then
        begin
            (*
            ** Empty filesystem.
            *)
            nextfree := 1
        end
        else if (!firstfree > 0) then
        begin
            (*
            ** There are free inodes at the end of the inodetable.
            *)
            nextfree := !firstfree;
        end;

        super.dns_nused <- !nused;
        super.dns_freeobjnums <- !freeino;
        super.dns_nextfree <- !nextfree;

        sys_log Sys_start "DNS: Found %d used Inode(s).\n"
                          super.dns_nused;

        (*
        ** Read and write directory content. 
        ** Directory contents are saved in AFS files.
        *)
            

        let read_dir  ~inode
            =
            try
            begin
                let fs_default = super.dns_fs_server.fs_default in
                let cap = inode.i_caps.(fs_default) in
                let size = inode.i_size in
                let dirbuf = buf_create size in

                (*
                ** Read the data...
                *)

                let stat,n = afs_read ~cap:cap
                                      ~offset:0
                                      ~buf:dirbuf
                                      ~size:size
                in
                if (stat <> std_OK) then
                    raise (Error stat);

                (*
                ** and decode the directory
                *)

                let _,dir = buf_get_dir ~buf:dirbuf
                                        ~pos:0 
                in

                std_OK,dir                
            end
            with
                | Error err -> err,nildnsdir
        in


        (*
        ** The create, read, modify and utility functions needed
        ** for the Dns_server module. 
        **
        ** Note:
        ** After a directory is locked, any modification of this
        ** directory leads to a new AFS file object and a new
        ** directory capability!
        ** In one copy mode, the old cap will be destroyed, after
        ** the new one is committed and the inode mapping was
        ** updated.
        *)

        let dns_read_dir ~obj
          =
          stats.op_read <- stats.op_read + 1;
          try
          begin

#ifdef DEBUG
            Db.Pr.sd 1 "dns_read_dir obj" obj;
#endif

            (*
            ** Get the inode mapping
            *)
            let stat,inode = inode_of_obj obj in
            let size = inode.i_size in

            if (stat <> std_OK) then
                raise (Error stat);

#ifdef DEBUG
            Db.Pr.ss 1 "dns_read_dir: get inode stat" (err_why stat);
            Db.Pr.ss 1 "dns_read_dir: fscap" (ar_cap inode.i_caps.(0));
            Db.Pr.sd 1 "dns_read_dir: size" size;
#endif


            let dir = try
                        Cache.lookup  
                                   ~cache:cache_data
                                   ~key:inode.i_objnum
                        with
                        | Not_found ->
                        begin
                            let stat,dir = read_dir inode in
                            if (stat <> std_OK) then
                                raise (Error stat);
                            dir
                        end;
                in

            dir.dd_live <- (let l,_ = live_get live_table 
                                                   dir.dd_objnum in l);
            dir,std_OK
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: read_dir: buffer overflow. Fatal.\n";
                nildnsdir,std_SYSERR
            end;
            | Error err -> nildnsdir,err;
        in

        let dns_create_dir ~dir =
          stats.op_create <- stats.op_create + 1;
          try
          begin
                let obj = dir.dd_objnum in
                let fs = super.dns_fs_server in
                let fs_default = super.dns_fs_server.fs_default in
                (*
                ** Create a new AFS object. If in two copy mode,
                ** the copy will be duplicated later.
                *)

                let stat,cap = afs_create ~cap:fs.fs_cap.(fs_default)
                                      ~buf:nilbuf
                                      ~size:0
                                      ~commit:afs_UNCOMMIT
                in

                if (stat <> std_OK) then
                    raise (Error stat);
#ifdef DEBUG
                Db.Pr.ss 1 "dns_create_dir: [afscap]" (ar_cap cap);
#endif
                dir.dd_state <- DD_unlocked;
                live_set live_table ~obj:obj ~time:dns_MAXLIVE ~flag:1;

                let inodeb = buf_create inode_SIZE in
                (*
                ** First read the inode from disk to update
                ** the inode cache (reads/writes always full blocks!).
                *)
                let stat = cache_read
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in

                if (stat <> std_OK) then
                    raise (Error stat);
        

                let inode = { i_objnum = obj;
                              i_caps = (  let ca =
                                                Array.create 2 nilcap in
                                          ca.(fs_default) <- cap;
                                          ca
                                       );
                              i_size = 0;
                              i_state = of_state FF_unlocked;
                            } in
                (*
                ** Code the inode ...
                *)

                if (obj = 0) then
                    failwith "dns_create_dir: inode with object 0";

                let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:inode in
                
                (*
                ** Write the inode data through the cache to disk ...
                *) 

                let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
    

                if (stat <> std_OK) then
                    raise (Error stat);

                Cache.add ~cache:cache_data
                          ~key:obj
                          ~data:dir;
                std_OK
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: create_dir: buffer overflow. Fatal.\n";
                std_SYSERR
            end;
            | Error err -> err;
        in

        let dns_modify_dir ~dir =
          stats.op_modify <- stats.op_modify + 1;
          try
          begin
            let obj = dir.dd_objnum in
            let fs = super.dns_fs_server in
            let fs_default = fs.fs_default in
            let stat,inode = inode_of_obj obj in

            if (stat <> std_OK) then
                raise (Error stat);

            (*
            ** Distinguish these two cases:
            **  dd_state = DD_modified -> a new AFS object must be created
            **  dd_state = DD_unlocked -> commit the AFS object
            *)

            if (dir.dd_state = DD_modified) then
            begin
                (*
                ** Create a new AFS object. If in two copy mode,
                ** the copy will be duplicated later.
                ** Destroy the old capabilities after this oepration
                ** succeeded.
                *)

                let oldcaps = inode.i_caps in
                
                let dirsize = dir_size dir in
                let dirbuf = buf_create dirsize in

                (*
                ** Code  the directory content ...
                *)

                let _ = buf_put_dir ~buf:dirbuf
                                    ~pos:0
                                    ~dir:dir in 

                let stat,cap = afs_create ~cap:fs.fs_cap.(fs_default)
                                      ~buf:dirbuf
                                      ~size:dirsize
                                      ~commit:afs_SAFETY
                in
                if (stat <> std_OK) then
                begin
#ifdef DEBUG
                    Db.Pr.ss 1 "dns_modify_dir: afs_create failed:"
                                (err_why stat);
#endif
                    raise (Error stat);        
                end;

                dir.dd_state <- DD_locked;
                let inodeb = buf_create inode_SIZE in
                let inode' = { i_objnum = obj;
                              i_caps = (  let ca =
                                                Array.create 2 nilcap in
                                          ca.(fs_default) <- cap;
                                          ca
                                       );
                              i_size = dirsize;
                              i_state = of_state FF_locked;
                            } in
                (*
                ** Code the inode ...
                *)
                if (obj = 0) then
                    failwith "dns_modify_dir: inode with object 0";

                let _ = buf_put_inode ~buf:inodeb
                                          ~pos:0
                                          ~inode:inode' in
                
                (*
                ** Write the inode data through the cache to disk ...
                *) 

                let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
            
                (*
                ** Destroy the old caps.
                *)
                for i = 0 to 1
                do
                    if (oldcaps.(i) <> nilcap &&
                        fs.fs_state.(i) = FS_up) then
                    begin
                        let stat = std_destroy ~cap:oldcaps.(i) in
                        if (stat <> std_OK) then
                        begin
                            sys_log Sys_err
                                    "DNS: can't destroy oldcap: %s (%s)\n"
                                              (ar_cap oldcaps.(i))
                                              (err_why stat);
                        end;
                    end;
                done;
                std_OK
            end
            else (* dir.dd_state = DD_unlocked *)
            begin
                let dirsize = dir_size dir in
                let dirbuf = buf_create dirsize in
                (*
                ** Code  the directory content ...
                *)

                let _ = buf_put_dir ~buf:dirbuf
                                    ~pos:0
                                    ~dir:dir in 

                let stat,_ = afs_modify ~cap:inode.i_caps.(fs_default)
                                      ~buf:dirbuf
                                      ~offset:0
                                      ~size:dirsize
                                      ~commit:afs_SAFETY
                in
                if (stat <> std_OK) then
                begin
#ifdef DEBUG
                    print_string ("dns_modify_dir: afs_modify failed:"^
                                   (err_why stat)^"("^
                                   (ar_cap inode.i_caps.(fs_default))^")");
                    print_newline ();
#endif
                    raise (Error stat);
                end;

                dir.dd_state <- DD_locked;
                let inodeb = buf_create inode_SIZE in
                inode.i_size <- dirsize;
                inode.i_state <- of_state FF_locked;

                (*
                ** Code the inode ...
                *)
                if (obj = 0) then
                    failwith "dns_modify_dir: inode with object 0";

                let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:inode in
                
                (*
                ** Write the inode data through the cache to disk ...
                *) 

                let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
            
                if (stat <> std_OK) then
                    raise (Error stat);
                std_OK
            end
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: modify_dir: buffer overflow. Fatal.\n";
                std_SYSERR
            end;
            | Error err -> err;
        in


        let dns_delete_dir ~dir =
          stats.op_destroy <- stats.op_destroy + 1;
          try
          begin
            let obj = dir.dd_objnum in

            live_set live_table ~obj:obj ~time:0 ~flag:0;
            (*
            ** Get the inode mapping
            *)

            let stat,inode = inode_of_obj obj in
            let size = inode.i_size in
            let oldcaps = inode.i_caps in
            let fs = super.dns_fs_server in

            (*
            ** Destroy first the old caps
            *)
            for i = 0 to 1
            do
                if (oldcaps.(i) <> nilcap &&
                    fs.fs_state.(i) = FS_up) then
                begin
                        let stat = std_destroy ~cap:oldcaps.(i) in
                        if (stat <> std_OK) then
                        begin
                            sys_log Sys_err 
                                    "DNS: can't destroy oldcap: %s (%s)\n"
                                                  (ar_cap oldcaps.(i))
                                                  (err_why stat);
                        end;
                end;
            done;
            (*
            ** Now destroy ourself!
            ** Write the inode data through the cache to disk ...
            *) 
            inode.i_state <- of_state FF_invalid;
            inode.i_size <- 0;
            inode.i_caps <- [|nilcap;nilcap|];

            (*
            ** Code the inode ...
            *)
            if (obj = 0) then
                    failwith "dns_delete_dir: inode with object 0";

            let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:inode in
                
            let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
            (*
            ** Invalidate cache entry.
            *)
            Cache.invalidate ~cache:cache_data
                             ~key:inode.i_objnum;
            stat
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: dns_delete_dir: buffer overflow. Fatal.\n";
                std_SYSERR
            end;
        in

        let dns_delete_inode ~obj =
          let stat = ref std_OK in
          stats.op_destroy <- stats.op_destroy + 1;
          try
          begin

            live_set live_table ~obj:obj ~time:0 ~flag:0;
            (*
            ** Get the inode mapping
            *)

            let stat',inode = inode_of_obj obj in
            let size = inode.i_size in
            let oldcaps = inode.i_caps in
            let fs = super.dns_fs_server in

            (*
            ** Destroy ourself without destroying AFS
            ** Write the inode data through the cache to disk ...
            *) 
            inode.i_state <- of_state FF_invalid;
            inode.i_size <- 0;
            inode.i_caps <- [|nilcap;nilcap|];

            (*
            ** Code the inode ...
            *)
            if (obj = 0) then
                    failwith "dns_delete_inode: inode with object 0";

            let _ = buf_put_inode ~buf:inodeb
                                      ~pos:0
                                      ~inode:inode in
                
            let stat' = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
            stat := stat';
            (*
            ** Invalidate cache entry.
            *)
            if (stat' = std_OK) then
                Cache.invalidate ~cache:cache_data
                                 ~key:inode.i_objnum;
            !stat
          end
          with
            | Buf_overflow -> 
            begin
                sys_log Sys_fatal "DNS: dns_delete_inode: buffer overflow. Fatal.\n";
                std_SYSERR
            end;
            | Error stat -> stat;
            | Not_found -> 
                (* Cache.invalidate ??? *)
                !stat;
        in

        let dns_read_super () =
            super,std_OK
        in

        
        let dns_stat () =
            let str = ref "DNS server status\n" in
            str := !str ^ "Inode cache statistics:\n" ^
                    (cache_stat cache_inode)^ 
                "\nServer statistics\n\n"^
                (sprintf "%s   %s   %s\n"
                    (sprintf "Read:         %8d" 
                             (stats.op_read))
                    (sprintf "Modify:       %8d"
                             (stats.op_modify))
                    (sprintf "Create:       %8d"
                             (stats.op_create))
                )^
                (sprintf "%s   %s   %s\n"
                    (sprintf "Touch:        %8d" 
                             (stats.op_touch))
                    (sprintf "Age:          %8d"
                             (stats.op_age))
                    (sprintf "Destroy:      %8d"
                             (stats.op_destroy))
                );

            std_OK,!str
        in

        let dns_touch ~dir =
            stats.op_touch <- stats.op_touch + 1;
            (*
            ** Keep object internally alive.
            *)
            dir.dd_live <- dns_MAXLIVE;
            live_set live_table ~obj:dir.dd_objnum 
                                ~time:dns_MAXLIVE
                                ~flag:1;
            (*
            ** Keep object(s) externally alive.
            *)
            let stat,_,inode = cap_of_obj dir.dd_objnum in
            if (stat = std_OK) then
            begin
                for i = 0 to 1 
                do
                    let cap = inode.i_caps.(i) in
                    if (cap <> nilcap) then
                    begin
                        let stat = std_touch cap in
#ifdef DEBUG
                        Printf.printf "dns_touch: cap %s obj %d" (Ar.ar_cap cap) dir.dd_objnum;
                        nl();
#endif
                        if (stat <> std_OK && stat <> std_NOTFOUND &&
                            stat <> rpc_FAILURE) then
                        begin
                            sys_log Sys_err
                                    "dns_touch: touch of obj %d failed: %s\n"
                                    dir.dd_objnum (err_why stat);
                        end;
                    end;
                done;
            end;
        in

        let dns_age ~obj =
            mu_lock super.dns_lock;

            stats.op_age <- stats.op_age + 1;
            let oldlive,flag = live_get live_table obj in
#ifdef DEBUG
            Db.Pr.sdd 1 "dns_age: obj,live" obj oldlive;
#endif
            if (flag = 1 && oldlive > 1) then
            begin
                live_set live_table ~obj:obj ~time:(oldlive-1) ~flag:flag;
                mu_unlock super.dns_lock;
                true,oldlive-1;
            end
            else if (flag = 1) then
            begin
                live_set live_table ~obj:obj ~time:0 ~flag:flag;
                mu_unlock super.dns_lock;
                true,0;
            end
            else
            begin
                mu_unlock super.dns_lock;
                false,0
            end
        in
    
        let dns_sync () =
            std_OK
        in

        let dns_exit () =
            sys_log Sys_info "DNS: writing live table...\n";
            let stat = live_write live_table in
            stat
        in

        sys_log Sys_start "DNS: reading root directory...";
        let rootdir,stat = dns_read_dir 1 in
        let rootcap = 
            if stat <> std_OK then
            begin
                print_string ("failed: "^(err_why stat));
                nilcap
            end
            else
            begin
                sys_log Sys_start " OK.\n";
                {   
                    cap_port = super.dns_putport;
                    cap_priv = prv_encode
                                    ~obj:(Objnum rootdir.dd_objnum)
                                    ~rights:prv_all_rights
                                    ~rand:rootdir.dd_random;
                };
            end;
            in


        let server = {
            dns_super = super;
            dns_read_dir = dns_read_dir;
            dns_modify_dir = dns_modify_dir;
            dns_create_dir = dns_create_dir;
            dns_delete_dir = dns_delete_dir;
            dns_delete_inode = dns_delete_inode;
            dns_read_super = dns_read_super;
            dns_sync = dns_sync;
            dns_stat = dns_stat;
            dns_touch = dns_touch;
            dns_age = dns_age;            
            dns_exit = dns_exit;
            dns_time = (fun () -> int_of_float((Sys.clock())/.10.0));
        } in
        std_OK,server,rootcap
    end
    with
        | Error err -> 
                sys_log Sys_fatal "\nDNS: Abort: %s\n" (err_why err);
                err,nildnsserver,nilcap
        | Buf_overflow -> 
                sys_log Sys_fatal "\nDNS: Abort: Buffer Overflow\n";
                std_OVERFLOW,nildnsserver,nilcap
        | _ -> 
                sys_log Sys_start "\nDNS: Abort: Input-Output Error\n";
                std_IOERR,nildnsserver,nilcap
