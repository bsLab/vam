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
**    $VERSION:     1.31
**
**    $INFO:
**
** The main module of the Atomic Filesystem Server
**
** UNIX version.
**
** TODO: Support for UNIX block devices.
**
**
**
**    $ENDOFINFO
**
*)




open Amoeba
open Bytebuf
open Thread
open Stderr
open Stdcom
open Buf
open Unix
open Syslog

open Afs_common
open Afs_server
open Afs_server_rpc
open Afs_cache
open Freelist

module String = StringLabels


let server_version = "1.10"


let _ = Db.set_level 2

(*
** Disk layout
**
**  Inode table Partition:
**
**  -------------------------
**  512 bytes: 
**      32 bytes    : magic header
**      480 bytes   : super structure
**  -------------------------
**  Inode Table
**
**  -------------------------
**  Livetime table
**  -------------------------
**    
**
*)


(*
** Default filesystem (data) block size. This should be
** the smallest value supported by the underlying OS hardware
** layer! This block size is always used for the inode partition.
*)

let def_block_size  = 512


(*
** Raw disk structures.
*)

let sizeof_int32 = 4
let sizeof_int16 = 2

type inode = {
    mutable i_file_num:     int;
    mutable i_disk_addr:    int;
    mutable i_disk_size:    int;
    mutable i_disk_res:     int;
    mutable i_state:        int;
    mutable i_time:         int;
    mutable i_random:       port;
}

let nilinode = { i_file_num=0; i_disk_addr=0; i_disk_size=0;
                 i_disk_res=0; i_state=0; i_time=0; i_random=nilport} 

(*
** Buffersize for each inode (== sizeof(inode)).
** Rounded up to fit mutiple times in one 512 byte block. 
*)

let inode_SIZE = 
    let size = 5 * sizeof_int32 + 1 * sizeof_int16 +
               port_SIZE 
    in
    if size <= 32 then
        32
    else if size <= 64 then
        64
    else if size <= 128 then
        128
    else
        failwith "inode_SIZE: out of range"


(*
** Store and extract inodes in a machine independent way.
*)

let buf_get_inode ~buf ~pos =
    let start = pos in
    let pos,inum   = buf_get_int32 ~buf:buf ~pos:pos in
    let pos,iaddr  = buf_get_int32 ~buf:buf ~pos:pos in
    let pos,isize  = buf_get_int32 ~buf:buf ~pos:pos in
    let pos,ires   = buf_get_int32 ~buf:buf ~pos:pos in
    let pos,istate = buf_get_int16 ~buf:buf ~pos:pos in
    let pos,itime  = buf_get_int32 ~buf:buf ~pos:pos in
    let pos,irand  = buf_get_port ~buf:buf ~pos:pos in
    (start+inode_SIZE),{
        i_file_num      = inum;
        i_disk_addr     = iaddr;
        i_disk_size     = isize;
        i_disk_res      = ires;
        i_state         = istate;
        i_time          = itime;
        i_random        = irand;
    }   

let buf_put_inode ~buf ~pos ~inode =
    let start = pos in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_file_num in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_disk_addr in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_disk_size in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_disk_res in
    let pos = buf_put_int16 ~buf:buf
                            ~pos:pos
                            ~int16:inode.i_state in
    let pos = buf_put_int32 ~buf:buf
                            ~pos:pos
                            ~int32:inode.i_time in

    let pos = buf_put_port ~buf:buf ~pos:pos
                                    ~port:inode.i_random in

    (start+inode_SIZE)


(*
** Convert the file state to an integer value
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
        




(*
** Cache parameters
*)

(*
** Default Number of cache entries for the inode cache
*)

let def_inode_ENTRIES = 500

let def_inode_SIZE = 1                  (* one block !!! *)


(*
** Default Number of cache entries for the data cache
*)

let def_data_ENTRIES = 200

(*
** Default Data buffer size (multiple of block_size)
*)

let def_data_BUFSIZE = 8 * def_block_size

(*
** Age periodically the data cache and remove timeout 
** cache objects [sec].
*)

let cache_GC_TIME = 10



(*
** Beacuse on file creation and modification, the total size
** of a file is not know in advance, it's necessary to reserve
** disk space in multiples of the def_res_SIZE. After
** the file was committed, the not used part of the last reserved
** disk cluster is returned to the free cluster list. [bytes]
*)

let def_res_SIZE = def_block_size * 2048


(*
** Default Partition names and paths (UNIX files)
*)

let def_lpath = "/unix/amoeba/afs"
let def_part_inode = "ldisk:00"
let def_part_data = "ldisk:01"

(*
** Disk Magic Labels (32 Bit AFS = 1 GByte (!!) maximal file size)
*)

let magic_str1 = "AMOEBA::AFS::INODE::032" 
let magic_str2 = "AMOEBA::AFS::DATAX::032" 


(*
** Cache parameters
*)

type cache_param = {
    mutable c_inode_buffers: int; (* Number of inode cache buffers           *)
    mutable c_inode_size: int;    (* Size of one inode cache buffer [blocks] *)

    mutable c_data_buffers: int;  (* Number of data cache buffers            *)
    mutable c_data_size: int;     (* Size of one data buffer [blocks]        *)
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
** Create the filesystem (two partitions = UNIX files). 
**
** Args:
**  label:      the filesystem label string [max 256 chars]
**  ninodes:    number of inodes ( = maximal number of files)
**  blocksize:  the block size  [bytes]
**  nblocks:    number of data blocks (block_size * nblocks = data size)
**  part_inode:     Partition A path and name
**  part_data:     Partition B path and name
**  
** Return:
**  status
**  afs_super 
*)

open Printf

let create_unix_fs
        ~label 
        ~ninodes
        ~blocksize
        ~nblocks
        ~part_inode
        ~part_data
        ~overwrite
    =
    
    try
    begin
        sys_log Sys_info "AFS: Creating Amoeba filesystem...\n"; 
        sys_log Sys_info "AFS: Blocksize: %8d [bytes]\n" blocksize;
        sys_log Sys_info "AFS: Number of total blocks: %8d\n" nblocks;
        sys_log Sys_info "AFS: Number of total inodes: %8d\n" ninodes;
    

        (*
        ** First create a server port and derive the public port.
        *)
        let priv_port = uniqport () in
        let pub_port  = priv2pub priv_port in
        let checkfield = uniqport () in

        (*
        ** The partition magic headers. After the magic string,
        ** the value 0xaa is written. 
        *)

        let magic1 = buf_create 32 in
        let magic2 = buf_create 32 in
    
        Bytebuf.fill magic2 ~pos:0 ~len:32 0xaa;
        ignore (buf_put_string ~buf:magic1 
                               ~pos:0
                               ~str:magic_str1);


        Bytebuf.fill magic2 ~pos:0 ~len:32 0xaa;
        ignore (buf_put_string ~buf:magic2
                               ~pos:0
                               ~str:magic_str2);

        (*
        ** The disk super structure.
        *)

        let super1 = buf_create 480 in
        let empty2 = buf_create 480 in

        Bytebuf.fill super1 ~pos:0 ~len:480 0xaa;
        Bytebuf.fill empty2 ~pos:0 ~len:480 0xaa;


        let pos = buf_put_string ~buf:super1 
                                 ~pos:0 ~str:label in
        let pos = buf_put_int32  ~buf:super1
                                 ~pos:256 ~int32:ninodes in
        let pos = buf_put_int32  ~buf:super1
                                 ~pos:pos ~int32:blocksize in
        let pos = buf_put_int32  ~buf:super1
                                 ~pos:pos ~int32:nblocks in
        let pos = buf_put_port   ~buf:super1
                                 ~pos:pos ~port:priv_port in
        let pos = buf_put_port   ~buf:super1
                                 ~pos:pos ~port:pub_port in
        let pos = buf_put_port   ~buf:super1
                                 ~pos:pos ~port:checkfield in

    
        sys_log Sys_info "AFS: inode part -> %s\n" part_inode; 
        sys_log Sys_info "AFS: data part  -> %s\n" part_data;
    
        let pfd1 = Unix.openfile part_inode
                            (if overwrite = false then
                             [O_CREAT;O_EXCL;O_RDWR]
                             else
                             [O_CREAT;O_RDWR])
                             384
        in
        let pfd2 = Unix.openfile part_data
                            (if overwrite = false then
                             [O_CREAT;O_EXCL;O_RDWR]
                             else
                             [O_CREAT;O_RDWR])
                             384
        in

        sys_log Sys_info "AFS: Writing partition magic headers... "; 

        let wrn = Unix.writeb pfd1 magic1 0 32 in
        if (wrn <> 32) then
            raise (Error std_IOERR);
        
        let wrn = Unix.writeb pfd2 magic2 0 32 in    
        if (wrn <> 32) then
            raise (Error std_IOERR);

        sys_log Sys_info "Done.\n";

        sys_log Sys_info "AFS: Writing super structure... "; 

        let wrn = Unix.writeb pfd1 super1 0 480 in
        if (wrn <> 480) then
            raise (Error std_IOERR);

        let wrn = Unix.writeb pfd2 empty2 0 480 in
        if (wrn <> 480) then
            raise (Error std_IOERR);

        sys_log Sys_info "Done.\n"; 


        (*
        ** Fill up the partitions to their final sizes.
        *)

        sys_log Sys_info "AFS: Resizing data partition... "; 

        let size_B = (512+nblocks*blocksize) in
        let lsn = Unix.lseek pfd2  size_B SEEK_SET in
        if (lsn <> size_B) then
            raise (Error std_IOERR);

        sys_log Sys_info "Done.\n";

        (*
        ** Write one byte to resize the file to the last position.
        *)

        sys_log Sys_info "AFS: Writing inodes... \n";  

        let lsn = Unix.lseek pfd1 512 SEEK_SET in

        if (lsn <> 512) then
            raise (Error std_IOERR);

        (*
        ** Write the inodes.
        *)
        let buf = buf_create inode_SIZE in

        let inode = {
            i_file_num = 0;
            i_disk_addr = 0;
            i_disk_size = 0;
            i_disk_res = 0;
            i_state = of_state FF_invalid;
            i_time = 0;
            i_random = nilport;
        } in
        for i = 0 to (ninodes-1)
        do
                inode.i_file_num <- i;
                let pos = buf_put_inode ~buf:buf ~pos:0
                                        ~inode:inode
                          in

                let wrn = Unix.writeb pfd1 buf 0 inode_SIZE in

                if (wrn <> inode_SIZE) then
                    raise (Error std_IOERR);
        done;            
    

        let wrn = Unix.write pfd2 " " 0 1 in
        if (wrn <> 1) then
            raise (Error std_IOERR);


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
        *)

        let live_size = ninodes + 1 in
        let live_table = buf_create live_size in


        let live_set lvt ~obj ~time ~flag =
            buf_set lvt obj ((time land 0x7f) lor
                                ((flag lsl 7) land 0x80))
        in

        let live_write lv =
            (*
            ** Unlock the live table
            *)
            let live_off = 512 + (ninodes*inode_SIZE) in
            let n = Unix.lseek pfd1 live_off SEEK_SET in

            live_set lv ~obj:0 ~time:0 ~flag:1;
            let wrn = Unix.writeb pfd1 lv 0 live_size in
            if (wrn <> live_size) then
                std_IOERR
            else
                std_OK
        in                    

        for i = 0 to ninodes-1 
        do
            live_set live_table ~obj:i ~time:afs_MAXLIVE ~flag:0;
        done;

        sys_log Sys_info "AFS: Writing live table... \n"; 

        let stat = live_write live_table in
        if (stat <> std_OK) then
        begin
            close pfd1;
            close pfd2;
            raise (Error stat);
        end;
    
        Unix.close pfd1;
        Unix.close pfd2;

    
        sys_log Sys_info "Finished.\n"; 

        std_OK, 
        {
            afs_lock = mu_create ();
            afs_name = label;
            afs_nfiles = ninodes;
            afs_nused = 0;
            afs_freeobjnums = [];
            afs_nextfree = 0;
            afs_getport = priv_port;
            afs_putport = pub_port;
            afs_checkfield = checkfield;
            afs_block_size = blocksize;
            afs_nblocks = nblocks;
        }
    end
    with
        | Error err -> 
               sys_log Sys_fatal "AFS: Abort: %s\n" (err_why err);
               err,nilafssuper
            
        | Buf_overflow -> 
               sys_log Sys_fatal "AFS: Abort: Buffer Overflow.\n"; 
               std_SYSERR,nilafssuper

        | _ -> 
               sys_log Sys_fatal "AFS: Abort: Input-Output Error.\n";
               std_IOERR,nilafssuper


(*
** Calculate the logical partition offset for an inode
*)

let off_of_inode inode =
    inode * inode_SIZE


(*
** Round up the value x to block_size
*)

let block x block_size =
    ((x+block_size-1)/block_size)*block_size


(*
** A physical disk address to object number mapping of uncommitted
** files (needed by the cache_gc thread).
*)

let obj_of_daddr = ref []

(*
** Server statistics
*)
type afs_stats = {
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
** Start the file system server (two partitions, UNIX files)
**
**  part_inode:     Partition A path and name
**  part_data:     Partition B path and name
**  cache:      Cache parameters
**
**  
** Return:
**  status
**  afs_server
*)

let start_unix_fs 
        ~part_inode
        ~part_data
        ~cache
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

    let nilafsserver = {
                    afs_super = nilafssuper;
                    afs_read_file = (fun ~file ~off ~size ~buf -> std_SYSERR);
                    afs_modify_file = (fun ~file ~off ~size ~buf -> std_SYSERR);
                    afs_modify_size = (fun ~file ~newsize -> std_SYSERR);
                    afs_commit_file = (fun ~file ~flag -> std_SYSERR);
                    afs_read_inode = (fun ~obj -> std_SYSERR,nilafsfile);
                    afs_create_inode = (fun ~file ~final -> std_SYSERR);
                    afs_delete_inode = (fun ~file -> std_SYSERR);
                    afs_modify_inode = (fun ~file -> std_SYSERR);
                    afs_read_super = (fun () -> nilafssuper,std_SYSERR);
                    afs_sync = (fun () -> std_SYSERR);
                    afs_stat = (fun ~obj -> std_SYSERR,"");
                    afs_age = (fun ~obj -> false,0);
                    afs_touch = (fun ~file -> ());
                    afs_exit = (fun () -> std_SYSERR);
                    afs_time = (fun () -> 0);
                } in
    try
    begin

        sys_log Sys_start "AFS: Atomic Filesystem Server, Ver. %s\n%s\n"
                            (server_version)
                          "     (C) 2003-2005 BSSLAB Dr. Stefan Bosse";

        sys_log Sys_start "AFS: Initializing normal mode...\n";

        (*
        ** Either the filesystem is read only or we have an 
        ** inconsistent filesystem. In that case, only read
        ** request are allowed.
        *)

        let readonly = ref false in

        (*
        ** Here we store the inodes and the data in two file partitions.
        *)

        sys_log Sys_start "AFS: Opening partitions...\n"; 

        (*
        ** Open the 'partitions'
        **
        **  Partition A: Inode table and super structure
        **  Partition B: Data area
        **
        *)
        sys_log Sys_start "AFS: inode part -> %s\n" part_inode;
        sys_log Sys_start "AFS: data part  -> %s\n" part_data;

        let part_fd_A = Unix.openfile part_inode [O_RDWR] 384 in
        let part_fd_B = Unix.openfile part_data [O_RDWR] 384 in 

        let close_inode () =
            close part_fd_A
        in
        let close_data () =
            close part_fd_B
        in


        (*
        ** Read the super structrure and the magic header block.
        *)

        sys_log Sys_start "AFS: Reading the Superblock... "; 

        let superb = buf_create 512 in

        let stat = 
            try
            begin
                let n = Unix.lseek part_fd_A 0 SEEK_SET in
                let n = Unix.readb part_fd_A superb 0 512 in

                if (n = 512) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        if (stat <> std_OK) then
        begin
            close_inode ();
            close_data ();
            raise (Error stat);
        end;


        sys_log Sys_start "Done.\n";
        sys_log Sys_start "AFS: Checking the magic Header... ";

        let pos,mg1 = buf_get_string ~buf:superb ~pos:0 in
         
        if (mg1 <> magic_str1) then
        begin
            close_inode ();
            close_data ();
            sys_log Sys_err "failed.\n"; 
            raise (Error std_NOTFOUND);
        end;

        sys_log Sys_start "OK.\n"; 
        
        let super = 
            let pos,str = buf_get_string  ~buf:superb
                                          ~pos:32
                            in
            let pos,ninodes = buf_get_int32 ~buf:superb
                                            ~pos:(256+32)
                            in
        
            let pos,blocksize = buf_get_int32   ~buf:superb
                                                ~pos:pos
                            in

            let pos,nblocks = buf_get_int32   ~buf:superb
                                                ~pos:pos
                            in
        
            let pos,priv_port = buf_get_port ~buf:superb
                                             ~pos:pos
                            in
        
            let pos,pub_port = buf_get_port  ~buf:superb
                                             ~pos:pos
                            in
        
            let pos,checkfield = buf_get_port  ~buf:superb
                                               ~pos:pos
                            in
        
            {
                afs_lock = mu_create ();
                afs_name = str;
                afs_nfiles = ninodes;
                afs_nused = 0;
                afs_freeobjnums = [];
                afs_nextfree = 1;
                afs_getport = priv_port;
                afs_putport = pub_port;
                afs_checkfield = checkfield;
                afs_block_size = blocksize;
                afs_nblocks = nblocks;
            }
        in

        let block_size = super.afs_block_size in

        sys_log Sys_start "AFS: Label = \"%s\"\n" super.afs_name;
        sys_log Sys_start "AFS: Maximal number of files (inodes) = %d\n"
                          super.afs_nfiles;
        sys_log Sys_start "AFS: Blocksize = %d bytes\n"
                          block_size;
        sys_log Sys_start "AFS: Total number of blocks = %d\n"
                          super.afs_nblocks;
        sys_log Sys_start"AFS: Filesystem size = %d bytes (%d MB)\n"
                         (super.afs_nblocks*block_size)
                         (super.afs_nblocks*block_size/1024/1024);

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
        *)

        let live_size = super.afs_nfiles in
        let live_table = buf_create live_size in

        let live_set lvt ~obj ~time ~flag =
#ifdef DEBUG
            Db.Pr.sddd 1 "live_set obj,val,flag" obj time flag;
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
          try
          begin  
            let live_off = 512 + (super.afs_nfiles*inode_SIZE) in
            let n = Unix.lseek part_fd_A live_off SEEK_SET in
            let n = Unix.readb part_fd_A lv 0 live_size in

            if ( n <> live_size) then
                std_IOERR
            else if ((live_get lv ~obj:0) = (0,1)) then
            begin   
                (*
                ** Now lock the live table. After a server crash,
                ** the restarted server will found the lock and must
                ** discard the live table. All server objects got
                ** the maximal livetime!
                *)
                live_set lv ~obj:0 ~time:1 ~flag:1;
                let n = Unix.lseek part_fd_A live_off SEEK_SET in
                let n = Unix.writeb part_fd_A lv 0 512 in
                if ( n <> 512) then
                    std_IOERR
                else
                    std_OK

            end
            else
            begin
                std_ARGBAD;
            end;
          end
          with
            | _ -> std_IOERR
        in

        let live_write lv =
          try
          begin
            (*
            ** Unlock the live table
            *)
            live_set lv ~obj:0 ~time:0 ~flag:1;

            let live_off = 512 + (super.afs_nfiles*inode_SIZE) in
            let n = Unix.lseek part_fd_A live_off SEEK_SET in

            let wrn = Unix.writeb part_fd_A lv 0 live_size in

            if (wrn <> live_size) then
                std_IOERR
            else
                std_OK
          end
          with
            | _ -> std_IOERR
        in

        (*
        ** Read the live table
        *)
        sys_log Sys_start "AFS: Reading the livetime table..."; 

        let stat = live_read live_table in

        if ( stat = std_ARGBAD) then 
        begin
            (*
            ** Found locked live table. Discard it and reinitialize.
            *)
            sys_log Sys_start "found locked live table: Reinitialize";
            for i = 1 to super.afs_nfiles-1
            do
                live_set live_table ~obj:i ~time:afs_MAXLIVE ~flag:0;
            done;            
        end else if (stat = std_IOERR) then
        begin
            close part_fd_A;
            close part_fd_B;
            raise (Error std_IOERR);
        end;
        
        sys_log Sys_start " Ok.\n"; 

        (*
        ** def_res_SIZE must be a multiple of the block_size
        *)
        let def_res_SIZE = block def_res_SIZE block_size in


        (*
        ** Raw disk Read and Write functions for the cache module.
        **
        ** Units:
        **  addr: blocks
        **  size: bytes
        *)

        let read_inode  ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin
                let off = addr * def_block_size in

#ifdef DEBUG
                Db.Pr.sdd 0 "server:read_inode: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_A off SEEK_SET in
                let n = Unix.readb part_fd_A data 0 size in

                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        let write_inode ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin    
                let off = addr * def_block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:write_inode: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_A off SEEK_SET in
                let n = Unix.writeb part_fd_A data 0 size in
                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        (*
        ** NOP
        *)

        let sync_inode ~obj = () in


        let read_data   ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin
                let off = addr * block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:read_data: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_B off SEEK_SET in
                let n = Unix.readb part_fd_B data 0 size in
                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        let write_data  ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin    
                let off = addr * block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:write_data: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_B off SEEK_SET in
                let n = Unix.writeb part_fd_B data 0 size in
                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in


        sys_log Sys_start "AFS: Creating data and inode caches... \n";
        
        (*
        ** First we need two caches; one for the inodes,
        ** and the second for the file data.
        **
        ** Note: All inodes are handled within ONE cache object of
        ** size = ninodes * size_INODE!
        *)

        let stat,cache_inode = cache_create 
                                   ~nbufs:cache.c_inode_buffers
                                   ~blocksize:def_block_size
                                   ~bufsize:cache.c_inode_size   
                                   ~read:read_inode
                                   ~write:write_inode
                                   ~sync:sync_inode
                                   ~mode:Cache_R
        in
        if (stat <> std_OK) then
        begin
            close_inode ();
            close_data ();
            raise (Error stat);
        end;

        (*
        ** The inode cache object. One object for all inodes!
        ** Disk address = obj = 1 [blocks]
        *)

        let _,inode_fse = cache_lookup ~cache:cache_inode
                                     ~obj:1      
                                     ~addr:1
                                     ~size:(off_of_inode super.afs_nfiles)
                                     ~state:FF_unlocked
        in


        (*
        ** File data was synced to disk. Update the inode state
        ** if necessary.
        *)

        let sync_data ~obj =

#ifdef DEBUG
            Db.Pr.sd 1 "sync_data [obj]" obj;
#endif
            mu_lock inode_fse.fse_lock;
            let inodeb = buf_create inode_SIZE in
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in
            if (stat <> std_OK) then
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
            end
            else
            begin
                let pos,inode = buf_get_inode ~buf:inodeb
                                              ~pos:0
                in

                if (inode.i_file_num <> obj) then
                begin
                    sys_log Sys_err "AFS: invalid inode number (got %d, expected %d)\n"
                                    inode.i_file_num
                                    obj;
                    mu_unlock inode_fse.fse_lock;   (* !!! *)
                    raise (Error stat);
                end;

                if (inode.i_state = of_state FF_commit) then
                begin
                    let tmpstat = inode_fse.fse_state in
                    inode_fse.fse_state <- FF_unlocked;
 
                    inode.i_state <- of_state FF_locked;
                    let pos = buf_put_inode ~buf:inodeb ~pos:0 
                                            ~inode:inode
                    in

                    let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in

                    inode_fse.fse_state <- tmpstat;
                    if (stat <> std_OK) then
                        sys_log Sys_err 
                                "AFS: sync_data: inode [%d] update failed: %s\n"
                                inode.i_file_num
                                (err_why stat);
                end;
                mu_unlock inode_fse.fse_lock;   (* !!! *)
            end;
        in

        let stat,cache_data = cache_create 
                                   ~nbufs:cache.c_data_buffers
                                   ~blocksize:block_size
                                   ~bufsize:cache.c_data_size
                                   ~read:read_data
                                   ~write:write_data
                                   ~sync:sync_data
                                   ~mode:Cache_RW
        in
        if (stat <> std_OK) then
        begin
            close_inode ();
            close_data ();
            raise (Error stat);
        end;
    
        sys_log Sys_start "AFS: Inode Cache -> %8d buffers of size %8d bytes\n"
                              cache.c_inode_buffers
                              (block_size*cache.c_inode_size);
        sys_log Sys_start "AFS: Data Cache  -> %8d buffers of size %8d bytes\n"
                              cache.c_data_buffers
                              (block_size*cache.c_data_size);

        (*
        ** Round up the value x to block_size
        *)

        let block x = ceil_block_bytes x block_size in
        let to_block x = to_block x block_size in
        let of_block x = of_block x block_size in


        (*
        ** Now build up the core tables:
        **
        ** 1. List of all free clusters (compounds of blocks)
        ** 2. List of all used clusters (compounds of blocks)
        ** 3. List of all free inodes and the next free (the one
        **    after the last used)
        **
        *)



        (*
        ** Read the inode table.
        ** Build a list with free inodes below the
        ** nextfree boundary (above nextfree there are all
        ** free inodes, if any).
        *)

        sys_log Sys_start "AFS: Reading the Inode Table... \n"; 
    
        let usedclu = ref [] in
        let freeino = ref [] in

        let firstfree = ref (-1) in
        let nextfree = ref (-1) in
        let nused = ref 0 in

        let inodeb = buf_create inode_SIZE in

        for i = 1 to (super.afs_nfiles-1)
        do
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in

            if (stat <> std_OK) then
                raise (Error stat);
            
            let pos,inode = buf_get_inode ~buf:inodeb ~pos:0 in
            let istate = to_state inode.i_state in

            (*
            ** Some sanity checks first
            *)

            if (inode.i_file_num <> i) then
            begin
                close_inode ();
                close_data (); 
                sys_log Sys_fatal "\nAFS: Invalid Inode entry. Abort\n";
                sys_log Sys_fatal
                        "AFS: expected inode number %d, but got %d.\n" 
                        i inode.i_file_num;
                raise (Error std_SYSERR);
            end;

            if (istate <> FF_locked) then
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

                if (istate = FF_unlocked ||
                    istate = FF_commit) then
                begin
                    sys_log Sys_info 
                            "AFS: Unlocked/uncommitted file found. Destroy it:\n%s\n"
                        (
                        "     [obj="^
                        (string_of_int i)^" state="^
                        (string_of_int inode.i_state)^" disk_addr="^
                        (string_of_int inode.i_disk_addr)^" disk_size="^
                        (string_of_int inode.i_disk_size)^"]"
                        ); 

                    inode.i_state <- of_state FF_invalid;
                    inode.i_disk_addr <- 0;
                    inode.i_disk_size <- 0;
                    let pos = buf_put_inode ~buf:inodeb ~pos:0 
                                            ~inode:inode
                    in

                    let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
                    if (stat <> std_OK) then
                        raise (Error stat);
                end;
            end
            else
            begin
                let time,_ = live_get live_table ~obj:i in   
                live_set live_table ~obj:i ~time:time ~flag:1;
                incr nused;
                usedclu := !usedclu @ [(inode.i_disk_addr,
                                        to_block (block inode.i_disk_size))];
            end;    
        done;

#ifdef DEBUG
        Db.Pr.sd 2 "server: [firstfree]" (!firstfree);
        Db.Pr.sd 2 "server: [nextfree]" (!nextfree);
#endif
        
        if (!nextfree <> -1 && !nextfree < super.afs_nfiles-1 ) then
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
        else if (!firstfree = 0 && !nextfree = super.afs_nfiles-1) then
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
    
#ifdef DEBUG
        Db.Pr.sd 2 "server: [nextfree]" (!nextfree);
#endif
        super.afs_nused <- !nused;
        super.afs_freeobjnums <- !freeino;
        super.afs_nextfree <- !nextfree;

        sys_log Sys_start "AFS: Found %d used Inode(s)\n"
                           super.afs_nused;

        (*
        ** Sort the used cluster list with increasing address.
        *)

        let usedcluster = Sort.list ( fun e1 e2 ->
                                let d1,_ = e1 in
                                let d2,_ = e2 in
                                (d1 < d2) 
                            ) !usedclu
        in

        let usedn = List.length usedcluster in
        sys_log Sys_start "AFS: Found %d valid file(s)\n"
                           usedn;

        let freeblocks = free_create 6 super.afs_nblocks in

        (*
        ** Build the free cluster list from the used cluster
        ** list (= holes list) and calculate the total free space.
        *)

        let biggesthole = ref 0 in
        let last = ref (0,0) in
        
        let rec f_iter ul = 
            match ul with
            | hd::tl ->
            begin
                let cd,cs = hd in
#ifdef DEBUG
                Db.Pr.sdd 5 "server: used cluster [da,ds]" cd cs;
#endif
                let ld,ls = !last in

                if (ld+ls = 0 && cd > 0) then
                begin
                    (*
                    ** Free cluster before first used cluster.
                    *)
                    if (cd > !biggesthole) then
                        biggesthole := cd;

                    free_insert freeblocks 
                                {fb_addr=ld;
                                 fb_size=cd;
                                 fb_flag=Cluster_FREE};
                end
                else if (ld+ls <> 0 && ld+ls < cd) then
                begin
                    let size = cd-ld-ls in
                    if ( size > !biggesthole) then
                        biggesthole := size;

                    free_insert freeblocks 
                                {fb_addr=ld+ls;
                                 fb_size=size;
                                 fb_flag=Cluster_FREE};
                end;
                last := (cd,cs);
                f_iter tl;
            end;
            | [] -> 
            begin
                let ld,ls = !last in
                let endd  = super.afs_nblocks in

                if (ld+ls < endd) then
                begin
                    let size = endd-ld-ls in
                    if (size > !biggesthole) then
                        biggesthole := size;

                    free_insert freeblocks 
                                {fb_addr=ld+ls;
                                 fb_size=size;
                                 fb_flag=Cluster_FREE};
                end;
            end;
        in

        f_iter usedcluster;        


        let freeholes,freespace,_ = free_info freeblocks in

        sys_log Sys_start "AFS: Found %d free hole(s)\n"
                           freeholes;
        sys_log Sys_start "AFS: Biggest hole: %d bytes (%d MB)\n"
                          (!biggesthole*block_size)
                          (!biggesthole*block_size/1024/1024);

        if (freespace > super.afs_nblocks) then
        begin
            sys_log Sys_warn "AFS: Warning: inconsistent file system!\n"; 
            sys_log Sys_warn "AFS: Switched to Read only mode.\n";
            readonly := true;
        end;

        sys_log Sys_start "AFS: Total free space: %d bytes (%d MB)\n"
                          (freespace * block_size)
                          (freespace * block_size /1024/1024);
    
        mu_unlock inode_fse.fse_lock;   (* !!! *)


        (*
        ** The create, read, modify and utility functions needed
        ** for the Afs_server module. 
        **
        ** Units:
        **  
        **      off:  bytes
        **      size: bytes
        **
        **
        ** Remember that the cache objects
        ** (inodes and data) are associated with the physical disk address
        ** and belongs to ONE cache object (inode_fse)!
        *)

        let afs_read_file   ~file
                            ~off
                            ~size
                            ~buf
            =
            stats.op_read <- stats.op_read + 1;
            let inode = file.ff_inode in
            
#ifdef DEBUG
            Db.Pr.sdd 2 "server:afs_read_file: (off,size)" off size;
#endif
            let _,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:inode.fi_daddr
                                   ~size:file.ff_size
                                   ~state:file.ff_state
            in

            (*
            ** Now read the data through the cache
            *)
            let stat = cache_read ~cache:cache_data
                                  ~fse:fse
                                  ~buf:buf
                                  ~off:off
                                  ~size:size
            in
            cache_release ~cache:cache_data ~fse:fse;
            stat
        in

        let afs_modify_file ~file
                            ~off
                            ~size
                            ~buf
            =
            stats.op_modify <- stats.op_modify + 1;
            let inode = file.ff_inode in

#ifdef DEBUG
            Db.Pr.sddd 2 "server:afs_modify_file: [obj,off,size]" 
                        file.ff_objnum off size;
#endif            
            let found,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:inode.fi_daddr
                                   ~size:file.ff_size
                                   ~state:file.ff_state
            in

            if (found = true) then
            begin

#ifdef DEBUG
                Db.Pr.sdd 3 "server:afs_modify_file: [fdsize,state]"
                         fse.fse_disk_size (of_state fse.fse_state);
#endif

                (*
                ** Now modify the data through the cache
                *)
                let stat = cache_write ~cache:cache_data
                                   ~fse:fse
                                   ~buf:buf
                                   ~off:off
                                   ~size:size
                in
                cache_release ~cache:cache_data ~fse:fse;
                stat
            end
            else
                std_NOTFOUND;       (* Cache timeout ! *)
        in

        let afs_modify_size ~file
                            ~newsize
            =
            let inode = file.ff_inode in

#ifdef DEBUG
            Db.Pr.sddd 2 "server:afs_modify_size: [obj,newsize,ressize]" 
                        file.ff_objnum newsize
                        inode.fi_res;
#endif
            (*
            ** We always try to reserve def_res_SIZE bytes
            ** disk space. If newsize < reserved size, only return
            ** newsize. In the other case, we must reserve another
            ** cluster of size def_res_SIZE.
            **
            **
            ** First look in the freecluster list for a cluster
            ** starting with address (file.ff_inode.fi_faddr+
            **                        file.ff_inode.fi_res)
            **
            ** If there is no such cluster or the cluster has insufficient
            ** size with respect to newsize, we must allocate a new
            ** cluster and copy the old file to the new position. Worst
            ** case. This must be done in the Afs_server module.
            **
            *)

            (*
            ** Keep the cache object consistent.
            *)

            let found,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:inode.fi_daddr
                                   ~size:file.ff_size
                                   ~state:file.ff_state
            in


            if (found = true && inode.fi_res >= newsize) then
            begin
                (*
                ** Nothing to do. 
                *)

                file.ff_size <- newsize;
                fse.fse_disk_size <- newsize;
                file.ff_modified <- true;

                cache_release ~cache:cache_data ~fse:fse;

#ifdef DEBUG
                Db.Pr.s 2 "server:afs_modify_size: got reserved";
#endif
                std_OK                
            end
            else if (found = true) then
            begin

                mu_lock super.afs_lock;


                (*
                ** Try to get a new contiguous cluster of size
                ** def_res_SIZE.
                *)

                let fdaddr = inode.fi_daddr + (to_block inode.fi_res) in
                let fdsize = to_block (def_res_SIZE) in

#ifdef DEBUG
                Db.Pr.sdd 2 "server:afs_modify_size: [fdaddr,fdsize]"
                            fdaddr fdsize;
#endif

                let fb = free_append freeblocks fdaddr fdsize in

                if (fb <> nilfb) then
                begin
                        (*
                        ** Simple. We got a new contiguous chunk of blocks.
                        *)
                        file.ff_size <- newsize;
                        inode.fi_res <- inode.fi_res + def_res_SIZE;
                        fse.fse_disk_size <- newsize;
                        file.ff_modified <- true;

                        cache_release ~cache:cache_data ~fse:fse;

                        mu_unlock super.afs_lock;
#ifdef DEBUG
                        Db.Pr.s 2 "server:afs_modify_size: Ok";
#endif
                        std_OK
                end
                else
                begin
                        (*
                        ** We can't reserve enough space.
                        ** Try it again with the desired size.
                        *)


                        let fdsize = to_block (block (newsize - file.ff_size))
                                     in
                        let fb = free_append freeblocks fdaddr fdsize in
    
                        if (fb <> nilfb) then
                        begin
                            (*
                            ** We got a new contiguous chunk of blocks.
                            *)
                            file.ff_size <- newsize;
                            inode.fi_res <- inode.fi_res + (of_block fdsize);
                            fse.fse_disk_size <- newsize;
                            file.ff_modified <- true;

                            cache_release ~cache:cache_data ~fse:fse;

                            mu_unlock super.afs_lock;
#ifdef DEBUG
                            Db.Pr.s 2 "server:afs_modify_size: Ok(2)";
#endif
                            std_OK
                    
                        end
                        else
                        begin
                            (*
                            ** No way to get the desired free space.
                            *)
                            cache_release ~cache:cache_data ~fse:fse;
                            mu_unlock super.afs_lock;
                            std_NOSPACE;
                        end;
                end;
            end
            else
                std_NOTFOUND;       (* Cache timeout ! *)
        in
    
        let afs_commit_file ~file
                            ~flag
            =
          stats.op_commit <- stats.op_commit + 1;
            
            let inode = file.ff_inode in

#ifdef DEBUG
            Db.Pr.sdd 1 "server:afs_commit_file: (obj,flag)" file.ff_objnum
                        flag;
#endif
            obj_of_daddr := List.filter (fun d_o ->
                                let d,o = d_o in
                                not (d = inode.fi_daddr) 
                            ) !obj_of_daddr;
            (*
            ** First fix the reserved disk cluster and return the
            ** remaining space to the free cluster list.
            *)

            mu_lock super.afs_lock;

            let dsize = (to_block inode.fi_res) - 
                        (to_block (block file.ff_size)) in
            let daddr = inode.fi_daddr + 
                        (to_block (block file.ff_size)) in

            inode.fi_res <- file.ff_size; 

            if (dsize > 0) then
                free_merge freeblocks {fb_addr=daddr;
                                       fb_size=dsize;
                                       fb_flag=Cluster_FREE};

#ifdef DEBUG
            Db.Pr.sdd 2 "server: afs_commit_file: free [daddr,dsize]"
                        daddr dsize;
#endif
            mu_unlock super.afs_lock;
            
            let found,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:inode.fi_daddr
                                   ~size:inode.fi_res
                                   ~state:file.ff_state
            in
            
            if (found = true) then
            begin

                (*
                ** Flush the cache only with the afs_SAFETY flag set.
                *)

                let stat = if (flag land afs_SAFETY = afs_SAFETY) then
                                cache_commit 
                                    ~cache:cache_data
                                    ~fse:fse
                       else
                            std_OK
                in
                fse.fse_state <- file.ff_state;
                cache_release ~cache:cache_data ~fse:fse;

#ifdef DEBUG
                Db.Pr.ss 2 "server:afs_commit_file: [stat]"
                       (err_why stat);
#endif
                stat
            end
            else
                std_NOTFOUND;       (* Cache timeout ! *)
        in

        let afs_read_inode ~obj 
          =
          try
          begin
            mu_lock inode_fse.fse_lock;   (* !!! *)


#ifdef DEBUG
            Db.Pr.sd 2 "server:afs_read_inode: [obj]" obj;
#endif
            let inodeb = buf_create inode_SIZE in
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode obj)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in
            if (stat <> std_OK) then
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                (stat,nilafsfile)
            end
            else
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                let pos,inode = buf_get_inode ~buf:inodeb
                                              ~pos:0
                in
                if (inode.i_file_num <> obj) then
                begin
                    sys_log Sys_fatal 
                            "AFS: invalid inode number (got %d, expected %d\n"
                            inode.i_file_num
                            obj;
                    raise (Error std_SYSERR);
                end;
                if ((to_state inode.i_state) <> FF_invalid) then
                begin
                  std_OK,
                  {
                    ff_lock = mu_create ();
                    ff_objnum = obj;
                    ff_random = inode.i_random;
                    ff_time = inode.i_time;
                    ff_live = (let l,_ = live_get live_table obj in l);
                    ff_state = to_state inode.i_state;
                    ff_size = inode.i_disk_size;
                    ff_inode = { fi_daddr = inode.i_disk_addr ; 
                                 fi_ioff = off_of_inode obj; 
                                 fi_res = inode.i_disk_res};
                    ff_modified = false;
                  }
                end
                else
                    std_NOTFOUND,nilafsfile
            end;
          end
          with
            | Buf_overflow ->
            begin
                sys_log Sys_fatal "AFS: read_inode: invalid inode structure.\n";
                std_SYSERR,nilafsfile
            end;
        in

        let afs_modify_inode ~file
          =
            mu_lock inode_fse.fse_lock;   (* !!! *)

#ifdef DEBUG
            Db.Pr.sddd 2 "server:afs_modify_inode: [obj,inode,ressize]" 
                          file.ff_objnum file.ff_inode.fi_ioff
                          file.ff_inode.fi_res;
#endif

            let inodeb = buf_create inode_SIZE in
            let inode = {
                i_file_num = file.ff_objnum;
                i_disk_addr = file.ff_inode.fi_daddr;
                i_disk_size = file.ff_size;
                i_disk_res  = file.ff_inode.fi_res;
                i_state = of_state file.ff_state;
                i_time = file.ff_time;
                i_random = file.ff_random;

            } in
            let pos = buf_put_inode ~buf:inodeb ~pos:0
                                    ~inode:inode in 

            let stat = cache_write ~cache:cache_inode
                                   ~fse:inode_fse
                                   ~off:file.ff_inode.fi_ioff
                                   ~buf:inodeb
                                   ~size:inode_SIZE
            in


            (*
            ** Inodes are written always through the cache only if the
            ** the file is locked!
            *)
        
            if (stat = std_OK && (file.ff_state = FF_locked ||
                                  file.ff_state = FF_commit)) then
            begin
                let stat = cache_commit ~cache:cache_inode ~fse:inode_fse
                           in
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                stat
            end
            else
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                stat
            end
        in

        let afs_create_inode ~file ~final =
          stats.op_create <- stats.op_create + 1;
            mu_lock super.afs_lock;
            mu_lock inode_fse.fse_lock;   (* !!! *)

#ifdef DEBUG
            Db.Pr.sdd 2 "server:afs_create_inode: [obj,size]" file.ff_objnum
                                                            file.ff_size;
#endif
            (*
            ** Be aware: inital file size can be zero!
            *)
            let fbsize = if (file.ff_size = 0) then 
                            block_size 
                         else         
                            block file.ff_size
                in

            (*
            ** First get a reasonable data cluster with the
            ** initial size of the file.
            *)
            let fb = if (final = false) then
                        free_new freeblocks
                               (to_block fbsize)
                     else
                        free_match freeblocks
                               (to_block fbsize)
            in

            if (fb <> nilfb) then
            begin  
                let faddr = fb.fb_addr in
                obj_of_daddr := !obj_of_daddr @ [faddr,file.ff_objnum];
                file.ff_inode.fi_daddr <- faddr; 
                file.ff_inode.fi_ioff <- (off_of_inode file.ff_objnum);
                file.ff_inode.fi_res <- fbsize;   (* !!! *)

                (*
                ** First make sure we have the inode in the cache (and
                ** all other inodes in the cached cluster).
                *)
                                      
                let inodeb = buf_create inode_SIZE in
                let stat = cache_read ~cache:cache_inode
                                      ~fse:inode_fse
                                      ~buf:inodeb
                                      ~off:(off_of_inode file.ff_objnum)
                                      ~size:inode_SIZE
                in
                if (stat <> std_OK) then
                begin
                    mu_unlock inode_fse.fse_lock;   (* !!! *)
                    raise (Error std_IOERR);      
                end;
                (*
                ** Overwrite the old content.
                *)

                let inode = {
                    i_file_num = file.ff_objnum;
                    i_disk_addr = file.ff_inode.fi_daddr;
                    i_disk_size = file.ff_size;
                    i_disk_res = file.ff_inode.fi_res;         
                    i_state = of_state file.ff_state;
                    i_time = file.ff_time;
                    i_random = file.ff_random;

                } in
                let pos = buf_put_inode ~buf:inodeb ~pos:0
                                    ~inode:inode in 

                let stat = cache_write ~cache:cache_inode
                                   ~fse:inode_fse
                                   ~off:file.ff_inode.fi_ioff
                                   ~buf:inodeb
                                   ~size:inode_SIZE
                in

                live_set live_table ~obj:file.ff_objnum 
                                    ~time:afs_MAXLIVE ~flag:1;

                mu_unlock inode_fse.fse_lock;   (* !!! *)
                mu_unlock super.afs_lock;

                (*
                ** Create a cache object for the new file.
                ** Simply done with cache_lookup and an immediately
                ** cache_release.
                *)

                let _,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:file.ff_inode.fi_daddr
                                   ~size:file.ff_inode.fi_res
                                   ~state:file.ff_state
                in
                cache_release ~cache:cache_data ~fse:fse;
                stat
            end
            else
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                mu_unlock super.afs_lock;
                std_NOSPACE
            end;
        in    
        
        let afs_delete_inode ~file
          =
          stats.op_destroy <- stats.op_destroy + 1;

            live_set live_table ~obj:file.ff_objnum ~time:0 ~flag:0;

#ifdef DEBUG
            Db.Pr.sddd 2 "server:afs_delete_inode: [obj,size,res]" 
                    file.ff_objnum file.ff_size file.ff_inode.fi_res; 
#endif
            let inode = file.ff_inode in
            (*
            ** First transfer the allocated disk space from this file to
            ** the freecluster list.
            *)

            let _,fse = cache_lookup ~cache:cache_data
                                   ~obj:file.ff_objnum
                                   ~addr:inode.fi_daddr
                                   ~size:inode.fi_res
                                   ~state:file.ff_state
            in
            
            mu_lock super.afs_lock;
            super.afs_nused <- super.afs_nused - 1;

            let daddr = inode.fi_daddr in
            let dsize = to_block (block inode.fi_res) in

            free_merge freeblocks
                       {fb_addr=daddr;
                        fb_size=dsize;
                        fb_flag=Cluster_FREE};

            mu_unlock super.afs_lock;            

            cache_release ~cache:cache_data ~fse:fse;
            ignore(cache_delete ~cache:cache_data ~fse:fse);

            mu_lock inode_fse.fse_lock;   (* !!! *)

            let inodeb = buf_create inode_SIZE in
            let inode = {
                i_file_num = file.ff_objnum;
                i_disk_addr = 0;
                i_disk_size = 0;
                i_disk_res = 0;
                i_state = of_state FF_invalid;
                i_time = 0;
                i_random = nilport;

            } in
            let pos = buf_put_inode ~buf:inodeb ~pos:0
                                    ~inode:inode in 

            let stat = cache_write ~cache:cache_inode
                                   ~fse:inode_fse
                                   ~off:file.ff_inode.fi_ioff
                                   ~buf:inodeb
                                   ~size:inode_SIZE
            in

            (*
            ** Inodes are written always through the cache!
            *)
#ifdef NOTUSED        
            if (stat = std_OK) then
            begin
                let stat = cache_commit ~cache:cache_inode ~fse:inode_fse 
                           in
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                stat
            end
            else
#endif
            begin
                mu_unlock inode_fse.fse_lock;   (* !!! *)
                stat
            end
        in
           
        let afs_read_super () =
            super,std_OK
        in

        let afs_sync () =
            sys_log Sys_info "AFS: syncing disks...\n"; 
            mu_lock super.afs_lock;
            inode_fse.fse_state <- FF_locked;
            let stat1 = cache_sync ~cache:cache_inode in
            let stat2 = cache_sync ~cache:cache_data in
            inode_fse.fse_state <- FF_unlocked;         (* !!! *)
            sys_log Sys_info "AFS: compacting freelist...\n"; 
            let freeblocks' = free_compact freeblocks in
            freeblocks.fbs_array <- freeblocks'.fbs_array;             
            mu_unlock super.afs_lock;
            Gc.compact ();
            if (stat1 <> std_OK) then stat1 else stat2
        in 

        let afs_stat ~obj =
            let holes,freespace,holestat = free_info freeblocks in
            let usedspace = super.afs_nblocks - freespace in
            let uncomm = ref 0 in
            let unlock = ref 0 in
            Hashtbl.iter ( fun addr fse ->
                    if (fse.fse_state = FF_unlocked) then
                        incr unlock;
                    if (fse.fse_state = FF_commit) then
                        incr uncomm;
                ) cache_data.fsc_table;
            
            std_OK,(
                "Cache statistics: Inode Cache\n\n"^
                (cache_stat ~cache:cache_inode)^
                "\nCache statistics: Data Cache\n\n"^
                (cache_stat ~cache:cache_data)^
                "\nFilesystem statistics\n\n"^
                (sprintf "%s  %s\n%s\n%s  %s \n"
                    (sprintf "Free:         %12d bytes" 
                             (freespace*block_size))
                    (sprintf "Used:         %12d bytes"
                             (usedspace*block_size))
                    (sprintf "Files:        %12d      " super.afs_nused)
                    (sprintf "Uncommitted:  %12d      " (!uncomm))
                    (sprintf "Unlocked:     %12d      " (!unlock)))^
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
                )^
                "\nFreelist statistics:\n\n"^
                (sprintf "Holes:        %12d      \n" holes)^
                (
                    let str = ref "" in
                    for i = 0 to (Array.length holestat)-1
                    do
                        let low,high,count = holestat.(i) in
                        str := !str ^
                        (sprintf "Free holes from %12d to %12d bytes: %8d\n"
                                 (low*block_size) 
                                 (high*block_size) count);
                    done;
                    !str
                )

            )
        in
        
        let afs_age ~obj =

            mu_lock super.afs_lock;

            let cur,flag = live_get live_table obj in
            if (flag = 1 && cur > 1) then
            begin
                stats.op_age <- stats.op_age + 1;
                live_set live_table ~obj:obj ~time:(cur-1) ~flag:flag;
                mu_unlock super.afs_lock;
                false,cur-1
            end
            else if (flag = 1) then
            begin
                stats.op_age <- stats.op_age + 1;
                live_set live_table ~obj:obj ~time:0 ~flag:flag;
                mu_unlock super.afs_lock;
                true,0;
            end
            else
            begin
                mu_unlock super.afs_lock;            
                false,0
            end
        in

        let afs_touch ~file =
            stats.op_touch <- stats.op_touch + 1;
            live_set live_table ~obj:file.ff_objnum 
                                ~time:afs_MAXLIVE ~flag:1;
            file.ff_live <- afs_MAXLIVE;
        in

        let afs_exit () =
            sys_log Sys_info "AFS: writing live table...\n"; 
            let stat = live_write live_table in
            stat
        in

        (*
        ** Start the cache garbage collector thread. This thread
        ** ages periodecally the cache and destroys timedout files
        ** (flag = FF_unlocked). Additionally, snyc the data cache.
        *)

        let cache_om () = 
            sys_log Sys_start "AFS: cache_om thread started...\n";
            while (true)
            do
                Unix.sleep cache_GC_TIME;
                let stat = cache_sync ~cache:cache_data in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err "AFS: cache_om -> cache_sync failed: %s\n"
                                  (err_why stat);
                end;
                let stat,killed = cache_age ~cache:cache_data
                                  in
#ifdef DEBUG
                Db.Pr.ss 5 "AFS: cache_om: killed " (err_why stat);
                Db.Pr.sd 5 "AFS: cache_om: killed " (List.length killed);
#endif

                if (stat <> std_OK) then
                begin
                    sys_log Sys_err "AFS: cache_om -> cache_age: %s\n"
                                    (err_why stat);
                    thread_exit ();
                end;
                if (killed <> []) then
                begin
                    (*
                    ** Cleanup the inodes and free the disk space.
                    *)
                    List.iter ( fun d_s ->
                        let daddr,size = d_s in
                        let obj = ref 0 in
                        (*
                        ** Search the list of uncommitted files to
                        ** the killed cache entry. Uncommitted files
                        ** are destroyed if a timeout occured.
                        *)
                        obj_of_daddr := List.filter (fun d_o ->
                                     let d,o = d_o in
                                     if (d = daddr) then
                                     begin
                                        obj := o;
                                        false
                                     end
                                     else
                                        true
                        ) !obj_of_daddr;
                        if (!obj <> 0) then
                        begin 
                            (*
                            ** It's an uncommitted file.
                            *) 
                            let stat',file = afs_read_inode !obj in

                            sys_log Sys_info 
                                    "AFS: cache_om: destroy file %d [Timeout,size= %d].\n"
                                     !obj file.ff_size;

                            let stat' = if (stat' = std_OK) then
                                            afs_delete_inode file 
                                        else
                                            stat'
                                        in
                            if (stat' <> std_OK) then
                            begin
                                sys_log Sys_err 
                                       "AFS: cache_om: afs_delete_inode failed: %s\n"
                                        (err_why stat');
                            end;
                        end;
                    ) killed;
                end;
            done
        in

        ignore(thread_create cache_om ());

        std_OK,{
            afs_super = super;
            afs_read_file = afs_read_file;
            afs_modify_file = afs_modify_file;
            afs_modify_size = afs_modify_size;
            afs_commit_file = afs_commit_file;
            afs_read_inode = afs_read_inode;
            afs_create_inode = afs_create_inode;
            afs_delete_inode = afs_delete_inode;
            afs_modify_inode = afs_modify_inode;
            afs_read_super = afs_read_super;
            afs_sync = afs_sync;
            afs_stat = afs_stat;
            afs_age = afs_age;
            afs_touch = afs_touch;
            afs_exit = afs_exit;
            afs_time = (fun () -> int_of_float ((Sys.clock())/.10.0));
        }
    end
    with
        | Error err ->
                sys_log Sys_fatal "\nAFS: Abort: %s\n" (err_why err);
                err,nilafsserver
        | Buf_overflow -> 
                sys_log Sys_fatal "\nAFS: Abort: Buffer Overflow\n";
                std_OVERFLOW,nilafsserver
        | _ -> 
                sys_log Sys_fatal "\nAFS: Abort: Input-Output Error\n";
                std_IOERR,nilafsserver


(*
** Show and manage filesystem fragmentation
**
** mode:
**  0: Show block allocation
**  1: Show cluster allocation (list form)
**  10: try to merge free clusters
**
*)

let admin_fs         
        ~part_inode
        ~part_data
        ~maxsize
        ~mode
    =
    try
    begin

        sys_log Sys_start "AFS: Atomic Filesystem Server, Ver. %s\n%s\n"
                          (server_version)
                          "     (C) 2003-2005 BSSLAB Dr. Stefan Bosse"; 

        sys_log Sys_start "AFS: Initializing administration mode...\n"; 

        (*
        ** Either the filesystem is read only or we have an 
        ** inconsistent filesystem. In that case, only read
        ** request are allowed.
        *)

        let readonly = ref false in

        (*
        ** Here we store the inodes and the data in two file partitions.
        *)

        sys_log Sys_start "AFS: Opening partitions...\n";

        (*
        ** Open the 'partitions'
        **
        **  Partition A: Inode table and super structure
        **  Partition B: Data area
        **
        *)
        sys_log Sys_start "AFS: inode part -> %s\n" part_inode; 
        sys_log Sys_start "AFS: data part  -> %s\n" part_data; 

        let part_fd_A = Unix.openfile part_inode [O_RDWR] 384 in
        let part_fd_B = Unix.openfile part_data [O_RDWR] 384 in 

        let close_inode () =
            close part_fd_A
        in
        let close_data () =
            close part_fd_B
        in


        (*
        ** Read the super structrure and the magic header block.
        *)

        sys_log Sys_start "AFS: Reading the Superblock... "; 

        let superb = buf_create 512 in

        let stat = 
            try
            begin
                let n = Unix.lseek part_fd_A 0 SEEK_SET in
                let n = Unix.readb part_fd_A superb 0 512 in

                if (n = 512) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        if (stat <> std_OK) then
        begin
            close_inode ();
            close_data ();
            raise (Error stat);
        end;


        sys_log Sys_start "Done.\n" ;
        sys_log Sys_start "AFS: Checking the magic Header... ";

        let pos,mg1 = buf_get_string ~buf:superb ~pos:0 in
         
        if (mg1 <> magic_str1) then
        begin
            close_inode ();
            close_data ();
            sys_log Sys_err "failed.\n"; 
            raise (Error std_NOTFOUND);
        end;

        sys_log Sys_start "OK.\n"; 
        
        let super = 
            let pos,str = buf_get_string  ~buf:superb
                                          ~pos:32
                            in
            let pos,ninodes = buf_get_int32 ~buf:superb
                                            ~pos:(256+32)
                            in
        
            let pos,blocksize = buf_get_int32   ~buf:superb
                                                ~pos:pos
                            in

            let pos,nblocks = buf_get_int32   ~buf:superb
                                                ~pos:pos
                            in
        
            let pos,priv_port = buf_get_port ~buf:superb
                                             ~pos:pos
                            in
        
            let pos,pub_port = buf_get_port  ~buf:superb
                                             ~pos:pos
                            in
        
            let pos,checkfield = buf_get_port  ~buf:superb
                                               ~pos:pos
                            in
        
            {
                afs_lock = mu_create ();
                afs_name = str;
                afs_nfiles = ninodes;
                afs_nused = 0;
                afs_freeobjnums = [];
                afs_nextfree = 1;
                afs_getport = priv_port;
                afs_putport = pub_port;
                afs_checkfield = checkfield;
                afs_block_size = blocksize;
                afs_nblocks = nblocks;
            }
        in

        let block_size = super.afs_block_size in

        sys_log Sys_start "AFS: Label = \"%s\"\n" super.afs_name;
        sys_log Sys_start "AFS: Maximal number of files (inodes) = %d\n"
                          super.afs_nfiles;
        sys_log Sys_start "AFS: Blocksize = %d bytes\n"
                          block_size;
        sys_log Sys_start "AFS: Total number of blocks = %d\n"
                          super.afs_nblocks;
        sys_log Sys_start "AFS: Filesystem size = %d bytes (%d MB)\n"
                          (super.afs_nblocks*block_size)
                          (super.afs_nblocks*block_size/1024/1024);

        

        (*
        ** def_res_SIZE must be a multiple of the block_size
        *)
        let def_res_SIZE = block def_res_SIZE block_size in


        (*
        ** Raw disk Read and Write functions for the cache module.
        **
        ** Units:
        **  addr: blocks
        **  size: bytes
        *)

        let read_inode  ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin
                let off = addr * block_size in

#ifdef DEBUG
                Db.Pr.sdd 0 "server:read_inode: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_A off SEEK_SET in
                let n = Unix.readb part_fd_A data 0 size in

                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        let write_inode ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin    
                let off = addr * block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:write_inode: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_A off SEEK_SET in
                let n = Unix.writeb part_fd_A data 0 size in
                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        (*
        ** NOP
        *)

        let sync_inode ~obj = () in

        let read_data   ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin
                let off = addr * block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:read_data: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_B off SEEK_SET in
                let n = Unix.readb part_fd_B data 0 size in

                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in

        let write_data  ~obj
                        ~addr 
                        ~data
                        ~size
            =
            try
            begin    
                let off = addr * block_size in
#ifdef DEBUG
                Db.Pr.sdd 2 "server:write_data: [off,size]" off size;
#endif
                let n = Unix.lseek part_fd_B off SEEK_SET in
                let n = Unix.writeb part_fd_B data 0 size in
                if ( n = size) then 
                    std_OK
                else
                    std_SYSERR
            end
            with
                | _ -> std_IOERR
        in


        sys_log Sys_start "AFS: Creating inode cache... \n";
        
        (*
        ** First we need two caches; one for the inodes,
        ** and the second for the file data.
        **
        ** Note: All inodes are handled within ONE cache object of
        ** size = ninodes * size_INODE!
        *)

        let stat,cache_inode = cache_create 
                                   ~nbufs:20
                                   ~blocksize:def_block_size
                                   ~bufsize:1
                                   ~read:read_inode
                                   ~write:write_inode
                                   ~sync:sync_inode
                                   ~mode:Cache_R
        in
        if (stat <> std_OK) then
        begin
            close_inode ();
            close_data ();
            raise (Error stat);
        end;

        (*
        ** Round up the value x to block_size
        *)

        let block x =
            ((x+block_size-1)/block_size)*block_size
        in

        let to_block x =
            x/block_size
        in
        let of_block x =
            x*block_size
        in            

        (*
        ** Now build up the core tables:
        **
        ** 1. List of all free clusters (compounds of blocks)
        ** 2. List of all used clusters (compounds of blocks)
        ** 3. List of all free inodes and the next free (the one
        **    after the last used)
        **
        *)

        (*
        ** The inode cache object. One object for all inodes!
        ** Disk address = obj = 1 [blocks]
        *)

        let _,inode_fse = cache_lookup ~cache:cache_inode
                                     ~obj:1
                                     ~addr:1      
                                     ~size:(off_of_inode super.afs_nfiles)
                                     ~state:FF_unlocked
        in

        (*
        ** Read the inode table.
        ** Build a list with free inodes below the
        ** nextfree boundary (above nextfree there are all
        ** free inodes, if any).
        *)

        sys_log Sys_start "AFS: Reading the Inode Table... \n";
    
        let ulist = ref [] in
        let freeino = ref [] in

        let firstfree = ref (-1) in
        let nextfree = ref (-1) in
        let nused = ref 0 in

        (*
        ** List of filesystem blocks.
        *)


        let inodeb = buf_create inode_SIZE in

        for i = 1 to (super.afs_nfiles-1)
        do
            let stat = cache_read ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE
            in

            if (stat <> std_OK) then
                raise (Error stat);
            
            let pos,inode = buf_get_inode ~buf:inodeb ~pos:0 in
            let istate = to_state inode.i_state in

            (*
            ** Some sanity checks first
            *)

            if (inode.i_file_num <> i) then
            begin
                close_inode ();
                close_data (); 
                sys_log Sys_fatal "AFS: Invalid Inode entry. Abort\n"; 
                sys_log Sys_fatal
                        "AFS: expected inode number %d, but got %d.\n" 
                        i inode.i_file_num;
                raise (Error std_SYSERR);
            end;

            if (istate <> FF_locked ) then
            begin

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

                if (istate = FF_unlocked ||
                    istate = FF_commit) then
                begin
                    sys_log Sys_info 
                            "AFS: Unlocked/uncommitted file found. Destroy it: %s\n"
                        (
                        "     [obj="^
                        (string_of_int i)^" state="^
                        (string_of_int inode.i_state)^" disk_addr="^
                        (string_of_int inode.i_disk_addr)^" disk_size="^
                        (string_of_int inode.i_disk_size)^"]"
                        );

                    inode.i_state <- of_state FF_invalid;
                    inode.i_disk_addr <- 0;
                    inode.i_disk_size <- 0;
                    let pos = buf_put_inode ~buf:inodeb ~pos:0 
                                            ~inode:inode
                    in

                    let stat = cache_write 
                                  ~cache:cache_inode
                                  ~fse:inode_fse
                                  ~off:(off_of_inode i)
                                  ~buf:inodeb
                                  ~size:inode_SIZE in
                    if (stat <> std_OK) then
                        raise (Error stat);
                end;
            end
            else
            begin
                incr nused;
                ulist := !ulist @ [(i,inode.i_disk_addr,
                                        to_block (block inode.i_disk_size),
                                        Cluster_USED)];
            end;    
        done;

#ifdef DEBUG
        Db.Pr.sd 2 "server: [firstfree]" (!firstfree);
        Db.Pr.sd 2 "server: [nextfree]" (!nextfree);
#endif
        
        if (!nextfree <> -1 && !nextfree < super.afs_nfiles-1 ) then
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
        else if (!firstfree = 0 && !nextfree = super.afs_nfiles-1) then
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
    
#ifdef DEBUG
        Db.Pr.sd 2 "server: [nextfree]" (!nextfree);
#endif
        super.afs_nused <- !nused;
        super.afs_freeobjnums <- !freeino;
        super.afs_nextfree <- !nextfree;

        sys_log Sys_start "AFS: Found %d used Inode(s).\n"
                          super.afs_nused;

        (*
        ** Sort the used cluster list with increasing address.
        *)

        let usedcluster = Sort.list ( fun e1 e2 ->
                                let _,d1,_,_ = e1 in
                                let _,d2,_,_ = e2 in
                                (d1 < d2) 
                            ) !ulist
        in

        let usedn = List.length usedcluster in
        sys_log Sys_start "AFS: Found %d valid file(s).\n"
                          usedn;



        (*
        ** Build the free cluster list from the used cluster
        ** list (= holes list) and calculate the total free space.
        *)
        let flist = ref [] in
        let biggesthole = ref 0 in
        let last = ref (0,0) in
        
        let rec f_iter ul = 
            match ul with
            | hd::tl ->
            begin
                let _,cd,cs,_ = hd in
#ifdef DEBUG
                Db.Pr.sdd 5 "server: used cluster [da,ds]" cd cs;
#endif
                let ld,ls = !last in

                if (ld+ls = 0 && cd > 0) then
                begin
                    (*
                    ** Free cluster before first used cluster.
                    *)
                    if (cd > !biggesthole) then
                        biggesthole := cd;

                    flist := !flist @ [0,ld,cd,Cluster_FREE]; 
                end
                else if (ld+ls <> 0 && ld+ls < cd) then
                begin
                    let size = cd-ld-ls in
                    if ( size > !biggesthole) then
                        biggesthole := size;

                    flist := !flist @ [0,ld+ls,size,Cluster_FREE];
                end;
                last := (cd,cs);
                f_iter tl;
            end;
            | [] -> 
            begin
                let ld,ls = !last in
                let endd  = super.afs_nblocks in

                if (ld+ls < endd) then
                begin
                    let size = endd-ld-ls in
                    if (size > !biggesthole) then
                        biggesthole := size;

                    flist := !flist @ [0,ld+ls,size,Cluster_FREE];
                end;
            end;
        in

        f_iter usedcluster;        

        sys_log Sys_start "AFS: Biggest hole: %d bytes (%d MB)\n"
                          (!biggesthole*block_size)
                          (!biggesthole*block_size/1024/1024);


        (*
        ** Sort the block list with increasing address
        *)
        let blist = ref (!ulist @ !flist) in
        let cluster_list = Sort.list ( fun e1 e2 ->
                                let _,d1,_,_ = e1 in
                                let _,d2,_,_ = e2 in
                                (d1 < d2)
                            ) !blist
        in

        if (mode = 0) then
        begin
            (*
            ** Show cluster list
            *)
            let nl = print_newline in
            print_string "---------------------------------------------------";
            nl ();
            print_string 
                "Cluster allocation:\n[Inode] Disk block address: block size  flag"; 
            nl ();
            print_string "---------------------------------------------------";
            nl ();
            List.iter (fun clu ->
                let ci,cd,cs,cf = clu in
                print_string (sprintf "[%6d] %10d:%8d %8s" ci cd cs
                              begin
                                match cf with 
                                | Cluster_FREE -> "FREE";
                                | Cluster_USED -> "USED";
                                | Cluster_RESERVED -> "RESERVED";
                              end); nl ();
            ) cluster_list
        end
        else if (mode = 1) then
        begin
            (*
            ** Show block drawing
            *)
            let nl = print_newline in
            nl ();
            print_string 
                "Block allocation:  - => Free, X => Used, R => Reserved"; nl ();  
            nl ();
            List.iter (fun clu ->
                let ci,cd,cs,cf = clu in
                for i = 1 to cs 
                do
                    print_string (sprintf "%c"
                              begin
                                match cf with 
                                | Cluster_FREE -> '-';
                                | Cluster_USED -> 'X';
                                | Cluster_RESERVED -> 'R';
                              end); 
                done;
            ) cluster_list;
            nl ();
        end
        else if (mode = 10) then
        begin
            (*
            ** Defragmentation:
            ** Move files to the end of the file system. Try to use
            ** the free space in a contingous way.
            *)

            sys_log Sys_start "AFS: Doing filesystem defragmentation...\n";
            sys_log Sys_start
                    "---------------------------------------------------\n";
            (*
            ** First sort the used list with increasing sizes, and
            ** the free list with decreasing addresses.
            **
            ** List argument: (inode number, disk address, disk size, flag)
            **
            *)
            let ucl = ref (Sort.list ( fun e1 e2 ->
                                let _,_,s1,_ = e1 in
                                let _,_,s2,_ = e2 in
                                (s1 < s2) 
                            ) !ulist)
                in
            let fcl = ref (Sort.list ( fun e1 e2 ->
                                let _,d1,_,_ = e1 in
                                let _,d2,_,_ = e2 in
                                (d1 > d2) 
                            ) !flist)
                in

            (*
            ** Get a new free cluster from the free list of
            ** desired size.
            *)
            let nilclu = (0,0,0,Cluster_FREE) in

            
            let head = ref [] in
            let rec get_free fl d1 s1 =
                match fl with
                | hd::tl ->
                begin       
                    let _,d2,s2,_ = hd in

                    if (d1 > d2) then
                    begin
                        (*
                        ** Only move up files.
                        *)
                        fcl := !head @ [hd] @ tl;
                        head := [];
                        nilclu
                    end else if (s2 < s1) then
                    begin
                        head  := !head @ [hd]; 
                        get_free tl d1 s1;
                    end       
                    else if (s2 = s1) then
                    begin
                        fcl := !head @ tl;
                        head := [];
                        hd 
                    end
                    else 
                    begin
                        (*
                        ** Take it from the end
                        *)
                        fcl := !head @ [0,d2,s2-s1,Cluster_FREE] @ tl;
                        head := [];
                        (0,d2+s2-s1,s1,Cluster_USED)
                   end;
                end;
                | [] -> head := []; 
                        nilclu
                in

            (*
            ** Insert a new free cluster. Keep the decreasing address order.
            *)
            let rec insert_free fl cl =
                match fl with
                | hd::tl ->
                begin
                    let _,d1,s1,_ = hd in
                    let _,d2,s2,_ = cl in 
                    if (d2 > d1) then
                    begin
                        fcl := !head @ [cl;hd] @ tl;
                        head := [];
                    end
                    else
                    begin
                        head := !head @ [hd];
                        insert_free tl cl;
                    end;    
                end;
                | [] -> 
                begin
                    fcl := !head @ [cl];
                    head := [];
                end;
                in

            (*
            ** Only move files with size <= maxsize
            *)
            let dbuf = buf_create block_size in
            let inodeb = buf_create inode_SIZE in

            (
              try
                List.iter (fun u1 ->
                    let i,d1,s1,f1 = u1 in
                    if (s1 <= maxsize) then
                    begin
                        let obj=i in
                        (*
                        ** Read first the inode from disk to
                        ** sync the inode cache.
                        *)
                        let stat = cache_read ~cache:cache_inode
                                      ~fse:inode_fse
                                      ~off:(off_of_inode i)
                                      ~buf:inodeb
                                      ~size:inode_SIZE
                        in

                        if (stat <> std_OK) then
                        begin
                            close_inode ();
                            close_data ();
                            raise (Error stat);
                        end;

                        let pos,inode = buf_get_inode ~buf:inodeb ~pos:0 in
                        let istate = to_state inode.i_state in

                        let u2 = get_free !fcl d1 s1 in

                        if (u2 <> nilclu) then
                        begin
                            let _,d2,s2,_ = u2 in
                            assert (s1 = s2);
                            (*
                            ** Now copy the data
                            *)
                            sys_log Sys_start
                            "AFS: moving file %d (size=%d) from addr %d to %d\n"
                            i s1 d1 d2;
 
                            for b = 0 to s1-1
                            do
                                let stat = read_data 
                                                 ~obj:obj
                                                 ~addr:(d1+b)
                                                 ~data:dbuf
                                                 ~size:block_size
                                in
                                if (stat <> std_OK) then
                                begin
                                    close_inode ();
                                    close_data ();
                                    raise (Error stat);
                                end;
                                let stat = write_data
                                                  ~obj:obj 
                                                  ~addr:(d2+b)
                                                  ~data:dbuf
                                                  ~size:block_size
                                in
                                if (stat <> std_OK) then
                                begin
                                    close_inode ();
                                    close_data ();
                                    raise (Error stat);
                                end;
                            done;
                            (*
                            ** Update the inode.
                            *)
                            inode.i_disk_addr <- d2;
                                                                    
                            let pos = buf_put_inode ~buf:inodeb ~pos:0 
                                                    ~inode:inode
                            in

                            let stat = cache_write 
                                          ~cache:cache_inode
                                          ~fse:inode_fse
                                          ~off:(off_of_inode i)
                                          ~buf:inodeb
                                          ~size:inode_SIZE in
                            if (stat <> std_OK) then
                                raise (Error stat);
                        end;
                    end;
                    ) !ucl
              with
                | Exit -> ();
            );
            sys_log Sys_start 
                    "---------------------------------------------------\n";
        end;

        close_inode ();
        close_data ();

    end
    with
        | Error err -> 
                sys_log Sys_fatal "\nAFS: Abort: %s\n" (err_why err);
        | Buf_overflow -> 
                sys_log Sys_fatal "\nAFS: Abort: Buffer Overflow\n"; 
        | _ -> 
                sys_log Sys_start "\nAFS: Abort: Input-Output Error\n"; 
