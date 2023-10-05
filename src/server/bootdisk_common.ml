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
**    $VERSION:     1.08
**    $INFO:
**
**  Kernel Boot directory and disk module server implementation.
**  This server module emulates a DNS/AFS interface for the kernel
**  boot partition holding kernels, binaries needed for bootstrap
**  purposes and configuation files with a very simple
**  filesystem.
**
**  Common defintions and types.
**
**    $ENDOFINFO    
**
*)
  
open Amoeba
open Bytebuf
open Stderr
open Bootdir
open Mutex
open Thread
open Stdsrvparams

(*
** State of a boot file object.
*)

type boot_flag =
    | Boot_locked   (* file is froozen but currently without a name *)
    | Boot_unlocked (* file can be modified *)
    | Boot_invalid  (* invalid entry *)
    | Boot_named    (* we've got the name - 
                    ** the file must be previously locked 
                    *)


(*
** The directory table of one bootdisk with additional informations
** needed for the DNS/AFS emulation.
*)

type bdisk_entry = {
    mutable boot_lock : Mutex.t;
    mutable boot_name : string; (* entry name *)
    mutable boot_order: int;      (* order number *)
                                  (* determines position in bootdisk table *)
    (*
    ** unique object cap - the public directory entry
    ** capability. Only used dynamically.
    *)
    mutable boot_obj : privat; 

    (*
    ** vdisk start block and size of object
    *)
    mutable boot_start : int;
    mutable boot_size  : int;
    (* 
    ** size in bytes - may differ only from boot_size*blksize if
    ** the file is created at runtime! 
    ** This information is needed for bd_insert,bd_modify,bd_read...
    *)
    mutable boot_file_size : int;           

    (*
    ** column maks
    *)
    mutable boot_cols : rights_bits array;

    mutable boot_flag : boot_flag;

    mutable boot_live : int;    (* livetime of entry *)
}

type bdisk = {
    mutable bdisk_blksize : int;    (* block size in bytes *)

    mutable bdisk_lock : Mutex.t;

    (*
    ** capability of the virtual disk 
    *)
    mutable bdisk_vcap : capability; 

    (*
    ** private and check port of this bdisk server = DNS
    ** The only object is the root directory!
    *)
    mutable bdisk_prvport : port; 
    mutable bdisk_pubport : port;
    mutable bdisk_chkport : port;

    (*
    ** For the file server we need also a port = AFS
    *)
    mutable bdisk_file_prvport : port;
    mutable bdisk_file_pubport : port;
    mutable bdisk_file_chkport : port;

    (*
    ** Path and name of this bdisk server, for example
    ** /hosts/develop/bootdisk
    *)
    mutable bdisk_name : string;
    mutable bdisk_path : string;    

    (*
    ** The directory table
    *)
    mutable bdisk_table : bdisk_entry list;    (* the content *)
    mutable bdisk_nfiles: int;    (* maximal size of bdisk_table *)

    mutable bdisk_sync : bool;  (* pending write operation ? *)
    mutable bdisk_buf : Bytebuf.buffer; (* temp buffer *)
    
    (*
    ** Free block list of vdisk
    *)
    mutable bdisk_freeblocks : (int*int) list;

    (*
    ** Function to read and write file content in BYTE units.
    ** This function will fix block align problems because the
    ** disk_read/write functions used to access the virtual disk
    ** are using only block units.
    **
    ** foff: relative file offset in bytes
    ** size : data size to read/write in bytes
    ** boff : relative buffer offset in bytes
    *)

    mutable bdisk_read : disk:bdisk -> file:bdisk_entry ->
                         foff:int -> size:int ->
                         buf:buffer -> boff:int -> status;
    mutable bdisk_write: disk:bdisk -> file:bdisk_entry ->
                         foff:int -> size:int ->
                         buf:buffer -> boff:int -> status;
}

let nilboot = {boot_lock = mu_create ();
               boot_name = "";
               boot_order = 0;
               boot_obj = nilpriv;
               boot_start = 0;
               boot_size = 0;
               boot_file_size = 0;
               boot_cols = [||];
               boot_flag = Boot_invalid;
               boot_live = 0;}

let bootdisk_MAXLIVE = 8
