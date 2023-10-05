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
**    $VERSION:     1.15
**
**    $INFO:
**
**  (Virtual) Disk Server Interface (High level)
**
**    $ENDOFINFO
**
*)




(*
** Disk server interface
*)


open Amoeba
open Bytebuf
open Rpc
open Stderr
open Stdcom
open Cmdreg
open Buf
open Machtype

let vdisk_SVR_NAME  = "vdisk" 
let pdisk_SVR_NAME  = "pdisk"
let ldisk_SVR_NAME  = "ldisk"
let fldisk_SVR_NAME = "floppy"

(* Disk server commands *)
let disk_INFO         = Command (disk_FIRST_COM + 1)
let disk_READ         = Command (disk_FIRST_COM + 2)
let disk_SIZE         = Command (disk_FIRST_COM + 3)
let disk_WRITE        = Command (disk_FIRST_COM + 4)
let disk_GETGEOMETRY  = Command (disk_FIRST_COM + 5)
let disk_CONTROL      = Command (disk_FIRST_COM + 6)

(*  Subcommands for DS_CONTROL  *)
let dsc_eject       = 1     (* for floppies and CD-ROMs *)
let dsc_format      = 2     (* for floppies *)

(* Rights for Disk capabilities *)

let disk_RGT_READ     = Rights_bits 0x2
let disk_RGT_WRITE    = Rights_bits 0x4
let disk_RGT_CTL      = Rights_bits 0x8


(* size of a unit of info returned by the DS_INFO command *)
let disk_INFO_SIZE    = 12

type disk_info = {
        disk_unit: int32;
        disk_firstblk: int32;
        disk_numblks: int32;
}

(*
** DISK_INFO
**      This is the client stub for the disk_info command.
**      It returns a list of disk_addr's which are tuples of
**      (unit, firstblock, # blocks).
*)

let disk_info cap =
    let hdr_req = header_new () in
    let repsize = disk_INFO_SIZE * 20 in
    let repbuf  = buf_create repsize in    

    hdr_req.h_port      <- cap.cap_port;
    hdr_req.h_priv      <- cap.cap_priv;
    hdr_req.h_command   <- disk_INFO;

    let (err,size,hdr_rep) = trans (hdr_req,nilbuf,0,
                                    repbuf,repsize) in

    if (err <> std_OK) then
        (err,[ {disk_unit=int32 0;
                disk_firstblk=int32 0;
                disk_numblks=int32 0} ])
    else if (hdr_rep.h_status <> std_OK) then
        (hdr_rep.h_status,[ 
                {disk_unit=int32 0;
                disk_firstblk=int32 0;
                disk_numblks=int32 0} ])
    else
    begin
        let cnt = hdr_rep.h_size / disk_INFO_SIZE in
        let dl = ref [] in

        (* unpack the data into the struct *)
        for i = 1 to cnt
        do
            let (pos,du) = buf_get_mach ~buf:repbuf ~pos:0 ~mach:Int32 in
            let (pos,df) = buf_get_mach ~buf:repbuf ~pos:pos ~mach:Int32 in
            let (pos,dn) = buf_get_mach ~buf:repbuf ~pos:pos ~mach:Int32 in
            dl := !dl @ [ {disk_unit=du;
                           disk_firstblk=df;
                           disk_numblks=dn;
                           }];

        done; 
        (std_OK, !dl)
    end

(*
** Convert block size in bytes to log2 block size
*)

let l2_of_block_size blk =
    match blk with
    |    512     -> 9;
    |    1024    -> 10;
    |    2048    -> 11;
    |    4096    -> 12;
    |    8192    -> 13;
    |    16384   -> 14;
    |    _       -> failwith "block size not supported or invalid value"



(*
** DISKREAD
**      This is the client stub for the disk_read command.
**      Since reads may be bigger than fit in a single transaction
**      we loop doing transactions until we are finished.
**      If it can read exactly what was requested it succeeds.
**      Otherwise it fails.  No partial reads are done.
*)



let disk_read cap 
              ~start:start  (* first blk                *) 
              ~num:num      (* number of blk's          *)
              ~blksize:blksize (* block size in bytes   *)
              ~buf:buf      (* buffer to write to       *)
              ~pos:pos      (* start position in buf    *) 
    =
    if (blksize > max_TRANS) then
        failwith "disk_read: block size larger than max transaction size";
    
    let hdr_req = header_new () in

    let l2vblksz    = l2_of_block_size blksize in
    let maxblks     = max_TRANS lsr l2vblksz  in
    let numblks     = ref num in
    let fstblk      = ref start in
    let bpos        = ref pos in

    let err     = ref std_OK in

    while (!err = std_OK && !numblks > 0)
    do
        hdr_req.h_port      <- cap.cap_port;
        hdr_req.h_priv      <- cap.cap_priv;
        hdr_req.h_command   <- cmd_copy disk_READ;

        hdr_req.h_size      <- l2vblksz;
        hdr_req.h_offset    <- !fstblk;

        let nblks = min maxblks !numblks in
        let tr_sz = nblks lsl l2vblksz in
        
        hdr_req.h_extra     <- nblks;

        let (reperr,size,hdr_rep) = transo (hdr_req,nilbuf,0,0,
                                            buf,!bpos,tr_sz) in
    
    
        if (reperr <> std_OK) then
            err := reperr
        else if (hdr_rep.h_status <> std_OK) then
            err := hdr_rep.h_status 
        else
        begin
            numblks := !numblks - nblks;
            fstblk  := !fstblk + nblks;
            bpos := !bpos + tr_sz;            
        end        
    done;  
    !err


(*
** DISKWRITE
**      This is the client stub for the disk_write command.
**      Since writes may be bigger than fit in a single transaction
**      we loop doing transactions until we are finished.
*)

let disk_write cap 
              ~start:start  (* first blk                *) 
              ~num:num      (* number of blk's          *)
              ~blksize:blksize (* block size in bytes   *)
              ~buf:buf      (* buffer to read from      *)
              ~pos:pos      (* start position in buf    *) 
    =
    if (blksize > max_TRANS) then
        failwith "disk_write: block size larger than max transaction size";
    
    let hdr_req = header_new () in

    let l2vblksz    = l2_of_block_size blksize in
    let maxblks     = max_TRANS lsr l2vblksz  in
    let numblks     = ref num in
    let fstblk      = ref start in
    let bpos        = ref pos in

    let err     = ref std_OK in

    while (!err = std_OK && !numblks > 0)
    do
        hdr_req.h_port      <- cap.cap_port;
        hdr_req.h_priv      <- cap.cap_priv;
        hdr_req.h_command   <- cmd_copy disk_WRITE;

        hdr_req.h_size      <- l2vblksz;
        hdr_req.h_offset    <- !fstblk;

        let nblks = min maxblks !numblks in
        let tr_sz = nblks lsl l2vblksz in
        
        hdr_req.h_extra     <- nblks;

        let (reperr,size,hdr_rep) = transo (hdr_req,buf,!bpos,tr_sz,
                                            nilbuf,0,0) in
    
    
        if (reperr <> std_OK) then
            err := reperr
        else if (hdr_rep.h_status <> std_OK) then
            err := hdr_rep.h_status 
        else
        begin
            numblks := !numblks - nblks;
            fstblk  := !fstblk + nblks;
            bpos := !bpos + tr_sz;            
        end        
    done;  
    !err

