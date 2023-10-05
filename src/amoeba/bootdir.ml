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
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.06
**
**    $INFO:
**
**  Boot/Kernel directory partition support.
**
**    $ENDOFINFO
**
*)

open Bytebuf
open Machtype

(*
** A kernel directory. Must be of size 512 bytes! Exact I mean.
*)
  
let bd_MAGIC = word32s "0x3017504"
let bd_NENTRIES = 21
let bde_NAMELEN = 16


type bootdir_entry = {
    mutable bde_start: word32;  (* start sector/block           *)
    mutable bde_size: word32;   (* size in sectors/blocks       *)  
    mutable bde_name: string;   (* name of entry - bde_NAMELEN  *)
}
 
let nilbde = {bde_start=word32 0;bde_size=word32 0;
              bde_name = (let str = String.create bde_NAMELEN in
                          String.fill str 0 (bde_NAMELEN-1) '\000';
                          str)}
type bootdir = {
    mutable bd_magic: word32;
    mutable bd_entries: bootdir_entry array;  (* bde_NENTRIES *)
    mutable bd_unused: word32;
}
 
(*
** Read one boot directory entry.
*)
let buf_get_bde ~buf ~pos =
    let bdes = ref [||] in 
    let pos  = ref pos in  
    for i = 0 to bd_NENTRIES-1
    do
        let str = String.create bde_NAMELEN in
        let pos',sec = buf_get_mach ~buf:buf ~pos:!pos ~mach:Word32 in
        pos := pos';
        let pos',num = buf_get_mach ~buf:buf ~pos:!pos ~mach:Word32 in
        pos := pos';
        blit_bs ~src:buf ~src_pos:!pos
                ~dst:str ~dst_pos:0 ~len:bde_NAMELEN;
        pos := !pos + bde_NAMELEN;
        if (sec <> (word32) 0 && num <> (word32 0)) then
            bdes := Array.append !bdes
                     [|{bde_start = dec_be sec;
                        bde_size   = dec_be num;
                        bde_name   = str;}|];
    done;
    !pos,!bdes

(*
** read boot directory table.
** Note: the bd_entries array only contains valid (used)
** descriptors!
*)
let buf_get_bd ~buf ~pos =
    let pos,kmag = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,bdes = buf_get_bde ~buf:buf ~pos:pos in
    let pos,unus = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    pos,
    {   
        bd_magic   = dec_be kmag;
        bd_entries = bdes;
        bd_unused  = dec_be unus;
    }


(*
** Write one boot directory entry.
*)
  
let buf_put_bde ~buf ~pos ~bdes =
    let bdes_n = Array.length bdes in
    let pos = ref pos in
    for i = 0 to bd_NENTRIES-1
    do
        let bde = if (i<bdes_n) then
                    bdes.(i)
                  else
                    nilbde
            in
        let pos' = buf_put_mach ~buf:buf ~pos:!pos
                                ~mach:(enc_be bde.bde_start) in
        pos := pos';
        let pos' = buf_put_mach ~buf:buf ~pos:!pos
                                ~mach:(enc_be bde.bde_size) in
        pos := pos';
        blit_sb ~src:(bde.bde_name) ~src_pos:0
                ~dst:buf ~dst_pos:!pos ~len:bde_NAMELEN;
        pos := !pos + bde_NAMELEN;
    done;
    !pos 

(*
** Write the boot directory table.
** Note: the bd_entries array may only contain valid (used)
** descriptors!
*)
let buf_put_bd ~buf ~pos ~bd =
    bd.bd_unused <- word32 (bd_NENTRIES - (Array.length bd.bd_entries));

    let pos = buf_put_mach ~buf:buf ~pos:pos 
                           ~mach:(enc_be bd.bd_magic) in
    let pos = buf_put_bde  ~buf:buf ~pos:pos ~bdes:bd.bd_entries in
    let pos = buf_put_mach ~buf:buf ~pos:pos 
                           ~mach:(enc_be bd.bd_unused) in
    pos

module String = StringLabels

(*
** Create the bde_name field
*)
let bde_toname str =
    let len = String.length str in
    if (len > bde_NAMELEN) then
        failwith "invalid kernel name";
    let bstr = String.create bde_NAMELEN in
    String.fill bstr 0 bde_NAMELEN '\000';
    String.blit ~src:str ~src_pos:0
                ~dst:bstr ~dst_pos:0 ~len:len;
    bstr

(*
** Extract the name string from the bde_name field
*)
let bde_fromname bname =
    let len = String.index bname '\000' in
    let str = String.create len in
    String.blit ~src:bname ~src_pos:0
                ~dst:str   ~dst_pos:0 ~len:len;
    str

