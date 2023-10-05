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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     21.7.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Create a bootdisk file system.
**
**    $ENDOFINFO
**
*)

open Amoeba 
open Machtype
open Bytebuf
open Disk_client
open Stderr  
open Stdcom
open Printf
open Ar
open Cap_env
open Name
open Ksys
open Bootdir

let check_stat stat =
    if stat <> std_OK then
        raise (Error stat)

let create vdisk overwrite =
  try
  begin
    let stat,vcap = name_lookup vdisk in
    check_stat stat;
    let buf = buf_create 512 in     (* kernel dir buffer / 1 block *)
    let off = 0 in
    let stat = disk_read vcap ~start:off ~num:1 ~blksize:512
                              ~buf:buf ~pos:0 in
    check_stat stat;
    if (overwrite = false) then
    begin
        let _,bd = buf_get_bd buf 0 in
        if (bd.bd_magic = bd_MAGIC) then
                raise (Error std_EXISTS);
    end;
    let bd' = {
            bd_magic = bd_MAGIC;
            bd_entries = [||];
            bd_unused = word32 (bd_NENTRIES);
            } in
    let buf' = buf_create 512 in
    let _ = buf_put_bd ~buf:buf' ~pos:0 ~bd:bd' in
    let stat = disk_write vcap ~start:off ~num:1 ~blksize:512
                               ~buf:buf' ~pos:0 in
    check_stat stat;        
    std_OK
  end
  with | Error stat -> stat
