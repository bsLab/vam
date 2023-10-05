(*
** Reboot a machine with a new kernel.
*)

open Amoeba
open Bytebuf
open Buf
open Stderr
open Stdcom
open Rpc

open Afs_common
open Afs_server
open Afs_server_rpc

(*
** We must provide a simple AFS server with only one object: the kernel
** image. This kernel imgae is read by the server disk_read functions. 
*)

(*
** The AFS super capability 
*)

let supercap = ref nilcap in


let afs_server kpath =
    ()

