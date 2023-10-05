

(*
** Init VAMsys.
** You can add own modules here.
**
** For example:
**          #load "mymod.cma" ;;
**          open Mymod;;
**
*)

open Gc;;
open Thread;;
open Mutex;;
open Sema;;
open Des48;;
open Machtype;;
open Amoeba;;
open Bytebuf
open Cap_env;;
open Rpc;;
open Cmdreg;;
open Stderr;;
open Stdcom;;
open Capset;;
open Buf;;
open Ar;;
open Soap;;
open Dir;;
open Name;;
open Circbuf;;
open Virtcirc;;
open Ksys;;

open Unix;;
open Afs_common
open Afs_client
open Dns_common
open Dns_client

open Config;;

(*
** Help system

let _ = 
    let rec iter pl =
        match pl with
        | hd::tl ->
          begin
            try
                help_read (hd^"/vamdoc.doc") 
            with
                Sys_error _ -> iter tl;
          end;
        | [] -> help_read "vamdoc.doc";
    in
    Printf.printf "Loading online help system...\n";
    iter !load_path
;;
*)
   
(*
** Install pretty printers
*)

#install_printer print_amoeba_status;;
#install_printer print_amoeba_port;;
#install_printer print_amoeba_priv;;
#install_printer print_amoeba_cap;;
#install_printer print_amoeba_cb;;
#install_printer print_amoeba_buf;;
#install_printer print_amoeba_sema;;
#install_printer print_amoeba_vc;;

(*
** Machine types
*)
#install_printer print_machtype_int8;;
#install_printer print_machtype_int16;;
#install_printer print_machtype_int32;;
#install_printer print_machtype_int64;;
#install_printer print_machtype_uint8;;
#install_printer print_machtype_uint16;;
#install_printer print_machtype_uint32;;
#install_printer print_machtype_uint64;;
#install_printer print_machtype_word32;;

