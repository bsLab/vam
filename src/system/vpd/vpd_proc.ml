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
**    $CREATED:     11.12.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**
**    $ENDOFINFO
**
*)
open Amoeba
open Stderr
open Buf
open Bytebuf
open Name
open Proc
open Ar
open Rpc
open Cap_env
open Name
open Afs_client
open Unix

let info str =
    print_string str;
    print_newline ()

(*
** Read the process descriptor from a file, either UNIX or AFS.
*)
let pd_read file verbose =

  try
  begin
    let path = Cap_env.path_resolve file in
    let buf_size = pd_SIZE in
    let buf = buf_create buf_size in
    match path with
    | Unix_path file ->
    begin
        if verbose > 2 then info "Opening Unix file...";

        let fd = Unix.openfile file [O_RDONLY] 0 in

        if verbose > 2 then info "Reading from Unix file...";
        
        let n = Unix.readb fd buf 0 buf_size in
        if n < buf_size then raise (Error std_IOERR);

        if verbose > 2 then info "Extracting PD from buffer...";
        let pos,pd = buf_get_pd ~buf:buf ~pos:0 in
        std_OK,pd      
    end;
    | Amoeba_path file ->
    begin
        let stat,cap = name_lookup file in
        if stat <> std_OK then
            raise (Error stat);
        
        let err,n = afs_read ~cap:cap
                             ~offset:0
                             ~buf:buf
                             ~size:buf_size in
        if (err <> std_OK) then
                raise (Error err);
        let pos,pd = buf_get_pd ~buf:buf ~pos:0 in
        std_OK,pd      
    end
  end
  with
        | Error stat -> stat,nilpd;
        | Buf_overflow -> std_OVERFLOW,nilpd
(*        | _ -> std_SYSERR,nilpd *)


    