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
**    $VERSION:     1.15
**
**    $INFO:
**
** DNS: Directory and Name Service
** Common values and structures - both for servers and clients
**
**
**    $ENDOFINFO
**
*)




open Amoeba
open Cmdreg
open Stderr
open Stdcom
open Capset

let dns_REQBUFSZ = ((1490 - header_SIZE) + (19 * 1490))

(*
** Some miscellaneous constants for this directory service: 
*)

let dns_BUFSIZE      = (8 * 1024)
let dns_MAXCOLUMNS   = 4
let dns_NTRY         = 3             (* # retries in idempotent ops *)


(*
** Commands:
*)

let  dns_CREATE       = Command (dns_FIRST_COM + 0)
let  dns_DISCARD      = Command (dns_FIRST_COM + 1)
let  dns_LIST         = Command (dns_FIRST_COM + 2)
let  dns_APPEND       = Command (dns_FIRST_COM + 3)
let  dns_CHMOD        = Command (dns_FIRST_COM + 4)
let  dns_DELETE       = Command (dns_FIRST_COM + 5)
let  dns_LOOKUP       = Command (dns_FIRST_COM + 6)
let  dns_SETLOOKUP    = Command (dns_FIRST_COM + 7)
let  dns_INSTALL      = Command (dns_FIRST_COM + 8)
let  dns_REPLACE      = Command (dns_FIRST_COM + 10)
let  dns_GETMASKS     = Command (dns_FIRST_COM + 11)
let  dns_GETSEQNR     = Command (dns_FIRST_COM + 12)
let  dns_RENAME       = Command (dns_FIRST_COM + 13)

let dns_GETDEFBULLET  = Command (dns_FIRST_COM + 50)
let dns_GETDEFAFS     = Command (dns_FIRST_COM + 50)

(*
** Rights: 
*)

let dns_COLMASK      = ((1 lsl dns_MAXCOLUMNS) - 1)
let dns_RGT_DEL       = 0x80
let dns_RGT_MOD       = 0x40


(*
** DNS default values
*)

(*
** Use the default DNS server
*)
let dns_DEFAULT      = nilcapset

let dns_NOMOREROWS   = (1 lsl 16)-1 

let dns_MAXPATH      = 255

let dns_DEFAULT_COLS = [|"owner";"group";"others"|]

(*
** Evaluate any "." or ".." components in name, remove multiple '/',
** evaluate relative paths. Returns the normalized path. 
*)

let path_normalize ~path =
    let path = if (path="") then "/" else path in
    let relpath     = if path.[0] = '/' then true else false in
    let pathlist    = Str.split (Str.regexp "/") path in
    
    (*
    ** Remove multiple slashes
    *)
    let pathlist'   = List.filter ( 
                        fun s ->  if (s <> "" && s <> ".") then 
                                        true 
                                  else
                                        false
                      ) pathlist
    in

    (*
    ** Resolve ".." parts
    *)


    let pathlist'' = ref [] in
    
    let rec iter pl =
        match pl with 
        | hd::tl ->
            if (hd <> "..") then
                pathlist'' := [hd] @ !pathlist'' 
            else
            begin
                if (List.length !pathlist'' = 0) then
                    failwith "path_normalize: invalid path"
                else
                begin
                    pathlist'' := List.tl !pathlist'';
                end;
            end;
            iter tl;
        | [] -> ();
    in
    iter pathlist';    

    
    (*
    ** Glue the path components again..
    *)
    
    let rec concpath pl =
        match pl with
        | hd::tl -> (
                            let sp = concpath tl in
                            sp
                     )^
                     (if tl <> [] then "/" else "")^
                     hd;
        | []     -> ""
    in     
    
    let path' = concpath !pathlist'' in

    if (relpath = false) then 
        "/"^path' 
    else 
        path'

        