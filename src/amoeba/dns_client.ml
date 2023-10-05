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
**    $VERSION:     1.01
**
**    $INFO:
**
** DNS: Directory and Name Service
** Client implementation.
**
**
**    $ENDOFINFO
**
*)



open StdLabels

open Amoeba
open Bytebuf
open Cmdreg
open Stderr
open Stdcom
open Capset
open Sys
open Rpc
open Buf

module Str = StrLabels
open Str


open Dns_common

(*
** DNS directory data structures.
*)

(* one row *)

type dns_dir_entry = {
    mutable d_name      : string;
    mutable d_columns   : rights_bits array;  
}


let nil_dns_dir_entry = { d_name = ""; d_columns = [||] }

(* one directory *)

type dns_dir = {
    mutable dd_capset   : capset;
    mutable dd_ncols    : int;
    mutable dd_nrows    : int;
    mutable dd_colnames : string array;
    mutable dd_rows     : dns_dir_entry list;
    mutable dd_curpos   : int;
}
   
   
(*
** default_colmask
**      Looks in the environment for the user's default capability masks
**      and returns them in an int array.  
**      If there is no
**      environment variable then all nmasks entries are set to 0xFF
*)

let default_colmask () =
    let nmasks = dns_MAXCOLUMNS in


    let def_mask = try (getenv "DNSMASK")
                   with Not_found -> "" 
    in

    let (mask_len,mask_array) =
        if (def_mask = "") then
        begin
            (dns_MAXCOLUMNS,(Array.create dns_MAXCOLUMNS 0xff)) 
        end
        else
        begin
            let def_mask_list = split ~sep:(regexp ":") def_mask in
            let def_mask_arr  = Array.of_list def_mask_list in
            let def_mask_len  = Array.length def_mask_arr in
            if (def_mask_len > dns_MAXCOLUMNS) then
                failwith "Dns_client.default_colmask: invalid dns mask from environment";

            let mask_array = Array.create def_mask_len 0 in
            for i = 0 to (def_mask_len-1)
            do
                mask_array.(i) <- (int_of_string def_mask_arr.(i));
            done; 
            (def_mask_len,mask_array)
        end
    in
    (mask_len,mask_array)    


(*
** Root and current working directory capability set
*)

let _dns_rootdir = ref nilcapset
let _dns_workdir = ref nilcapset

       
(*
** Return the current working directory cap set
** XX TODO XX
*)

let get_workdir () =
    let stat = 
            if (!_dns_workdir = nilcapset) then
                std_CAPBAD 
            else
                std_OK
    in
    (stat,cs_copy (!_dns_workdir))

(*
** Return the root dir cap set
*)

let get_rootdir () =
    let stat = 
            if (!_dns_rootdir = nilcapset) then
                std_CAPBAD 
            else
                std_OK
    in
    (stat,cs_copy (!_dns_rootdir))


(*
** Set the current working and root directory
*)


let set_workdir cap =
    _dns_workdir := (cs_singleton cap)

let set_rootdir cap =
    _dns_rootdir := (cs_singleton cap)


(*
** Perform a request to the DNS server. This function tries
** to find out a valid capability from the capset 'dir_cs'.
*)

let mktrans 
            ~ntries
            ~hdr_in
            ~dir_cs
            ~inbuf
            ~insize
            ~outbuf
            ~outsize
            =
            
    let err = ref std_OK in
    let failed = ref std_SYSERR in
    let hdr_out = ref nilheader in
    
    try
    (
        for tries = 1 to ntries
        do
          for i = 0 to (dir_cs.cs_final - 1)
          do
            let s = dir_cs.cs_suite.(i) in

            if (s.s_current = true ) then
            begin
                hdr_in.h_port <- s.s_object.cap_port;
                hdr_in.h_priv <- s.s_object.cap_priv;


                (*
                ** Do the real transaction 
                *)
                let (trans_stat,n,hdr2) = trans (hdr_in,
                                                    inbuf,insize, 
                                                    outbuf,outsize) in

                hdr_out := hdr2;
                
                if (trans_stat <> std_OK) then
                    err := trans_stat
                else
                    err := !hdr_out.h_status;

                if (!err = rpc_FAILURE && ntries > 1) then
                    (*
                    ** Retry idempotent operation, but return RPC_FAILURE
                    ** in case we don't succeed in a following iteration.
                    *)
                    failed := rpc_FAILURE
                else if (!err <> rpc_FAILURE) then
                begin
                    raise Exit;
                end; 
            end;
          done;
        done;
        (!failed,!hdr_out);
    ) with Exit -> (!err,!hdr_out)
            

(*
** lookup  returns  the capability-set stored under the name
** 'path' relative to the directory 'root'.  The root directory
** can be any directoty in the directory tree.
*)

let dns_lookup
        ~root
        ~path
        =

    try
    if path = "/" then (std_OK,root) else
    begin
        let path = path_normalize path in 
        let path_len = String.length path in

        if (path_len = 0) then
            raise (Error std_ARGBAD);

        (*
        ** Protection against infinitely looping:
        *)

        let max_steps = 10 in       

        let in_buf_size_max = (path_len + 2) in
        let in_buf          = buf_create in_buf_size_max in
    
        let out_buf_size    = (dns_MAXPATH + 1 + capset_SIZE) in
        let out_buf         = buf_create out_buf_size in

        let (err,cs_root) = 
            if (root = dns_DEFAULT) then
            begin 
                if (path.[0] = '/') then
                    get_rootdir ()
                else
                    get_workdir ();
            end
            else
                (std_OK,(cs_copy root))
        in
        if (err <> std_OK) then
            raise (Error err);

        let hdr_in = header_new () in
    

        (*
        ** Loop over the path components step by step and resolve the path.
        ** Each run, the next path component is the new parent directory
        ** for the next lookup.
        *)


        let rec path_iter depth path cs_parent =

            if (depth > max_steps) then
                raise (Error std_OVERFLOW);

            let path_len = String.length path in

    
            if (path = "." || path_len = 0)
            then
                cs_parent
            else
            begin
                let in_buf_size = buf_put_string ~buf:in_buf 
                                                 ~pos:0 
                                                 ~str:(path^"\000") in

                hdr_in.h_command <- dns_LOOKUP ;
                hdr_in.h_size <- in_buf_size;

            
                let (err,hdr_out) =   mktrans   
                                            ~ntries:dns_NTRY
                                            ~hdr_in:hdr_in
                                            ~dir_cs:cs_parent
                                            ~inbuf:in_buf
                                            ~insize:in_buf_size
                                            ~outbuf:out_buf
                                            ~outsize:out_buf_size
                in


                if (err <> std_OK) then
                    raise (Error err);

                if (hdr_out.h_status <> std_OK) then
                    raise (Error hdr_out.h_status);

                let (pos,path_ret) = buf_get_string ~buf:out_buf ~pos:0
                    in
                (* get the capset *)
                let (pos,cs_part) = buf_get_capset ~buf:out_buf   
                                                   ~pos:pos
                    in

                (*
                ** Remove a leading slash from path 
                *)
                let path_len = String.length path_ret in

                let path_mod = if (path_len > 0 && path_ret.[0] = '/') then
                                String.sub path_ret ~pos:(1)
                                                    ~len:(path_len-1)
                               else
                                    path_ret
                   in
                
                (* and the next lookup if necessary *)
                path_iter (depth+1) path_mod cs_part;
            end;       
        in


        (* remove a leading slash from path *)
        let path_mod = if (path_len > 0 && path.[0] = '/') then
                            String.sub path ~pos:(1)
                                            ~len:(path_len-1)
                       else
                            path
        in

        let stat,cs_dir =
            try 
                std_OK,(path_iter 1 path_mod cs_root)
            with Error err -> (err,nilcapset)
        in 
        
        (stat,cs_dir)
    end
    with
        | Error err -> (err,nilcapset)

        
    
(*
** Lookup a set of directory capabilities with specified row names
** given with a (dir capset,name) tuple list.
** The (lookup status,row time, row capset) tuple list is returned.
*)

        
let dns_setlookup ~server 
              ~dirs        
    =
    try
    begin

        let dirsn = List.length dirs in
        let req_size = dirsn*(cap_SIZE+max_PATHLEN) in
        let rep_size = dirsn*(int16_SIZE + int32_SIZE +  
                                     cap_SIZE + capset_SIZE)
        in
        let buf_req = buf_create req_size in
        let buf_rep = buf_create rep_size in
    
        let hdr_req = {(header_new ()) with
                    h_command = dns_SETLOOKUP;
                  } in

        let pos = ref 0 in

        List.iter (fun d ->
            let cs,name = d in
            let pos' = buf_put_capset ~buf:buf_req
                                      ~pos:!pos
                                      ~cs:cs
                in
            let pos' = buf_put_string ~buf:buf_req
                                      ~pos:pos'
                                      ~str:name
                in
            pos := pos';
        ) dirs;

        let req_size = !pos in
        let stat,hdr_rep = mktrans ~ntries:dns_NTRY
                               ~hdr_in:hdr_req
                               ~dir_cs:server
                               ~inbuf:buf_req
                               ~insize:req_size
                               ~outbuf:buf_rep
                               ~outsize:rep_size
        in
        if (stat <> std_OK) then
            raise (Error stat);
    
        let ll = ref [] in
        let pos = ref 0 in
        for i = 1 to dirsn
        do
            
            (*
            ** Status
            *)
            let pos',stat =  buf_get_int16 ~buf:buf_rep
                                          ~pos:!pos
                in
            let pos',_ = buf_get_cap ~buf:buf_rep
                                    ~pos:pos'
                in
            let pos',time = buf_get_int32 ~buf:buf_rep
                                     ~pos:pos'
                in
            let pos',cs = buf_get_capset ~buf:buf_rep
                                    ~pos:pos'
                in
            ll := !ll @ [(Status stat,time,cs)];
            pos := pos';
        done;
        std_OK,!ll
    end
    with
        | Buf_overflow -> std_OVERFLOW,[]
        | Error err -> err,[]


let dns_rename ~dir ~oldname ~newname =
    let in_buf_size = (String.length oldname) + 1 +
                      (String.length newname) + 1 
        in
    let in_buf          = buf_create in_buf_size in
    let hdr_req = {(header_new ()) with
                    h_command = dns_RENAME; 
                  } in
    let pos = buf_put_string ~buf:in_buf ~pos:0 ~str:oldname in
    let pos = buf_put_string ~buf:in_buf ~pos:pos ~str:newname in
    let stat,hdr_rep = mktrans ~ntries:dns_NTRY
                               ~hdr_in:hdr_req
                               ~dir_cs:dir
                               ~inbuf:in_buf
                               ~insize:in_buf_size
                               ~outbuf:nilbuf
                               ~outsize:0
        in
    if (stat <> std_OK) then
        stat
    else
        hdr_rep.h_status

(*
** Append new 'obj' capability to directory specified with capset 'dir'
** with row 'name' and the new column rights 'cols'. 'name' may not 
** a path! Use dns_lookup to resolve directory capability instead!
*)

let dns_append ~dir ~name ~obj ~cols =
    let ncols = Array.length cols in
    if (ncols > dns_MAXCOLUMNS) then
        failwith "dns_append: invalid cols array";

    let in_buf_size = (dns_MAXPATH + 1) + 
                      (dns_MAXCOLUMNS * 4 + capset_SIZE) in
    let in_buf = buf_create in_buf_size in

    let hdr_req = {(header_new ()) with
                    h_command = dns_APPEND; 
                  } in

    let bufpos = ref 0 in
    bufpos := buf_put_string ~buf:in_buf ~pos:(!bufpos) ~str:name;
    bufpos := buf_put_capset ~buf:in_buf ~pos:(!bufpos) ~cs:obj;
        
    for n = 0 to (ncols-1)
    do
        bufpos := buf_put_right_bits ~buf:in_buf ~pos:(!bufpos)
                                     ~right:(cols.(n));
    done;        
    let stat,hdr_rep = mktrans ~ntries:1
                               ~hdr_in:hdr_req
                               ~dir_cs:dir
                               ~inbuf:in_buf
                               ~insize:in_buf_size
                               ~outbuf:nilbuf
                               ~outsize:0 in
    stat


(*
** dns_delete deletes the directory entry (which may itself be a
** directory capability)  specified  by  'name' in directory 
** specified with capset 'dir'.  
*)

let dns_delete ~dir ~name =
    let in_buf_size = (String.length name) + 1 in
    let in_buf      = buf_create in_buf_size in
    let hdr_req = {(header_new ()) with
                    h_command = dns_DELETE; 
                  } in
    let pos = buf_put_string ~buf:in_buf ~pos:0 ~str:name in

    let stat,hdr_rep = mktrans ~ntries:1
                               ~hdr_in:hdr_req
                               ~dir_cs:dir
                               ~inbuf:in_buf
                               ~insize:in_buf_size
                               ~outbuf:nilbuf
                               ~outsize:0
        in
    if (stat <> std_OK) then
        stat
    else
        hdr_rep.h_status

(*
** Try to get default file server cap from directory server.
*)

let dns_getdefafs ~dir = 
  try
  begin
    let hdr_req = {(header_new ()) with
                    h_command = dns_GETDEFAFS; 
                  } in
    let out_buf_size = 500 in
    let out_buf      = buf_create out_buf_size in
    let stat,hdr_rep = mktrans ~ntries:dns_NTRY
                               ~hdr_in:hdr_req
                               ~dir_cs:dir
                               ~inbuf:nilbuf
                               ~insize:0
                               ~outbuf:out_buf
                               ~outsize:out_buf_size
        in
    if (stat <> std_OK) then
        raise (Error stat);

    if hdr_rep.h_status <> std_OK then
        raise (Error hdr_rep.h_status);

    let pos,cap = buf_get_cap out_buf 0 in
    std_OK,cap
  end
  with Error stat -> stat,nilcap

(*
** Create a new directory and return the capability set of this
** newly created one. The 'dir' argument can be any diretory of
** the DNS server on which the new directory should be created!
*)

let dns_create ~dir ~cols =
  try
  begin
    let ncols = Array.length cols in
    if (ncols > dns_MAXCOLUMNS) then
        raise (Error std_ARGBAD);

    
    let inbuf_size = dns_MAXCOLUMNS*(dns_MAXPATH+1)  in
    let inbuf = buf_create inbuf_size in
    let out_buf_size = 500 in
    let out_buf      = buf_create out_buf_size in

    let bufpos = ref 0 in
        
    for n = 0 to (ncols-1)
    do
            bufpos := buf_put_string ~buf:inbuf ~pos:(!bufpos)
                                     ~str:cols.(n);
    done;        

    let hdr_req = {(header_new ()) with
                    h_command = dns_CREATE; 
                  } in

    let stat,hdr_rep = mktrans ~ntries:1
                               ~hdr_in:hdr_req
                               ~dir_cs:dir
                               ~inbuf:inbuf
                               ~insize: !bufpos
                               ~outbuf:out_buf
                               ~outsize:out_buf_size
        in

    if (stat <> std_OK) then
        raise (Error stat);

    if hdr_rep.h_status <> std_OK then
        raise (Error hdr_rep.h_status);

    let cap = { cap_port = hdr_rep.h_port; cap_priv = hdr_rep.h_priv } in
    std_OK,cs_singleton cap
  end
  with Error stat -> stat,nilcapset
