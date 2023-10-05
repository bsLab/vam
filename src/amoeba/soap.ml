(*
**  THIS SOFTWARE IS OWNED AND COPYRIGHTED BY
**
**    ###     ####   ####               #         ##         #####
**    #  #    #      #                 #         # #        #     #
**    #   #   #      #                #         #  #       #      #
**    #   #   #      #               #         #   #      #      #   
**    ####    ####   ####  ####     #         ######     ########
**    #   #      #      #          #         #     #    #      #
**    #   #      #      #         #         #      #   #       #
**    #  #       #      #        #         #       #  #       #
**    ###     ####   ####       ######### #        # #########
**
**    Stefan Bosse (c) 2003
**   
**  THIS SOFTWARE MAY NOT BE COPIED, EXTRACTED, MODIFIED, OR 
**  OTHERWISE USED IN A CONTEXT OUTSIDE OF THE VAM SYSTEM.
** 
*)


(*
** SOAP high level dirctory module
*)

let _version = "1.04"

open StdLabels

open Amoeba
open Bytebuf
open Cmdreg
open Stderr
open Stdcom
open Capset
open Rpc
open Buf

module Str = StrLabels
open Str

open Ar
open Sys
open Printf 



(*
** Some miscellaneous constants for this directory service: 
*)

let sp_BUFSIZE      = (8 * 1024)
let sp_MAXCOLUMNS   = 4
let sp_NTRY         = 3             (* # retries in idempotent ops *)


(*
** Commands:
*)

let  sp_CREATE       = Command (sp_FIRST_COM + 0)
let  sp_DISCARD      = Command (sp_FIRST_COM + 1)
let  sp_LIST         = Command (sp_FIRST_COM + 2)
let  sp_APPEND       = Command (sp_FIRST_COM + 3)
let  sp_CHMOD        = Command (sp_FIRST_COM + 4)
let  sp_DELETE       = Command (sp_FIRST_COM + 5)
let  sp_LOOKUP       = Command (sp_FIRST_COM + 6)
let  sp_SETLOOKUP    = Command (sp_FIRST_COM + 7)
let  sp_INSTALL      = Command (sp_FIRST_COM + 8)
let  sp_PUTTYPE      = Command (sp_FIRST_COM + 9)
let  sp_REPLACE      = Command (sp_FIRST_COM + 10)
let  sp_GETMASKS     = Command (sp_FIRST_COM + 11)
let  sp_GETSEQNR     = Command (sp_FIRST_COM + 12)

(*
** Errors
*)

let sp_UNAVAIL      = Status (sp_FIRST_ERR -1)
let sp_NOTEMPTY     = Status (sp_FIRST_ERR -2)
let sp_UNREACH      = Status (sp_FIRST_ERR -3)
let sp_CLASH        = Status (sp_FIRST_ERR -4)

(*
** Rights: 
*)

let sp_COLMASK      = ((1 lsl sp_MAXCOLUMNS) - 1)
let sp_DELRGT       = 0x80
let sp_MODRGT       = 0x40


(*
** Use the default soap server
*)

let sp_DEFAULT      = nilcapset

let sp_NOMOREROWS   = (1 lsl 16)-1 

(*
** SOAP directory data structures.
*)

(* one row *)

type sp_dir_entry = {
    mutable d_name      : string;
    mutable d_columns   : rights_bits array;  
}

let nil_sp_dir_entry = { d_name = ""; d_columns = [||] }

(* one directory *)

type sp_dir = {
    mutable dd_capset   : capset;
    mutable dd_ncols    : int;
    mutable dd_nrows    : int;
    mutable dd_colnames : string array;
    mutable dd_rows     : sp_dir_entry list;
    mutable dd_curpos   : int;
}

let nil_sp_dir = { dd_capset = nilcapset; dd_ncols=0;
                   dd_nrows = 0; dd_colnames = [||]; dd_rows = [];
                   dd_curpos = 0 }
 

(*
** sp_mask
**      Looks in the environment for the user's default capability masks
**      and returns them in an int array.  
**      If there is no
**      environment variable then all nmasks entries are set to 0xFF
*)

let sp_mask () =
    let nmasks = sp_MAXCOLUMNS in


    let def_mask = try (getenv "SPMASK")
                   with Not_found -> "" 
    in

    let (mask_len,mask_array) =
        if (def_mask = "") then
        begin
            (sp_MAXCOLUMNS,(Array.create sp_MAXCOLUMNS 0xff)) 
        end
        else
        begin
            let def_mask_list = split ~sep:(regexp ":") def_mask in
            let def_mask_arr  = Array.of_list def_mask_list in
            let def_mask_len  = Array.length def_mask_arr in
            if (def_mask_len > sp_MAXCOLUMNS) then
                failwith "sp_mask: invalid soap mask from environment";

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
** Evaluate any "." or ".." components in name, remove multiple '/',
** evaluate relative paths. Returns (root,normpath) tuple. The 'root' 
** capability is for example the current working directory cap.
*)

let path_normalize ~path =
    let path = if (path="") then "/" else path in
    let relpath     = Filename.is_relative path in
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
    let rec concpath pl =
        match pl with
        | hd::tl -> hd^(
                            let sp = concpath tl in
                            if (sp <> "") then  
                                "/"^sp
                            else
                                ""
                        );
        | []     -> ""
    in     
    let path' = concpath pathlist' in

    (* TODO *)

    (nilcap,if (relpath = false) then "/"^path' else path')

(*
** SOAP directory and name interfaces
*)


let path_MAX = 255        (* maximal length of path *)


(*
** Perform a transaction to the SOAP server, used by the following
** requests. Returns (err_stat,hdr_trans) tuple. The request header
** argument fields extra and offset must be set outside from sp_mktrans. 
*)

let sp_mktrans 
        ~ntries:ntries
        ~dir_cs:dir
        ~cmd:cmd
        ~extra:extra    
        ~offset:offset
        ~inbuf:inbuf
        ~insize:insize
        ~outbuf: outbuf
        ~outsize:outsize
        =
    
    let hdr = header_new () in
    let hdr_rep = ref nilheader in

    let err = ref std_OK in
    let failed = ref std_SYSERR in

    try
    (
        for tries = 1 to ntries
        do
          for i = 0 to (dir.cs_final - 1)
          do
            let s = dir.cs_suite.(i) in

            if (s.s_current = true ) then
            begin
                hdr.h_command <- cmd;
                hdr.h_port <- s.s_object.cap_port;
                hdr.h_priv <- s.s_object.cap_priv;

                if (insize > 0) then
                    hdr.h_size <- insize
                else
                    hdr.h_size <- outsize;


                (* do the real transaction *)
                let (trans_stat,n,hdr2) = trans (hdr,inbuf,insize, 
                                                 outbuf,outsize) in

                hdr_rep := hdr2;

                if (trans_stat <> std_OK) then
                    err := trans_stat
                else
                    err := hdr2.h_status;

                if (!err = rpc_FAILURE && ntries > 1) then
                    (*
                    ** Retry idempotent operation, but return RPC_FAILURE
                    ** in case we don't succeed in a following iteration.
                    *)
                    failed := rpc_FAILURE
                else if (!err <> rpc_FAILURE) then
                begin
                    if (!err = std_OK && n > 0 && hdr2.h_size <> n ) then
                    begin
                        (*
                        ** Work around bug in old soap svr: it didn't always
                        ** set hdr.h_size (e.g. in sp_lookup).
                        *)
                        hdr2.h_size <- n;
                          
                    end;
                    raise Exit;
                end; 
            end;
          done;
        done;
        (!failed,!hdr_rep);
    ) with Exit -> (!err,!hdr_rep)


(*
** Root and current working directory capability set 
** XX TODO XX
*)
let dir_rootcs = ref nilcapset
let dir_workcs = ref nilcapset
 
(*
** Return the current working directory cap set
** XX TODO XX
*)

let sp_get_workdir () =
    let stat = 
            if (!dir_workcs = nilcapset) then
                std_CAPBAD 
            else
                std_OK
    in
    (stat,cs_copy (!dir_workcs))

(*
** Return the root dir cap set
*)

let sp_get_rootdir () =
    let stat = 
            if (!dir_rootcs = nilcapset) then
                std_CAPBAD 
            else
                std_OK
    in
    (stat,cs_copy (!dir_rootcs))


(*
** Set the current working and root directory
*)


let sp_set_workdir cap =
    dir_workcs := (cs_singleton cap)

let sp_set_rootdir cap =
    dir_rootcs := (cs_singleton cap)



(*
** sp_lookup  returns  the capability-set stored under the name
** 'path' relative to the directory 'root'. Warning:  if  the  NULL-string is
** given  as  the  path  then  the capability in 'dir' is for the directory
** required and so it is returned without checking to  see  if  it  is a
** valid capability.                                  
*)                                                    

let sp_lookup 
        ~root
        ~path
        =
    let _,path = path_normalize path in 
    let path_len = String.length path in

    if (path_len = 0) then
        failwith "sp_lookup: zero length path string";

    (*
    ** Protection against infinitely looping:
    *)

    let max_steps = 10 in       

    let out_buf_size    = (path_MAX + 1 + capset_SIZE) in
    let out_buf         = buf_create out_buf_size in

    let (err,cs_root) = 
        if (root = sp_DEFAULT) then
        begin 
            if (path.[0] = '/') then
                sp_get_rootdir ()
            else
                sp_get_workdir ();
        end
        else
            (std_OK,(cs_copy root))
    in


    (*
    ** Loop over the path components step by step and resolve the path.
    ** Each run, the next path component is the new parent directory
    ** for the next lookup.
    *)
    

    let err_stat = ref std_OK in

    let rec path_iter depth path cs_parent =


        if (depth > max_steps) then
        begin
            err_stat := std_OVERFLOW;
            raise Exit;
        end;

        let path_len = String.length path in

        if (path_len = 0 || 
            (path.[0] = '.' && path_len = 1))
        then
            cs_parent
        else
        begin
            
            let (err,hdr) =
                        sp_mktrans ~ntries:sp_NTRY
                                   ~dir_cs:cs_parent
                                   ~cmd:sp_LOOKUP
                                   ~extra:0
                                   ~offset:0
                                   ~inbuf:(buf_of_string (path^"\000"))
                                   ~insize:(path_len+1)
                                   ~outbuf:out_buf
                                   ~outsize:out_buf_size
            in


            if (err <> std_OK) then
            begin
                err_stat := err;
                raise Exit;
            end
            else if (hdr.h_status <> std_OK) then
            begin
                err_stat := hdr.h_status;
                raise Exit;
            end
            else
            begin
                let (pos,path_ret) = buf_get_string ~buf:out_buf ~pos:0
                in
                (* get the capset *)
                let (pos,cs_part) = buf_get_capset ~buf:out_buf   
                                                   ~pos:pos
                in

                (* remove a leading slash from path *)
                let path_len = String.length path_ret 
                in
                let path_mod = if (path_len > 0 && path_ret.[0] = '/') then
                                String.sub path_ret ~pos:(1)
                                                    ~len:(path_len-1)
                               else
                                    path_ret
                   in
                
                (* and the next lookup if necessary *)
                path_iter (depth+1) path_mod cs_part;
                                   
            end;       
        end;
    in


    if (err = std_OK) then
    begin       
        (* remove a leading slash from path *)
        let slash_pos = try String.index path '/'
                        with Not_found -> (-1)
        in
        let path_mod = if (slash_pos = -1) then
                            path
                       else if (slash_pos < path_len) then
                            String.sub path ~pos:(slash_pos+1)
                                            ~len:(path_len-slash_pos-1)
                       else
                            ""
        in
                        

        let cs_dir =
            try 
                path_iter 1 path_mod cs_root
            with Exit -> (nilcapset)
        in 
        
        (!err_stat,cs_dir)
    end
    else
        (err,nilcapset)


(*
** sp_traverse  returns  the last component of the path name 
** It also returns the capability for the directory up until the 
** last component of the path (relative to the directory dir).
**
** 
** NOTE: This function will return an unnormalized
** "." or "..", if that was the last or only component of the input path.
** This is correct, since this function is used only when the path name is
** logically specifying a directory and the name of an entry (in that
** directory) that is to be modified or have its attributes returned.
** 
** E.g. The path "/foo/bar/.." is supposed to refer to the (virtual) ".."
** entry in directory "/foo/bar", not to "/foo", and similarly for paths
** ending in ".".  If you don't see the difference, consider that
** "del -d /foo/." should return an error (can't delete "." from a
** directory), while "del -d /foo" should actually delete the directory
** /foo.  They are not equivalent in UNIX.
*)

let sp_traverse ~dir:dir ~path:path =

    let dir_cs = ref nilcapset in
    let err_stat = ref std_OK in

    (* last component in path *)
    let dir_name = Filename.dirname path in
    let last_name = Filename.basename path in

    if ((Filename.is_relative path) = false) then
    begin
        (* absolute path *)

        let (err,cs) = sp_lookup dir dir_name in
        dir_cs := cs;
        err_stat := err;
    end
    else
    begin
        (* relative directory *)
        if (dir = sp_DEFAULT) then
        begin
            let (err,cs) = sp_get_workdir () in
            dir_cs := cs;
            err_stat := err;
        end
        else
            dir_cs := cs_copy dir;
        err_stat := std_OK;
    end;
    (!err_stat,last_name,!dir_cs)

(*
** Sp_append  adds the directory entry specified by 'name', relative to
** the directory specified by 'dir'. The entry may not exist already.  The
** new entry has the capability-set cs.
** The  column  masks  for the directory entry are specified by the ncols
** entries in the array cols. To avoid first having to look up the number
** of  columns the directory has, any legal number of columns masks (1 up
** to SP_MAXCOLUMNS) is accepted. Masks referring to non-existent columns
** are ignored, and missing masks are set to zero.
*)

let sp_append ~dir:dir ~name:name ~obj:obj ~cols:cols =
    let ncols = Array.length cols in
    if (ncols > sp_MAXCOLUMNS) then
        failwith "sp_append: invalid cols array";

    let inbuf_size = (path_MAX + 1) + 
                     (sp_MAXCOLUMNS * 4 + capset_SIZE) in
    let inbuf = buf_create inbuf_size in

    let (err,last,dir_cs) = sp_traverse ~dir:dir ~path:name in

    if (err = std_OK) then
    begin
        let bufpos = ref 0 in
        bufpos := buf_put_string ~buf:inbuf ~pos:(!bufpos) ~str:last;
        bufpos := buf_put_capset ~buf:inbuf ~pos:(!bufpos) ~cs:obj;
        
        for n = 0 to (ncols-1)
        do
            bufpos := buf_put_right_bits ~buf:inbuf ~pos:(!bufpos)
                                          ~right:(cols.(n));
        done;        
        let (err,hdr) = sp_mktrans  ~ntries:1
                                    ~dir_cs:dir_cs
                                    ~cmd:sp_APPEND
                                    ~extra:0
                                    ~offset:0
                                    ~inbuf:inbuf 
                                    ~insize:!bufpos
                                    ~outbuf:nilbuf 
                                    ~outsize:0
        in


        err
        
    end
    else
        err 

(*
** sp_delete deletes the directory entry (which may itself be a
** directory capability)  specified  by  'name'.  
*)


let sp_delete ~dir:dir ~name:name =

    let (err,last,dir_cs) = sp_traverse ~dir:dir ~path:name in

    if (err = std_OK) then
    begin
        let (err,hdr) = sp_mktrans  ~ntries:1
                                    ~dir_cs:dir_cs
                                    ~cmd:sp_DELETE
                                    ~extra:0
                                    ~offset:0
                                    ~inbuf:(buf_of_string (last^"\000")) 
                                    ~insize:((String.length last)+1)
                                    ~outbuf:nilbuf
                                    ~outsize:0
        in
        err
        
    end
    else
        err 
    
(*
** Create a new directory capability set.
*)

let sp_create ~server ~cols =

    let ncols = Array.length cols in
    if (ncols > sp_MAXCOLUMNS) then
        failwith "sp_create: invalid cols array";

    
    let err,srv_cs =
                    if (server=sp_DEFAULT) then
                        sp_get_rootdir()
                    else
                        std_OK,server
    in
        
    let inbuf_size = sp_MAXCOLUMNS*(path_MAX+1)  in
    let inbuf = buf_create inbuf_size in

    if (err=std_OK) then
    begin
        let bufpos = ref 0 in
        
        for n = 0 to (ncols-1)
        do
            bufpos := buf_put_string ~buf:inbuf ~pos:(!bufpos) ~str:cols.(n);
        done;        
        let (err,hdr) = sp_mktrans  ~ntries:sp_NTRY
                                    ~dir_cs:srv_cs
                                    ~cmd:sp_CREATE
                                    ~extra:0
                                    ~offset:0
                                    ~inbuf:inbuf 
                                    ~insize:!bufpos
                                    ~outbuf:nilbuf 
                                    ~outsize:0
        in
        if (err=std_OK) then
        begin
            let cap = { cap_port = hdr.h_port; cap_priv = hdr.h_priv } in
            std_OK,cs_singleton cap
        end
        else
            err,nilcapset
    end
    else
        err,nilcapset 


(*
** sp_list: return the row list of the directory specified by
**          'dir'
*)

let sp_list ~dir:dir =
    (*
    ** Extract directory 'numrows' rows from buffer 'buf' of content 
    ** size 'size' starting at offset 'start' to 'dd'.
    *)

    let errstat = ref std_OK in
  
    let addtodir dd firstrow numrows buf start size=

        let cpos = ref start in  

        for i = firstrow to (numrows-1)
        do
            let (pos,name) = buf_get_string buf !cpos in
            let rbits = Array.create dd.dd_ncols (Rights_bits 0) in

            cpos := pos;
            for i = 0 to (dd.dd_ncols-1) 
            do
                let (pos,rib) = buf_get_right_bits buf !cpos in
                rbits.(i) <- Rights_bits rib;
                cpos := pos; 
            done;

            dd.dd_rows <- dd.dd_rows @ [{ 
                                            d_name = name; 
                                            d_columns = rbits;
                                         }] ;           

            if (!cpos - start > size) then
                raise Buf_overflow;
        done;
    in

    let buf = buf_create sp_BUFSIZE  in

    let (err,hdr_rep) = sp_mktrans  ~ntries:sp_NTRY
                                    ~dir_cs:dir
                                    ~cmd:sp_LIST
                                    ~extra:0            (* start at row 0 *)
                                    ~offset:0
                                    ~inbuf:nilbuf
                                    ~insize:0 
                                    ~outbuf:buf
                                    ~outsize:sp_BUFSIZE
    in
    errstat := err;

    if (err = std_OK) then
    begin
        let ret_size    = hdr_rep.h_size in
        let next_row    = hdr_rep.h_extra in
        let (pos,ncols) = buf_get_int16 buf 0 in
        let (pos,nrows) = buf_get_int16 buf pos in
        let colnames    = Array.create ncols "" in

        let cpos = ref pos in
        for n = 0 to (ncols-1) 
        do
            let (pos,name) = buf_get_string buf !cpos in
            colnames.(n) <- name;            
            cpos := pos;
        done;

        let dd = {
                    dd_capset = dir;
                    dd_ncols  = ncols;
                    dd_nrows  = nrows;
                    dd_colnames = colnames;
                    dd_rows = [];
                    dd_curpos = 0;
                 }
        in
        
        if (nrows > 0) then
        begin
            (*
            ** append the current chunk of rows to dd 
            *)
            let numrows = if next_row <> sp_NOMOREROWS then 
                            next_row 
                          else
                            nrows
            in
#ifdef DEBUG
            Db.Pr.sd 1 "sp_list: numrows" numrows;
#endif
            addtodir dd 0 numrows buf !cpos ret_size;

            if (next_row > 0) then
            begin
                (* 
                ** There are some more rows that didn't fit in one request.
                ** Get them, and add them to the dd row list.
                *)

                let gotrows = ref numrows in
                let nextrow = ref next_row in

                try
                while (!gotrows < nrows && !nextrow <> sp_NOMOREROWS)
                do
                    let (err,hdr_rep) = sp_mktrans  
                                    ~ntries:sp_NTRY
                                    ~dir_cs:dir
                                    ~cmd:sp_LIST
                                    ~extra:(!nextrow)
                                    ~offset:0
                                    ~inbuf:nilbuf
                                    ~insize:0 
                                    ~outbuf:buf
                                    ~outsize:sp_BUFSIZE
                    in
                    if (err = std_OK) then
                    begin
                        let ret_size    = hdr_rep.h_size in
                        let next_row    = hdr_rep.h_extra in
                        let (pos,ncols) = buf_get_int16 buf 0 in
                        let (pos,nrows) = buf_get_int16 buf pos in
                        let colnames    = Array.create ncols "" in

                        let cpos = ref pos in
                        for n = 0 to (ncols-1) 
                        do
                            let (pos,name) = buf_get_string buf !cpos in
                            cpos := pos;
                        done;
                        (*
                        ** append the current chunk of rows to dd 
                        *)
                        let numrows = if next_row > 0 then 
                                (next_row - !nextrow) 
                              else
                                (nrows - !nextrow)
                        in
                        addtodir dd !nextrow numrows buf !cpos ret_size;
                        gotrows := !gotrows + numrows;
                    end
                    else
                    begin
                        errstat := err;
                        raise Exit;    
                    end;

                    nextrow := next_row;

                done;
                with Exit -> ();             
            end
        end;

        (!errstat,dd) 
    end
    else
        (!errstat,nil_sp_dir)



