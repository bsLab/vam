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
**    $AUTHORS:     
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.13
**
**    $INFO:
**
** DNS: Directory and Name Service
** Server RPC loop.
**
**
**    $ENDOFINFO
**
*)

/* #define DEBUG 1 */


open StdLabels

open Amoeba
open Bytebuf
open Cmdreg
open Stderr
open Stdcom
open Stdcom2
open Capset
open Sys
open Rpc
open Buf
open Mutex
open Dblist
open Thread
open Ar
open Syslog

module Str = StrLabels
open Str

let debug = 1


open Dns_common
open Dns_server

let dns_dying = ref false 


(*
** The server loop. The input request and output reply buffer sizes must be
** specified.
**  server: afs_server
**  sema: semaphore used to synchronosize the master server
**  nthreads: number of total server threads
**  inbuf_size: request buffer size (commonly dns_REQBUFSZ)
**  outbuf_size: reply buffer size  (commonly dns_REQBUFSZ)
*)

let server_loop ~server
                ~sema
                ~nthreads
                ~inbuf_size
                ~outbuf_size
    =
    let super = server.dns_super in
    dns_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!dns_dying = false) then
                       begin
                            dns_dying := true;
                            (*
                            ** Sync the disk.
                            *)
                            let stat' = server.dns_sync () in
                            
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (nthreads-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up sema;
                            done;
                            stat'
                       end
                       else
                            std_OK
                       in
            stat
    in        

    try
    begin
        let ibuf = buf_create inbuf_size in     (* request buffer *)
        let obuf = buf_create outbuf_size in    (* reply buffer   *)
        
        let hdr_rep = header_new () in
        let replen = ref 0 in
        
        let getport = server.dns_super.dns_getport in

        while (true)
        do
        try
          begin
            replen := 0;
            hdr_rep.h_size <- 0;
            hdr_rep.h_status <- std_OK;
            hdr_rep.h_priv <- priv_copy nilpriv;
                        
            let stat,reqlen,hdr_req = getreq (getport,ibuf,inbuf_size) in

            let priv =hdr_req.h_priv in

            (
            match hdr_req.h_command with

            (*
            ** dns_LOOKUP
            *)
            
            | com when com = dns_LOOKUP ->
            begin

#ifdef DEBUG
                Db.Pr.ss debug "dns_LOOKUP"
                    (Ar.ar_priv hdr_req.h_priv);
#endif
                (*
                ** Get the path string
                *)
                
                let pos,path = buf_get_string ~buf:ibuf ~pos:0 in

#ifdef DEBUG
                Db.Pr.ss debug "LOOKUP path" path;
#endif                
                let stat,cs,path_rest = dns_req_lookup ~server:server
                                                       ~priv:priv
                                                       ~path:path
                                                       in
                if (stat = std_OK) then
                begin
                    (*
                    ** Put the path_rest string and the capset.
                    *)
                    let pos = buf_put_string ~buf:obuf 
                                             ~str:path_rest 
                                             ~pos:0 
                                             in
                    let pos = buf_put_capset ~buf:obuf
                                             ~cs:cs
                                             ~pos:pos
                                             in
                    replen := pos;                         
#ifdef DEBUG
                    Db.Pr.ss debug "LOOKUP" "Ok";
#endif
                end;
                
                hdr_rep.h_status <- stat;
            end;

            (*
            ** dns_LIST
            *)
            
            | com when com = dns_LIST ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server: dns_LIST" (ar_priv priv);
#endif            
                let firstrow = hdr_req.h_extra in
                let stat,nrows,ncols,colnames,rows = 
                        dns_req_list ~server:server
                                     ~priv:priv
                                     ~firstrow:firstrow
                                     in

#ifdef DEBUG
                Db.Pr.sd debug "LIST nrows" nrows;
                Db.Pr.sd debug "LIST ncols" ncols;
                Db.Pr.sd debug "LIST rows" (List.length rows);
#endif                
                (*
                ** Put nrows,ncols and all rows fitting in the obuf.
                *)
                
                let pos = buf_put_int16 ~buf:obuf ~pos:0 ~int16:ncols in
                let pos = buf_put_int16 ~buf:obuf ~pos:pos ~int16:nrows in
                
                (*
                ** Put the col names
                *)
                let pos = ref pos in
                for i = 0 to (ncols-1)
                do
                    let pos' = buf_put_string ~buf:obuf
                                              ~pos:!pos
                                              ~str:colnames.(i)
                                              in
                    pos := pos';                                              
                done;
                
                let ndone = ref 0 in
                let rec pack rl = 
                    match rl with
                    | hd::tl ->
                    begin
                        let name,cols = hd in
                        let pos' = ref (buf_put_string ~buf:obuf
                                                  ~pos:!pos
                                                  ~str:name
                                       ) in 

#ifdef DEBUG
                        Db.Pr.ss debug "LIST row" name;
                        Db.Pr.sd debug "LIST cols" (Array.length cols);
#endif                         
                        for i = 0 to ncols-1 
                        do
                            pos' :=  (buf_put_rights_bits ~buf:obuf
                                                       ~pos:!pos'
                                                       ~rights:cols.(i)
                                     );                  
                        done;
                        pos := !pos';
                        incr ndone;
                        pack tl;
                    end;
                    | [] -> ();
                in
                
                (
                    try
                        begin
                            pack rows;
                            hdr_rep.h_extra <- dns_NOMOREROWS;
                        end
                    with
                        | Buf_overflow -> 
                        begin
                            hdr_rep.h_extra <- firstrow + !ndone;
                        end;
                );
                
                hdr_rep.h_size  <- !pos; 

#ifdef DEBUG
                Db.Pr.sd debug "LIST extra" (hdr_rep.h_extra);
                Db.Pr.sd debug "LIST size" (!pos);
#endif
                replen := !pos;    
                hdr_rep.h_status <- std_OK;
            end;

            | com when com = dns_APPEND ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_APPEND";
#endif
                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                (*
                ** Get the capset
                *)
                
                let pos,cs = buf_get_capset ~buf:ibuf
                                            ~pos:pos
                                            in
#ifdef DEBUG
                Db.Pr.ss debug "APPEND: newcs" (Ar.ar_cs cs);
#endif
                                            
                (*
                ** Allow 1 up to dns_MAXCOLUMNS new masks to be present.
                ** The caller may not know (or care) how many columns this
                ** directory has.                                            
                *)
                
                let cols = ref [] in
                let pos = ref pos in
                let stat = ref std_OK in
                
                (
                  try
                  begin
                    for i = 1 to dns_MAXCOLUMNS
                    do
                        let pos',rights = buf_get_rights_bits 
                                                    ~buf:ibuf
                                                    ~pos:!pos
                                                    in
                        pos := pos';
                        
                        cols := !cols @ [rights];                                                               
#ifdef DEBUG
                        Db.Pr.sd debug "dns_APPEND: got rights"
                                ( let Rights_bits r = rights in r );
#endif
                        if (!pos >= reqlen) then
                            raise Exit;     (* got all rights *)
                    done;
                  end
                  with 
                    | Buf_overflow 
                    | Exit ->
                    begin
                        if (!cols = []) then
                            stat := std_ARGBAD;
                    end;
                );
                if (!stat = std_OK) then
                begin
                    let cols = Array.of_list !cols in
                    stat := dns_req_append ~server:server
                                           ~priv:priv
                                           ~name:rowname
                                           ~cols:cols
                                           ~capset:cs;
                                            
                end;       
                hdr_rep.h_status <- !stat;
            end;
            
            | com when com = dns_CREATE ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_CREATE";
#endif

                (*
                ** TODO: DNS_GROUP 
                ** In this case, the original request was prefixed 
                ** with the check field for the new directory.
                *)
                
                (*
                ** Get the column names
                *)
                
                let cols = ref [] in
                let pos = ref 0 in
                let stat = ref std_OK in
                
                (
                  try
                  while (true) 
                  do
                    if (!pos >= reqlen) then
                        raise Exit;
                        
                    let pos',col = buf_get_string ~buf:ibuf
                                                  ~pos:!pos
                                                  in
                    pos := pos';
                    cols := !cols @ [col];                                                  
                  done
                  with
                    | Exit ->
                    begin
                        if (List.length !cols = 0) then
                            stat := std_ARGBAD;        
                    end;
                );
                if (!stat = std_OK) then
                begin
                    let cols = Array.of_list !cols in
                    
                    let stat',cs = dns_req_create ~server:server
                                                  ~priv:priv 
                                                  ~colnames:cols
                                                  in
                    stat := stat';
                    if (!stat = std_OK) then
                    begin
                        (*
                        ** The return priv field contains the new
                        ** created dir object.
                        *)
                        
                        let cap = cs.cs_suite.(0).s_object in
#ifdef DEBUG
                        Db.Pr.ss debug "dns_CREATE" (Ar.ar_cap cap);
#endif
                        hdr_rep.h_port <- port_copy (cap.cap_port);
                        hdr_rep.h_priv <- priv_copy (cap.cap_priv);
                    end;                                                  
                end;
                hdr_rep.h_status <- !stat;
                
            end;

            | com when com = dns_DISCARD ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_DISCARD";
#endif
                let stat = dns_req_discard ~server:server
                                           ~priv:priv
                                           in
                hdr_rep.h_status <- stat;                                           
            end;
            
            | com when com = dns_CHMOD ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_CHMOD";
#endif
                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                (*
                ** Allow 1 up to dns_MAXCOLUMNS new masks to be present.
                ** The caller may not know (or care) how many columns this
                ** directory has.                                            
                *)
                
                let cols = ref [] in
                let pos = ref pos in
                let stat = ref std_OK in
                
                (
                  try
                  begin
                    for i = 1 to dns_MAXCOLUMNS
                    do
                        if (!pos >= reqlen) then
                            raise Exit;
                            
                        let pos',rights = buf_get_rights_bits 
                                                    ~buf:ibuf
                                                    ~pos:!pos
                                                    in
                        pos := pos';
                        cols := !cols @ [rights];                                                               
                    done;
                  end
                  with 
                    | Exit | Buf_overflow ->
                    begin
                        if (!cols = []) then
                            stat := std_ARGBAD;
                    end;
                );
                if (!stat = std_OK) then
                begin
                    let cols = Array.of_list !cols in

#ifdef DEBUG
                    Db.Pr.sd debug "CHMOD cols" (Array.length cols);
#endif                    
                    stat := dns_req_chmod  ~server:server
                                           ~priv:priv
                                           ~name:rowname
                                           ~cols:cols;
                                            
                end;       
                hdr_rep.h_status <- !stat;
                
            end;
            | com when com = dns_DELETE ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_DELETE";
#endif
                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                let stat = dns_req_delete ~server:server
                                          ~priv:priv
                                          ~name:rowname
                                          in
                hdr_rep.h_status <- stat;                                                          
            end;

            | com when com = dns_REPLACE ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_REPLACE";
#endif

                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                (*
                ** Get the capset
                *)
                
                let pos,cs = buf_get_capset ~buf:ibuf
                                            ~pos:pos
                                            in

                let stat = dns_req_replace ~server:server
                                           ~priv:priv
                                           ~name:rowname
                                           ~newcs:cs
                                           in

                hdr_rep.h_status <- stat;                                                       
            end;
            
            | com when com = dns_RENAME ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_RENAME";
#endif

                (*
                ** Get the old rowname
                *)
                
                let pos,oldname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                (*
                ** Get the new rowname
                *)
                
                let pos,newname = buf_get_string ~buf:ibuf 
                                                 ~pos:pos
                                                 in
                                                 

                let stat = dns_req_rename  ~server:server
                                           ~priv:priv
                                           ~oldname:oldname
                                           ~newname:newname
                                           in

                hdr_rep.h_status <- stat;                                                       
            end;
            
            | com when com = dns_GETMASKS ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_server" "dns_GETMAKS";
#endif
                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                let stat,cols = dns_req_getmasks ~server:server
                                                 ~priv:priv
                                                 ~name:rowname
                                                 in
                                  
                let ncols = Array.length cols in                                                 
                let pos = ref 0 in
                
                if (stat = std_OK) then
                begin
                    for i = 0 to ncols-1                                                                                                  
                    do
                        let pos' = buf_put_rights_bits ~buf:obuf 
                                                       ~pos:!pos
                                                       ~rights:cols.(i)
                                                       in
                                                   
                        pos := pos';                                                   
                    done;
                    hdr_rep.h_size <- !pos;
                end
                else 
                    hdr_rep.h_size <- 0;

                replen := hdr_rep.h_size;
                hdr_rep.h_status <- stat;
            end;

            | com when com = dns_SETLOOKUP ->
            begin
#ifdef DEBUG
                Db.Pr.ss debug "dns_SETLOOKUP" (
                    Ar.ar_priv hdr_req.h_priv);
#endif

                (*
                ** Get the dir capsets and the rownames from
                ** the input buffer.
                *)
                let pos = ref 0 in
                
                let dirs = ref [] in
                (
                    try
                        while (true) 
                        do
                            if (!pos >= reqlen) then
                                raise Exit;
                                
                            let pos',cs =  buf_get_capset ~buf:ibuf
                                                          ~pos:!pos
                                                          in
                            let pos',name = buf_get_string ~buf:ibuf
                                                           ~pos:pos'
                                                            in 

#ifdef DEBUG
                            Db.Pr.ss debug "SETLOOKUP row" name;
#endif
                            dirs := !dirs @ [cs,name];
                            pos := pos';                                                                                                                       
                        done;
                    with
                        | Exit -> ();
                );
#ifdef DEBUG
                Db.Pr.sd debug "SETLOOKUP dirs" (List.length !dirs);
#endif                
                let rows = dns_req_setlookup ~server:server
                                                  ~dirs:!dirs
                                                  in
#ifdef DEBUG
                Db.Pr.sd debug "SETLOOKUP rows" (List.length rows);
#endif

                let stat = std_OK in
                
                if (stat=std_OK) then
                begin
#ifdef DEBUG
                    Db.Pr.ss debug "SETLOOKUP" "Ok";
#endif                    
                    let pos = ref 0 in
                    
                    let rec pack dl =
                        match dl with
                        | hd::tl ->
                        begin
                            let stat,time,cs = hd in
                            (*
                            ** Status = std_OK 
                            *)
                            let pos' = buf_put_int16 ~buf:obuf
                                                     ~pos:!pos
                                                     ~int16:(
                                                        let Status stat =
                                                            stat in
                                                        stat)
                            in
                            (*
                            ** Type cap ?? 
                            *)
                            let pos' = buf_put_cap ~buf:obuf
                                                   ~pos:pos'
                                                   ~cap:nilcap
                                                   in
                            (*
                            ** Row Time field                                                   
                            *)
                            let pos' = buf_put_int32 ~buf:obuf
                                                     ~pos:pos'
                                                     ~int32:time
                            in
                            (*
                            ** Row capset
                            *)
                            let pos' = buf_put_capset ~buf:obuf
                                                      ~pos:pos'
                                                      ~cs:cs
                                                      in
                            pos := pos';  
                            pack tl;                                                    
                        end;
                        | [] -> ();
                    in

                    try
                    begin
                        pack rows;
#ifdef DEBUG
                        Db.Pr.sd debug "SETLOOKUP size" (!pos);
#endif                        
                        hdr_rep.h_size <- !pos;
                        replen := !pos;
                        hdr_rep.h_status <- std_OK;     
                    end;
                    with
                        | Buf_overflow ->
                        begin
                            (* ?? *)
                            hdr_rep.h_size <- !pos;
                            replen := !pos;
                            hdr_rep.h_status <- std_OK;     
                        end;                
                        
                end
                else
                begin
                    let pos = buf_put_int16 ~buf:obuf
                                            ~pos:0
                                            ~int16:(
                                                let Status stat =
                                                    std_NOTFOUND in
                                                stat 
                                            ) in
                    hdr_rep.h_size <- pos;
                    replen := pos;
                                            
                    hdr_rep.h_status <- stat;
                end;                                                                              
            end;
            | com when com = dns_GETDEFBULLET ->
            begin
                let fs_default = super.dns_fs_server.fs_default in
                let fscap = super.dns_fs_server.fs_cap.(fs_default) in
                let pos = buf_put_cap ~buf:obuf
                                      ~pos:0
                                      ~cap:fscap in
#ifdef DEBUG
                Db.Pr.ss 1 "dns_GETDEFBULLET: fscap" (ar_cap fscap);
#endif
                hdr_rep.h_size <- pos;
                hdr_rep.h_status <- std_OK;
                replen := pos;                                          
            end;

            | com when com = std_INFO ->
            begin
                let Objnum obj = prv_number hdr_req.h_priv in
                let Rights_bits rights = prv_rights hdr_req.h_priv in
                
#ifdef DEBUG
                Db.Pr.sd debug "std_INFO" obj;
#endif
                let stat = ref std_OK in
                let pos = ref 0 in

                let str = 
                    if (obj = 0) then
                        "DNS server capability"
                    else
                    begin
                        let stat',dir = request_dir ~server:server
                                                    ~priv:priv
                                                    ~req:(Rights_bits 0)
                                                    in
                        stat := stat';
                        if (stat' = std_OK) then
                        begin
                            ignore(release_dir ~server:server ~dir:dir);

                            "/"^
                            (if rights land dns_RGT_DEL > 0 then "d" else "-")^ 
                            (if rights land dns_RGT_MOD > 0 then "w" else "-")^ 
                            (if rights land 0x1 > 0 then "1" else "-")^ 
                            (if rights land 0x2 > 0 then "2" else "-")^ 
                            (if rights land 0x4 > 0 then "3" else "-")^ 
                            (if rights land 0x8 > 0 then "4" else "-")
                        end
                        else
                            "";                                                            
                    end;
                in
                if (!stat = std_OK) then
                begin
                    let pos' = buf_put_string ~buf:obuf
                                              ~pos:0
                                              ~str:str
                                              in
                    pos := pos';
                end;                    
                hdr_rep.h_size <- !pos;
                hdr_rep.h_status <- !stat;
                replen := !pos;                                          
            end;


            | com when com = std_STATUS ->
            begin
                let Objnum obj = prv_number priv in
                let stat = ref std_OK in
                let pos = ref 0 in
                
                if (obj> 0) then
                begin
                    let stat',dir = request_dir ~server:server
                                            ~priv:priv
                                            ~req:(Rights_bits 0)
                    in
                    stat := stat';
                    if (stat' = std_OK) then
                    begin
                        let objnum = dir.dd_objnum in
                        let live = dir.dd_live in
                        let time = dir.dd_time in
                        let nrows = dir.dd_nrows in
                        let ncols = dir.dd_ncols in
                        let cols  = dir.dd_colnames in
                        let state = match dir.dd_state with
                                | DD_invalid -> "Invalid ?";
                                | DD_modified -> "Modified";
                                | DD_locked -> "Locked";
                                | DD_unlocked -> "Unlocked";
                                in
                                
                        ignore(release_dir ~server:server ~dir:dir);
                        let statstr = "DNS object: "^
                                  (if objnum > 0 then 
                                    "Directory "^(string_of_int objnum) 
                                   else
                                    "Administration ")^
                                  "\nTime = "^(string_of_int time)^"  "^
                                  "Live = "^(string_of_int live)^"  "^
                                  "State = "^state^"  "^
                                  "\nNumRows = "^(string_of_int nrows)^"  "^
                                    "NumCols = "^(string_of_int ncols)^
                                  "\n"^"Column names: "^(
                                    let colstr = ref "" in
                                    for i = 0 to (Array.length cols)-1
                                    do
                                        colstr := !colstr^
                                                  (if i> 0 then 
                                                    " # "
                                                   else
                                                    "")^cols.(i);
                                    done;
                                    !colstr
                                  ) 
                                  in 
                        pos := buf_put_string ~buf:obuf ~pos:0
                                              ~str:statstr;                                    
                                  
                    end
                end
                else
                begin
                    (*
                    ** Server capability. Not a directory.
                    *)
                    let auth = prv_decode ~prv:priv
                                          ~rand:super.dns_checkfield
                        in
                    if (auth = true) then
                    begin
                        let stat',str = server.dns_stat ()in
                        stat := stat';
                        pos := buf_put_string ~buf:obuf ~pos:0
                                                  ~str:str;                                    
                
                    end
                    else
                        stat := std_DENIED;
                end;                                        

                hdr_rep.h_size <- !pos;
                replen := !pos;                                          
                hdr_rep.h_status <- !stat;
            end;

            | com when com = std_TOUCH ->
            begin
                let stat = dns_req_touch ~server:server
                                         ~priv:priv
                    in
                hdr_rep.h_status <- stat;            
            end;

            | com when com = std_AGE ->
            begin
                let stat = dns_req_age   ~server:server
                                         ~priv:priv
                    in
                hdr_rep.h_status <- stat;            
            end;

            | com when com = std_EXIT ->
            begin
                    initial := not (!dns_dying);
                    let rights = prv_rights hdr_req.h_priv in
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (obj = 0 &&
                       (prv_decode ~prv:hdr_req.h_priv 
                                   ~rand:super.dns_checkfield)
                       ) = true then
                    begin 
                        let stat = on_exit { cap_port = hdr_req.h_port;
                                             cap_priv = priv} in

                        hdr_rep.h_status <- stat;
                        ignore( putrep (hdr_rep,obuf,!replen));
                        raise Exit;            
                    end
                    else
                    begin
                        hdr_rep.h_status <- std_DENIED;
                    end;
            end;
            
            | _ -> 
            begin
                let Command com = hdr_req.h_command in
#ifdef DEBUG
                Db.Pr.sd debug "dns_server std_COMBAD" com;
#endif
                hdr_rep.h_status <- std_COMBAD;
            end;

            );
            
            ignore( putrep (hdr_rep,obuf,!replen));


          end
          with
            | Buf_overflow ->
            begin
                replen := 0;
                hdr_rep.h_status <- std_ARGBAD;
                ignore( putrep (hdr_rep,obuf,!replen));
            end;
        done;
    end
    with
        | Exit -> 
                  sys_log Sys_info "DNS: Server thread exited normally.\n"; 
                  if (!initial) then
                    Sema.sema_up sema;
  

