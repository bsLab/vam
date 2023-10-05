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
**    $INITIAL:     (C) 2004
**    $CREATED:
**    $MODIFIED:
**    $VERSION:     1.05
**
**  Kernel Boot directory and disk module server implementation.
**  This server module emulates a DNS/AFS interface for the kernel
**  boot partition holding kernels, binaries needed for bootstrap
**  purposes and configuation files with a very simple
**  filesystem.
**
**  RPC server loop.
**
**    $ENDOFINFO
**
*)

(* #define DEBUG *)

  
open Amoeba
open Bytebuf
open Stderr 
open Disk_common
open Disk_client
open Afs_common 
open Dns_common 
open Bootdir    
open Bootdisk_common
open Bootdisk_server
open Mutex
open Thread
open Machtype
open Capset  
open Stdcom
open Stdcom2
open Rpc
open Buf
open Ar
open Printf
open Stdsrvparams
open Syslog

let bd_dying = ref false 

(*
** The server loop. The input request and output reply buffer sizes must be
** specified.
**  disk
**  sema: semaphore used to synchronosize the master server
**  nthreads: number of total server threads
**  inbuf_size: request buffer size (commonly disk_REQBUFSZ)
**  outbuf_size: reply buffer size  (commonly disk_REQBUFSZ)
*)

(*
** DNS module
*)

let server_dns_loop ~disk
                ~sema
                ~nthreads
                ~inbuf_size
                ~outbuf_size
    =
    bd_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!bd_dying = false) then
                       begin
                            bd_dying := true;
                            
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (nthreads-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up sema;
                            done;
                            std_OK
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
        
        let getport = disk.bdisk_prvport in
        let putport = priv2pub getport in

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
                Db.Pr.ss 1 "dns_LOOKUP"
                    (Ar.ar_priv hdr_req.h_priv);
#endif
                (*
                ** Get the path string
                *)
                let pos,path = buf_get_string ~buf:ibuf ~pos:0 in

#ifdef DEBUG
                Db.Pr.ss 1 "LOOKUP path" path;
#endif                
                let stat,cs,path_rest = bd_lookup ~disk:disk
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
                    Db.Pr.ss 1 "LOOKUP" "Ok";
#endif
                end;
                
                hdr_rep.h_status <- stat;
            end;

            | com when com = dns_SETLOOKUP ->
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "dns_SETLOOKUP" (
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
                            Db.Pr.ss 1 "SETLOOKUP row" name;
#endif
                            dirs := !dirs @ [cs,name];
                            pos := pos';                                        
                                                                               
                        done;
                    with
                        | Exit -> ();
                );
#ifdef DEBUG
                Db.Pr.sd 1 "SETLOOKUP dirs" (List.length !dirs);
#endif                
                let rows = bd_setlookup ~disk:disk
                                        ~dirs:!dirs
                                        in
#ifdef DEBUG
                Db.Pr.sd 1 "SETLOOKUP rows" (List.length rows);
#endif

                let stat = std_OK in
                
                if (stat=std_OK) then
                begin
#ifdef DEBUG
                    Db.Pr.ss 1 "SETLOOKUP" "Ok";
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
                        Db.Pr.sd 1 "SETLOOKUP size" (!pos);
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

            (*
            ** dns_LIST
            *)
            
            | com when com = dns_LIST ->
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "bootdisk_server: dns_LIST" (ar_priv priv);
#endif            
                let firstrow = hdr_req.h_extra in
                let stat,nrows,ncols,colnames,rows = 
                        bd_list ~disk:disk
                                     ~priv:priv
                                     ~firstrow:firstrow
                                     in

#ifdef DEBUG
                Db.Pr.sd 1 "LIST nrows" nrows;
                Db.Pr.sd 1 "LIST ncols" ncols;
                Db.Pr.sd 1 "LIST rows" (List.length rows);
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
                        Db.Pr.ss 1 "LIST row" name;
                        Db.Pr.sd 1 "LIST cols" (Array.length cols);
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
                Db.Pr.sd 1 "LIST extra" (hdr_rep.h_extra);
                Db.Pr.sd 1 "LIST size" (!pos);
#endif
                replen := !pos;    
                hdr_rep.h_status <- std_OK;
            end;

            | com when com = dns_APPEND ->
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "server_dns_loop" "dns_APPEND";
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
                Db.Pr.ss 1 "APPEND: newcs" (Ar.ar_cs cs);
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
                        Db.Pr.sd 1 "dns_APPEND: got rights"
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
                    stat := bd_append ~disk:disk
                                           ~priv:priv
                                           ~name:rowname
                                           ~cols:cols
                                           ~capset:cs;
                                            
                end;       
                hdr_rep.h_status <- !stat;
            end;

            | com when com = dns_DELETE ->
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "bd_dns_server" "dns_DELETE";
#endif
                (*
                ** Get the rowname
                *)
                
                let pos,rowname = buf_get_string ~buf:ibuf 
                                                 ~pos:0
                                                 in
                                                 
                let stat = bd_delete ~disk:disk
                                          ~priv:priv
                                          ~name:rowname
                                          in
                hdr_rep.h_status <- stat;                                       
                   
            end;

            | com when com = dns_RENAME ->
            begin
#ifdef DEBUG
                Db.Pr.ss 1 "bdisk_dns_server" "dns_RENAME";
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
                                                 

                let stat = bd_rename  ~disk:disk
                                           ~priv:priv
                                           ~oldname:oldname
                                           ~newname:newname
                                           in

                hdr_rep.h_status <- stat;                                       
                
            end;

            | com when com = dns_GETDEFAFS ->
            begin
                let fscap = {cap_port = disk.bdisk_file_pubport;
                             cap_priv = prv_encode (Objnum 0)
                                                   prv_all_rights
                                                   disk.bdisk_file_chkport} in
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
                Db.Pr.sd 1 "server_dns_loop: std_INFO" obj;
#endif
                (*
                ** DNS object !
                *)
                let stat = ref std_OK in
                let pos = ref 0 in
                let str = 
                    if (obj = 0) then
                        "BOOTDISK server capability"
                    else
                    begin
                        let stat' = bd_request_dir ~disk:disk
                                                   ~priv:priv
                                                   ~req:(Rights_bits 0)
                                                   in
                        stat := stat';
                        if (stat' = std_OK) then
                        begin
                            ignore(bd_release_dir ~disk:disk);

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
                let Objnum obj = prv_number hdr_req.h_priv in
                let Rights_bits rights = prv_rights hdr_req.h_priv in
                
#ifdef DEBUG
                Db.Pr.sd 1 "server_dns_loop: std_STATUS" obj;
#endif
                (*
                ** DNS object !
                *)
                let stat = ref std_OK in
                let pos = ref 0 in
                let str = 
                        let stat' = bd_request_dir ~disk:disk
                                                   ~priv:priv
                                                   ~req:(Rights_bits 0)
                                                   in
                        stat := stat';
                        if (stat' = std_OK) then
                        begin
                            ignore(bd_release_dir ~disk:disk);
                            let str = ref "" in
                            str := !str ^ "Bootdisk table:\n";
                            List.iter (fun file ->
                                    str := !str ^ (sprintf
                                    "  File <%s> bstart=%d bsize=%d fsize=%d flag=%s\n"
                                    file.boot_name
                                    file.boot_start
                                    file.boot_size
                                    file.boot_file_size
                                    (match file.boot_flag with
                                     | Boot_unlocked -> "UNLOCKED";
                                     | Boot_locked -> "LOCKED";
                                     | Boot_named -> "NAMED";
                                     | Boot_invalid -> "INVAL"));
                                ) disk.bdisk_table;
                            str := !str ^ "Freeblock table:\n";
                            List.iter (fun fb ->
                                    let bstart,bsize = fb in
                                    str := !str ^ (sprintf
                                    "  bstart=%d bsize=%d\n"
                                    bstart bsize);
                                ) disk.bdisk_freeblocks;
                            !str
                        end
                        else
                            "";                                                 
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
            
            | com when com = std_EXIT ->
            begin
                    initial := not (!bd_dying);
                    let rights = prv_rights hdr_req.h_priv in
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (obj = 0 || obj=1) &&
                       (prv_decode ~prv:hdr_req.h_priv 
                                   ~rand:disk.bdisk_chkport) then
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
                Db.Pr.sd 1 "server_dns_loop std_COMBAD" com;
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
                  sys_log Sys_info "BOOTDISK: DNS Server thread exited normally.\n"; 
                  if (!initial) then
                    Sema.sema_up sema

let bd_afs_dying = ref false 

(*
** The same for the AFS module
*)
let server_afs_loop ~disk
                ~sema
                ~nthreads
                ~inbuf_size
                ~outbuf_size
    =
    bd_dying := false;
    let initial = ref false in
    let on_exit cap =
            let stat = if (!bd_afs_dying = false) then
                       begin
                            bd_afs_dying := true;
                            
                            (*
                            ** Tell the other threads we're
                            ** dying.
                            *)
                            for i = 1 to (nthreads-1)
                            do
                                ignore(std_exit cap);     
                                Sema.sema_up sema;
                            done;
                            std_OK
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
        
        let getport = disk.bdisk_file_prvport in
        let putport = priv2pub getport in

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
            ** AFS_SIZE request
            *)
            | com when (com = afs_SIZE) ->
            begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
                    let stat,size = bd_file_size ~disk:disk
                                                 ~priv:priv
                                    in
                    hdr_rep.h_offset <- size;
                    hdr_rep.h_status <- stat;
            end;

            (*
            ** AFS_READ request
            ** Required rights: afs_RGT_READ.
            *)
            | com when (com = afs_READ) ->
            begin
                    let off = hdr_req.h_offset in
                    let size = hdr_req.h_size in
                    
                    let stat,size =
                            if (size > outbuf_size) then
                                    std_ARGBAD,0
                            else
                               bd_file_read ~disk:disk
                                            ~priv:priv
                                            ~buf:obuf
                                            ~off:off
                                            ~size:size
                                      in
                    hdr_rep.h_status <- stat;

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- hdr_req.h_priv;
                        hdr_rep.h_port <- putport;
                        hdr_rep.h_size <- size;
                        replen := size;
                    end;
            end;
            (*
            ** AFS_CREATE request.
            ** Required rights: afs_RGT_CREATE.
            **
            *)
            | com when (com = afs_CREATE) ->
            begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = bd_create ~disk:disk
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
#ifdef DEBUG
                        Db.Pr.ss 1 "afs_CREATE" (ar_cap newcap);
#endif
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;
            end;
            (*
            ** AFS_MODIFY request
            ** Required rights: afs_RGT_MODIFY.
            *)
            | com when (com = afs_MODIFY) ->
            begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = bd_modify ~disk:disk
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~off:off
                                                     ~size:size
                                                     ~commit:flag
                                      in

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;

            end;


            (*
            ** AFS_INSERT request
            ** Required rights: afs_RGT_MODIFY.
            *)
            | com when (com = afs_INSERT) ->
            begin
                    let off = hdr_req.h_offset in
                    let flag = hdr_req.h_extra in

                    let size = hdr_req.h_size in
                    
                    let stat,newcap = bd_insert ~disk:disk
                                                     ~priv:priv
                                                     ~buf:ibuf
                                                     ~off:off
                                                     ~size:size
                                                     ~commit:flag
                                      in
                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;


            end;



            | com when (com = std_INFO) ->
            begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let pos = ref 0 in

                    if (obj > 0) then
                    begin
                        let stat',af = bd_acquire_file ~disk:disk
                                                    ~obj:obj
                        in
    
                        if (stat' = std_OK) then
                        begin
                            (*
                            ** Validate the private field.
                            *)

                            if (prv_decode ~prv:priv 
                                           ~rand:af.boot_obj.prv_random
                                = true) then
                            begin
                                let filesize = if af.boot_file_size > 0
                                                then af.boot_file_size
                                                else
                                                  (af.boot_size*
                                                   disk.bdisk_blksize) in
                                let istr = sprintf "- %8d bytes" filesize
                                    in
                                let pos' = buf_put_string 
                                            ~buf:obuf
                                            ~pos:0
                                            ~str:istr
                                in
                                pos := pos';

                            end
                            else
                                stat := std_DENIED;
                            ignore(bd_release_file ~disk:disk
                                                   ~file:af);
                        end
                        else
                            stat := stat';
                    end
                    else
                    begin
                        (*
                        ** Super Capability.
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv 
                                       ~rand:disk.bdisk_file_chkport
                            = true) then
                        begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"BDISK: Filesystem Server: Super Cap"
                            in
                            pos := pos';
                        end
                        else
                            stat := std_DENIED;
                    end;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
            end;


            | com when (com = std_STATUS) ->
            begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                    let stat = ref std_OK in
                    let pos = ref 0 in

                    if (obj > 0) then
                    begin
                        let stat',af = bd_acquire_file ~disk:disk
                                                    ~obj:obj
                        in
    
                        if (stat' = std_OK) then
                        begin
                            (*
                            ** Validate the private field.
                            *)

                            if (prv_decode ~prv:priv 
                                           ~rand:af.boot_obj.prv_random
                                = true) then
                            begin
                                (*
                                ** Find position in disk_table.
                                *)
                                let fpos = ref 0 in

                                (try 
                                    List.iter (fun file ->
                                        let Objnum obj' = prv_number 
                                                          file.boot_obj in
                                        if obj <> obj' then
                                            incr fpos
                                        else 
                                            raise Exit; 
                                    ) disk.bdisk_table;
                                    with Exit -> ());

                                let istr = sprintf 
                                           "Bootdisk obj=%d order=%d bstart=%d bsize=%d" 
                                           !fpos 
                                           af.boot_order
                                           af.boot_start
                                           af.boot_size    
                                    in
                                let pos' = buf_put_string 
                                            ~buf:obuf
                                            ~pos:0
                                            ~str:istr
                                    in
                                pos := pos';
                            end
                            else
                                stat := std_DENIED;
                            ignore(bd_release_file ~disk:disk
                                                   ~file:af);
                        end
                        else
                            stat := stat';
                    end
                    else
                    begin
                        (*
                        ** Super Capability.
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv 
                                       ~rand:disk.bdisk_file_chkport
                            = true) then
                        begin
                            let pos' = 
                                buf_put_string 
                                    ~buf:obuf
                                    ~pos:0
                                    ~str:"BDISK: Filesystem Server: Super Cap"
                            in
                            pos := pos';
                        end
                        else
                            stat := std_DENIED;
                    end;

                    hdr_rep.h_size <- !pos;
                    hdr_rep.h_status <- !stat;
                    replen := !pos;
            end;


            | com when (com = std_SETPARAMS) ->
            begin
                
                let Objnum obj = prv_number hdr_req.h_priv in
                let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                let stat = ref std_OK in
                let pos = ref 0 in

                if (obj > 0) then
                begin
                    let stat',af = bd_acquire_file ~disk:disk
                                                   ~obj:obj
                        in
    
                    if (stat' = std_OK) then
                    begin
                        (*
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv 
                                      ~rand:af.boot_obj.prv_random
                                = true) then
                        begin
                            (*
                            ** The only parameter of a file: 
                            ** the order number.
                            *)
                            ignore(bd_acquire_dir ~disk:disk ~obj:1);
                            let pl = [{
                                p_name="order";
                                p_val = Param_int af.boot_order;
                                p_unit = "";
                                p_desc = "order in directory";
                                p_set = (fun pv ->
                                    match pv with
                                    | Param_int i -> af.boot_order <- i;
                                    | _ -> ());
                                p_get = (fun () -> Param_int af.boot_order);                                
                                p_min = Param_int 0;
                                p_max = Param_int 21;
                                }] in
                            let paramlen = hdr_req.h_extra in
                            let nparams = hdr_req.h_size in
                            let stat' = stdsrv_params_set 
                                                  ~paramlist:pl
                                                  ~reqbuf:ibuf
                                                  ~paramlen:paramlen
                                                  ~nparams:nparams in       
                            if stat' = std_OK then
                                disk.bdisk_sync <- true;

                            stat := stat';
                            ignore(bd_release_dir ~disk:disk);
                        end
                        else
                            stat := std_DENIED;
                        ignore(bd_release_file ~disk:disk ~file:af);
                    end
                    else
                        stat := std_CAPBAD;
                end
                else
                    stat := std_CAPBAD;
                hdr_rep.h_status <- !stat;
            end;

            | com when (com = std_GETPARAMS) ->
            begin
                let Objnum obj = prv_number hdr_req.h_priv in
                let Rights_bits rights = prv_rights hdr_req.h_priv in
        
                let stat = ref std_OK in
                let pos = ref 0 in

                if (obj > 0) then
                begin
                    let stat',af = bd_acquire_file ~disk:disk
                                                    ~obj:obj
                        in
    
                    if (stat' = std_OK) then
                    begin
                        (*
                        ** Validate the private field.
                        *)

                        if (prv_decode ~prv:priv 
                                      ~rand:af.boot_obj.prv_random
                                = true) then
                        begin
                            (*
                            ** The only parameter of a file: 
                            ** the order number.
                            *)
                            let pl = [{
                                p_name="order";
                                p_val = Param_int af.boot_order;
                                p_unit = "";
                                p_desc = "order in directory";
                                p_set = (fun pv -> ());
                                p_get = (fun () -> Param_int af.boot_order); 
                                p_min = Param_int 0;
                                p_max = Param_int 21;
                                }] in
                            let paramlen = hdr_req.h_extra in
                            let nparams = hdr_req.h_size in
                            let stat',paramlen,nparams = 
                                            stdsrv_params_get 
                                                  ~paramlist:pl
                                                  ~repbuf:obuf in
                            stat := stat';
                            if stat' = std_OK then
                            begin
                                hdr_rep.h_extra <- paramlen;
                                hdr_rep.h_size <- nparams;
                                replen := paramlen;
                            end;
                            ignore(bd_release_file ~disk:disk ~file:af);
                        end
                        else
                            stat := std_DENIED;
                    end
                    else
                        stat := std_CAPBAD;
                end
                else
                    stat := std_CAPBAD;
                hdr_rep.h_status <- !stat;
            end;

            
            | com when com = std_EXIT ->
            begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if obj = 0 &&
                       (portcmp hdr_req.h_priv.prv_random 
                                disk.bdisk_file_chkport) then
                    begin
                        initial := not (!bd_afs_dying);
                        let stat = on_exit { cap_port = hdr_req.h_port;
                                         cap_priv = priv} in

                        hdr_rep.h_status <- stat;
                        ignore( putrep (hdr_rep,obuf,!replen));
                        raise Exit;            
                    end
                    else
                        hdr_rep.h_status <- std_COMBAD;

            end;

            | _ -> 
            begin
                let Command com = hdr_req.h_command in
#ifdef DEBUG
                Db.Pr.sd 1 "server_afs_loop std_COMBAD" com;
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
                  sys_log Sys_info "BOOTDISK: AFS Server thread exited normally.\n"; 
                  if (!initial) then
                    Sema.sema_up sema;

    