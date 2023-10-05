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
**    $CREATED:     24.4.2005
**    $VERSION:     1.03
**
**    $INFO:
**
**  An AFS file server which maps UNIX files to AFS objects. 
**  Emulates AFS interface for UNIX files with AFS_READ and AFS_SIZE
**  operations. AFS_WRITE requests are only handled with in memory
**  objects.
**  Primary used for Amoeba process execution.
**
**    $ENDOFINFO
**
*)

open Amoeba
open Unix
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
open Printf
open Ar
open Name
open Syslog

open Afs_common
open Afs_server


(*
** Minimal AFS server loop
*)

(*
** One UNIX file or memory handled object (for example a stack segment
** of a starting process)
*)

type afu_type = 
    | Unix_file
    | Memory_file
    | Not_used

type afu_file = {
    mutable afu_obj: int;
    mutable afu_type: afu_type;         (* File type                *)
    mutable afu_path: string;           (* Unix path *)
    mutable afu_size: int;              (* Current size of file     *)
    mutable afu_fd: Unix.file_descr;    (* Either                   *)
    mutable afu_buf: Bytebuf.buffer;    (* or depending on afu_type *)
    mutable afu_rand: port;             (* security port            *)
}

let nilafu = {
    afu_obj = 0;
    afu_type = Not_used;
    afu_path = "";
    afu_size = 0;
    afu_fd = nilfd;
    afu_buf = nilbuf;
    afu_rand = nilport;
}

type afu_server = {
    mutable afu_buf_size: int;      (* Request/Reply buffer size *)

    mutable afu_getport: port;      (* Private server port *)
    mutable afu_putport: port;      (* Public server port  *)

    mutable afu_verbose: int;       (* Verbose level *)
    mutable afu_dying: bool;        (* Shutdown ? *)


    mutable afu_max_files: int;     (* Maximal number of serviced files *)
    mutable afu_files: afu_file array;  (* All the files ... *)
    mutable afu_next_obj: int;

    mutable afu_lock: Mutex.t;
}

let nilafuserver = {
        afu_buf_size=0;
        afu_getport=nilport;
        afu_putport=nilport;
        afu_verbose=0;
        afu_dying=false;
        afu_max_files=0;
        afu_files=[||];
        afu_next_obj=0;
        afu_lock=mu_create ();
}

let info str =
    sys_log Sys_info "AFU: %s\n" str
 
let afu_server_loop ~server
    =
    let initial = ref false in
    begin
        let ibuf = buf_create server.afu_buf_size in    (* request buffer *)
        let obuf = buf_create server.afu_buf_size in    (* reply buffer   *)
        
        let hdr_rep = header_new () in
        let replen = ref 0 in

        if (server.afu_verbose > 1) then
        begin
            info ("afs_srv started. port "^(ar_port server.afu_getport)); 
        end;
        
        while (server.afu_dying = false)
        do
        try
          begin

            let stat,reqlen,hdr_req = getreq (server.afu_getport,
                                              ibuf,
                                              server.afu_buf_size) in

            mu_lock server.afu_lock;

            replen := 0;
            hdr_rep.h_size <- 0;
            hdr_rep.h_status <- std_OK;
            hdr_rep.h_priv <- priv_copy nilpriv;


            let priv =hdr_req.h_priv in
            (

                if (server.afu_verbose > 2) then
                begin
                    info ("request "^(ar_priv hdr_req.h_priv)^
                          " command: "^(let Command com = hdr_req.h_command 
                                        in string_of_int com));
                end;

                match hdr_req.h_command with

    
                (*
                ** AFS_SIZE request
                *)
                | com when (com = afs_SIZE) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in

                    let stat,size =
                        if (obj > 0 && obj <= server.afu_max_files) then
                        begin
                            let stat,size = std_OK,
                                            server.afu_files.(obj-1).afu_size in

                            if (server.afu_verbose > 2) then
                            begin
                               info ("afs_SIZE:  obj size "^
                                        (string_of_int size));
                            end;
                            stat,size
                        end
                        else
                            std_OBJBAD,0 in

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
                    let size = min hdr_req.h_size server.afu_buf_size in
                    
                    let Objnum obj = prv_number hdr_req.h_priv in

                    if (server.afu_verbose > 2) then
                    begin
                        info ("afs_READ:  obj read "^
                             (string_of_int obj)^
                             " off="^(string_of_int off)^
                             " size="^(string_of_int size)); 
                    end;

                    let stat,size' =
                        match obj with
                        | obj when (obj > 0 && 
                                    obj <= server.afu_max_files &&
                          server.afu_files.(obj-1).afu_type = Unix_file) -> 

                            let fd = server.afu_files.(obj-1).afu_fd in
                            ignore(Unix.lseek 
                                        fd off SEEK_SET);
                            
                            (*
                            ** Read size bytes from UNIX file, possible
                            ** fragmented. 
                            *)
                            let curpos = ref 0 in
                            let cursize = ref 0 in
                            let remainsize = ref size in
                            let bufread () =
                                while (!curpos < size &&
                                       (!cursize > 0 || !curpos = 0))
                                do
                                    cursize := Unix.readb 
                                                    fd
                                                    obuf 
                                                    !curpos 
                                                    !remainsize;
                                    if (!cursize > 0) then
                                    begin
                                        curpos := !curpos + !cursize;                                    
                                        remainsize := !remainsize - 
                                                       !cursize;
                                    end;
                                    
                                done
                                in
                            bufread ();

                            let size' = !curpos in
                            if (size' >= 0) then 
                                std_OK,size'
                            else 
                                std_IOERR,0
                        | obj when (obj > 0 && 
                                    obj <= server.afu_max_files &&
                          server.afu_files.(obj-1).afu_type = Memory_file) -> 
                            (*
                            ** Temporary in core file object
                            *)
                            let buf = server.afu_files.(obj-1).afu_buf in
                            if (buf <> nilbuf) then
                            begin
                                blit_bb ~src:buf
                                        ~src_pos:off
                                        ~dst:obuf
                                        ~dst_pos:0
                                        ~len:size;
                                std_OK,size;
                            end
                            else
                                std_CAPBAD,0
                        | _ -> std_OBJBAD,0;
                        in

                    hdr_rep.h_status <- stat;

                    if (stat = std_OK) then
                    begin
                        hdr_rep.h_priv <- hdr_req.h_priv;
                        hdr_rep.h_port <- server.afu_putport;
                        hdr_rep.h_size <- size';
                        replen := size';
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

                    if (server.afu_verbose > 2) then
                    begin
                        info ("afs_CREATE: create obj of size "^
                                (string_of_int size)^
                              " off="^(string_of_int off));
                    end;

                    (*
                    ** Create a new temporarily file object only
                    ** handled in memory.
                    *)

                    let next = server.afu_next_obj in
                    if next < server.afu_max_files then
                    begin
                        server.afu_next_obj <- next + 1;
                        
                        let new_afu = {
                                afu_obj = next;
                                afu_type = Memory_file;
                                afu_path = "";
                                afu_size = size;
                                afu_fd = nilfd;
                                afu_buf = buf_create size;
                                afu_rand = uniqport ();
                            } in
                        server.afu_files.(next-1) <- new_afu;

                        blit_bb ~src:ibuf
                                ~src_pos:0
                                ~dst:new_afu.afu_buf
                                ~dst_pos:off
                                ~len:size;

                        hdr_rep.h_priv <- prv_encode ~obj:(Objnum next)
                                                     ~rights:prv_all_rights
                                                     ~rand:new_afu.afu_rand;
                        hdr_rep.h_port <- server.afu_putport;
                        hdr_rep.h_status <- stat;
                    end
                        else hdr_rep.h_status <- std_EXISTS;
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
                    let Objnum obj = prv_number hdr_req.h_priv in
                    
                    let oldcap = {cap_port = hdr_req.h_port;
                                  cap_priv = hdr_req.h_priv } in

                    let stat,newcap = 
                        if (obj > 0  && 
                            obj < server.afu_max_files &&
                            server.afu_files.(obj-1).afu_type = Memory_file)
                            then  std_OK,oldcap 
                            else  std_CAPBAD,nilcap in

                    if (server.afu_verbose > 2) then
                    begin
                        info ("afs_MODIFY: obj "^
                                (string_of_int obj)^
                             " size="^(string_of_int size)^
                             " off="^(string_of_int off));
                    end;

                    if (stat = std_OK) then
                    begin
                        let buf = server.afu_files.(obj-1).afu_buf in
                        let blen = buf_len buf in
                        if (blen < (off+size)) then
                        begin
                            (*
                            ** OBJ2 buffer must be extended.
                            *)
                            let blen' = blen * 2 in
                            let oldbuf = buf in
                            let newbuf = buf_create blen' in
                            server.afu_files.(obj-1).afu_buf <- newbuf;
                            server.afu_files.(obj-1).afu_size <- off+size;
                            blit_bb ~src:oldbuf
                                    ~src_pos:0
                                    ~dst:newbuf
                                    ~dst_pos:0
                                    ~len:blen;
                        end;
                        (*
                        ** Buffer can be  changed...
                        *)
                        let buf = server.afu_files.(obj-1).afu_buf in
                        blit_bb ~src:ibuf
                                ~src_pos:0
                                ~dst:buf
                                ~dst_pos:off
                                ~len:size;
                                
                        hdr_rep.h_priv <- newcap.cap_priv;
                        hdr_rep.h_port <- newcap.cap_port;
                    end;
                    hdr_rep.h_status <- stat;

                end;

                | com when (com = std_DESTROY) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    if (server.afu_verbose > 2) then
                    begin
                        info ("std_DESTROY:  obj "^
                                (string_of_int obj)); 
                    end;
                    if (obj > 0 && obj < server.afu_max_files &&
                        server.afu_files.(obj-1).afu_type = Memory_file) then
                    begin
                        server.afu_files.(obj-1) <- nilafu;
                        hdr_rep.h_status <- std_OK;                    
                    end
                    else
                        hdr_rep.h_status <- std_OBJBAD;
                end;

                | com when (com = std_INFO) ->
                begin
                    let Objnum obj = prv_number hdr_req.h_priv in
                    let Rights_bits rights = prv_rights hdr_req.h_priv in

                    let stat,size =
                        if (obj > 0 && obj <= server.afu_max_files) then
                        begin
                            let stat,size = 
                                std_OK, server.afu_files.(obj-1).afu_size in
                            if stat = std_OK then
                            begin
                                let str = sprintf "- %8d" size in
                                let pos = buf_put_string obuf 0 str in
                                if (server.afu_verbose > 2) then
                                begin
                                    info (sprintf "std_INFO:  obj=%d size=%d"
                                            obj size);
                                end;
                                
                                std_OK,pos;
                            end
                                else stat,0;
                        end
                        else
                            std_OBJBAD,0 in
                   
                    hdr_rep.h_size <- size;
                    replen := size;
                    hdr_rep.h_status <- stat;
                end;
                | com when com = std_EXIT -> 
                begin
                    server.afu_dying <- true;
                    hdr_rep.h_status <- std_OK
                end;

                | _ -> 
                begin
                    let Command com = hdr_req.h_command in
                    hdr_rep.h_status <- std_COMBAD;
                end;

            );

            if (server.afu_verbose > 2) then
                info (sprintf "reply %s" (err_why hdr_rep.h_status));
            
            mu_unlock server.afu_lock;

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
    
        sys_log Sys_info "AFU: AFU Server thread exited normally.\n"; 
    end

(*
** Open and map UNIX file
*)
let afu_open_file ~server ~filename =
  try
  begin
    if server.afu_verbose > 1 then
        info ("afu_open_file "^filename);

    mu_lock server.afu_lock;

    let next = server.afu_next_obj in
    if next < server.afu_max_files then
    begin
       let new_afu = {
            afu_obj = next;
            afu_type = Unix_file;
            afu_path = filename;
            afu_size = 0;
            afu_fd = Unix.openfile filename [O_RDONLY] 0;
            afu_buf = nilbuf;
            afu_rand = uniqport ();
            } in
    
        let stats = Unix.stat filename in
        new_afu.afu_size <- stats.st_size;
        server.afu_next_obj <- next + 1;
        server.afu_files.(next-1) <- new_afu;
        mu_unlock server.afu_lock;
        std_OK,new_afu
    end
    else
    begin
        if server.afu_verbose > 1 then
            info ("afu_open_file: file table full");
        mu_unlock server.afu_lock;
        std_NOSPACE,nilafu
    end;
  end
    with _ -> if server.afu_verbose > 1 then info "afu_open_file: exception"; 
              mu_unlock server.afu_lock;
              std_IOERR,nilafu

(*
** Close UNIX file
*)

let afu_close_file ~server ~afu_obj =
    if (afu_obj.afu_fd != nilfd) then
        Unix.close afu_obj.afu_fd

let afu_close_all_files ~server =
    for i = 1 to server.afu_max_files
    do
        afu_close_file server server.afu_files.(i-1);
    done

(*
** Publish file cap of handled UNIX file in the Amoeba filesystem
*)
let afu_publ_file ~server ~afu_obj ~path =
  try
  begin
    let objnum = afu_obj.afu_obj in

    if server.afu_verbose > 1 then
        info (sprintf "afu_publ_file obj=%d path=%s" objnum path);

    let stat,_ = name_lookup path in
    if stat = std_OK then
    begin
        let stat = name_delete path in
        if stat <> std_OK then
        begin
            if server.afu_verbose > 0 
                then info "afu_publ_file: name_delete failed.";
            raise (Error stat);
        end;
    end;
    (*
    ** generate file capability
    *)
    let cap = {cap_port = server.afu_putport;
               cap_priv = prv_encode ~obj:(Objnum objnum)
                            ~rights:prv_all_rights
                            ~rand:afu_obj.afu_rand;
               } in     
    let stat = name_append path cap in
    if stat <> std_OK then
    begin
        if server.afu_verbose > 0 
            then info "afu_publ_file: name_append failed.";
        raise (Error stat)
    end;
    std_OK;
  end
  with Error stat -> stat

let afu_unpubl_file ~server ~path =
    name_delete path


let afu_file_cap ~server ~afu_obj =
    {cap_port = server.afu_putport;
     cap_priv = prv_encode ~obj:(Objnum afu_obj.afu_obj)
                           ~rights:prv_all_rights
                           ~rand:afu_obj.afu_rand}

let afu_init ~buf_size ~max_files  =
    let server = 
        {
            afu_buf_size = buf_size;
            afu_getport = uniqport ();
            afu_putport = nilport;
            afu_verbose = 0;
            afu_dying = false;
            afu_max_files = max_files;
            afu_files = Array.create max_files nilafu;
            afu_next_obj = 1;
            afu_lock = mu_create ();
        } in
    server.afu_putport <- priv2pub server.afu_getport;
    server

let afu_stop ~server =
    let cap = afu_file_cap server server.afu_files.(0) in
    let stat = std_exit cap in
    stat
