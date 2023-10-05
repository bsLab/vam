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
**    $INITIAL:     (C) 2004-2005 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.03
**
**    $INFO:
**
**  Virtual disk server.
**
**    $ENDOFINFO
**
*)




open Amoeba 
open Bytebuf
open Stderr
open Stdcom
open Thread
open Disk_common
open Disk_server
open Disk_server_rpc
open Buf 
open Unix  
open Printf
open Ar    
open Sema   
open Name


let nl = print_newline 
let info str =
    print_string ("VDISK: "^str); nl ()

(*
** Initialize the device 
*)

let init ~blksize ~devpath ~hostpath ~verbose ~geom =
  try
  begin
    info (sprintf "Initializing disk %s ..." devpath);
    let disk_fd = Unix.openfile devpath [O_RDWR] 0 in

    (*
    ** Reads # size of blocks from disk starting at off block.
    *) 
    let read ~first ~size ~buf ~off =
      try
      begin
        if verbose then
            info (sprintf "read: first=%d size=%d boff=%d"
                          first size off);

        let off' = first*blksize in
        let size' = size*blksize in
        let n = lseek disk_fd off' SEEK_SET in
        if n <> off' then
            raise (Error std_IOERR);
        let off = ref off in
        let sizen = ref size' in
        while !sizen > 0 
        do
            let n = readb disk_fd buf !off !sizen in
            if n < 0 then
                raise (Error std_IOERR);
            sizen := !sizen - n;
            off := !off + n;
        done;

        std_OK
      end
        with | Error err -> err
             | _ -> std_IOERR
        in

    (*
    ** Write # size of blocks to disk starting at off block.
    *) 
    let write ~first ~size ~buf ~off =
      try
      begin
        if verbose then
            info (sprintf "write: first=%d size=%d boff=%d"
                          first size off);

        let off' = first*blksize in
        let size' = size*blksize in
        let n = lseek disk_fd off' SEEK_SET in
        if n <> off' then
            raise (Error std_IOERR);

        let off = ref off in
        let sizen = ref size' in
        while !sizen > 0 
        do
            let n = writeb disk_fd buf !off !sizen in
            if n < 0 then
                raise (Error std_IOERR);
            sizen := !sizen - n;
            off := !off + n;
        done;



        std_OK
      end
        with | Error err -> err
             | _ -> std_IOERR
        in
    let dev = {
            dev_host = hostpath;
            dev_path = devpath;
            dev_blksize = blksize;
            dev_read = read;
            dev_write = write;
            dev_labels = [];
            dev_geom = geom;
        } in
    let disks = {
            pdisks = [];
            vdisks = [];
            ldisks = [] } in

    let stat = vdisk_init ~dev:dev ~disks:disks in
    info (sprintf "disk_init: %s" (err_why stat));

    if stat <> std_OK then exit 0;

    dev,disk_fd,disks    
  end
  with
    | Failure str -> failwith str
    | _ -> failwith "Disk initialization failed: UNIX error"


let start ~blksize ~nthr ~hostpath ~disk_table ~verbose =

        let vt = disk_table in
        (*
        ** Publish disk caps
        *)

        info (sprintf "Publishing disk caps in %s..." hostpath);
        let stat = vdisk_publish vt hostpath in
        if stat <> std_OK then
        begin
            info (sprintf "Failed to publish disk caps: %s." 
                    (err_why stat));
            exit 1;
        end;

        (*
        ** Start the server threads...
        *)
        let sem = sema_create 0 in
        info (sprintf "Starting %d server threads..." nthr);
        for i = 1 to nthr
        do
            ignore (thread_create (fun () -> server_loop vt
                                                     sem
                                                     nthr
                                                     disk_REQBUFSZ
                                                     disk_REQBUFSZ
                                                     blksize) ());
                                            
        done;
        (*
        ** Wait for server threads untill all are exited...
        *)
        for i = 1 to nthr
        do
            sema_down sem;
        done


let stop ~hostpath ~disk_table =
        let stat = vdisk_remove disk_table hostpath in
        if stat <> std_OK then
        begin
            info (sprintf "Failed to remove disk caps: %s." 
                    (err_why stat));
            exit 1;
        end;
    