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
**    $VERSION:     1.02
**
**    $INFO:
**
** THE STATE OF THE ART
**
**
** NB: Object = File
**
** This module provides a generic fixed size filesystem cache.
**
** The idea behind this module:
**
** 1. The file cache consists of several fixed size buffers, commonly
**     a multiple of the disk block size.
**
** 2. Parts of a file (this can be the real file data or an inode block) 
**    or the whole file is cached.
**
** 3. Objects are identified by their unique object number (inode number).
**
** 4. It's assumed that files are stored continguesly on disk. That
**    means: logical offset <=> disk address * block_size + offset. 
**
** 5. Offsets start with number 0. Physical addresses in blocks.
**
** 6. All buffers from a cache object are stored in a
**    list sorted with increasing offset.
**
** All the single buffers are allocated on startup with the
** cache_create function.
** 
** On a request, first the cache must be lookuped for
** an already cached file object. If there is no such object,
** a new cache entry is created. Therefore, the cache_read
** and cache_write functions must always provide
** the logical disk offset (when file offset = 0), and the
** current state of the file. 
**
** If a read request (obj,off,size) arrives, 
** the desired fragment (off,size) is read into the cache, but the size is
** enlarged to the cache buffer size and the offset
** is adjusted modulo to the buffer size. The requested
** fragment is copied in the user target buffer.
**
** If the block file offset and size is already available in the cache, 
** only copy the desired data to the buffer.
**
** If there are already parts of the request cached, only the
** missing parts are read into the cache.
**
** Additionaly, there is an inode cache. The same behaviour.
**
**
** Basic File Parameters and units:
**
**      File Sizes:     Logical, in bytes
**      Disk addresses: Physical, in blocks [super.afs_bock_size]  
**      File offsets:   Logical, in bytes  
**
**      Free/Used
**      Clusters:       Physical (both addr and size!), in blocks
**
**      A File always occupy full blocks.
**
**
**    $ENDINFO
*)

(*
** TODO: put allocated buffers back to free list if 
**       disk_read/write failed.
*)

open Amoeba
open Bytebuf
open Thread
open Stderr
open Afs_common
open Afs_server
open Syslog

open String
module String = StringLabels


let cache_MAXLIVE = 8

(*
** The Cache mode.
**
** Write through cache: All cache_write request are written immediately
**                      to disk (and cached)
**
*)

type afs_cache_mode = 
    | Cache_R           (* Write through cache          *)
    | Cache_RW          (* Lazy Read and Write Caching  *)

type afs_cache_state =
    | Cache_Empty           (* Empty cache buffer                       *)
    | Cache_Sync            (* The cache buffer is synced with disk     *)
    | Cache_Modified        (* The cache buffer must be written to disk *)

(*
** One buffer.
*)

type fsc_buf = {

    (*
    ** The index of this buffer
    *)
    mutable fsb_index: int;


    (*
    ** The buffer
    *)
    mutable fsb_buf: buffer;
    
    (*
    ** Logical file offset [bytes]
    *)
    mutable fsb_off: int;

    (*
    ** Amount of cached data in this buffer [bytes]
    *)
    mutable fsb_size: int;

    (*
    ** State of the buffer
    *)

    mutable fsb_state: afs_cache_state;

    (*
    ** Buffer lock
    *)

    mutable fsb_lock: Mutex.t;
}

let nilfsc = { fsb_index = -1; fsb_buf = nilbuf; fsb_off = -1; 
               fsb_size = -1; fsb_state = Cache_Empty; 
               fsb_lock = mu_create ()}

(*
** One object cached by this module.
*)

type fsc_entry = {
    (*
    ** Object number. Must be unique!
    *)
    mutable fse_objnum: int;

    (*
    ** Physical disk address of this cached object. 
    **
    **  Address: blocks
    **  Size: bytes
    *)

    mutable fse_disk_addr: int;
    mutable fse_disk_size: int;
    

    (*
    ** List of all cached buffers. 
    *)
    mutable fse_cached: int list;

    (*
    ** The last cached buffer. Speeds up the cache lookup
    ** on sequential reads or writes.
    *)

    mutable fse_lastbuf: int;

    (*
    ** List of all  buffers written to disk before the
    ** file was committed. On further modify request, these
    ** blocks must be reread into the cache!
    ** 
    **  (logical offset * size) list
    *) 

    mutable fse_written: (int * int) list;
    
    (*
    ** State of the file.
    *)
    mutable fse_state: afs_file_state;

    (*
    ** Live time
    *)

    mutable fse_live: int;

    (*
    ** Lock
    *)

    mutable fse_lock: Mutex.t;
}

let nilfse = { fse_objnum=0; fse_disk_addr = -1; fse_disk_size = -1 ;
               fse_cached = []; fse_lastbuf = -1; fse_written = [];
               fse_state = FF_invalid; fse_live=0;
               fse_lock = mu_create ()} 


(*
** Cache statistics
*)

type fsc_stat = {
    mutable cs_cache_read: int;
    mutable cs_cache_write: int;
    mutable cs_cache_commit: int;
    mutable cs_cache_delete: int;
    mutable cs_cache_compact: int;
    mutable cs_cache_timeout: int;
        
    mutable cs_cache_age: int;
    
    mutable cs_disk_read: int;
    mutable cs_disk_write: int;

    mutable cs_cache_hit: int;
    mutable cs_cache_miss: int;
}

type fsc_cache = {
    (*
    ** Number of buffers in the cache.
    *)

    mutable fsc_size: int;

    (*
    ** Blocksize in bytes
    *)

    mutable fsc_block_size: int;

    (*
    ** Size of one buffer in bytes. Fixed! Commonly multiple of the
    ** disk block size.
    *)

    mutable fsc_buf_size: int;


    (*
    ** The buffer array
    *)
    mutable fsc_buffers: fsc_buf array;

    (*
    ** List of all free buffers in the cache.
    *)

    mutable fsc_free_bufs: int list;  

    (*
    ** All informations of currently cached objects are stored in a
    ** hash table. The key is the physical disk address [blocks]!
    *)

    mutable fsc_table: (int , fsc_entry)  Hashtbl.t;

    (*
    ** User supplied disk read function.
    ** Args:
    **  addr: Physical disk address [blocks]
    **  data: The buffer to read from
    **  size: The desired data size to read
    **
    ** Return:
    **  status
    *)

    mutable fsc_read:          obj:int ->
                               addr:int ->
                               data: buffer ->
                               size: int ->
                               status; 

    (*
    ** User supplied disk write function.
    ** Args:
    **  addr: Physical disk address [blocks]
    **  data: The buffer to write to
    **  size: The data size to write
    **
    ** Return:
    **  status
    *)

    mutable fsc_write:      obj:int ->
                            addr:int ->
                            data: buffer ->
                            size: int ->
                            status;


    (*
    ** Notify the server about a synced file.
    *)

    mutable fsc_synced: obj:int -> unit;

    mutable fsc_lock: Mutex.t;
    
    mutable fsc_mode: afs_cache_mode;

    mutable fsc_stat: fsc_stat;
}


(*
** Util functions
*)

module Stat =
    struct
    let rd cache =
        cache.fsc_stat.cs_cache_read <-
            cache.fsc_stat.cs_cache_read + 1
    let wr cache =
        cache.fsc_stat.cs_cache_write <-
            cache.fsc_stat.cs_cache_write + 1
    let comm cache =
        cache.fsc_stat.cs_cache_commit <-
            cache.fsc_stat.cs_cache_commit + 1
    let del cache =
        cache.fsc_stat.cs_cache_delete <-
            cache.fsc_stat.cs_cache_delete + 1
    let comp cache =
        cache.fsc_stat.cs_cache_compact <-
            cache.fsc_stat.cs_cache_compact + 1
    let time cache =
        cache.fsc_stat.cs_cache_timeout <-
            cache.fsc_stat.cs_cache_timeout + 1
    let age cache =
        cache.fsc_stat.cs_cache_age <-
            cache.fsc_stat.cs_cache_age + 1
    let dird cache =
        cache.fsc_stat.cs_disk_read <-
            cache.fsc_stat.cs_disk_read + 1
    let diwr cache =
        cache.fsc_stat.cs_disk_write <-
            cache.fsc_stat.cs_disk_write + 1
    let hit cache =
        cache.fsc_stat.cs_cache_hit <-
            cache.fsc_stat.cs_cache_hit + 1
    let miss cache =
        cache.fsc_stat.cs_cache_miss <-
            cache.fsc_stat.cs_cache_miss + 1
end 

(*
** Cache compaction
**
** Assumption:
**  Unlocked files are mainly written sequential to the cache.
**
** Remove objects with livetime = 0 and transfer their buffers to the 
** freelist.  If there are still not enough free buffers, 
** decrease the buffer list of the uncommitted (FF_locked) and
** committed (FF_unlocked) objects.
**
** The code is slightly blurred with cache entry and buffer locking:
**
**  1. Only unlocked cache entries can be removed from the cache.
**  2. Only unlocked buffers can be transfered to the free list.
**
** Remember I:  the case only one object uses the entire cache! 
** Remember II: cache_compact must be called with a locked fsc_lock!
**
**
*)

let cache_compact ~cache =
    let clear_buf buf = 
        buf.fsb_off  <- -1;
        buf.fsb_size <- -1;
        buf.fsb_state <- Cache_Empty;
    in
    let to_block x =
        x/cache.fsc_block_size
    in
    let block x = let block_size = cache.fsc_block_size in
                  ((x+block_size-1)/block_size)*block_size 
    in
    
    Stat.comp cache;

    (*
    ** Plan A: Remove  objects with live time = 0.
    ** Plan B: Steal committed files some buffers.
    ** Plan C: Write buffers from an uncommitted file to disk. 
    *)

    let dying  = ref [] in
    let comm   = ref [] in
    let notcom1 = ref [] in
    let notcom2 = ref [] in

    Hashtbl.iter (fun key fe ->
            if (fe.fse_live = 0) then
            begin
                (*
                ** Kick out this object from the cache.
                *)
                dying := !dying @ [fe];
            end
            else if (fe.fse_state = FF_locked) then
            begin
                if (fe.fse_cached <> []) then
                    comm := !comm @ [fe];    
            end
            else if (fe.fse_state = FF_unlocked) then
            begin
                (*
                ** A not committed cache entry. Only used,
                ** if there are no committed cache entries available.
                *)
                if (fe.fse_cached <> []) then
                    notcom2 := !notcom2 @ [fe];    
            end
            else if (fe.fse_state = FF_commit) then
            begin
                (*
                ** A not committed but finished cache entry. Only used,
                ** if there are no committed cache entries available.
                *)
                if (fe.fse_cached <> []) then
                    notcom1 := !notcom1 @ [fe];    
            end;
            
            
        ) cache.fsc_table;
    
    let notcom = ref (!notcom1 @ !notcom2) in
    (*
    ** Remove first the dead ones (Live time = 0).
    *)

    if (!dying <> []) then
    begin
#ifdef DEBUG
        Db.Pr.ss 10 "cache_compact" "Plan A";
#endif

        List.iter (fun fe ->
                (*
                ** Only remove cache entries for those we get the lock!
                *)

                if (mu_trylock fe.fse_lock = true) then
                begin
                    List.iter (fun fi ->
                        let fb = cache.fsc_buffers.(fi) in

                        mu_lock fb.fsb_lock;
                        clear_buf fb;
                        mu_unlock fb.fsb_lock;

                    ) fe.fse_cached;
                    cache.fsc_free_bufs <- cache.fsc_free_bufs @ fe.fse_cached;
                    Hashtbl.remove cache.fsc_table fe.fse_objnum;
                    mu_unlock fe.fse_lock;
                end;
            ) !dying;
    end;

    let newbufs = ref ( List.length cache.fsc_free_bufs) in

    (*
    ** Try to get at least n/4 free buffers from the cache
    *)
    let newneeded = cache.fsc_size / 4 in

    if (!newbufs < newneeded && !notcom <> []) then 
    begin
#ifdef DEBUG
        Db.Pr.ss 10 "cache_compact" "Plan B";
#endif
        (*
        ** Plan B:
        ** Iterate the list of uncommitted files and
        ** steal them buffers. Use the oldest ones first!
        ** We must first commit the buffers to disk.
        *)

        let notcomli = Sort.list (fun f1 f2 ->
                          f1.fse_live < f2.fse_live
                       ) !notcom
        in

        List.iter ( fun fe ->        
        if (!newbufs < newneeded) then
        begin
            let nbf = 
                min  ((List.length fe.fse_cached)/2)
                     (newneeded - !newbufs) in

            (*
            ** Try to get at least n/2 buffers
            *)

            for i = 0 to nbf
            do
                let newbuf = match fe.fse_cached with
                        | hd::tl -> 
                        begin
                            let fb = cache.fsc_buffers.(hd) in

                            (*
                            ** Only remove blocks we get the lock for.
                            *)

                            if (mu_trylock fb.fsb_lock = true) then
                            begin

                              if (fb.fsb_state = Cache_Modified) then
                              begin
                                (*
                                ** First write the data to disk!
                                *)

                                Stat.diwr cache;
    
                                let stat =
                                cache.fsc_write 
                                        ~obj:fe.fse_objnum
                                        ~addr:((to_block fb.fsb_off)+
                                                fe.fse_disk_addr)
                                        ~data:fb.fsb_buf
                                        ~size:(block fb.fsb_size)
                                in                                        
                                if (stat = std_OK) then
                                begin
                                    fe.fse_written <- fe.fse_written @
                                                    [fb.fsb_off,
                                                     (block fb.fsb_size)];

                                    fe.fse_cached <- tl;
                                    clear_buf fb;
                                    incr newbufs;
                                    mu_unlock fb.fsb_lock;
                                    [hd]
                                end
                                else
                                begin
                                    mu_unlock fb.fsb_lock;
                                    []
                                end;
                              end
                              else
                              begin
                                fe.fse_cached <- tl;
                                clear_buf fb;
                                incr newbufs;
                                mu_unlock fb.fsb_lock;
                                [hd]
                              end;
                            end
                            else    (* still in use ? *)
                                [];
                        end;
                        | [] -> []
                in
                cache.fsc_free_bufs <- cache.fsc_free_bufs @ newbuf;
            done;
        end; 
        ) notcomli;
    end;
    
    if (!newbufs < newneeded && !comm <> []) then 
    begin
#ifdef DEBUG
        Db.Pr.ss 10 "cache_compact" "Plan C";
#endif
        (*
        ** Plan C:
        ** Iterate the list of committed files and
        ** steal them buffers. Use the oldest ones first!
        *)

        let comli = Sort.list (fun f1 f2 ->
                        f1.fse_live < f2.fse_live
                    ) !comm
        in

        List.iter ( fun fe ->        
        if (!newbufs < newneeded) then
        begin
            let nbf = 
                min ((List.length fe.fse_cached)/2) 
                     (newneeded - !newbufs) in

            (*
            ** Try to get at least n/2 buffers
            *)

            for i = 0 to nbf
            do
                let newbuf = match fe.fse_cached with
                             | hd::tl -> 
                             begin
                                let fb = cache.fsc_buffers.(hd) in

                                (*
                                ** Only remove blocks we get the lock for.
                                *)

                                if (mu_trylock fb.fsb_lock = true) then 
                                begin
                                    fe.fse_cached <- tl;
                                    incr newbufs;
                                    clear_buf fb;
                                    mu_unlock fb.fsb_lock;
                                    [hd]
                                end
                                else    (* still in use ? *)
                                    []
                             end;
                             | [] -> []
                in
                cache.fsc_free_bufs <- cache.fsc_free_bufs @ newbuf;
            done;
        end; 
        ) comli;
    end

#ifdef DEBUG
    ;
    Db.Pr.sd 10 "cache_compact: new" (!newbufs)    
#endif
    
    
    


(*
** Get a new buffer from the cache. If there are no free buffers,
** the cache_compact function must be called. 
*)

let get_buf ~cache =

#ifdef DEBUG
  Db.Pr.ss 15 "afs_cache: get_buf" "start";
#endif

  mu_lock cache.fsc_lock;
  match cache.fsc_free_bufs with
    | hd::tl ->
    begin
        (*
        ** We have luck.
        *)
        cache.fsc_free_bufs <- tl;
#ifdef DEBUG
        Db.Pr.ss 15 "afs_cache: get_buf" "end";
#endif
        mu_unlock cache.fsc_lock;
        std_OK,hd
    end
    | [] ->
    begin
        (*
        ** We must use  already used buffers. A little bit dirty.
        ** Use compact_cache to get free buffers.
        *)

        cache_compact cache;

        match cache.fsc_free_bufs with
        | hd::tl ->
        begin
            (*
            ** We have finally luck.
            *)
            cache.fsc_free_bufs <- tl;
#ifdef DEBUG
            Db.Pr.ss 15 "afs_cache: get_buf" "end";
#endif
            mu_unlock cache.fsc_lock;
            std_OK,hd
        end
        | [] ->
        begin
            (*
            ** No free buffers anymore. Fatal.
            *)
#ifdef DEBUG
            Db.Pr.ss 15 "afs_cache: get_buf" "failed";
#endif
            mu_unlock cache.fsc_lock;
            std_NOSPACE,-1
        end;
    end

(*
** Create a new cache.
** Args:
**  nbufs: Number of fixed size buffers.
**  blocksize: The filesystem blocksize [bytes].
**  bufsize: The fixed size of one buffer in the cache [blocks].
**  read: The user supplied disk read function. See above.
**  write: The user supplied disk write function. See above.
**
** Return:
**
**  status
**  cache
**
** The server must create one cache for the file data, and one
** cache for the inode blocks.
*)


let cache_create ~nbufs
                 ~blocksize
                 ~bufsize 
                 ~read
                 ~write
                 ~sync
                 ~mode
    =
    let rec freelist n =
        match n with
        | 0 -> [0];
        | _ -> (freelist (n-1)) @ [n];
    in  
    std_OK,
    {
        fsc_size = nbufs;
        fsc_block_size = blocksize;
        fsc_buf_size = bufsize*blocksize;
        fsc_buffers = Array.init nbufs (fun i -> 
                            {
                                fsb_index = i;
                                fsb_buf = buf_create (blocksize*bufsize);
                                fsb_off = -1;
                                fsb_size = -1;
                                fsb_state = Cache_Empty;
                                fsb_lock = mu_create ();
                            });
        fsc_free_bufs = freelist (nbufs-1);
        fsc_table = Hashtbl.create 20;
        fsc_read = read;
        fsc_write = write;
        fsc_synced = sync;
        fsc_lock = mu_create ();
        fsc_mode = mode;
        fsc_stat = {
            cs_cache_read = 0;
            cs_cache_write = 0;
            cs_cache_commit = 0;
            cs_cache_delete = 0;
            cs_cache_compact = 0;
            cs_cache_timeout = 0;
            cs_cache_age = 0;
            cs_disk_read = 0;
            cs_disk_write = 0;
            cs_cache_hit = 0;
            cs_cache_miss = 0;
        };
    }  





(*
** Lookup the cache for a file object. If there is no such object
** already cached, create a new one, else return the fsc entry.
** The object is returned with a locked mutex. Additionaly the
** success of the Hashtbl lookup is returned.
*)

let cache_lookup ~cache 
                 ~obj
                 ~addr
                 ~size
                 ~state
    =
    mu_lock cache.fsc_lock;
    let found = ref true in
    let file = try
                    Hashtbl.find cache.fsc_table obj 
               with
               | Not_found -> 
               begin
                    found := false;
                    let ne =
                    {
                        fse_objnum = obj;
                        fse_disk_addr = addr;
                        fse_disk_size = size;
                        fse_cached = [];
                        fse_lastbuf = -1;
                        fse_written = [];
                        fse_live = cache_MAXLIVE;
                        fse_state = state;
                        fse_lock = mu_create ();
                    } in
                    Hashtbl.add cache.fsc_table obj ne;
                    ne
               end
    in
    mu_lock file.fse_lock;
    mu_unlock cache.fsc_lock;
    !found,file    


(*
** If the work is done, this function must be called.
*)

let cache_release ~cache 
                  ~fse
    =
    mu_unlock fse.fse_lock


(*
**
** Args:
**
**  fse: the fsc entry returned by the lookup function
**  buf: the client buffer to read in
**  obj: The object/file number
**  off: The logical file offset [bytes]
**  size: The size of the desired fragment or the whole file [bytes]
**
** Return:
**
**  status:
**
*) 

let cache_read  ~cache
                ~fse
                ~buf
                ~off
                ~size   
    =
  try
  begin
   
#ifdef DEBUG
        Db.Pr.sddd 8 "cache_read [daddr,foff,size]" 
                  fse.fse_disk_addr 
                  off
                  size;
#endif

        let nc2 = cache.fsc_size / 2 in

        let to_block x =
            x/cache.fsc_block_size
        in

        let daddr  = fse.fse_disk_addr in
        let bufcl = fse.fse_cached in
        let stat  = ref std_OK in
        let foff  = ref off in          (* file offeset         *)
        let size  = ref size in         (* size to be read      *)
        let doff  = ref 0 in            (* dst buffer offest    *)


        let dst = buf in         (* the destination buf  *)

        Stat.rd cache;

        (*
        ** Insert a new created buffers in the fse_cached list
        ** at the right position (before current hd
        ** :: increasing buf offset order).
        ** Return the current hd entry
        ** and the tail list after
        ** the inserted and the current hd entry.
        *)

        let insert_cached fse ne =
              let bli = ref [] in

              let nc = cache.fsc_buffers.(ne) in
              let no = nc.fsb_off in
              let ns = nc.fsb_size in

              let rec iter fl = 
                match fl with 
                | hd::tl ->
                begin
                    let fc = cache.fsc_buffers.(hd) in
                    let fo = fc.fsb_off in
                    let fs = fc.fsb_size in
                    
                    if (no < fo) then
                    begin
                        let hdtl = [hd] @ tl in
                        bli := !bli @ [ne] @ hdtl;
                        hdtl;
                    end
                    else
                    begin
                        bli := !bli @ [hd];
                        iter tl;
                    end;
                end;
                | [] -> 
                begin 
                    bli := !bli @ [ne];
                    []
                end;
                    
        
            in


            let tl = iter fse.fse_cached in
            fse.fse_cached <- !bli;

            tl
        in


        (*
        ** Iterate the list of cached buffers and check the file 
        ** offset and the size of each buffer. Missing buffers
        ** must be  inserted at the right position in the buffer list.
        ** But first check the last buffer (fse_lastbuf) to see wheter
        ** we have a sequential read operation. This speeds up this
        ** operation significantly.
        *)

        let rec iter bl =
        match bl with
        | hd::tl -> 
          begin

            let fb = cache.fsc_buffers.(hd) in

            mu_lock fb.fsb_lock;

            if (!foff >= fb.fsb_off &&
                !foff < (fb.fsb_off + fb.fsb_size)) then
            begin

                Stat.hit cache;

                (*
                ** Some data for us
                *)

                let cboff  = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let src = fb.fsb_buf in

                blit_bb ~src:src
                        ~src_pos:cboff
                        ~dst:dst
                        ~dst_pos:!doff
                        ~len:cbsiz;
                
                size := !size - cbsiz;
                foff := !foff + cbsiz;
                doff := !doff + cbsiz;

                mu_unlock fb.fsb_lock;

                if (!size > 0) then
                    iter tl;
            end
            else if (!foff < fb.fsb_off) then
            begin
                Stat.miss cache;

                (*
                ** Missing data. Create a new buffer and read
                ** the file data from disk. 
                *)

                mu_unlock fb.fsb_lock;

                (*
                ** Be aware: After a get_buf call, the fse_cached
                ** list might be modified!
                *)

                let stat,bufc = get_buf cache in

                if (stat <> std_OK) then
                begin
                    sys_log Sys_err "CACHE: cache_read: failed to get a buffer: %s\n"
                                      (err_why stat);
                    raise (Error std_SYSERR);
                end;


                let fb = cache.fsc_buffers.(bufc) in
                mu_lock fb.fsb_lock;

                let cbl = cache.fsc_buf_size in

                fb.fsb_off <- (!foff / cbl) * cbl; 
                fb.fsb_size <- (
                    if (fb.fsb_off + cbl > fse.fse_disk_size) then
                    begin
                        fse.fse_disk_size - fb.fsb_off
                    end
                    else
                        cbl
                );

                (*
                ** READ 
                *)

                Stat.dird cache;

                let stat =
                    cache.fsc_read 
                               ~obj:fse.fse_objnum
                               ~addr:(daddr+
                                      (to_block fb.fsb_off))
                               ~data:fb.fsb_buf
                               ~size:fb.fsb_size
                in

                if (stat <> std_OK) then
                begin
                    
                    sys_log Sys_err "CACHE: cache_read: disk_read failed: %s\n"
                                         (err_why stat);
                    mu_unlock fb.fsb_lock;
                    raise (Error stat);
                end;


                let cboff = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let src = fb.fsb_buf in

                blit_bb ~src:src
                        ~src_pos:cboff
                        ~dst:dst
                        ~dst_pos:!doff
                        ~len:cbsiz;

                fb.fsb_state <- Cache_Sync;

                let hdtl = insert_cached fse bufc in

                size := !size - cbsiz;
                foff := !foff + cbsiz;
                doff := !doff + cbsiz;

                mu_unlock fb.fsb_lock;

                if (!size > 0) then
                    iter hdtl;

            end
            else
            begin
                mu_unlock fb.fsb_lock;
                iter tl;
            end;
          end;
        | [] -> 
        begin
                (*
                ** Missing data at the end. Create a new buffer and read
                ** the file data from disk. Append the new buffer
                ** at the end of the buffer list.
                *)
                
                Stat.miss cache;

                let stat,bufc = 
                    if (List.length fse.fse_cached) < nc2 then
                        get_buf cache 
                    else
                    begin
                        (*
                        ** Too much buffers allocated.
                        ** Take the new buffer from the
                        ** head of the cached list. 
                        *)
                        let hd,tl = match fse.fse_cached with
                                    | hd::tl -> hd,tl;
                                    | [] -> failwith
                                        "programming error";
                            in
                        fse.fse_cached <- tl;
                        let fb = cache.fsc_buffers.(hd) in

                        if (fb.fsb_state = Cache_Modified) then
                        begin
                            mu_lock fb.fsb_lock;
                            (*
                            ** First write the data to disk!
                            *)

                            Stat.diwr cache;
    
                            let stat = cache.fsc_write
                                            ~obj:fse.fse_objnum 
                                            ~addr:((to_block fb.fsb_off)+
                                                    fse.fse_disk_addr)
                                            ~data:fb.fsb_buf
                                            ~size:fb.fsb_size
                                in                                        
                            mu_unlock fb.fsb_lock;
                            if (stat <> std_OK) then
                            begin
                                sys_log Sys_err
                                        "CACHE: cache_read: disk_write failed: %s\n"
                                        (err_why stat);
                                raise (Error stat);
                            end;
                        end;
                        std_OK,hd
                    end
                in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err
                        "CACHE: cache_read: failed to get a buffer: %s\n"
                            (err_why stat);
                    raise (Error stat); 
                end;

                let fb = cache.fsc_buffers.(bufc) in
                mu_lock fb.fsb_lock;

                let cbl = cache.fsc_buf_size in

                fb.fsb_off <- (!foff / cbl) * cbl; 
                fb.fsb_size <- (
                    if (fb.fsb_off + cbl > fse.fse_disk_size) then
                    begin
                        fse.fse_disk_size - fb.fsb_off
                    end
                    else
                        cbl
                );

                (*
                ** READ 
                *)

                Stat.dird cache;

                let stat =
                    cache.fsc_read 
                               ~obj:fse.fse_objnum
                               ~addr:(daddr+
                                     (to_block fb.fsb_off))
                               ~data:fb.fsb_buf
                               ~size:fb.fsb_size
                in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err
                        "CACHE: cache_read: disk_read failed: %s\n"
                        (err_why stat);
                    mu_unlock fb.fsb_lock;
                    raise (Error stat);
                end;

                fb.fsb_state <- Cache_Sync;

                fse.fse_cached <- fse.fse_cached @ [bufc];  
                fse.fse_lastbuf <- bufc;

                let cboff = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let src = fb.fsb_buf in

                blit_bb ~src:src
                        ~src_pos:cboff
                        ~dst:dst
                        ~dst_pos:!doff
                        ~len:cbsiz;

                size := !size - cbsiz;
                foff := !foff + cbsiz;
                doff := !doff + cbsiz;

                mu_unlock fb.fsb_lock;

                if (!size > 0) then
                    iter [];
        end;

        in

        if (!foff + !size > fse.fse_disk_size) then
        begin
            raise (Error std_ARGBAD);
        end;
        (*
        ** Check first the last buffer.
        *)
        if (fse.fse_lastbuf >= 0) then
        begin
            let fb = cache.fsc_buffers.(fse.fse_lastbuf) in
            if (off >= fb.fsb_off) then
            begin
                (*
                ** Sequential read. 
                *)
                iter [fse.fse_lastbuf];
            end
            else
                iter bufcl;
        end
        else
            iter bufcl;
        std_OK
    end
    with
        | Buf_overflow ->
        begin
            sys_log Sys_fatal 
                "CACHE: cache_read: caught buffer overflow error. Fatal.\n";
            std_SYSERR;
        end;
        | Error err -> err

 
    
    


(*
**
** Args:
**
**  fse: The fsc entry returned by the lookup function
**  buf: The data to be written
**  off: The file offset
**  size: The size of the desired fragment or the whole file
**
** Return:
**
**  status:
**
*) 

let cache_write ~cache
                ~fse
                ~buf
                ~off
                ~size   
    =
    try
    begin
#ifdef DEBUG
        Db.Pr.sdddd 5 "cache_write [daddr,dsize,foff,size]" 
                  fse.fse_disk_addr 
                  fse.fse_disk_size
                  off
                  size;
#endif

        let clear_buf buf = 
            buf.fsb_off  <- -1;
            buf.fsb_size <- -1;
            buf.fsb_state <- Cache_Empty;
        in
        let to_block x =
            x/cache.fsc_block_size
        in
        let block x = let block_size = cache.fsc_block_size in
                      ((x+block_size-1)/block_size)*block_size 
        in

        let nc2 = cache.fsc_size / 2 in

        let daddr  = fse.fse_disk_addr in

        let bufcl = fse.fse_cached in
        let stat  = ref std_OK in
        let foff  = ref off in          (* file offeset         *)
        let size  = ref size in         (* size to be write     *)
        let soff  = ref 0 in            (* src buffer offest    *)

        let src = buf in                (* the source buf       *)


        Stat.wr cache;

        if (fse.fse_state <> FF_unlocked || 
            !foff + !size > fse.fse_disk_size) then
        begin
            raise (Error std_ARGBAD);
        end;

        (*
        ** Insert new created buffers in the fse_cached list
        ** at the right position (before current hd
        ** -> increasing buffer offset order).
        ** Return the current hd entry
        ** and the tail list after
        ** the inserted and the current hd entry.
        *)

        let insert_cached fse ne =
              let bli = ref [] in

              let nc = cache.fsc_buffers.(ne) in
              let no = nc.fsb_off in
              let ns = nc.fsb_size in

              let rec iter fl = 
                match fl with 
                | hd::tl ->
                begin
                    let fc = cache.fsc_buffers.(hd) in
                    let fo = fc.fsb_off in
                    let fs = fc.fsb_size in
                    
                    if (no < fo) then
                    begin
                        let hdtl = [hd] @ tl in
                        bli := !bli @ [ne] @ hdtl;
                        hdtl;
                    end
                    else
                    begin
                        bli := !bli @ [hd];
                        iter tl;
                    end;
                end;
                | [] -> 
                begin 
                    bli := !bli @ [ne];
                    []
                end;
                    
        
            in


            let tl = iter fse.fse_cached in
            fse.fse_cached <- !bli;


            tl
        in


        (*
        ** Iterate the list of cached buffers and check the file 
        ** offset and the size for each buffer. Missing buffers
        ** must be  inserted at the right position in the buffer list.
        ** But first check the last buffer cached. This speeds up
        ** sequential writes.
        *)

        let rec iter bl =
        match bl with
        | hd::tl -> 
          begin
                        

            let fb = cache.fsc_buffers.(hd) in
            mu_lock fb.fsb_lock;

#ifdef DEBUG
            Db.Pr.sdd 6 "cache_write: [boff,bsiz]"
                        fb.fsb_off fb.fsb_size;
#endif

            (*
            ** If this is the last buffer, we must perhaps fix
            ** the buffer size due to an increased file size!
            ** The last buffer is recognized with a smaller
            ** fsb_size than fsc_buf_size.
            *)

            if (fb.fsb_size < cache.fsc_buf_size &&
                fb.fsb_off + fb.fsb_size < fse.fse_disk_size) then
            begin
#ifdef DEBUG
                Db.Pr.s 6 "cache_write: fixing buf size";
#endif

                let s = block (fse.fse_disk_size - fb.fsb_off) in
                fb.fsb_size <- if (s > cache.fsc_buf_size) then
                                        cache.fsc_buf_size
                                    else
                                        s;
            end;  


            if (!foff >= fb.fsb_off &&
                !foff < (fb.fsb_off + fb.fsb_size)) then
            begin

                (*
                ** Some data for us
                *)

                Stat.hit cache;

                let cboff  = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let dst = fb.fsb_buf in

                blit_bb     ~src:src
                            ~src_pos:!soff
                            ~dst:dst
                            ~dst_pos:cboff
                            ~len:cbsiz;

                (*
                ** Cache write through mode ?
                *)

                if (cache.fsc_mode = Cache_R) then
                begin
                    let stat = cache.fsc_write 
                                        ~obj:fse.fse_objnum
                                        ~addr:((to_block fb.fsb_off)+
                                                    fse.fse_disk_addr)
                                        ~data:fb.fsb_buf
                                        ~size:(block fb.fsb_size)
                               in
                    if (stat = std_OK) then
                        fb.fsb_state <- Cache_Sync
                    else
                    begin
                        sys_log Sys_err 
                                "cache_write: disk_write failed: %s\n"
                                (err_why stat);
                        mu_unlock fb.fsb_lock;
                        raise (Error stat);                                        
                    end;
                end
                else
                    fb.fsb_state <- Cache_Modified;
                
                size := !size - cbsiz;
                foff := !foff + cbsiz;
                soff := !soff + cbsiz;


                mu_unlock fb.fsb_lock;

                if (!size > 0) then
                    iter tl;
            
            end
            else if (!foff < fb.fsb_off) then
            begin
                (*
                ** Missing data. Create a new buffer and write
                ** the file data to this buffer. 
                ** If there is this buffer offset already
                ** in the written list, it's necessary to read
                ** the data first from disk!
                *)

                Stat.miss cache;

                mu_unlock fb.fsb_lock;

                (*
                ** Be aware: After a get_buf call, the fse_cached
                ** list might be modified!
                *)

                let stat,bufc = get_buf cache in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err 
                            "cache_write: failed to get a buffer: %s\n"
                            (err_why stat);
                    raise (Error std_SYSERR); 
                end;


                let fb = cache.fsc_buffers.(bufc) in
                mu_lock fb.fsb_lock;
                
                let cbl = cache.fsc_buf_size in

                fb.fsb_off <- (!foff / cbl) * cbl; 
                fb.fsb_size <- (
                    if (fb.fsb_off + cbl > fse.fse_disk_size) then
                    begin
                        fse.fse_disk_size - fb.fsb_off
                    end
                    else
                        cbl
                );

                (*
                ** READ the data first from disk ? But only
                ** 'size' bytes. 
                *)

                let fromdisk = ref 0 in
                List.iter ( fun os ->
                        let off,size = os in
                        if (off = fb.fsb_off) then
                            fromdisk := size;
                    ) fse.fse_written;


                if (!fromdisk <> 0) then
                begin
                    Stat.dird cache;

                    let stat =
                        cache.fsc_read 
                               ~obj:fse.fse_objnum
                               ~addr:(daddr+
                                      (to_block fb.fsb_off))
                               ~data:fb.fsb_buf
                               ~size:!fromdisk
                    in
                    if (stat <> std_OK) then
                    begin
                        sys_log Sys_err
                                "cache_write: disk_write failed: %s\n"
                                (err_why stat);
                        mu_unlock fb.fsb_lock;
                        raise (Error stat);
                    end;
                end;


                let cboff = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let dst = fb.fsb_buf in

                blit_bb     ~src:src
                            ~src_pos:!soff
                            ~dst:dst
                            ~dst_pos:cboff
                            ~len:cbsiz;

                (*
                ** Write through cache ? 
                *)

                if (cache.fsc_mode = Cache_R) then
                begin
                    let stat = cache.fsc_write 
                                        ~obj:fse.fse_objnum
                                        ~addr:((to_block fb.fsb_off)+
                                                    fse.fse_disk_addr)
                                        ~data:fb.fsb_buf
                                        ~size:(block fb.fsb_size)
                               in
                    if (stat = std_OK) then
                        fb.fsb_state <- Cache_Sync
                    else
                    begin
                        mu_unlock fb.fsb_lock;
                        sys_log Sys_err
                                "cache_write: disk_write failed: %s\n"
                                (err_why stat);
                        raise (Error stat);                                        
                    end;
                end
                else
                    fb.fsb_state <- Cache_Modified;

                let hdtl = insert_cached fse bufc in

                mu_unlock fb.fsb_lock;

                size := !size - cbsiz;
                foff := !foff + cbsiz;
                soff := !soff + cbsiz;


                if (!size > 0) then
                    iter hdtl;          (* !!! *)

            end
            else
            begin
                mu_unlock fb.fsb_lock;

                iter tl;
            end;
          end;
        | [] -> 
        begin
                (*
                ** Missing data at the end. Create a new buffer and copy
                ** the file data to the cache buffer. Append the new buffer
                ** at the end of the buffer list.
                ** If there is this buffer offset already
                ** in the written list, it's necessary to read
                ** the data first from disk!
                *)

                Stat.miss cache;

                let stat,bufc = if (List.length fse.fse_cached) < nc2 then
                                    get_buf cache 
                                else
                                begin
                                    (*
                                    ** Too much buffers allocated.
                                    ** Take the new buffer from the
                                    ** head of the cached list. 
                                    *)
                                    let hd,tl = match fse.fse_cached with
                                                | hd::tl -> hd,tl;
                                                | [] -> failwith
                                                        "programming error";
                                    in
                                    fse.fse_cached <- tl;
                                    let fb = cache.fsc_buffers.(hd) in

                                    if (fb.fsb_state = Cache_Modified) then
                                    begin
                                        mu_lock fb.fsb_lock;
                                        (*
                                        ** First write the data to disk!
                                        *)

                                        Stat.diwr cache;
    
                                        let stat =
                                            cache.fsc_write
                                            ~obj:fse.fse_objnum 
                                            ~addr:((to_block fb.fsb_off)+
                                                    fse.fse_disk_addr)
                                            ~data:fb.fsb_buf
                                            ~size:(block fb.fsb_size)
                                        in                                        
                                        if (stat = std_OK) then
                                        begin
                                            fse.fse_written <- 
                                                        fse.fse_written @
                                                        [fb.fsb_off,
                                                        (block fb.fsb_size)];

                                            clear_buf fb;
                                        end;
                                        mu_unlock fb.fsb_lock;
                                        if (stat <> std_OK) then
                                        begin
                                            sys_log Sys_err
                                            "cache_write: disk_write failed: %s\n"
                                            (err_why stat);
                                            raise (Error stat);
                                        end;
                                    end;
                                    std_OK,hd
                                end;
                in
                if (stat <> std_OK) then
                begin
                    sys_log Sys_err
                            "cache_write: failed to get a buffer: %s\n"
                            (err_why stat);
                    raise (Error stat); 
                end;


                let fb = cache.fsc_buffers.(bufc) in
                mu_lock fb.fsb_lock;

                let cbl = cache.fsc_buf_size in

                fb.fsb_off <- (!foff / cbl) * cbl; 
                fb.fsb_size <- (
                    if (fb.fsb_off + cbl > fse.fse_disk_size) then
                    begin
                        fse.fse_disk_size - fb.fsb_off
                    end
                    else
                        cbl
                );

                (*
                ** READ the data first from disk ? But only
                ** 'size' bytes. 
                *)

                let fromdisk = ref 0 in
                List.iter ( fun os ->
                        let off,size = os in
                        if (off = fb.fsb_off) then
                            fromdisk := size;
                    ) fse.fse_written;

                if (!fromdisk <> 0) then
                begin
                    Stat.dird cache;

                    let stat =
                        cache.fsc_read 
                               ~obj:fse.fse_objnum
                               ~addr:(daddr+
                                      (to_block fb.fsb_off))
                               ~data:fb.fsb_buf
                               ~size:!fromdisk
                    in
                    if (stat <> std_OK) then
                    begin
                        sys_log Sys_err 
                                "cache_write: disk_read failed: %s\n"
                                (err_why stat);
                        mu_unlock fb.fsb_lock;
                        raise (Error stat);
                    end;
                end;


                fse.fse_cached <- fse.fse_cached @ [bufc];  
                fse.fse_lastbuf <- bufc;

                let cboff = !foff - fb.fsb_off  in
                let cbsiz' = fb.fsb_size - cboff in
                let cbsiz  = if cbsiz' > !size then
                                !size
                            else
                                cbsiz'
                in

                (*
                ** COPY 
                *)

                let dst = fb.fsb_buf in

                blit_bb     ~src:src
                            ~src_pos:!soff
                            ~dst:dst
                            ~dst_pos:cboff
                            ~len:cbsiz;

                size := !size - cbsiz;
                foff := !foff + cbsiz;
                soff := !soff + cbsiz;

                (*
                ** Write throuch cache ?
                *)

                if (cache.fsc_mode = Cache_R) then
                begin
                    let stat = cache.fsc_write 
                                        ~obj:fse.fse_objnum
                                        ~addr:((to_block fb.fsb_off)+
                                                    fse.fse_disk_addr)
                                        ~data:fb.fsb_buf
                                        ~size:(block fb.fsb_size)
                               in
                    if (stat = std_OK) then
                        fb.fsb_state <- Cache_Sync
                    else
                    begin
                        mu_unlock fb.fsb_lock;
                        sys_log Sys_err
                                "cache_write: disk_write failed: %s\n"
                                (err_why stat);
                        raise (Error stat);                                        
                    end;
                end
                else
                    fb.fsb_state <- Cache_Modified;

                mu_unlock fb.fsb_lock;

                if (!size > 0) then
                    iter [];
        end;

        in

        (*
        ** Check first the last buffer.
        *)
        if (fse.fse_lastbuf >= 0) then
        begin
            let fb = cache.fsc_buffers.(fse.fse_lastbuf) in
            if (off >= fb.fsb_off) then
            begin
                (*
                ** Sequential write. 
                *)
                iter [fse.fse_lastbuf];
            end
            else
                iter bufcl;
        end
        else
            iter bufcl;

#ifdef DEBUG
        Db.Pr.sd 5 "cache_write: exit" fse.fse_disk_addr;
#endif
        std_OK
    end
    with
        | Buf_overflow ->
        begin
            sys_log Sys_fatal 
                    "cache_read: caught buffer overflow error. Fatal.\n";
            std_SYSERR;
        end;
        | Error err -> err

 
(*
** Invalidate a cached file. Clean the cache.
*)

let cache_delete  ~cache 
                  ~fse
    =
    Stat.del cache;
    let clear_buf buf = 
        buf.fsb_off  <- -1;
        buf.fsb_size <- -1;
    in
    let to_block x =
        x/cache.fsc_block_size
    in

    (*
    ** Don't forget to move the used buffers to the cache
    ** free list.
    *)
    cache.fsc_free_bufs <- cache.fsc_free_bufs @ fse.fse_cached;

    List.iter ( fun fb ->
            clear_buf (cache.fsc_buffers.(fb));            
        ) fse.fse_cached;
    
    Hashtbl.remove cache.fsc_table fse.fse_objnum;
    std_OK


(*
** (Safety) Commit a cached object, do all outstanding write operations.
*)

let cache_commit ~cache
                 ~fse
    =
    try
    begin
        Stat.comm cache;

        let to_block x =
            x/cache.fsc_block_size
        in
        let block x = let block_size = cache.fsc_block_size in
                      ((x+block_size-1)/block_size)*block_size 
        in

#ifdef DEBUG
        Db.Pr.sd 2 "afs_cache: cache_commit: [obj]" fse.fse_objnum;
#endif


        List.iter ( fun fi ->
            let fb = cache.fsc_buffers.(fi) in
            mu_lock fb.fsb_lock; 
            if (fb.fsb_state = Cache_Modified) then
            begin
                let stat =
                    cache.fsc_write 
                            ~obj:fse.fse_objnum
                            ~addr:((to_block fb.fsb_off)+
                                   fse.fse_disk_addr)
                            ~data:fb.fsb_buf
                            ~size:(block fb.fsb_size)
                in                                        
                if (stat <> std_OK) then
                begin
                    mu_unlock fb.fsb_lock;
                    raise (Error stat);
                end;
                fse.fse_written <- fse.fse_written @
                                   [fb.fsb_off,(block fb.fsb_size)];

            
                fb.fsb_state <- Cache_Sync;
            end;
            mu_unlock fb.fsb_lock;
          ) fse.fse_cached;
        (*
        ** Notify server we've synced the disk for this object.
        *)
        cache.fsc_synced ~obj:fse.fse_objnum; 
        std_OK
    end
    with
        | Error err -> err

    
(*
** Flush the cache.
*)

let cache_sync ~cache
    =
    try
    begin
        mu_lock cache.fsc_lock;
        Hashtbl.iter ( fun obj fse ->
            if (fse.fse_state = FF_commit) then
            begin

#ifdef DEBUG
                Db.Pr.sd 3 "cache_sync: FF_commit" obj;
#endif

                mu_lock fse.fse_lock;
                let stat = cache_commit cache fse in
                if (stat <> std_OK) then
                begin
                    mu_unlock fse.fse_lock;
                    mu_unlock cache.fsc_lock;
                    raise (Error stat);
                end;
                fse.fse_state <- FF_locked;
                mu_unlock fse.fse_lock;
            end;
        ) cache.fsc_table;
        mu_unlock cache.fsc_lock;
        std_OK
    end
    with
        | Error err -> err



(*
** Decrement (age) the live times of all currently cached objects.
** Remove objects with live time = 0. Must be called periodically
** to keep only currently used objects in the cache. This
** functions returns a (daddr,dsize) list of killed files. 
*)

let cache_age ~cache 
    =
    try
    begin
        let dying = ref [] in
        let killed = ref [] in

        Stat.age cache;

        mu_lock cache.fsc_lock;
        Hashtbl.iter ( fun addr fse ->
            mu_lock fse.fse_lock;
            fse.fse_live <- fse.fse_live - 1;
        
            if (fse.fse_live = 0) then
            begin
                if (fse.fse_state = FF_commit) then
                begin
                    (*
                    ** Flush the cache.
                    *)

                    fse.fse_state <- FF_locked;
                    let stat = cache_commit cache fse in
                    
                    if (stat <> std_OK) then
                    begin
                        mu_unlock fse.fse_lock;
                        mu_unlock cache.fsc_lock;
                        raise (Error stat);
                    end;
                end;
                (*
                ** Unlocked objects will be destroyed!
                *)
                Stat.time cache;
                dying := !dying @ [fse];
            end;
            mu_unlock fse.fse_lock;
        ) cache.fsc_table;

        if (!dying <> []) then
            List.iter ( fun fse ->
                        mu_lock fse.fse_lock;
                        let stat = cache_delete cache fse in
                        if (stat <> std_OK) then
                        begin
                            mu_unlock fse.fse_lock;
                            mu_unlock cache.fsc_lock;
                            raise (Error stat);
                        end;
                        killed := !killed @ [
                                      (fse.fse_disk_addr,
                                       fse.fse_disk_size)
                                    ];
                        mu_unlock fse.fse_lock;
                ) !dying;

        mu_unlock cache.fsc_lock;
        std_OK,!killed
    end
    with
        | Error err -> err,[]


(*
** Format the statistic fields.
*)

open Printf

let cache_stat ~cache =
   sprintf 
        "%s   %s   %s\n%s   %s   %s\n%s   %s   %s\n%s   %s\n"
        (sprintf "Read:         %8d" cache.fsc_stat.cs_cache_read)
        (sprintf "Write:        %8d" cache.fsc_stat.cs_cache_write)
        (sprintf "Commit:       %8d" cache.fsc_stat.cs_cache_commit)
        (sprintf "Delete:       %8d" cache.fsc_stat.cs_cache_delete)
        (sprintf "Compact:      %8d" cache.fsc_stat.cs_cache_compact)
        (sprintf "Disk Read:    %8d" cache.fsc_stat.cs_disk_read)
        (sprintf "Disk Write:   %8d" cache.fsc_stat.cs_disk_write)
        (sprintf "Cache hit:    %8d" cache.fsc_stat.cs_cache_hit)
        (sprintf "Cache miss:   %8d" cache.fsc_stat.cs_cache_miss)
        (sprintf "Timeout:      %8d" cache.fsc_stat.cs_cache_timeout)
        (sprintf "Age:          %8d" cache.fsc_stat.cs_cache_age)
    
