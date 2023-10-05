
open Bytebuf


val cache_MAXLIVE : int


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

(*
** One object cached by this module.
*)

type fsc_entry = {
    (*
    ** Object number. Must be unique.
    *)
    mutable fse_objnum: int;

    (*
    ** Physical disk address of this cached object. This is the
    ** object identifier because it's an unique value.
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
    mutable fse_state: Afs_server.afs_file_state;

    (*
    ** Live time
    *)

    mutable fse_live: int;

    (*
    ** Lock
    *)

    mutable fse_lock: Mutex.t;
}

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
    ** Blocksize in bytes [multiple of the disk block_size]
    *)

    mutable fsc_block_size: int;

    (*
    ** One buffer size in blocks. Fixed! Commonly multiple of the
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
    ** hash table. The key is the physical disk address!
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
    mutable fsc_read:          obj: int ->
                               addr:int ->
                               data: buffer ->
                               size: int ->
                               Amoeba.status; 

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
                            Amoeba.status;

    (*
    ** Notify the server about a synced file.
    *)

    mutable fsc_synced: obj:int -> unit;


    mutable fsc_lock: Mutex.t;


    mutable fsc_mode: afs_cache_mode;

    mutable fsc_stat: fsc_stat;
}


val nilfsc : fsc_buf
val nilfse : fsc_entry



(*
** Age all objects, remove objects with live = 0 (Cache_Read 
** or Cache_Committed) and transfer their buffers to the freelist. 
** If there are still no free buffers, decrease the buffer list
** of the oldest object(s).
**
** The code is slightly blurred with cache entry and buffer locking:
**
**  1. Only unlocked cache entries can be removed from the cache.
**  2. Only unlocked buffers can be transfered to the free list.
**
** Remember the case only one object uses the entire cache! 
**
*)

val cache_compact : cache:fsc_cache -> unit


(*
** Get a new buffer from the cache. If there are no free buffers,
** the cache_compact function is called. 
*)


val get_buf : cache:fsc_cache -> Amoeba.status * int

(*
** Create a new cache.
** Args:
**  nbufs: Number of fixed size buffers.
**  blocksize: The filesystem blocksize [bytes].
**  bufsize: The fixed size of one buffer in the cache [blocks].
**  read: The user supplied disk read function. See above.
**  write: The user supplied disk write function. See above.
**  sync: Notify the server about a synced file.
**
** Return:
**
**  status
**  cache
**
** The server must create one cache for the file data, and one
** cache for the inode blocks.
*)

val cache_create : 
    nbufs:int ->
    blocksize:int ->
    bufsize:int ->
    read:(obj:int -> addr:int -> data:buffer -> size:int -> Amoeba.status) ->
    write:(obj:int -> addr:int -> data:buffer -> size:int -> Amoeba.status) ->
    sync:(obj:int -> unit) ->
    mode:afs_cache_mode ->
    Amoeba.status * fsc_cache

(*
** Lookup the cache for a file object. If there is no such object
** already cached, create a new one, else return the fsc entry.
** The cache entry is returned with a locked mutex! 
*)

val cache_lookup :
    cache:fsc_cache ->
    obj:int ->
    addr:int ->
    size:int -> state:Afs_server.afs_file_state 
    -> bool * fsc_entry


(*
** If the work is done, this function must be called.
*)

val cache_release : cache:fsc_cache ->
                    fse:fsc_entry ->
                    unit


(*
**
** Args:
**
**  fse: the fsc entry returned by the lookup function
**  buf: the client buffer to read in
**  obj: The object/file number
**  off: The logical file offset
**  size: The size of the desired fragment or the whole file
**
** Return:
**
**  status:
**
*)


val cache_read : cache:fsc_cache ->
    fse:fsc_entry ->
    buf:buffer -> off:int -> size:int 
    -> Amoeba.status

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

val cache_write : cache:fsc_cache ->
    fse:fsc_entry ->
    buf:buffer -> off:int -> size:int 
    -> Amoeba.status


(*
** Invalidate a cached file. Clean the cache.
*)


val cache_delete : cache:fsc_cache -> 
            fse:fsc_entry 
            -> Amoeba.status

(*
** (Safety) Commit a file, do all outstanding write operations.
*)

val cache_commit : cache:fsc_cache -> 
            fse:fsc_entry 
            -> Amoeba.status



(*
** Flush the cache.
*)

val cache_sync : cache:fsc_cache -> Amoeba.status


(*
** Decrement (age) the live times of all currently cached objects.
** Remove objects with live time = 0. Must be called periodically
** to keep only currently used objects in the cache. Returns
** a (daddr,dsize) list of all killed files.
*)

val cache_age : cache:fsc_cache -> Amoeba.status * (int * int) list

(*
** Format statistic infromations
*)

val cache_stat : cache:fsc_cache -> string
