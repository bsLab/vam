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
**    $INITIAL:     (C) 2003-2005 BSSLAB
**    $CREATED:
**    $MODIFIED:
**    $VERSION:     1.05
**
**    $INFO:
**
** Low level support for process execution and memeory segment management.
**
**
**    $ENDOFINFO
**
*)


(*
** name of the process server directory entry in the processor directory
*)

val process_SRV_NAME : string

(*
** name of the process list directory entry likewise
*)
val process_LIST_NAME : string

(*
** Process server command codes.
*)
val ps_STUN : Amoeba.command
val ps_GETOWNER : Amoeba.command
val ps_SETOWNER : Amoeba.command
val ps_EXEC : Amoeba.command
val ps_GETDEF : Amoeba.command
val ps_GETLOAD : Amoeba.command

(*
** commands for managing segments
*)
val ps_SEG_CREATE : Amoeba.command
val ps_SEG_WRITE : Amoeba.command
val ps_SEG_READ : Amoeba.command
val ps_SEG_SIZE : Amoeba.command
(*
** Process owner command codes (from kernel to owner).
*)
val ps_CHECKPOINT : Amoeba.command

(*
** Process info text
*)
val ps_SETCOMMENT : Amoeba.command
val ps_GETCOMMENT : Amoeba.command
(*
** Command to tell an owner its property has a new process capability
*)
val ps_SWAPPROC : Amoeba.command
(*
** New getload command for secure process server
*)
val ps_SGETLOAD : Amoeba.command

(*
** Rights bits in process or segment capabilities.
*)
val psr_READ : Amoeba.rights_bits
val psr_WRITE : Amoeba.rights_bits
val psr_CREATE : Amoeba.rights_bits
val psr_DELETE : Amoeba.rights_bits
val psr_EXEC : Amoeba.rights_bits
val psr_KILL : Amoeba.rights_bits
val psr_ALL : Amoeba.rights_bits

(*
** Segment descriptor.
*)
type segment_d = {
  mutable sd_cap : Amoeba.capability;   (* Capability (if unmapped) *)
  mutable sd_offset : Machtype.word32;  (* Offset in file *)
  mutable sd_addr : Machtype.word32;    (* Virtual address *)
  mutable sd_len : Machtype.word32;     (* Length (bytes) *)
  mutable sd_type : Machtype.word32;    (* Type bits (see below) *)
} 
val nilsd : segment_d

(*
** Bits in sd_type (also for seg_map)
*)
val map_GROWMASK : Machtype.word32      (* Growth direction *)
val map_GROWNOT : Machtype.word32
val map_GROWUP : Machtype.word32
val map_GROWDOWN : Machtype.word32

val map_TYPEMASK : Machtype.word32      (* Text/data indication *)
val map_TYPETEXT : Machtype.word32
val map_TYPEDATA : Machtype.word32

val map_PROTMASK : Machtype.word32      (* Read/write indication *)
val map_READONLY : Machtype.word32
val map_READWRITE : Machtype.word32

val map_SPECIAL : Machtype.word32
val map_INPLACE : Machtype.word32       (* Map in place, don't copy *)
val map_AND_DESTROY : Machtype.word32   (* Map, and std_destroy original *)
(*
** Shared between different
** local processes 
*)

val map_SHARED : Machtype.word32

val map_BINARY : Machtype.word32        (* Flags only in binary file *)
val map_SYSTEM : Machtype.word32        (* This seg wil recv args *)
val map_SYMTAB : Machtype.word32        (* Symbol table *)
val map_NEW_SYMTAB : Machtype.word32    (* New style symbol table *)

(*
** Fault frames - architecture dependent
** Entries in order they appear in the as-is fault frame returned by the
** kernel.
*)

type f_i386 = {
  mutable i386_memmap : Machtype.word32;
  mutable i386_gs : Machtype.word32;
  mutable i386_fs : Machtype.word32;
  mutable i386_es : Machtype.word32;
  mutable i386_ds : Machtype.word32;
  mutable i386_edi : Machtype.word32;
  mutable i386_esi : Machtype.word32;
  mutable i386_ebp : Machtype.word32;
  mutable i386_faddr : Machtype.word32;
  mutable i386_ebx : Machtype.word32;
  mutable i386_edx : Machtype.word32;
  mutable i386_ecx : Machtype.word32;
  mutable i386_eax : Machtype.word32;
  mutable i386_trap : Machtype.word32;
  mutable i386_errcode : Machtype.word32;
  mutable i386_eip : Machtype.word32;
  mutable i386_cs : Machtype.word32;
  mutable i386_eflag : Machtype.word32;
  mutable i386_esp : Machtype.word32;
  mutable i386_ss : Machtype.word32;
} 
and fault_frame = F_i386 of f_i386


and thread_idle = {
  mutable tdi_pc : Machtype.word32;     (* pc register *)
  mutable tdi_sp : Machtype.word32;     (* sp register *)
} 
and thread_kstate = {
  mutable tdk_timeout : Machtype.word32;
  mutable tdk_sigvec : Machtype.word32;
  mutable tdk_svlen : Machtype.word32;
  mutable tdk_signal : Machtype.word32;
  mutable tdk_local : Machtype.word32;
} 
and thread_info =
    Thread_idle of thread_idle
  | Thread_frame of fault_frame
  | Thread_kstate of thread_kstate
  | Thread_noinfo

(*
** Thread descriptor.
** This is followed by additional data indicated by td_extra.
** Extra data structures are present in the order of ascending bits.
*)

and thread_d = {
  mutable td_state : Machtype.word32;
  mutable td_len : Machtype.word32;
  mutable td_extra : Machtype.word32;
  mutable td_info : thread_info list;
} 
val niltd : thread_d

(*
** Bits in td_state (some are kernel internals)
*)
val tds_RUN : Machtype.word32       (* Thread is runnable *)
val tds_DEAD : Machtype.word32      (* (kernel only) thread is busy dying *)
val tds_EXC : Machtype.word32       (* Thread got some sort of trap *)
val tds_INT : Machtype.word32       (* (kernel only) thread got interrupt *)
val tds_FIRST : Machtype.word32     (* Schedule this thread first *)
val tds_NOSYS : Machtype.word32     (* Thread cannot do syscalls *)
val tds_START : Machtype.word32     (* (kernel only) immigrating thread *)
val tds_SRET : Machtype.word32      (* Thread is returning from sig *)
val tds_LSIG : Machtype.word32      (* Thread got lw signal *)
val tds_STUN : Machtype.word32      (* Thread stunned *)
val tds_HSIG : Machtype.word32      (* Compatibility *)
val tds_SHAND : Machtype.word32     (* Thread is calling sighand *)
val tds_DIE : Machtype.word32       (* DIE! *)
val tds_STOP : Machtype.word32      (* Stopped for stun handling *)
val tds_USIG : Machtype.word32      (* Sig should be delivered to user *)
val tds_CONTINUE : Machtype.word32  (* Continue thread after signal handling *)
(*
** Bits in td_extra, indicating presence of extra data structures:
*)

val tdx_IDLE : Machtype.word32      (* pc/sp struct follows *)
val tdx_KSTATE : Machtype.word32    (* kstate struct follows *)
val tdx_USTATE : Machtype.word32    (* ustate struct follows *)

(*
** Process descriptor.
** This is followed by pd_nseg segment descriptors (segment_d),
** reachable through PD_SD(p)[i], for 0 <= i < p->pd_nseg.
** The index in the segment array is also the segment identifier.
** Following the segments are pd_nthread variable-lenght thread descriptors.
** Sample code to walk through the threads:
**        thread_d *t = PD_TD(p);
**        for (i = 0; i < p->pd_nthread; ++i, t = TD_NEXT(t))
**                <here *t points to thread number i>;
*)
type process_d = {
    mutable pd_magic : string;            (* Architecture *)
    mutable pd_self : Amoeba.capability;  (* Process capability (if running) *)
    mutable pd_owner : Amoeba.capability; (* Default checkpoint recipient *)
    mutable pd_nseg : int;                (* Number of segments *)
    mutable pd_nthread : int;             (* Number of threads *)
    mutable pd_segs : segment_d array;    (* The segments *)
    mutable pd_threads : thread_d array;  (* The threads  *)
    (*
    ** Symbol table for internal usage only. Loaded first on exec_pd call...
    *)
    mutable pd_symtab: Symtable.nlist array;
} 

val nilpd : process_d

(*
** Checkpoint types in h_extra for PS_CHECKPOINT; detail in h_offset.
*)
val term_NORMAL : int       (* Normal termination; detail: exit status *)
val term_STUNNED : int      (* Process stunned; detail: pro_stun arg *) 
val term_EXCEPTION : int    (* Promoted exception; detail: exception nr. *)

(*
** Known architectures
*)
val arch_SIZE : int
val arch_list : (string * string) array

(*
** Process management options
*)
  
type p_option =
    | P_loadsym     (* load symbol table from PD *)


(*
** Transaction buffer size used by process/segment server (in bytes)
*)
val psrv_BUFSZ : int

(*
** Buffer functions
*)

(*
** buf_put_thread_d
**      puts the 3 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)
val buf_put_thread_d : buf:Bytebuf.buffer -> pos:int -> td:thread_d -> int

(*
** buf_put_thread_idle
**      puts the 2 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)
val buf_put_thread_idle : buf:Bytebuf.buffer -> pos:int -> td:thread_d -> int

(*
** buf_put_thread_kstate 
**      puts the 5 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)
val buf_put_thread_kstate :
  buf:Bytebuf.buffer -> pos:int -> td:thread_d -> int

(*
** buf_put_process_d
**      The reason we test to see if ARCHSIZE bytes fit and not a whole
**      process_d is that we only need to be sure we don't write beyond
**      the buffer ourselves. buf_put_cap(), etc don't write beyond the end
**      of the buffer.
*)
val buf_put_process_d : buf:Bytebuf.buffer -> pos:int -> pd:process_d -> int

(*
** buf_put_segment_d
**      puts the capability and 4 longs of this struct into the buffer.
**      buf_put_cap() & buf_put_int32() check to make sure that they fit.
*)
val buf_put_segment_d : buf:Bytebuf.buffer -> pos:int -> sd:segment_d -> int

(*
** buf_put_pd
**      This routine puts a process descriptor plus subsequent segment
**      descriptors and thread descriptors into a buffer.  It does not
**      pack the thread_ustate because this is so machine dependent (it
**      is a fault frame) that anyone who gets it had better know what 
**      to do with it themselves.  To avoid alignment problems it is copied
**      byte for byte into the buffer and must be retrieved that way.
**      Like all other buf_put_/buf_get_ routines it returns 0 if the buffer
is
**      not large enough.  Otherwise it returns a pointer to the next
**      available byte (after the process descriptor) in the buffer. 
**      NB: If the thread descriptors have invalid lengths then we return 0!
*)
val buf_put_pd : buf:Bytebuf.buffer -> pos:int -> pd:process_d -> int

(*
** buf_get_process_d
**      This applies a following sanity check to catch random data
**      offered as process descriptor:
**
**              pd_magic must be a string of at most ARCHSIZE-1
**              printable characters padded to ARCHSIZE with zero bytes.
*)
val buf_get_process_d : buf:Bytebuf.buffer -> pos:int -> int * process_d

(*
** buf_get_segment_d
**      gets the capability and 4 longs of the segment descriptor struct from
**      the buffer.
**      buf_get_cap() & buf_get_mach() check for the end of the buffer.
*)
val buf_get_segment_d : buf:Bytebuf.buffer -> pos:int -> int * segment_d

(*
** buf_get_thread_d
**      gets the 3 longs of the thread descriptor struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)
val buf_get_thread_d : buf:Bytebuf.buffer -> pos:int -> int * thread_d

(*
** buf_get_thread_idle
**      gets the 2 longs of the thread_idle struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)
val buf_get_thread_idle : buf:Bytebuf.buffer -> pos:int -> td:thread_d -> int

(*
** buf_get_thread_kstate
**      gets the 5 longs of the thread kernel state struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)
val buf_get_thread_kstate :
  buf:Bytebuf.buffer -> pos:int -> td:thread_d -> int

val buf_get_thread_ustate :
  buf:Bytebuf.buffer -> pos:int -> len:int -> pd:process_d -> td:thread_d -> int

(*
** buf_get_pd
**      This routine gets a process descriptor plus subsequent segment
**      descriptors and thread descriptors from a buffer.  It does not
**      unpack the thread_ustate because this is so machine dependent (it
**      is a fault frame) that anyone who gets it had better know what   
**      to do with it themselves.  We just dump it at the end of the thread
**      descriptor and hope that the receiver can figure out alignment, etc.
**      Like all other buf_put_/buf_get_ routines it returns 0 if the buffer is
**      not large enough.  Otherwise it returns a pointer to the next
**      available byte (after the process descriptor) in the buffer. 
**      NB: we work out the biggest the process_d could possibly be and
**      malloc a chunk of memory ourselves.  We then return a pointer to the
**      to the position in memory immediately after the pd.
**      NB: If the thread descriptors have invalid lengths then we return 0!
*)
val buf_get_pd : buf:Bytebuf.buffer -> pos:'a -> int * process_d

(*
** This size should hold a fresh process descriptor...
*)
val pd_SIZE : int
val pd_size : 'a -> int

(*
** pd_patch()
**
** Loop over the segment descriptors, patching those that
** must point into the executable file.
** They are recognizable by a nonzero offset and a zero port.
*)
val pd_patch : cap:Amoeba.capability -> pd:process_d -> unit

(*
** pd_read                               
**
**      Read a process descriptor from a file specified with 'cap', 
**      and convert it to host
**      byte order.  (The pd was stored in network byte order by ainstall.)
**      Returns the status and a fresh process descriptor pd.
*)
val pd_read : cap:Amoeba.capability -> Amoeba.status * process_d

(*
** pro_exec - Execute a process.
**      This is a very low-level interface.  It gives a process descriptor to
**      the process/segment server on the machine specified by 'host' which
**      starts the process if all is well.
*)
val pro_exec :
  host:Amoeba.capability -> pd:process_d -> Amoeba.status * Amoeba.capability

(*
** Default stack size 
*)
val def_STACKSIZE : int

(*
** Create a stack segment on the AFS server and fill in the
** environment!
*)
external buildstack :
  Bytebuf.buffer * Machtype.word32 * string array * string array *
  (string * string) array -> Machtype.word32 = "buildstack_ext"

val stack_create :
  server:Amoeba.capability ->
  sd:segment_d ->
  size:int ->
  args:string list ->
  strenv:string list ->
  capenv:(string * Amoeba.capability) list ->
  Amoeba.status * Amoeba.capability * Machtype.word32

(*
** Execute a process descriptor.
** 
** Arguments:
**
**  pd: process_d               /* In: process descriptor */
**  host: capability            /* In: host processor (process server) */
**  owner: capability           /* In: process owner */
**  stacksize: int              /* In: stack size (bytes) */
**  args:string list            /* In: program arguments */
**  strenv: string list         /* In: string environment */
**  capenv: caplist             /* In: capability environment */
**
** Returns:
** 
**  status   
**  retcap: capability          /* Out: capabilty for running process */
**
*)
val exec_pd :
  pd:process_d ->
  host:Amoeba.capability ->
  owner:Amoeba.capability ->
  stacksize:'a ->
  args:string list ->
  strenv:string list ->
  capenv:(string * Amoeba.capability) list ->
  opt:p_option list ->
  Amoeba.status * Amoeba.capability

(*
** Try to print a stacktrace from a fault frame
** (compiler dependent - currently only GCC-2.9 is supported!)
*)
val print_stacktrace : process_d -> fault_frame -> unit
val print_stacktrace_str : process_d -> fault_frame -> string

(*
** Print the content of a process descriptor:
**
**  Segments
**  Threads 
**  Fault frames
**  Stack traces (if segments still available)
*)
val print_pd : process_d -> unit
val print_pd_str : process_d -> string

(*
** Print segment descriptor informations
*)
val print_sd : segment_d -> unit
val print_sd_str : segment_d -> string

(*
** Send a process signal to the process specified by 'cap'.
*)
val pro_stun : cap:Amoeba.capability -> signal:int -> Amoeba.status

(*
** Process info
*)
val pro_setcomment : cap:Amoeba.capability -> str:string -> Amoeba.status
val pro_getcomment : cap:Amoeba.capability -> Amoeba.status * string

(*
** pro_setowner
**      Set the owner of a process to be someone else.
*)
val pro_setowner : cap:Amoeba.capability -> owner:Amoeba.capability -> Amoeba.status

(*
** pro_getowner
**      Find the capability of the owner of a particular process.
*)

val pro_getowner : cap:Amoeba.capability -> Amoeba.status * Amoeba.capability 



(*
** Write 'size' bytes of a process memory segment specified with 'segcap'
** starting at offset 'off' from the buffer 'buf'.
*)

val ps_segwrite: segcap:Amoeba.capability -> off:int ->  buf:Bytebuf.buffer
                 -> size:int -> Amoeba.status

(*
** Read 'size' bytes of a process memory segment specified with 'segcap'
** starting at offset 'off' into the buffer 'buf'.
*)
val ps_segread: segcap:Amoeba.capability -> off:int ->  buf:Bytebuf.buffer
                 -> size:int -> Amoeba.status

(*
** ps_segcreate - create a memory segment
**      Creates a memory segment on the machine specified by 'host'.  If
**      'clone' is not the null-cap then the segment created is a copy
**      of the segment specified by 'clone'.  ('clone' may well be an AFS
**      file capability.)
*)

val ps_segcreate: srv:Amoeba.capability ->  size:int -> clone:Amoeba.capability 
                -> Amoeba.status * Amoeba.capability 

