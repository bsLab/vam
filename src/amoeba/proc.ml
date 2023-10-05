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




open Amoeba
open Cmdreg
open Stdcom
open Stderr
open Bytebuf
open Buf
open Afs_common
open Afs_client
open Rpc
open Machtype
open Format
open Ar
open Printf
open Afs_client
open Symtable

let w32_0 = word32 0

let out = print_string
let nl = print_newline

(*
** name of the process server directory entry in the processor directory
*)
let process_SRV_NAME    = "proc"


(*
** name of the process list directory entry likewise
*)

let process_LIST_NAME   = "ps"

(*
** Process server command codes.
*)
let ps_STUN         = Command ps_FIRST_COM          (* pro_stun *)
let ps_GETOWNER     = Command (ps_FIRST_COM+1)      (* get owner capability *)
let ps_SETOWNER     = Command (ps_FIRST_COM+2)      (* set owner capability *)
let ps_EXEC         = Command (ps_FIRST_COM+3)      (* pro_exec *)
let ps_GETDEF       = Command (ps_FIRST_COM+4)      (* get mmu parameters *)
let ps_GETLOAD      = Command (ps_FIRST_COM+5)      (* get cpu parameters *)

(*
** commands for managing segments
*)
let ps_SEG_CREATE   = Command (ps_FIRST_COM+6)      (* create a segment *)
let ps_SEG_WRITE    = Command (ps_FIRST_COM+7)      (* write a segment  *)
let ps_SEG_READ     = Command (ps_FIRST_COM+8)      (* read a segment   *)
let ps_SEG_SIZE     = Command (ps_FIRST_COM+9)      (* get segment size *)

(*
** Process owner command codes (from kernel to owner).
*)
let ps_CHECKPOINT   = Command (ps_FIRST_COM+10)      (* process checkpoint *)

(*
** Commands to set/get a comment (usually the command line string) 
*)
let ps_SETCOMMENT   = Command (ps_FIRST_COM+11)      
let ps_GETCOMMENT   = Command (ps_FIRST_COM+12)      

(*
** Command to tell an owner its property has a new process capability 
*)
let ps_SWAPPROC     = Command (ps_FIRST_COM+13)      

(*
** New getload command for secure process server 
*)
let ps_SGETLOAD     = Command (ps_FIRST_COM+14)      




(*
** Rights bits in process or segment capabilities.
*)

let  psr_READ        = (Rights_bits 0x01)
let  psr_WRITE       = (Rights_bits 0x02)
let  psr_CREATE      = (Rights_bits 0x04)
let  psr_DELETE      = (Rights_bits 0x08)
let  psr_EXEC        = (Rights_bits 0x10)
let  psr_KILL        = (Rights_bits 0x20)
let  psr_ALL         = (Rights_bits 0xff)


(*
** Segment descriptor.
*)

type segment_d = {
    mutable sd_cap: capability;       (* Capability (if unmapped) *)
    mutable sd_offset: word32;         (* Offset in file *)
    mutable sd_addr: word32;           (* Virtual address *)
    mutable sd_len: word32;            (* Length (bytes) *)
    mutable sd_type: word32;           (* Type bits (see below) *)
}

let nilsd = {sd_cap=nilcap;sd_offset=w32_0;sd_addr=w32_0;sd_len=w32_0;sd_type=w32_0}

(*
** Bits in sd_type (also for seg_map) 
*)

let map_GROWMASK        = word32s "0x0000000f"   (* Growth direction *)
let map_GROWNOT         = word32s "0x00000000"
let map_GROWUP          = word32s "0x00000001"
let map_GROWDOWN        = word32s "0x00000002"

let map_TYPEMASK        = word32s "0x000000f0"  (* Text/data indication *)
let map_TYPETEXT        = word32s "0x00000010"
let map_TYPEDATA        = word32s "0x00000020"

let map_PROTMASK        = word32s "0x00000f00"  (* Read/write indication *)
let map_READONLY        = word32s "0x00000100"
let map_READWRITE       = word32s "0x00000300"

let map_SPECIAL         = word32s "0x0f000000"
let map_INPLACE         = word32s "0x01000000"  (* Map in place, don't copy *)
let map_AND_DESTROY     = word32s "0x02000000"  (* Map, and std_destroy original *)
let map_SHARED          = word32s "0x04000000"        
                                              (* Shared between different 
                                              ** local processes 
                                              *)
let map_BINARY          = word32s "0xf0000000"  (* Flags only in binary file *)
let map_SYSTEM          = word32s "0x80000000"  (* This seg wil recv args *)
let map_SYMTAB          = word32s "0x40000000"  (* Symbol table *)
let map_NEW_SYMTAB      = word32s "0x20000000"  (* New style symbol table *)

(*
** Fault frames - architecture dependent
** Entries in order they appear in the as-is fault frame returned by the
** kernel.
*)
type f_i386 = {
    mutable i386_memmap:    word32;
    mutable i386_gs:        word32;
    mutable i386_fs:        word32;
    mutable i386_es:        word32;
    mutable i386_ds:        word32;
    mutable i386_edi:       word32;
    mutable i386_esi:       word32;
    mutable i386_ebp:       word32;
    mutable i386_faddr:     word32;
    mutable i386_ebx:       word32;
    mutable i386_edx:       word32;
    mutable i386_ecx:       word32;
    mutable i386_eax:       word32;
    mutable i386_trap:      word32;
    mutable i386_errcode:   word32;
    mutable i386_eip:       word32;
    mutable i386_cs:        word32; /* two byte word ! */
    mutable i386_eflag:     word32;
    mutable i386_esp:       word32;
    mutable i386_ss:        word32;
}
type fault_frame = 
    | F_i386 of f_i386

type thread_idle = {
    mutable tdi_pc: word32;      (* pc register *)
    mutable tdi_sp: word32;      (* sp register *)
}

type thread_kstate = {
    mutable tdk_timeout:    word32;
    mutable tdk_sigvec:     word32;
    mutable tdk_svlen:      word32;
    mutable tdk_signal:     word32;
    mutable tdk_local:      word32;
}

type thread_info = 
    | Thread_idle of thread_idle
    | Thread_frame of fault_frame
    | Thread_kstate of thread_kstate
    | Thread_noinfo

(*
** Thread descriptor.
** This is followed by additional data indicated by td_extra.
** Extra data structures are present in the order of ascending bits.
*)

type thread_d = {
    mutable td_state: word32;           (* State *)
    mutable td_len: word32;             (* Total length *)
    mutable td_extra: word32;           (* bitvector of extra stuff *)
    mutable td_info:  thread_info list;
}


let niltd = {td_state=w32_0;td_len=w32_0;td_extra=w32_0;td_info=[]}

(*
** Bits in td_state (some are kernel internals) 
*)
let tds_RUN         = word32s "0x0001"        (* Thread is runnable *)
let tds_DEAD        = word32s "0x0002"        (* (kernel only) thread is busy dying *)
let tds_EXC         = word32s "0x0004"        (* Thread got some sort of trap *)
let tds_INT         = word32s "0x0008"        (* (kernel only) thread got interrupt *)
let tds_FIRST       = word32s "0x0010"        (* Schedule this thread first *)
let tds_NOSYS       = word32s "0x0020"        (* Thread cannot do syscalls *)
let tds_START       = word32s "0x0040"        (* (kernel only) immigrating thread *)
let tds_SRET        = word32s "0x0080"        (* Thread is returning from sig *)
let tds_LSIG        = word32s "0x0100"        (* Thread got lw signal *)
let tds_STUN        = word32s "0x0200"        (* Thread stunned *)
let tds_HSIG        = tds_STUN      (* Compatibility *)
let tds_SHAND       = word32s "0x0400"        (* Thread is calling sighand *)
let tds_DIE         = word32s "0x0800"        (* DIE! *)
let tds_STOP        = word32s "0x1000"        (* Stopped for stun handling *)
let tds_USIG        = word32s "0x2000"        (* Sig should be delivered to user *)
let tds_CONTINUE    = word32s "0x4000"        (* Continue thread after signal handling *)

(*
** Bits in td_extra, indicating presence of extra data structures: 
*)

let tdx_IDLE        = word32s "0x00000001"        (* pc/sp struct follows *)
let tdx_KSTATE      = word32s "0x00000002"        (* kstate struct follows *)
let tdx_USTATE      = word32s "0x00010000"        (* ustate struct follows *)

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
    mutable pd_magic: string;           (* Architecture *)
    mutable pd_self: capability;        (* Process capability (if running) *)
    mutable pd_owner: capability;       (* Default checkpoint recipient *)
    mutable pd_nseg: int;               (* Number of segments *)
    mutable pd_nthread: int;            (* Number of threads *)
    mutable pd_segs: segment_d array;   (* The segments *)
    mutable pd_threads: thread_d array; (* The threads  *)
    (*
    ** Symbol table for internal usage only. Loaded first on exec_pd call...
    *)
    mutable pd_symtab: Symtable.nlist array;    
}

let nilpd = {pd_magic="";pd_self=nilcap;pd_owner=nilcap;
             pd_nseg=0;pd_nthread=0;pd_segs=[||];pd_threads=[||];
             pd_symtab=[||]}
(*
** Checkpoint types in h_extra for PS_CHECKPOINT; detail in h_offset.
*)

let term_NORMAL  = 0        (* Normal termination; detail: exit status *)
let term_STUNNED = 1        (* Process stunned; detail: pro_stun arg *)
let term_EXCEPTION = 2      (* Promoted exception; detail: exception nr. *)


(*
** Known architectures
*)

let arch_SIZE = 8

let arch_list = [|
    "i80386  ",     "pd.i80386" ;
    "mc68000 ",     "pd.mc68000";
    "mipsel  ",     "pd.mipsel" ;
    "mipseb  ",     "pd.mipseb" ;
    "sparc   ",     "pd.sparc"  ;
    "vax     ",     "pd.vax"    ;
    "bytecode",     "pd.bytecode";       (* OCaML of course ! *)   
|]

(*
** Process management options
*)

type p_option =
    | P_loadsym     (* load symbol table from PD *)


(*
** Transaction buffer size used by process/segment server (in bytes)
*)

let psrv_BUFSZ  = 8192      (* max_PDSIZE *)

(*
** Buffer functions
*)

(*
** buf_put_thread_d
**      puts the 3 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)

let buf_put_thread_d ~buf ~pos ~td =
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:td.td_state in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:td.td_len in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:td.td_extra in
    pos

(*
** buf_put_thread_idle
**      puts the 2 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)

let buf_put_thread_idle ~buf ~pos ~td =
    let pos' = ref pos in
    List.iter (fun tdi ->
        match tdi with
        | Thread_idle ti ->
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:ti.tdi_pc in
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:ti.tdi_sp in  
            pos' := pos;
        | _ -> failwith "Programming error"
        ) td.td_info;
    !pos'

(*
** buf_put_thread_kstate
**      puts the 5 longs of this struct into the buffer.
**      buf_put_mach() checks to make sure that they fit.
*)


let buf_put_thread_kstate ~buf ~pos ~td =
    let pos' = ref pos in
    List.iter (fun tdi ->
        match tdi with
        | Thread_kstate tk ->
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:tk.tdk_timeout in
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:tk.tdk_sigvec in
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:tk.tdk_svlen in
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:tk.tdk_signal in
            let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:tk.tdk_local in
            pos' := pos;
        | _ -> failwith "Programming error"
        ) td.td_info;
    !pos'

(*
** buf_put_process_d
**      The reason we test to see if ARCHSIZE bytes fit and not a whole
**      process_d is that we only need to be sure we don't write beyond
**      the buffer ourselves. buf_put_cap(), etc don't write beyond the end
**      of the buffer.
*)
let buf_put_process_d ~buf ~pos ~pd
    =
    let _ = buf_put_string ~buf:buf ~pos:pos ~str:pd.pd_magic in
    let pos = pos + arch_SIZE in    (* fixed size !!! *)
    let pos = buf_put_cap ~buf:buf ~pos:pos ~cap:pd.pd_self in
    let pos = buf_put_cap ~buf:buf ~pos:pos ~cap:pd.pd_owner in
    let pos = buf_put_int16 ~buf:buf ~pos:pos ~int16:pd.pd_nseg in
    let pos = buf_put_int16 ~buf:buf ~pos:pos ~int16:pd.pd_nthread in
    pos

(*
** buf_put_segment_d
**      puts the capability and 4 longs of this struct into the buffer.
**      buf_put_cap() & buf_put_int32() check to make sure that they fit.
*)

let buf_put_segment_d ~buf ~pos ~sd
    =
    let pos = buf_put_cap ~buf:buf ~pos:pos ~cap:sd.sd_cap in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:sd.sd_offset in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:sd.sd_addr in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:sd.sd_len in
    let pos = buf_put_mach ~buf:buf ~pos:pos ~mach:sd.sd_type in
    pos

(*
** buf_put_pd
**      This routine puts a process descriptor plus subsequent segment
**      descriptors and thread descriptors into a buffer.  It does not
**      pack the thread_ustate because this is so machine dependent (it
**      is a fault frame) that anyone who gets it had better know what
**      to do with it themselves.  To avoid alignment problems it is copied
**      byte for byte into the buffer and must be retrieved that way.
**      Like all other buf_put_/buf_get_ routines it returns 0 if the buffer is
**      not large enough.  Otherwise it returns a pointer to the next
**      available byte (after the process descriptor) in the buffer.
**      NB: If the thread descriptors have invalid lengths then we return 0!
*)

let buf_put_pd ~buf ~pos ~pd =
    (*
    ** put the process descriptor struct into the buffer
    *)
    let pos = buf_put_process_d ~buf:buf ~pos:pos 
                                ~pd:pd 
        in

    (*
    ** Put the segment descriptor structs into the buffer
    *)

    let pos = ref pos in
    for i = 0 to pd.pd_nseg-1
    do
        pos := buf_put_segment_d ~buf:buf ~pos:!pos
                                     ~sd:pd.pd_segs.(i);
    done;

    (*
    ** Put the thread descriptor structs into the buffer.
    ** These beasties may be followed by a thread_idle, thread_kstate and/or
    ** thread_ustate so we check to see what is there and put it in the buffer.
    *)

    for i = 0 to pd.pd_nthread-1
    do
        let td = pd.pd_threads.(i) in
        pos := buf_put_thread_d ~buf:buf ~pos:!pos 
                                    ~td:td;
        if (td.td_extra land tdx_IDLE = tdx_IDLE) then
            pos := buf_put_thread_idle ~buf:buf ~pos:!pos
                                       ~td:td;
    done;

    (*
    ** If there is still more thread descriptor left we copy it
    ** as raw bytes since it is probably the thread_ustate which
    ** is a fault frame and we really can't encode it since we don't
    ** know what it is.  The receiver must work it out itself.
    *)
    !pos

(*
** buf_get_process_d
**      This applies a following sanity check to catch random data
**      offered as process descriptor:
**
**              pd_magic must be a string of at most ARCHSIZE-1
**              printable characters padded to ARCHSIZE with zero bytes.
*)

let buf_get_process_d ~buf ~pos =
    let magstr = String.create arch_SIZE in
    Bytebuf.blit_bs ~src:buf ~src_pos:pos 
                    ~dst:magstr ~dst_pos:0 ~len:arch_SIZE;
    let pos = pos + arch_SIZE in
    let pos,self = buf_get_cap ~buf:buf ~pos:pos in
    let pos,owner = buf_get_cap ~buf:buf ~pos:pos in
    let pos,nseg = buf_get_int16 ~buf:buf ~pos:pos in
    let pos,nthr = buf_get_int16 ~buf:buf ~pos:pos in
    pos,{
        pd_magic = magstr;
        pd_self  = self; 
        pd_owner = owner;
        pd_nseg  = nseg;
        pd_nthread = nthr;
        pd_segs = [||];
        pd_threads = [||];
        pd_symtab = [||];
    }

(*
** buf_get_segment_d
**      gets the capability and 4 longs of the segment descriptor struct from
**      the buffer.
**      buf_get_cap() & buf_get_mach() check for the end of the buffer.
*)

let buf_get_segment_d ~buf ~pos =
    let pos,cap = buf_get_cap ~buf:buf ~pos:pos  in
    let pos,off = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,addr = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,len = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,typ = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    pos,{
        sd_cap = cap;
        sd_offset = off;
        sd_addr = addr;
        sd_len = len;
        sd_type = typ;
    }


(*
** buf_get_thread_d
**      gets the 3 longs of the thread descriptor struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)

let buf_get_thread_d ~buf ~pos =
    let pos,st = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,len = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,extra = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    pos,{
        td_state = st;
        td_len = len;
        td_extra = extra;
        td_info = [];
    }

(*
** buf_get_thread_idle
**      gets the 2 longs of the thread_idle struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)
let buf_get_thread_idle ~buf ~pos ~td =
    let pos,pc = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,sp = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let ti = { tdi_pc = pc; tdi_sp = sp; } in
    td.td_info <- td.td_info @ [Thread_idle ti];
    pos

(*
** buf_get_thread_kstate
**      gets the 5 longs of the thread kernel state struct from the buffer.
**      buf_get_mach() checks for the end of the buffer.
*)


let buf_get_thread_kstate ~buf ~pos ~td =
    let pos,tm = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,sv = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,sl = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,si = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let pos,lo = buf_get_mach ~buf:buf ~pos:pos ~mach:Word32 in
    let tk = { tdk_timeout=tm;
               tdk_sigvec=sv;
               tdk_svlen=sl;
               tdk_signal=si;
               tdk_local=lo } in
    td.td_info <- td.td_info @ [Thread_kstate tk];
    pos


let buf_get_thread_ustate ~buf ~pos ~len ~pd ~td =
    let fp = String.create len in
    blit_bs ~src:buf ~src_pos:pos
            ~dst:fp ~dst_pos:0 ~len:len;
    (*
    ** This is fault frame. Machine dependent. Try to
    ** get the pc and sp values from this frame.
    *)
    begin
        match pd.pd_magic with
        | "i80386" | "i80386\000\000" ->
        begin
                    let get_word s p =
                        let s1 = word32 (int_of_char fp.[p]) in
                        let s2 = word32 (int_of_char fp.[p+1]) in
                        let s3 = word32 (int_of_char fp.[p+2]) in
                        let s4 = word32 (int_of_char fp.[p+3]) in
                        (   s1 lor
                            (s2 lsl 8) lor
                            (s3 lsl 16) lor
                            (s4 lsl 24));
                        in
                    (*
                    ** machdep/arch/i80386/fault.h !
                    *)
                    let fp_memmap = 0 in
                    let fp_gs = 1*int32_SIZE in
                    let fp_fs = 2*int32_SIZE in
                    let fp_es = 3*int32_SIZE in
                    let fp_ds = 4*int32_SIZE in
                    let fp_edi = 5*int32_SIZE in
                    let fp_esi = 6*int32_SIZE in
                    let fp_ebp = 7*int32_SIZE in
                    let fp_faddr = 8*int32_SIZE in  /* or esp */
                    let fp_ebx = 9*int32_SIZE in
                    let fp_edx = 10*int32_SIZE in
                    let fp_ecx = 11*int32_SIZE in
                    let fp_eax = 12*int32_SIZE in
                    let fp_trap = 13*int32_SIZE in
                    let fp_errcode = 14*int32_SIZE in
                    let fp_eip = 15*int32_SIZE in
                    let fp_cs = 16*int32_SIZE in
                    let fp_eflag = 17*int32_SIZE in
                    let fp_esp = 18*int32_SIZE in
                    let fp_ss = 19*int32_SIZE in


                    let ff = {
                            i386_memmap=get_word fp fp_memmap;
                            i386_gs=get_word fp fp_gs;
                            i386_fs=get_word fp fp_fs;
                            i386_es=get_word fp fp_es;
                            i386_ds=get_word fp fp_ds;
                            i386_edi=get_word fp fp_edi;
                            i386_esi=get_word fp fp_esi;
                            i386_ebp=get_word fp fp_ebp;
                            i386_faddr=get_word fp fp_faddr;
                            i386_ebx=get_word fp fp_ebx;
                            i386_edx=get_word fp fp_edx;
                            i386_ecx=get_word fp fp_ecx;
                            i386_eax=get_word fp fp_eax;
                            i386_trap=get_word fp fp_trap;
                            i386_errcode=get_word fp fp_errcode;
                            i386_eip=get_word fp fp_eip;
                            i386_cs=get_word fp fp_cs;
                            i386_eflag=get_word fp fp_eflag;
                            i386_esp=get_word fp fp_esp;
                            i386_ss=get_word fp fp_ss;

                        } in

                    td.td_info <- td.td_info @ [Thread_frame (F_i386 ff)];
        end;
        | _ -> (); 
    end;
    (pos+len)


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

let buf_get_pd ~buf ~pos =
    (*
    ** get the process descriptor struct from the buffer
    *)
    let pos,pd = buf_get_process_d ~buf:buf ~pos:0 in

    (*
    ** get the segment descriptor structs from the buffer
    *)
    let pos = ref pos in
    pd.pd_segs <- Array.create pd.pd_nseg nilsd;
    for i = 0 to pd.pd_nseg-1
    do
        let pos',sd = buf_get_segment_d ~buf:buf ~pos:!pos
            in
        pd.pd_segs.(i) <- sd;
        pos := pos';
    done;

    (*
    ** Try to get as much thread descriptor structs from the buffer as possible.
    ** These beasties may be followed by a thread_idle, thread_kstate and/or
    ** thread_ustate so we have to get them as well.
    *)

    pd.pd_threads <- Array.create pd.pd_nthread niltd;

    for i = 0 to pd.pd_nthread-1
    do
        let start = !pos in

        let pos',td = buf_get_thread_d ~buf:buf ~pos:!pos in
        pos := pos';

        pd.pd_threads.(i) <- td;

        if (td.td_extra land tdx_IDLE = tdx_IDLE) then
        begin
            let pos' = buf_get_thread_idle ~buf:buf ~pos:!pos ~td:td in
            pos := pos';
        end;        

        if (td.td_extra land tdx_KSTATE = tdx_KSTATE) then
        begin
            let pos' = buf_get_thread_kstate ~buf:buf ~pos:!pos ~td:td in
            pos := pos';
        end;       

        if (td.td_extra land tdx_USTATE = tdx_USTATE) then
        begin
            let ustate_len = start + (to_int td.td_len) - !pos in
            let pos' = buf_get_thread_ustate ~buf:buf ~pos:!pos
                                             ~len:ustate_len ~pd:pd ~td:td in

            pos := start + (to_int td.td_len);
        end;       
    done;
    !pos,pd


(*
** This size should hold a fresh process descriptor...
*)
let pd_SIZE = 10000

let pd_size pd =
    0
(*
** pd_patch()
**
** Loop over the segment descriptors, patching those that
** must point into the executable file.
** They are recognizable by a nonzero offset and a zero port.
*)

let pd_patch ~cap ~pd =
    for i = 0 to pd.pd_nseg-1
    do
        let sd = pd.pd_segs.(i) in
        if (sd.sd_offset <> w32_0 &&
            nullport(sd.sd_cap.cap_port) = true) then
            sd.sd_cap <- cap;
    done

  
    
(*
** pd_read 
**
**      Read a process descriptor from a file specified with 'cap', 
**      and convert it to host
**      byte order.  (The pd was stored in network byte order by ainstall.)
**      Returns the status and a fresh process descriptor pd.
*)

let pd_read ~cap =
    try
    begin
        (*
        ** should be dynamically adjusted ..
        *)
        let buf_size = pd_SIZE in
        let buf = buf_create buf_size in
        let err,n = afs_read ~cap:cap
                             ~offset:0
                             ~buf:buf
                             ~size:buf_size
            in
        if (err <> std_OK) then
            raise (Error err);

        let pos,pd = buf_get_pd ~buf:buf ~pos:0 
            in
        pd_patch cap pd;
        std_OK,pd
        end
    with
        | Error err -> err,nilpd;
        | _ -> std_IOERR,nilpd


(*
** Try to print a stacktrace from a fault frame 
** (compiler dependent - currently only GCC-2.9 is supported!)
*)

let print_stacktrace_str pd tf =
    let str' = ref "" in
    let out str = str' := !str' ^ str in
    let nl () = str' := !str' ^ "\n" in
    let symtab = pd.pd_symtab in

    let buf = buf_create 8000 in
    let seg_info addr =
        let is_text = ref false in
        let stat = ref std_NOTFOUND in
        let n = ref 0 in
        try
        begin
            (*
            ** Find the segment holding address addr
            *)
            for i = 0 to pd.pd_nseg-1
            do
                let sd = pd.pd_segs.(i) in
                if (addr >= sd.sd_addr && 
                    addr <  sd.sd_addr + sd.sd_len) then
                begin
                    (*
                    ** Either an AFS or PROC server object -
                    ** nonetheless the same access method
                    *)
                    let off = addr - sd.sd_addr in
                    let stat',n' = afs_size ~cap:sd.sd_cap
                        in
                    
                    stat := stat';
                    n := n';
                    if (sd.sd_type land map_TYPETEXT) = map_TYPETEXT then
                        is_text := true;
                    raise Exit;
                end;
            done;
            !stat,(word32 !n), !is_text
        end
        with | Exit -> !stat,(word32 !n), !is_text
        in
    
    let seg_read addr len =
        let is_text = ref false in
        let stat = ref std_NOTFOUND in
        let n = ref 0 in
        try
        begin
            (*
            ** Find the segment holding address addr
            *)
            for i = 0 to pd.pd_nseg-1
            do
                let sd = pd.pd_segs.(i) in
                if (addr >= sd.sd_addr && 
                    addr+len < sd.sd_addr+sd.sd_len) then
                begin
                    (*
                    ** Either an AFS or PROC server object -
                    ** nonetheless the same access method
                    *)
                    let off = addr - sd.sd_addr in
                    let stat',n' = afs_read ~cap:sd.sd_cap
                                            ~offset:(to_int off)
                                            ~buf:buf
                                            ~size:(to_int len)
                        in
                    stat := stat';
                    n := n';
                    if (sd.sd_type land map_TYPETEXT) = map_TYPETEXT then
                        is_text := true;
                    raise Exit;
                end;
            done;
            !stat,!n,!is_text
        end
        with | Exit -> !stat,!n,!is_text
        in
    try (
    match tf with
    | F_i386 ff ->
    begin
        out (" Stacktrace [ESP] EIP:"); nl();
        let ebp = ff.i386_ebp in
        let esp = ff.i386_esp in
        let eip = ff.i386_eip in
        let w0 = word32 0 in
        let w4 = word32 4 in
        let w8 = word32 8 in
        let w80 = word32 80 in
        let w160 = word32 160 in

        let print_frame ebp =
            let nextframe = ref ebp in
            let stat,stacksize,is_text = seg_info ebp in
            if (stat <> std_OK) then raise (Error stat);
            while (!nextframe >= ebp &&
               !nextframe < ebp + stacksize)
            do
                let ssize = (word32 (2*int32_SIZE)) in
                let stat,n,is_text = seg_read !nextframe ssize in
                if (stat <> std_OK) then raise (Error stat);
                if ((word32 n) <> ssize) then raise (Error std_NOSPACE);
                let _,sp = buf_get_mach buf 0 Word32 in 
                let _,pc = buf_get_mach buf 4 Word32 in

                out (sprintf "        [%10s] %10s (%s%s:%s)"
                     (to_str sp) (to_str pc) 
                     (find_sym_file symtab pc)
                     (let s = find_sym_line symtab pc in 
                          if s <> "" then (" "^s) else "")
                     (find_sym_name symtab pc)
                    ); nl ();
                nextframe := sp;
            done;
            in

        if ebp <> w0 then
        begin
            (*
            ** GNUC: EBP points to last stackframe [push ebp]
            *)
            print_frame ebp;
        end
        else
        begin
            (* 
            ** If ebp is 0, the thread is probably blocked in a system call.
            ** We need a valid ebp to get a traceback, however.  We can try
            ** to find it using esp: it should be close to the top of the stack.
            ** We skip the first two words (one containing _thread_local)
            ** and start looking for a frame pointer, using some guesswork.
            *)
            (*
            ** Check for a valid stack frame chain. The last valid one entry 
            ** must have at least ESP (pointing to next frame) = 0!
            ** EIP must be 0 for the main thread, too.
            *)
            let check_stack esp epc =

                let nextframe = ref esp in
                let stat,stacksize,is_text = seg_info esp in
                if (stat <> std_OK) then raise (Error stat);
                try
                (
                if epc <> w0 then
                begin
                    (*
                    ** Valid address ??
                    *)
                    let stat,_,is_text = seg_info epc in
                    if stat <> std_OK then
                        raise (Error stat);
                    if not is_text then 
                        raise Not_found;    (* invalid pc !!! *)
                end;                    
                while (!nextframe >= esp &&
                       !nextframe < esp + stacksize)
                do
                    let ssize = (word32 (2*int32_SIZE)) in
                    let stat,n,is_text = seg_read !nextframe ssize in
                    if (stat <> std_OK) then raise (Error stat);
                    if ((word32 n) <> ssize) then raise (Error std_NOSPACE);
                    let _,sp = buf_get_mach buf 0 Word32 in 
                    let _,pc = buf_get_mach buf 4 Word32 in
                    if pc <> w0 then
                    begin
                        (*
                        ** Valid address ??
                        *)
                        let stat,_,is_text = seg_info pc in
                        if stat <> std_OK then
                            raise (Error stat); (* invalid pc !!! *)
                        if sp = w0 && is_text then
                            raise Exit;
                        if not is_text then 
                            raise Not_found;    (* invalid pc !!! *)

                        nextframe := sp;                        
                    end
                    else
                    begin
                        if sp = w0 then
                            raise Exit;
                        nextframe := sp;
                    end;
                done;
                false
                )
                with
                    | Exit -> true;     (* must be a valid stack frame entry *)
                    | _ -> false;
                in
        

            out ("  (searching stackframe...)"); nl();

            let addr = ref (esp + w8) in
            let esp80 = esp + w80 in
            let nebp = ref w0 in

            let stat,stacksize,is_text = seg_info !addr in
            if (stat <> std_OK) then raise (Error stat);
            let stat,n,is_text = seg_read !addr w80 in
            if (stat <> std_OK) then raise (Error stat);



            let pos = ref 0 in
            protect (while (!addr < esp80)
            do
                let pos',tryit = buf_get_mach buf !pos Word32 in
                addr := !addr + w4;
                pos := pos';

                if tryit > esp && tryit < (esp + w160 + w8) then
                begin
                    (* 
                    ** this looks like a frame pointer 
                    *)
                    let _,pc = buf_get_mach buf !pos Word32 in
                    let found = check_stack tryit pc in
                    if found then
                    begin
                        nebp := tryit;
                        out (sprintf "        [%10s] %10s (%s%s:%s)"
                            (to_str tryit) (to_str pc) 
                            (find_sym_file symtab pc)
                            (let s = find_sym_line symtab pc in 
                                if s <> "" then (" "^s) else "")
                            (find_sym_name symtab pc)
                           ); nl ();
                        raise Exit; 
                    end;
                end;
                
            done);
            if !nebp <> w0 then
                print_frame (!nebp)
            else
                raise (Error std_NOTFOUND);
        end;
        !str'
    end ) with | Error stat -> 
        out ("        failed: "^(err_why stat)); nl(); !str'

(*
** Print information about a segment descriptor
*)
let print_sd_str sd =
    let str' = ref "" in
    let out str = str' := !str' ^ str in
    let nl () = str' := !str' ^ "\n" in

    out "Segment "; nl ();
        let t = sd.sd_type in
    out (sprintf 
            " sd_cap=%s\n sd_type=%s\n sd_offset=%10s    sd_addr=%10s    sd_len=%10s"
                        (ar_cap sd.sd_cap)
                        (
                          (if t land map_TYPETEXT <> w32_0 then "TEXT " else "")^
                          (if t land map_TYPEDATA <> w32_0 then "DATA " else "")^
                          (if t land map_READONLY <> w32_0 &&
                              t land map_READWRITE = w32_0 then "RD " else "")^
                          (if t land map_READWRITE <> w32_0 then "RDWR " else "")^
                          (if t land map_GROWUP <> w32_0 then "GROWUP " else "")^
                          (if t land map_GROWDOWN <> w32_0 then "GROWDOWN " else "")^
                          (if t land map_INPLACE <> w32_0 then "INPL " else "")^
                          (if t land map_AND_DESTROY <> w32_0 then "DESTR " else "")^
                          (if t land map_SHARED <> w32_0 then "SHRD " else "")^
                          (if t land map_SYSTEM <> w32_0 then "SYS " else "")^
                          (if t land map_SYMTAB <> w32_0 then "SYM " else ""))
                        (to_str sd.sd_offset)  
                        (to_str sd.sd_addr)
                        (to_str  sd.sd_len));

    nl ();
    !str'

    
(*
** Print the content of a process descriptor:
**
**  Segments
**  Threads
**  Fault frames
**  Stack traces (if segments still available)
*)

let print_pd_str pd =
    let str' = ref "" in
    let out str = str' := !str' ^ str in
    let nl () = str' := !str' ^ "\n" in
    let symtab = pd.pd_symtab in

    (* print_symtab symtab; *)

    out "Process descriptor"; nl ();
    out ("Magic: "^pd.pd_magic); nl ();    
    out ("Process capabilities:  [Self]="^(ar_cap pd.pd_self)); 
    nl ();
    out ("                       [Owner]="^(ar_cap pd.pd_owner)); 
    nl ();
    out ("Number of Threads: "^(string_of_int pd.pd_nthread)^
                  "          Number of Segments: "^(string_of_int pd.pd_nseg));
    nl ();
    out "Segments:"; nl ();
    for i = 0 to pd.pd_nseg-1
    do
        let sd = pd.pd_segs.(i) in
        let t = sd.sd_type in
        out (sprintf 
             "%2d. sd_cap=%s\n    sd_type=%s\n    sd_offset=%10s    sd_addr=%10s    sd_len=%10s"
                        (i+1)
                        (ar_cap sd.sd_cap)
                        (
                          (if t land map_TYPETEXT <> w32_0 then "TEXT " else "")^
                          (if t land  map_TYPEDATA <> w32_0 then "DATA " else "")^
                          (if t land  map_READONLY <> w32_0 &&
                              t land  map_READWRITE = w32_0 then "RD " else "")^
                          (if t land map_READWRITE <> w32_0 then "RDWR " else "")^
                          (if t land map_GROWUP <> w32_0 then "GROWUP " else "")^
                          (if t land map_GROWDOWN <> w32_0 then "GROWDOWN " else "")^
                          (if t land map_INPLACE <> w32_0 then "INPL " else "")^
                          (if t land map_AND_DESTROY <> w32_0 then "DESTR " else "")^
                          (if t land map_SHARED <> w32_0 then "SHRD " else "")^
                          (if t land map_SYSTEM <> w32_0 then "SYS " else "")^
                          (if t land map_SYMTAB <> w32_0 then "SYM " else ""))
                        (to_str sd.sd_offset)
                        (to_str sd.sd_addr)
                        (to_str sd.sd_len));

        nl ();
    done;
    out "Threads: "; nl ();
    for i = 0 to pd.pd_nthread-1
    do
        out (sprintf "Thread %d:" i); nl();
        let td = pd.pd_threads.(i) in
        let s = td.td_state in
        List.iter ( fun tdi ->
          match tdi with
          | Thread_idle ti ->
            out (sprintf 
                      "%2d.   pc=%10s    sp=%10s    state=%s"
                        (i+1) 
                        (to_str ti.tdi_pc) 
                        (to_str ti.tdi_sp)
                        (
                          (if s land tds_RUN <> w32_0 then "RUN " else "")^
                          (if s land tds_DEAD <> w32_0 then "DEAD " else "")^
                          (if s land tds_EXC <> w32_0 then "EXC " else "")^
                          (if s land tds_INT <> w32_0 then "INT " else "")^
                          (if s land tds_FIRST <> w32_0 then "FIRST " else "")^
                          (if s land tds_NOSYS <> w32_0 then "NOSYS " else "")^
                          (if s land tds_START <> w32_0 then "START " else "")^
                          (if s land tds_SRET <> w32_0 then "SIGRET " else "")^
                          (if s land tds_LSIG <> w32_0 then "LWSIG " else "")^
                          (if s land tds_SHAND <> w32_0 then "SIGHAND " else "")^
                          (if s land tds_DIE <> w32_0 then "DIE " else "")^
                          (if s land tds_STOP <> w32_0 then "STOP " else "")^
                          (if s land tds_USIG <> w32_0 then "USRSIG " else "")^
                          (if s land tds_CONTINUE <> w32_0 then "CONT " else "")^
                          (if s land tds_STUN <> w32_0 then "STUN " else ""))
                    );
            nl ();
          | Thread_frame tf ->
          begin
            out (" Fault frame:"); nl();
            match tf with
            | F_i386 ff ->
            begin
                out (sprintf 
                 "   CS= %10s  SS= %10s  EIP=%10s  ESP=%s"
                 (to_str ff.i386_cs)
                 (to_str ff.i386_ss)
                 (to_str ff.i386_eip)
                 (to_str ff.i386_esp)); nl();
                out (sprintf
                 "   GS= %10s  FS= %10s  ES= %10s  DS=%s"
                 (to_str ff.i386_gs)
                 (to_str ff.i386_fs)
                 (to_str ff.i386_es)
                 (to_str ff.i386_ds)); nl();
                out (sprintf 
                 "   EDI=%10s  ESI=%10s  EBP=%10s  FADDR=%s"
                 (to_str ff.i386_edi)
                 (to_str ff.i386_esi)
                 (to_str ff.i386_ebp)
                 (to_str ff.i386_faddr)); nl();
                out (sprintf 
                 "   EAX=%10s  EBX=%10s  ECX=%10s  EDX=%s"
                 (to_str ff.i386_eax)
                 (to_str ff.i386_ebx)
                 (to_str ff.i386_ecx)
                 (to_str ff.i386_edx)); nl();
                out (sprintf 
                 "   ERR=%10s EFLAG=%10s CR3=%10s  TRAP=%s"
                 (to_str ff.i386_errcode)
                 (to_str ff.i386_eflag)
                 (to_str ff.i386_memmap)
                 (to_str ff.i386_trap)); nl();
                (*
                ** Try to extract a stack trace
                *)
                out (sprintf "        [Current IP] %10s (%s%s:%s)"
                        (to_str ff.i386_eip)
                        (find_sym_file symtab ff.i386_eip)
                        (let s = find_sym_line symtab ff.i386_eip in 
                                if s <> "" then (" "^s) else "")
                        (find_sym_name symtab ff.i386_eip)
                    ); nl ();
                out (print_stacktrace_str pd tf);
            end;
          end;
          | Thread_kstate tk ->
            out (" Kernel state:"); nl();
          | Thread_noinfo -> ();
          ) td.td_info;
    done;
    !str'


(*
** Output to stdout channel
*)
let print_stacktrace pd ft = 
    out (print_stacktrace_str pd ft);
    nl ()

let print_pd pd = 
    out (print_pd_str pd);
    nl ()

let print_sd sd = 
    out (print_sd_str sd);
    nl ()


(*
** pro_exec - Execute a process.
**      This is a very low-level interface.  It gives a process descriptor to
**      the process/segment server on the machine specified by 'host' which
**      starts the process if all is well.
*)
    
let pro_exec ~host
             ~pd
    =
    let buf = buf_create pd_SIZE in
    let pos = buf_put_pd ~buf:buf ~pos:0 ~pd in
    let hdr_req = {(header_new ()) with
                    h_port = host.cap_port;
                    h_priv = host.cap_priv;
                    h_size = pos;
                    h_command = ps_EXEC;
                } in
    let (stat,n,hdr_rep) = trans (hdr_req,buf,pos,nilbuf,0) in
    if (stat <> std_OK) then
        stat,nilcap
    else if (hdr_rep.h_status <> std_OK) then
        hdr_rep.h_status,nilcap
    else
    begin
        std_OK,{
            cap_port = hdr_rep.h_port;
            cap_priv = hdr_rep.h_priv;
        }
    end

let def_STACKSIZE = (16*1024)       (* Default stack size *)


(*
** Create a stack segment on the AFS server and fill in the
** environment!
*)

external buildstack : Bytebuf.buffer *
                      word32 *
                      string array *
                      string array *
                      (string * string) array   (* name,cap *)
                     -> word32
    = "buildstack_ext"

let stack_create ~server 
                 ~sd 
                 ~size      
                 ~args 
                 ~strenv 
                 ~capenv =
    if (server = nilcap) then
        raise (Error std_CAPBAD);
    
    (*
    ** Size must be host page aligned!!!
    *)
    let stack = buf_create size in

    (*
    ** Build the stack - very low level - too much for here !
    ** Use the external buildstack function from the amunix lib.
    *)

    let stackstart = sd.sd_addr in
    let stackend = sd.sd_addr + (word32 size) in
    
    (*
    ** Store the capabilities in strings ...
    *)
    let capenv' = List.map (fun ce ->
                    let name,cap = ce in
                    let buf = buf_create cap_SIZE in
                    let _ = buf_put_cap ~buf:buf ~pos:0 ~cap:cap in
                    name,(string_of_buf buf)
                  ) capenv 
        in

    let sp =   buildstack (stack,
                             stackstart,
                             (Array.of_list args),
                             (Array.of_list strenv),
                             (Array.of_list capenv')) in

    let stat,scap = afs_create ~cap:server
                               ~buf:stack
                               ~size:size
                               ~commit:0    (* only TEMP *)
        in
    if (stat <> std_OK) then
        raise (Error stat);

    (* 
    ** Fill in the segment descriptor and the stack pointer in the
    ** first thread descriptor (in fact, we assume there is only one
    ** thread).  Because some programs leave the flags for the stack segment
    ** zero, we set them here.  We must set MAP_AND_DESTROY anyway. 
    *)

    sd.sd_cap   <- scap;
    sd.sd_addr  <- stackstart ;
    sd.sd_len   <- word32 size;
    sd.sd_type  <- map_GROWDOWN lor 
                   map_TYPEDATA  lor 
                   map_READWRITE lor
                   map_AND_DESTROY;
    stat,scap,sp


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
**  options: proc_option list
**
** Returns:
**
**  status
**  retcap: capability          /* Out: capabilty for running process */
**
*)

let exec_pd ~pd
            ~host
            ~owner
            ~stacksize
            ~args
            ~strenv
            ~capenv
            ~opt
    =
    try
    begin
        if (capenv = []) then
        begin
            out "exec_pd: empty cap environment";nl();
            raise (Error std_ARGBAD);
        end;

        (*
        ** Find the slot for the stack segment.
        ** Heuristic: must have null port, and flags must either have
        ** the MAP_SYSTEM bit set (which is the 'right' way to do it),
        ** or grow down and have type data.
        *)
        let fsrv = ref nilcap in            (* File server *)
        let stacksd = ref nilsd in          (* Stack segment desc *)
        let stackcap = ref nilcap in        (* Stack segment cap *)
        let stacksp = ref w32_0 in


        for i = 0 to pd.pd_nseg-1
        do
            let sd = pd.pd_segs.(i) in  

            (*
            ** Load symbol table if desired...
            *)
            if (List.mem P_loadsym opt) &&
               (sd.sd_type land map_SYMTAB) = map_SYMTAB then
            begin
                let off = to_int sd.sd_offset in
                let len = to_int sd.sd_len in
                let cap = sd.sd_cap in
                let buf = buf_create len in
                let err,n = afs_read ~cap:cap
                                     ~offset:off
                                     ~buf:buf
                                     ~size:len in
                if err <> std_OK then raise (Error err);
                let failed = not (protects (
                    let _,symtab,strtab = buf_get_symtab buf 0 in
                    pd.pd_symtab <- symtab;
                    resolve_symtab pd.pd_symtab strtab;
                    )) in
                if failed then raise (Error std_SYSERR);
            end;

            if  (nullport(sd.sd_cap.cap_port) = true) &&
                ((sd.sd_type land map_SYSTEM = map_SYSTEM) ||
                 (sd.sd_type land map_GROWDOWN = map_GROWDOWN) &&
                 (sd.sd_type land map_TYPEDATA = map_TYPEDATA)) then
            begin
                (*
                ** Create the stack segment on the AFS server and
                ** fill in the environment!
                *)
                stacksd := sd;
                let stat,cap,sp = stack_create ~server:!fsrv
                                       ~sd:sd 
                                       ~size:(if sd.sd_len = w32_0 then
                                                def_STACKSIZE 
                                              else
                                                (to_int sd.sd_len))
                                       ~args:args
                                       ~strenv:strenv
                                       ~capenv:capenv in
                if (stat <> std_OK) then
                begin
                    out "exec_pd: stack build failed";nl();
                    raise (Error stat);
                end;
                stackcap := cap;
                stacksp := sp;
            end
            else if (nullport(sd.sd_cap.cap_port) = false &&
                     nullport(!fsrv.cap_port) = true) then
                    fsrv := sd.sd_cap;
        done;        
        if (!stacksd = nilsd) then
        begin
            out "exec_pd: no stack segment found"; nl ();
            raise (Error std_SYSERR);
        end;
        if (!stackcap = nilcap) then
        begin
            out "exec_pd: empty stack capability"; nl ();
            raise (Error std_IOERR);
        end;
        (*
        ** Set the default process-signal owner
        *)

        pd.pd_owner <- owner;

        (*
        ** Adjust the thread stackpointer...
        *)

        if (Array.length pd.pd_threads) = 0 then
        begin
            out "exec_pd: no threads in pd"; nl ();
            raise (Error std_ARGBAD);
        end;
        List.iter (fun tdi ->
            match tdi with
            | Thread_idle ti ->
                    ti.tdi_sp <- !stacksp; 
            | _ -> failwith "exec_pd: no thread idle info found";
            ) pd.pd_threads.(0).td_info;

        (*
        ** Try to exec the pd...
        *)

        let stat,pcap = pro_exec host pd in
        if (stat <> std_OK) then
        begin
            out "exec_pd: pro_exec failed"; nl ();
            (*
            ** Destroy the stack segment 
            *)
            let _ = std_destroy !stackcap in
            raise (Error stat);
        end;

        std_OK,pcap
    end
    with
        | Error err -> err,nilcap




(*
** Send a process signal to the process specified by 'cap'.
*)

let pro_stun ~cap ~signal =
    let h = header_new () in
    h.h_port <- cap.cap_port;
    h.h_priv <- cap.cap_priv;
    h.h_offset <- signal;
    h.h_command <- ps_STUN;
    let stat,n,h' = trans (h,nilbuf,0,nilbuf,0) in
    if (stat <> std_OK ) then
        stat
    else
        h'.h_status

(*
** Process info
*)

let pro_setcomment ~cap ~str =
    let h = header_new () in
    h.h_port <- cap.cap_port;
    h.h_priv <- cap.cap_priv;
    h.h_command <- ps_SETCOMMENT;
    let buf = buf_of_string str in
    let buflen = buf_len buf in
    let stat,n,h' = trans (h,buf,buflen,nilbuf,0) in
    if (stat <> std_OK ) then
        stat
    else
        h'.h_status

let pro_getcomment ~cap =
    let h = header_new () in
    h.h_port <- cap.cap_port;
    h.h_priv <- cap.cap_priv;
    h.h_command <- ps_GETCOMMENT;
    let buflen = 1000 in
    let buf = buf_create buflen in
    let stat,n,h' = trans (h,nilbuf,0,buf,buflen) in
    if (stat <> std_OK ) then
        stat,""
    else if (h'.h_status = std_OK) then
        std_OK,(buf_tostring buf 0 n)
    else
        h'.h_status,""


(*
** pro_setowner
**      Set the owner of a process to be someone else.
*)
let pro_setowner ~cap ~owner =
    let h = header_new () in
    h.h_port <- cap.cap_port;
    h.h_priv <- cap.cap_priv;
    h.h_command <- ps_SETOWNER;
    let buflen = 2 * cap_SIZE in
    let buf = buf_create buflen in
    __(buf_put_cap buf 0 owner);
    let stat,n,h' = trans (h,buf,buflen,nilbuf,0) in
    if (stat <> std_OK ) then
        stat
    else
        h'.h_status

(*
** pro_getowner
**      Find the capability of the owner of a particular process.
*)

let pro_getowner ~cap =
    let h = header_new () in
    h.h_port <- cap.cap_port;
    h.h_priv <- cap.cap_priv;
    h.h_command <- ps_GETOWNER;
    let stat,n,h' = trans (h,nilbuf,0,nilbuf,0) in
    if (stat <> std_OK ) then
        stat,nilcap
    else if (h'.h_status = std_OK) then 
    begin
        let cap = {
                cap_port = h'.h_port;
                cap_priv = h'.h_priv;
            } in
        std_OK,cap
    end
    else
        h'.h_status,nilcap
 
(*
** Write 'size' bytes of a process memory segment specified with 'segcap' 
** starting at offset 'off' from the buffer 'buf'.
*)

let ps_segwrite ~segcap ~off ~buf ~size =
    let stat = ref std_SYSERR in
    let size = ref size in
    let off = ref off in
    let boff = ref 0 in
    protect (
        let hdr = header_new () in
        while !size > 0 
        do
            hdr.h_port <- segcap.cap_port;
            hdr.h_priv <- segcap.cap_priv;
            hdr.h_command <- ps_SEG_WRITE;
            hdr.h_offset <- !off;
            hdr.h_size <- min !size psrv_BUFSZ; 
            let stat',n,hdr' = transo (hdr,buf,!boff,hdr.h_size,nilbuf,0,0) in
            if stat' <> std_OK then
            begin
                stat := stat';
                raise Exit;
            end;
            off := !off + hdr.h_size;
            size := !size - hdr.h_size;
            boff := !boff + hdr.h_size;
        done;   
        stat := std_OK;
        );
    !stat

(*
** Read 'size' bytes of a process memory segment specified with 'segcap' 
** starting at offset 'off' into the buffer 'buf'.
*)

let ps_segread ~segcap ~off ~buf ~size =
    let stat = ref std_SYSERR in
    let size = ref size in
    let off = ref off in
    let boff = ref 0 in
    protect (
        let hdr = header_new () in
        while !size > 0 
        do
            hdr.h_port <- segcap.cap_port;
            hdr.h_priv <- segcap.cap_priv;
            hdr.h_command <- ps_SEG_READ;
            hdr.h_offset <- !off;
            hdr.h_size <- min !size psrv_BUFSZ; 
            let stat',n,hdr' = transo (hdr,nilbuf,0,0,buf,!boff,hdr.h_size) in
            if stat' <> std_OK then
            begin
                stat := stat';
                raise Exit;
            end;
            off := !off + hdr.h_size;
            size := !size - hdr.h_size;
            boff := !boff + hdr.h_size;
        done;   
        stat := std_OK;
        );
    !stat

(*
** ps_segcreate - create a memory segment
**      Creates a memory segment on the machine specified by 'host'.  If
**      'clone' is not the null-cap then the segment created is a copy
**      of the segment specified by 'clone'.  ('clone' may well be an AFS
**      file capability.)
*)

let ps_segcreate ~srv ~size ~clone =
    let bufsize = 2* cap_SIZE in
    let buf = if clone <> nilcap then buf_create bufsize else nilbuf in
    let hdr = header_new () in
    hdr.h_port <- srv.cap_port;
    hdr.h_priv <- srv.cap_priv;
    hdr.h_command <- ps_SEG_CREATE;
    hdr.h_offset <- size;
    if clone <> nilcap then
        __(buf_put_cap buf 0 clone);        
    
    let stat,n,hdr' = trans (hdr,buf,bufsize,nilbuf,0) in
    if stat = std_OK then
        std_OK,{
            cap_port = hdr'.h_port;
            cap_priv = hdr'.h_priv;
        }
    else
        stat,nilcap



