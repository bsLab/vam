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
**    $VERSION:     1.04
**
**    $INFO:
**
** Circular buffer package.
**
** Circular buffers are used to transfer a stream of bytes between a
** reader and a writer, usually in different threads.  The stream is
** ended after the writer closes the stream; when the reader has read
** the last byte, the next read call returns an end indicator.  Flow
** control is simple: the reader will block when no data is immediately
** available, and the writer will block when no buffer space is
** immediately available.
**
** This package directly supports concurrent access by multiple readers
** and/or writers.
**
**
**
**    $ENDOFINFO
**
*)



open Amoeba
open Bytebuf
open Mutex
open Sema

type circular_buf = 
{
    empty:   semaphore; (* locked if no data in buffer *)
    full:   semaphore; (* locked if no free space in buffer *)       
    lock:   Mutex.t;    (* serializes access to other members *)

    (*
    ** Sema empty is used to block until at least one byte of data
    ** becomes available.  Similarly, you can block on full until at
    ** least one free byte becomes available in the buffer.
    ** To prevent deadlock, always acquire full or empty before lock,
    ** and never acquire full and empty together.
    ** When closed, full and empty should always be unlocked.
    *)
    
    cbuf:   buffer;

    (*
    ** Data is between out and in, and wraps from last to first.
    ** When in == out, use nbytes to see if the buffer is full or empty.
    *)

    mutable in_pos:     int;
    mutable out_pos:    int;
    last_pos:           int;

    mutable nbytes: int;
    mutable getpcount: int;
    mutable putpcount: int;

    mutable closed: bool;   (* when set, no new data is accepted *)
}

(*
** nil cb structure, needed for references on cb's
*)

val nilcb: circular_buf

(*
** cb_create -- allocate a new circular buffer of a given size.
*)

val cb_create: size:int -> circular_buf

(*
** cb_close -- set closed flag.
** May be called as often as you want, by readers and writers.
** Once closed, no new data can be pushed into the buffer,
** but data already in it is still available to readers.
*)

val cb_close: circular_buf -> unit


(*
** cb_full -- return number of available data bytes.
** When closed and there are no bytes available, return -1.
*)

val cb_full: circular_buf -> int


(*
** cb_empty -- return number of available free bytes.
** Return -1 if closed (this can be used as a test for closedness).
*)


val cb_empty: circular_buf -> int


(*
** CB put and get functions: char and string version
*)

(*
** cb_putc -- put char into cb.
** Return 1 if OK, -1 if closed.
*)

val cb_putc: circbuf: circular_buf-> chr:char -> int

(*
** cb_puts -- put n chars into cb. Return number of written
** chars, or -1 if cb is closed.
*)

val cb_puts: circbuf: circular_buf-> str:string -> int

(*
** cb_getc -- get next byte from cb and return it in char converted
** form.
** Returns always true and CB content char if OK,
** else (false,'\000') if closed and no more data is available.
*)

val cb_getc: circular_buf-> bool * char

(*
** cb_trygetc -- try to get byte from cb.
** Returns (false,'\000') if closed or no chars are available.
*)

val cb_trygetc: circular_buf-> bool * char

(*
** cb_gets -- get between minlen and maxlen chars from circbuf. Returns
** a new string of length (minlen < avail_bytes < maxlen). Returns empty
** string if cb was closed.
*)

val cb_gets: circbuf:circular_buf-> minlen:int -> 
             maxlen:int -> string

(*
** cb_getsn -- get between minlen and maxlen bytes from circbuf.
** Returns -1 if cb closed.
*)

val cb_getsn: circbuf:circular_buf->
              dst:string -> dstpos:int -> minlen:int -> 
              maxlen:int -> int

(*
** cb_putsn -- put n bytes into cb. Return number of written
** bytes, or -1 if cb is closed. String version.
*)

val cb_putsn: circbuf:circular_buf->
              src:string -> srcpos:int -> len:int -> int



(*
** CB put and get functions: generic amoeba buffer version
*)

(*
** cb_putb -- put byte (int) into cb.
** Return 1 if OK, -1 if closed.
*)

val cb_putb: circbuf:circular_buf-> byte:int -> int

(*
** cb_putbn -- put n bytes into cb. Return number of written
** bytes, or -1 if cb is closed.
*)

val cb_putbn: circbuf:circular_buf->
              src:buffer -> srcpos:int -> len:int -> int

(*
** cb_getb -- get next byte from cb and return it in int converted
** form.
** Return always true and CB byte content if OK,
** else false if closed and no more data is available.
*)

val cb_getb: circular_buf-> bool * int

(*
** cb_trygetb -- try to get one byte from cb.
** Return false if closed or no byte is available.
*)

val cb_trygetb: circular_buf-> bool * int

(*
** cb_getbn -- get between minlen and maxlen bytes from circbuf.
** Returns -1 if cb closed.
*)

val cb_getbn: circbuf:circular_buf->
              dst:buffer -> dstpos:int -> minlen:int -> 
              maxlen:int -> int

(*
** cb_getp -- get the position for the next output byte in the buffer.
** Returns (-1,-1) if cb closed, (0,-1) if no bytes available,
** else (num,pos) of available bytes, but limited to the upper bound of
** the cb, and the position within the buffer.
**
** If nonzero return, a call to cb_getpdone must follow to announce how
** many bytes were actually consumed.
*)

val cb_getp: circular_buf-> int*int

val cb_getpdone: circular_buf-> int -> unit

(*
** cb_putp -- get the position for the next free byte in the buffer.
** Returns (-1,-1) if cb closed, (0,-1) if no free bytes available,
** else (num,pos) of available bytes, but limited to the upper bound of
** the cb, and the position within the buffer.
**
** If nonzero return, a call to cb_putpdone must follow to announce how
** many bytes were actually consumed.
*)

val cb_putp: circular_buf-> int*int

val cb_putpdone: circular_buf-> int -> unit


(*
** Print information about a circular buffer.
*)

val print_amoeba_cb: circular_buf-> unit

