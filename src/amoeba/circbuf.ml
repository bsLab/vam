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
**    $VERSION:     1.05
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
**    $ENDOFINFO
**
*)





open StdLabels

open Amoeba
open Bytebuf
open Buf
open Thread
open Mutex
open Sema
open Format
open String

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

let nilcb =
    {
        empty = nilsema;
        full = nilsema;
        lock = nilmutex;
        cbuf = buf_of_string "";
        in_pos = 0;
        out_pos = 0;
        last_pos = 0;
        nbytes = 0;
        getpcount = 0;
        putpcount = 0;
        closed = false;
    }

(*
** cb_create -- allocate a new circular buffer of a given size.
*)

let cb_create ~size:size =
    let cb = 
    {
        empty    = sema_create 1;
        full     = sema_create 1;
        lock     = mu_create ();
        cbuf     = buf_create size;
        in_pos   = 0;
        out_pos  = 0;
        last_pos = size;
        nbytes   = 0;
        getpcount = 0;
        putpcount = 0;
        closed   = false;
    } in
    sema_down cb.empty;
    cb


(*
** cb_close -- set closed flag.
** May be called as often as you want, by readers and writers.
** Once closed, no new data can be pushed into the buffer,
** but data already in it is still available to readers.
*)

let cb_close cb =
    mu_lock cb.lock;
    if (cb.closed = true) then
        mu_unlock cb.lock   (* already all done *)
    else
    begin
        if (cb.nbytes = 0) then
            sema_up cb.empty
        else if (cb.in_pos = cb.out_pos) then
                sema_up cb.full;
        cb.closed <- true;
        mu_unlock cb.lock;
    end

(*
** cb_full -- return number of available data bytes.
** When closed and there are no bytes available, return -1.
*)

let cb_full cb =
    mu_lock cb.lock;
    let n = if (cb.nbytes = 0 && cb.closed = true) then
                -1
            else
                cb.nbytes 
    in
    mu_unlock cb.lock;
    n

(*
** cb_empty -- return number of available free bytes.
** Return -1 if closed (this can be used as a test for closedness).
*)

let cb_empty cb =
    mu_lock cb.lock;
    let n = if (cb.closed = true) then
                -1
            else
                (cb.last_pos - cb.nbytes)
    in
    mu_unlock cb.lock;
    n

(*
** CB put and get functions: char and string version
*)

(*
** cb_putc -- put char into cb.
** Return 1 if OK, -1 if closed.
*)

let cb_putc ~circbuf:cb ~chr:c =
    sema_down cb.full;
    mu_lock cb.lock;
    let b = cb.cbuf in

    let n = 
        if (cb.closed = true) then
        begin
            mu_unlock cb.lock;
            sema_up cb.full;
            -1    
        end
        else
        begin
            if (cb.nbytes = 0) then
                sema_up cb.empty;  (* First byte being added to empty buffer *)

            buf_set b cb.in_pos (int_of_char c);

            cb.in_pos <- cb.in_pos + 1;

            if (cb.in_pos = cb.last_pos) then
                    cb.in_pos <- 0;

            cb.nbytes <- cb.nbytes + 1;

            if (cb.in_pos <> cb.out_pos) then
                    sema_up cb.full; (* Buffer is not yet full *)

            mu_unlock cb.lock;

            1
        end
    in
    n

(*
** Buffer puts worker. This function must be called with a
** locked cb mutex! Buffer version.
*)

let cb_put_buf_n cb src srcpos len =
        let b = cb.cbuf in      

        if ((cb.last_pos - cb.in_pos) >= len) then
        begin

            (* simple case: no circular wrap around *)
            blit_bb ~src:src     ~src_pos:srcpos
                    ~dst:b          ~dst_pos:cb.in_pos
                    ~len:len;
            (*
            buf_sets b cb.in_pos (
                String.sub (string_of_buf src) srcpos len
            );
            *)

            cb.in_pos <- cb.in_pos + len;

            if (cb.in_pos = cb.last_pos) then 
                cb.in_pos <- 0; (* upper limit reached, skip down *)

            cb.nbytes <- cb.nbytes + len;
        end
        else
        begin
            (* first copy all untill the upper limit is reached *)
            let push1 = cb.last_pos - cb.in_pos in
            
            blit_bb     ~src:src     ~src_pos:srcpos
                        ~dst:b          ~dst_pos:cb.in_pos
                        ~len:push1;
            (*
            buf_sets b cb.in_pos (
                String.sub (string_of_buf src) srcpos push1
            );
            *)

            (* now the rest starting at cbuf[0] *)          
            let push2 = len - push1 in

            
            blit_bb     ~src:src     ~src_pos:(srcpos + push1)
                        ~dst:b          ~dst_pos:0
                        ~len:push2;

            (*
            buf_sets b 0 (
                String.sub (string_of_buf src) (srcpos+push1) push2
            );
            *)

            cb.in_pos <- push2;
            cb.nbytes <- cb.nbytes + len;  
        end;

        if (cb.in_pos <> cb.out_pos) then
            sema_up cb.full  
            (* Buffer is not yet full *)

(*
** Buffer puts worker. This function must be called with a
** locked cb mutex! String version.
*)

let cb_put_str_n cb src srcpos len =
        let b = cb.cbuf in      

        if ((cb.last_pos - cb.in_pos) >= len) then
        begin

            (* simple case: no circular wrap around *)
            blit_sb ~src:src     ~src_pos:srcpos
                    ~dst:b          ~dst_pos:cb.in_pos
                    ~len:len;
            (*
            buf_sets b cb.in_pos (
                String.sub (string_of_buf src) srcpos len
            );
            *)

            cb.in_pos <- cb.in_pos + len;

            if (cb.in_pos = cb.last_pos) then 
                cb.in_pos <- 0; (* upper limit reached, skip down *)

            cb.nbytes <- cb.nbytes + len;
        end
        else
        begin
            (* first copy all untill the upper limit is reached *)
            let push1 = cb.last_pos - cb.in_pos in
            
            blit_sb     ~src:src     ~src_pos:srcpos
                        ~dst:b          ~dst_pos:cb.in_pos
                        ~len:push1;
            (*
            buf_sets b cb.in_pos (
                String.sub (string_of_buf src) srcpos push1
            );
            *)

            (* now the rest starting at cbuf[0] *)          
            let push2 = len - push1 in

            
            blit_sb     ~src:src     ~src_pos:(srcpos + push1)
                        ~dst:b          ~dst_pos:0
                        ~len:push2;

            (*
            buf_sets b 0 (
                String.sub (string_of_buf src) (srcpos+push1) push2
            );
            *)

            cb.in_pos <- push2;
            cb.nbytes <- cb.nbytes + len;  
        end;

        if (cb.in_pos <> cb.out_pos) then
            sema_up cb.full  
            (* Buffer is not yet full *)



(*
** cb_puts -- put n chars into cb. Return number of written
** chars, or -1 if cb is closed.
*)

let cb_puts ~circbuf:cb ~str:sbuf =
    sema_down cb.full;
    mu_lock cb.lock;

    let b = cb.cbuf in
    let len = String.length sbuf in

    if (cb.closed = true) then
    begin
        mu_unlock cb.lock;
        sema_up cb.full;
        -1    
    end
    else if ((cb.last_pos - cb.nbytes) >= len) then
    begin
        if (cb.nbytes = 0) then
            sema_up cb.empty;  (* First byte being added to empty buffer *)

        cb_put_buf_n cb (buf_of_string sbuf) 0 len;

        mu_unlock cb.lock;
        len
    end 
    else
    begin
        let count = ref (cb.last_pos - cb.nbytes) in
        cb_put_buf_n cb (buf_of_string sbuf) 0 (!count);

        (* wait untill enough space is available in cbuf *)
        while ((!count < len) && cb.closed = false)
        do
            mu_unlock cb.lock;
            sema_down cb.full;
            mu_lock cb.lock;
            if (cb.closed = false) then
            begin
                let avail = (cb.last_pos - cb.nbytes) in
                cb_put_buf_n cb (buf_of_string sbuf) (!count) avail;
            end;
        done;
        if (cb.closed = true) then
        begin
            mu_unlock cb.lock;
            -1
        end
        else
        begin
            mu_unlock cb.lock;
            len
        end
    end


(*
** cb_getc -- get next byte from cb and return it in char converted
** form. 
** Returns always true and CB content char if OK, 
** else false,'\000' if closed and no more data is available.
*)

let cb_getc cb =
    sema_down cb.empty;
    mu_lock cb.lock;
    let b = cb.cbuf in

    let n =
        if (cb.nbytes = 0) then
        begin
            if (cb.closed = false) then
                failwith "Circbuf: cb_getc: Got past full, not closed, no bytes???";
            mu_unlock cb.lock;
            sema_up cb.empty;
            (false,'\000')                    
        end
        else
        begin
            if (cb.in_pos = cb.out_pos) then
                sema_up cb.full; (* Buffer no longer full *)
            let c = char_of_int (buf_get b cb.out_pos) in

            cb.out_pos <- cb.out_pos + 1;
            if (cb.out_pos = cb.last_pos) then
                cb.out_pos <- 0;

            cb.nbytes <- cb.nbytes - 1;
            if (cb.nbytes <> 0 || cb.closed = true) then
                sema_up cb.empty; (* Buffer is not yet empty *)

            mu_unlock cb.lock;
            (true,c) 
        end
    in
    n

(*
** cb_trygetc -- try to get byte from cb.
** Return false if closed or no chars are available.
*)

let cb_trygetc cb =
    let n =
        if ((sema_trydown cb.empty 0) = false) then
            (false,'\000')
        else
        begin
            mu_lock cb.lock;
            let b = cb.cbuf in

            if (cb.nbytes = 0) then
            begin
                if (cb.closed = false) then
                    failwith "Circbuf: cb_getc: Got past full, not closed, no bytes???";
                mu_unlock cb.lock;
                sema_up cb.empty;
                (false,'\000')                    
            end
            else
            begin
                if (cb.in_pos = cb.out_pos) then
                    sema_up cb.full; (* Buffer no longer full *)
                let c = char_of_int (buf_get b cb.out_pos) in

                cb.out_pos <- cb.out_pos + 1;
                if (cb.out_pos = cb.last_pos) then
                    cb.out_pos <- 0;

                cb.nbytes <- cb.nbytes - 1;
                if (cb.nbytes <> 0 || cb.closed = true) then
                    sema_up cb.empty; (* Buffer is not yet empty *)

                mu_unlock cb.lock;
                (true,c) 
            end
        end
    in
    n


(*
** Buffer gets worker. This function must be called with a
** locked cb mutex! Buffer version.
*)

let cb_get_buf_n cb dst dstpos len =
  
        let b = cb.cbuf in      
        if ((cb.out_pos + len) <= cb.last_pos) then
        begin
            (* really simple; copy one contigous block *) 
            
            blit_bb     ~src:b       ~src_pos:cb.out_pos  
                        ~dst:dst  ~dst_pos:dstpos
                        ~len:len;
            (*
            let s = buf_gets b cb.out_pos len in
            buf_sets dst dstpos s;
            *)
        end
        else
        begin
            (* 
            ** Copy first block upto upper end, and second block starting 
            ** from at bottom of the cb. 
            *)
            let top_bytes = cb.last_pos - cb.out_pos in
            
            blit_bb     ~src:b       ~src_pos:cb.out_pos  
                        ~dst:dst  ~dst_pos:dstpos
                        ~len:top_bytes;

            (*
            let s = buf_gets b cb.out_pos top_bytes in
            buf_sets dst dstpos s;
            *)

            let bottom_bytes = len - top_bytes in
            
            blit_bb     ~src:b       ~src_pos:0  
                        ~dst:dst  ~dst_pos:(dstpos+top_bytes)
                        ~len:bottom_bytes;
            (*
            let s = buf_gets b 0 bottom_bytes in
            buf_sets dst (dstpos+top_bytes) s;
            *)
        end;
        
        if (cb.in_pos = cb.out_pos) then
            sema_up cb.full; 
            (* 
            ** Buffer no longer full. Remeber: first cb.full is locked,
            ** than cb.lock!  
            *)

        cb.nbytes <- cb.nbytes - len;
        cb.out_pos <- cb.out_pos + len;

        if (cb.out_pos > cb.last_pos) then
        begin
            (* cb wrap around - fix it *)
            cb.out_pos <- cb.out_pos - cb.last_pos;    
        end;             
        if (cb.nbytes <> 0 || cb.closed = true ) then
            sema_up cb.empty  (* Buffer is not yet empty *)

(*
** Buffer gets worker. This function must be called with a
** locked cb mutex! String version.
*)

let cb_get_str_n cb dst dstpos len =
  
        let b = cb.cbuf in      
        if ((cb.out_pos + len) <= cb.last_pos) then
        begin
            (* really simple; copy one contigous block *) 
            
            blit_bs     ~src:b       ~src_pos:cb.out_pos  
                        ~dst:dst  ~dst_pos:dstpos
                        ~len:len;
            (*
            let s = buf_gets b cb.out_pos len in
            buf_sets dst dstpos s;
            *)
        end
        else
        begin
            (* 
            ** Copy first block upto upper end, and second block starting 
            ** from at bottom of the cb. 
            *)
            let top_bytes = cb.last_pos - cb.out_pos in
            
            blit_bs     ~src:b       ~src_pos:cb.out_pos  
                        ~dst:dst  ~dst_pos:dstpos
                        ~len:top_bytes;

            (*
            let s = buf_gets b cb.out_pos top_bytes in
            buf_sets dst dstpos s;
            *)

            let bottom_bytes = len - top_bytes in
            
            blit_bs     ~src:b       ~src_pos:0  
                        ~dst:dst  ~dst_pos:(dstpos+top_bytes)
                        ~len:bottom_bytes;
            (*
            let s = buf_gets b 0 bottom_bytes in
            buf_sets dst (dstpos+top_bytes) s;
            *)
        end;
        
        if (cb.in_pos = cb.out_pos) then
            sema_up cb.full; 
            (* 
            ** Buffer no longer full. Remeber: first cb.full is locked,
            ** than cb.lock!  
            *)

        cb.nbytes <- cb.nbytes - len;
        cb.out_pos <- cb.out_pos + len;

        if (cb.out_pos > cb.last_pos) then
        begin
            (* cb wrap around - fix it *)
            cb.out_pos <- cb.out_pos - cb.last_pos;    
        end;             
        if (cb.nbytes <> 0 || cb.closed = true ) then
            sema_up cb.empty  (* Buffer is not yet empty *)


(*
** cb_gets -- get between minlen and maxlen chars from circbuf. Returns
** a new string of length (minlen < avail_bytes < maxlen). Returns empty
** string if cb was closed.
*)

let cb_gets ~circbuf:cb 
            ~minlen:minlen 
            ~maxlen:maxlen =

    sema_down cb.empty;
    mu_lock cb.lock;

    
    if (cb.nbytes < minlen) then
    begin
        (* 
        ** Not enough available; wait untill at least
        ** minlen bytes are available.
        *)

        let count = ref (cb.nbytes) in

        let stb = buf_physical maxlen in
        cb_get_buf_n cb stb 0 (!count); 
    
        
        while (!count < minlen && cb.closed = false)
        do
                mu_unlock cb.lock;
                sema_down cb.empty;
                mu_lock cb.lock;
                let get_bytes = if ( (!count + cb.nbytes) <= maxlen) then
                                    cb.nbytes 
                                else
                                    (maxlen - !count)
                in 
                cb_get_buf_n cb stb (!count) get_bytes; 
                count := !count + get_bytes;
        done;               

        if (cb.closed = false) then
        begin
            mu_unlock cb.lock;

            if (minlen <> maxlen) then
                String.sub (string_of_buf stb) ~pos:0 ~len:(!count)
            else
                (string_of_buf stb);
        end
        else
        begin
            mu_unlock cb.lock;
            ""
        end;
    end
    else 
    begin

        let avail_bytes = cb.nbytes in
        (* at least minlen bytes are available *)
        let get_bytes = if (avail_bytes <= maxlen) then
                            avail_bytes
                        else
                            maxlen
        in
        let stb = buf_physical get_bytes in
        cb_get_buf_n cb stb 0 get_bytes;
        mu_unlock cb.lock;

        (string_of_buf stb)
    end

(*
** CB put and get functions: generic amoeba buffer version 
*)

(*
** cb_putb -- put byte (int) into cb.
** Return 1 if OK, -1 if closed.
*)

let cb_putb ~circbuf:cb ~byte:c =
    sema_down cb.full;
    mu_lock cb.lock;
    let b = cb.cbuf in

    let n = 
        if (cb.closed = true) then
        begin
            mu_unlock cb.lock;
            sema_up cb.full;
            -1    
        end
        else
        begin
            if (cb.nbytes = 0) then
                sema_up cb.empty;  (* First byte being added to empty buffer *)

            buf_set b cb.in_pos c;

            cb.in_pos <- cb.in_pos + 1;

            if (cb.in_pos = cb.last_pos) then
                    cb.in_pos <- 0;

            cb.nbytes <- cb.nbytes + 1;

            if (cb.in_pos <> cb.out_pos) then
                    sema_up cb.full; (* Buffer is not yet full *)

            mu_unlock cb.lock;

            1
        end
    in
    n

(*
** cb_putbn -- put n bytes into cb. Return number of written
** bytes, or -1 if cb is closed.
*)

let cb_putbn ~circbuf:cb ~src:src ~srcpos:spos ~len:len =
    let sbuf = src in

    sema_down cb.full;
    mu_lock cb.lock;

    let b = cb.cbuf in

    if (cb.closed = true) then
    begin
        mu_unlock cb.lock;
        sema_up cb.full;
        -1    
    end
    else if ((cb.last_pos - cb.nbytes) >= len) then
    begin
        if (cb.nbytes = 0) then
        begin
            sema_up cb.empty;  (* First byte being added to empty buffer *)
        end;
        cb_put_buf_n cb sbuf spos len;

        mu_unlock cb.lock;
        len
    end 
    else
    begin
        let count = ref (cb.last_pos - cb.nbytes) in
        cb_put_buf_n cb sbuf 0 (!count);

        (* wait untill enough space is available in cbuf *)
        while ((!count < len) && cb.closed = false)
        do
            mu_unlock cb.lock;
            sema_down cb.full;
            mu_lock cb.lock;
            if (cb.closed = false) then
            begin
                let avail = (cb.last_pos - cb.nbytes) in
                cb_put_buf_n cb sbuf (!count + spos) avail;
            end;
        done;
        if (cb.closed = true) then
        begin
            mu_unlock cb.lock;
            -1
        end
        else
        begin
            mu_unlock cb.lock;
            len
        end
    end



(*
** cb_getb -- get next byte from cb and return it in int converted
** form. 
** Return always true and CB byte content if OK, 
** else false if closed and no more data is available.
*)

let cb_getb cb =
    sema_down cb.empty;
    mu_lock cb.lock;
    let b = cb.cbuf in

    let n =
        if (cb.nbytes = 0) then
        begin
            if (cb.closed = false) then
                failwith "Circbuf: cb_getc: Got past full, not closed, no bytes???";
            mu_unlock cb.lock;
            sema_up cb.empty;
            (false,0)                    
        end
        else
        begin
            if (cb.in_pos = cb.out_pos) then
                sema_up cb.full; (* Buffer no longer full *)
            let c = buf_get b cb.out_pos in

            cb.out_pos <- cb.out_pos + 1;
            if (cb.out_pos = cb.last_pos) then
                cb.out_pos <- 0;

            cb.nbytes <- cb.nbytes - 1;
            if (cb.nbytes <> 0 || cb.closed = true) then
                sema_up cb.empty; (* Buffer is not yet empty *)

            mu_unlock cb.lock;
            (true,c)
        end
    in
    n

(*
** cb_trygetb -- try to get one byte from cb.
** Return false if closed or no byte is available.
*)

let cb_trygetb cb =
    let n =
        if ((sema_trydown cb.empty 0) = false) then
            (false,0)
        else
        begin
            mu_lock cb.lock;
            let b = cb.cbuf in

            if (cb.nbytes = 0) then
            begin
                if (cb.closed = false) then
                    failwith "Circbuf: cb_getc: Got past full, not closed, no bytes???";
                mu_unlock cb.lock;
                sema_up cb.empty;
                (false,0)                   
            end
            else
            begin
                if (cb.in_pos = cb.out_pos) then
                    sema_up cb.full; (* Buffer no longer full *)
                let c = buf_get b cb.out_pos in
    
                cb.out_pos <- cb.out_pos + 1;
                if (cb.out_pos = cb.last_pos) then
                    cb.out_pos <- 0;

                cb.nbytes <- cb.nbytes - 1;
                if (cb.nbytes <> 0 || cb.closed = true) then
                    sema_up cb.empty; (* Buffer is not yet empty *)

                mu_unlock cb.lock;
                (true,c) 
            end
        end
    in
    n

(*
** cb_getbn -- get between minlen and maxlen bytes from circbuf.
** Returns -1 if cb closed.
*)

let cb_getbn ~circbuf:cb 
             ~dst:dst 
             ~dstpos:pos 
             ~minlen:minlen 
             ~maxlen:maxlen =

    let buf = dst in

    sema_down cb.empty;
    mu_lock cb.lock;

    if (cb.nbytes < minlen) then
    begin
        (* 
        ** Not enough available; wait untill at least
        ** minlen bytes are available.
        *)

        let count = ref (cb.nbytes) in

        cb_get_buf_n cb buf pos (!count); 
    
        
        while (!count < minlen && cb.closed = false)
        do
                mu_unlock cb.lock;
                sema_down cb.empty;
                mu_lock cb.lock;
                let get_bytes = if ( (!count + cb.nbytes) <= maxlen) then
                                    cb.nbytes 
                                else
                                    (maxlen - !count - cb.nbytes)
                in 
                cb_get_buf_n cb buf (!count + pos) get_bytes; 
                count := !count + get_bytes;
        done;               

        if (cb.closed = false) then
        begin
            mu_unlock cb.lock;
            !count
        end
        else
        begin
            mu_unlock cb.lock;
            -1
        end

    end
    else 
    begin

        let avail_bytes = cb.nbytes in
        (* at least minlen bytes are available *)
        let get_bytes = if (avail_bytes <= maxlen) then
                            avail_bytes
                        else
                            maxlen
        in
        cb_get_buf_n cb buf pos get_bytes;
        mu_unlock cb.lock;

        get_bytes
    end

(*
** cb_getsn -- get between minlen and maxlen bytes from circbuf.
** Returns -1 if cb closed. String version.
*)

let cb_getsn ~circbuf:cb 
             ~dst:dst 
             ~dstpos:pos 
             ~minlen:minlen 
             ~maxlen:maxlen =

    let str = dst in

    sema_down cb.empty;
    mu_lock cb.lock;

    
    if (cb.nbytes < minlen) then
    begin
        (* 
        ** Not enough available; wait untill at least
        ** minlen bytes are available.
        *)

        let count = ref (cb.nbytes) in

        cb_get_str_n cb str pos (!count); 
    
        
        while (!count < minlen && cb.closed = false)
        do
                mu_unlock cb.lock;
                sema_down cb.empty;
                mu_lock cb.lock;
                let get_bytes = if ( (!count + cb.nbytes) <= maxlen) then
                                    cb.nbytes 
                                else
                                    (maxlen - !count - cb.nbytes)
                in 
                cb_get_str_n cb str (!count + pos) get_bytes; 
                count := !count + get_bytes;
        done;               

        if (cb.closed = false) then
        begin
            mu_unlock cb.lock;
            !count
        end
        else
        begin
            mu_unlock cb.lock;
            -1
        end

    end
    else 
    begin

        let avail_bytes = cb.nbytes in
        (* at least minlen bytes are available *)
        let get_bytes = if (avail_bytes <= maxlen) then
                            avail_bytes
                        else
                            maxlen
        in
        cb_get_str_n cb str pos get_bytes;
        mu_unlock cb.lock;

        get_bytes
    end

(*
** cb_putsn -- put n bytes into cb. Return number of written
** bytes, or -1 if cb is closed. String version.
*)

let cb_putsn ~circbuf:cb ~src:src ~srcpos:spos ~len:len =
    let sbuf = src in

    sema_down cb.full;
    mu_lock cb.lock;

    let b = cb.cbuf in

    if (cb.closed = true) then
    begin
        mu_unlock cb.lock;
        sema_up cb.full;
        -1    
    end
    else if ((cb.last_pos - cb.nbytes) >= len) then
    begin
        if (cb.nbytes = 0) then
            sema_up cb.empty;  (* First byte being added to empty buffer *)

        cb_put_str_n cb sbuf spos len;

        mu_unlock cb.lock;
        len
    end 
    else
    begin
        let count = ref (cb.last_pos - cb.nbytes) in
        cb_put_str_n cb sbuf 0 (!count);

        (* wait untill enough space is available in cbuf *)
        while ((!count < len) && cb.closed = false)
        do
            mu_unlock cb.lock;
            sema_down cb.full;
            mu_lock cb.lock;
            if (cb.closed = false) then
            begin
                let avail = (cb.last_pos - cb.nbytes) in
                cb_put_str_n cb sbuf (!count + spos) avail;
            end;
        done;
        if (cb.closed = true) then
        begin
            mu_unlock cb.lock;
            -1
        end
        else
        begin
            mu_unlock cb.lock;
            len
        end
    end



(*
** cb_getp -- get the position for the next output byte in the buffer.
** Returns (-1,-1) if cb closed, (0,-1) if no bytes available, 
** else (num,pos) of available bytes, but limited to the upper bound of 
** the cb, and the position within the buffer.
**
** If nonzero return, a call to cb_getpdone must follow to announce how
** many bytes were actually consumed.
*)

let cb_getp cb =

    if (cb.getpcount <> 0) then
        failwith "cb_getp: already under progress";

    sema_down cb.empty;
    mu_lock cb.lock;

    if (cb.nbytes = 0) then
    begin
        if (cb.closed = false) then
            failwith "cb_getp: Got past full, not closed, no bytes???";
        mu_unlock cb.lock;
        sema_up cb.empty;
        (-1,-1)
    end
    else
    begin
        cb.getpcount <- (if (cb.in_pos > cb.out_pos) then
                            cb.nbytes
                         else
                            cb.last_pos - cb.out_pos);
        mu_unlock cb.lock;
        (cb.getpcount,cb.out_pos)
    end    


let cb_getpdone cb len =
    if (cb.getpcount = 0 ||
        len < 0 ||
        len > cb.getpcount) then
        failwith "cb_getpdone: Require 0 <= len <= getpcount, and previous cb_getp call";

    mu_lock cb.lock;
    if (len=0) then
        sema_up cb.empty
    else
    begin
        if (cb.in_pos = cb.out_pos) then
            sema_up cb.full;    (* Buffer no longer full *)

        cb.out_pos <- cb.out_pos + len;
        if (cb.out_pos = cb.last_pos) then
            cb.out_pos <- 0;

        cb.nbytes <- cb.nbytes - len;
        if (cb.nbytes <> 0 || cb.closed = true) then
            sema_up cb.empty;   (* Buffer is not yet empty *)

        cb.getpcount <- 0;
        mu_unlock cb.lock;
    end

(*
** cb_putp -- get the position for the next free byte in the buffer.  
** Returns (-1,-1) if cb closed, (0,-1) if no free bytes available,
** else (num,pos) of available bytes, but limited to the upper bound of
** the cb, and the position within the buffer.
**
** If nonzero return, a call to cb_putpdone must follow to announce how
** many bytes were actually consumed.
*)

let cb_putp cb =

    if (cb.putpcount <> 0) then
        failwith "cb_putp: already under progress";

    sema_down cb.full;
    mu_lock cb.lock;

    if (cb.closed = true) then
    begin
        mu_unlock cb.lock;
        sema_up cb.full;
        (-1,-1)
    end
    else
    begin
        cb.putpcount <- (if (cb.out_pos > cb.in_pos) then
                            cb.out_pos
                         else
                            cb.last_pos) - cb.in_pos;
        mu_unlock cb.lock;
        
        (
            cb.putpcount,
            (if (cb.putpcount>0) then cb.in_pos else 0)
        )
    end    


let cb_putpdone cb len =
    if (cb.putpcount = 0 ||
        len < 0 ||
        len > cb.putpcount) then
        failwith "cb_putpdone: Require 0 <= len <= putpcount and previous cb_putp call";

    mu_lock cb.lock;

    if (len=0 || cb.closed = true) then
        sema_up cb.full
    else
    begin
        if (cb.nbytes = 0) then
            sema_up cb.empty;   (* Buffer no longer empty *)

        cb.in_pos <- cb.in_pos + len;
        if (cb.in_pos = cb.last_pos) then
            cb.in_pos <- 0;

        cb.nbytes <- cb.nbytes + len;
        if (cb.out_pos <> cb.in_pos) then
            sema_up cb.full;    (* Buffer is not yet full *)

        cb.putpcount <- 0;
    end;
    mu_unlock cb.lock




(*
** Print information about a circular buffer.
*)

let print_amoeba_cb cb =
    open_hvbox 0;
    print_string ("'len=" ^ 
                  (string_of_int cb.last_pos) ^ 
                  " full=" ^ 
                  (if (sema_level cb.full)=0 then "true" else "false") ^ 
                  " empty=" ^ 
                  (if (sema_level cb.empty)=0 then "true" else "false") ^ 
                  " inbytes=" ^ 
                  (string_of_int cb.nbytes) ^ 
                  "'");
    close_box ()
