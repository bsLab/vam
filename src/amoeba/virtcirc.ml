/*
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
**    $VERSION:     1.04
**
**    $INFO:
**
**  Virtual Circuit implementation (distributed circula buffers)
**
**    $ENDOFINFO
**
*/




(*
** virtual circuit module
*)


open Amoeba
open Bytebuf
open Thread
open Mutex
open Sema
open Circbuf
open Cmdreg 
open Rpc
open Stderr
open Stdcom

let client      = 0
let server      = 1

let bit x = 1 lsl x 

let vc_DATA = (vc_FIRST_COM + 100)  (* Data requested, sent      *)
let vc_HUP  = (vc_FIRST_COM + 101)  (* Hangup request, ack'd     *)
let vc_EOF  = (vc_FIRST_COM + 102)  (* No more data, EOF detected*)
let vc_INTR = (vc_FIRST_COM + 103)  (* Rem server got trans sig  *)
let vc_EOF_HUP = (vc_FIRST_COM + 104)  (* I and O cb closed      *)

(*
** Argument to close.
*)

let vc_IN       = bit 0             (* Close input virtual circuit      *)
let vc_OUT      = bit 1             (* Close output virtual circuit     *)
let vc_BOTH     = (bit 0) + (bit 1) (* Close virtual circuit completely *)
let vc_ASYNC    = bit 2             (* Don't wait for completion        *)

let vc_GOT_TRANS_SIG    = bit 0     (* Server received transaction signal *)
and vc_GOT_SERVER_SIG   = bit 1     (* Client received server signal      *)
and vc_GOT_CLIENT_SIG   = bit 2     (* Server received client signal      *)
and vc_CLIENT_CB_ALIVE  = bit 3     (* Client circular not de-allocated   *)
and vc_SERVER_CB_ALIVE  = bit 4     (* Server circular not de-allocated   *)
and vc_ICLOSED          = bit 5     (* Input closed                       *)
and vc_OCLOSED          = bit 6     (* Output closed                      *)



(*
** The virtual circuit control structure
*)

type virt_circ  =
{
    vc_mutex: Mutex.t;          (* VC control block mutex           *)

    vc_client_hup: semaphore;
    vc_client_dead: semaphore;
    vc_oclosed: semaphore;
    vc_iclosed: semaphore;
    vc_done: semaphore;

    mutable vc_status: int;     (* Virtual circuit status           *)
    vc_sigs: int array;         (* Client to srv/srv to client sig  *)
    vc_cb: circular_buf array;  (* In and output circular buffers   *)
    vc_port: port array;        (* Remote/local ports               *)
}

(*
** Returne a new vc control structure, initally empty, needed for
** references onto vc's.
*)

let nilvc  =
    {
        vc_mutex = nilmutex;
        vc_client_hup = nilsema;
        vc_client_dead = nilsema;
        vc_oclosed = nilsema;
        vc_iclosed = nilsema;
        vc_done = nilsema;

        vc_status = 0;     
        vc_sigs = [| 0;0 |];
        vc_cb = [| nilcb;nilcb |];
        vc_port = [| nilport;nilport |];

    }

(*
** vc_zap_cb - remove a circular buffer.
*)

let vc_zap_cb vc which =
    if (which = server) then
    begin 
        if ((vc.vc_status land vc_SERVER_CB_ALIVE) <> 0) then
        begin
            vc.vc_status <- vc.vc_status land (lnot vc_SERVER_CB_ALIVE);
            cb_close vc.vc_cb.(server); (* TODO: really close ? *)
        end;
    end
    else
    begin
        if ((vc.vc_status land vc_CLIENT_CB_ALIVE) <> 0) then
        begin
            vc.vc_status <- vc.vc_status land (lnot vc_CLIENT_CB_ALIVE);
            cb_close vc.vc_cb.(client); (* TODO: really close ? *)            
        end;
    end

(*
** die - clean up.
*)

let vc_die vc which =
    
    if (which = client) then
    begin
        (* release waiting server thread *)
        sema_up vc.vc_client_dead; 
    end
    else
    begin
        sema_down vc.vc_client_dead;
        sema_down vc.vc_iclosed;
        vc_zap_cb vc client;
        sema_down vc.vc_oclosed;
        vc_zap_cb vc server;
        sema_up vc.vc_done;
        (* Now we're sure nobody will touch anything. *)
    end;
    thread_exit ()


(*
** Part I: the server
*)

let vc_close_server vc =
    if ((vc.vc_status land vc_OCLOSED) = 0) then
    begin
        vc.vc_status <- vc.vc_status lor vc_OCLOSED;
        cb_close vc.vc_cb.(server);
        sema_up vc.vc_oclosed;  (* output closed now *)
    end

(*
** vc_local_server_failure, called upon fatal errors.
*)

let vc_local_server_failure vc =
    vc_close_server vc;

    mu_lock vc.vc_mutex;
    if ((vc.vc_status land vc_GOT_CLIENT_SIG) <> 0) then
    begin
        vc.vc_status <- vc.vc_status lor vc_GOT_SERVER_SIG;
        (* sig_raise vc.vc_sigs.(server) TODO *)
    end;
    mu_unlock vc.vc_mutex;

    vc_die vc server
    (* not reached *)

let vc_server vc =
    let dying       = ref false in
    let req_port    = vc.vc_port.(server) in
    let rep_size    = ref 0 in
    let rep_stat    = ref 0 in
    let hdr_rep     = header_new () in

    while (!dying = false)
    do

        let (err,size,hdr_req) = getreq (req_port,nilbuf,0) in


        rep_stat := 0;
        rep_size := 0;

        if ( err = std_OK ) then
        begin
                (* Check states :interrupted or killed *)

            if ((vc.vc_status land vc_GOT_TRANS_SIG) <> 0) then
            begin
                hdr_rep.h_status <- stat_copy (Status vc_INTR);
                hdr_rep.h_size   <- 0;

                ignore(putrep (hdr_rep,nilbuf,0));
            end     
            else
                 (* Now handle the received request. *)

            match (hdr_req.h_command) with 
            | com when (com = std_INFO) ->
            begin
                (* STD_INFO request; simple *)

                let rep_buf         = buf_of_string "Virtual Circuit" in
                hdr_rep.h_size      <- (buf_len rep_buf);
                hdr_rep.h_status    <- stat_copy std_OK;
                
                ignore (putrep (hdr_rep,rep_buf,hdr_rep.h_size)); 
            end;
            | com when (com = Command vc_HUP) ->
            begin
                (* Close server cb; blocked threads will be released. *)
                vc_close_server vc;
                
                sema_down vc.vc_client_hup;

                (* TODO GOT_TRANS_SIG ... *)
                if ((vc.vc_status land vc_GOT_TRANS_SIG) <>0) then
                begin
                    hdr_rep.h_status <- stat_copy (Status vc_INTR);

                    ignore (putrep (hdr_rep,nilbuf,0));
                end
                else
                begin
                    hdr_rep.h_size      <- 0;
                    hdr_rep.h_status    <- stat_copy (Status vc_HUP);

                    ignore( putrep (hdr_rep,nilbuf,0));
                end;
    
                vc_die vc server;
                (* not reached *)
            end;
            | com when (com = Command vc_DATA) ->
            begin
                (* Other side wants data *)


                let (blen,bpos) = cb_getp (vc.vc_cb.(server)) in
                if ((vc.vc_status land vc_GOT_TRANS_SIG) <> 0) then
                begin
                    (* we're interrupted/killed *)
                    hdr_rep.h_status <- stat_copy (Status vc_INTR);
                    hdr_rep.h_size   <- 0;

                    ignore( putrep (hdr_rep,nilbuf,0)); 
                end
                else
                begin
                    if (blen <= 0) then
                    begin

                        rep_size := 0;
                        if ((vc.vc_status land vc_ICLOSED) <> 0 && 
                            (vc.vc_status land vc_OCLOSED) <> 0) then
                            rep_stat := vc_EOF_HUP
                        else
                            rep_stat := vc_EOF;
                    end 
                    else
                    begin
                        rep_size := blen;
                        rep_stat := vc_DATA;
                    end;
            
                    if (!rep_size > max_TRANS) then
                        rep_size := max_TRANS;
                    if (!rep_size > hdr_req.h_size) then
                        rep_size := hdr_req.h_size;

                    (* 
                    ** transmit the data.
                    *)

                    hdr_rep.h_size      <- !rep_size;
                    hdr_rep.h_status    <- (Status !rep_stat); 

                    if (!rep_size > 0) then
                        ignore ( putrepo (hdr_rep,
                                      vc.vc_cb.(server).cbuf,
                                      bpos,
                                      !rep_size))
                    else    
                        ignore ( putrep (hdr_rep,nilbuf,0));

                    if (!rep_stat = vc_DATA) then
                    begin
                        cb_getpdone vc.vc_cb.(server) !rep_size;
                    end;

                    if (!rep_stat = vc_EOF_HUP) then
                        vc_local_server_failure vc;

                    if (!rep_stat = vc_EOF) then
                        vc_zap_cb vc server;

                end;
                                
            end 
            | _ ->
            begin
                hdr_rep.h_status    <- stat_copy std_COMBAD;
                hdr_rep.h_size      <- 0;

                ignore (putrep (hdr_rep,nilbuf,0)); 
            end;
        end          
    done

(*
** Part II: the client transaction
*)

let vc_close_client vc =
    if ((vc.vc_status land vc_ICLOSED) = 0) then
    begin
        vc.vc_status <- vc.vc_status lor vc_ICLOSED;
        cb_close vc.vc_cb.(client);
        sema_up vc.vc_iclosed;  (* input closed now *)
    end

(*
** external failure - the remote server crashed, did something
** wrong or whatever; clean up the mess
*)

let vc_external_failure vc =

    vc_close_client vc;
    mu_lock vc.vc_mutex;

    if ((vc.vc_status land vc_GOT_SERVER_SIG) <> 0 ) then
    begin
        thread_switch ();
        vc.vc_status <- vc.vc_status lor vc_GOT_CLIENT_SIG;
        (* TODO sig_raise vc.vc_sigs.(client) *)    
    end; 
    mu_unlock vc.vc_mutex;

    vc_die vc client
    (* not reached *)

(*
** client_hup_state - transmit a HUP
*)

let vc_client_hup_state vc hdr =

    let alldone  = ref false in 
    let saveport = hdr.h_port in

    (*
    ** close client circular buffer; release blocked threads
    *)

    vc_close_client vc;
    sema_up vc.vc_client_hup;
    while(!alldone = false)
    do
        hdr.h_port <- port_copy saveport;
        hdr.h_size <- 0;
        hdr.h_command <- Command vc_HUP;


        let (err,size,hdr_rep) = trans (hdr,nilbuf,0,nilbuf,0) in

        hdr.h_status <- hdr_rep.h_status;

        if (err <> std_OK) then
            vc_external_failure vc;

        if (hdr.h_status = Status vc_HUP) then
            alldone := true;
    done;
    (*
    ** we're done 
    *)

    vc_die vc client
    (* not reached *)



let vc_client vc =
    let dying       = ref false in
    let transdone   = ref false in
    let hdr_req     = header_new () in
    let rep_size    = ref 0 in

    
    while (!dying = false)
    do
        transdone := false;

        (*
        ** Get an amount of free bytes in the client cb
        *)


        let (blen,bpos) = if ((vc.vc_status land vc_ICLOSED) = 0) then
                            cb_putp vc.vc_cb.(client)
                          else
                            (-1,-1) 

        in
                                  
        if (blen <= 0) then
        begin
            (* HUP, output cb closed *)
            vc_zap_cb vc client;
            vc_client_hup_state vc hdr_req;

            (* not reached !*)
            dying := true;
        end              
        else
        begin            
            (*
            ** We've got a receive buffer, request some bytes
            *)
            rep_size := blen;

            while (!transdone = false)
            do
                hdr_req.h_port      <- vc.vc_port.(client);
                hdr_req.h_command   <- (Command vc_DATA);
                hdr_req.h_size      <- !rep_size;

                let (err,size,hdr_rep) = transo (hdr_req,
                                     nilbuf,0,0,
                                     vc.vc_cb.(client).cbuf,
                                     bpos,
                                     blen) in

                rep_size := size;

                if (err = rpc_ABORTED) then
                    (* NOTHING TODO *) ()
                else if (err <> std_OK) then
                    vc_external_failure vc
                else if (hdr_rep.h_status = Status vc_INTR) then
                    (* NOTHING TODO *) ()
                else if (hdr_rep.h_status = Status vc_EOF ||
                         hdr_rep.h_status = Status vc_DATA ) then
                begin
                    (* okay, we've got data or at least EOF *)

                    transdone := true;
                end
                else    (* i.e. VC_EOF_HUP *)
                begin
                    vc_close_server vc; (* close server, too ? Deadlock *)
                    vc_external_failure vc;
                end;

                if (hdr_rep.h_status = Status vc_EOF) then
                begin
                    vc_client_hup_state vc hdr_req;
                end
                else
                begin
                    if (!rep_size <> hdr_rep.h_size) then
                        failwith "vc_client: returned trans size <> h_size"
                    else
                        cb_putpdone vc.vc_cb.(client) !rep_size;
                end;
            done;
        end;
    done

(*
** vc_close - Close one or both circular buffers.
*)

let vc_close vc which =

    let async = which land vc_ASYNC in
    let which = which land (lnot vc_ASYNC) in

    if ((which land vc_IN) <> 0) then
        vc_close_client vc;

    if ((which land vc_OUT) <> 0) then
        vc_close_server vc;

    (*
    ** Wait for completion 
    *)
    if ( async = 0 &&
        (vc.vc_status land vc_ICLOSED) <> 0 &&
        (vc.vc_status land vc_OCLOSED) <> 0) then
    begin
        sema_down vc.vc_done;
    end

(*
** vc_create - create a full-duplex virtual circuit.
*)

let vc_create ~iport:iport 
              ~oport:oport
              ~isize:isize
              ~osize:osize
    =
    
    let vc = 
        {
            vc_mutex        = mu_create ();
            vc_client_hup   = sema_create 0;
            vc_client_dead  = sema_create 0; 
            vc_oclosed      = sema_create 0;
            vc_iclosed      = sema_create 0;
            vc_done         = sema_create 0;

            vc_status   = vc_CLIENT_CB_ALIVE + vc_SERVER_CB_ALIVE;
            vc_sigs     = [| 
                             0; (* Server to client signal number CNI *)
                             0  (* Client to server signal number CNI *)
                          |];

            (* Allocate circular buffers *)
            vc_cb       = [|
                                (cb_create ~size:isize);    (* client *)
                                (cb_create ~size:osize)     (* server *)
                          |];

            vc_port     = [| 
                                (port_copy iport);          (* client *)
                                (port_copy oport)           (* server *)
                          |];
        }
    in

    (*
    ** Start server and client.
    *)

    let tid1 = thread_create ~func:vc_server ~arg:vc in
    let tid2 = thread_create ~func:vc_client ~arg:vc in

    vc


(*
** vc_read - read string (maximal length len) from vc.
*)

let vc_reads vc ~str:str ~pos:pos ~len:len =
    cb_getsn ~circbuf:vc.vc_cb.(client)
             ~dst:str
             ~dstpos:pos
             ~minlen:1
             ~maxlen:len

(*
** read instead in a  buffer at position pos (maximal length len)
*)

let vc_readb vc ~buf:buf ~pos:pos ~len:len =
    cb_getbn ~circbuf:vc.vc_cb.(client)
             ~dst:buf
             ~dstpos:pos
             ~minlen:1
             ~maxlen:len


(*
** Write a string to the vc
*)

let vc_writes vc ~str:str ~pos:pos ~len:len =
    cb_putsn ~circbuf:vc.vc_cb.(server)
             ~src:str
             ~srcpos:pos
             ~len:len


(*
** Write to the vc from a buffer starting at position pos and length len.
*)

let vc_writeb vc ~buf:buf ~pos:pos ~len:len =
    cb_putbn ~circbuf:vc.vc_cb.(server)
             ~src:buf
             ~srcpos:pos
             ~len:len

 

(*
** vc_getp - get a circular buffer pointer to fetch data
** from
*)

let vc_getp vc =
    cb_getp vc.vc_cb.(client)

(*
** vc_getpdone - mark it done
*)

let vc_getpdone vc len =
    cb_getpdone vc.vc_cb.(client) len



(*
** vc_putp - get a circular buffer pointer to store data in.
*)

let vc_putp vc =
    cb_putp vc.vc_cb.(server) 

(*
** vc_putpdone - mark it done
*)

let vc_putpdone vc len =
    cb_putpdone vc.vc_cb.(server) len


(*
** vc_avail - see how many bytes (or buffer space) are available.
*)

let vc_avail vc which =
    if (which <> vc_IN && which <> vc_OUT) then
        failwith "vc_avail: programming error: either vc_IN or vc_OUT";

    if (which = vc_IN) then
        cb_full vc.vc_cb.(client)
    else
        cb_full vc.vc_cb.(server)


open Format
open String

let print_amoeba_vc vc =
    let cbc = vc.vc_cb.(client) in
    let cbs = vc.vc_cb.(server) in

    open_hvbox 0;
    print_string ("'Clen=" ^ 
                  (string_of_int cbc.last_pos) ^ 
                  " Cfull=" ^ 
                  (if (sema_level cbc.full)=0 then "true" else "false") ^ 
                  " Cempty=" ^ 
                  (if (sema_level cbc.empty)=0 then "true" else "false") ^ 
                  " Cbytes=" ^ 
                  (string_of_int cbc.nbytes) ^ 
                  " Slen=" ^ 
                  (string_of_int cbs.last_pos) ^ 
                  " Sfull=" ^ 
                  (if (sema_level cbs.full)=0 then "true" else "false") ^ 
                  " Sempty=" ^ 
                  (if (sema_level cbs.empty)=0 then "true" else "false") ^ 
                  " Sbytes=" ^ 
                  (string_of_int cbs.nbytes) ^ 
                  "'");
    close_box ()
    