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
**    $AUTHORS:     
**    $INITIAL:     (C) INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
**                                                                     
**                           xlib for Ocaml                            
**                                                                     
**       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       
**                                                                     
**  Copyright 1998 Institut National de Recherche en Informatique et   
**  Automatique.  Distributed only by permission.                      
**
** 
**  Modified and restructured by 
**
**        Stefan Bosse
**        sbosse@physik.uni-bremen.de
**
**  Last modified:  03/05/02
**
**  Changes:	-- more comments (in english)
**              -- pthreads support
**              -- Thread support changed
**              -- moved all OS dependent functions to Xos
**              -- X connection shutdown now sync'ed
**
**  $ENDINFO
**
*)

(*
** Main X display management
*)                                                                     

open Thread
open Concur
open KeyBind

open Xbuffer
open Xtypes
open Conv_event
open Ll_trans
open Xos

open Printf
open Os

(*
** X error handling
*)
  
let xopcode opcode = xopcodes.(Obj.magic (opcode : requestOpcode))
let xerror error = xerrors.(Obj.magic (error : errorCode))

let print_xerror error =
  Printf.printf "Warning: Uncaught X Error: %s for request %s\n" (xerror error.err_code) (xopcode error.err_request_code);
  Printf.printf "    Resource: %d  Minor code: %d" error.err_resourceid error.err_minor_code;
  print_newline ()

let xerror_to_string error =
  Printf.sprintf "X Error: %s for request %s\n    Resource: %d  Minor code: %d"  (xerror error.err_code) (xopcode error.err_request_code) error.err_resourceid error.err_minor_code
  
(*
** Reader thread for incoming messages
*)

let readWholeReply comm_channel =
  let read_buffer = Shbuf.readBuffer () in

  Ll_trans.read_channel ~chan:comm_channel ~off:0 ~buf:read_buffer ~len:32;

  if read_buffer.[0]='\001' then
    let len = getCard32 read_buffer 4 
    in
    if len=0 then
      read_buffer (* the expected response *)
    else 
        (* read the (unexpected ?) rest of the message *)
        let b2 = Shbuf.addBuffer read_buffer len in
        Ll_trans.read_channel ~chan:comm_channel 
                              ~off:32 ~buf:b2 ~len:(len lsl 2);
        b2
  else
    read_buffer


(*
** Search the display.wrapper_queue list for the new request
*)

let rec iter_fifo display request list =
  match list with
  | [] -> 
      (* Log.printf "SERIAL: not in the queue %d!\n" request; *)
      (* not in the queue.. *)
      raise Xfifo.Empty         

  | (serial, wrapper):: tail ->
      if serial = request then 
      begin
          (* Log.printf "SERIAL: in the queue %d!\n" serial; *)

          (* 
          ** OK. The request is in the queue. We simply have to remove 
          ** all preceeding elements. 
          *)

          let rec remove () =
            let (serial,wrapper) = Xfifo.read display.wrappers_queue in
            if serial <> request then 
            begin
              let (serial, wrapper) = Xfifo.take display.wrappers_queue
              in                       
              (try
                  (* Log.printf "%s" "O"; *)
                  (* no response *)
                  wrapper " "   
                with
                  _ -> ()
              );
              remove ()
            end
          in
          remove ()
      end else 
          iter_fifo display request tail        

(*
** Find the client wrapper (reply) routine. The wrapper function gets
** the received X message.
*)

let find_wrapper display request =
  let (serial,wrapper) = Xfifo.read display.wrappers_queue
  in
  if serial <> request then 
  begin
      (* Log.printf "\nSERIAL: request received: %d\n" request; *)
      (* Log.printf "SERIAL: waiting: %d\n" serial; *)

      let list = Xfifo.to_list display.wrappers_queue in
      iter_fifo display request list
  end
    
(*
** Read an incoming message
*)

let readReply display =
  let b = readWholeReply display.comm_channel in    

  match b.[0] with
  | '\000'      (* it's an error *)

  | '\001' ->   (* it's a response *)
      (* response message serial number *)
      let request = getCard16 b 2 
      in

      Thread.mu_lock xlib_mutex;

      (try
          find_wrapper display request;
          let (serial, wrapper) = Xfifo.take display.wrappers_queue in 
          (* Log.printf "\nOUT[%d]\n" serial; *)
          (* execute the client wrapper function *)
          wrapper b;
        with
          Xfifo.Empty when b.[0] = '\000' -> 
            (* Warning for error *)
            (* Log.printf "%s\n" "ERREUR"; *)
            let error = getXError b in
            print_xerror error
          | _ -> (* discard the exception *) ()
      );

      Thread.mu_unlock xlib_mutex

  | _ ->        (* it's an event *)
      Thread.mu_lock xlib_mutex;
      display.serial_in <- display.serial_in + 1;

      Xequeue.put display.event_queue
        (convertCore2Event b display.serial_in);

      (* signal for nextEvent *)
      Thread.thread_wakeup xlib_wait;    
      Thread.mu_unlock xlib_mutex
  
	  
(*
** The functions for other threads
*)
      
let send_alone display buf =
  Thread.mu_lock xlib_mutex;

  (* the unique serial number for each message *)
  let serial = (display.serial_out + 1) land 65535 in
  display.serial_out <- serial;
  (* Log.printf "IN[%d]\n" serial; *)

  Ll_trans.write_channel ~chan:display.comm_channel 
                         ~off:0 ~buf:buf ~len:(Shbuf.bufSize buf lsl 2);

  Thread.mu_unlock xlib_mutex;
  serial

let send_with_wrapper display buf wrapper =
  Thread.mu_lock xlib_mutex;

  (* the unique serial number for each message *)
  let serial =  (display.serial_out + 1) land 65535 in
  display.serial_out <- serial;

  Xfifo.put display.wrappers_queue (serial,wrapper);
  (* Log.printf "INR[%d]\n" serial; *)

  Ll_trans.write_channel ~chan:display.comm_channel 
                         ~off:0 ~buf:buf ~len:(Shbuf.bufSize buf lsl 2);

  Thread.mu_unlock xlib_mutex

(*
** The X server connections
*)

(*
** Close a connection - simple (or not ?)
*)

let closeDisplay display = 

    display.dpy_conn_status <- Connection_closed;
    decr display_open;

    (*
    ** We have a problem here:
    ** There is no way to tell the X server that we close the
    ** X connection. Therefore, we cut off the connection.
    ** But there is still the listener thread, blocked
    ** on this connection. To force the listener thread to exit,
    ** we send a dummy request. Above, we have marked our connection
    ** as closed, and the listener thread will exit on the
    ** next reply from the server and cut the connection. Sigh.
    *)

    let root = display.dpy_roots.(display.dpy_screen_default).scr_root in


    let b = newString 32 in    
    setCard8 b 0 0x3; (* OPCode GetWindowAttributes *)
    setCard16 b 2 2;  (* request length *)
    setCard32 b 4 (int_of_enum root);


    Ll_trans.write_channel ~chan:display.comm_channel 
                           ~off:0 ~buf:b ~len:8;
    thread_switch (); 

    Thread.await Concur.shutdown_done;
    thread_switch ()


(*
** Open a new connection to the X server. A new listener thread
** will be started.
*)

let openDisplay name =
  Xos_init.init ();

  let name =
    if name = "" then 
      try
        Sys.getenv "DISPLAY" 
      with
        Not_found -> ":0"
    else name 
  in


  let (chan,server_name,screen_num,auth) = 
        openConnectionWithDisplay name 
  in


  let (xauth_name,xauth_data) = auth in
  let len = 3 +(strLen (String.length xauth_name))+
                (strLen (String.length xauth_data)) in
  let b = newString len  in

  setCard8 b 0 0x6C; (* pour PC *)
  setCard16 b 2 11; (* protocol major *)
  setCard16 b 4 0;  (* protocol minor *)
  setCard16 b 6 (String.length xauth_name);  (* auth protocol *)
  setCard16 b 8 (String.length xauth_data);  (* auth data *)
  (* 10:16 unused *)


  if xauth_name<>"" then 
    setString b 12 xauth_name;
  if xauth_data<>"" then 
    setString b (12+4*(strLen (String.length xauth_name))) xauth_data;
  
  Ll_trans.write_channel ~chan:chan ~off:0 ~buf:b ~len:(len lsl 2);
  Ll_trans.read_channel  ~chan:chan ~off:0 ~buf:b ~len:8;


  let len = getCard16 b 6 in
  let add = newString (len+1) in

  Ll_trans.read_channel ~chan:chan ~off:0 ~buf:add ~len:(len lsl 2);



  if b.[0]='\000' then
    failwith 
      (Printf.sprintf"open_display: Connection refused by %s : %s" server_name (getString add 0 (getCard8 b 1)))
  else if b.[0]='\002' then
    failwith ("open_display: authentification required:"^add)
  else if b.[0] = '\001' then
  begin
      let (maj,min)=(getCard16 b 2,getCard16 b 4)
      in
      let b = add
      in
      
      let display  =
            getXServerInfo b chan maj min screen_num server_name
      in

      incr display_open;
      (*
      ** Start a new listener thread
      *)
      Concur.new_listener 
            (function () -> 

                try
                  readReply display
                with
                  (*
                  ** Lost X server connection or connection closed ? 
                  *)
                  Ll_trans.BrokenConnection ->
                      if (display.dpy_conn_status <> Connection_closed) then
                      begin
                        display.dpy_broken ();
                        raise Ll_trans.BrokenConnection;
                      end
                      else
                      begin
                        (* force a listener thread exit *)
                        raise Exit;
                      end;

            ) display;     
      display
  end
  else
    failwith "open_display: Response inconsistent"

(*
** Get a new display id
*)

let alloc_id display =
  let incr = ref (display.resource_incr)
  in
  let id = ref (display.last_resource_id+display.resource_incr)
  in 
  while (!id land display.resource_mask) <= (display.last_resource_id) do
    incr := (!id lor display.resource_mask) lxor display.resource_mask;
    id := (!id)+(!incr)
  done;
  id := (!id lor (!incr - 1)) lxor (!incr - 1);
  display.last_resource_id <- !id;
  !id lor display.resource_base

let newBuffer = Shbuf.newBuffer
  
let newRequest (op : requestOpcode) len =
  let b = Shbuf.newBuffer len
  in
  (* Log.printf "<%d>" (Obj.magic op); *)
  setEnum8 b 0 op;
  setCard16 b 2 len;
  b

let newWinRequest (op : requestOpcode) len win =
  let b = Shbuf.newBuffer len
  in
  (* Log.printf "<%d>" (Obj.magic op); *)
  setEnum8 b 0 op;
  setCard16 b 2 len;
  setEnum32 b 4 win;
  b

let emptyRequest mask =
  newRequest mask 1

let simpleRequest mask data =
  let b = newRequest mask 2
  in
  setEnum32 b 4 data;
  b

let doubleRequest mask data1 data2 =
  let b = newRequest mask 3
  in
  setEnum32 b 4 data1;
  setEnum32 b 8 data2;
  b

let littleRequest opcode data =
  let b = emptyRequest opcode
  in
  setEnum8 b 1 data;
  b

let parse_buffer event wrapper buffer =
  (*  Log.printf "?%s" "SIGNAL"; *)
  try
    if buffer.[0]='\000' then
      Jeton.signal_exception event (XError (
          let b = getXError buffer in
          if !Xdebug.debug_flag then
            print_xerror b;
          b))
    else
      Jeton.signal_value event (wrapper buffer)
  with
    e -> Jeton.signal_exception event e
      
      
      
   