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
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003-2006 BSSLAB
**    $CREATED:     
**    $VERSION:     1.03
**
**    $INFO:
**
**  Device Driver Interface
**
**    $ENDOFINFO
**
*)


external ext_io_check_region :
  Machtype.int32 -> Machtype.int32 -> Amoeba.capability -> Amoeba.status
  = "ext_io_check_region"
val io_check_region :
  start:Machtype.int32 ->
  size:Machtype.int32 -> syscap:Amoeba.capability -> Amoeba.status
external ext_io_map_region :
  Machtype.int32 ->
  Machtype.int32 -> string -> Amoeba.capability -> Amoeba.status
  = "ext_io_map_region"
val io_map_region :
  start:Machtype.int32 ->
  size:Machtype.int32 ->
  devname:string -> syscap:Amoeba.capability -> Amoeba.status
external ext_io_unmap_region :
  Machtype.int32 -> Machtype.int32 -> Amoeba.capability -> Amoeba.status
  = "ext_io_unmap_region"
val io_unmap_region :
  start:Machtype.int32 ->
  size:Machtype.int32 -> syscap:Amoeba.capability -> Amoeba.status
external ext_io_out_byte : Machtype.int32 -> Machtype.int32 -> unit
  = "ext_io_out_byte"
val out_byte : addr:Machtype.int32 -> data:Machtype.int32 -> unit

external ext_io_in_byte : Machtype.int32 -> Machtype.int32 = "ext_io_in_byte"
val in_byte : addr:Machtype.int32 -> Machtype.int32

external ext_io_in_bytes : Machtype.int32 -> 
                           Machtype.int32 ->
                           Bytebuf.buffer ->
                           unit = "ext_io_in_bytes"
val in_bytes : addr:Machtype.int32 -> 
              len:Machtype.int32 ->
              buf:Bytebuf.buffer -> 
              unit

type timer_event = Thread.thread_event
val timer_create_event : unit -> timer_event
external ext_timer_init : timer_event -> int -> int -> bool -> int -> int
  = "ext_timer_init"
val timer_init :
  event:timer_event ->
  interval:int -> uni:Thread.time_unit -> once:bool -> int
val timer_reinit :
  event:timer_event ->
  interval:int -> uni:Thread.time_unit -> once:bool -> int
external timer_await : timer_event -> int = "ext_timer_await"

(*
** Register a new interrupt handler.
** The system cap is currently the root capability of the kernel.
**
** [stat,isr] =
** io_map_region ~start:int32 ->
**               ~size:int32 -> 
**               ~devname: string ->
**               ~syscap:capability -> status
*)
type isr
type isr_flags =
  | IRQ_NORMAL
  | IRQ_SHARED

type isr_done =
  | IRQ_SERVICED
  | IRQ_UNKNOWN
  
external ext_interrupt_register : 
                             int -> 
                             isr_flags ->
                             string -> 
                             Amoeba.capability -> 
                             Amoeba.status * isr =
         "ext_interrupt_register"

val interrupt_register : irq:int ->
                         flags: isr_flags ->
                         devname:string ->
                         syscap:Amoeba.capability -> 
                         Amoeba.status * isr

external ext_interrupt_unregister : 
                             isr -> 
                             Amoeba.capability ->
                            Amoeba.status =
         "ext_interrupt_unregister"

val interrupt_unregister : isr:isr -> 
                           syscap: Amoeba.capability ->
                           Amoeba.status 

/*
** Wait for an interrupt event.
*/
external ext_interrupt_await : 
                             isr -> 
                             int ->
                             Amoeba.status =
         "ext_interrupt_await"

val interrupt_await : isr:isr -> 
                      timeout:int ->
                      Amoeba.status 

/*
** Acknowledge an interrupt event.
*/
external ext_interrupt_done : 
                             isr -> 
                             isr_done ->
                             Amoeba.status =
         "ext_interrupt_done"

val interrupt_done  : isr:isr -> 
                      service:isr_done ->
                      Amoeba.status 

