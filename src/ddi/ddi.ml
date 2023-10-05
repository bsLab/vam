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
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003-2006 BSSLAB
**    $CREATED:     
**    $VERSION:     1.03
**
**    $INFO:
**
**  Device driver Interface
**
**    $ENDOFINFO
**
*)



open Machtype 
open Amoeba
open Thread

(*
** Check an IO port region, starting at address 'start' and with an extent
** of 'size' bytes. If status std_Ok was returned, this
** IO region can be mapped in the current process.
** The system cap is currently the root capability of the kernel.
**
** io_check_region ~start:int32 ->
**                         ~size:int32 -> 
**                         ~syscap:capability -> status
*)
external ext_io_check_region : int32 -> 
                               int32 -> 
                               capability -> status =
         "ext_io_check_region"

let io_check_region ~start 
                    ~size
                    ~syscap =
    ext_io_check_region start size syscap



(*
** Map in an already checked IO port region. After this (successfull) call,
** IO ports can be read and written using the funtions below.
** Access to IO ports not mapped in the process address space raises
** a memory access violatione exception.
** The system cap is currently the root capability of the kernel.
**
** io_map_region ~start:int32 ->
**               ~size:int32 -> 
**               ~devname: string ->
**               ~syscap:capability -> status
*)
external ext_io_map_region : int32 -> 
                             int32 -> 
                             string -> 
                             capability -> status =
         "ext_io_map_region"

let io_map_region ~start 
                  ~size
                  ~devname
                  ~syscap =
    ext_io_map_region start size devname syscap



(*
** Unmap an IO port region, starting at address 'start' and with an extent
** of 'size' bytes. 
**
** io_unmap_region ~start:int32 ->
**                         ~size:int32 -> 
**                         ~syscap:capability -> status
*)
external ext_io_unmap_region : int32 -> 
                               int32 -> 
                               capability -> status =
         "ext_io_unmap_region"

let io_unmap_region ~start 
                    ~size
                    ~syscap =
    ext_io_unmap_region start size syscap



(*
** Write a byte value to an IO port. The port address must be mapped in the
** process.
*)

external ext_io_out_byte : int32 -> 
                           int32 -> 
                           unit =
         "ext_io_out_byte"

let out_byte ~addr
             ~data =
    ext_io_out_byte addr data


(*
** Read a byte value from an IO port. The port address must be mapped in the
** process.
*)

external ext_io_in_byte : int32 -> 
                          int32 =
         "ext_io_in_byte"

let in_byte ~addr =
    ext_io_in_byte addr 


(*
** Read n bytes to buffer from IO port
*)

external ext_io_in_bytes : Machtype.int32 -> 
                           Machtype.int32 ->
                           Bytebuf.buffer ->
                           unit = "ext_io_in_bytes"

let in_bytes ~addr ~len ~buf =
    ext_io_in_bytes addr len buf



(*
** The  timer_init function initializes and installs a new software 
** interval timer. The user specified event 'ev' will be wakedup after 
** the time interval  period in unit
** (SEC, MILLISEC, MICROSEC) relative to the current system time has elapsed.
** If the  once argument is equal zero, the timer function will be called 
** periodically, else only one time.
** The timer_reinit function changes (resets or removes) an already installed
** timer handler.
*)

type timer_event = Thread.thread_event

let timer_create_event () =
    ((thread_create_event ()):timer_event)

external ext_timer_init : timer_event -> int -> int -> bool ->
                          int ->
                          int =
         "ext_timer_init"


let timer_init ~event ~interval ~uni ~once =
    let tunit = 
        match uni with
        | SEC -> 1000000;
        | MILLISEC -> 1000;
        | MICROSEC -> 1 in

    ext_timer_init event interval tunit once 0


let timer_reinit ~event ~interval ~uni ~once =
    let tunit = 
        match uni with
        | SEC -> 1000000;
        | MILLISEC -> 1000;
        | MICROSEC -> 1 in
    ext_timer_init event interval tunit once 1


(*
** Wait for the timer event
** Returns nregative value if the call was interrupted.
*)

external timer_await : timer_event ->
                       int =
         "ext_timer_await"



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
                             capability -> status * isr =
         "ext_interrupt_register"

let interrupt_register ~irq
                       ~flags
                       ~devname
                       ~syscap =
    ext_interrupt_register irq flags devname syscap

external ext_interrupt_unregister : 
                             isr -> 
                             Amoeba.capability ->
                            Amoeba.status =
         "ext_interrupt_unregister"   

let interrupt_unregister ~isr
                         ~syscap =
    ext_interrupt_unregister isr syscap


external ext_interrupt_await : 
                             isr -> 
                             int ->
                             Amoeba.status =
         "ext_interrupt_await"   

let interrupt_await ~isr
                    ~timeout =
    ext_interrupt_await isr timeout

external ext_interrupt_done : 
                             isr -> 
                             isr_done ->
                             Amoeba.status =
         "ext_interrupt_done"   

let interrupt_done ~isr
                   ~service =
    ext_interrupt_done isr service
    

