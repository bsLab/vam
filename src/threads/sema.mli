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
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
** Semaphore Module
**
** Thread synchronization using counting semaphores
**
** These  operations  implement  counting  semaphores. What follows is an
** intuitive explanation of semaphores, not a formal definition:
** A semaphore contains a non-negative integer variable,  usually called
** its level. The two important operations on semaphores are up and down,
** which increment and decrement the level, respectively. However, when a
** call  to  down would decrement the level below zero, it blocks until a
** call to up is made (by another thread) that increments the level above
** zero. This is done in such a way that the level will never actually go
** negative. You could also say that the total number of  completed down
** calls for a particular semaphore will never exceed the total number of
** up calls (not necessarily completed), plus its initial level.
**
**
**  $ENDINFO
**
*)

(*
** Semaphores with FIFO behaviour. The first thread gone to
** sleep will be waked up first. Abstract type handled by
** AMUTHR.
*)
type semaphore

(*
** A  semaphore  must  be  initialized to a certain level by calling this
** function. 
** The initial level must not be negative.
*)
external sema_create : level:int -> semaphore = "am_sema_create"

(*
** Dummy semaphore
*)
val nilsema : semaphore

(*
** Sema down operation. If count value is zero, block
** untill a sema up operation was performed (by another thread).
*)
external sema_down : semaphore -> unit = "am_sema_down"

(*
** Sema try down operation. If count value is zero, do nothing and
** return false, else decrement the semaphore counter and return true.
** Second argument specifies timeout (<0,0,>0).
*)
external sema_trydown : semaphore -> int -> bool = "am_sema_trydown"

(*
** Increment a semaphore counter. If currently zero, and threads
** waiting for this sema, wakeup them in FIFO order. Don't increment 
** the sema counter !
*)
external sema_up : semaphore -> unit = "am_sema_up"

(*
** Return current level of semaphore
*)
external sema_level :
    semaphore -> int = "am_sema_level"

(*
** Pretty printer for a semaphore.
*)

external am_sema_print : semaphore -> string = "am_sema_print"
val print_amoeba_sema : semaphore -> unit
