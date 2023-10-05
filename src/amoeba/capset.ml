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
**    $INITIAL:     (C) 2005 BSSLAB
**    $CREATED:     ?
**    $VERSION:     1.02
**
**    $INFO:
**
**  Capability set utilities
**
**    $ENDOFINFO
**
*)


open Amoeba
open Cmdreg
open Stderr
open Stdcom


let capset_MAX = 8
let short_SIZE = 2
let capset_SIZE = 2 * short_SIZE + capset_MAX * (cap_SIZE + short_SIZE)

(*
** Cap set structure
*)

type suite = { 
    mutable s_object : capability;
    mutable s_current: bool;
}

type capset = {
    mutable cs_initial: int;
    mutable cs_final  : int;
    mutable cs_suite  : suite array;
}




(*
** Convert a capability to a cap set
*)

let cs_singleton cap =
    {
        cs_initial = 1;
        cs_final   = 1;
        cs_suite = [|{
                        s_object  = cap;
                        s_current = true;
                      }|];
    }    


(*
** Get a useable capability from a capset, and retrun this cap.
** Returns the first capability in the set for which std_info returns STD_OK.
** If there are no caps in the set for which std_info returns STD_OK,
** then the last cap in the set is returned and  the err status STD_INFO.
*)

let cs_goodcap cs =

    let err_stat = ref std_OK in
    let lastcap  = ref nilcap in

    try 
    (
        for i = 0 to (cs.cs_final - 1)
        do
            let s = cs.cs_suite.(i) in
            if (s.s_current = true) then
            begin
                lastcap := s.s_object;
                let (err,buf) = (std_info ~cap:(!lastcap) ~bufsize:50) in
                    err_stat := err;

                if (!err_stat = std_OK || !err_stat = std_OVERFLOW) then
                begin
                    err_stat := std_OK;
                    raise Exit;
                end;
            end;
        done;
        (* no good found ! *)
        (!err_stat,nilcap)
    ) with 
      (* one good found, use it *)
      | Exit -> (!err_stat,!lastcap)

        

(*
** Get a capability from a capset giving preference to a working capability.
** If there is only one cap in the set, this cap is returned.  If and only
** if there is more than one, try std_info on each of them to obtain one
** that is useable, and return this one.  If none of the multiple
** caps work, the last one is returned.  Callers who need to know whether the
** cap is useable should use cs_goodcap(), above.  Returns STD_OK, unless
** the capset has no caps, in which case, returns STD_SYSERR.
*)

let cs_to_cap cs =
    let cs_size = ref 0 in
    let cs_save = ref (cs.cs_suite.(0)) in

    for i = 0 to (cs.cs_final - 1)
    do
        if ((cs.cs_suite.(i)).s_current = true ) then
        begin
            cs_save := cs.cs_suite.(i);
            incr cs_size;
        end    
    done;

    match (!cs_size) with
    | 0 -> (std_SYSERR,nilcap);
    | 1 -> (std_OK,(!cs_save).s_object);
    | _ -> cs_goodcap cs


        
(*
** Zero capset - simple
*)

let nilcapset = cs_singleton nilcap

(*
** Return a fresh capset and copy the original contents
*)

let cs_copy cs =
    {
        cs_initial = cs.cs_initial;
        cs_final   = cs.cs_final;
        cs_suite   = (
            let cn = Array.length cs.cs_suite in
            let ct = cs.cs_suite in
            let ct' = Array.create cn {s_object=nilcap;s_current=false} in
            for i = 0 to cn-1
            do
                ct'.(i).s_object <- cap_copy ct.(i).s_object;
                ct'.(i).s_current <- ct.(i).s_current;
            done;
            ct'
        );

    }


