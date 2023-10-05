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
**    $INITIAL:     (C)
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**  Server side of standard paramter handling.
**
**    $ENDOFINFO
**
*)
open Amoeba
open Stdcom
open Stderr
open Buf
open Printf 

(*
** Type of parameter value
*)
type param_type =
    | Param_int of int
    | Param_str of string
    | Param_cap of capability

type stdsrv_param = {
    mutable p_name : string;        (* name of param *)
    mutable p_val  : param_type;    (* current value *)
    mutable p_unit : string;        (* value unit string *)
    mutable p_desc : string;        (* description string *)
    mutable p_set  : param_type -> unit;  (* param set function *)
    mutable p_get  : unit -> param_type;  (* param get function *)
    mutable p_min  : param_type;    (* minimal allowed value *)
    mutable p_max  : param_type;    (* maximal allowed value *)
}

let get_int pv = 
        match pv with
        | Param_int i -> i;
        | _ -> failwith "expected Param_int"

(*
** Set parameters (nparams) specified in paramlsit extracted from reqbuf
*)
let stdsrv_params_set   ~paramlist
                        ~reqbuf
                        ~paramlen
                        ~nparams
    =
  try
  begin
    let pos = ref 0 in
    for i = 1 to nparams
    do
        let pos',pnam = buf_get_string ~buf:reqbuf ~pos: !pos in
        let pos',pval = buf_get_string ~buf:reqbuf ~pos: pos' in
        let rec find_param pl =
            match pl with
            | pa::tl -> 
                if pa.p_name = pnam then
                begin
                    match pa.p_val with
                    | Param_int _ ->
                    begin
                        let i = int_of_string pval in
                        pa.p_set (Param_int i);
                    end;
                    | _ -> ();  
                end
                else
                    find_param tl;
            | [] -> raise (Error std_NOTFOUND);     
            in
        find_param paramlist;
    done;
    std_OK
  end
  with
    | Error stat -> stat

(*
** Store all parameter informations from paramlist in repbuf.
*)

let stdsrv_params_get   ~paramlist
                        ~repbuf
    =
    let pos = ref 0 in
    let nparams = ref 0 in
    List.iter (fun pa ->
            incr nparams;
            let pos' = buf_put_string ~buf:repbuf ~pos: !pos 
                                      ~str:pa.p_name in

            match (pa.p_get ()) with
            | Param_int i -> 
            begin
                let imin = get_int pa.p_min in
                let imax = get_int pa.p_max in
                let str = sprintf "%d..%d %s" imin imax pa.p_unit in
                let pos' = buf_put_string ~buf:repbuf ~pos:pos'
                                          ~str:str in
                let pos' = buf_put_string ~buf:repbuf ~pos:pos'
                                          ~str:pa.p_desc in
 
                let str = sprintf "%d" i in
                let pos' = buf_put_string ~buf:repbuf ~pos:pos'
                                          ~str:str in
                pos := pos';
            end;
            | _ -> ();
        ) paramlist;        
    std_OK, !pos, !nparams

