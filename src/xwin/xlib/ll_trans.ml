(*
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
**  Last modified:  02/05/02
**
**  Changes:	-- more comments (in english)
**              -- threads support
**              -- Thread support changed
**              -- OS dependent functions moved to Xos
**
**
*)
                                                                     
open Thread
open Xtypes
open Xos


(*
** 
**    Low-Level Transmission with X-server
**
*)

exception BrokenConnection
  
let e = BrokenConnection

(*
** Generic connection channel handlers
*)


let read_channel ~chan ~off ~buf ~len =
    let rec read_it chan off buf len =  
        let nrecv = xos_fun.xos_read ~chan:chan ~buf:buf ~off:off ~len:len in
        if nrecv <= 0 then raise e;
        let left = len - nrecv in
        if left > 0 then read_it chan (off+nrecv) buf left
    in
    read_it chan off buf len


let write_channel ~chan ~off ~buf ~len =
    let rec write_it sock off b len =
        let nsent = xos_fun.xos_write ~chan:chan 
                                      ~buf:b ~off:off ~len:len 
        in
        if nsent <= 0 then raise e;
        let left = len - nsent in
        if left > 0 then write_it chan (off+nsent) buf left
    in
    write_it chan off buf len


      
exception NotADigit of int

let discardInt name curs =
  try
    for i=curs downto 0 do
      let c=name.[i]
      in
      if(c>'9') || (c<'0') then
        raise (NotADigit i)
    done;
    (-1,int_of_string (String.sub name 0 (curs+1)))
  with
    NotADigit i ->
      (i,int_of_string (String.sub name (i+1) (curs-i)))


(*
** Return adresse * port * ecran 
*)

let parseDisplayName name =
  let len = String.length name
  and port = ref 0
  and num = ref 0
  and dname = ref ""
  and semi = ref 0
  in
  try
    let (pos,n1) = discardInt name (len-1)
    in
    if name.[pos]='.' then 
      let (pos,n2) = discardInt name (pos-1)
      in
      port := n2;
      num := n1;
      semi := pos
    else
      (
        port := n1;
        semi := pos
      );
    if(name.[!semi]=':') then
      (String.sub name 0 !semi,!port,!num)
    else
      failwith("Invalid Display Name")
  with
    Failure _ -> failwith("Invalid Display Name")


(*
** Open a X window connection
*)


let openConnectionWithDisplay name =

  let (server_name,dport,dnum)= parseDisplayName name
  in
  let (chan,auth) = xos_fun.xos_open     ~server_name:server_name
                                     ~dpy_port:dport
                                     ~dpy_num:dnum
  in
  (chan,server_name,dnum,auth)


let closeConnection chan =
    xos_fun.xos_close chan

