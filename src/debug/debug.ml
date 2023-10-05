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
**    $CREATED:     12.5.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  Debugging print support
**
**    $ENDOFINFO
**
*)

open Thread
open Printf

let debug_level = ref 1
let debug_chan = ref Pervasives.stdout
let debug_log = ref false
let debug_lock = mu_create ()

(*
**
*)

let debug level fmt =
    let debug_silent = level < !debug_level in

    if !debug_log && not debug_silent then
    begin
        (*
        ** extract the message 
        *)
        let message =   
            let fmt = (Obj.magic fmt : string) in
            let len = String.length fmt in
            let dest = Buffer.create (len + 16) in
            let rec doprn i =
            if i >= len then 
            begin
                let res = Buffer.contents dest in
                Buffer.clear dest;  (* just in case sprintf is partially applied *)
                Obj.magic res
            end 
            else
            match String.unsafe_get fmt i with
            | '%' -> scan_format fmt i cont_s cont_a cont_t
            |  c  -> Buffer.add_char dest c; doprn (succ i)
            and cont_s s i =
                Buffer.add_string dest s; doprn i
            and cont_a printer arg i =
                Buffer.add_string dest (printer () arg); doprn i
            and cont_t printer i =
                Buffer.add_string dest (printer ()); doprn i
          in doprn 0 in
    
        ()
        (* TODO *)
    end;

    let print chan fmt =
        let fmt = (Obj.magic fmt : string) in
        let len = String.length fmt in
        let fl () = Pervasives.flush chan in
        let rec doprn i =
            if i >= len then fl ();
            if i >= len then Obj.magic () else
            match String.unsafe_get fmt i with
            | '%'  -> scan_format fmt i cont_s cont_a cont_t
            | '\n' -> if not debug_silent then
                      begin   
                        output_char chan '\n';
                        Pervasives.flush chan;
                      end;
                      doprn (succ i);
            |  c   -> if not debug_silent then
                            output_char chan c; 
                     doprn (succ i)
          and cont_s s i =
            if not debug_silent then output_string chan s; doprn i
          and cont_a printer arg i =
            if not debug_silent then printer chan arg; doprn i
          and cont_t printer i =
            if not debug_silent then printer chan; doprn i
          in 
          doprn 0;
        in

    (*
    ** print the message to the standard output channel
    *)
    print !debug_chan fmt



