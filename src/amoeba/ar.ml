(*
**  THIS SOFTWARE IS OWNED AND COPYRIGHTED BY
**
**    ###     ####   ####               #         ##         #####
**    #  #    #      #                 #         # #        #     #
**    #   #   #      #                #         #  #       #      #
**    #   #   #      #               #         #   #      #      #   
**    ####    ####   ####  ####     #         ######     ########
**    #   #      #      #          #         #     #    #      #
**    #   #      #      #         #         #      #   #       #
**    #  #       #      #        #         #       #  #       #
**    ###     ####   ####       ######### #        # #########
**
**    Stefan Bosse (c) 2003
**   
**  THIS SOFTWARE MAY NOT BE COPIED, EXTRACTED, MODIFIED, OR 
**  OTHERWISE USED IN A CONTEXT OUTSIDE OF THE VAM SYSTEM.
** 
*)

open StdLabels

(*
** Ascii representation of ports and capabilities
*)

let mod_ar_ver = 1.01

open Amoeba
open Printf
open String
open List
open Format
open Stderr
open Capset

module Str = StrLabels
open Str

(*
** Convert a port to an array
*)
 
let port_to_array port =
    let (Port str) = port in
    let arr = Array.create port_SIZE 0 in

    if (String.length str <> port_SIZE) then
        failwith "port_to_array: invalid port";

    for i = 0 to (port_SIZE-1)
    do
        arr.(i) <- (int_of_char (String.get str i));  
    done;
    arr



(*
** Return an ASCII representation of a capability
*)


let ar_cap cap =
    let capport = cap.cap_port in
    let cappriv = cap.cap_priv in
    let (Objnum capobj) = cappriv.prv_object in
    let (Rights_bits caprights) = cappriv.prv_rights in
    let caprand = cappriv.prv_random in


    let pa = port_to_array capport in  
    let ra = port_to_array caprand in
    
    let str =
        sprintf "%x:%x:%x:%x:%x:%x/%d(%x)/%x:%x:%x:%x:%x:%x"
        (pa.(0)) (pa.(1)) (pa.(2)) (pa.(3)) (pa.(4)) (pa.(5))
        capobj caprights 
        (ra.(0)) (ra.(1)) (ra.(2)) (ra.(3)) (ra.(4)) (ra.(5))
    in
    str

(*
** Return an ASCII representation of an Amoeba port
*)


let ar_port port =


    let pa = port_to_array port in  
    
    let str =
        sprintf "%x:%x:%x:%x:%x:%x"
        (pa.(0)) (pa.(1)) (pa.(2)) (pa.(3)) (pa.(4)) (pa.(5))
    in
    str


(*
** Return an ASCII representation of an Amoeba private part
*)


let ar_priv priv =
    let (Objnum capobj) = priv.prv_object in
    let (Rights_bits caprights) = priv.prv_rights in
    let caprand = priv.prv_random in


    let ra = port_to_array caprand in
    
    let str =
        sprintf "%d(%x)/%x:%x:%x:%x:%x:%x"
        capobj caprights 
        (ra.(0)) (ra.(1)) (ra.(2)) (ra.(3)) (ra.(4)) (ra.(5))
    in
    str

    

(*
** Convert a port string representation in the form
**
**  "x:x:x:x:x:x"       [x: hexadecimal]
**
** to an Amoeba port.
*)


let ar_toport portstr =
    let pstr = String.create port_SIZE in
    let plist = split ~sep:(regexp ":") portstr in

    if (List.length plist <> port_SIZE) then
        failwith ("ar_toport: invalid port string: "^portstr);

    for i = 0 to (port_SIZE-1)
    do
        String.set pstr i 
            (char_of_int ((int_of_string ("0x"^(List.nth plist i))) land 0xff));
    done;
    (Port pstr)    


(*
** Convert a capability string representation in the form
**
**  "x:x:x:x:x:x/d(x)/x:x:x:x:x:x"  [x: hexadecimal, d:decimal]
**  <- port ->objnum/rights<- random -> 
**
** to an Amoeba port.
*)


let ar_tocap capstr =
    let split_1 = Str.split ~sep:(regexp "/") capstr in

    if (List.length split_1 <> 3) then
        failwith ("ar_tocap: invalid capability string: "^capstr);

    let port_list = Str.split ~sep:(regexp ":") (List.nth split_1 0) in
    let acc_list = Str.split ~sep:(regexp "(")  (List.nth split_1 1) in
    let rand_list = Str.split ~sep:(regexp ":") (List.nth split_1 2) in

    if(List.length port_list <> port_SIZE) then
        failwith "ar_tocap: invalid port string part";
    if(List.length acc_list <> 2) then
        failwith "ar_tocap: invalid objectnum/rights string part";
    if(List.length rand_list <> port_SIZE) then
        failwith "ar_tocap: invalid port string part";
    
    let port_str = String.create port_SIZE in
 
    for i = 0 to (port_SIZE-1)
    do
        String.set port_str i 
            (char_of_int (
                (int_of_string ("0x"^(List.nth port_list i))) land 0xff));
    done;

    let rand_str = String.create port_SIZE in
 
    for i = 0 to (port_SIZE-1)
    do
        String.set rand_str i 
            (char_of_int (
                (int_of_string ("0x"^(List.nth rand_list i))) land 0xff));
    done;

    let objnum = int_of_string (List.nth acc_list 0) in
    let rights_field = Str.split ~sep:(regexp ")") (List.nth acc_list 1) in
    let rights = int_of_string ("0x"^(List.nth rights_field 0)) in 

    {
        cap_port = (Port port_str);
        cap_priv = {
                        prv_object = (Objnum objnum);
                        prv_rights = (Rights_bits rights);
                        prv_random = (Port rand_str);
                    }
    }


    
(*
** Convert a private string representation in the form
**
**  "d(x)/x:x:x:x:x:x"  [x: hexadecimal, d:decimal]
**  objnum(rights)<- random -> 
**
** to an Amoeba private structure.
*)


let ar_topriv privstr =
    let split_1 = Str.split ~sep:(regexp "/") privstr in

    if (List.length split_1 <> 2) then
        failwith ("ar_topriv: invalid private string: "^privstr);

    let acc_list = Str.split ~sep:(regexp "(")  (List.nth split_1 0) in
    let rand_list = Str.split ~sep:(regexp ":") (List.nth split_1 1) in

    if(List.length acc_list <> 2) then
        failwith "ar_tocap: invalid objectnum/rights string part";
    if(List.length rand_list <> port_SIZE) then
        failwith "ar_tocap: invalid port string part";
    

    let rand_str = String.create port_SIZE in
 
    for i = 0 to (port_SIZE-1)
    do
        String.set rand_str i 
            (char_of_int (
                (int_of_string ("0x"^(List.nth rand_list i))) land 0xff));
    done;

    let objnum = int_of_string (List.nth acc_list 0) in
    let rights_field = Str.split ~sep:(regexp ")") (List.nth acc_list 1) in
    let rights = int_of_string ("0x"^(List.nth rights_field 0)) in 

    {
        prv_object = (Objnum objnum);
        prv_rights = (Rights_bits rights);
        prv_random = (Port rand_str);
    }


let ar_cs cs =
    let str = ref (Printf.sprintf "Initial: %d Final: %d\n"
                    cs.cs_initial cs.cs_final) in
    if (Array.length cs.cs_suite > 0) then
    begin
        for i = 0 to cs.cs_final-1
        do
            str := !str ^ (Printf.sprintf "Current: %s Cap=%s\n"
                        (if cs.cs_suite.(i).s_current then "true" else "false")
                        (ar_cap cs.cs_suite.(i).s_object));
        done
    end
    else
    begin
        str := !str ^ "Empty suite.\n";
    end;
    !str

    
(*
** Some synonymes
*)

let c2a = ar_cap 
let a2c = ar_tocap

(*
** Some pretty printers
*)

let print_amoeba_status s =
    open_hvbox 0;
    print_string ("Status '" ^ (err_why s) ^ "'");
    close_box ()

let print_amoeba_port p =
    open_hvbox 0;
    print_string ("Port '" ^ (ar_port p) ^ "'");
    close_box ()

let print_amoeba_priv p =
    open_hvbox 0;
    print_string ("Private '" ^ (ar_priv p) ^ "'");
    close_box ()

let print_amoeba_cap c =
    open_hvbox 0;
    print_string ("Capability '" ^ (ar_cap c) ^ "'");
    close_box ()

