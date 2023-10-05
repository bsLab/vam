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
**    $INITIAL:     (C) 2005
**    $CREATED:     23.1.2005
**    $MODIFIED:    
**    $VERSION:     1.14
**    $INFO:
**
**  Extract the structure of the document: the table of content.
**
**    $ENDOFINFO
**
*)



open Doc_core  
open Printf

let cur_sb = ref empty_block 

let syntax_error s =
    failwith (
            "html_of_tree: "^
            s^
            " in "^
            (!((!cur_sb).s_name))^
            " line "^
            (string_of_int (!cur_sb.s_line))
    )



type env = {
    mutable doc_name: string;

    mutable ignore_head: bool;
    mutable ignore_childs: bool;

    mutable cur_s1: section option;
    mutable cur_s2: section option;
    mutable cur_s3: section option;
    mutable cur_s4: section option;
    mutable cur_mp: section option;
    mutable cur_mp_paragr : section option;

    (*
    ** Table of content
    *)

    mutable toc: section;
    mutable cur_sec: section;
}

let toc_of_tree ~ds ~sections =
    let rec doc_head = { sec_parent = doc_head; sec_childs = []; 
                         sec_name = ""; sec_type = "Main"; sec_num=0;} 
    in  

    let sec_num = ref 1 in
    let env = {
        doc_name        = "";
        ignore_head     = false;
        ignore_childs   = false;
        cur_s1          = None;
        cur_s2          = None;
        cur_s3          = None;
        cur_s4          = None;
        cur_mp          = None;
        cur_mp_paragr   = None;

        toc             = doc_head;
        cur_sec         = doc_head;
        } in
    (*
    ** External sections
    *)
    if (sections <> []) then
    List.iter (fun s ->
        let sec = 
            match s with
            | Sec_s1 s -> 
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S1";sec_num= !sec_num} in
                         env.cur_s1 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s2 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S2";sec_num= !sec_num} in
                         env.cur_s2 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s3 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S3";sec_num= !sec_num} in
                         env.cur_s3 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_s4 s ->
                         let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= s;sec_type="S4";sec_num= !sec_num} in
                         env.cur_s4 <- Some sec;
                         incr sec_num;
                         sec;
            | Sec_mp m -> let sec = {sec_parent=env.cur_sec;sec_childs=[];
                          sec_name= m;sec_type="MP";sec_num= !sec_num} in
                         env.cur_mp <- Some sec;
                         incr sec_num;
                         sec;
        in
        env.cur_sec.sec_childs <- env.cur_sec.sec_childs @ [sec];
        env.cur_sec <- sec;

        ) sections;
    

    (*
    ** Parse an attribute list
    *)

    (*
    ** Convert a text structure element to a string
    *)

    let str_of_text tln =
        let str = ref "" in

        List.iter (fun t ->
            match t.s_content with
            | S_Text t -> List.iter (fun s ->
                                        str := !str ^ " " ^ s;
                          ) t;
            | _ -> syntax_error 
                    "str_of_text: not a S_Text element";
        ) tln;
        !str 
        in    
    (*
    ** Extract all text from a structure element inclusive his childs.
    *)

    let str_of_struc s =
        let str = ref "" in

        let rec iter se =
            (
                match se.s_content with   
                | S_Text t -> List.iter (fun s ->
                                        str := !str ^ " " ^ s;
                          ) t;
                | _ -> List.iter iter se.s_childs;
            );
        in   
        iter s;
        !str
        in
    (*
    ** Return glued text of structure list
    *)
    let str_of_childs ds =
        let str = ref "" in
        List.iter (fun d -> str := !str ^ (str_of_struc d)) ds;
        !str
        in   
    (*
    ** Add a new section to the TOC and change to the new section
    *)

    let new_sec env sec =
        env.cur_sec.sec_childs <- env.cur_sec.sec_childs @ [sec];
        env.cur_sec <- sec;
        in

    (*
    ** A section was closed. Change to the parent again.
    *)

    let close_sec env sec =
        env.cur_sec <- env.cur_sec.sec_parent;
        in                
    (*
    ** The real worker: translate the structure elements.
    *)

    let rec cont_trans ds =

        cur_sb := ds;
        (
          match ds.s_content with
            | S_S1 -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                       let pname = 
                                            (str_of_text dna.s_childs) in
                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S1";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        env.ignore_head <- true;
                                        env.cur_s1 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;
            | S_S2 -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                       let pname = 
                                            (str_of_text dna.s_childs) in
                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S2";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        env.ignore_head <- true;
                                        env.cur_s2 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;
            | S_S3 -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                       let pname = 
                                            (str_of_text dna.s_childs) in

                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S3";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        env.ignore_head <- true;
                                        env.cur_s3 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;
            | S_S4 -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin
                                       let pname = 
                                            (str_of_text dna.s_childs) in

                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="S4";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        env.ignore_head <- true;
                                        env.cur_s4 <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;
            | S_MP -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                       let pname = 
                                            (str_of_text dna.s_childs) in
                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="MP";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        new_sec env sec;
                                        env.ignore_head <- true;
                                        env.cur_mp <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;
            | S_MP_paragr -> 
                begin
                    (*
                    ** The first child must be the section name.
                    *)

                    (
                        match ds.s_childs with
                        | dna::tl ->
                            begin
                                match dna.s_content with
                                | S_Name -> 
                                    begin

                                       let pname = 
                                            (str_of_text dna.s_childs) in
                                       let sec = { sec_parent=env.cur_sec;
                                                    sec_childs=[];
                                                    sec_name= pname;
                                                    sec_type="MP_paragr";
                                                    sec_num= !sec_num;
                                        } in
                                        incr sec_num;
                                        env.ignore_head <- true;
                                        env.cur_mp_paragr <- Some sec;
                                    end;
                                | _ -> syntax_error "Section without name"; 
                            end;
                        | [] -> syntax_error "Section without name";
                    );
                end;

        | _ -> ();

        );

        if (ds.s_childs <> []) then
        begin
            if (env.ignore_childs = false) then
            begin
                if (env.ignore_head = false) then
                    List.iter cont_trans ds.s_childs
                else
                begin
                    env.ignore_head <- false;
                    List.iter cont_trans (List.tl ds.s_childs);
                end;            
            end
            else
                env.ignore_childs <- false;
        end;

            

(*
** Action after the current structure element 
*) 



        (
          match ds.s_content with

            | S_S1 -> 
                begin
                    close_sec env Doc_multi_s1;
                    env.cur_s1 <- None;
                end;
            | S_S2 -> 
                begin
                    close_sec env Doc_multi_s2;
                    env.cur_s2 <- None;
                end;
            | S_S3 -> 
                begin
                    close_sec env Doc_multi_s3;
                    env.cur_s3 <- None;
                end;
            | S_S4 -> 
                begin
                    close_sec env Doc_multi_s4;
                    env.cur_s4 <- None;
                end;
            | S_MP -> 
                begin
                    close_sec env Doc_multi_mp;
                    env.cur_mp <- None;
                end;
            | S_MP_paragr -> 
                begin
                    env.cur_mp_paragr <- None;
                end;
            | _ -> ();
            );
    in
#if 0
    try
#endif
        cont_trans ds; env.toc
#if 0
    with
        Failure s -> syntax_error s
#endif                     

(*
** Find the next child section (of same level) following the current
** section (if any). Returns None if there is no next section.
*)
let toc_get_next toc cur =
    if (cur.sec_type <> "Main") then
    begin
        (*
        ** Find the parent in toc tree.
        *)
        let rec find_parent sl =
            match sl with
            | hd::tl ->
                if hd.sec_name <> "" &&
                   hd.sec_name  =  cur.sec_name then
                begin
                    let p = hd.sec_parent in
                    Some p;
                end
                else
                begin
                    let found = find_parent tl in
                    if found = None 
                        then find_parent hd.sec_childs 
                        else found;
                end;
            | [] -> None;  
            in
        match (find_parent toc.sec_childs) with
        | Some p ->
        begin
            (*
            ** Find the next one after current section, if any.
            *)
            let rec find_next sl =
                match sl with
                | hd::tl ->
                    if hd.sec_name <> "" &&
                       hd.sec_name  =  cur.sec_name then
                    begin
                        match tl with
                        | hd'::tl' -> Some hd';
                        | [] -> None;
                    end
                    else
                        find_next tl;
                | [] -> None;  
                in
            find_next p.sec_childs;
        end;
        | None -> None;
    end
    else
        None        

(*
** Find the previous child section (of same level) following the current
** section (if any). Returns None if there is no next section.
*)
let toc_get_prev toc cur =
    if (cur.sec_type <> "Main") then
    begin
        (*
        ** Find the parent in toc tree.
        *)
        let rec find_parent sl =
            match sl with
            | hd::tl ->
                if hd.sec_name <> "" &&
                   hd.sec_name  =  cur.sec_name then
                begin
                    let p = hd.sec_parent in
                    Some p;
                end
                else
                begin
                    let found = find_parent tl in
                    if found = None 
                        then find_parent hd.sec_childs 
                        else found;
                end;
            | [] -> None;  
            in
        match (find_parent toc.sec_childs) with
        | Some p ->
        begin
            (*
            ** Find the next one after current section, if any.
            *)
            let prev = ref None in
            let rec find_prev sl =
                match sl with
                | hd::tl ->
                    if hd.sec_name <> "" &&
                       hd.sec_name  =  cur.sec_name then
                    begin
                        !prev;
                    end
                    else
                    begin
                        prev := Some hd;
                        find_prev tl;
                    end;
                | [] -> None;  
                in
            find_prev p.sec_childs;
        end;
        | None -> None;
    end
    else
        None        
