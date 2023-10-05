open Xtypes
open WX_types
open WX_tree
open Unix


open Amoeba
open Ar
open Dir
open Name
open Stderr
open Stdcom
open Cap_env
open Thread
open Printf
open Sema

open Xnc_widget

exception AmError of status


(*
** One program block entry
*)

type prog_desc = {
    mutable pr_name : string;
    mutable pr_tree : WX_tree.t;
    mutable pr_label : WX_label.t;
    mutable pr_branch : WX_tree.node;
    mutable pr_selected : bool;
    mutable pr_open : bool;
}


type block_desc = {
    mutable pb_name : string;
    mutable pb_desc : WX_label.t;
    mutable pb_leaf : WX_tree.node;
    mutable pb_selected : bool;
}



(*
** CNC program structure (if any)
** The program parser tries to extract block informations
** to get control about machining different program blocks. If there
** is no structured found, the full content is stored in header!
** A local definition call is treated like a pseudo block.
*)
type block_type = 
    (*
    ** The header:
    ** ( %HEADERSTART )
    ** ...
    ** ( %HEADEREND )
    **
    ** or CNC word definitions:
    ** : P1 
    ** ...
    ** ;
    *)
    | Header
    | Block

type block = {
    mutable b_type : block_type;
    mutable b_name : string;
    mutable b_cont : string;
    mutable b_desc : block_desc option;
}

let cnc_file = ref "<Empty>"
let cnc_program = ref []



(*
** Select mode flag
*)

let sel_add = ref false
(*
** Check for select mode (add to = false), and unselect already
** selected objects.
*)

let unsel_all_blocks pl =
    List.iter (fun block ->
        block.pb_desc#configure [Background "lightblue"];
        block.pb_selected <- false;
        ) !pl;  
    pl := []

let sel_all_blocks pl =
    List.iter (fun block ->
        block.pb_desc#configure [Background "steelblue"];
        block.pb_selected <- true;
        ) !pl;  
    pl := []



(*
** A program block was selected. Mark it.
*)

let select_block block  =
    let d =  match block.b_desc with
             | Some d -> d;
             | None -> failwith "block";
         in
    if (d.pb_selected = false ) then
    begin
        d.pb_desc#configure [Foreground "Black"];
        d.pb_selected <- true;
    end else
    begin
        d.pb_desc#configure [Foreground "Grey50"];
        d.pb_selected <- false;
    end

(*
** Unselect all 
*)
let unsel_all () =
    let pl = cnc_program in
    List.iter (fun block ->
        let d =  match block.b_desc with
                 | Some d -> d;
                 | None -> failwith "block";
            in
        d.pb_desc#configure [Foreground "Grey50"];
        d.pb_desc#refresh;
        d.pb_selected <- false;
        ) !pl


(*
** Load and parse the program. Return program block list.
** All data untill first defintion or first block occurs is treated
** like header data.
*)
type cnc_data =
    | Header_begin
    | Header_end
    | Block_begin
    | Block_end
    | Def_begin of string
    | Def_end
    | Data of string
    | Comment of string

let load_program progname =
    let fail str =
        output_string Pervasives.stderr str; print_newline ();
        failwith str
        in
    
    (*
    ** Found next non space char
    *)
    let parse_spaces str len start =
        let idx = ref start in
        let exit = ref false in
        while (!idx < len && not !exit)
        do
            if str.[!idx] = ' ' then 
                incr idx
            else
                exit := true;            
        done;
        !idx
        in

    (*
    ** Parse a word delimited with space char
    *)
    let parse_word str len start =
        let str' = ref "" in
        let s1 = " " in
        let idx = ref start in
        let exit = ref false in
        while (!idx < len && not !exit)
        do
            if str.[!idx] <> ' ' then 
            begin
                s1.[0] <- str.[!idx];
                str' := !str' ^ s1;
                incr idx
            end
            else
                exit := true;            
        done;
        !idx,!str'
        in

    let parse_line str =
        (*
        ** Find first character (non space)
        *)
        let len = String.length str in
        if len > 0 then
        begin
          let idx = parse_spaces str len 0 in
          if idx < len then
          begin
            match str.[idx] with
            | ':' -> 
            begin
                (*
                ** get the name
                *)
                let idx = parse_spaces str len idx in
                let idx,str = parse_word str len idx in
                Def_begin str;
            end;
            | ';' -> Def_end;
            | '(' ->
            begin
                let idx = parse_spaces str len (idx+1) in
                let idx,name = parse_word str len idx in
                match name with
                | "%BLOCKSTART" -> Block_begin;
                | "%BLOCKEND"   -> Block_end;
                | "%HEADERSTART"-> Header_begin;
                | "%HEADEREND"  -> Header_end;
                | _ -> Comment str;
            end;
            | _ -> 
            begin
                Data (String.sub str idx (len-idx));
            end;    
          end
          else
            Data "";
        end
        else
            Data str
        in


    printf "%s\n" progname;
    let blocknum = ref 1 in
    let header = ref "" in
    let blocks = ref [] in
    let block = ref "" in   (* block name,block content tuple list *)
    let defs = ref [] in
    let def = ref "" in
    let rest = ref "" in
    let in_header = ref true in
    let in_block = ref false in
    let in_def = ref false in

    let ic = open_in progname in

  try
  begin

    while (true)
    do
        let line = input_line ic in
        (*
        ** Search for : definitions or special ( ) comments
        *)
        let special = parse_line line in
        match special with
        | Header_begin -> 
        begin
            if !in_block then fail "Header_begin";
            in_header := true;
        end;
        | Header_end -> 
        begin
            if not !in_header || !in_block then fail "Header_end";
            in_header := false;
        end;
        | Def_begin name ->
        begin
            if !in_header then in_header := false;
            if !in_def || !in_block then fail "Def_begin";
            in_def := true;
            defs := !defs @ [name]; (* save name for later block resolving *)
            def := !def ^ line ^ "\n";
        end;
        | Def_end ->           
        begin
            if not !in_def then fail "Def_end";
            in_def := false;
            (*
            ** Add defintion to header section
            *)
            header := !header ^ !def ^ line ^ "\n"; 
            def := "";
        end;
        | Block_begin ->
        begin
            if !in_header then in_header := false;
            if !in_block then fail "Block_begin";
            in_block := true;
        end;
        | Block_end ->           
        begin
            if not !in_block then fail "Block_end";
            in_block := false;
            blocks := !blocks @ [{
                        b_type = Block;
                        b_name = (sprintf "BLOCK%d" !blocknum);
                        b_cont = !block;
                        b_desc = None;}];
            incr blocknum;
            block := "";
        end;
        | Data line ->
        begin
            if line = "" then () else
            if !in_header then header := !header ^ line ^ "\n" else
            if !in_block then block := !block ^ line ^ "\n" else
            if !in_def then def := !def ^ line ^ "\n" else
            begin
                (*
                ** Perhaps a defintion call and therefore a
                ** new pseudo block ? Get the name and check it...
                *)
                let len = String.length line in
                let idx,name = parse_word line len 0 in
                if name <> "" then 
                begin
                    if (List.mem name !defs) then
                        blocks := !blocks @ [{
                                        b_type=Block;
                                        b_name=name;
                                        b_cont=name;
                                        b_desc=None;}]
                    else
                    begin
                        (*
                        ** Treat this line as a pseudo block!
                        *)
                        blocks := !blocks @ [
                                    {b_type=Block;
                                     b_name=line;
                                     b_cont=line^"\n";
                                     b_desc=None;}];
                    end;
                end;
            end;
        end;
        | Comment line ->
        begin
            (* ignored *)
            ()                
        end;
    done;
    close_in ic;
    cnc_program := 
                    (if !header <> "" then [{
                                         b_type = Header;
                                         b_name = "HEADER";
                                         b_cont = !header;
                                         b_desc = None;}] else [])@
                    !blocks;
    !cnc_program;
  end
    with | End_of_file ->
         begin
            close_in ic;
            cnc_program :=
                (if !header <> "" then [{b_type = Header;
                                         b_name = "HEADER";
                                         b_cont = !header;
                                         b_desc = None;}] else [])@
                !blocks;
            !cnc_program;
         end;
        | _ -> cnc_program := []; !cnc_program        




let make_prog_blocks progname tree fn =
    let blocks = fn progname in
    let block_map =
        List.map ( fun b ->
                    let label' = new WX_label.t tree#container b.b_name 
                                    [Foreground (
                                        if b.b_type = Block 
                                            then "Grey50"
                                            else "black")]
                        in
                    let pb =
                    {
                        pb_name = b.b_name;
                        pb_desc = label';
                        pb_leaf = (leaf 0 label'#contained);
                        pb_selected = if  b.b_type = Block 
                                        then false
                                        else true;                
                    } in
                    b.b_desc <- Some pb;
                    label'#configure [ Bindings [
                                     ButtonPress, (fun _ -> select_block b);
                                     ]];

                    pb
                 ) blocks
    in
    let map = 
            ( List.map (fun p -> 
                            p.pb_leaf
                        ) block_map
            )
    in
    tree#set_desc map;  
    block_map

(*
** Open/Close event for a program branch. Only one main branch exists!
*)
  
let rec set_notify tree treelist fn =

    let notify leaf = 
        try
        List.iter ( fun p ->
                    let b = p.pr_branch in
                    if (leaf = b) then
                    begin
                        if (p.pr_open = false) then
                        begin
                            p.pr_open <- true;
                            (*
                            ** Load and parse program now.
                            *)
                            let block_map = make_prog_blocks 
                                                    !cnc_file
                                                    p.pr_tree fn in
                            ()
                        end
                        else
                        begin
                            p.pr_tree#destroy_desc;
                            p.pr_open <- false;
                        end;
                    end;
                  ) treelist
        with
            | _ -> ()
    in
    tree#set_notify notify



let tree_desc = ref None 

let prog_change name =
    cnc_file := name;
    
    let tree = xw_main.xw_prog_tree in
    let root = match !tree_desc with
               | Some r -> r ;
               | None -> failwith "not inited"
        in
    root.pr_label#destroy;
    root.pr_tree#destroy_desc;

    let tree_root = new WX_tree.t tree.xw_t_tree#container [] in
    let label_root = new WX_label.t tree.xw_t_tree#container 
                            (Filename.basename !cnc_file) [] in
    let root =
                    {
                        pr_name = !cnc_file;
                        pr_tree = tree_root;
                        pr_label = label_root;
                        pr_branch = (branch true    
                                        label_root#contained
                                        tree_root#contained 
                                     );
                        pr_selected = false;
                        pr_open = false;
                    }  in
    tree_desc := Some root;
    tree.xw_t_tree#set_desc [root.pr_branch];
    set_notify tree.xw_t_tree [root] load_program;
    tree.xw_t_tree#refresh
        

let prog_init xw =
    let tree = xw.xw_prog_tree in
    let tree_root = new WX_tree.t tree.xw_t_tree#container [] in
    let label_root = new WX_label.t tree.xw_t_tree#container 
                            (Filename.basename !cnc_file) [] in
    let root =
                    {
                        pr_name = !cnc_file;
                        pr_tree = tree_root;
                        pr_label = label_root;
                        pr_branch = (branch true    
                                        label_root#contained
                                        tree_root#contained 
                                     );
                        pr_selected = false;
                        pr_open = false;
                    }  in
    
    tree_desc := Some root;

    tree.xw_t_tree#set_desc [root.pr_branch];
    set_notify tree.xw_t_tree [root] load_program

