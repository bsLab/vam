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
open Xnc_prog

exception AmError of status

(*
** Directory entry (branches)
*)

type dir_desc = {
    dir_name : string;
    dir_path : string;
    dir_tree : WX_tree.t;
    dir_label : WX_label.t;
    dir_branch : WX_tree.node;
    mutable dir_selected : bool;
    mutable dir_open : bool;
}
(*
** File entry
*)

type file_desc = {
    file_name : string;
    file_path : string;
    file_label : WX_label.t;
    file_leaf : WX_tree.node;
    mutable file_selected : bool;
}
(*
** List of all selected files
*)

let sel_files = ref []
let cur_path = ref ""
let cur_sel = ref ""


(*
** Select mode flag
*)

let sel_add = ref false
(*
** Check for select mode (add to = false), and unselect already
** selected objects, both files and directory lists.
*)

let unsel_files fl =
    List.iter (fun file ->
        file.file_label#configure [Background "lightblue"];
        file.file_selected <- false;
        ) !fl;  
    fl := []

let sel_file_check fl =
    if (!sel_add = false) then
    begin
        unsel_files fl;
    end

(*
** A file was selected. Mark it.
*)

let select_file file fl =
    if (file.file_selected = false ) then
    begin
        sel_file_check fl;
        file.file_label#configure [Background "steelblue"];
        file.file_selected <- true;
        let tpath = if file.file_path = "/"
                        then
                            "/"^file.file_name
                        else
                            file.file_path^"/"^file.file_name
                    in

        (*
        ** Add the file to the selection list
        *)
        if (List.mem file !fl) = false then
            fl := !fl @ [file];
        cur_sel := tpath;
        (*
        ** Change program block tree
        *)
        prog_change tpath;
        
    end else
    begin
        file.file_label#configure [Background "lightblue"];
        file.file_selected <- false;
        (*
        ** Remove file from the selected list
        *)
        fl := List.filter 
                    ( fun e ->
                        if e <> file then 
                            true
                        else
                            false;       
                    ) !fl ;
    end

(*
** Unselect all after an operation (copy, delete, ...)
*)
let unsel_all () =
    let fl = sel_files in
    List.iter (fun file ->
        file.file_label#configure [Background "lightblue"];
        file.file_label#refresh;
        file.file_selected <- false;
        ) !fl;  
    fl := [];
    cur_path := "";
    cur_sel := ""
    
(*
** Load a unix directory. Return sub dir and file list.
*)

let load_unix_dir dirname =
    Db.Pr.ss 10 "load_unix_dir" dirname;

    let filenames' = Utils.list_dir dirname in
    let subdirs = List.fold_left (fun files filename ->
          if filename <> "." && filename <> ".." then
            let fullname = Filename.concat dirname filename in
            let stats = Unix.lstat fullname in
            if stats.st_kind = S_DIR then filename::files else
              files
          else files
      ) [] filenames' in
    let filenames = List.filter ( fun filename ->
                    if filename <> "." && filename <> ".."  &&
                    ( 
                        let fullname = Filename.concat dirname filename in
                        (Unix.lstat fullname).st_kind <> S_DIR
                    )
                    then
                        true
                    else
                        false
                    ) filenames' in
    (Sort.list (fun a b ->
                    let na,_ = a in
                    let nb,_ = b in
                    na <= nb) (List.map (fun d -> d,std_OK)
                                subdirs),
     Sort.list (fun a b ->
                   let na,_ = a in
                   let nb,_ = b in
                   na <= nb) (List.map (fun f -> f,std_OK) filenames))



(*
** Load a directory and build the subtree. Returns descriptor list
** for branches (sub directories). 
*)

let make_sub_dir dir tree f =
    let (dirs,files) = f dir in
    let fl = sel_files in
 
    let dir_map =
        List.map ( fun se ->
                    let s,e = se in
                    let tree' = (new WX_tree.t tree#container []) in
                    let label' = (new WX_label.t tree#container s 
                                    (if e <> std_OK 
                                        then [Foreground "blue"] else [])) in
                    let dd =
                    {
                        dir_name = s;
                        dir_path = dir;
                        dir_tree = tree';
                        dir_label = label';
                        dir_branch = (branch true 
                                        label'#contained 
                                        tree'#contained
                                     );
                        dir_selected = false;                         
                        dir_open = false;
                    } in
                    dd
                 ) dirs 
    in

    let file_map =
        List.map ( fun se ->
                    let s,e = se in
                    let label' = (new WX_label.t tree#container s 
                                    (if e <> std_OK then [Foreground "blue"]
                                        else [])) in
                    let fd =
                    {
                        file_name = s;
                        file_path = dir;
                        file_label = label';
                        file_leaf = (leaf 0 label'#contained);
                        file_selected = false;                         
                    } in
                    label'#configure [ Bindings [
                                     ButtonPress, (fun _ -> select_file fd fl);
                                     ]];

                    fd
                 ) files
    in
    let map = 
            ( List.map (fun d -> 
                            d.dir_branch
                        ) dir_map
            ) @
            ( List.map (fun f -> 
                            f.file_leaf
                        ) file_map
            ) 
    in
    tree#set_desc map;
    dir_map

(*
** Open/Close event for directory branches
*)
  
let rec set_notify tree treelist f =

    let fl = sel_files in
    
    (*
    ** Is path s a subdir of p ?
    ** F.e. s="/tmp/dir1"
    **      p="/tmp"  -> true
    **
    *)

    let check_path p s =
        let pl = Str.split (Str.regexp "/") p in
        let sl = Str.split (Str.regexp "/") s in
        let pn = List.length pl in
        let eq = ref true in
        for i = 0 to pn-1
        do
            if (List.nth pl i) <> (List.nth sl i) then
                eq := false;
        done;                     
        !eq                                        
    in                


    let notify leaf = 

        List.iter ( fun d ->
                    let b = d.dir_branch in
                    if (leaf = b) then
                    begin
                        let path = if d.dir_path = "/" then 
                                        "/"^d.dir_name 
                                   else
                                     d.dir_path^"/"^d.dir_name 
                        in 

                        if (d.dir_open = false) then
                        begin
                            d.dir_label#configure [Foreground "red"];
                            let dirmap = make_sub_dir 
                                                 path
                                                 d.dir_tree f in
                            d.dir_open <- true;
                            d.dir_label#configure [Foreground "black"];

                            set_notify d.dir_tree dirmap f;                    
                        end
                        else
                        begin

                            d.dir_tree#destroy_desc;   
                            d.dir_open <- false;


                            fl := List.filter 
                                  ( fun f ->
                                        
                                    let fpath = if f.file_path = "/" then 
                                                    "/"^f.file_name 
                                                else 
                                                    f.file_path^"/"^f.file_name 
                                    in 

                                    if (check_path path fpath) = false
                                    then
                                        true
                                    else
                                        false;
                                   ) !fl;
                        end;
                    end;
                  ) treelist
    in
    tree#set_notify notify


let file_init xw =
    let tree = xw.xw_tree in
    let tree_root = new WX_tree.t tree.xw_t_tree#container [] in
    let label_root = new WX_label.t tree.xw_t_tree#container "UNIX" [] in
    let dir_unix_root =
                    {
                        dir_name = "";
                        dir_path = "/";
                        dir_tree = tree_root;
                        dir_label = label_root;
                        dir_branch = (branch true    
                                        label_root#contained
                                        tree_root#contained 
                                     );
                        dir_selected = false;
                        dir_open = false;
                    }  in
    tree.xw_t_tree#set_desc [dir_unix_root.dir_branch];
    set_notify tree.xw_t_tree [dir_unix_root] load_unix_dir;
    ()

