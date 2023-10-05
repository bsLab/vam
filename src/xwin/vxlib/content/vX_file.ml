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
**    $CREATED:     4.7.2005
**    $VERSION:     1.01
**
**    $INFO:
**
**  File tree management widgets.
**
**    $ENDOFINFO
**
*)

open VX_types
open VX_common
open VX_tree
open Amoeba
open Unix
open Stderr


(*
** Starting at tree node 'node', load content of specified directory 'path'
** (absolute path). Returns status of operation. Reads all entries
** of the specified directory and checks each entry for directory
** or other object kind type. For new found directories, new tree 
** branches are created. The specified 'treew' argument is the 
** associated tree widget.
*)
let unix_file_tree treew path node =
    let tree = ref [] in
    let failed = not (protects (
        let dirhd = Unix.opendir path in
        let dirs = ref [] in
        let files = ref [] in
        protect (
          while(true)
          do
            let entry = Unix.readdir dirhd in
            
            if entry <> "" &&
               entry <> "." &&
               entry <> ".." then
            begin
                protect (
                  let stat = Unix.stat (path^"/"^entry) in
                  match stat.st_kind with
                  | S_DIR ->
                      dirs := !dirs @ [entry];
                  | _ ->
                      files := !files @ [entry];
                );
            end;
          done;
        );

        (*
        ** Alphabetical sorting...
        *)
        let dirs' = List.sort (fun a b -> if a > b then 1 
                                  else if a < b then -1
                                  else 0) !dirs in
        let files' = List.sort (fun a b -> if a > b then 1 
                                  else if a < b then -1
                                  else 0) !files in
        List.iter (fun entry -> let n = treew#new_node entry in
                                  tree := !tree @ [Node n]) dirs';
        List.iter (fun entry -> let l = treew#new_leaf entry in
                                  tree := !tree @ [Leaf l]) files';
      ))
      in
    if failed then std_SYSERR else 
    begin
        treew#tree_add_s node !tree;
        treew#update;
        std_OK
    end
