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
**    $AUTHORS:     Stefan Bosse
**    $INITIAL:     (C) 2003 BSSLAB
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.02
**
**    $INFO:
**
** High level directory service stubs
**
**
**    $ENDOFINFO
**
*)


val _ds_ncolumns : int ref
val _ds_colnames : string array ref
val _ds_NCOLUMNS : int
val _ds_colmasks : int array ref

(*
** Returns the (status,capability) tuple for the directory lookup
** of 'name'. The server capability is an optional argument.
*)
val dir_lookup :
  root:Amoeba.capability ->
  name:string -> Amoeba.status * Amoeba.capability

(*
** Append a new object capability under with the given name
** to the directory tree.
*)
val dir_append :
  root:Amoeba.capability ->
  name:string -> obj:Amoeba.capability -> Amoeba.status

(*
** Rename a directory entry.
*)
val dir_rename :
  dir:Amoeba.capability ->
  oldname:string -> 
  newname:string -> 
  Amoeba.status

(*
** This function sets the default column masks used when appending names
** with the dir/name interface.  It should be called when the masks as
** specified by the environment variable SPMASK are not what we need.
**
*)
val dir_set_colmasks : int array -> int -> unit

(*
** Delete a directory entry spicified with 'name'.
*) 
val dir_delete : root:Amoeba.capability -> name:string -> Amoeba.status

(*
** Create a new directory for server 'server'. Returns the new
** directory capability set. This capset can then appended somewhere
** in the directory tree.
*)
val dir_create : server:Amoeba.capability -> Amoeba.status *
                 Amoeba.capability


(*
** UNIX like directory interface: open - readnext -close. The directory
** capability can be resolved with 'dir_lookup'.
*)
   
type dir_row = 
{
    mutable dr_name: string;
    mutable dr_time: int;
    mutable dr_cols: int array;
}

type dir_desc =
{
    mutable dir_rows    : dir_row array;
    mutable dir_curpos  : int;
    mutable dir_ncols   : int;
    mutable dir_nrows   : int;
    mutable dir_colnames: string array;
}

val nil_dir_desc: dir_desc 

(*
** Return dir structure for directory 'dir'.
*)

val dir_open: dir:Amoeba.capability -> Amoeba.status * dir_desc

(*
** Get the next row (name,colmasks) entry.
*)

exception Dir_empty

val dir_next: dirdesc:dir_desc -> dir_row

(*
** Close directory. NOP
*)

val dir_close: dirdesc:dir_desc -> unit

