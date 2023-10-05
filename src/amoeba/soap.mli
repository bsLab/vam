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

(*
** SOAP high level dirctory module
*)

val _version : string


(*
** Some miscellaneous constants for this directory service:
*)
val sp_BUFSIZE : int
val sp_MAXCOLUMNS : int
val sp_NTRY : int

(*
** Commands:
*)

val sp_CREATE : Amoeba.command
val sp_DISCARD : Amoeba.command
val sp_LIST : Amoeba.command
val sp_APPEND : Amoeba.command
val sp_CHMOD : Amoeba.command
val sp_DELETE : Amoeba.command
val sp_LOOKUP : Amoeba.command
val sp_SETLOOKUP : Amoeba.command
val sp_INSTALL : Amoeba.command
val sp_PUTTYPE : Amoeba.command
val sp_REPLACE : Amoeba.command
val sp_GETMASKS : Amoeba.command
val sp_GETSEQNR : Amoeba.command

(*
** Errors
*)

val sp_UNAVAIL : Amoeba.status
val sp_NOTEMPTY : Amoeba.status
val sp_UNREACH : Amoeba.status
val sp_CLASH : Amoeba.status

(*
** Rights:
*)

val sp_COLMASK : int
val sp_DELRGT : int
val sp_MODRGT : int

(*
** Use the default soap server
*)
val sp_DEFAULT : Capset.capset

(*
** Current working directory and root directory
** capsets (extracted from cap environment)
*)

val dir_rootcs : Capset.capset ref
val dir_workcs : Capset.capset ref


(*
** SOAP directory data structures.
*)

(* one row of a directory *)

type sp_dir_entry = {
  mutable d_name : string;
  mutable d_columns : Amoeba.rights_bits array;
} 

val nil_sp_dir_entry : sp_dir_entry

(* one directory *)

type sp_dir = {
    mutable dd_capset   : Capset.capset;
    mutable dd_ncols    : int;
    mutable dd_nrows    : int;
    mutable dd_colnames : string array;
    mutable dd_rows     : sp_dir_entry list;
    mutable dd_curpos   : int;
} 

val nil_sp_dir : sp_dir




(*
** sp_mask
**      Looks in the environment for the user's default capability masks
**      and returns them in an int array.  
**      If there is no
**      environment variable then all nmasks entries are set to 0xFF
*)
val sp_mask : unit -> int * int array

(*
** Evaluate any "." or ".." components in name
*)

val path_normalize : path:string -> Amoeba.capability*string

val path_MAX : int


(*
** Return the current working directory cap set
*)
val sp_get_workdir : unit -> Amoeba.status * Capset.capset

(*
** Return the root dir cap set
*)
val sp_get_rootdir : unit -> Amoeba.status * Capset.capset

(*
** Set the current working and root directory
*)
val sp_set_workdir : Amoeba.capability -> unit
val sp_set_rootdir : Amoeba.capability -> unit

(*
** sp_lookup  returns  the capability-set stored under the name
** 'name' relative to the directory 'dir'. Warning:  if  the  NULL-string is
** given  as  the  path  then  the capability in 'dir' is for the directory
** required and so it is returned without checking to  see  if  it  is a
** valid capability.
*)
val sp_lookup : root:Capset.capset -> path:string -> Amoeba.status * Capset.capset

(*
** sp_traverse  returns  the last component of the path name
** It also returns the capability for the directory up until the
** last component of the path (relative to the directory dir).
**
**
** NOTE: This function will return an unnormalized
** "." or "..", if that was the last or only component of the input path.
** This is correct, since this function is used only when the path name is
** logically specifying a directory and the name of an entry (in that
** directory) that is to be modified or have its attributes returned.
**
** E.g. The path "/foo/bar/.." is supposed to refer to the (virtual) ".."
** entry in directory "/foo/bar", not to "/foo", and similarly for paths
** ending in ".".  If you don't see the difference, consider that
** "del -d /foo/." should return an error (can't delete "." from a
** directory), while "del -d /foo" should actually delete the directory
** /foo.  They are not equivalent in UNIX.
*)
val sp_traverse :
  dir:Capset.capset -> path:string -> Amoeba.status * string * Capset.capset

(*
** Sp_append  adds the directory entry specified by 'name', relative to
** the directory specified by 'dir'. The entry may not exist already.  The
** new entry has the capability-set cs.
** The  column  masks  for the directory entry are specified by the ncols
** entries in the array cols. To avoid first having to look up the number
** of  columns the directory has, any legal number of columns masks (1 up
** to SP_MAXCOLUMNS) is accepted. Masks referring to non-existent columns
** are ignored, and missing masks are set to zero.
*)
val sp_append :
  dir:Capset.capset ->
  name:string -> obj:Capset.capset -> cols:int array -> Amoeba.status

(*
** sp_delete deletes the directory entry (which may itself be a
** directory capability)  specified  by  'name'.
*)
val sp_delete : dir:Capset.capset -> name:string -> Amoeba.status

(*
** Create a new directoty capability set on server 'server'.
*)

val sp_create : server:Capset.capset -> cols:string array ->
                Amoeba.status * Capset.capset

(*
** sp_list: return the row list of the directory specified by
**          'dir'
*)

val sp_list: dir:Capset.capset -> Amoeba.status * sp_dir

