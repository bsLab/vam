
(*
** DOS file system module
*)

val version: string

open Amoeba
open Stderr
open Disk
open Unix


(*
** Supported FAT types
*)

type fat_Type = Fat12 | Fat16 | Fat32

(*
** DOS Boot Block info structure
*)


type boot_info =
{
    doscap: capability;     (* disk drive capability *)

    oem_name: string;       (* OEM name and number *)

    vol_size:int;           (* Number of logical sectors in the vol *)
    sector_size:int;        (* Bytes per sector    *)
    track_size:int;         (* Sectors per track   *)
    num_heads:int;          (* number of heads     *)

    cluster_size:int;       (* Sector per cluster  *)
    fat_size:int;           (* Sectors per FAT     *)
    fat_type:fat_Type;

    reserved_sect:int;      (* reserved sectors    *)
    num_fats:int;           (* number of FATs      *)
    num_root_entr:int;      (* Number of root directory entries *)
    hidden_sect:int;        (* Number of hidden sectors *)

    media_type:string*int*int;         (* Medium descriptor byte
                                       ** -> id string("Floppy"/"HardDisk",
                                       **    number of tracks,
                                       **    sect_per_tracks
                                       *)

    vol_serial:int;         (* Volume serial number     *)
    vol_label:string;       (* Volume label (11 bytes)  *)
    fs_id_str:string;       (* Filesystem ID string     *)



}

val attr_READ_ONLY  : int
val attr_HIDDEN     : int
val attr_SYSTEM     : int
val attr_VOLUME_ID  : int
val attr_DIRECTORY  : int
val attr_ARCHIVE    : int
val attr_LONG_NAME  : int

exception Invalid_Fat

(*
** Directory entry
*)


val dir_ENTRY_SIZE : int

type dir_entry = {

    mutable dir_short_name:string;      (* Short name                   *)
    mutable dir_long_name:string;       (* Long VFAT name               *)

    mutable dir_attr:int;               (* File attributes              *)
    mutable dir_file_size:int;          (* File size                    *)

    mutable dir_cluster_num:int;        (* Size in clusters             *)
    mutable dir_first_cluster:int;      (* First cluster number of file *)
    mutable dir_cluster_list:int array; (* Complete cluster list        *)

    mutable dir_time_create:tm;         (* Creation time                *)
    mutable dir_time_lastacc:tm;        (* Last access date             *)
    mutable dir_time_write:tm;          (* Last Write time              *)

    (* For the case this is a directory: hold the sub directory list *)
    mutable dir_sub:(string,dir_entry) Hashtbl.t;

    mutable dir_sub_num:int;            (* number of entries in the hash *)                

}

val nilhash: (string,dir_entry) Hashtbl.t

val nilbootinfo : boot_info

val niltm : tm

val nildir : dir_entry

(*
** Read the boot sector of a DOS partition (logical block 0). Cap is the
** partition capability.
*)


val read_dosboot : capability -> status * boot_info 

(*
** Read the first FAT.
*)

val read_dosfat : boot_info -> status * int array

(*
** Directory inconsistency
*)

exception Invalid_Dir


(*
** Read the Root directory.
**
** Returns:
**
** [ errstat * volid * directory ]
*)

val read_dosroot :  bootinfo:boot_info ->
                    fat:int array      -> 
                    Amoeba.status * dir_entry * dir_entry 

(*
** Return the clusters allocated by a file or directory. Start is
** the first cluster. Num ist the number of expected clusters calculated
** from the filesize. For directories, num must be 0. Returns
** (number_of_clusters * cluster_array) tuple.
*)

val cluster_chain :     fat:int array -> 
                        bs:boot_info -> 
                        start:int -> 
                        num:int -> 
                        int * int array

(*
** Read the a directory starting at cluster n.
**
** Returns:
**
** [ errstat * directory list ]
*)

val read_dosdir :       bootinfo:boot_info ->
                        fat: int array -> 
                        dir: dir_entry ->
                        Amoeba.status 


