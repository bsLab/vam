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
**    $INITIAL:     (C) 2004 BSSLAB
**    $CREATED:     
**    $VERSION:     1.08
**
**    $INFO:
**
**  Logical and physical Disk with partition table structures - server parts
**  (Low level interface - high level interface can be found in the
**   vdisk module)
**  - contains machine dependent parts !!! -
**
**  Some basics:
**
**  Each physical disk is described with a disklabel structure.
**  This structure contains upto 8 sublabels, called partitions.
**  The disktype is set to "physical".
**  Each physical partition can build a new amoeba virtual disk, 
**  containing also sub partitions (vdisk:00, vdisk:01,...). 
**  Each virtual disk is described with a disklabel structure, too.
**  Physical partitions not amoeba related are labeled "logical" and
**  each get his own (dummy) disklabel, with disktype="logical" set.
**  The partition array contains only the own partition entry! 
**  
**    $ENDOFINFO
**
*)

open Amoeba
open Machtype

(*
** DISK
*)

(*
** Physical disk block size
*)
val d_PHYS_SHIFT   : int
val d_PHYSBLKSZ    : int

(*
** DISKLABEL
*)

(*
** maximum # of Amoeba partitions per physical disk 
*)
val max_PARTNS_PDISK : int

(*
** Amoeba magic number
*)

val am_DISK_MAGIC : int



(*
** One partition table structure
**
** p_firstblk:  gives the first physical block number of the partition.
**              For purity of implementation this should be the start of a
**              cyclinder.
** p_numblks:   gives the number of physical blocks allocated to this
**              partition.
** p_piece:     each partition forms a part of exactly one virtual disk.
**              p_piece specifies the number of the part that this partition
**              is.
** p_many:      p_many specifies the total number of parts that make up the
**              virtual disk.
** p_cap        is the capability of the virtual disk to which this partition
**              belongs.
** p_sysid:     system id string (aka. partition type name, Linux,...)
*)
type part_tab = {
    mutable p_firstblk: int32;
    mutable p_numblks:  int32;
    mutable p_piece: int16;   
    mutable p_many:  int16;    
    mutable p_cap: capability;
    mutable p_sysid: string;
}


(*
** Disk geometry structure, extracted from disk label!
*)

type pdisk_geom = {
    mutable g_bpt:  int16;        (* bytes per track *)
    mutable g_bps:  int16;        (* bytes per sector *)
    mutable g_numcyl: int16;      (* number of cylinders *)
    mutable g_altcyl: int16;      (* reserved *)
    mutable g_numhead: int16;     (* number of heads *)
    mutable g_numsect: int16;     (* number of sectors per track *) 
}


(*
** Disk Label Structure.
**
** d_filler:    Pads the disk label out to physical block size.
** d_disktype:  A string describing the brand and type of the disk.
** d_geom:      The disk geometry parameters are only relevant for SMD
**              drives since scsi disks manage this information internally.
** d_ptab:      Partition table.
** d_magic:     A quick check to make sure that this really is an Amoeba label.
** d_filler2:   To make sure that the struct fills to a 4 byte boundary.
** d_chksum:    This is an xor of all the 16-bit words in the disk label to
**              give some confidence that the label hasn't been corrupted.
*)

(*
** max # chars describing the type of disk
*)
val typeLEN : int

(*
** filler size must pad the struct to D_PHYSBLKSZ bytes. 
** Must be adjusted for Amoeba 4!
*)

val sizeof_geom : int
val sizeof_part_tab : int
val filler_SIZE : int

(*
** Different kinds of disks:
** physical: the hard disk
** logical: hard disk main partitions
** amoeba: virtual disks contained  in one main partition
*)

type disktype = 
    | Disk_physical of string
    | Disk_logical of string
    | Disk_amoeba of string


type disklabel = {
    (* d_filler: filler_SIZE bytes first *)
    mutable d_disktype : disktype;    (* typeLEN *)
    mutable d_geom: pdisk_geom;
    mutable d_ptab: part_tab array; (* max_PARTNS_PDISK *)
    mutable d_magic: int32;
    (* d_filler2: 2 bytes *)
    mutable d_chksum: uint16;
}


(*
** VDISK
** Each physical Amoeba partition is divied in upto 8 subpartitions,
** called virtual disks. The vdisk number is increased beyond physical
** disk boundaries, therfore an Amoeba filesystem can spwan several
** physical disks.
*)

(*
** size of disk thread's transaction buffer
*)
val disk_REQBUFSZ : int

val min_VBLK_SHIFT : int
val max_VBLK_SHIFT : int


(*
** Disk server structure. One for each device. But all devices
** share the vdisk, ldisk and pdisk lists!
*)
  

(*
** Device geometry - suppilied externally
*)
type dev_geom = {
    mutable dev_numcyl:    int; 
    mutable dev_numhead:   int; 
    mutable dev_numsect:   int; 
}

type disk_dev = {
    mutable dev_host: string;   (* amoeba host path *)
    mutable dev_path: string;   (* unix device path *)
    mutable dev_blksize : int;  (* default block size in bytes *)
    mutable dev_geom : dev_geom;
    (*
    ** Raw device read and write functions.
    ** Args: start block, size in blocks, buffer to read from or to write to
    **       and offset in buffer of first byte.         
    *)                                                 
    mutable dev_read: first:int -> size:int ->         
                      buf:Bytebuf.buffer -> off:int -> status;
    mutable dev_write: first:int -> size:int -> 
                      buf:Bytebuf.buffer -> off:int -> status;
     

    (*
    ** All disklabels found on this disks:
    **  1. Hostlabel (physical partition table)
    **  2. Amoeba labels (subpartitions of an physical amoeba partition
    **                    or of a whole disk without host partition table)
    **  3. Logical disk labels (otehr OSes)
    *)
    mutable dev_labels: disklabel list;

}

val max_PARTNS_VDISK : int

(*
**  Part_list is the basis for a list of partitions for use by amoeba
**
** dev          : The disk device managing the partition
** labelblock   : The disk address where the partition label belongs.
** label        : Details of the partition table in a data structure that
**                Amoeba knows.  It must be filled in by the host OS dependent
**                routines.
*)

type part = {
    mutable dev : disk_dev;
    mutable labelblock : int;
    mutable label: disklabel;
}

(*
**
** Virtual Disk Partition Table Structure.
**
** i_physpart:  Partition struct of the physical partition on which           
**              the sub-partition is found.
** i_part:      The number of the partition (on the physical disk) from which
**              this piece of virtual disk is taken.
** i_firstblk:  Number of the first block in this partition available for use
**              by clients.
** i_numblks:   Number of blocks, starting from i_firstblk, available for use
**              by clients.
*)

type vpart = {
    mutable i_physpart: part option;
    mutable i_part: int;
    mutable i_firstblk: int;
    mutable i_numblks: int;
}

(*
** Virtual Disk Structure. One virtual disk vdisk can spawn
** several physical partitions and local disks (v_parts, v_many)!
**
** v_cap:       Capability for this virtual disk.
** v_numblks:   Total number of "physical" blocks available on virtual disk.
** v_many:      Number of entries in the v_parts array.
** v_parts:     Array of in-core partition descriptions of the
**              partitions that comprise the virtual disk.
*)

type vdisk = {
    mutable v_name: string;
    mutable v_cap: capability;
    mutable v_numblks: int;
    mutable v_many: int;
    mutable v_parts: vpart array;
}


(*
** All devices share the vdisk, ldisk and pdisk lists!
*)
type disk = { 
    (*
    ** disk_init will fill out these entries...
    *)

    mutable pdisks: vdisk list;  
    mutable vdisks: vdisk list;
    mutable ldisks: vdisk list;
}
