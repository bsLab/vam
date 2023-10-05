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
**  Logical and physical Disk with partition table support - server parts
**  (Low level interface - high level interface can be found in the
**   vdisk module)
**  - contains machine dependent parts !!! -
**
**    $ENDOFINFO
**
*)

(*
** Get informations 
*)

(*
** Print out informations about the partitions list of one device.
*)

val info_disklabels : dev:Disk_common.disk_dev  -> string

(*
** Get infos about all virtual disks (v,l,p)
*)

val info_vdisks : dev:Disk_common.disk_dev -> disks:Disk_common.disk -> string


(*
** Print the virtual disk table
*)

val print_vdisk_table : Disk_common.vdisk option array -> string

(*
** Initialize one disk:   
**  1. Read physical (host) partition table.
**  2. Search for amoeba partitions, read the amoeba disklabels
**      (subpartition table).
**  3. Create and initialize all virtual disk structures.
*)

val vdisk_init : dev:Disk_common.disk_dev -> 
                 disks:Disk_common.disk -> 
                 Amoeba.status

(*
** Create one huge virtual disk table containing vdisks, pdisks and ldisks.
**  1. The real virtual amoeba disks (with highest vdisk#)
**  2. Physical disks (objnum = vdisk#+1...)
**  3. Logical disks
** The capabilities of the 2. & 3. disks must recomputed with
** respect to theit object numbers!
*)
val vdisk_table : Disk_common.disk -> Disk_common.vdisk option array

(*
** Publish virtual disk capabilities (in directory
** dirname) somewhere in the DNS tree.
**
** Note: the virtual disk structure (v_cap) holds the private (get) port!
*)

val vdisk_publish: disk_table:Disk_common.vdisk option array ->
                   dirname: string -> Amoeba.status 

(*
** Remove published vdisk caps
*)
val vdisk_remove: disk_table:Disk_common.vdisk option array ->
                   dirname: string -> Amoeba.status

(*
** DISK_RW
**
**      Returns STD_OK if it could successfully read/write
**      "vblksz" * "num_vblks" bytes beginning at disk block "start" from/to
**      the virtual disk specified by "priv" into/from "buf".  Otherwise it
**      returns an error status indicating the nature of the fault.
**
*)
  
val vdisk_rw : hdr:Amoeba.header ->
               vblksz:int ->
               vstartblk:int ->
               num_vblks:int ->
               buf:Bytebuf.buffer -> 
               vdisks: Disk_common.vdisk option array ->
               Amoeba.status

(*
** DISK_INFO
**
**      This routine returns in 'buf' a pointer to an array containing
**      containing the disk partition startblock and size for each
**      physical disk partition comprising a virtual disk.  The information
**      was calculated at boot time and stored in network byte order at
**      initialisation time.
**      hdr.h_size is modified.
*)

val vdisk_info :    hdr:Amoeba.header ->
                    buf:Bytebuf.buffer ->
                    vdisks:Disk_common.vdisk option array ->
                    Amoeba.status

(*
** DISK_SIZE
**
**      Returns STD_CAPBAD if the capability was invalid or referred to a
**      virtual disk with size <= 0.  Otherwise it returns STD_OK and
**      returns in "maxblocks" the maximum number of virtual blocks of size
**      2^"l2vblksz" that fit on the virtual disk specified by "priv".
**      Returns status and size.
**
*)

val vdisk_size : hdr:Amoeba.header ->
                 vblksz:int ->
                 vdisks:Disk_common.vdisk option array ->
                 Amoeba.status * int

(*
** DISK_GETGEOMETRY
**
** Because the geometry information in 386/AT bus machines is not on the
** disk but in eeprom the fdisk program cannot get at it.  Therefore we
** provide this ugly hack to let it get it.
** hdr.h_size is modified.
*)
val vdisk_getgeom : hdr:Amoeba.header ->
                    buf:Bytebuf.buffer ->
                    vdisks:Disk_common.vdisk option array ->
                    Amoeba.status 

(*
**  DISK_STD_INFO
**
**      Returns standard string describing the disk server object.
**      This includes the size of the disk in kilobytes.
**      hdr.h_size is modified.
*)

val vdisk_std_info : hdr:Amoeba.header ->
                    buf:Bytebuf.buffer ->
                    vdisks:Disk_common.vdisk option array ->
                    Amoeba.status 

val vdisk_std_status : hdr:Amoeba.header ->
                    buf:Bytebuf.buffer ->
                    vdisks:Disk_common.vdisk option array ->
                    Amoeba.status 


(*
**  DISK_STD_RESTRICT
**
**      The following implements the STD_RESTRICT command
**      There is only one rights bit anyway so it isn't too exciting.
**      The new private is stored in hdr.h_priv.
*)

val vdisk_std_restrict: hdr:Amoeba.header ->
                        rights_mask:int ->
                        vdisks:Disk_common.vdisk option array ->
                        Amoeba.status 
