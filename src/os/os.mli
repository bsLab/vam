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

val version: float

(*
** Machine and architecture description.
*)


type vm_architecture_type = CPU_I386
and vm_machine_type = MACH_Pc86
and vm_os_type = OS_Amoeba | OS_Unix

(*
** Os, Machine and Architecture  specifiers.
*)

val vm_os : vm_os_type
val vm_mach : vm_machine_type
val vm_arch : vm_architecture_type
