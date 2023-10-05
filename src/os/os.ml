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

let version = 1.01

(*
** Machine and architecture description.
*)

open Sys

type vm_architecture_type   = CPU_I386 
      
type vm_machine_type        = MACH_Pc86   
type vm_os_type             = OS_Amoeba | OS_Unix 

let vm_os = 
    match os_type with
    | "Unix"    -> OS_Unix;
    | "Amoeba"  -> OS_Amoeba;
    | _         -> failwith ("Os: Unknown OS "^os_type)

let vm_mach = 
    match model_type with
    | "default" -> MACH_Pc86;
    | _         -> failwith ("Os: Unknown machine model "^model_type)

let vm_arch = 
    match arch_type with
    | "i386"    -> CPU_I386;
    | _         -> failwith ("Os: Unknown architecture "^arch_type)

 

    
    


