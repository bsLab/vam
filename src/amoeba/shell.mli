val mod_shell_ver : float
type com_options = Rights | Info | Cap | Help
val com_out : out_channel ref
val dir :
  ?root:Amoeba.capability -> path:string -> args:com_options list -> unit
val std_status :
  ?root:Amoeba.capability -> name:string -> args:com_options list -> unit
val std_info :
  ?root:Amoeba.capability -> name:string -> args:com_options list -> unit
val std_exit :
  ?root:Amoeba.capability -> name:string -> args:com_options list -> unit
val mkdir :
  ?root:Amoeba.capability -> name:string -> args:com_options list -> unit
 