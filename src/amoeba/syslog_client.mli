
val syslog_inited : bool ref
val syslog_deliver_connected : bool ref
val syslog_server : string ref


(*
** Send a message both to standard output channel (1)
** and to the syslog server (2)
**  (1): depends on syslog_mute call
**  (2): depends on syslog_init call
*)
val sys_log : Syslog_common.syslog_type -> ('a, out_channel, unit) format -> 'a


(*
** Initialize syslog module. The 'server' path is optional. If not
** specified (""), the default path is used.
*)

val syslog_init : string -> unit


(*
** Call this function with true argument to suppress message
** printing to standard out channel.
*)

val syslog_mute : bool -> unit

(*
** Set the syslog output channel [Pervasives.stdout,stderr]    
*)
val syslog_chan : Pervasives.out_channel -> unit

(*
** Set the syslog output function (string and char versions).
** Default: output_string and output_char to specified
** output channel.
*)

val syslog_output_str : (string -> unit) -> unit
val syslog_output_char : (char -> unit) -> unit
