type format_type =
    Format_name
  | Format_cap
  | Format_rights
  | Format_time
  | Format_info
  | Format_status
  | Format_header
  | Format_pad
and format_just = Just_left | Just_right | Just_center
and format_spec = {
  fs_type : format_type;
  fs_len : int;
  fs_just : format_just;
} 
and obj_type = File_obj | Dir_obj | Cap_obj | Nil_obj
val shell_dir_timeout : int ref
exception Format_error of string
exception Shell_ex of (Amoeba.status * string)
val shell_log_fun : (string -> unit) ref
val shell_set_log : (string -> unit) -> unit
val parse_format : string -> format_spec list
val print_box : int -> format_just -> string -> bool -> string
val nchars : int -> char -> string
val print : string -> unit
val is_filter : string -> string -> bool
