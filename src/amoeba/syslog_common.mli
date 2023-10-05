val sys_DEBUG : int
val sys_INFO : int
val sys_NOTICE : int
val sys_WARN : int
val sys_ERROR : int
val sys_ERR : int
val sys_FATAL : int
val sys_PRINT : int
val sys_STARTUP : int
type syslog_type =
    Sys_debug
  | Sys_info
  | Sys_notice
  | Sys_warn
  | Sys_err
  | Sys_fatal
  | Sys_print
  | Sys_start
val syslog_WRITE : Amoeba.command
val syslog_READ : Amoeba.command
val syslog_LEVEL : Amoeba.command
val syslog_SHUTDOWN : Amoeba.command
val syslog_SYNC : Amoeba.command
val syslog_SETOUT : Amoeba.command
val syslog_VERBOSE : Amoeba.command
val syslog_CAPINFO : int
val syslog_KWRITE : int
val syslog_verbose_level : int ref
val syslog_BUFSIZE : int
val syslog_CHECK : int
val syslog_FLUSH : int
val syslog_SERVERPATH : string
val syslog_PATH : string
