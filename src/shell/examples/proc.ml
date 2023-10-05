open Vtty_server
open Shell_exec
open Thread
open Myenv

let def1 = {
    exec_name = "test";
    exec_src = Unix_src (Exec_path "/unix/amoeba/Amcross/bin/i86_pc/ls");
    exec_dst = Amoeba_dst (Exec_path "/hosts/core47/proc");
    exec_args = ["ls";"-l";"/"];
    exec_env = [

    ];
    exec_ops = [
        Exec_coldstart;
        Exec_stop;
    ];
}


let _ =
    let stat,tty_srv = tty_init 30000 2 in
    let ttycap = tty_cap tty_srv in
    def1.exec_env <- def1.exec_env @ [Env_cap ("TTY",ttycap)];
    let obj1 = exec_control_obj def1 in
    thread_delay 2 SEC;    
    tty_exit tty_srv;
