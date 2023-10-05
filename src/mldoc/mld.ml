(*
** MlDoc frontend program
*)

open Doc_core
open Doc_text
open Doc_html
open Doc_latex
open Doc_help

type option =
    | Op_Term       (* without any command option   *)
    | Op_BinDoc     (* -b -o <docname>              *)
    | Op_LatDoc     (* -l -o <docname>              *)
    | Op_Out of string 
    | Op_In of string
    | Op_None

let term fname =
    let at = atoms_of_file fname in
    let ds = tree_of_atoms at in
    text_of_tree ds [TEXT_terminal;TEXT_notoc] []

let doc iname oname =
    help_file  ~fname:iname;
    help_write ~fname:oname

let lat iname oname =
    let at = atoms_of_file iname in
    let ds = tree_of_atoms at in
    tex_of_tree ds [TEX_doc_name oname;
                    TEX_color;
                    TEX_link_ref;
                   ] []
                                                           
   
let usage s =
    let us = "
    mld <inputfile>           
        [Output mldoc file to text terminal]
    mld -b -o <outputfile> <inputfile>   
        [Output mldoc file to binary output]
    mld -l -o <outputfile> <inputfile>   
        [Output mldoc file to TeX/LaTeX output]
    " in 
    output_string Pervasives.stdout (s^"\n"^"usage: \n"^us^"\n");
    raise Exit
     
let _ =
    let argv = Array.to_list (Sys.argv) in
    let argn = List.length argv in
    
    let do_doc  = ref Op_None in
    let do_lat  = ref Op_None in
    let do_out  = ref Op_None in
    let do_in   = ref Op_None in
        
    if (argn = 1) then
    begin
        usage "Input filename missing";
    end
    else
    begin
        let rec iter ol =
            match ol with
            | hd::tl -> 
              begin
                match hd with 
                | "-o" -> 
                  begin
                          let name = match tl with
                                     | hd'::tl' -> 
                                        begin
                                            iter tl';
                                            hd';
                                        end;
                                     | [] -> usage "Invalid -o option";   
                          in
                          do_out := Op_Out name;
                  end;        
                | "-b" -> do_doc := Op_BinDoc; iter tl;
                | "-l" -> do_doc := Op_LatDoc; iter tl;
                | s -> do_in := Op_In s; iter tl;
              end;
            | [] -> ();      
        in
        iter argv;

        if (!do_doc = Op_BinDoc) then
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            doc !iname !oname;
        end
        else
        if (!do_doc = Op_LatDoc) then
        begin
            let iname = ref "" in
            let oname = ref "" in
            
            if (!do_in <> Op_None) then
            begin
                iname := (match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Input filename missing";

            if (!do_out <> Op_None) then
            begin
                oname := (match !do_out with
                            | Op_Out s -> s;
                            | _ -> usage "";
                         );
            end
            else
                usage "Output filename missing";
            
            lat !iname !oname;
        end
        else
        begin
            if (!do_in <> Op_None) then
            begin
                let fname = match !do_in with
                            | Op_In s -> s;
                            | _ -> usage "";
                in
                term fname;
            end
            else
                usage "Input filename missing";
        end; 
    end
