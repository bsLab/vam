open X
open VX_types
open VX_box
open VX_text
open VX_texttable
open VX_ps 

let display = new VX_display.t ""
let root = new VX_root.t display 0 
let top = new VX_wmtop.t root      
          [MinHeight 200; MinWidth 300]

let vbox = new VX_box.v top#container 
                            [
                                Border [];
                                Background "lightblue";
                                IpadX 10;
                                IpadY 10;
                                ExpandX true;
                                ExpandY true;
                                AdjustY true;
                            ] 

let hbox = new VX_box.h vbox#container 
                            [
                                AdjustX true;
                                ExpandX true;
                                Background "lightblue";

                            ] 

let tab_cont = [|
    [| "Name:";"";|];
    [| "Street:";"";|];
|]


let grey50 = top#color_make "grey50" true

let attr = [
    Background "white";
    IpadX 5;
    IpadY 5;
    Text_font Times;
    Text_style Bold;
    Text_size 12;
    Border [];
    Rows [|
        [IpadY 5];
        [IpadY 5];
    |];
    Cols [|
        [| 
            [Align Right;
             Width 60;];
            [Text_font Courier; Align Left; Mutable true;
             Width 120;
             Text_baseline [];

             But [
                Sym S_OK;Sym S_ENTER;
                ActionSSS (fun str st -> 
                    if st = St_Modified then
                    begin
                        print_string str; print_newline ();
                    end;
                    St_Submitted
                     );
                ];
            ];
        |];
        [| 
            [Align Right;
             Width 60;];
            [Text_font Courier; Align Left; Mutable true; 
             Width 120;
             Text_baseline [];
             But [
                Sym S_OK;Sym S_ENTER;Sym S_ERR;
                 ActionSSS (fun str stat -> 
                    if stat = St_Modified then
                    begin    
                        print_string str; print_newline ();
                    end;
                    let v = ref 0 in
                    let failed = not (protects (v := int_of_string str)) in
                    if failed then
                        St_Failed
                    else
                        St_Submitted
                    );
                ];
            ];
        |];
    

    
    |];
]

let tab = new VX_texttable.t hbox#container tab_cont attr  

let _ =
    hbox#container_add_s [tab#contained];
    vbox#container_add_s [hbox#contained];

    top#setWM_NAME "VX test";
    top#container_add vbox#contained;
    top#show;

    let printed = ref false in
    top#configure [Bindings [
        EnterWindow,(fun _ ->
            if not !printed then
            begin
                printed := true;
                print_eps "texttable5.eps" 1.0 vbox#contained;
            end;
            );   
        ]];   

    try
      loop ();
    with Exit -> ();
