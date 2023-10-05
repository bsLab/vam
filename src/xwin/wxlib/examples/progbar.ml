open WX_types

let root = new WX_root.from_display "" 0
let top = new WX_top.t root None []
let vbar = new WX_bar.v top#container []

let pb1 = new WX_valbar.t vbar#container [MinWidth 200]

let hbar1 = new WX_bar.h vbar#container [ IpadX 20; IpadY 20; 
                        Background "black"]
let hbar2 = new WX_bar.h vbar#container [ IpadX 20; IpadY 20;]

let pb2 = new WX_valbar.t hbar1#container [MinWidth 200]
let pb3 = new WX_valbar.t hbar1#container [MinWidth 200]

let sb = new WX_slider.h vbar#container [MinWidth 200; IpadX 10; IpadY 10;]

let prog () =
    let v = ref 0.0 in
    for i = 1 to 10
    do
        Unix.sleep 1;
        pb1#set_value !v;
        pb2#set_value !v;
        pb3#set_value !v;
        sb#set_value !v;
        v := !v +. 10.0;
    done;
    for i = 1 to 11
    do
        Unix.sleep 1;
        pb1#set_value !v;
        pb2#set_value !v;
        pb3#set_value !v;
        sb#set_value !v;
        v := !v -. 10.0;
    done;
    Unix.sleep 1;
    exit 0

let _ =
  pb1#set_format "%2.0f %% Voltage";
  pb2#set_color "red";
  pb3#set_color "green";
  sb#set_value 40.0;

  top#container_add vbar#contained;
  hbar1#container_add pb1#contained;
  hbar2#container_add pb2#contained;
  vbar#container_add_s [hbar1#contained; hbar2#contained; 
                        pb3#contained;sb#contained];
  top#show;
  ignore (Thread.thread_create prog ());
  loop ()
