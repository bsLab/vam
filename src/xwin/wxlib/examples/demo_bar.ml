open WX_types

let root = new WX_root.from_display "" 0
let top = new WX_top.t root None []
let vbar = new WX_bar.v top#container []

let text = new WX_text.of_string vbar#container 
"Your program has raised a stack overflow exception
Click OK if you want to continue.
Click CANCEL if you want to abort.
Click HELP if you need some help.
" [BorderWidth 2]

let hbar = new WX_bar.h vbar#container [ IpadX 5; IpadY 5]
let hbar2 = new WX_bar.h vbar#container [IpadX 5; IpadY 10]

let attrs = [IpadX 1; IpadY 1; ExpandX true]

let ok = new WX_button.with_label hbar2#container "OK" attrs
let cancel = new WX_button.with_label hbar2#container "CANCEL" attrs
let help = new WX_button.with_label hbar2#container "HELP" attrs

let _ =
  ok#set_action (fun _ -> exit 0);
  cancel#set_action (fun _ -> exit 1);
  help#set_action (fun _ -> print_string "HELP"; print_newline ());
  top#container_add vbar#contained;
  hbar#container_add text#contained;
  vbar#container_add_s [hbar#contained; hbar2#contained];
  hbar2#container_add_s [ok#contained; cancel#contained; help#contained];
  top#show;
  loop ()
