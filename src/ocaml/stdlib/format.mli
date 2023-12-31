(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: format.mli,v 1.43 2001/12/07 13:40:51 xleroy Exp $ *)

(** Pretty printing.

   This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box
   structure.

   Warning: the material output by the following functions is delayed
   in the pretty-printer queue in order to compute the proper line
   breaking. Hence, you should not mix calls to the printing functions
   of the basic I/O system with calls to the functions of this module:
   this could result in some strange output seemingly unrelated with
   the evaluation order of printing commands.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line breaking. The addition of
   pretty-printing annotations to your regular [printf] formats gives you
   fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit box management and printing functions
   provided by this module. This style is more basic but more verbose
   than the [fprintf] concise formats.

   For instance, the sequence 
   [open_box (); print_string "x ="; print_space (); print_int 1; close_box ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@\[%s@ %i@\]" "x =" 1], or even shorter
   [printf "@\[x =@ %i@\]" 1].

   Rule of thumb for casual users of this library:
-   use simple boxes (as obtained by [open_box 0]);
-   use simple break hints (as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint);
-   once a box is opened, display its material with basic printing
   functions (e. g. [print_int] and [print_string]);
-   when the material for a box has been printed, call [close_box ()] to
   close the box;
-   at the end of your routine, evaluate [print_newline ()] to close
   all remaining boxes and flush the pretty-printer.

   The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. Each box opened via
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, the system closes all opened boxes and
   flushes all pending text (as with the [print_newline] function)
   after each phrase. Each phrase is therefore executed in the initial
   state of the pretty-printer.
*)


(** {2 Boxes} *)

val open_box : int -> unit
(** [open_box d] opens a new pretty-printing box
   with offset [d].
   This box is the general purpose pretty-printing box.
   Material in this box is displayed ``horizontal or vertical'':
   break hints inside the box may lead to a new line, if there
   is no more room on the line to print the remainder of the box,
   or if a new line may lead to a new indentation
   (demonstrating the indentation of the box).
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val close_box : unit -> unit
(** Close the most recently opened pretty-printing box. *)

(** {2 Formatting functions} *)

val print_string : string -> unit
(** [print_string str] prints [str] in the current box. *)

val print_as : int -> string -> unit
(** [print_as len str] prints [str] in the
   current box. The pretty-printer formats [str] as if
   it were of length [len]. *)

val print_int : int -> unit
(** Print an integer in the current box. *)

val print_float : float -> unit
(** Print a floating point number in the current box. *)

val print_char : char -> unit
(** Print a character in the current box. *)

val print_bool : bool -> unit
(** Print a boolean in the current box. *)


(** {2 Break hints} *)

val print_space : unit -> unit
(** [print_space ()] is used to separate items (typically to print
   a space between two words). 
   It indicates that the line may be split at this
   point. It either prints one space or splits the line.
   It is equivalent to [print_break 1 0]. *)

val print_cut : unit -> unit
(** [print_cut ()] is used to mark a good break position.
   It indicates that the line may be split at this 
   point. It either prints nothing or splits the line.
   This allows line splitting at the current
   point, without printing spaces or adding indentation.
   It is equivalent to [print_break 0 0]. *)

val print_break : int -> int -> unit
(** Insert a break hint in a pretty-printing box.
   [print_break nspaces offset] indicates that the line may
   be split (a newline character is printed) at this point,
   if the contents of the current box does not fit on the
   current line. 
   If the line is split at that point, [offset] is added to
   the current indentation. If the line is not split,
   [nspaces] spaces are printed. *)

val print_flush : unit -> unit
(** Flush the pretty printer: all opened boxes are closed,
   and all pending text is displayed. *)

val print_newline : unit -> unit
(** Equivalent to [print_flush] followed by a new line. *)

val force_newline : unit -> unit
(** Force a newline in the current box. Not the normal way of
   pretty-printing, you should prefer break hints. *)

val print_if_newline : unit -> unit
(** Execute the next formatting command if the preceding line
   has just been split. Otherwise, ignore the next formatting
   command. *)


(** {2 Margin} *)

val set_margin : int -> unit
(** [set_margin d] sets the value of the right margin
   to [d] (in characters): this value is used to detect line
   overflows that leads to split lines.
   Nothing happens if [d] is smaller than 2 or
   bigger than 999999999. *)

val get_margin : unit -> int
(** Return the position of the right margin. *)


(** {2 Maximum indentation limit} *)

val set_max_indent : int -> unit
(** [set_max_indent d] sets the value of the maximum
   indentation limit to [d] (in characters):
   once this limit is reached, boxes are rejected to the left,
   if they do not fit on the current line.
   Nothing happens if [d] is smaller than 2 or
   bigger than 999999999. *)

val get_max_indent : unit -> int
(** Return the value of the maximum indentation limit (in characters). *)


(** {2 Formatting depth: maximum number of boxes allowed before ellipsis} *)

val set_max_boxes : int -> unit
(** [set_max_boxes max] sets the maximum number
   of boxes simultaneously opened.
   Material inside boxes nested deeper is printed as an
   ellipsis (more precisely as the text returned by
   [get_ellipsis_text ()]).
   Nothing happens if [max] is not greater than 1. *)

val get_max_boxes : unit -> int
(** Return the maximum number of boxes allowed before ellipsis. *)

val over_max_boxes : unit -> bool
(** Test if the maximum number of boxes allowed have already been opened. *)


(** {2 Advanced formatting} *)

val open_hbox : unit -> unit
(** [open_hbox ()] opens a new pretty-printing box.
   This box is ``horizontal'': the line is not split in this box
   (new lines may still occur inside boxes nested deeper). *)

val open_vbox : int -> unit
(** [open_vbox d] opens a new pretty-printing box
   with offset [d].
   This box is ``vertical'': every break hint inside this
   box leads to a new line.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val open_hvbox : int -> unit
(** [open_hvbox d] opens a new pretty-printing box
   with offset [d].
   This box is ``horizontal-vertical'': it behaves as an
   ``horizontal'' box if it fits on a single line,
   otherwise it behaves as a ``vertical'' box.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)

val open_hovbox : int -> unit
(** [open_hovbox d] opens a new pretty-printing box
   with offset [d].
   This box is ``horizontal or vertical'': break hints
   inside this box may lead to a new line, if there is no more room
   on the line to print the remainder of the box.
   When a new line is printed in the box, [d] is added to the
   current indentation. *)


(** {2 Tabulations} *)

val open_tbox : unit -> unit
(** Open a tabulation box. *)

val close_tbox : unit -> unit
(** Close the most recently opened tabulation box. *)

val print_tbreak : int -> int -> unit
(** Break hint in a tabulation box.
   [print_tbreak spaces offset] moves the insertion point to
   the next tabulation ([spaces] being added to this position).
   Nothing occurs if insertion point is already on a
   tabulation mark.
   If there is no next tabulation on the line, then a newline
   is printed and the insertion point moves to the first
   tabulation of the box.
   If a new line is printed, [offset] is added to the current
   indentation. *)

val set_tab : unit -> unit
(** Set a tabulation mark at the current insertion point. *)

val print_tab : unit -> unit
(** [print_tab ()] is equivalent to [print_tbreak (0,0)]. *)


(** {2 Ellipsis} *)

val set_ellipsis_text : string -> unit
(** Set the text of the ellipsis printed when too many boxes
   are opened (a single dot, [.], by default). *)

val get_ellipsis_text : unit -> string
(** Return the text of the ellipsis. *)


(** {2 Redirecting formatter output} *)

val set_formatter_out_channel : out_channel -> unit
(** Redirect the pretty-printer output to the given channel. *)


(** {2 Changing the meaning of printing material} *)

val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
(** [set_formatter_output_functions out flush] redirects the
   pretty-printer output to the functions [out] and [flush].
   The [out] function performs the pretty-printer output.
   It is called with a string [s], a start position [p],
   and a number of characters [n]; it is supposed to output
   characters [p] to [p + n - 1] of [s]. The [flush] function is
   called whenever the pretty-printer is flushed using
   [print_flush] or [print_newline]. *)

val get_formatter_output_functions :
  unit -> (string -> int -> int -> unit) * (unit -> unit)
(** Return the current output functions of the pretty-printer. *)


(** {2 Changing the meaning of pretty printing (indentation, line breaking, and printing material)} *)

val set_all_formatter_output_functions :
  out:(string -> int -> int -> unit) -> flush:(unit -> unit) ->
    newline:(unit -> unit) -> spaces:(int -> unit) -> unit
(** [set_all_formatter_output_functions out flush outnewline outspace]
   redirects the pretty-printer output to the functions
   [out] and [flush] as described in
   [set_formatter_output_functions]. In addition, the pretty-printer
   function that outputs a newline is set to the function [outnewline]
   and the function that outputs indentation spaces is set to the
   function [outspace].
   This way, you can change the meaning of indentation (which
   can be something else than just printing a space character) and
   the meaning of new lines opening (which can be connected to
   any other action needed by the application at hand).
   The two functions [outspace] and [outnewline] are normally
   connected to [out] and [flush]: respective default values for
   [outspace] and [outnewline] are [out (String.make n ' ') 0 n]
   and [out "\n" 0 1]. *)
	  
val get_all_formatter_output_functions :
  unit ->
    (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
      (int -> unit)
(** Return the current output functions of the pretty-printer,
   including line breaking and indentation functions. *)


(** {2 Multiple formatted output} *)

type formatter
(** Abstract data type corresponding to a pretty-printer and
   all its machinery.
   Defining new pretty-printers permits the output of
   material in parallel on several channels.
   Parameters of a pretty-printer are local to this pretty-printer:
   margin, maximum indentation limit, maximum number of boxes
   simultaneously opened, ellipsis, and so on, are specific to
   each pretty-printer and may be fixed independently.
   Given an output channel [oc], a new formatter writing to
   that channel is obtained by calling [formatter_of_out_channel oc].
   Alternatively, the [make_formatter] function allocates a new
   formatter with explicit output and flushing functions
   (convenient to output material to strings for instance). *)

val formatter_of_out_channel : out_channel -> formatter
(** [formatter_of_out_channel oc] returns a new formatter that
   writes to the corresponding channel [oc]. *)

val std_formatter : formatter
(** The standard formatter used by the formatting functions
   above. It is defined as [formatter_of_out_channel stdout]. *)

val err_formatter : formatter
(** A formatter to use with formatting functions below for
   output to standard error. It is defined as
  [formatter_of_out_channel stderr]. *)

val formatter_of_buffer : Buffer.t -> formatter
(** [formatter_of_buffer b] returns a new formatter writing to
   buffer [b]. As usual, the formatter has to be flushed at
   the end of pretty printing, using [pp_print_flush] or
   [pp_print_newline], to display all the pending material. *)

val stdbuf : Buffer.t
(** The string buffer in which [str_formatter] writes. *)

val str_formatter : formatter
(** A formatter to use with formatting functions below for
   output to the [stdbuf] string buffer.
   [str_formatter] is defined as 
   [formatter_of_buffer stdbuf]. *)

val flush_str_formatter : unit -> string
(** Returns the material printed with [str_formatter], flushes
   the formatter and reset the corresponding buffer. *)

val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
(** [make_formatter out flush] returns a new formatter that
   writes according to the output function [out], and the flushing
   function [flush]. Hence, a formatter to the out channel [oc]
   is returned by [make_formatter (output oc) (fun () -> flush oc)]. *)


(** {2 Basic functions to use with formatters} *)

val pp_open_hbox : formatter -> unit -> unit
val pp_open_vbox : formatter -> int -> unit
val pp_open_hvbox : formatter -> int -> unit
val pp_open_hovbox : formatter -> int -> unit
val pp_open_box : formatter -> int -> unit
val pp_close_box : formatter -> unit -> unit
val pp_print_string : formatter -> string -> unit
val pp_print_as : formatter -> int -> string -> unit
val pp_print_int : formatter -> int -> unit
val pp_print_float : formatter -> float -> unit
val pp_print_char : formatter -> char -> unit
val pp_print_bool : formatter -> bool -> unit
val pp_print_break : formatter -> int -> int -> unit
val pp_print_cut : formatter -> unit -> unit
val pp_print_space : formatter -> unit -> unit
val pp_force_newline : formatter -> unit -> unit
val pp_print_flush : formatter -> unit -> unit
val pp_print_newline : formatter -> unit -> unit
val pp_print_if_newline : formatter -> unit -> unit
val pp_open_tbox : formatter -> unit -> unit
val pp_close_tbox : formatter -> unit -> unit
val pp_print_tbreak : formatter -> int -> int -> unit
val pp_set_tab : formatter -> unit -> unit
val pp_print_tab : formatter -> unit -> unit
val pp_set_margin : formatter -> int -> unit
val pp_get_margin : formatter -> unit -> int
val pp_set_max_indent : formatter -> int -> unit
val pp_get_max_indent : formatter -> unit -> int
val pp_set_max_boxes : formatter -> int -> unit
val pp_get_max_boxes : formatter -> unit -> int
val pp_over_max_boxes : formatter -> unit -> bool
val pp_set_ellipsis_text : formatter -> string -> unit
val pp_get_ellipsis_text : formatter -> unit -> string
val pp_set_formatter_out_channel : formatter -> out_channel -> unit
val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)
val pp_set_all_formatter_output_functions :
  formatter -> out:(string -> int -> int -> unit) -> flush:(unit -> unit) ->
    newline:(unit -> unit) -> spaces:(int -> unit) -> unit
val pp_get_all_formatter_output_functions :
  formatter -> unit ->
    (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
      (int -> unit)
(** These functions are the basic ones: usual functions
   operating on the standard formatter are defined via partial
   evaluation of these primitives. For instance,
   [print_string] is equal to [pp_print_string std_formatter]. *)


(** {2 [printf] like functions for pretty-printing.} *)

val fprintf : formatter -> ('a, formatter, unit) format -> 'a
(** [fprintf ff format arg1 ... argN] formats the arguments
   [arg1] to [argN] according to the format string [format],
   and outputs the resulting string on the formatter [ff].
   The format is a character string which contains three types of
   objects: plain characters and conversion specifications as
   specified in the [printf] module, and pretty-printing
   indications.
   The pretty-printing indication characters are introduced by
   a [@] character, and their meanings are:
   - [@\[]: open a pretty-printing box. The type and offset of the
     box may be optionally specified with the following syntax:
     the [<] character, followed by an optional box type indication,
     then an optional integer offset, and the closing [>] character. 
     Box type is one of [h], [v], [hv], [b], or [hov],
     which stand respectively for an horizontal box, a vertical box,
     an ``horizontal-vertical'' box, or an ``horizontal or
     vertical'' box ([b] standing for an ``horizontal or
     vertical'' box demonstrating indentation and [hov] standing
     for a regular``horizontal or vertical'' box).
     For instance, [@\[<hov 2>] opens an ``horizontal or vertical''
     box with indentation 2 as obtained with [open_hovbox 2].
     For more details about boxes, see the various box opening
     functions [open_*box].
   - [@\]]: close the most recently opened pretty-printing box.
   - [@,]: output a good break as with [print_cut ()].
   - [@ ]: output a space, as with [print_space ()].
   - [@\n]: force a newline, as with [force_newline ()].
   - [@;]: output a good break as with [print_break]. The
     [nspaces] and [offset] parameters of the break may be
     optionally specified with the following syntax: 
     the [<] character, followed by an integer [nspaces] value,
     then an integer offset, and a closing [>] character. 
   - [@?]: flush the pretty printer as with [print_flush ()].
   - [@.]: flush the pretty printer and output a new line, as with
     [print_newline ()].
   - [@<n>]: print the following item as if it were of length [n].
     Hence, [printf "@<0>%s" arg] is equivalent to [print_as 0 arg].
     If [@<n>] is not followed by a conversion specification,
     then the following character of the format is printed as if
     it were of length [n].
   - [@@]: print a plain [@] character.
   
   Example: [printf "@[%s@ %d@]" "x =" 1] is equivalent to 
   [open_box (); print_string "x ="; print_space (); print_int 1; close_box ()].
   It prints [x = 1] within a pretty-printing box. *)

val printf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [std_formatter]. *)

val eprintf : ('a, formatter, unit) format -> 'a
(** Same as [fprintf] above, but output on [err_formatter]. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as [printf] above, but instead of printing on a formatter,
   return a string containing the result of formatting the arguments.
   Note that the pretty-printer queue is flushed at the end of each
   call to [sprintf].
   In case of multiple and related calls to [sprintf] to output material on a
   string, you should consider using [fprintf] with a
   formatter writing to a buffer: flushing the buffer at the
   end of pretty-printing returns the desired string. You can use the
   predefined formatter [str_formatter] and call [flush_str_formatter
   ()] to get the result. *)

val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a
(** Same as [sprintf] above, but instead of printing on a string,
   writes into the given extensible buffer.
   As for [sprintf], the pretty-printer queue is flushed at the end of each
   call to [bprintf].
   In case of multiple and related calls to [bprintf] to output material on the
   same buffer [b], you should consider using [fprintf] with a
   formatter writing to the buffer [b] (as obtained by
   [formatter_of_buffer b]), otherwise the repeated flushes of the
   pretty-printer queue would result in badly formatted output. *)
