
open XNum;
open Unicode.Types;
open FontMetric;

(* |load_font <name> <params>| tries to load the given font. *)

value load_font : string -> font_load_params -> font_metric;

