
open Runtime;
open VM;
open Types;
open Typesetting;
open Engine;
open Environment;
open Evaluate;

value encode_gfx_cmd : Graphic.graphic_command dim_arg Box.box -> partial_value;
value decode_gfx_cmd : string -> unknown -> Graphic.graphic_command dim_arg Box.box;

value encode_line_cap  : Graphic.line_cap -> partial_value;
value encode_line_join : Graphic.line_join -> partial_value;
value decode_line_cap  : string -> unknown -> Graphic.line_cap;
value decode_line_join : string -> unknown -> Graphic.line_join;

