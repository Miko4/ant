
open Runtime;
open VM;
open Types;
open Typesetting;
open Engine;
open Environment;
open Node;

value encode_box_cmd : Box.box_cmd -> partial_value;
value decode_box_cmd : string -> unknown -> Box.box_cmd;

value encode_area_contents : area_contents_arg -> partial_value;
value decode_area_contents : string -> unknown -> area_contents_arg;

value encode_glue_function : (environment -> list Box.box -> list Box.box) -> partial_value;
value decode_glue_function : string -> unknown -> (environment -> list Box.box -> list Box.box);

value encode_node      : node -> partial_value;
value encode_node_list : list node -> partial_value;
value decode_node      : string -> unknown -> node;
value decode_node_list : string -> unknown -> list node;

