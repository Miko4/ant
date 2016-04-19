
open Runtime;
open Unicode;
open Types;
open Typesetting;
open Engine;

value tracing_stacks : ref bool;
value tracing_macros : ref bool;
value tracing_input  : ref bool;

type mode =
[= `Preamble
|  `Galley
|  `Paragraph
|  `Math
|  `HBox
|  `LRBox
|  `RLBox
|  `VBox
|  `Table
];

type parse_state =
{
  job               : Job.job;
  input_stream      : UCStream.istream;
  parse_stack       : Stack.t (mode * ListBuilder.builder Node.node);
  default_char_cmd  : mutable command;
  command_table     : mutable DynUCTrie.t command;
  pattern_table     : mutable DynUCTrie.t command;
  saved_commands    : mutable DynUCTrie.t (list command);
  saved_patterns    : mutable DynUCTrie.t (list command);
  environment_table : mutable DynUCTrie.t (command * command);
  environment_stack : Stack.t (uc_list * list uc_list);
  math_codes        : mutable Charmap.charmap (Box.math_code * (int * int) * (uc_char * uc_char));
  al_scope          : mutable VM.Machine.scope;
  global_variables  : mutable SymbolTable.SymbolMap.t VM.Types.partial_value;
  counter_table     : mutable Counter.counter_table;
  old_references    : mutable DynUCTrie.t uc_string;
  references        : mutable DynUCTrie.t uc_string
}
and command =
{
  execute : parse_state -> unit;
  expand  : parse_state -> uc_list -> uc_list
};

value mode_to_string  : mode -> string;

value create          : Job.job -> parse_state;
value duplicate       : parse_state -> parse_state;
value set_stream      : parse_state -> UCStream.istream -> unit;
value location        : parse_state -> UCStream.location;

value open_node_list  : parse_state -> mode -> unit;
value close_node_list : parse_state -> mode -> list Node.node;
value add_node        : parse_state -> Node.node -> unit;
value current_mode    : parse_state -> mode;

value set_default_char_cmd : parse_state -> command -> unit;

value define_command       : parse_state -> uc_list -> command -> unit;
value define_pattern       : parse_state -> uc_list -> command -> unit;
value lookup_command       : parse_state -> uc_list -> option command;
value save_command         : parse_state -> uc_list -> unit;
value restore_command      : parse_state -> uc_list -> unit;
value save_pattern         : parse_state -> uc_list -> unit;
value restore_pattern      : parse_state -> uc_list -> unit;

value push_env             : parse_state -> uc_list -> list uc_list -> unit;
value pop_env              : parse_state -> (uc_list * list uc_list);
value set_env_args         : parse_state -> list uc_list -> unit;
value top_env              : parse_state -> (uc_list * list uc_list);

value lookup_env           : parse_state -> uc_list -> option (command * command);
value define_env           : parse_state -> uc_list -> command -> command -> unit;

value set_math_code_table  : parse_state -> Charmap.charmap (Box.math_code * (int * int) * (uc_char * uc_char)) -> unit;
value set_math_code        : parse_state -> uc_char -> Box.math_code -> int -> uc_char -> int -> uc_char -> unit;
value get_math_code        : parse_state -> uc_char -> (Box.math_code * (int * int) * (uc_char * uc_char));

value new_counter          : parse_state -> uc_string -> int -> option uc_string -> unit;
value get_counter          : parse_state -> uc_string -> int;
value set_counter          : parse_state -> uc_string -> int -> unit;

value gen_unique_name      : unit -> uc_list;

value add_reference        : parse_state -> uc_list -> uc_string -> unit;
value reference_exists     : parse_state -> uc_list -> bool;
value lookup_reference     : parse_state -> uc_list -> uc_string;
value iter_references      : parse_state -> (uc_string -> uc_string -> unit) -> unit;
value store_old_references : parse_state -> unit;
value compare_references   : parse_state -> bool;
value write_references     : parse_state -> string -> unit;

value execute_next_char        : parse_state -> bool;
value execute_stream           : parse_state -> UCStream.istream -> unit;
value execute_argument         : parse_state -> unit;
value execute_argument_in_mode : parse_state -> mode -> list Node.node;
value run_parser               : parse_state -> mode -> list Node.node;

value execute_string_in_mode   : parse_state -> uc_list -> mode -> list Node.node;

