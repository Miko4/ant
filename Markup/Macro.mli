
open Runtime;
open Unicode.Types;
open ParseState;

type arg_specifier =
[ Arg
| Opt of uc_list
| Bool
];

value parse_arg_template : UCStream.location -> uc_list -> list arg_specifier;
value substitute         : list uc_list -> uc_list -> uc_list;
value expand             : parse_state -> uc_list;
value noexpand           : parse_state -> uc_list -> uc_list;
value expand_string      : parse_state -> uc_list -> uc_list;

value execute_macro      : list arg_specifier -> uc_list -> parse_state -> unit;
value expand_macro       : list arg_specifier -> uc_list -> parse_state ->
                           uc_list -> uc_list;
value execute_command    : parse_state -> uc_list -> unit;

value begin_env          : parse_state -> uc_list -> unit;
value end_env            : parse_state -> uc_list -> unit;

value execute_begin_environment : uc_list -> list arg_specifier -> uc_list -> parse_state -> unit;
value expand_begin_environment  : uc_list -> list arg_specifier -> uc_list -> parse_state ->
                                  uc_list -> uc_list;
value execute_end_environment   : uc_list -> parse_state -> unit;
value expand_end_environment    : uc_list -> parse_state -> uc_list -> uc_list;

