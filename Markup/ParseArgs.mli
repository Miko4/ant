
open XNum;
open Unicode.Types;
open Runtime;
open Engine;
open ParseState;

value arg_expanded : parse_state -> uc_list;
value arg_execute  : parse_state -> mode -> list Node.node;
value arg_num      : parse_state -> num;
value arg_int      : parse_state -> int;
value arg_skip     : parse_state -> Environment.skip_arg;
value arg_key_val  : parse_state -> DynUCTrie.t (option uc_list);
value arg_dim      : parse_state -> Environment.dim_arg;
value opt_expanded : parse_state -> uc_list -> uc_list;
value opt_key_val  : parse_state -> DynUCTrie.t (option uc_list);
value opt_int      : parse_state -> int -> int;
value arg_TeX_dim  : parse_state -> Environment.dim_arg;

