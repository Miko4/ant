
open XNum;
open Runtime;
open Unicode.Types;
open Typesetting;
open Engine;

value match_substring   : UCStream.istream -> uc_list -> int -> bool;
value newline_to_par    : UCStream.istream -> unit;
value skip_spaces       : UCStream.istream -> unit;
value skip_blanks       : UCStream.istream -> unit;
value skip_comment      : UCStream.istream -> unit;
value read_token        : UCStream.istream -> uc_list;
value read_token_tail   : UCStream.istream -> uc_list;
value peek_token        : UCStream.istream -> uc_list;
value read_argument     : UCStream.istream -> uc_list;
value read_optional     : UCStream.istream -> uc_list -> uc_list;
value read_bool         : UCStream.istream -> bool;
value read_keyword      : UCStream.istream -> uc_list;
value read_key_val_list : UCStream.istream -> DynUCTrie.t (option uc_list);

value is_command_sequence : uc_list -> bool;
value is_token            : uc_list -> bool;

type expr 'a;

value make_expression      : 'a -> expr 'a;
value evaluate_expression  : expr 'a -> (num -> 'a) -> ('a -> 'b) ->
                             ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) ->
                             (num -> 'b -> 'b) -> 'b;
value read_expression      : (UCStream.istream -> expr 'a) -> (UCStream.location -> num -> 'a) ->
                             UCStream.istream -> expr 'a;

value read_unsigned_int    : UCStream.istream -> (int * int);
value read_int             : UCStream.istream -> (int * int);
value read_number          : UCStream.istream -> num;
value read_skip            : UCStream.istream -> Environment.skip_arg;
value read_skip_with_order : UCStream.istream -> (Environment.skip_arg * int);
value read_dim             : UCStream.istream -> Environment.dim_arg;

value read_num_expression         : UCStream.istream -> num;
value read_skip_expression        : UCStream.istream -> Environment.skip_arg;
value read_dim_expression         : UCStream.istream -> Environment.dim_arg;
value read_simple_num_expression  : UCStream.istream -> num;
value read_simple_skip_expression : UCStream.istream -> Environment.skip_arg;
value read_simple_dim_expression  : UCStream.istream -> Environment.dim_arg;

value read_range                  : UCStream.istream -> (num * num);

value str_to_bool      : uc_list -> bool;
value str_to_uint      : uc_list -> int;
value str_to_int       : uc_list -> int;
value str_to_num       : uc_list -> num;
value str_to_skip      : uc_list -> Environment.skip_arg;
value str_to_dim       : uc_list -> Environment.dim_arg;
value str_expr_to_num  : uc_list -> num;
value str_expr_to_skip : uc_list -> Environment.skip_arg;
value str_expr_to_dim  : uc_list -> Environment.dim_arg;
value str_to_list      : uc_list -> list (uc_list);
value str_to_key_val   : uc_list -> DynUCTrie.t (option uc_list);

