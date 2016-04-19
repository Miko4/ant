
open XNum;
open Runtime;
open ParseState;

value arg_expanded ps = do
{
  Macro.expand_string ps (Parser.read_argument ps.input_stream)
};
value arg_execute ps mode = do
{
  execute_argument_in_mode ps mode
};
value arg_num ps = do
{
  Parser.str_expr_to_num (arg_expanded ps)
};
value arg_int ps = do
{
  int_of_num (integer_num (arg_num ps))
};
value arg_skip ps = do
{
  Parser.str_expr_to_skip (arg_expanded ps)
};
value arg_key_val ps = do
{
  Parser.str_to_key_val (arg_expanded ps)
};
value arg_dim ps = do
{
  Parser.str_expr_to_dim (arg_expanded ps)
};
value opt_expanded ps default = do
{
  Macro.expand_string ps (Parser.read_optional ps.input_stream default)
};
value opt_key_val ps = do
{
  Parser.str_to_key_val (Parser.read_optional ps.input_stream [])
};
value opt_int ps default = do
{
  match opt_expanded ps [] with
  [ []  -> default
  | str -> int_of_num (integer_num (Parser.str_expr_to_num str))
  ]
};

(*
  |arg_TeX_dim| reads either a dim expression enclosed in braces or a simple dim expression
   without braces. This is used for compatibility wiht TeX commands like \vskip, \hskip, \kern.
*)

value arg_TeX_dim ps = do
{
  Parser.skip_blanks ps.input_stream;

  match CharCode.cat_code (UCStream.next_char ps.input_stream) with
  [ CharCode.BeginGroup | CharCode.Escape -> arg_dim ps
  | _ -> Parser.read_simple_dim_expression ps.input_stream
  ]
};

