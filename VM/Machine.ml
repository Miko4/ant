
open XNum;
open Types;
open VMPrivate;
open Runtime;

value tracing_bytecode = Evaluate.tracing_bytecode;

(* scopes *)

type scope = Scope.scope;

value make_scope     = Primitives.initial_scope;
value bind_primitive = Primitives.bind_primitive;
value bind_bin_op_l  = Primitives.bind_bin_op_l;
value bind_bin_op_n  = Primitives.bind_bin_op_n;
value bind_bin_op_r  = Primitives.bind_bin_op_r;
value bind_pre_op    = Primitives.bind_pre_op;
value bind_post_op   = Primitives.bind_post_op;

(* symbols *)

value string_to_symbol = Unicode.SymbolTable.string_to_symbol;
value symbol_to_string = Unicode.SymbolTable.symbol_to_string;

(* evaluation *)

value uc_string_to_char_list = Primitives.uc_string_to_char_list;
value uc_list_to_char_list   = Primitives.uc_list_to_char_list;
value ascii_to_char_list     = Primitives.ascii_to_char_list;

value execute_declarations scope decls  = do
{
  let code = Compile.compile_declarations scope decls;

  ignore (Evaluate.execute code [ref Unbound])
};

value execute                = Evaluate.execute;
value evaluate_lin_form      = Evaluate.evaluate_lin_form;
value evaluate_opaque        = Evaluate.evaluate_opaque;
value unify                  = Evaluate.unify;

value set_unknown x v = do
{
  Evaluate.forced_unify x (ref v)
};

value evaluate_expression scope stream = do
{
  let code = Compile.compile_expression scope stream;

  let x = Evaluate.execute code [];

  !x
};

value evaluate_function f args = do
{
  Evaluate.execute [| BGlobal f; BApply (List.length args) |] args
};

value evaluate_monad_expr scope stream init = do
{
  let code = Compile.compile_expression scope stream;

  let f = Evaluate.execute code [];

  let x = evaluate_function f [ref init];

  !x
};

value decode_string = Primitives.evaluate_char_list;
value decode_list   = Evaluate.evaluate_list;
value decode_num    = Evaluate.evaluate_num;

value evaluate_string_expr name scope stream = do
{
  decode_string name (ref (evaluate_expression scope stream))
};

value lookup_symbol scope sym = do
{
  let x = Scope.lookup_global scope (string_to_symbol sym);
  !x
};

