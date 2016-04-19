
open Unicode.Types;
open Unicode.SymbolTable;
open Types;

type scope =
{
  symbol_table   : Hashtbl.t uc_string Lexer.token_class;
  global_symbols : Hashtbl.t int unknown;
  local_symbols  : Hashtbl.t int (int * int);
  depth          : int;
  shift          : int
};

value create () =
{
  symbol_table   = Lexer.initial_symbol_table  ();
  global_symbols = Hashtbl.create 1000;
  local_symbols  = Hashtbl.create 100;
  depth          = 0;
  shift          = 0
};

value symbol_table scope = scope.symbol_table;

value add_bin_op scope pri assoc sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add scope.symbol_table str (Lexer.BINOP sym pri assoc)
};

value add_pre_op scope sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add scope.symbol_table str (Lexer.PREOP sym)
};

value add_post_op scope sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add scope.symbol_table str (Lexer.POSTOP sym)
};

value copy scope =
{
  symbol_table   = Hashtbl.copy scope.symbol_table;
  global_symbols = Hashtbl.copy scope.global_symbols;
  local_symbols  = Hashtbl.copy scope.local_symbols;
  depth          = scope.depth;
  shift          = scope.shift
};

value add_global scope symbol val = do
{
  Hashtbl.replace scope.global_symbols symbol (create_unknown val)
};

value push scope symbols = do
{
  let local = Hashtbl.copy scope.local_symbols;
  let depth = scope.depth + 1;

  iter 0 symbols

  where rec iter n symbols = match symbols with
  [ [] ->
      ({
        (scope)
         with
         local_symbols  = local;
         depth          = depth
       },
       n)
  | [s::ss] -> do
    {
      Hashtbl.replace local s (depth, n);
      iter (n+1) ss
    }
  ]
};

value shift scope off = do
{
  { (scope) with shift = scope.shift + off }
};

value lookup_local scope symbol = do
{
  let (level, index) = Hashtbl.find scope.local_symbols symbol;

  (scope.depth - level + scope.shift, index)
};

value lookup_global scope symbol = do
{
  Hashtbl.find scope.global_symbols symbol
};

value lookup scope symbol = do
{
  try
    let (depth, index) = lookup_local scope symbol;

    BVariable depth index
  with
  [ Not_found -> BGlobal (lookup_global scope symbol) ]
};

(*
value print_scope s = do
{
  Runtime.Logging.log_string "globals:\n";
  Hashtbl.iter
    (fun n _ -> do
      {
        Runtime.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
        Runtime.Logging.log_string "\n"
      })
    s.global_symbols;

  Hashtbl.iter
    (fun n _ -> do
      {
        Runtime.Logging.log_uni_string (Unicode.SymbolTable.symbol_to_string n);
        Runtime.Logging.log_string "\n"
      })
    s.local_symbols
};
*)

