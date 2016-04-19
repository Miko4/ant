
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

exception Syntax_error of UCStream.location and uc_string;
exception Runtime_error of uc_string;

value runtime_error : string -> 'a;

(*
type compare = [ Equal | Less | LessEqual | Greater | GreaterEqual | Inequal ];

type relation = unit; (* FIX *)
*)

type unknown = ref partial_value

and partial_value =
[ Unbound
| Constraint of list unknown
| Bool of bool
| Number of num
| Char of uc_char
| Symbol of symbol
| LinForm of LinForm.lin_form unknown
| Primitive1 of unknown -> partial_value
| Primitive2 of unknown -> unknown -> partial_value
| PrimitiveN of int and (list unknown -> partial_value)
| Function of environment and int and array bytecode
| Chain of environment and array bytecode
| Relation of int and array bytecode        (* aritiy, local variables, and equations *)
| Application of partial_value and int and list unknown
| Nil
| List of unknown and unknown
| Tuple of array unknown
| Dictionary of SymbolMap.t unknown
| Opaque of Opaque.opaque unknown
]
and bytecode =
[ BDup
| BPop
| BPopN of int
| BConst of partial_value
| BGlobal of unknown
| BVariable of int and int
| BFunction of int and array bytecode
| BDictionary of array symbol
| BPair
| BTuple of int
| BSet of int and int
| BApply of int
| BReturn
| BCondJump of int
| BJump of int
| BLocal of int
| BEndLocal
| BMatch1 of list pattern_check and int and int and int
| BMatchN of array (list pattern_check) and int and int and int
| BUnify
| BRaise of string
]
and pattern_check =
[ PCAnything
| PCVariable of int
| PCNumber of num
| PCChar of uc_char
| PCSymbol of symbol
| PCTuple of int
| PCNil
| PCConsList
| PCAssign of int
]
and environment = list (array unknown);

value identical         : unknown -> unknown -> bool;
value compare_unknowns  : unknown -> unknown -> LinForm.compare_result;

value create_unbound    : 'a -> unknown;
value create_unknown    : partial_value -> unknown;
value add_constraint    : unknown -> list unknown -> list unknown;
value merge_constraints : list unknown -> list unknown -> list unknown;

value type_name         : partial_value -> string;

