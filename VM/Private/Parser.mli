
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;
open Lexer;

(*
  Priorities:

   2 right ||
   3 right &&
   4 non   == <> > < >= <=
   5 left  land lor lxor lsr lsl
   6 left  + -
   7 left  * / mod
   8 left  ^
   9 left  function application
  11 right prefix operations
  12 left  postfix operations

*)

type pattern =
[ PAnything
| PId of symbol
| PNumber of num
| PChar of uc_char
| PSymbol of symbol
| PTuple of list pattern
| PList of list pattern
| PListTail of list pattern and pattern
| PAssign of symbol and pattern
];

type term =
[ TUnbound
| TId of symbol
| TNumber of num
| TChar of uc_char
| TSymbol of symbol
| TApp of term and list term
| TTuple of list term
| TList of list term
| TListTail of list term and term
| TFun of list (list pattern * option term * term)
| TLocal of list decl and term
| TSequence of list stmt and term
| TDo of list stmt
| TIfThenElse of term and term and term
| TMatch of term and list (pattern * option term * term)
]
and decl =
[ DFun of symbol and list pattern and option term and term
| DPattern of pattern and term
]
and stmt =
[ SEquation of term and term
| SIfThen of term and stmt
| SIfThenElse of term and stmt and stmt
(*| SForce of array term*)
| SFunction of term
];

value parse_program    : lexer -> list decl;
value parse_expression : lexer -> term;

