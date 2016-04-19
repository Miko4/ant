
open XNum;
open Runtime;
open Unicode.Types;
open Unicode.SymbolTable;

type assoc = [ Left | NonA | Right ];

type token_class =
[ EOF
| LID of symbol
| UID of symbol
| NUMBER of num
| CHARACTER of int
| STRING of list int
| BINOP of symbol and int and assoc
| PREOP of symbol
| POSTOP of symbol
| PARENOPEN
| PARENCLOSE
| BRACKETOPEN
| BRACKETCLOSE
| BRACEOPEN
| BRACECLOSE
| APOSTROPHE
| QUOTE
| COMMA
| COLON
| SEMICOLON
| UNDERSCORE
| EQUAL
| COLON_EQUAL
| BAR
| AMPERSAND
| PERIOD
| DO
| IF
| THEN
| ELSE
| ELSEIF
| END
| FORCE
| BEGIN
| MATCH
| WITH
| LOCAL
| WHERE
| INFIX of assoc
| PREFIX
| POSTFIX
];

type lexer;

value initial_symbol_table : unit -> Hashtbl.t uc_string token_class;
value make_lexer           : Hashtbl.t uc_string token_class -> UCStream.istream -> lexer;
value set_stream           : lexer -> UCStream.istream -> unit;
value read_token           : lexer -> token_class;
value restore_token        : lexer -> token_class -> unit;
value add_bin_op           : lexer -> int -> assoc -> symbol -> unit;
value add_pre_op           : lexer -> symbol -> unit;
value add_post_op          : lexer -> symbol -> unit;
value token_to_string      : token_class -> uc_string;
value syntax_error         : lexer -> string -> 'a;
value syntax_error_uc      : lexer -> uc_string -> 'a;

