
open XNum;
open Unicode.Types;
open Types;
open Runtime;
open Unicode.SymbolTable;

module UString = Unicode.UString;  (* we cannot open Unicode because of the name clash with Types *)
module UChar   = Unicode.UChar;

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

type lexer =
{
  input    : mutable UCStream.istream;
  tokens   : mutable list token_class;
  keywords : Hashtbl.t uc_string token_class
};

value syntax_error_uc lexer msg = do
{
  raise (Syntax_error (UCStream.location lexer.input) msg)
};

value syntax_error lexer msg = syntax_error_uc lexer (UString.uc_string_of_ascii msg);

type char_class = [ LetterLC | LetterUC | Digit | Symbol | Whitespace | Special of token_class];

value category_to_class cat = match cat with
[ UChar.Ll | UChar.Lm | UChar.Lo -> LetterLC
| UChar.Lu | UChar.Lt            -> LetterUC
| UChar.Mn | UChar.Mc | UChar.Me
| UChar.Zs | UChar.Zl | UChar.Zp
| UChar.Cc | UChar.Cf | UChar.Cs
| UChar.Co | UChar.Cn            -> Whitespace
| UChar.Nd | UChar.Nl | UChar.No -> Digit
| UChar.Pc | UChar.Pd | UChar.Ps
| UChar.Pe | UChar.Pi | UChar.Pf
| UChar.Po | UChar.Sm | UChar.Sc
| UChar.Sk | UChar.So            -> Symbol
];

value special_cases = Array.init 128 (fun c -> category_to_class (UChar.category c));

do
{
  special_cases.(0x22) := Special QUOTE;
  special_cases.(0x27) := Special APOSTROPHE;
  special_cases.(0x28) := Special PARENOPEN;
  special_cases.(0x29) := Special PARENCLOSE;
  special_cases.(0x2c) := Special COMMA;
  special_cases.(0x3b) := Special SEMICOLON;
  special_cases.(0x5b) := Special BRACKETOPEN;
  special_cases.(0x5d) := Special BRACKETCLOSE;
  special_cases.(0x5f) := Special UNDERSCORE;
  special_cases.(0x7b) := Special BRACEOPEN;
  special_cases.(0x7d) := Special BRACECLOSE
};

value char_class c = do
{
  if c < 128 then
    special_cases.(c)
  else
    category_to_class (UChar.category c);
};


value rec read_id istream = do
{
  let c = UCStream.next_char istream;

  if c < 0 then
    []
  else match char_class c with
  [ LetterLC | LetterUC -> read_id_alpha istream
  | Digit               -> read_id_num   istream
  | Symbol              -> read_id_sym   istream
  | _                   -> []
  ]
}
and read_id_alpha istream = do
{
  let c = UCStream.next_char istream;

  if c < 0 then
    []
  else match char_class c with
  [ LetterLC | LetterUC -> do
    {
      UCStream.pop istream;
      [c :: read_id_alpha istream]
    }
  | Special UNDERSCORE -> do
    {
      UCStream.pop istream;
      [c :: read_id istream]
    }
  | _ -> []
  ]
}
and read_id_sym istream = do
{
  let c = UCStream.next_char istream;

  if c < 0 then
    []
  else match char_class c with
  [ Symbol -> do
    {
      UCStream.pop istream;
      [c :: read_id_sym istream]
    }
  | Special UNDERSCORE -> do
    {
      UCStream.pop istream;
      [c :: read_id istream]
    }
  | _ -> []
  ]
}
and read_id_num istream = do
{
  let c = UCStream.next_char istream;

  if c < 0 then
    []
  else match char_class c with
  [ Digit -> do
    {
      UCStream.pop istream;
      [c :: read_id_num istream]
    }
  | Special UNDERSCORE -> do
    {
      UCStream.pop istream;
      [c :: read_id istream]
    }
  | _ -> []
  ]
};

value read_symbol istream = do
{
  let c = UCStream.next_char istream;

  if c < 0 then
    []
  else match char_class c with
  [ Symbol -> do
    {
      UCStream.pop istream;
      [c :: read_id_sym istream]
    }
  | _ -> []
  ]
};

value rec read_number istream first_char = do
{
  let (int, base) = read_natural istream first_char;

  let c = UCStream.next_char istream;

  if c < 0 then
    int
  else if c = 46 then do  (* . *)
  {
    UCStream.pop istream;

    read_fraction base int istream;
  }
  else if c = 47          (* / *)
       && char_class (UCStream.get_char istream 1) = Digit then do
  {
    UCStream.pop istream;

    let (denom, _) = read_natural istream (UCStream.pop istream);

    int // denom
  }
  else
    int
}
and read_natural istream first_char = do
{
  let base = select_base istream first_char;
  let b    = num_of_int base;

  iter (num_of_int (first_char - 48))

  where rec iter x = do
  {
    let d = read_digit base istream;

    if d < 0 then
      (x, base)
    else
      iter (b */ x +/ num_of_int d)
  }
}
and read_fraction base x istream = do
{
  let r = num_one // num_of_int base;

  iter x r

  where rec iter x y = do
  {
    let d = read_digit base istream;

    if d < 0 then
      x
    else
      iter (x +/ y */ num_of_int d) (y */ r)
  }
}
and select_base istream first_char = do
{
  if first_char <> 48 then
    10
  else match UCStream.next_char istream with
  [ 98  -> do { UCStream.pop istream;   2 }    (* b *)
  | 111 -> do { UCStream.pop istream;   8 }    (* o *)
  | 120 -> do { UCStream.pop istream;  16 }    (* x *)
  | _   -> 10
  ]
}
and read_digit base istream = do
{
  let c = UCStream.next_char istream;

  if c < 48 then
    -1
  else if c = 95 then do        (* _ *)
  {
    UCStream.pop istream;
    read_digit base istream
  }
  else if base <= 10 then do
  {
    if c < 48 + base then do
    {
      UCStream.pop istream;
      c - 48
    }
    else
      -1
  }
  else do
  {
    if c < 58 then do
    {
      UCStream.pop istream;
      c - 48
    }
    else if 97 <= c then do
    {
      if c < 87 + base then do
      {
        UCStream.pop istream;
        c - 87
      }
      else
        -1
    }
    else if 65 <= c && c < 55 + base then do
    {
      UCStream.pop istream;
      c - 55
    }
    else
      -1
  }
};

value read_character istream = do
{
  let c = UCStream.pop istream;

  if c <> 92 then  (* \ *)
    c
  else do
  {
    match UCStream.pop istream with
    [  98 ->  7 (* b *)
    | 101 -> 27 (* e *)
    | 102 -> 12 (* f *)
    | 110 -> 10 (* n *)
    | 114 -> 13 (* r *)
    | 116 ->  9 (* t *)
    | 117 -> do (* u *)
      {
        let d1 = read_digit 16 istream;
        let d2 = read_digit 16 istream;
        let d3 = read_digit 16 istream;
        let d4 = read_digit 16 istream;

        if d1 >= 0 && d2 >= 0 && d3 >= 0 && d4 >= 0 then
          0x1000 * d1 + 0x100 * d2 + 0x10 * d3 + d4
        else
          0
      }
    | 120 -> do (* x *)
      {
        let d1 = read_digit 16 istream;
        let d2 = read_digit 16 istream;

        if d1 >= 0 && d2 >= 0 then
          0x10 * d1 + d2
        else
          0
      }
    | c -> do   (* '\'', '\"', \, decimal digits, or an invalid sequence *)
      {
        if c >= 48 && c <= 57 then do
        {
          let d2 = UCStream.get_char istream 0;

          if d2 >= 48 && d2 <= 57 then do
          {
            let d3 = UCStream.get_char istream 1;

            if d3 >= 48 && d3 <= 57 then do
            {
              UCStream.remove istream 2;
              100 * (c - 48) + 10 * (d2 - 48) + d3 - 48
            }
            else do
            {
              UCStream.remove istream 1;
              10 * (c - 48) + d2 - 48
            }
          }
          else
            c - 48
        }
        else
          c
      }
    ]
  }
};

value read_char_constant lexer = do
{
  let c = read_character lexer.input;

  if UCStream.pop lexer.input <> 39 then    (* '\'' *)
    syntax_error lexer "' expected"
  else
    c
};

value read_string_constant lexer = do
{
  let str = ListBuilder.make ();

  iter ()

  where rec iter () = match UCStream.next_char lexer.input with
  [ 34   -> do   (* '\"' *)
    {
      UCStream.pop lexer.input;
      ListBuilder.get str
    }
  | (-1) -> syntax_error lexer "undelimited string constant"
  | _    -> do
    {
      ListBuilder.add str (read_character lexer.input);
      iter ()
    }
  ]
};

value keywords = do
{
  let t = Hashtbl.create 97;

  let add_token str token = do
  {
    let ustr = UString.uc_string_of_ascii str;

    Hashtbl.add t ustr token
  };
  let add_bin_op str pri ass = do
  {
    let ustr = UString.uc_string_of_ascii str;

    Hashtbl.add t ustr (BINOP (string_to_symbol ustr) pri ass)
  };
  let add_pre_op str = do
  {
    let ustr = UString.uc_string_of_ascii str;

    Hashtbl.add t ustr (PREOP (string_to_symbol ustr))
  };

  add_token "do"     DO;
  add_token "if"     IF;
  add_token "then"   THEN;
  add_token "else"   ELSE;
  add_token "elseif" ELSEIF;
  add_token "end"    END;
  add_token "force"  FORCE;
  add_token "begin"  BEGIN;
  add_token "match"  MATCH;
  add_token "with"   WITH;
  add_token "local"  LOCAL;
  add_token "where"  WHERE;

  add_token "="      EQUAL;
  add_token ":="     COLON_EQUAL;
  add_token "|"      BAR;
  add_token "&"      AMPERSAND;
  add_token "."      PERIOD;
  add_token ":"      COLON;
  add_token ";"      SEMICOLON;

  add_token "declare_infix_left"  (INFIX Left);
  add_token "declare_infix_non"   (INFIX NonA);
  add_token "declare_infix_right" (INFIX Right);
  add_token "declare_prefix"      PREFIX;
  add_token "declare_postfix"     POSTFIX;

  add_bin_op "land" 5 Left;
  add_bin_op "lor"  5 Left;
  add_bin_op "lxor" 5 Left;
  add_bin_op "lsr"  5 Left;
  add_bin_op "lsl"  5 Left;
  add_bin_op "mod"  7 Left;

  add_bin_op "||"   2 Right;
  add_bin_op "&&"   3 Right;
  add_bin_op "=="   4 NonA;
  add_bin_op "<>"   4 NonA;
  add_bin_op ">"    4 NonA;
  add_bin_op "<"    4 NonA;
  add_bin_op ">="   4 NonA;
  add_bin_op "<="   4 NonA;
  add_bin_op "+"    6 Left;
  add_bin_op "-"    6 Left;
  add_bin_op "*"    7 Left;
  add_bin_op "/"    7 Left;
  add_bin_op "^"    8 Left;
  add_pre_op "~";

  t
};

value initial_symbol_table () = Hashtbl.copy keywords;

value rec skip_line_comment istream = do
{
  let c = UCStream.pop istream;

  if c < 0 || c = 10 || c = 13 then
    ()
  else
    skip_line_comment istream
};

value rec skip_comment istream nest = match UCStream.pop istream with
[ (-1) -> ()
| 59   -> if UCStream.next_char istream = 93 then do    (* ;] *)
          {
            UCStream.remove istream 1;

            if nest > 0 then
              skip_comment istream (nest-1)
            else ()
          }
          else
            skip_comment istream nest
| 91   -> if UCStream.next_char istream = 59 then       (* [; *)
            skip_comment istream (nest+1)
          else
            skip_comment istream nest
| _    -> skip_comment istream nest
];

value make_lexer keyword_table stream =
{
  input    = stream;
  tokens   = [];
  keywords = keyword_table
};

value set_stream lexer stream = do
{
  lexer.input := stream
};

value rec read_token lexer = match lexer.tokens with
[ [t::ts] -> do
  {
    lexer.tokens := ts;
    t
  }
| [] -> do
  {
    let c = UCStream.pop lexer.input;

    if c < 0 then
      EOF
    else match char_class c with
    [ Whitespace -> read_token lexer
    | LetterLC   -> do
      {
        let lid = Array.of_list [c :: read_id_alpha lexer.input];

        try
          Hashtbl.find lexer.keywords lid
        with
        [ Not_found -> LID (string_to_symbol lid) ]
      }
    | LetterUC   -> UID (string_to_symbol (Array.of_list [c :: read_id_alpha lexer.input]))
    | Symbol     -> do
      {
        let sym = Array.of_list [c :: read_symbol lexer.input];

        try
          Hashtbl.find lexer.keywords sym
        with
        [ Not_found -> LID (string_to_symbol sym) ]
      }
    | Digit     -> NUMBER (read_number lexer.input c)
    | Special s -> match s with
      [ APOSTROPHE -> CHARACTER (read_char_constant lexer)
      | QUOTE      -> STRING  (read_string_constant lexer)
      | SEMICOLON  -> do
        {
          if UCStream.next_char lexer.input = 59 then do   (* ;; *)
          {
            skip_line_comment lexer.input;
            read_token lexer
          }
          else
            s
        }
      | BRACKETOPEN -> do
        {
          if UCStream.get_char lexer.input 0 = 59 then do  (* [; *)
          {
            skip_comment lexer.input 0;
            read_token lexer
          }
          else
            s
        }
      | _ -> s
      ]
    ]
  }
];

value restore_token lexer tok = do
{
  lexer.tokens := [tok :: lexer.tokens]
};

value add_bin_op lexer pri assoc sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add lexer.keywords str (BINOP sym pri assoc)
};

value add_pre_op lexer sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add lexer.keywords str (PREOP sym)
};

value add_post_op lexer sym = do
{
  let str = symbol_to_string sym;

  Hashtbl.add lexer.keywords str (POSTOP sym)
};

value token_to_string tok = match tok with
[ EOF           -> UString.uc_string_of_ascii "end-of-file"
| LID _         -> UString.uc_string_of_ascii "lowercase identifier"
| UID _         -> UString.uc_string_of_ascii "uppercase identifier"
| NUMBER _      -> UString.uc_string_of_ascii "number"
| CHARACTER c   -> Array.of_list [39; c; 39]
| STRING str    -> Array.of_list [34 :: str @ [34]]
| BINOP sym _ _ -> symbol_to_string sym
| PREOP sym     -> symbol_to_string sym
| POSTOP sym    -> symbol_to_string sym
| PARENOPEN     -> UString.uc_string_of_ascii "("
| PARENCLOSE    -> UString.uc_string_of_ascii ")"
| BRACKETOPEN   -> UString.uc_string_of_ascii "["
| BRACKETCLOSE  -> UString.uc_string_of_ascii "]"
| BRACEOPEN     -> UString.uc_string_of_ascii "{"
| BRACECLOSE    -> UString.uc_string_of_ascii "}"
| APOSTROPHE    -> UString.uc_string_of_ascii "'"
| QUOTE         -> UString.uc_string_of_ascii "\""
| COMMA         -> UString.uc_string_of_ascii ","
| COLON         -> UString.uc_string_of_ascii ":"
| SEMICOLON     -> UString.uc_string_of_ascii ";"
| UNDERSCORE    -> UString.uc_string_of_ascii "_"
| EQUAL         -> UString.uc_string_of_ascii "="
| COLON_EQUAL   -> UString.uc_string_of_ascii ":="
| BAR           -> UString.uc_string_of_ascii "|"
| AMPERSAND     -> UString.uc_string_of_ascii "&"
| PERIOD        -> UString.uc_string_of_ascii "."
| DO            -> UString.uc_string_of_ascii "do"
| IF            -> UString.uc_string_of_ascii "if"
| THEN          -> UString.uc_string_of_ascii "then"
| ELSE          -> UString.uc_string_of_ascii "else"
| ELSEIF        -> UString.uc_string_of_ascii "elseif"
| END           -> UString.uc_string_of_ascii "end"
| FORCE         -> UString.uc_string_of_ascii "force"
| BEGIN         -> UString.uc_string_of_ascii "begin"
| MATCH         -> UString.uc_string_of_ascii "match"
| WITH          -> UString.uc_string_of_ascii "with"
| LOCAL         -> UString.uc_string_of_ascii "local"
| WHERE         -> UString.uc_string_of_ascii "where"
| INFIX Left    -> UString.uc_string_of_ascii "declare_infix_left"
| INFIX NonA    -> UString.uc_string_of_ascii "declare_infix_non"
| INFIX Right   -> UString.uc_string_of_ascii "declare_infix_right"
| PREFIX        -> UString.uc_string_of_ascii "declare_prefix"
| POSTFIX       -> UString.uc_string_of_ascii "declare_postfix"
];

(* vim:set foldmarker=[\|,\|] foldmethod=marker: *)
