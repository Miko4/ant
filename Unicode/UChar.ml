
open XNum;
open Types;

type category =
[ Lu | Ll | Lt | Lm | Lo
| Mn | Mc | Me
| Nd | Nl | No
| Pc | Pd | Ps | Pe | Pi | Pf | Po
| Sm | Sc | Sk | So
| Zs | Zl | Zp
| Cc | Cf | Cs | Co | Cn
];

type bidi_class =
[ L | LRE | LRO | R | AL | RLE | RLO | PDF | EN | ES | ET | AN | CS | NSM | BN | B | S | WS | ON ];

external name            : uc_char -> string     = "stub_name";
external category        : uc_char -> category   = "stub_category";
external combining_class : uc_char -> int        = "stub_combining_class";
external bidi_class      : uc_char -> bidi_class = "stub_bidi_class";
external decomposition   : uc_char -> string     = "stub_decomposition";
external number1         : uc_char -> int        = "stub_number1";
external number2         : uc_char -> int        = "stub_number2";
external number3         : uc_char -> int        = "stub_number3";
external number4         : uc_char -> int        = "stub_number4";
external mirrored        : uc_char -> bool       = "stub_mirrored";
external comment         : uc_char -> string     = "stub_comment";
external to_upper        : uc_char -> uc_char    = "stub_to_upper";
external to_lower        : uc_char -> uc_char    = "stub_to_lower";
external to_title        : uc_char -> uc_char    = "stub_to_title";

external is_letter       : uc_char -> bool       = "stub_is_letter";
external is_mark         : uc_char -> bool       = "stub_is_mark";
external is_number       : uc_char -> bool       = "stub_is_number";
external is_punct        : uc_char -> bool       = "stub_is_punct";
external is_symbol       : uc_char -> bool       = "stub_is_symbol";
external is_separator    : uc_char -> bool       = "stub_is_separator";
external is_control      : uc_char -> bool       = "stub_is_control";
external is_space        : uc_char -> bool       = "stub_is_space";

value number c = (number1 c, number2 c, num_of_ints (number3 c) (number4 c));

