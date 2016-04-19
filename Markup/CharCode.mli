
open Unicode.Types;

type cat_code =
[ Letter
| Newline
| Space
| Escape
| BeginGroup
| EndGroup
| BeginOptional
| EndOptional
| Macro
| Comment
| Other
| EOF
];

value macro_char          : uc_char;
value comment_char        : uc_char;
value escape_char         : uc_char;
value begin_optional_char : uc_char;
value end_optional_char   : uc_char;
value begin_group_char    : uc_char;
value end_group_char      : uc_char;

value cat_code : uc_char -> cat_code;

