
open Runtime;
open Unicode;
open Types;
open Typesetting.Box;

(* catcodes and math-codes *)

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

value macro_char          =  35;  (* # *)
value comment_char        =  37;  (* % *)
value begin_optional_char =  91;  (* [ *)
value escape_char         =  92;  (* \ *)
value end_optional_char   =  93;  (* ] *)
value begin_group_char    = 123;  (* { *)
value end_group_char      = 125;  (* } *)

value default_cat_codes_xx =
[|
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other
|];

value default_cat_codes_00 =
[|
  Space; Space; Space; Space; Space; Space; Space; Space;
  Space; Space;
  Newline;       (* newline *)
  Space; Space; Space; Space; Space;
  Space; Space; Space; Space; Space; Space; Space; Space;
  Space; Space; Space; Space; Space; Space; Space; Space;
  Space;         (* space *)
  Other;         (* exclamation mark *)
  Other;         (* quotation mark *)
  Macro;         (* number sign *)
  Other;         (* dollar sign *)
  Comment;       (* percent sign *)
  Other;         (* ampersand *)
  Other;         (* apostrophe *)
  Other;         (* left parenthesis *)
  Other;         (* right parenthesis *)
  Other;         (* asterisk *)
  Other;         (* plus sign *)
  Other;         (* comma *)
  Other;         (* hyphen-minus *)
  Other;         (* period *)
  Other;         (* slash *)
  Other;         (* digit zero *)
  Other;         (* digit one *)
  Other;         (* digit two *)
  Other;         (* digit three *)
  Other;         (* digit four *)
  Other;         (* digit five *)
  Other;         (* digit six *)
  Other;         (* digit seven *)
  Other;         (* digit eight *)
  Other;         (* digit nine *)
  Other;         (* colon *)
  Other;         (* semicolon *)
  Other;         (* less-than sign *)
  Other;         (* equals sign *)
  Other;         (* greater-than sign *)
  Other;         (* question mark *)
  Other;         (* commercial at *)
  Letter;        (* latin capital letter a *)
  Letter;        (* latin capital letter b *)
  Letter;        (* latin capital letter c *)
  Letter;        (* latin capital letter d *)
  Letter;        (* latin capital letter e *)
  Letter;        (* latin capital letter f *)
  Letter;        (* latin capital letter g *)
  Letter;        (* latin capital letter h *)
  Letter;        (* latin capital letter i *)
  Letter;        (* latin capital letter j *)
  Letter;        (* latin capital letter k *)
  Letter;        (* latin capital letter l *)
  Letter;        (* latin capital letter m *)
  Letter;        (* latin capital letter n *)
  Letter;        (* latin capital letter o *)
  Letter;        (* latin capital letter p *)
  Letter;        (* latin capital letter q *)
  Letter;        (* latin capital letter r *)
  Letter;        (* latin capital letter s *)
  Letter;        (* latin capital letter t *)
  Letter;        (* latin capital letter u *)
  Letter;        (* latin capital letter v *)
  Letter;        (* latin capital letter w *)
  Letter;        (* latin capital letter x *)
  Letter;        (* latin capital letter y *)
  Letter;        (* latin capital letter z *)
  BeginOptional; (* left square bracket *)
  Escape;        (* backslash *)
  EndOptional;   (* right square bracket *)
  Other;         (* circumflex accent *)
  Other;         (* underline *)
  Other;         (* grave accent *)
  Letter;        (* latin small letter a *)
  Letter;        (* latin small letter b *)
  Letter;        (* latin small letter c *)
  Letter;        (* latin small letter d *)
  Letter;        (* latin small letter e *)
  Letter;        (* latin small letter f *)
  Letter;        (* latin small letter g *)
  Letter;        (* latin small letter h *)
  Letter;        (* latin small letter i *)
  Letter;        (* latin small letter j *)
  Letter;        (* latin small letter k *)
  Letter;        (* latin small letter l *)
  Letter;        (* latin small letter m *)
  Letter;        (* latin small letter n *)
  Letter;        (* latin small letter o *)
  Letter;        (* latin small letter p *)
  Letter;        (* latin small letter q *)
  Letter;        (* latin small letter r *)
  Letter;        (* latin small letter s *)
  Letter;        (* latin small letter t *)
  Letter;        (* latin small letter u *)
  Letter;        (* latin small letter v *)
  Letter;        (* latin small letter w *)
  Letter;        (* latin small letter x *)
  Letter;        (* latin small letter y *)
  Letter;        (* latin small letter z *)
  BeginGroup;    (* left curly bracket *)
  Other;         (* vertical line *)
  EndGroup;      (* right curly bracket *)
  Other;         (* tilde *)
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other;
  Other;         (* no-break space *)
  Other;         (* inverted exclamation mark *)
  Other;         (* cent sign *)
  Other;         (* pound sign *)
  Other;         (* currency sign *)
  Other;         (* yen sign *)
  Other;         (* broken bar *)
  Other;         (* section sign *)
  Other;         (* diaeresis *)
  Other;         (* copyright sign *)
  Other;         (* feminine ordinal indicator *)
  Other;         (* left guillemet *)
  Other;         (* not sign *)
  Other;         (* soft hyphen *)
  Other;         (* registered trade mark sign *)
  Other;         (* macron, overline *)
  Other;         (* degree sign *)
  Other;         (* plus-minus sign *)
  Other;         (* superscript two *)
  Other;         (* superscript three *)
  Other;         (* acute accent *)
  Other;         (* micro sign *)
  Other;         (* paragraph sign *)
  Other;         (* middle dot, kana conjoctive *)
  Other;         (* cedilla *)
  Other;         (* superscript one *)
  Other;         (* masculine ordinal indicator *)
  Other;         (* right guillemet *)
  Other;         (* vulgar fraction one quarter *)
  Other;         (* vulgar fraction one half *)
  Other;         (* vulgar fraction three quarters *)
  Other;         (* inverted question mark *)
  Letter;        (* latin capital letter a with grave accent *)
  Letter;        (* latin capital letter a with acute accent *)
  Letter;        (* latin capital letter a with circumflex accent *)
  Letter;        (* latin capital letter a with tilde *)
  Letter;        (* latin capital letter a with diaeresis *)
  Letter;        (* latin capital letter a with ring above *)
  Letter;        (* latin capital letter a with e *)
  Letter;        (* latin capital letter c with cedilla *)
  Letter;        (* latin capital letter e with grave accent *)
  Letter;        (* latin capital letter e with acute accent *)
  Letter;        (* latin capital letter e with circumflex accent *)
  Letter;        (* latin capital letter e with diaeresis *)
  Letter;        (* latin capital letter i with grave accent *)
  Letter;        (* latin capital letter i with acute accent *)
  Letter;        (* latin capital letter i with circumflex accent *)
  Letter;        (* latin capital letter i with diaeresis *)
  Letter;        (* latin capital letter eth *)
  Letter;        (* latin capital letter n with tilde *)
  Letter;        (* latin capital letter o with grave accent *)
  Letter;        (* latin capital letter o with acute accent *)
  Letter;        (* latin capital letter o with circumflex accent *)
  Letter;        (* latin capital letter o with tilde *)
  Letter;        (* latin capital letter o with diaeresis *)
  Other;         (* multiplication sign *)
  Letter;        (* latin capital letter o with oblique stroke *)
  Letter;        (* latin capital letter u with grave accent *)
  Letter;        (* latin capital letter u with acute accent *)
  Letter;        (* latin capital letter u with circumflex accent *)
  Letter;        (* latin capital letter u with diaeresis *)
  Letter;        (* latin capital letter y with acute accent *)
  Letter;        (* latin capital letter thorn *)
  Letter;        (* latin small letter sharp s *)
  Letter;        (* latin small letter a with grave accent *)
  Letter;        (* latin small letter a with acute accent *)
  Letter;        (* latin small letter a with circumflex accent *)
  Letter;        (* latin small letter a with tilde *)
  Letter;        (* latin small letter a with diaeresis *)
  Letter;        (* latin small letter a with ring above *)
  Letter;        (* latin small letter a with e *)
  Letter;        (* latin small letter c with cedilla *)
  Letter;        (* latin small letter e with grave accent *)
  Letter;        (* latin small letter e with acute accent *)
  Letter;        (* latin small letter e with circumflex accent *)
  Letter;        (* latin small letter e with diaeresis *)
  Letter;        (* latin small letter i with grave accent *)
  Letter;        (* latin small letter i with acute accent *)
  Letter;        (* latin small letter i with circumflex accent *)
  Letter;        (* latin small letter i with diaeresis *)
  Letter;        (* latin small letter eth *)
  Letter;        (* latin small letter n with tilde *)
  Letter;        (* latin small letter o with grave accent *)
  Letter;        (* latin small letter o with acute accent *)
  Letter;        (* latin small letter o with circumflex accent *)
  Letter;        (* latin small letter o with tilde *)
  Letter;        (* latin small letter o with diaeresis *)
  Other;         (* division sign *)
  Letter;        (* latin small letter o with oblique stroke *)
  Letter;        (* latin small letter u with grave accent *)
  Letter;        (* latin small letter u with acute accent *)
  Letter;        (* latin small letter u with circumflex accent *)
  Letter;        (* latin small letter u with diaeresis *)
  Letter;        (* latin small letter y with acute accent *)
  Letter;        (* latin small letter thorn *)
  Letter         (* latin small letter y with diaeresis *)
|];

value default_cat_codes_01 =
[|
  Letter;  (* latin capital letter a with macron *)
  Letter;  (* latin small letter a with macron *)
  Letter;  (* latin capital letter a with breve *)
  Letter;  (* latin small letter a with breve *)
  Letter;  (* latin capital letter a with ogonek *)
  Letter;  (* latin small letter a with ogonek *)
  Letter;  (* latin capital letter c with acute accent *)
  Letter;  (* latin small letter c with acute accent *)
  Letter;  (* latin capital letter c with circumflex *)
  Letter;  (* latin small letter c with circumflex *)
  Letter;  (* latin capital letter c with dot above *)
  Letter;  (* latin small letter c with dot above *)
  Letter;  (* latin capital letter c with caron *)
  Letter;  (* latin small letter c with caron *)
  Letter;  (* latin capital letter d with hacek *)
  Letter;  (* latin small letter d with hacek *)
  Letter;  (* latin capital letter d with stroke *)
  Letter;  (* latin small letter d with stroke *)
  Letter;  (* latin capital letter e with macron *)
  Letter;  (* latin small letter e with macron *)
  Letter;  (* latin capital letter e with breve *)
  Letter;  (* latin small letter e with breve *)
  Letter;  (* latin capital letter e with dot above *)
  Letter;  (* latin small letter e with dot above *)
  Letter;  (* latin capital letter e with ogonek *)
  Letter;  (* latin small letter e with ogonek *)
  Letter;  (* latin capital letter e with hacek *)
  Letter;  (* latin small letter e with hacek *)
  Letter;  (* latin capital letter g with circumflex *)
  Letter;  (* latin small letter g with circumflex *)
  Letter;  (* latin capital letter g with breve *)
  Letter;  (* latin small letter g with breve *)
  Letter;  (* latin capital letter g with dot above *)
  Letter;  (* latin small letter g with dot above *)
  Letter;  (* latin capital letter g with cedilla *)
  Letter;  (* latin small letter g with cedilla *)
  Letter;  (* latin capital letter h with circumflex *)
  Letter;  (* latin small letter h with circumflex *)
  Letter;  (* latin capital letter h with stroke *)
  Letter;  (* latin small letter h with stroke *)
  Letter;  (* latin capital letter i with tilde *)
  Letter;  (* latin small letter i with tilde *)
  Letter;  (* latin capital letter i with macron *)
  Letter;  (* latin small letter i with macron *)
  Letter;  (* latin capital letter i with breve *)
  Letter;  (* latin small letter i with breve *)
  Letter;  (* latin capital letter i with ogonek *)
  Letter;  (* latin small letter i with ogonek *)
  Letter;  (* latin capital letter i with dot above *)
  Letter;  (* latin small letter i without dot above *)
  Letter;  (* latin capital ligature ij *)
  Letter;  (* latin small ligature ij *)
  Letter;  (* latin capital letter j with circumflex *)
  Letter;  (* latin small letter j with circumflex *)
  Letter;  (* latin capital letter k with cedilla *)
  Letter;  (* latin small letter k with cedilla *)
  Letter;  (* latin small letter kra *)
  Letter;  (* latin capital letter l with acute accent *)
  Letter;  (* latin small letter l with acute accent *)
  Letter;  (* latin capital letter l with cedilla *)
  Letter;  (* latin small letter l with cedilla *)
  Letter;  (* latin capital letter l with hacek *)
  Letter;  (* latin small letter l with hacek *)
  Letter;  (* latin capital letter l with middle dot *)
  Letter;  (* latin small letter l with middle dot *)
  Letter;  (* latin capital letter l with stroke *)
  Letter;  (* latin small letter l with stroke *)
  Letter;  (* latin capital letter n with acute accent *)
  Letter;  (* latin small letter n with acute accent *)
  Letter;  (* latin capital letter n with cedilla *)
  Letter;  (* latin small letter n with cedilla *)
  Letter;  (* latin capital letter n with hacek *)
  Letter;  (* latin small letter n with hacek *)
  Letter;  (* latin small letter n preceded by apostrophe *)
  Letter;  (* latin capital letter eng *)
  Letter;  (* latin small letter eng *)
  Letter;  (* latin capital letter o with macron *)
  Letter;  (* latin small letter o with macron *)
  Letter;  (* latin capital letter o with breve *)
  Letter;  (* latin small letter o with breve *)
  Letter;  (* latin capital letter o with double acute accent *)
  Letter;  (* latin small letter o with double acute accent *)
  Letter;  (* latin capital ligature o with e *)
  Letter;  (* latin small ligature o with e *)
  Letter;  (* latin capital letter r with acute accent *)
  Letter;  (* latin small letter r with acute accent *)
  Letter;  (* latin capital letter r with cedilla *)
  Letter;  (* latin small letter r with cedilla *)
  Letter;  (* latin capital letter r with hacek *)
  Letter;  (* latin small letter r with hacek *)
  Letter;  (* latin capital letter s with acute accent *)
  Letter;  (* latin small letter s with acute accent *)
  Letter;  (* latin capital letter s with circumflex *)
  Letter;  (* latin small letter s with circumflex *)
  Letter;  (* latin capital letter s with cedilla *)
  Letter;  (* latin small letter s with cedilla *)
  Letter;  (* latin capital letter s with hacek *)
  Letter;  (* latin small letter s with hacek *)
  Letter;  (* latin capital letter t with cedilla *)
  Letter;  (* latin small letter t with cedilla *)
  Letter;  (* latin capital letter t with hacek *)
  Letter;  (* latin small letter t with hacek *)
  Letter;  (* latin capital letter t with stroke *)
  Letter;  (* latin small letter t with stroke *)
  Letter;  (* latin capital letter u with tilde *)
  Letter;  (* latin small letter u with tilde *)
  Letter;  (* latin capital letter u with macron *)
  Letter;  (* latin small letter u with macron *)
  Letter;  (* latin capital letter u with breve *)
  Letter;  (* latin small letter u with breve *)
  Letter;  (* latin capital letter u with ring above *)
  Letter;  (* latin small letter u with ring above *)
  Letter;  (* latin capital letter u with double acute accent *)
  Letter;  (* latin small letter u with double acute accent *)
  Letter;  (* latin capital letter u with ogonek *)
  Letter;  (* latin small letter u with ogonek *)
  Letter;  (* latin capital letter w with circumflex *)
  Letter;  (* latin cmall letter w with circumflex *)
  Letter;  (* latin capital letter y with circumflex *)
  Letter;  (* latin small letter y with circumflex *)
  Letter;  (* latin capital letter y with diaeresis *)
  Letter;  (* latin capital letter z with acute accent *)
  Letter;  (* latin small letter z with acute accent *)
  Letter;  (* latin capital letter z with dot above *)
  Letter;  (* latin small letter z with dot above *)
  Letter;  (* latin capital letter z with hacek *)
  Letter;  (* latin small letter z with hacek *)
  Letter;  (* latin small letter long s *)
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other;
  Other;             (* latin small letter script f, florin sign *)
  Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other;
  Letter;  (* latin capital letter a with ring above and acute *)
  Letter;  (* latin small letter a with ring above and acute *)
  Letter;  (* latin capital ligature ae with acute *)
  Letter;  (* latin small ligature ae with acute *)
  Letter;  (* latin capital letter o with stroke and acute *)
  Letter   (* latin small letter o with stroke and acute *)
|];

value default_cat_codes_03 =
[|
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other;
  Other;   (* greek tonos *)
  Other;   (* greek dialytika tonos *)
  Letter;  (* greek capital letter alpha with tonos *)
  Other;   (* greek ano teleia *)
  Letter;  (* greek capital letter epsilon with tonos *)
  Letter;  (* greek capital letter eta with tonos *)
  Letter;  (* greek capital letter iota with tonos *)
  Other;
  Letter;  (* greek capital letter omicron with tonos *)
  Other;
  Letter;  (* greek capital letter upsilon with tonos *)
  Letter;  (* greek capital letter omega with tonos *)
  Letter;  (* greek small letter iota with dialytika and tonos *)
  Letter;  (* greek capital letter alpha *)
  Letter;  (* greek capital letter beta *)
  Letter;  (* greek capital letter gamma *)
  Letter;  (* greek capital letter delta *)
  Letter;  (* greek capital letter epsilon *)
  Letter;  (* greek capital letter zeta *)
  Letter;  (* greek capital letter eta *)
  Letter;  (* greek capital letter theta *)
  Letter;  (* greek capital letter iota *)
  Letter;  (* greek capital letter kappa *)
  Letter;  (* greek capital letter lamda *)
  Letter;  (* greek capital letter mu *)
  Letter;  (* greek capital letter nu *)
  Letter;  (* greek capital letter xi *)
  Letter;  (* greek capital letter omicron *)
  Letter;  (* greek capital letter pi *)
  Letter;  (* greek capital letter rho *)
  Other;
  Letter;  (* greek capital letter sigma *)
  Letter;  (* greek capital letter tau *)
  Letter;  (* greek capital letter upsilon *)
  Letter;  (* greek capital letter phi *)
  Letter;  (* greek capital letter chi *)
  Letter;  (* greek capital letter psi *)
  Letter;  (* greek capital letter omega *)
  Letter;  (* greek capital letter iota with dialytika *)
  Letter;  (* greek capital letter upsilon with dialytika *)
  Letter;  (* greek small letter alpha with tonos *)
  Letter;  (* greek small letter epsilon with tonos *)
  Letter;  (* greek small letter eta with tonos *)
  Letter;  (* greek small letter iota with tonos *)
  Letter;  (* greek small letter upsilon with dialytika and tonos *)
  Letter;  (* greek small letter alpha *)
  Letter;  (* greek small letter beta *)
  Letter;  (* greek small letter gamma *)
  Letter;  (* greek small letter delta *)
  Letter;  (* greek small letter epsilon *)
  Letter;  (* greek small letter zeta *)
  Letter;  (* greek small letter eta *)
  Letter;  (* greek small letter theta *)
  Letter;  (* greek small letter iota *)
  Letter;  (* greek small letter kappa *)
  Letter;  (* greek small letter lamda *)
  Letter;  (* greek small letter mu *)
  Letter;  (* greek small letter nu *)
  Letter;  (* greek small letter xi *)
  Letter;  (* greek small letter omicron *)
  Letter;  (* greek small letter pi *)
  Letter;  (* greek small letter rho *)
  Letter;  (* greek small letter final sigma *)
  Letter;  (* greek small letter sigma *)
  Letter;  (* greek small letter tau *)
  Letter;  (* greek small letter upsilon *)
  Letter;  (* greek small letter phi *)
  Letter;  (* greek small letter chi *)
  Letter;  (* greek small letter psi *)
  Letter;  (* greek small letter omega *)
  Letter;  (* greek small letter iota with dialytika *)
  Letter;  (* greek small letter upsilon with dialytika *)
  Letter;  (* greek small letter omicron with tonos *)
  Letter;  (* greek small letter upsilon with tonos *)
  Letter;  (* greek small letter omega with tonos *)
  Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other
|];

value default_cat_codes_1e =
[|
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Letter;  (* latin capital letter w with grave *)
  Letter;  (* latin small letter w with grave *)
  Letter;  (* latin capital letter w with acute *)
  Letter;  (* latin small letter w with acute *)
  Letter;  (* latin capital letter w with diaeresis *)
  Letter;  (* latin small letter w with diaeresis *)
  Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other;
  Letter;  (* latin capital letter y with grave *)
  Letter;  (* latin small letter y with grave *)
  Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other
|];

value default_cat_codes_fb =
[|
  Letter;  (* latin small ligature ff *)
  Letter;  (* latin small ligature fi *)
  Letter;  (* latin small ligature fl *)
  Letter;  (* latin small ligature ffi *)
  Letter;  (* latin small ligature ffl *)
  Letter;  (* latin small ligature long s t *)
  Letter;  (* latin small ligature st *)
  Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other;
  Other; Other; Other; Other; Other; Other; Other; Other
|];

value cat_code_table = Charmap.build
[|
  default_cat_codes_00; default_cat_codes_01; default_cat_codes_xx; default_cat_codes_03;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_1e; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_fb;
  default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx; default_cat_codes_xx
|];

value cat_code char = do
{
  if char < 0 then
    EOF
  else
    Charmap.lookup cat_code_table char
};

(* vim:set foldmarker=[\|,\|] foldmethod=marker: *)
