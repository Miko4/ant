
open Unicode;
open Substitute;

value empty_table =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value undefined = Charmap.build
[|
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table
|];

(* OT1 encoding *)

value uc_to_ot1_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* space *)
  Simple 0x21;       (* exclamation mark *)
  Simple 0x22;       (* quotation mark *)
  Simple 0x23;       (* number sign *)
  Simple 0x24;       (* dollar sign *)
  Simple 0x25;       (* percent sign *)
  Simple 0x26;       (* ampersand *)
  Simple 0x27;       (* apostrophe *)
  Simple 0x28;       (* left parenthesis *)
  Simple 0x29;       (* right parenthesis *)
  Simple 0x2a;       (* asterisk *)
  Simple 0x2b;       (* plus sign *)
  Simple 0x2c;       (* comma *)
  Simple 0x2d;       (* hyphen-minus *)
  Simple 0x2e;       (* period *)
  Simple 0x2f;       (* slash *)
  Simple 0x30;       (* digit zero *)
  Simple 0x31;       (* digit one *)
  Simple 0x32;       (* digit two *)
  Simple 0x33;       (* digit three *)
  Simple 0x34;       (* digit four *)
  Simple 0x35;       (* digit five *)
  Simple 0x36;       (* digit six *)
  Simple 0x37;       (* digit seven *)
  Simple 0x38;       (* digit eight *)
  Simple 0x39;       (* digit nine *)
  Simple 0x3a;       (* colon *)
  Simple 0x3b;       (* semicolon *)
  Undef;             (* less-than sign *)
  Simple 0x3d;       (* equals sign *)
  Undef;             (* greater-than sign *)
  Simple 0x3f;       (* question mark *)
  Simple 0x40;       (* commercial at *)
  Simple 0x41;       (* latin capital letter a *)
  Simple 0x42;       (* latin capital letter b *)
  Simple 0x43;       (* latin capital letter c *)
  Simple 0x44;       (* latin capital letter d *)
  Simple 0x45;       (* latin capital letter e *)
  Simple 0x46;       (* latin capital letter f *)
  Simple 0x47;       (* latin capital letter g *)
  Simple 0x48;       (* latin capital letter h *)
  Simple 0x49;       (* latin capital letter i *)
  Simple 0x4a;       (* latin capital letter j *)
  Simple 0x4b;       (* latin capital letter k *)
  Simple 0x4c;       (* latin capital letter l *)
  Simple 0x4d;       (* latin capital letter m *)
  Simple 0x4e;       (* latin capital letter n *)
  Simple 0x4f;       (* latin capital letter o *)
  Simple 0x50;       (* latin capital letter p *)
  Simple 0x51;       (* latin capital letter q *)
  Simple 0x52;       (* latin capital letter r *)
  Simple 0x53;       (* latin capital letter s *)
  Simple 0x54;       (* latin capital letter t *)
  Simple 0x55;       (* latin capital letter u *)
  Simple 0x56;       (* latin capital letter v *)
  Simple 0x57;       (* latin capital letter w *)
  Simple 0x58;       (* latin capital letter x *)
  Simple 0x59;       (* latin capital letter y *)
  Simple 0x5a;       (* latin capital letter z *)
  Simple 0x5b;       (* left square bracket *)
  Undef;             (* backslash *)
  Simple 0x5d;       (* right square bracket *)
  Simple 0x5e;       (* circumflex accent *)
  Undef;             (* underline *)
  Simple 0x12;       (* grave accent *)
  Simple 0x61;       (* latin small letter a *)
  Simple 0x62;       (* latin small letter b *)
  Simple 0x63;       (* latin small letter c *)
  Simple 0x64;       (* latin small letter d *)
  Simple 0x65;       (* latin small letter e *)
  Simple 0x66;       (* latin small letter f *)
  Simple 0x67;       (* latin small letter g *)
  Simple 0x68;       (* latin small letter h *)
  Simple 0x69;       (* latin small letter i *)
  Simple 0x6a;       (* latin small letter j *)
  Simple 0x6b;       (* latin small letter k *)
  Simple 0x6c;       (* latin small letter l *)
  Simple 0x6d;       (* latin small letter m *)
  Simple 0x6e;       (* latin small letter n *)
  Simple 0x6f;       (* latin small letter o *)
  Simple 0x70;       (* latin small letter p *)
  Simple 0x71;       (* latin small letter q *)
  Simple 0x72;       (* latin small letter r *)
  Simple 0x73;       (* latin small letter s *)
  Simple 0x74;       (* latin small letter t *)
  Simple 0x75;       (* latin small letter u *)
  Simple 0x76;       (* latin small letter v *)
  Simple 0x77;       (* latin small letter w *)
  Simple 0x78;       (* latin small letter x *)
  Simple 0x79;       (* latin small letter y *)
  Simple 0x7a;       (* latin small letter z *)
  Undef;             (* left curly bracket *)
  Undef;             (* vertical line *)
  Undef;             (* right curly bracket *)
  Simple 0x7e;       (* tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* no-break space *)
  Simple 0x3c;       (* inverted exclamation mark *)
  Undef;             (* cent sign *)
  Undef;             (* pound sign *)
  Undef;             (* currency sign *)
  Undef;             (* yen sign *)
  Undef;             (* broken bar *)
  Undef;             (* section sign *)
  Simple 0x7f;       (* diaeresis *)
  Undef;             (* copyright sign *)
  Undef;             (* feminine ordinal indicator *)
  Undef;             (* left guillemet *)
  Undef;             (* not sign *)
  Undef;             (* soft hyphen *)
  Undef;             (* registered trade mark sign *)
  Simple 0x16;       (* macron, overline *)
  Simple 0x17;       (* degree sign *)
  Undef;             (* plus-minus sign *)
  Undef;             (* superscript two *)
  Undef;             (* superscript three *)
  Simple 0x13;       (* acute accent *)
  Undef;             (* micro sign *)
  Undef;             (* paragraph sign *)
  Undef;             (* middle dot, kana conjoctive *)
  Simple 0x18;       (* cedilla *)
  Undef;             (* superscript one *)
  Undef;             (* masculine ordinal indicator *)
  Undef;             (* right guillemet *)
  Undef;             (* vulgar fraction one quarter *)
  Undef;             (* vulgar fraction one half *)
  Undef;             (* vulgar fraction three quarters *)
  Simple 0x3e;       (* inverted question mark *)
  Accent 0x12 0x41;  (* latin capital letter a with grave accent *)
  Accent 0x13 0x41;  (* latin capital letter a with acute accent *)
  Accent 0x5e 0x41;  (* latin capital letter a with circumflex accent *)
  Accent 0x7e 0x41;  (* latin capital letter a with tilde *)
  Accent 0x7f 0x41;  (* latin capital letter a with diaeresis *)
  Accent 0x12 0x41;  (* latin capital letter a with ring above *)
  Simple 0x1d;       (* latin capital letter a with e *)
  Accent 0x18 0x43;  (* latin capital letter c with cedilla *)
  Accent 0x12 0x45;  (* latin capital letter e with grave accent *)
  Accent 0x13 0x45;  (* latin capital letter e with acute accent *)
  Accent 0x5e 0x45;  (* latin capital letter e with circumflex accent *)
  Accent 0x7f 0x45;  (* latin capital letter e with diaeresis *)
  Accent 0x12 0x49;  (* latin capital letter i with grave accent *)
  Accent 0x13 0x49;  (* latin capital letter i with acute accent *)
  Accent 0x5e 0x49;  (* latin capital letter i with circumflex accent *)
  Accent 0x7f 0x49;  (* latin capital letter i with diaeresis *)
  Undef;             (* latin capital letter eth *)
  Accent 0x7e 0x4e;  (* latin capital letter n with tilde *)
  Accent 0x12 0x4f;  (* latin capital letter o with grave accent *)
  Accent 0x13 0x4f;  (* latin capital letter o with acute accent *)
  Accent 0x5e 0x4f;  (* latin capital letter o with circumflex accent *)
  Accent 0x7e 0x4f;  (* latin capital letter o with tilde *)
  Accent 0x7f 0x4f;  (* latin capital letter o with diaeresis *)
  Undef;             (* multiplication sign *)
  Simple 0x1f;       (* latin capital letter o with oblique stroke *)
  Accent 0x12 0x55;  (* latin capital letter u with grave accent *)
  Accent 0x13 0x55;  (* latin capital letter u with acute accent *)
  Accent 0x5e 0x55;  (* latin capital letter u with circumflex accent *)
  Accent 0x7f 0x55;  (* latin capital letter u with diaeresis *)
  Accent 0x7f 0x59;  (* latin capital letter y with acute accent *)
  Undef;             (* latin capital letter thorn *)
  Simple 0x19;       (* latin small letter sharp s *)
  Accent 0x12 0x61;  (* latin small letter a with grave accent *)
  Accent 0x13 0x61;  (* latin small letter a with acute accent *)
  Accent 0x5e 0x61;  (* latin small letter a with circumflex accent *)
  Accent 0x7e 0x61;  (* latin small letter a with tilde *)
  Accent 0x7f 0x61;  (* latin small letter a with diaeresis *)
  Accent 0x17 0x61;  (* latin small letter a with ring above *)
  Simple 0x1a;       (* latin small letter a with e *)
  Accent 0x18 0x63;  (* latin small letter c with cedilla *)
  Accent 0x12 0x65;  (* latin small letter e with grave accent *)
  Accent 0x13 0x65;  (* latin small letter e with acute accent *)
  Accent 0x5e 0x65;  (* latin small letter e with circumflex accent *)
  Accent 0x7f 0x65;  (* latin small letter e with diaeresis *)
  Accent 0x12 0x10;  (* latin small letter i with grave accent *)
  Accent 0x13 0x10;  (* latin small letter i with acute accent *)
  Accent 0x5e 0x10;  (* latin small letter i with circumflex accent *)
  Accent 0x7f 0x10;  (* latin small letter i with diaeresis *)
  Undef;             (* latin small letter eth *)
  Accent 0x7e 0x6e;  (* latin small letter n with tilde *)
  Accent 0x12 0x6f;  (* latin small letter o with grave accent *)
  Accent 0x13 0x6f;  (* latin small letter o with acute accent *)
  Accent 0x5e 0x6f;  (* latin small letter o with circumflex accent *)
  Accent 0x7e 0x6f;  (* latin small letter o with tilde *)
  Accent 0x7f 0x6f;  (* latin small letter o with diaeresis *)
  Undef;             (* division sign *)
  Simple 0x1c;       (* latin small letter o with oblique stroke *)
  Accent 0x12 0x75;  (* latin small letter u with grave accent *)
  Accent 0x13 0x75;  (* latin small letter u with acute accent *)
  Accent 0x5e 0x75;  (* latin small letter u with circumflex accent *)
  Accent 0x7f 0x75;  (* latin small letter u with diaeresis *)
  Accent 0x13 0x79;  (* latin small letter y with acute accent *)
  Undef;             (* latin small letter thorn *)
  Accent 0x7f 0x79   (* latin small letter y with diaeresis *)
|];

value uc_to_ot1_01 =
[|
  Accent 0x16 0x41;  (* latin capital letter a with macron *)
  Accent 0x16 0x61;  (* latin small letter a with macron *)
  Accent 0x15 0x41;  (* latin capital letter a with breve *)
  Accent 0x15 0x61;  (* latin small letter a with breve *)
  Undef;             (* latin capital letter a with ogonek *)
  Undef;             (* latin small letter a with ogonek *)
  Accent 0x13 0x43;  (* latin capital letter c with acute accent *)
  Accent 0x13 0x63;  (* latin small letter c with acute accent *)
  Accent 0x5e 0x43;  (* latin capital letter c with circumflex *)
  Accent 0x5e 0x63;  (* latin small letter c with circumflex *)
  Accent 0x5f 0x43;  (* latin capital letter c with dot above *)
  Accent 0x5f 0x63;  (* latin small letter c with dot above *)
  Accent 0x14 0x43;  (* latin capital letter c with caron *)
  Accent 0x14 0x63;  (* latin small letter c with caron *)
  Accent 0x14 0x44;  (* latin capital letter d with hacek *)
  Accent 0x14 0x64;  (* latin small letter d with hacek *)
  Undef;             (* latin capital letter d with stroke *)
  Undef;             (* latin small letter d with stroke *)
  Accent 0x16 0x45;  (* latin capital letter e with macron *)
  Accent 0x16 0x65;  (* latin small letter e with macron *)
  Accent 0x15 0x45;  (* latin capital letter e with breve *)
  Accent 0x15 0x65;  (* latin small letter e with breve *)
  Accent 0x5f 0x45;  (* latin capital letter e with dot above *)
  Accent 0x5f 0x65;  (* latin small letter e with dot above *)
  Undef;             (* latin capital letter e with ogonek *)
  Undef;             (* latin small letter e with ogonek *)
  Accent 0x14 0x45;  (* latin capital letter e with hacek *)
  Accent 0x14 0x65;  (* latin small letter e with hacek *)
  Accent 0x5e 0x47;  (* latin capital letter g with circumflex *)
  Accent 0x5e 0x67;  (* latin small letter g with circumflex *)
  Accent 0x15 0x47;  (* latin capital letter g with breve *)
  Accent 0x15 0x67;  (* latin small letter g with breve *)
  Accent 0x5f 0x47;  (* latin capital letter g with dot above *)
  Accent 0x5f 0x67;  (* latin small letter g with dot above *)
  Accent 0x18 0x47;  (* latin capital letter g with cedilla *)
  Undef;             (* latin small letter g with cedilla *)
  Accent 0x5e 0x48;  (* latin capital letter h with circumflex *)
  Accent 0x5e 0x68;  (* latin small letter h with circumflex *)
  Undef;             (* latin capital letter h with stroke *)
  Undef;             (* latin small letter h with stroke *)
  Accent 0x7e 0x49;  (* latin capital letter i with tilde *)
  Accent 0x7e 0x10;  (* latin small letter i with tilde *)
  Accent 0x16 0x49;  (* latin capital letter i with macron *)
  Accent 0x16 0x10;  (* latin small letter i with macron *)
  Accent 0x15 0x49;  (* latin capital letter i with breve *)
  Accent 0x15 0x10;  (* latin small letter i with breve *)
  Undef;             (* latin capital letter i with ogonek *)
  Undef;             (* latin small letter i with ogonek *)
  Accent 0x5f 0x49;  (* latin capital letter i with dot above *)
  Accent 0x5f 0x10;  (* latin small letter i without dot above *)
  Undef;             (* latin capital ligature ij *)
  Undef;             (* latin small ligature ij *)
  Accent 0x5e 0x4a;  (* latin capital letter j with circumflex *)
  Accent 0x5e 0x11;  (* latin small letter j with circumflex *)
  Accent 0x18 0x4b;  (* latin capital letter k with cedilla *)
  Accent 0x18 0x6b;  (* latin small letter k with cedilla *)
  Undef;             (* latin small letter kra *)
  Accent 0x13 0x4c;  (* latin capital letter l with acute accent *)
  Accent 0x13 0x6c;  (* latin small letter l with acute accent *)
  Accent 0x18 0x4c;  (* latin capital letter l with cedilla *)
  Accent 0x18 0x6c;  (* latin small letter l with cedilla *)
  Accent 0x14 0x4c;  (* latin capital letter l with hacek *)
  Accent 0x14 0x6c;  (* latin small letter l with hacek *)
  Accent 0x5f 0x4c;  (* latin capital letter l with middle dot *)
  Accent 0x5f 0x6c;  (* latin small letter l with middle dot *)
  Accent 0x20 0x4c;  (* latin capital letter l with stroke *)
  Accent 0x20 0x6c;  (* latin small letter l with stroke *)
  Accent 0x13 0x4e;  (* latin capital letter n with acute accent *)
  Accent 0x13 0x6e;  (* latin small letter n with acute accent *)
  Accent 0x18 0x4e;  (* latin capital letter n with cedilla *)
  Accent 0x18 0x6e;  (* latin small letter n with cedilla *)
  Accent 0x14 0x4e;  (* latin capital letter n with hacek *)
  Accent 0x14 0x6e;  (* latin small letter n with hacek *)
  Undef; (* FIX *)   (* latin small letter n preceded by apostrophe *)
  Undef;             (* latin capital letter eng *)
  Undef;             (* latin small letter eng *)
  Accent 0x16 0x4f;  (* latin capital letter o with macron *)
  Accent 0x16 0x6f;  (* latin small letter o with macron *)
  Accent 0x15 0x4f;  (* latin capital letter o with breve *)
  Accent 0x15 0x6f;  (* latin small letter o with breve *)
  Accent 0x13 0x4f;  (* latin capital letter o with double acute accent *)
  Accent 0x13 0x6f;  (* latin small letter o with double acute accent *)
  Simple 0x1e;       (* latin capital ligature o with e *)
  Simple 0x1b;       (* latin small ligature o with e *)
  Accent 0x13 0x52;  (* latin capital letter r with acute accent *)
  Accent 0x13 0x72;  (* latin small letter r with acute accent *)
  Accent 0x18 0x52;  (* latin capital letter r with cedilla *)
  Accent 0x18 0x72;  (* latin small letter r with cedilla *)
  Accent 0x14 0x52;  (* latin capital letter r with hacek *)
  Accent 0x14 0x72;  (* latin small letter r with hacek *)
  Accent 0x13 0x53;  (* latin capital letter s with acute accent *)
  Accent 0x13 0x73;  (* latin small letter s with acute accent *)
  Accent 0x5e 0x53;  (* latin capital letter s with circumflex *)
  Accent 0x5e 0x73;  (* latin small letter s with circumflex *)
  Accent 0x18 0x53;  (* latin capital letter s with cedilla *)
  Accent 0x18 0x73;  (* latin small letter s with cedilla *)
  Accent 0x14 0x53;  (* latin capital letter s with hacek *)
  Accent 0x14 0x73;  (* latin small letter s with hacek *)
  Accent 0x18 0x54;  (* latin capital letter t with cedilla *)
  Accent 0x18 0x74;  (* latin small letter t with cedilla *)
  Accent 0x14 0x54;  (* latin capital letter t with hacek *)
  Accent 0x14 0x74;  (* latin small letter t with hacek *)
  Undef;             (* latin capital letter t with stroke *)
  Undef;             (* latin small letter t with stroke *)
  Accent 0x7e 0x55;  (* latin capital letter u with tilde *)
  Accent 0x7e 0x75;  (* latin small letter u with tilde *)
  Accent 0x16 0x55;  (* latin capital letter u with macron *)
  Accent 0x16 0x75;  (* latin small letter u with macron *)
  Accent 0x15 0x55;  (* latin capital letter u with breve *)
  Accent 0x15 0x75;  (* latin small letter u with breve *)
  Accent 0x17 0x55;  (* latin capital letter u with ring above *)
  Accent 0x17 0x75;  (* latin small letter u with ring above *)
  Accent 0x13 0x55;  (* latin capital letter u with double acute accent *)
  Accent 0x13 0x75;  (* latin small letter u with double acute accent *)
  Undef;             (* latin capital letter u with ogonek *)
  Undef;             (* latin small letter u with ogonek *)
  Accent 0x5e 0x57;  (* latin capital letter w with circumflex *)
  Accent 0x5e 0x77;  (* latin cmall letter w with circumflex *)
  Accent 0x5e 0x59;  (* latin capital letter y with circumflex *)
  Accent 0x5e 0x79;  (* latin small letter y with circumflex *)
  Accent 0x7f 0x59;  (* latin capital letter y with diaeresis *)
  Accent 0x13 0x5a;  (* latin capital letter z with acute accent *)
  Accent 0x13 0x7a;  (* latin small letter z with acute accent *)
  Accent 0x5f 0x5a;  (* latin capital letter z with dot above *)
  Accent 0x5f 0x7a;  (* latin small letter z with dot above *)
  Accent 0x14 0x5a;  (* latin capital letter z with hacek *)
  Accent 0x14 0x7a;  (* latin small letter z with hacek *)
  Undef;             (* latin small letter long s *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin small letter script f, florin sign *)
  Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin capital letter a with ring above and acute *)
  Undef;             (* latin small letter a with ring above and acute *)
  Accent 0x13 0x1d;  (* latin capital ligature ae with acute *)
  Accent 0x13 0x1a;  (* latin small ligature ae with acute *)
  Accent 0x13 0x1f;  (* latin capital letter o with stroke and acute *)
  Accent 0x13 0x1c   (* latin small letter o with stroke and acute *)
|];

value uc_to_ot1_02 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x11;       (* latin small letter dotless j *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x5e;       (* nonspacing circumflex accent *)
  Simple 0x14;       (* modifier letter hacek *)
  Undef;
  Simple 0x16;       (* modifier letter macron *)
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x15;       (* breve *)
  Simple 0x5f;       (* dot above *)
  Simple 0x17;       (* ring above *)
  Undef;             (* ogonek *)
  Simple 0x7e;       (* nonspacing tilde *)
  Simple 0x7d;       (* modifier letter double prime *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_03 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* greek tonos *)
  Undef;             (* greek dialytika tonos *)
  Undef;             (* greek capital letter alpha with tonos *)
  Undef;             (* greek ano teleia *)
  Undef;             (* greek capital letter epsilon with tonos *)
  Undef;             (* greek capital letter eta with tonos *)
  Undef;             (* greek capital letter iota with tonos *)
  Undef;
  Undef;             (* greek capital letter omicron with tonos *)
  Undef;
  Undef;             (* greek capital letter upsilon with tonos *)
  Undef;             (* greek capital letter omega with tonos *)
  Undef;             (* greek small letter iota with dialytika and tonos *)
  Simple 0x41;       (* greek capital letter alpha *)
  Simple 0x41;       (* greek capital letter beta *)
  Simple 0x00;       (* greek capital letter gamma *)
  Simple 0x01;       (* greek capital letter delta *)
  Simple 0x45;       (* greek capital letter epsilon *)
  Simple 0x5a;       (* greek capital letter zeta *)
  Simple 0x48;       (* greek capital letter eta *)
  Simple 0x02;       (* greek capital letter theta *)
  Simple 0x49;       (* greek capital letter iota *)
  Simple 0x4b;       (* greek capital letter kappa *)
  Simple 0x03;       (* greek capital letter lamda *)
  Simple 0x4d;       (* greek capital letter mu *)
  Simple 0x4e;       (* greek capital letter nu *)
  Simple 0x04;       (* greek capital letter xi *)
  Simple 0x4f;       (* greek capital letter omicron *)
  Simple 0x05;       (* greek capital letter pi *)
  Simple 0x50;       (* greek capital letter rho *)
  Undef;
  Simple 0x06;       (* greek capital letter sigma *)
  Simple 0x54;       (* greek capital letter tau *)
  Simple 0x07;       (* greek capital letter upsilon *)
  Simple 0x08;       (* greek capital letter phi *)
  Simple 0x58;       (* greek capital letter chi *)
  Simple 0x09;       (* greek capital letter psi *)
  Simple 0x0a;       (* greek capital letter omega *)
  Accent 0x7f 0x49;  (* greek capital letter iota with dialytika *)
  Accent 0x7f 0x07;  (* greek capital letter upsilon with dialytika *)
  Undef;             (* greek small letter alpha with tonos *)
  Undef;             (* greek small letter epsilon with tonos *)
  Undef;             (* greek small letter eta with tonos *)
  Undef;             (* greek small letter iota with tonos *)
  Undef;             (* greek small letter upsilon with dialytika and tonos *)
  Undef;             (* greek small letter alpha *)
  Undef;             (* greek small letter beta *)
  Undef;             (* greek small letter gamma *)
  Undef;             (* greek small letter delta *)
  Undef;             (* greek small letter epsilon *)
  Undef;             (* greek small letter zeta *)
  Undef;             (* greek small letter eta *)
  Undef;             (* greek small letter theta *)
  Undef;             (* greek small letter iota *)
  Undef;             (* greek small letter kappa *)
  Undef;             (* greek small letter lamda *)
  Undef;             (* greek small letter mu *)
  Undef;             (* greek small letter nu *)
  Undef;             (* greek small letter xi *)
  Simple 0x6f;       (* greek small letter omicron *)
  Undef;             (* greek small letter pi *)
  Undef;             (* greek small letter rho *)
  Undef;             (* greek small letter final sigma *)
  Undef;             (* greek small letter sigma *)
  Undef;             (* greek small letter tau *)
  Undef;             (* greek small letter upsilon *)
  Undef;             (* greek small letter phi *)
  Undef;             (* greek small letter chi *)
  Undef;             (* greek small letter psi *)
  Undef;             (* greek small letter omega *)
  Undef;             (* greek small letter iota with dialytika *)
  Undef;             (* greek small letter upsilon with dialytika *)
  Undef;             (* greek small letter omicron with tonos *)
  Undef;             (* greek small letter upsilon with tonos *)
  Undef;             (* greek small letter omega with tonos *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_1e =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Accent 0x12 0x57;  (* latin capital letter w with grave *)
  Accent 0x12 0x77;  (* latin small letter w with grave *)
  Accent 0x13 0x57;  (* latin capital letter w with acute *)
  Accent 0x13 0x77;  (* latin small letter w with acute *)
  Accent 0x7f 0x57;  (* latin capital letter w with diaeresis *)
  Accent 0x7f 0x77;  (* latin small letter w with diaeresis *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Accent 0x12 0x59;  (* latin capital letter y with grave *)
  Accent 0x12 0x79;  (* latin small letter y with grave *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_20 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Simple 0x7b;        (* en dash *)
  Simple 0x7c;        (* em dash *)
  Undef;              (* horizontal bar *)
  Undef;
  Undef;              (* double low line *)
  Simple 0x60;        (* left single quotation mark *)
  Simple 0x27;        (* right single quotation mark *)
  Simple 0x2c;        (* single low-9 quotation mark single *)
  Undef;              (* high-reversed-9 quotation mark *)
  Simple 0x5c;        (* left double quotation mark *)
  Simple 0x22;        (* right double quotation mark *)
  Undef;             (* double low-9 quotation mark *)
  Undef;
  Undef;             (* dagger *)
  Undef;             (* double dagger *)
  Undef;             (* bullet *)
  Undef; Undef; Undef;
  Undef;             (* horizontal ellipsis *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* per mille sign *)
  Undef;
  Undef;             (* prime *)
  Undef;             (* double prime *)
  Undef; Undef; Undef; Undef; Undef;
  Undef;             (* single left-pointing angle quotation mark *)
  Undef;             (* single right-pointing angle quotation mark *)
  Undef;
  Undef;             (* double exclamation mark *)
  Undef;
  Undef;             (* overline *)
  Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* fraction slash *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* superscript latin small letter n *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* french franc sign *)
  Undef;             (* lira sign *)
  Undef; Undef;
  Undef;             (* peseta sign *)
  Undef; Undef; Undef; Undef;
  Undef;             (* euro currency symbol *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_21 =
[|
  Undef; Undef; Undef; Undef; Undef;
  Undef;             (* care of *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* script small l *)
  Undef; Undef;
  Undef;             (* numero sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* trademark sign *)
  Undef; Undef; Undef;
  Simple 0x0a;       (* ohm sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* estimated symbol *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* vulgar fraction one eighth *)
  Undef;             (* vulgar fraction three eighths *)
  Undef;             (* vulgar fraction five eighths *)
  Undef;             (* vulgar fraction seven eighths *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* leftwards arrow *)
  Undef;             (* upwards arrow *)
  Undef;             (* rightwards arrow *)
  Undef;             (* downwards arrow *)
  Undef;             (* left right arrow *)
  Undef;             (* up down arrow *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* up down arrow with base *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_22 =
[|
  Undef; Undef;
  Undef;             (* partial differential *)
  Undef; Undef; Undef;
  Simple 0x01;       (* increment *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x05;       (* n-ary product *)
  Undef;
  Simple 0x06;       (* n-ary summation *)
  Undef;             (* minus sign *)
  Undef; Undef;
  Undef;             (* division slash *)
  Undef; Undef; Undef;
  Undef;             (* bullet operator *)
  Undef;             (* square root *)
  Undef; Undef; Undef;
  Undef;             (* infinity *)
  Undef;             (* right angle *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* intersection *)
  Undef;
  Undef;             (* integral *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* almost equal to *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* not equal to *)
  Undef;             (* identical to *)
  Undef; Undef;
  Undef;             (* less-than or equal to *)
  Undef;             (* greater-than or equal to *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1_fb =
[|
  Simple 0x0b;       (* latin small ligature ff *)
  Simple 0x0c;       (* latin small ligature fi *)
  Simple 0x0d;       (* latin small ligature fl *)
  Simple 0x0e;       (* latin small ligature ffi *)
  Simple 0x0f;       (* latin small ligature ffl *)
  Undef;             (* latin small ligature long s t *)
  Undef;             (* latin small ligature st *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ot1 = Charmap.build
[|
  uc_to_ot1_00; uc_to_ot1_01; uc_to_ot1_02; uc_to_ot1_03;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  uc_to_ot1_1e; empty_table;
  uc_to_ot1_20; uc_to_ot1_21; uc_to_ot1_22; empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  uc_to_ot1_fb;
  empty_table;  empty_table;  empty_table;  empty_table
|];

(* T1 encoding *)

value uc_to_t1_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* space *)
  Simple 0x21;       (* exclamation mark *)
  Simple 0x22;       (* quotation mark *)
  Simple 0x23;       (* number sign *)
  Simple 0x24;       (* dollar sign *)
  Simple 0x25;       (* percent sign *)
  Simple 0x26;       (* ampersand *)
  Simple 0x27;       (* apostrophe *)
  Simple 0x28;       (* left parenthesis *)
  Simple 0x29;       (* right parenthesis *)
  Simple 0x2a;       (* asterisk *)
  Simple 0x2b;       (* plus sign *)
  Simple 0x2c;       (* comma *)
  Simple 0x2d;       (* hyphen-minus *)
  Simple 0x2e;       (* period *)
  Simple 0x2f;       (* slash *)
  Simple 0x30;       (* digit zero *)
  Simple 0x31;       (* digit one *)
  Simple 0x32;       (* digit two *)
  Simple 0x33;       (* digit three *)
  Simple 0x34;       (* digit four *)
  Simple 0x35;       (* digit five *)
  Simple 0x36;       (* digit six *)
  Simple 0x37;       (* digit seven *)
  Simple 0x38;       (* digit eight *)
  Simple 0x39;       (* digit nine *)
  Simple 0x3a;       (* colon *)
  Simple 0x3b;       (* semicolon *)
  Simple 0x3c;       (* less-than sign *)
  Simple 0x3d;       (* equals sign *)
  Simple 0x3e;       (* greater-than sign *)
  Simple 0x3f;       (* question mark *)
  Simple 0x40;       (* commercial at *)
  Simple 0x41;       (* latin capital letter a *)
  Simple 0x42;       (* latin capital letter b *)
  Simple 0x43;       (* latin capital letter c *)
  Simple 0x44;       (* latin capital letter d *)
  Simple 0x45;       (* latin capital letter e *)
  Simple 0x46;       (* latin capital letter f *)
  Simple 0x47;       (* latin capital letter g *)
  Simple 0x48;       (* latin capital letter h *)
  Simple 0x49;       (* latin capital letter i *)
  Simple 0x4a;       (* latin capital letter j *)
  Simple 0x4b;       (* latin capital letter k *)
  Simple 0x4c;       (* latin capital letter l *)
  Simple 0x4d;       (* latin capital letter m *)
  Simple 0x4e;       (* latin capital letter n *)
  Simple 0x4f;       (* latin capital letter o *)
  Simple 0x50;       (* latin capital letter p *)
  Simple 0x51;       (* latin capital letter q *)
  Simple 0x52;       (* latin capital letter r *)
  Simple 0x53;       (* latin capital letter s *)
  Simple 0x54;       (* latin capital letter t *)
  Simple 0x55;       (* latin capital letter u *)
  Simple 0x56;       (* latin capital letter v *)
  Simple 0x57;       (* latin capital letter w *)
  Simple 0x58;       (* latin capital letter x *)
  Simple 0x59;       (* latin capital letter y *)
  Simple 0x5a;       (* latin capital letter z *)
  Simple 0x5b;       (* left square bracket *)
  Simple 0x5c;       (* backslash *)
  Simple 0x5d;       (* right square bracket *)
  Simple 0x5e;       (* circumflex accent *)
  Simple 0x5f;       (* underline *)
  Simple 0x00;       (* grave accent *)
  Simple 0x61;       (* latin small letter a *)
  Simple 0x62;       (* latin small letter b *)
  Simple 0x63;       (* latin small letter c *)
  Simple 0x64;       (* latin small letter d *)
  Simple 0x65;       (* latin small letter e *)
  Simple 0x66;       (* latin small letter f *)
  Simple 0x67;       (* latin small letter g *)
  Simple 0x68;       (* latin small letter h *)
  Simple 0x69;       (* latin small letter i *)
  Simple 0x6a;       (* latin small letter j *)
  Simple 0x6b;       (* latin small letter k *)
  Simple 0x6c;       (* latin small letter l *)
  Simple 0x6d;       (* latin small letter m *)
  Simple 0x6e;       (* latin small letter n *)
  Simple 0x6f;       (* latin small letter o *)
  Simple 0x70;       (* latin small letter p *)
  Simple 0x71;       (* latin small letter q *)
  Simple 0x72;       (* latin small letter r *)
  Simple 0x73;       (* latin small letter s *)
  Simple 0x74;       (* latin small letter t *)
  Simple 0x75;       (* latin small letter u *)
  Simple 0x76;       (* latin small letter v *)
  Simple 0x77;       (* latin small letter w *)
  Simple 0x78;       (* latin small letter x *)
  Simple 0x79;       (* latin small letter y *)
  Simple 0x7a;       (* latin small letter z *)
  Simple 0x7b;       (* left curly bracket *)
  Simple 0x7c;       (* vertical line *)
  Simple 0x7d;       (* right curly bracket *)
  Simple 0x7e;       (* tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* no-break space *)
  Simple 0xbd;       (* inverted exclamation mark *)
  Undef;             (* cent sign *)
  Simple 0xbf;       (* pound sign *)
  Undef;             (* currency sign *)
  Undef;             (* yen sign *)
  Undef;             (* broken bar *)
  Simple 0x9f;       (* section sign *)
  Simple 0x04;       (* diaeresis *)
  Undef;             (* copyright sign *)
  Undef;             (* feminine ordinal indicator *)
  Simple 0x13;       (* left guillemet *)
  Undef;             (* not sign *)
  Undef;             (* soft hyphen *)
  Undef;             (* registered trade mark sign *)
  Simple 0x09;       (* macron, overline *)
  Simple 0x06;       (* degree sign *)
  Undef;             (* plus-minus sign *)
  Undef;             (* superscript two *)
  Undef;             (* superscript three *)
  Simple 0x01;       (* acute accent *)
  Undef;             (* micro sign *)
  Undef;             (* paragraph sign *)
  Undef;             (* middle dot, kana conjoctive *)
  Simple 0x0b;       (* cedilla *)
  Undef;             (* superscript one *)
  Undef;             (* masculine ordinal indicator *)
  Simple 0x14;       (* right guillemet *)
  Undef;             (* vulgar fraction one quarter *)
  Undef;             (* vulgar fraction one half *)
  Undef;             (* vulgar fraction three quarters *)
  Simple 0xbe;       (* inverted question mark *)
  Simple 0xc0;       (* latin capital letter a with grave accent *)
  Simple 0xc1;       (* latin capital letter a with acute accent *)
  Simple 0xc2;       (* latin capital letter a with circumflex accent *)
  Simple 0xc3;       (* latin capital letter a with tilde *)
  Simple 0xc4;       (* latin capital letter a with diaeresis *)
  Simple 0xc5;       (* latin capital letter a with ring above *)
  Simple 0xc6;       (* latin capital letter a with e *)
  Simple 0xc7;       (* latin capital letter c with cedilla *)
  Simple 0xc8;       (* latin capital letter e with grave accent *)
  Simple 0xc9;       (* latin capital letter e with acute accent *)
  Simple 0xca;       (* latin capital letter e with circumflex accent *)
  Simple 0xcb;       (* latin capital letter e with diaeresis *)
  Simple 0xcc;       (* latin capital letter i with grave accent *)
  Simple 0xcd;       (* latin capital letter i with acute accent *)
  Simple 0xce;       (* latin capital letter i with circumflex accent *)
  Simple 0xcf;       (* latin capital letter i with diaeresis *)
  Simple 0xd0;       (* latin capital letter eth *)
  Simple 0xd1;       (* latin capital letter n with tilde *)
  Simple 0xd2;       (* latin capital letter o with grave accent *)
  Simple 0xd3;       (* latin capital letter o with acute accent *)
  Simple 0xd4;       (* latin capital letter o with circumflex accent *)
  Simple 0xd5;       (* latin capital letter o with tilde *)
  Simple 0xd6;       (* latin capital letter o with diaeresis *)
  Undef;             (* multiplication sign *)
  Simple 0xd8;       (* latin capital letter o with oblique stroke *)
  Simple 0xd9;       (* latin capital letter u with grave accent *)
  Simple 0xda;       (* latin capital letter u with acute accent *)
  Simple 0xdb;       (* latin capital letter u with circumflex accent *)
  Simple 0xdc;       (* latin capital letter u with diaeresis *)
  Simple 0xdd;       (* latin capital letter y with acute accent *)
  Simple 0xde;       (* latin capital letter thorn *)
  Simple 0xff;       (* latin small letter sharp s *)
  Simple 0xe0;       (* latin small letter a with grave accent *)
  Simple 0xe1;       (* latin small letter a with acute accent *)
  Simple 0xe2;       (* latin small letter a with circumflex accent *)
  Simple 0xe3;       (* latin small letter a with tilde *)
  Simple 0xe4;       (* latin small letter a with diaeresis *)
  Simple 0xe5;       (* latin small letter a with ring above *)
  Simple 0xe6;       (* latin small letter a with e *)
  Simple 0xe7;       (* latin small letter c with cedilla *)
  Simple 0xe8;       (* latin small letter e with grave accent *)
  Simple 0xe9;       (* latin small letter e with acute accent *)
  Simple 0xea;       (* latin small letter e with circumflex accent *)
  Simple 0xeb;       (* latin small letter e with diaeresis *)
  Simple 0xec;       (* latin small letter i with grave accent *)
  Simple 0xed;       (* latin small letter i with acute accent *)
  Simple 0xee;       (* latin small letter i with circumflex accent *)
  Simple 0xef;       (* latin small letter i with diaeresis *)
  Simple 0xf0;       (* latin small letter eth *)
  Simple 0xf1;       (* latin small letter n with tilde *)
  Simple 0xf2;       (* latin small letter o with grave accent *)
  Simple 0xf3;       (* latin small letter o with acute accent *)
  Simple 0xf4;       (* latin small letter o with circumflex accent *)
  Simple 0xf5;       (* latin small letter o with tilde *)
  Simple 0xf6;       (* latin small letter o with diaeresis *)
  Undef;             (* division sign *)
  Simple 0xf8;       (* latin small letter o with oblique stroke *)
  Simple 0xf9;       (* latin small letter u with grave accent *)
  Simple 0xfa;       (* latin small letter u with acute accent *)
  Simple 0xfb;       (* latin small letter u with circumflex accent *)
  Simple 0xfc;       (* latin small letter u with diaeresis *)
  Simple 0xfd;       (* latin small letter y with acute accent *)
  Simple 0xfe;       (* latin small letter thorn *)
  Simple 0xb8        (* latin small letter y with diaeresis *)
|];

value uc_to_t1_01 =
[|
  Accent 0x09 0x41;  (* latin capital letter a with macron *)
  Accent 0x09 0x61;  (* latin small letter a with macron *)
  Simple 0x80;       (* latin capital letter a with breve *)
  Simple 0xa0;       (* latin small letter a with breve *)
  Simple 0x81;       (* latin capital letter a with ogonek *)
  Simple 0xa1;       (* latin small letter a with ogonek *)
  Simple 0x82;       (* latin capital letter c with acute accent *)
  Simple 0xa2;       (* latin small letter c with acute accent *)
  Accent 0x02 0x43;  (* latin capital letter c with circumflex *)
  Accent 0x02 0x63;  (* latin small letter c with circumflex *)
  Accent 0x0a 0x43;  (* latin capital letter c with dot above *)
  Accent 0x0a 0x63;  (* latin small letter c with dot above *)
  Simple 0x83;       (* latin capital letter c with caron *)
  Simple 0xa3;       (* latin small letter c with caron *)
  Simple 0x84;       (* latin capital letter d with hacek *)
  Simple 0xa4;       (* latin small letter d with hacek *)
  Undef;             (* latin capital letter d with stroke *)
  Undef;             (* latin small letter d with stroke *)
  Accent 0x09 0x45;  (* latin capital letter e with macron *)
  Accent 0x09 0x65;  (* latin small letter e with macron *)
  Accent 0x08 0x45;  (* latin capital letter e with breve *)
  Accent 0x08 0x65;  (* latin small letter e with breve *)
  Accent 0x0a 0x45;  (* latin capital letter e with dot above *)
  Accent 0x0a 0x65;  (* latin small letter e with dot above *)
  Simple 0x86;       (* latin capital letter e with ogonek *)
  Simple 0xa6;       (* latin small letter e with ogonek *)
  Simple 0x85;       (* latin capital letter e with hacek *)
  Simple 0xa5;       (* latin small letter e with hacek *)
  Accent 0x02 0x47;  (* latin capital letter g with circumflex *)
  Accent 0x02 0x67;  (* latin small letter g with circumflex *)
  Simple 0x87;       (* latin capital letter g with breve *)
  Simple 0xa7;       (* latin small letter g with breve *)
  Accent 0x0a 0x47;  (* latin capital letter g with dot above *)
  Accent 0x0a 0x67;  (* latin small letter g with dot above *)
  Accent 0x0b 0x47;  (* latin capital letter g with cedilla *)
  Undef;             (* latin small letter g with cedilla *)
  Accent 0x02 0x48;  (* latin capital letter h with circumflex *)
  Accent 0x02 0x68;  (* latin small letter h with circumflex *)
  Undef;             (* latin capital letter h with stroke *)
  Undef;             (* latin small letter h with stroke *)
  Accent 0x03 0x49;  (* latin capital letter i with tilde *)
  Accent 0x03 0x10;  (* latin small letter i with tilde *)
  Accent 0x09 0x49;  (* latin capital letter i with macron *)
  Accent 0x09 0x10;  (* latin small letter i with macron *)
  Accent 0x08 0x49;  (* latin capital letter i with breve *)
  Accent 0x08 0x10;  (* latin small letter i with breve *)
  Undef;             (* latin capital letter i with ogonek *)
  Undef;             (* latin small letter i with ogonek *)
  Accent 0x0a 0x49;  (* latin capital letter i with dot above *)
  Accent 0x0a 0x10;  (* latin small letter i without dot above *)
  Simple 0x9c;       (* latin capital ligature ij *)
  Simple 0xbc;       (* latin small ligature ij *)
  Accent 0x02 0x4a;  (* latin capital letter j with circumflex *)
  Accent 0x02 0x11;  (* latin small letter j with circumflex *)
  Accent 0x0b 0x4b;  (* latin capital letter k with cedilla *)
  Accent 0x0b 0x6b;  (* latin small letter k with cedilla *)
  Undef;             (* latin small letter kra *)
  Simple 0x88;       (* latin capital letter l with acute accent *)
  Simple 0xa8;       (* latin small letter l with acute accent *)
  Accent 0x0b 0x4c;  (* latin capital letter l with cedilla *)
  Accent 0x0b 0x6c;  (* latin small letter l with cedilla *)
  Simple 0x89;       (* latin capital letter l with hacek *)
  Simple 0x89;       (* latin small letter l with hacek *)
  Accent 0x0a 0x4c;  (* latin capital letter l with middle dot *)
  Accent 0x0a 0x6c;  (* latin small letter l with middle dot *)
  Simple 0x8a;       (* latin capital letter l with stroke *)
  Simple 0xaa;       (* latin small letter l with stroke *)
  Simple 0x8b;       (* latin capital letter n with acute accent *)
  Simple 0xab;       (* latin small letter n with acute accent *)
  Accent 0x0b 0x4e;  (* latin capital letter n with cedilla *)
  Accent 0x0b 0x6e;  (* latin small letter n with cedilla *)
  Simple 0x8c;       (* latin capital letter n with hacek *)
  Simple 0xac;       (* latin small letter n with hacek *)
  Undef; (* FIX *)   (* latin small letter n preceded by apostrophe *)
  Simple 0x8d;       (* latin capital letter eng *)
  Simple 0x8d;       (* latin small letter eng *)
  Accent 0x09 0x4f;  (* latin capital letter o with macron *)
  Accent 0x09 0x6f;  (* latin small letter o with macron *)
  Accent 0x08 0x4f;  (* latin capital letter o with breve *)
  Accent 0x08 0x6f;  (* latin small letter o with breve *)
  Simple 0x8e;       (* latin capital letter o with double acute accent *)
  Simple 0xae;       (* latin small letter o with double acute accent *)
  Simple 0x1e;       (* latin capital ligature o with e *)
  Simple 0x1b;       (* latin small ligature o with e *)
  Simple 0x8f;       (* latin capital letter r with acute accent *)
  Simple 0xaf;       (* latin small letter r with acute accent *)
  Accent 0x0b 0x52;  (* latin capital letter r with cedilla *)
  Accent 0x0b 0x72;  (* latin small letter r with cedilla *)
  Simple 0x90;       (* latin capital letter r with hacek *)
  Simple 0xb0;       (* latin small letter r with hacek *)
  Simple 0x91;       (* latin capital letter s with acute accent *)
  Simple 0xb1;       (* latin small letter s with acute accent *)
  Accent 0x02 0x53;  (* latin capital letter s with circumflex *)
  Accent 0x02 0x73;  (* latin small letter s with circumflex *)
  Simple 0x93;       (* latin capital letter s with cedilla *)
  Simple 0xb3;       (* latin small letter s with cedilla *)
  Simple 0x92;       (* latin capital letter s with hacek *)
  Simple 0xb2;       (* latin small letter s with hacek *)
  Simple 0x95;       (* latin capital letter t with cedilla *)
  Simple 0xb5;       (* latin small letter t with cedilla *)
  Simple 0x94;       (* latin capital letter t with hacek *)
  Simple 0xb4;       (* latin small letter t with hacek *)
  Undef;             (* latin capital letter t with stroke *)
  Undef;             (* latin small letter t with stroke *)
  Accent 0x03 0x55;  (* latin capital letter u with tilde *)
  Accent 0x03 0x75;  (* latin small letter u with tilde *)
  Accent 0x09 0x55;  (* latin capital letter u with macron *)
  Accent 0x09 0x75;  (* latin small letter u with macron *)
  Accent 0x08 0x55;  (* latin capital letter u with breve *)
  Accent 0x08 0x75;  (* latin small letter u with breve *)
  Simple 0x97;       (* latin capital letter u with ring above *)
  Simple 0xb7;       (* latin small letter u with ring above *)
  Simple 0x96;       (* latin capital letter u with double acute accent *)
  Simple 0xb6;       (* latin small letter u with double acute accent *)
  Undef;             (* latin capital letter u with ogonek *)
  Undef;             (* latin small letter u with ogonek *)
  Accent 0x02 0x57;  (* latin capital letter w with circumflex *)
  Accent 0x02 0x77;  (* latin cmall letter w with circumflex *)
  Accent 0x02 0x59;  (* latin capital letter y with circumflex *)
  Accent 0x02 0x79;  (* latin small letter y with circumflex *)
  Simple 0x98;       (* latin capital letter y with diaeresis *)
  Simple 0x99;       (* latin capital letter z with acute accent *)
  Simple 0xb9;       (* latin small letter z with acute accent *)
  Simple 0x9b;       (* latin capital letter z with dot above *)
  Simple 0x9b;       (* latin small letter z with dot above *)
  Simple 0x9a;       (* latin capital letter z with hacek *)
  Simple 0xba;       (* latin small letter z with hacek *)
  Undef;             (* latin small letter long s *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin small letter script f, florin sign *)
  Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin capital letter a with ring above and acute *)
  Undef;             (* latin small letter a with ring above and acute *)
  Accent 0x01 0x1d;  (* latin capital ligature ae with acute *)
  Accent 0x01 0x1a;  (* latin small ligature ae with acute *)
  Accent 0x01 0x1f;  (* latin capital letter o with stroke and acute *)
  Accent 0x01 0x1c   (* latin small letter o with stroke and acute *)
|];

value uc_to_t1_02 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x1a;       (* latin small letter dotless j *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x02;       (* nonspacing circumflex accent *)
  Simple 0x07;       (* modifier letter hacek *)
  Undef;
  Simple 0x09;       (* modifier letter macron *)
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x08;       (* breve *)
  Simple 0x0a;       (* dot above *)
  Simple 0x06;       (* ring above *)
  Simple 0x0c;       (* ogonek *)
  Simple 0x03;       (* nonspacing tilde *)
  Simple 0x22;       (* modifier letter double prime *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_t1_1e =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Accent 0x00 0x57;  (* latin capital letter w with grave *)
  Accent 0x00 0x77;  (* latin small letter w with grave *)
  Accent 0x01 0x57;  (* latin capital letter w with acute *)
  Accent 0x01 0x77;  (* latin small letter w with acute *)
  Accent 0x04 0x57;  (* latin capital letter w with diaeresis *)
  Accent 0x04 0x77;  (* latin small letter w with diaeresis *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Accent 0x00 0x59;  (* latin capital letter y with grave *)
  Accent 0x00 0x79;  (* latin small letter y with grave *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_t1_20 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Simple 0x15;       (* en dash *)
  Simple 0x16;       (* em dash *)
  Undef;             (* horizontal bar *)
  Undef;
  Undef;             (* double low line *)
  Simple 0x60;       (* left single quotation mark *)
  Simple 0x27;       (* right single quotation mark *)
  Simple 0x0d;       (* single low-9 quotation mark single *)
  Undef;             (* high-reversed-9 quotation mark *)
  Simple 0x10;       (* left double quotation mark *)
  Simple 0x11;       (* right double quotation mark *)
  Simple 0x12;       (* double low-9 quotation mark *)
  Undef;
  Undef;             (* dagger *)
  Undef;             (* double dagger *)
  Undef;             (* bullet *)
  Undef; Undef; Undef;
  Undef;             (* horizontal ellipsis *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* per mille sign *)
  Undef;
  Undef;             (* prime *)
  Undef;             (* double prime *)
  Undef; Undef; Undef; Undef; Undef;
  Simple 0x0e;       (* single left-pointing angle quotation mark *)
  Simple 0x0f;       (* single right-pointing angle quotation mark *)
  Undef;
  Undef;             (* double exclamation mark *)
  Undef;
  Undef;             (* overline *)
  Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* fraction slash *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* superscript latin small letter n *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* french franc sign *)
  Undef;             (* lira sign *)
  Undef; Undef;
  Undef;             (* peseta sign *)
  Undef; Undef; Undef; Undef;
  Undef;             (* euro currency symbol *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_t1_fb =
[|
  Simple 0x0b;       (* latin small ligature ff *)
  Simple 0x0c;       (* latin small ligature fi *)
  Simple 0x0d;       (* latin small ligature fl *)
  Simple 0x0e;       (* latin small ligature ffi *)
  Simple 0x0f;       (* latin small ligature ffl *)
  Undef;             (* latin small ligature long s t *)
  Undef;             (* latin small ligature st *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_t1 = Charmap.build
[|
  uc_to_t1_00; uc_to_t1_01; uc_to_t1_02; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; uc_to_t1_1e; empty_table;
  uc_to_t1_20; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; uc_to_t1_fb;
  empty_table; empty_table; empty_table; empty_table
|];

(* OTT encoding *)

value uc_to_ott_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x20;       (* space *)
  Simple 0x21;       (* exclamation mark *)
  Simple 0x22;       (* quotation mark *)
  Simple 0x23;       (* number sign *)
  Simple 0x24;       (* dollar sign *)
  Simple 0x25;       (* percent sign *)
  Simple 0x26;       (* ampersand *)
  Simple 0x27;       (* apostrophe *)
  Simple 0x28;       (* left parenthesis *)
  Simple 0x29;       (* right parenthesis *)
  Simple 0x2a;       (* asterisk *)
  Simple 0x2b;       (* plus sign *)
  Simple 0x2c;       (* comma *)
  Simple 0x2d;       (* hyphen-minus *)
  Simple 0x2e;       (* period *)
  Simple 0x2f;       (* slash *)
  Simple 0x30;       (* digit zero *)
  Simple 0x31;       (* digit one *)
  Simple 0x32;       (* digit two *)
  Simple 0x33;       (* digit three *)
  Simple 0x34;       (* digit four *)
  Simple 0x35;       (* digit five *)
  Simple 0x36;       (* digit six *)
  Simple 0x37;       (* digit seven *)
  Simple 0x38;       (* digit eight *)
  Simple 0x39;       (* digit nine *)
  Simple 0x3a;       (* colon *)
  Simple 0x3b;       (* semicolon *)
  Simple 0x3c;       (* less-than sign *)
  Simple 0x3d;       (* equals sign *)
  Simple 0x3e;       (* greater-than sign *)
  Simple 0x3f;       (* question mark *)
  Simple 0x40;       (* commercial at *)
  Simple 0x41;       (* latin capital letter a *)
  Simple 0x42;       (* latin capital letter b *)
  Simple 0x43;       (* latin capital letter c *)
  Simple 0x44;       (* latin capital letter d *)
  Simple 0x45;       (* latin capital letter e *)
  Simple 0x46;       (* latin capital letter f *)
  Simple 0x47;       (* latin capital letter g *)
  Simple 0x48;       (* latin capital letter h *)
  Simple 0x49;       (* latin capital letter i *)
  Simple 0x4a;       (* latin capital letter j *)
  Simple 0x4b;       (* latin capital letter k *)
  Simple 0x4c;       (* latin capital letter l *)
  Simple 0x4d;       (* latin capital letter m *)
  Simple 0x4e;       (* latin capital letter n *)
  Simple 0x4f;       (* latin capital letter o *)
  Simple 0x50;       (* latin capital letter p *)
  Simple 0x51;       (* latin capital letter q *)
  Simple 0x52;       (* latin capital letter r *)
  Simple 0x53;       (* latin capital letter s *)
  Simple 0x54;       (* latin capital letter t *)
  Simple 0x55;       (* latin capital letter u *)
  Simple 0x56;       (* latin capital letter v *)
  Simple 0x57;       (* latin capital letter w *)
  Simple 0x58;       (* latin capital letter x *)
  Simple 0x59;       (* latin capital letter y *)
  Simple 0x5a;       (* latin capital letter z *)
  Simple 0x5b;       (* left square bracket *)
  Simple 0x5c;       (* backslash *)
  Simple 0x5d;       (* right square bracket *)
  Simple 0x5e;       (* circumflex accent *)
  Simple 0x5f;       (* underline *)
  Simple 0x12;       (* grave accent *)
  Simple 0x61;       (* latin small letter a *)
  Simple 0x62;       (* latin small letter b *)
  Simple 0x63;       (* latin small letter c *)
  Simple 0x64;       (* latin small letter d *)
  Simple 0x65;       (* latin small letter e *)
  Simple 0x66;       (* latin small letter f *)
  Simple 0x67;       (* latin small letter g *)
  Simple 0x68;       (* latin small letter h *)
  Simple 0x69;       (* latin small letter i *)
  Simple 0x6a;       (* latin small letter j *)
  Simple 0x6b;       (* latin small letter k *)
  Simple 0x6c;       (* latin small letter l *)
  Simple 0x6d;       (* latin small letter m *)
  Simple 0x6e;       (* latin small letter n *)
  Simple 0x6f;       (* latin small letter o *)
  Simple 0x70;       (* latin small letter p *)
  Simple 0x71;       (* latin small letter q *)
  Simple 0x72;       (* latin small letter r *)
  Simple 0x73;       (* latin small letter s *)
  Simple 0x74;       (* latin small letter t *)
  Simple 0x75;       (* latin small letter u *)
  Simple 0x76;       (* latin small letter v *)
  Simple 0x77;       (* latin small letter w *)
  Simple 0x78;       (* latin small letter x *)
  Simple 0x79;       (* latin small letter y *)
  Simple 0x7a;       (* latin small letter z *)
  Simple 0x7b;       (* left curly bracket *)
  Simple 0x7c;       (* vertical line *)
  Simple 0x7d;       (* right curly bracket *)
  Simple 0x7e;       (* tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* no-break space *)
  Simple 0x0e;       (* inverted exclamation mark *)
  Undef;             (* cent sign *)
  Undef;             (* pound sign *)
  Undef;             (* currency sign *)
  Undef;             (* yen sign *)
  Undef;             (* broken bar *)
  Undef;             (* section sign *)
  Simple 0x7f;       (* diaeresis *)
  Undef;             (* copyright sign *)
  Undef;             (* feminine ordinal indicator *)
  Undef;             (* left guillemet *)
  Undef;             (* not sign *)
  Undef;             (* soft hyphen *)
  Undef;             (* registered trade mark sign *)
  Simple 0x16;       (* macron, overline *)
  Simple 0x17;       (* degree sign *)
  Undef;             (* plus-minus sign *)
  Undef;             (* superscript two *)
  Undef;             (* superscript three *)
  Simple 0x13;       (* acute accent *)
  Undef;             (* micro sign *)
  Undef;             (* paragraph sign *)
  Undef;             (* middle dot, kana conjoctive *)
  Simple 0x18;       (* cedilla *)
  Undef;             (* superscript one *)
  Undef;             (* masculine ordinal indicator *)
  Undef;             (* right guillemet *)
  Undef;             (* vulgar fraction one quarter *)
  Undef;             (* vulgar fraction one half *)
  Undef;             (* vulgar fraction three quarters *)
  Simple 0x0f;       (* inverted question mark *)
  Accent 0x12 0x41;  (* latin capital letter a with grave accent *)
  Accent 0x13 0x41;  (* latin capital letter a with acute accent *)
  Accent 0x5e 0x41;  (* latin capital letter a with circumflex accent *)
  Accent 0x7e 0x41;  (* latin capital letter a with tilde *)
  Accent 0x7f 0x41;  (* latin capital letter a with diaeresis *)
  Accent 0x12 0x41;  (* latin capital letter a with ring above *)
  Simple 0x1e;       (* latin capital letter a with e *)
  Accent 0x18 0x43;  (* latin capital letter c with cedilla *)
  Accent 0x12 0x45;  (* latin capital letter e with grave accent *)
  Accent 0x13 0x45;  (* latin capital letter e with acute accent *)
  Accent 0x5e 0x45;  (* latin capital letter e with circumflex accent *)
  Accent 0x7f 0x45;  (* latin capital letter e with diaeresis *)
  Accent 0x12 0x49;  (* latin capital letter i with grave accent *)
  Accent 0x13 0x49;  (* latin capital letter i with acute accent *)
  Accent 0x5e 0x49;  (* latin capital letter i with circumflex accent *)
  Accent 0x7f 0x49;  (* latin capital letter i with diaeresis *)
  Undef;             (* latin capital letter eth *)
  Accent 0x7e 0x4e;  (* latin capital letter n with tilde *)
  Accent 0x12 0x4f;  (* latin capital letter o with grave accent *)
  Accent 0x13 0x4f;  (* latin capital letter o with acute accent *)
  Accent 0x5e 0x4f;  (* latin capital letter o with circumflex accent *)
  Accent 0x7e 0x4f;  (* latin capital letter o with tilde *)
  Accent 0x7f 0x4f;  (* latin capital letter o with diaeresis *)
  Undef;             (* multiplication sign *)
  Simple 0x1f;       (* latin capital letter o with oblique stroke *)
  Accent 0x12 0x55;  (* latin capital letter u with grave accent *)
  Accent 0x13 0x55;  (* latin capital letter u with acute accent *)
  Accent 0x5e 0x55;  (* latin capital letter u with circumflex accent *)
  Accent 0x7f 0x55;  (* latin capital letter u with diaeresis *)
  Accent 0x7f 0x59;  (* latin capital letter y with acute accent *)
  Undef;             (* latin capital letter thorn *)
  Simple 0x19;       (* latin small letter sharp s *)
  Accent 0x12 0x61;  (* latin small letter a with grave accent *)
  Accent 0x13 0x61;  (* latin small letter a with acute accent *)
  Accent 0x5e 0x61;  (* latin small letter a with circumflex accent *)
  Accent 0x7e 0x61;  (* latin small letter a with tilde *)
  Accent 0x7f 0x61;  (* latin small letter a with diaeresis *)
  Accent 0x17 0x61;  (* latin small letter a with ring above *)
  Simple 0x1a;       (* latin small letter a with e *)
  Accent 0x18 0x63;  (* latin small letter c with cedilla *)
  Accent 0x12 0x65;  (* latin small letter e with grave accent *)
  Accent 0x13 0x65;  (* latin small letter e with acute accent *)
  Accent 0x5e 0x65;  (* latin small letter e with circumflex accent *)
  Accent 0x7f 0x65;  (* latin small letter e with diaeresis *)
  Accent 0x12 0x10;  (* latin small letter i with grave accent *)
  Accent 0x13 0x10;  (* latin small letter i with acute accent *)
  Accent 0x5e 0x10;  (* latin small letter i with circumflex accent *)
  Accent 0x7f 0x10;  (* latin small letter i with diaeresis *)
  Undef;             (* latin small letter eth *)
  Accent 0x7e 0x6e;  (* latin small letter n with tilde *)
  Accent 0x12 0x6f;  (* latin small letter o with grave accent *)
  Accent 0x13 0x6f;  (* latin small letter o with acute accent *)
  Accent 0x5e 0x6f;  (* latin small letter o with circumflex accent *)
  Accent 0x7e 0x6f;  (* latin small letter o with tilde *)
  Accent 0x7f 0x6f;  (* latin small letter o with diaeresis *)
  Undef;             (* division sign *)
  Simple 0x1c;       (* latin small letter o with oblique stroke *)
  Accent 0x12 0x75;  (* latin small letter u with grave accent *)
  Accent 0x13 0x75;  (* latin small letter u with acute accent *)
  Accent 0x5e 0x75;  (* latin small letter u with circumflex accent *)
  Accent 0x7f 0x75;  (* latin small letter u with diaeresis *)
  Accent 0x13 0x79;  (* latin small letter y with acute accent *)
  Undef;             (* latin small letter thorn *)
  Accent 0x7f 0x79   (* latin small letter y with diaeresis *)
|];

value uc_to_ott_01 =
[|
  Accent 0x16 0x41;  (* latin capital letter a with macron *)
  Accent 0x16 0x61;  (* latin small letter a with macron *)
  Accent 0x15 0x41;  (* latin capital letter a with breve *)
  Accent 0x15 0x61;  (* latin small letter a with breve *)
  Undef;             (* latin capital letter a with ogonek *)
  Undef;             (* latin small letter a with ogonek *)
  Accent 0x13 0x43;  (* latin capital letter c with acute accent *)
  Accent 0x13 0x63;  (* latin small letter c with acute accent *)
  Accent 0x5e 0x43;  (* latin capital letter c with circumflex *)
  Accent 0x5e 0x63;  (* latin small letter c with circumflex *)
  Accent 0x5f 0x43;  (* latin capital letter c with dot above *)
  Accent 0x5f 0x63;  (* latin small letter c with dot above *)
  Accent 0x14 0x43;  (* latin capital letter c with caron *)
  Accent 0x14 0x63;  (* latin small letter c with caron *)
  Accent 0x14 0x44;  (* latin capital letter d with hacek *)
  Accent 0x14 0x64;  (* latin small letter d with hacek *)
  Undef;             (* latin capital letter d with stroke *)
  Undef;             (* latin small letter d with stroke *)
  Accent 0x16 0x45;  (* latin capital letter e with macron *)
  Accent 0x16 0x65;  (* latin small letter e with macron *)
  Accent 0x15 0x45;  (* latin capital letter e with breve *)
  Accent 0x15 0x65;  (* latin small letter e with breve *)
  Accent 0x5f 0x45;  (* latin capital letter e with dot above *)
  Accent 0x5f 0x65;  (* latin small letter e with dot above *)
  Undef;             (* latin capital letter e with ogonek *)
  Undef;             (* latin small letter e with ogonek *)
  Accent 0x14 0x45;  (* latin capital letter e with hacek *)
  Accent 0x14 0x65;  (* latin small letter e with hacek *)
  Accent 0x5e 0x47;  (* latin capital letter g with circumflex *)
  Accent 0x5e 0x67;  (* latin small letter g with circumflex *)
  Accent 0x15 0x47;  (* latin capital letter g with breve *)
  Accent 0x15 0x67;  (* latin small letter g with breve *)
  Accent 0x5f 0x47;  (* latin capital letter g with dot above *)
  Accent 0x5f 0x67;  (* latin small letter g with dot above *)
  Accent 0x18 0x47;  (* latin capital letter g with cedilla *)
  Undef;             (* latin small letter g with cedilla *)
  Accent 0x5e 0x48;  (* latin capital letter h with circumflex *)
  Accent 0x5e 0x68;  (* latin small letter h with circumflex *)
  Undef;             (* latin capital letter h with stroke *)
  Undef;             (* latin small letter h with stroke *)
  Accent 0x7e 0x49;  (* latin capital letter i with tilde *)
  Accent 0x7e 0x10;  (* latin small letter i with tilde *)
  Accent 0x16 0x49;  (* latin capital letter i with macron *)
  Accent 0x16 0x10;  (* latin small letter i with macron *)
  Accent 0x15 0x49;  (* latin capital letter i with breve *)
  Accent 0x15 0x10;  (* latin small letter i with breve *)
  Undef;             (* latin capital letter i with ogonek *)
  Undef;             (* latin small letter i with ogonek *)
  Accent 0x5f 0x49;  (* latin capital letter i with dot above *)
  Accent 0x5f 0x10;  (* latin small letter i without dot above *)
  Undef;             (* latin capital ligature ij *)
  Undef;             (* latin small ligature ij *)
  Accent 0x5e 0x4a;  (* latin capital letter j with circumflex *)
  Accent 0x5e 0x11;  (* latin small letter j with circumflex *)
  Accent 0x18 0x4b;  (* latin capital letter k with cedilla *)
  Accent 0x18 0x6b;  (* latin small letter k with cedilla *)
  Undef;             (* latin small letter kra *)
  Accent 0x13 0x4c;  (* latin capital letter l with acute accent *)
  Accent 0x13 0x6c;  (* latin small letter l with acute accent *)
  Accent 0x18 0x4c;  (* latin capital letter l with cedilla *)
  Accent 0x18 0x6c;  (* latin small letter l with cedilla *)
  Accent 0x14 0x4c;  (* latin capital letter l with hacek *)
  Accent 0x14 0x6c;  (* latin small letter l with hacek *)
  Accent 0x5f 0x4c;  (* latin capital letter l with middle dot *)
  Accent 0x5f 0x6c;  (* latin small letter l with middle dot *)
  Accent 0x20 0x4c;  (* latin capital letter l with stroke *)
  Accent 0x20 0x6c;  (* latin small letter l with stroke *)
  Accent 0x13 0x4e;  (* latin capital letter n with acute accent *)
  Accent 0x13 0x6e;  (* latin small letter n with acute accent *)
  Accent 0x18 0x4e;  (* latin capital letter n with cedilla *)
  Accent 0x18 0x6e;  (* latin small letter n with cedilla *)
  Accent 0x14 0x4e;  (* latin capital letter n with hacek *)
  Accent 0x14 0x6e;  (* latin small letter n with hacek *)
  Undef;             (* latin small letter n preceded by apostrophe *)
  Undef;             (* latin capital letter eng *)
  Undef;             (* latin small letter eng *)
  Accent 0x16 0x4f;  (* latin capital letter o with macron *)
  Accent 0x16 0x6f;  (* latin small letter o with macron *)
  Accent 0x15 0x4f;  (* latin capital letter o with breve *)
  Accent 0x15 0x6f;  (* latin small letter o with breve *)
  Accent 0x13 0x4f;  (* latin capital letter o with double acute accent *)
  Accent 0x13 0x6f;  (* latin small letter o with double acute accent *)
  Simple 0x1e;       (* latin capital ligature o with e *)
  Simple 0x1b;       (* latin small ligature o with e *)
  Accent 0x13 0x52;  (* latin capital letter r with acute accent *)
  Accent 0x13 0x72;  (* latin small letter r with acute accent *)
  Accent 0x18 0x52;  (* latin capital letter r with cedilla *)
  Accent 0x18 0x72;  (* latin small letter r with cedilla *)
  Accent 0x14 0x52;  (* latin capital letter r with hacek *)
  Accent 0x14 0x72;  (* latin small letter r with hacek *)
  Accent 0x13 0x53;  (* latin capital letter s with acute accent *)
  Accent 0x13 0x73;  (* latin small letter s with acute accent *)
  Accent 0x5e 0x53;  (* latin capital letter s with circumflex *)
  Accent 0x5e 0x73;  (* latin small letter s with circumflex *)
  Accent 0x18 0x53;  (* latin capital letter s with cedilla *)
  Accent 0x18 0x73;  (* latin small letter s with cedilla *)
  Accent 0x14 0x53;  (* latin capital letter s with hacek *)
  Accent 0x14 0x73;  (* latin small letter s with hacek *)
  Accent 0x18 0x54;  (* latin capital letter t with cedilla *)
  Accent 0x18 0x74;  (* latin small letter t with cedilla *)
  Accent 0x14 0x54;  (* latin capital letter t with hacek *)
  Accent 0x14 0x74;  (* latin small letter t with hacek *)
  Undef;             (* latin capital letter t with stroke *)
  Undef;             (* latin small letter t with stroke *)
  Accent 0x7e 0x55;  (* latin capital letter u with tilde *)
  Accent 0x7e 0x75;  (* latin small letter u with tilde *)
  Accent 0x16 0x55;  (* latin capital letter u with macron *)
  Accent 0x16 0x75;  (* latin small letter u with macron *)
  Accent 0x15 0x55;  (* latin capital letter u with breve *)
  Accent 0x15 0x75;  (* latin small letter u with breve *)
  Accent 0x17 0x55;  (* latin capital letter u with ring above *)
  Accent 0x17 0x75;  (* latin small letter u with ring above *)
  Accent 0x13 0x55;  (* latin capital letter u with double acute accent *)
  Accent 0x13 0x75;  (* latin small letter u with double acute accent *)
  Undef;             (* latin capital letter u with ogonek *)
  Undef;             (* latin small letter u with ogonek *)
  Accent 0x5e 0x57;  (* latin capital letter w with circumflex *)
  Accent 0x5e 0x77;  (* latin cmall letter w with circumflex *)
  Accent 0x5e 0x59;  (* latin capital letter y with circumflex *)
  Accent 0x5e 0x79;  (* latin small letter y with circumflex *)
  Accent 0x7f 0x59;  (* latin capital letter y with diaeresis *)
  Accent 0x13 0x5a;  (* latin capital letter z with acute accent *)
  Accent 0x13 0x7a;  (* latin small letter z with acute accent *)
  Accent 0x5f 0x5a;  (* latin capital letter z with dot above *)
  Accent 0x5f 0x7a;  (* latin small letter z with dot above *)
  Accent 0x14 0x5a;  (* latin capital letter z with hacek *)
  Accent 0x14 0x7a;  (* latin small letter z with hacek *)
  Undef;             (* latin small letter long s *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin small letter script f, florin sign *)
  Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin capital letter a with ring above and acute *)
  Undef;             (* latin small letter a with ring above and acute *)
  Accent 0x13 0x1d;  (* latin capital ligature ae with acute *)
  Accent 0x13 0x1a;  (* latin small ligature ae with acute *)
  Accent 0x13 0x1f;  (* latin capital letter o with stroke and acute *)
  Accent 0x13 0x1c   (* latin small letter o with stroke and acute *)
|];

value uc_to_ott_02 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x11;       (* latin small letter dotless j *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x5e;       (* nonspacing circumflex accent *)
  Simple 0x14;       (* modifier letter hacek *)
  Undef;
  Simple 0x16;       (* modifier letter macron *)
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x15;       (* breve *)
  Simple 0x5f;       (* dot above *)
  Simple 0x17;       (* ring above *)
  Undef;             (* ogonek *)
  Simple 0x7e;       (* nonspacing tilde *)
  Undef;             (* modifier letter double prime *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott_03 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* greek tonos *)
  Undef;             (* greek dialytika tonos *)
  Undef;             (* greek capital letter alpha with tonos *)
  Undef;             (* greek ano teleia *)
  Undef;             (* greek capital letter epsilon with tonos *)
  Undef;             (* greek capital letter eta with tonos *)
  Undef;             (* greek capital letter iota with tonos *)
  Undef;
  Undef;             (* greek capital letter omicron with tonos *)
  Undef;
  Undef;             (* greek capital letter upsilon with tonos *)
  Undef;             (* greek capital letter omega with tonos *)
  Undef;             (* greek small letter iota with dialytika and tonos *)
  Simple 0x41;       (* greek capital letter alpha *)
  Simple 0x41;       (* greek capital letter beta *)
  Simple 0x00;       (* greek capital letter gamma *)
  Simple 0x01;       (* greek capital letter delta *)
  Simple 0x45;       (* greek capital letter epsilon *)
  Simple 0x5a;       (* greek capital letter zeta *)
  Simple 0x48;       (* greek capital letter eta *)
  Simple 0x02;       (* greek capital letter theta *)
  Simple 0x49;       (* greek capital letter iota *)
  Simple 0x4b;       (* greek capital letter kappa *)
  Simple 0x03;       (* greek capital letter lamda *)
  Simple 0x4d;       (* greek capital letter mu *)
  Simple 0x4e;       (* greek capital letter nu *)
  Simple 0x04;       (* greek capital letter xi *)
  Simple 0x4f;       (* greek capital letter omicron *)
  Simple 0x05;       (* greek capital letter pi *)
  Simple 0x50;       (* greek capital letter rho *)
  Undef;
  Simple 0x06;       (* greek capital letter sigma *)
  Simple 0x54;       (* greek capital letter tau *)
  Simple 0x07;       (* greek capital letter upsilon *)
  Simple 0x08;       (* greek capital letter phi *)
  Simple 0x58;       (* greek capital letter chi *)
  Simple 0x09;       (* greek capital letter psi *)
  Simple 0x0a;       (* greek capital letter omega *)
  Accent 0x7f 0x49;  (* greek capital letter iota with dialytika *)
  Accent 0x7f 0x07;  (* greek capital letter upsilon with dialytika *)
  Undef;             (* greek small letter alpha with tonos *)
  Undef;             (* greek small letter epsilon with tonos *)
  Undef;             (* greek small letter eta with tonos *)
  Undef;             (* greek small letter iota with tonos *)
  Undef;             (* greek small letter upsilon with dialytika and tonos *)
  Undef;             (* greek small letter alpha *)
  Undef;             (* greek small letter beta *)
  Undef;             (* greek small letter gamma *)
  Undef;             (* greek small letter delta *)
  Undef;             (* greek small letter epsilon *)
  Undef;             (* greek small letter zeta *)
  Undef;             (* greek small letter eta *)
  Undef;             (* greek small letter theta *)
  Undef;             (* greek small letter iota *)
  Undef;             (* greek small letter kappa *)
  Undef;             (* greek small letter lamda *)
  Undef;             (* greek small letter mu *)
  Undef;             (* greek small letter nu *)
  Undef;             (* greek small letter xi *)
  Simple 0x6f;       (* greek small letter omicron *)
  Undef;             (* greek small letter pi *)
  Undef;             (* greek small letter rho *)
  Undef;             (* greek small letter final sigma *)
  Undef;             (* greek small letter sigma *)
  Undef;             (* greek small letter tau *)
  Undef;             (* greek small letter upsilon *)
  Undef;             (* greek small letter phi *)
  Undef;             (* greek small letter chi *)
  Undef;             (* greek small letter psi *)
  Undef;             (* greek small letter omega *)
  Undef;             (* greek small letter iota with dialytika *)
  Undef;             (* greek small letter upsilon with dialytika *)
  Undef;             (* greek small letter omicron with tonos *)
  Undef;             (* greek small letter upsilon with tonos *)
  Undef;             (* greek small letter omega with tonos *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott_1e =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Accent 0x12 0x57;  (* latin capital letter w with grave *)
  Accent 0x12 0x77;  (* latin small letter w with grave *)
  Accent 0x13 0x57;  (* latin capital letter w with acute *)
  Accent 0x13 0x77;  (* latin small letter w with acute *)
  Accent 0x7f 0x57;  (* latin capital letter w with diaeresis *)
  Accent 0x7f 0x77;  (* latin small letter w with diaeresis *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Accent 0x12 0x59;  (* latin capital letter y with grave *)
  Accent 0x12 0x79;  (* latin small letter y with grave *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott_20 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;              (* en dash *)
  Undef;              (* em dash *)
  Undef;              (* horizontal bar *)
  Undef;
  Undef;              (* double low line *)
  Simple 0x60;        (* left single quotation mark *)
  Simple 0x27;        (* right single quotation mark *)
  Simple 0x2c;        (* single low-9 quotation mark single *)
  Undef;              (* high-reversed-9 quotation mark *)
  Undef;              (* left double quotation mark *)
  Undef;              (* right double quotation mark *)
  Undef;             (* double low-9 quotation mark *)
  Undef;
  Undef;             (* dagger *)
  Undef;             (* double dagger *)
  Undef;             (* bullet *)
  Undef; Undef; Undef;
  Undef;             (* horizontal ellipsis *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* per mille sign *)
  Undef;
  Simple 0x0d;       (* prime *)
  Simple 0x22;       (* double prime *)
  Undef; Undef; Undef; Undef; Undef;
  Undef;             (* single left-pointing angle quotation mark *)
  Undef;             (* single right-pointing angle quotation mark *)
  Undef;
  Undef;             (* double exclamation mark *)
  Undef;
  Undef;             (* overline *)
  Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* fraction slash *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* superscript latin small letter n *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* french franc sign *)
  Undef;             (* lira sign *)
  Undef; Undef;
  Undef;             (* peseta sign *)
  Undef; Undef; Undef; Undef;
  Undef;             (* euro currency symbol *)
  Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott_21 =
[|
  Undef; Undef; Undef; Undef; Undef;
  Undef;             (* care of *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* script small l *)
  Undef; Undef;
  Undef;             (* numero sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* trademark sign *)
  Undef; Undef; Undef;
  Simple 0x0a;       (* ohm sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* estimated symbol *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* vulgar fraction one eighth *)
  Undef;             (* vulgar fraction three eighths *)
  Undef;             (* vulgar fraction five eighths *)
  Undef;             (* vulgar fraction seven eighths *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* leftwards arrow *)
  Simple 0x0b;       (* upwards arrow *)
  Undef;             (* rightwards arrow *)
  Simple 0x0d;       (* downwards arrow *)
  Undef;             (* left right arrow *)
  Undef;             (* up down arrow *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* up down arrow with base *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott_22 =
[|
  Undef; Undef;
  Undef;             (* partial differential *)
  Undef; Undef; Undef;
  Simple 0x01;       (* increment *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x05;       (* n-ary product *)
  Undef;
  Simple 0x06;       (* n-ary summation *)
  Undef;             (* minus sign *)
  Undef; Undef;
  Undef;             (* division slash *)
  Undef; Undef; Undef;
  Undef;             (* bullet operator *)
  Undef;             (* square root *)
  Undef; Undef; Undef;
  Undef;             (* infinity *)
  Undef;             (* right angle *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* intersection *)
  Undef;
  Undef;             (* integral *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* almost equal to *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* not equal to *)
  Undef;             (* identical to *)
  Undef; Undef;
  Undef;             (* less-than or equal to *)
  Undef;             (* greater-than or equal to *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_ott = Charmap.build
[|
  uc_to_ott_00; uc_to_ott_01; uc_to_ott_02; uc_to_ott_03;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  uc_to_ott_1e; empty_table;
  uc_to_ott_20; uc_to_ott_21; uc_to_ott_22; empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table
|];

(* OMS encoding *)

value uc_to_oms_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* space *)
  Undef;             (* exclamation mark *)
  Undef;             (* quotation mark *)
  Undef;             (* number sign *)
  Undef;             (* dollar sign *)
  Undef;             (* percent sign *)
  Undef;             (* ampersand *)
  Undef;             (* apostrophe *)
  Undef;             (* left parenthesis *)
  Undef;             (* right parenthesis *)
  Simple 0x03;       (* asterisk *)
  Undef;             (* plus sign *)
  Undef;             (* comma *)
  Undef;             (* hyphen-minus *)
  Undef;             (* period *)
  Undef;             (* slash *)
  Undef;             (* digit zero *)
  Undef;             (* digit one *)
  Undef;             (* digit two *)
  Undef;             (* digit three *)
  Undef;             (* digit four *)
  Undef;             (* digit five *)
  Undef;             (* digit six *)
  Undef;             (* digit seven *)
  Undef;             (* digit eight *)
  Undef;             (* digit nine *)
  Undef;             (* colon *)
  Undef;             (* semicolon *)
  Undef;             (* less-than sign *)
  Undef;             (* equals sign *)
  Undef;             (* greater-than sign *)
  Undef;             (* question mark *)
  Undef;             (* commercial at *)
  Simple 0x41;       (* latin capital letter a *)
  Simple 0x42;       (* latin capital letter b *)
  Simple 0x43;       (* latin capital letter c *)
  Simple 0x44;       (* latin capital letter d *)
  Simple 0x45;       (* latin capital letter e *)
  Simple 0x46;       (* latin capital letter f *)
  Simple 0x47;       (* latin capital letter g *)
  Simple 0x48;       (* latin capital letter h *)
  Simple 0x49;       (* latin capital letter i *)
  Simple 0x4a;       (* latin capital letter j *)
  Simple 0x4b;       (* latin capital letter k *)
  Simple 0x4c;       (* latin capital letter l *)
  Simple 0x4d;       (* latin capital letter m *)
  Simple 0x4e;       (* latin capital letter n *)
  Simple 0x4f;       (* latin capital letter o *)
  Simple 0x50;       (* latin capital letter p *)
  Simple 0x51;       (* latin capital letter q *)
  Simple 0x52;       (* latin capital letter r *)
  Simple 0x53;       (* latin capital letter s *)
  Simple 0x54;       (* latin capital letter t *)
  Simple 0x55;       (* latin capital letter u *)
  Simple 0x56;       (* latin capital letter v *)
  Simple 0x57;       (* latin capital letter w *)
  Simple 0x58;       (* latin capital letter x *)
  Simple 0x59;       (* latin capital letter y *)
  Simple 0x5a;       (* latin capital letter z *)
  Undef;             (* left square bracket *)
  Simple 0x6e;       (* backslash *)
  Undef;             (* right square bracket *)
  Undef;             (* circumflex accent *)
  Undef;             (* underline *)
  Undef;             (* grave accent *)
  Undef;             (* latin small letter a *)
  Undef;             (* latin small letter b *)
  Undef;             (* latin small letter c *)
  Undef;             (* latin small letter d *)
  Undef;             (* latin small letter e *)
  Undef;             (* latin small letter f *)
  Undef;             (* latin small letter g *)
  Undef;             (* latin small letter h *)
  Undef;             (* latin small letter i *)
  Undef;             (* latin small letter j *)
  Undef;             (* latin small letter k *)
  Undef;             (* latin small letter l *)
  Undef;             (* latin small letter m *)
  Undef;             (* latin small letter n *)
  Undef;             (* latin small letter o *)
  Undef;             (* latin small letter p *)
  Undef;             (* latin small letter q *)
  Undef;             (* latin small letter r *)
  Undef;             (* latin small letter s *)
  Undef;             (* latin small letter t *)
  Undef;             (* latin small letter u *)
  Undef;             (* latin small letter v *)
  Undef;             (* latin small letter w *)
  Undef;             (* latin small letter x *)
  Undef;             (* latin small letter y *)
  Undef;             (* latin small letter z *)
  Simple 0x66;       (* left curly bracket *)
  Simple 0x6a;       (* vertical line *)
  Simple 0x67;       (* right curly bracket *)
  Undef;             (* tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* no-break space *)
  Undef;             (* inverted exclamation mark *)
  Undef;             (* cent sign *)
  Undef;             (* pound sign *)
  Undef;             (* currency sign *)
  Undef;             (* yen sign *)
  Undef;             (* broken bar *)
  Simple 0x78;       (* section sign *)
  Undef;             (* diaeresis *)
  Undef;             (* copyright sign *)
  Undef;             (* feminine ordinal indicator *)
  Undef;             (* left guillemet *)
  Simple 0x3a;       (* not sign *)
  Undef;             (* soft hyphen *)
  Undef;             (* registered trade mark sign *)
  Undef;             (* macron, overline *)
  Undef;             (* degree sign *)
  Simple 0x06;       (* plus-minus sign *)
  Undef;             (* superscript two *)
  Undef;             (* superscript three *)
  Undef;             (* acute accent *)
  Undef;             (* micro sign *)
  Simple 0x7b;       (* paragraph sign *)
  Simple 0x01;       (* middle dot, kana conjoctive *)
  Undef;             (* cedilla *)
  Undef;             (* superscript one *)
  Undef;             (* masculine ordinal indicator *)
  Undef;             (* right guillemet *)
  Undef;             (* vulgar fraction one quarter *)
  Undef;             (* vulgar fraction one half *)
  Undef;             (* vulgar fraction three quarters *)
  Undef;             (* inverted question mark *)
  Undef;             (* latin capital letter a with grave accent *)
  Undef;             (* latin capital letter a with acute accent *)
  Undef;             (* latin capital letter a with circumflex accent *)
  Undef;             (* latin capital letter a with tilde *)
  Undef;             (* latin capital letter a with diaeresis *)
  Undef;             (* latin capital letter a with ring above *)
  Undef;             (* latin capital letter a with e *)
  Undef;             (* latin capital letter c with cedilla *)
  Undef;             (* latin capital letter e with grave accent *)
  Undef;             (* latin capital letter e with acute accent *)
  Undef;             (* latin capital letter e with circumflex accent *)
  Undef;             (* latin capital letter e with diaeresis *)
  Undef;             (* latin capital letter i with grave accent *)
  Undef;             (* latin capital letter i with acute accent *)
  Undef;             (* latin capital letter i with circumflex accent *)
  Undef;             (* latin capital letter i with diaeresis *)
  Undef;             (* latin capital letter eth *)
  Undef;             (* latin capital letter n with tilde *)
  Undef;             (* latin capital letter o with grave accent *)
  Undef;             (* latin capital letter o with acute accent *)
  Undef;             (* latin capital letter o with circumflex accent *)
  Undef;             (* latin capital letter o with tilde *)
  Undef;             (* latin capital letter o with diaeresis *)
  Simple 0x02;       (* multiplication sign *)
  Undef;             (* latin capital letter o with oblique stroke *)
  Undef;             (* latin capital letter u with grave accent *)
  Undef;             (* latin capital letter u with acute accent *)
  Undef;             (* latin capital letter u with circumflex accent *)
  Undef;             (* latin capital letter u with diaeresis *)
  Undef;             (* latin capital letter y with acute accent *)
  Undef;             (* latin capital letter thorn *)
  Undef;             (* latin small letter sharp s *)
  Undef;             (* latin small letter a with grave accent *)
  Undef;             (* latin small letter a with acute accent *)
  Undef;             (* latin small letter a with circumflex accent *)
  Undef;             (* latin small letter a with tilde *)
  Undef;             (* latin small letter a with diaeresis *)
  Undef;             (* latin small letter a with ring above *)
  Undef;             (* latin small letter a with e *)
  Undef;             (* latin small letter c with cedilla *)
  Undef;             (* latin small letter e with grave accent *)
  Undef;             (* latin small letter e with acute accent *)
  Undef;             (* latin small letter e with circumflex accent *)
  Undef;             (* latin small letter e with diaeresis *)
  Undef;             (* latin small letter i with grave accent *)
  Undef;             (* latin small letter i with acute accent *)
  Undef;             (* latin small letter i with circumflex accent *)
  Undef;             (* latin small letter i with diaeresis *)
  Undef;             (* latin small letter eth *)
  Undef;             (* latin small letter n with tilde *)
  Undef;             (* latin small letter o with grave accent *)
  Undef;             (* latin small letter o with acute accent *)
  Undef;             (* latin small letter o with circumflex accent *)
  Undef;             (* latin small letter o with tilde *)
  Undef;             (* latin small letter o with diaeresis *)
  Simple 0x04;       (* division sign *)
  Undef;             (* latin small letter o with oblique stroke *)
  Undef;             (* latin small letter u with grave accent *)
  Undef;             (* latin small letter u with acute accent *)
  Undef;             (* latin small letter u with circumflex accent *)
  Undef;             (* latin small letter u with diaeresis *)
  Undef;             (* latin small letter y with acute accent *)
  Undef;             (* latin small letter thorn *)
  Undef              (* latin small letter y with diaeresis *)
|];

value uc_to_oms_22 =
[|
  Undef; Undef;
  Undef;             (* partial differential *)
  Undef; Undef; Undef;
  Undef;             (* increment *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* n-ary product *)
  Undef;
  Undef;             (* n-ary summation *)
  Undef;             (* minus sign *)
  Undef; Undef;
  Undef;             (* division slash *)
  Undef; Undef; Undef;
  Simple 0x0f;       (* bullet operator *)
  Simple 0x70;       (* square root *)
  Undef; Undef; Undef;
  Simple 0x31;       (* infinity *)
  Undef;             (* right angle *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* intersection *)
  Undef;
  Simple 0x73;       (* integral *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x19;       (* almost equal to *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* not equal to *)
  Simple 0x11;       (* identical to *)
  Undef; Undef;
  Simple 0x14;       (* less-than or equal to *)
  Simple 0x15;       (* greater-than or equal to *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_oms = Charmap.build
[|
  uc_to_oms_00; empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  uc_to_oms_22; empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table
|];

(* OML encoding *)

value uc_to_oml_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* space *)
  Undef;             (* exclamation mark *)
  Undef;             (* quotation mark *)
  Undef;             (* number sign *)
  Undef;             (* dollar sign *)
  Undef;             (* percent sign *)
  Undef;             (* ampersand *)
  Undef;             (* apostrophe *)
  Undef;             (* left parenthesis *)
  Undef;             (* right parenthesis *)
  Simple 0x3f;       (* asterisk *)
  Undef;             (* plus sign *)
  Simple 0x3b;       (* comma *)
  Undef;             (* hyphen-minus *)
  Simple 0x3a;       (* period *)
  Simple 0x3d;       (* slash *)
  Simple 0x30;       (* digit zero *)
  Simple 0x31;       (* digit one *)
  Simple 0x32;       (* digit two *)
  Simple 0x33;       (* digit three *)
  Simple 0x34;       (* digit four *)
  Simple 0x35;       (* digit five *)
  Simple 0x36;       (* digit six *)
  Simple 0x37;       (* digit seven *)
  Simple 0x38;       (* digit eight *)
  Simple 0x39;       (* digit nine *)
  Undef;             (* colon *)
  Undef;             (* semicolon *)
  Simple 0x3c;       (* less-than sign *)
  Undef;             (* equals sign *)
  Simple 0x3e;       (* greater-than sign *)
  Undef;             (* question mark *)
  Undef;             (* commercial at *)
  Simple 0x41;       (* latin capital letter a *)
  Simple 0x42;       (* latin capital letter b *)
  Simple 0x43;       (* latin capital letter c *)
  Simple 0x44;       (* latin capital letter d *)
  Simple 0x45;       (* latin capital letter e *)
  Simple 0x46;       (* latin capital letter f *)
  Simple 0x47;       (* latin capital letter g *)
  Simple 0x48;       (* latin capital letter h *)
  Simple 0x49;       (* latin capital letter i *)
  Simple 0x4a;       (* latin capital letter j *)
  Simple 0x4b;       (* latin capital letter k *)
  Simple 0x4c;       (* latin capital letter l *)
  Simple 0x4d;       (* latin capital letter m *)
  Simple 0x4e;       (* latin capital letter n *)
  Simple 0x4f;       (* latin capital letter o *)
  Simple 0x50;       (* latin capital letter p *)
  Simple 0x51;       (* latin capital letter q *)
  Simple 0x52;       (* latin capital letter r *)
  Simple 0x53;       (* latin capital letter s *)
  Simple 0x54;       (* latin capital letter t *)
  Simple 0x55;       (* latin capital letter u *)
  Simple 0x56;       (* latin capital letter v *)
  Simple 0x57;       (* latin capital letter w *)
  Simple 0x58;       (* latin capital letter x *)
  Simple 0x59;       (* latin capital letter y *)
  Simple 0x5a;       (* latin capital letter z *)
  Undef;             (* left square bracket *)
  Undef;             (* backslash *)
  Undef;             (* right square bracket *)
  Undef;             (* circumflex accent *)
  Undef;             (* underline *)
  Undef;             (* grave accent *)
  Simple 0x61;       (* latin small letter a *)
  Simple 0x62;       (* latin small letter b *)
  Simple 0x63;       (* latin small letter c *)
  Simple 0x64;       (* latin small letter d *)
  Simple 0x65;       (* latin small letter e *)
  Simple 0x66;       (* latin small letter f *)
  Simple 0x67;       (* latin small letter g *)
  Simple 0x68;       (* latin small letter h *)
  Simple 0x69;       (* latin small letter i *)
  Simple 0x6a;       (* latin small letter j *)
  Simple 0x6b;       (* latin small letter k *)
  Simple 0x6c;       (* latin small letter l *)
  Simple 0x6d;       (* latin small letter m *)
  Simple 0x6e;       (* latin small letter n *)
  Simple 0x6f;       (* latin small letter o *)
  Simple 0x70;       (* latin small letter p *)
  Simple 0x71;       (* latin small letter q *)
  Simple 0x72;       (* latin small letter r *)
  Simple 0x73;       (* latin small letter s *)
  Simple 0x74;       (* latin small letter t *)
  Simple 0x75;       (* latin small letter u *)
  Simple 0x76;       (* latin small letter v *)
  Simple 0x77;       (* latin small letter w *)
  Simple 0x78;       (* latin small letter x *)
  Simple 0x79;       (* latin small letter y *)
  Simple 0x7a;       (* latin small letter z *)
  Undef;             (* left curly bracket *)
  Undef;             (* vertical line *)
  Undef;             (* right curly bracket *)
  Undef;             (* tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* no-break space *)
  Undef;             (* inverted exclamation mark *)
  Undef;             (* cent sign *)
  Undef;             (* pound sign *)
  Undef;             (* currency sign *)
  Undef;             (* yen sign *)
  Undef;             (* broken bar *)
  Undef;             (* section sign *)
  Undef;             (* diaeresis *)
  Undef;             (* copyright sign *)
  Undef;             (* feminine ordinal indicator *)
  Undef;             (* left guillemet *)
  Undef;             (* not sign *)
  Undef;             (* soft hyphen *)
  Undef;             (* registered trade mark sign *)
  Undef;             (* macron, overline *)
  Undef;             (* degree sign *)
  Undef;             (* plus-minus sign *)
  Undef;             (* superscript two *)
  Undef;             (* superscript three *)
  Undef;             (* acute accent *)
  Undef;             (* micro sign *)
  Undef;             (* paragraph sign *)
  Undef;             (* middle dot, kana conjoctive *)
  Undef;             (* cedilla *)
  Undef;             (* superscript one *)
  Undef;             (* masculine ordinal indicator *)
  Undef;             (* right guillemet *)
  Undef;             (* vulgar fraction one quarter *)
  Undef;             (* vulgar fraction one half *)
  Undef;             (* vulgar fraction three quarters *)
  Undef;             (* inverted question mark *)
  Undef;             (* latin capital letter a with grave accent *)
  Undef;             (* latin capital letter a with acute accent *)
  Undef;             (* latin capital letter a with circumflex accent *)
  Undef;             (* latin capital letter a with tilde *)
  Undef;             (* latin capital letter a with diaeresis *)
  Undef;             (* latin capital letter a with ring above *)
  Undef;             (* latin capital letter a with e *)
  Undef;             (* latin capital letter c with cedilla *)
  Undef;             (* latin capital letter e with grave accent *)
  Undef;             (* latin capital letter e with acute accent *)
  Undef;             (* latin capital letter e with circumflex accent *)
  Undef;             (* latin capital letter e with diaeresis *)
  Undef;             (* latin capital letter i with grave accent *)
  Undef;             (* latin capital letter i with acute accent *)
  Undef;             (* latin capital letter i with circumflex accent *)
  Undef;             (* latin capital letter i with diaeresis *)
  Undef;             (* latin capital letter eth *)
  Undef;             (* latin capital letter n with tilde *)
  Undef;             (* latin capital letter o with grave accent *)
  Undef;             (* latin capital letter o with acute accent *)
  Undef;             (* latin capital letter o with circumflex accent *)
  Undef;             (* latin capital letter o with tilde *)
  Undef;             (* latin capital letter o with diaeresis *)
  Undef;             (* multiplication sign *)
  Undef;             (* latin capital letter o with oblique stroke *)
  Undef;             (* latin capital letter u with grave accent *)
  Undef;             (* latin capital letter u with acute accent *)
  Undef;             (* latin capital letter u with circumflex accent *)
  Undef;             (* latin capital letter u with diaeresis *)
  Undef;             (* latin capital letter y with acute accent *)
  Undef;             (* latin capital letter thorn *)
  Undef;             (* latin small letter sharp s *)
  Undef;             (* latin small letter a with grave accent *)
  Undef;             (* latin small letter a with acute accent *)
  Undef;             (* latin small letter a with circumflex accent *)
  Undef;             (* latin small letter a with tilde *)
  Undef;             (* latin small letter a with diaeresis *)
  Undef;             (* latin small letter a with ring above *)
  Undef;             (* latin small letter a with e *)
  Undef;             (* latin small letter c with cedilla *)
  Undef;             (* latin small letter e with grave accent *)
  Undef;             (* latin small letter e with acute accent *)
  Undef;             (* latin small letter e with circumflex accent *)
  Undef;             (* latin small letter e with diaeresis *)
  Undef;             (* latin small letter i with grave accent *)
  Undef;             (* latin small letter i with acute accent *)
  Undef;             (* latin small letter i with circumflex accent *)
  Undef;             (* latin small letter i with diaeresis *)
  Undef;             (* latin small letter eth *)
  Undef;             (* latin small letter n with tilde *)
  Undef;             (* latin small letter o with grave accent *)
  Undef;             (* latin small letter o with acute accent *)
  Undef;             (* latin small letter o with circumflex accent *)
  Undef;             (* latin small letter o with tilde *)
  Undef;             (* latin small letter o with diaeresis *)
  Undef;             (* division sign *)
  Undef;             (* latin small letter o with oblique stroke *)
  Undef;             (* latin small letter u with grave accent *)
  Undef;             (* latin small letter u with acute accent *)
  Undef;             (* latin small letter u with circumflex accent *)
  Undef;             (* latin small letter u with diaeresis *)
  Undef;             (* latin small letter y with acute accent *)
  Undef;             (* latin small letter thorn *)
  Undef              (* latin small letter y with diaeresis *)
|];

value uc_to_oml_01 =
[|
  Undef;             (* latin capital letter a with macron *)
  Undef;             (* latin small letter a with macron *)
  Undef;             (* latin capital letter a with breve *)
  Undef;             (* latin small letter a with breve *)
  Undef;             (* latin capital letter a with ogonek *)
  Undef;             (* latin small letter a with ogonek *)
  Undef;             (* latin capital letter c with acute accent *)
  Undef;             (* latin small letter c with acute accent *)
  Undef;             (* latin capital letter c with circumflex *)
  Undef;             (* latin small letter c with circumflex *)
  Undef;             (* latin capital letter c with dot above *)
  Undef;             (* latin small letter c with dot above *)
  Undef;             (* latin capital letter c with caron *)
  Undef;             (* latin small letter c with caron *)
  Undef;             (* latin capital letter d with hacek *)
  Undef;             (* latin small letter d with hacek *)
  Undef;             (* latin capital letter d with stroke *)
  Undef;             (* latin small letter d with stroke *)
  Undef;             (* latin capital letter e with macron *)
  Undef;             (* latin small letter e with macron *)
  Undef;             (* latin capital letter e with breve *)
  Undef;             (* latin small letter e with breve *)
  Undef;             (* latin capital letter e with dot above *)
  Undef;             (* latin small letter e with dot above *)
  Undef;             (* latin capital letter e with ogonek *)
  Undef;             (* latin small letter e with ogonek *)
  Undef;             (* latin capital letter e with hacek *)
  Undef;             (* latin small letter e with hacek *)
  Undef;             (* latin capital letter g with circumflex *)
  Undef;             (* latin small letter g with circumflex *)
  Undef;             (* latin capital letter g with breve *)
  Undef;             (* latin small letter g with breve *)
  Undef;             (* latin capital letter g with dot above *)
  Undef;             (* latin small letter g with dot above *)
  Undef;             (* latin capital letter g with cedilla *)
  Undef;             (* latin small letter g with cedilla *)
  Undef;             (* latin capital letter h with circumflex *)
  Undef;             (* latin small letter h with circumflex *)
  Undef;             (* latin capital letter h with stroke *)
  Undef;             (* latin small letter h with stroke *)
  Undef;             (* latin capital letter i with tilde *)
  Undef;             (* latin small letter i with tilde *)
  Undef;             (* latin capital letter i with macron *)
  Undef;             (* latin small letter i with macron *)
  Undef;             (* latin capital letter i with breve *)
  Undef;             (* latin small letter i with breve *)
  Undef;             (* latin capital letter i with ogonek *)
  Undef;             (* latin small letter i with ogonek *)
  Undef;             (* latin capital letter i with dot above *)
  Simple 0x7b;       (* latin small letter i without dot above *)
  Undef;             (* latin capital ligature ij *)
  Undef;             (* latin small ligature ij *)
  Undef;             (* latin capital letter j with circumflex *)
  Undef;             (* latin small letter j with circumflex *)
  Undef;             (* latin capital letter k with cedilla *)
  Undef;             (* latin small letter k with cedilla *)
  Undef;             (* latin small letter kra *)
  Undef;             (* latin capital letter l with acute accent *)
  Undef;             (* latin small letter l with acute accent *)
  Undef;             (* latin capital letter l with cedilla *)
  Undef;             (* latin small letter l with cedilla *)
  Undef;             (* latin capital letter l with hacek *)
  Undef;             (* latin small letter l with hacek *)
  Undef;             (* latin capital letter l with middle dot *)
  Undef;             (* latin small letter l with middle dot *)
  Undef;             (* latin capital letter l with stroke *)
  Undef;             (* latin small letter l with stroke *)
  Undef;             (* latin capital letter n with acute accent *)
  Undef;             (* latin small letter n with acute accent *)
  Undef;             (* latin capital letter n with cedilla *)
  Undef;             (* latin small letter n with cedilla *)
  Undef;             (* latin capital letter n with hacek *)
  Undef;             (* latin small letter n with hacek *)
  Undef;             (* latin small letter n preceded by apostrophe *)
  Undef;             (* latin capital letter eng *)
  Undef;             (* latin small letter eng *)
  Undef;             (* latin capital letter o with macron *)
  Undef;             (* latin small letter o with macron *)
  Undef;             (* latin capital letter o with breve *)
  Undef;             (* latin small letter o with breve *)
  Undef;             (* latin capital letter o with double acute accent *)
  Undef;             (* latin small letter o with double acute accent *)
  Undef;             (* latin capital ligature o with e *)
  Undef;             (* latin small ligature o with e *)
  Undef;             (* latin capital letter r with acute accent *)
  Undef;             (* latin small letter r with acute accent *)
  Undef;             (* latin capital letter r with cedilla *)
  Undef;             (* latin small letter r with cedilla *)
  Undef;             (* latin capital letter r with hacek *)
  Undef;             (* latin small letter r with hacek *)
  Undef;             (* latin capital letter s with acute accent *)
  Undef;             (* latin small letter s with acute accent *)
  Undef;             (* latin capital letter s with circumflex *)
  Undef;             (* latin small letter s with circumflex *)
  Undef;             (* latin capital letter s with cedilla *)
  Undef;             (* latin small letter s with cedilla *)
  Undef;             (* latin capital letter s with hacek *)
  Undef;             (* latin small letter s with hacek *)
  Undef;             (* latin capital letter t with cedilla *)
  Undef;             (* latin small letter t with cedilla *)
  Undef;             (* latin capital letter t with hacek *)
  Undef;             (* latin small letter t with hacek *)
  Undef;             (* latin capital letter t with stroke *)
  Undef;             (* latin small letter t with stroke *)
  Undef;             (* latin capital letter u with tilde *)
  Undef;             (* latin small letter u with tilde *)
  Undef;             (* latin capital letter u with macron *)
  Undef;             (* latin small letter u with macron *)
  Undef;             (* latin capital letter u with breve *)
  Undef;             (* latin small letter u with breve *)
  Undef;             (* latin capital letter u with ring above *)
  Undef;             (* latin small letter u with ring above *)
  Undef;             (* latin capital letter u with double acute accent *)
  Undef;             (* latin small letter u with double acute accent *)
  Undef;             (* latin capital letter u with ogonek *)
  Undef;             (* latin small letter u with ogonek *)
  Undef;             (* latin capital letter w with circumflex *)
  Undef;             (* latin cmall letter w with circumflex *)
  Undef;             (* latin capital letter y with circumflex *)
  Undef;             (* latin small letter y with circumflex *)
  Undef;             (* latin capital letter y with diaeresis *)
  Undef;             (* latin capital letter z with acute accent *)
  Undef;             (* latin small letter z with acute accent *)
  Undef;             (* latin capital letter z with dot above *)
  Undef;             (* latin small letter z with dot above *)
  Undef;             (* latin capital letter z with hacek *)
  Undef;             (* latin small letter z with hacek *)
  Undef;             (* latin small letter long s *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin small letter script f, florin sign *)
  Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* latin capital letter a with ring above and acute *)
  Undef;             (* latin small letter a with ring above and acute *)
  Accent 0x13 0x1d;  (* latin capital ligature ae with acute *)
  Accent 0x13 0x1a;  (* latin small ligature ae with acute *)
  Accent 0x13 0x1f;  (* latin capital letter o with stroke and acute *)
  Accent 0x13 0x1c   (* latin small letter o with stroke and acute *)
|];

value uc_to_oml_03 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef;
  Undef;             (* greek tonos *)
  Undef;             (* greek dialytika tonos *)
  Undef;             (* greek capital letter alpha with tonos *)
  Undef;             (* greek ano teleia *)
  Undef;             (* greek capital letter epsilon with tonos *)
  Undef;             (* greek capital letter eta with tonos *)
  Undef;             (* greek capital letter iota with tonos *)
  Undef;
  Undef;             (* greek capital letter omicron with tonos *)
  Undef;
  Undef;             (* greek capital letter upsilon with tonos *)
  Undef;             (* greek capital letter omega with tonos *)
  Undef;             (* greek small letter iota with dialytika and tonos *)
  Simple 0x41;       (* greek capital letter alpha *)
  Simple 0x41;       (* greek capital letter beta *)
  Simple 0x00;       (* greek capital letter gamma *)
  Simple 0x01;       (* greek capital letter delta *)
  Simple 0x45;       (* greek capital letter epsilon *)
  Simple 0x5a;       (* greek capital letter zeta *)
  Simple 0x48;       (* greek capital letter eta *)
  Simple 0x02;       (* greek capital letter theta *)
  Simple 0x49;       (* greek capital letter iota *)
  Simple 0x4b;       (* greek capital letter kappa *)
  Simple 0x03;       (* greek capital letter lamda *)
  Simple 0x4d;       (* greek capital letter mu *)
  Simple 0x4e;       (* greek capital letter nu *)
  Simple 0x04;       (* greek capital letter xi *)
  Simple 0x4f;       (* greek capital letter omicron *)
  Simple 0x05;       (* greek capital letter pi *)
  Simple 0x50;       (* greek capital letter rho *)
  Undef;
  Simple 0x06;       (* greek capital letter sigma *)
  Simple 0x54;       (* greek capital letter tau *)
  Simple 0x07;       (* greek capital letter upsilon *)
  Simple 0x08;       (* greek capital letter phi *)
  Simple 0x58;       (* greek capital letter chi *)
  Simple 0x09;       (* greek capital letter psi *)
  Simple 0x0a;       (* greek capital letter omega *)
  Undef;             (* greek capital letter iota with dialytika *)
  Undef;             (* greek capital letter upsilon with dialytika *)
  Undef;             (* greek small letter alpha with tonos *)
  Undef;             (* greek small letter epsilon with tonos *)
  Undef;             (* greek small letter eta with tonos *)
  Undef;             (* greek small letter iota with tonos *)
  Undef;             (* greek small letter upsilon with dialytika and tonos *)
  Simple 0x0b;       (* greek small letter alpha *)
  Simple 0x0c;       (* greek small letter beta *)
  Simple 0x0d;       (* greek small letter gamma *)
  Simple 0x0e;       (* greek small letter delta *)
  Simple 0x0f;       (* greek small letter epsilon *)
  Simple 0x10;       (* greek small letter zeta *)
  Simple 0x11;       (* greek small letter eta *)
  Simple 0x12;       (* greek small letter theta *)
  Simple 0x13;       (* greek small letter iota *)
  Simple 0x14;       (* greek small letter kappa *)
  Simple 0x15;       (* greek small letter lamda *)
  Simple 0x16;       (* greek small letter mu *)
  Simple 0x17;       (* greek small letter nu *)
  Simple 0x18;       (* greek small letter xi *)
  Simple 0x67;       (* greek small letter omicron *)
  Simple 0x19;       (* greek small letter pi *)
  Simple 0x1a;       (* greek small letter rho *)
  Simple 0x26;       (* greek small letter final sigma *)
  Simple 0x1b;       (* greek small letter sigma *)
  Simple 0x1c;       (* greek small letter tau *)
  Simple 0x1d;       (* greek small letter upsilon *)
  Simple 0x1e;       (* greek small letter phi *)
  Simple 0x1f;       (* greek small letter chi *)
  Simple 0x20;       (* greek small letter psi *)
  Simple 0x21;       (* greek small letter omega *)
  Undef;             (* greek small letter iota with dialytika *)
  Undef;             (* greek small letter upsilon with dialytika *)
  Undef;             (* greek small letter omicron with tonos *)
  Undef;             (* greek small letter upsilon with tonos *)
  Undef;             (* greek small letter omega with tonos *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_oml_21 =
[|
  Undef; Undef; Undef; Undef; Undef;
  Undef;             (* care of *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* script small l *)
  Undef; Undef;
  Undef;             (* numero sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef;
  Undef;             (* trademark sign *)
  Undef; Undef; Undef;
  Simple 0x0a;       (* ohm sign *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* estimated symbol *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;             (* vulgar fraction one eighth *)
  Undef;             (* vulgar fraction three eighths *)
  Undef;             (* vulgar fraction five eighths *)
  Undef;             (* vulgar fraction seven eighths *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* leftwards arrow *)
  Undef;             (* upwards arrow *)
  Undef;             (* rightwards arrow *)
  Undef;             (* downwards arrow *)
  Undef;             (* left right arrow *)
  Undef;             (* up down arrow *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* up down arrow with base *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_oml_22 =
[|
  Undef; Undef;
  Simple 0x40;       (* partial differential *)
  Undef; Undef; Undef;
  Simple 0x01;       (* increment *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x05;       (* n-ary product *)
  Undef;
  Simple 0x06;       (* n-ary summation *)
  Undef;             (* minus sign *)
  Undef; Undef;
  Undef;             (* division slash *)
  Undef; Undef; Undef;
  Undef;             (* bullet operator *)
  Undef;             (* square root *)
  Undef; Undef; Undef;
  Undef;             (* infinity *)
  Undef;             (* right angle *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;
  Undef;             (* intersection *)
  Undef;
  Undef;             (* integral *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* almost equal to *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;             (* not equal to *)
  Undef;             (* identical to *)
  Undef; Undef;
  Undef;             (* less-than or equal to *)
  Undef;             (* greater-than or equal to *)
  Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value uc_to_oml = Charmap.build
[|
  uc_to_oml_00; uc_to_oml_01; empty_table;  uc_to_oml_03;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  uc_to_oml_21; uc_to_oml_22; empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table;
  empty_table;  empty_table;  empty_table;  empty_table
|];

value fake_00 =
[|
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x00a0; (* space -> no-break space *)
  Undef;         (* exclamation mark *)
  Undef;         (* quotation mark *)
  Undef;         (* number sign *)
  Undef;         (* dollar sign *)
  Undef;         (* percent sign *)
  Undef;         (* ampersand *)
  Undef;         (* apostrophe *)
  Undef;         (* left parenthesis *)
  Undef;         (* right parenthesis *)
  Undef;         (* asterisk *)
  Undef;         (* plus sign *)
  Undef;         (* comma *)
  Undef;         (* hyphen-minus *)
  Undef;         (* full stop *)
  Undef;         (* solidus *)
  Undef;         (* digit zero *)
  Undef;         (* digit one *)
  Undef;         (* digit two *)
  Undef;         (* digit three *)
  Undef;         (* digit four *)
  Undef;         (* digit five *)
  Undef;         (* digit six *)
  Undef;         (* digit seven *)
  Undef;         (* digit eight *)
  Undef;         (* digit nine *)
  Undef;         (* colon *)
  Undef;         (* semicolon *)
  Undef;         (* less-than sign *)
  Undef;         (* equals sign *)
  Undef;         (* greater-than sign *)
  Undef;         (* question mark *)
  Undef;         (* commercial at *)
  Undef;         (* latin capital letter a *)
  Undef;         (* latin capital letter b *)
  Undef;         (* latin capital letter c *)
  Undef;         (* latin capital letter d *)
  Undef;         (* latin capital letter e *)
  Undef;         (* latin capital letter f *)
  Undef;         (* latin capital letter g *)
  Undef;         (* latin capital letter h *)
  Undef;         (* latin capital letter i *)
  Undef;         (* latin capital letter j *)
  Undef;         (* latin capital letter k *)
  Undef;         (* latin capital letter l *)
  Undef;         (* latin capital letter m *)
  Undef;         (* latin capital letter n *)
  Undef;         (* latin capital letter o *)
  Undef;         (* latin capital letter p *)
  Undef;         (* latin capital letter q *)
  Undef;         (* latin capital letter r *)
  Undef;         (* latin capital letter s *)
  Undef;         (* latin capital letter t *)
  Undef;         (* latin capital letter u *)
  Undef;         (* latin capital letter v *)
  Undef;         (* latin capital letter w *)
  Undef;         (* latin capital letter x *)
  Undef;         (* latin capital letter y *)
  Undef;         (* latin capital letter z *)
  Undef;         (* left square bracket *)
  Undef;         (* reverse solidus *)
  Undef;         (* right square bracket *)
  Undef;         (* circumflex accent *)
  Undef;         (* low line *)
  Undef;         (* grave accent *)
  Undef;         (* latin small letter a *)
  Undef;         (* latin small letter b *)
  Undef;         (* latin small letter c *)
  Undef;         (* latin small letter d *)
  Undef;         (* latin small letter e *)
  Undef;         (* latin small letter f *)
  Undef;         (* latin small letter g *)
  Undef;         (* latin small letter h *)
  Accent 0x0131 0x02d9; (* latin small letter i *)
  Accent 0x0237 0x02d9; (* latin small letter j *)
  Undef;         (* latin small letter k *)
  Undef;         (* latin small letter l *)
  Undef;         (* latin small letter m *)
  Undef;         (* latin small letter n *)
  Undef;         (* latin small letter o *)
  Undef;         (* latin small letter p *)
  Undef;         (* latin small letter q *)
  Undef;         (* latin small letter r *)
  Undef;         (* latin small letter s *)
  Undef;         (* latin small letter t *)
  Undef;         (* latin small letter u *)
  Undef;         (* latin small letter v *)
  Undef;         (* latin small letter w *)
  Undef;         (* latin small letter x *)
  Undef;         (* latin small letter y *)
  Undef;         (* latin small letter z *)
  Undef;         (* left curly bracket *)
  Simple 0x00a6; (* vertical line -> broken bar *)
  Undef;         (* right curly bracket *)
  Simple 0x02dc; (* tilde -> small tilde *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Simple 0x0020; (* no-break space -> space *)
  Undef;         (* inverted exclamation mark *)
  Undef;         (* cent sign *)
  Undef;         (* pound sign *)
  Undef;         (* currency sign *)
  Undef;         (* yen sign *)
  Simple 0x007c; (* broken bar -> vertical line *)
  Undef;         (* section sign *)
  Undef;         (* diaeresis *)
  Undef;         (* copyright sign *)
  Undef;         (* feminine ordinal indicator *)
  Sequence [0x2039; 0x2039]; (* left-pointing double angle quotation mark ->
                                 2x single left-pointing angle quotation mark *)
  Undef;         (* not sign *)
  Undef;         (* soft hyphen *)
  Undef;         (* registered sign *)
  Undef;         (* macron *)
  Undef;         (* degree sign *)
  Undef;         (* plus-minus sign *)
  Undef;         (* superscript two *)
  Undef;         (* superscript three *)
  Undef;         (* acute accent *)
  Simple 0x03bc; (* micro sign -> greek small letter mu *)
  Undef;         (* pilcrow sign *)
  Undef;         (* middle dot *)
  Undef;         (* cedilla *)
  Undef;         (* superscript one *)
  Undef;         (* masculine ordinal indicator *)
  Sequence [0x203A; 0x203A];         (* right-pointing double angle quotation mark ->
                                         2x single right pointing angle quotation mark *)
  Sequence [0x00b9; 0x2044; 0x2084]; (* vulgar fraction one quarter ->
                                         superscript one, fraction slash, subscript four *)
  Sequence [0x00b9; 0x2044; 0x2081]; (* vulgar fraction one half ->
                                         superscript one, fraction slash, subscript one *)
  Sequence [0x00b3; 0x2044; 0x2084]; (* vulgar fraction three quarters
                                         supercript three, fraction slash, subscript four *)
  Undef;                     (* inverted question mark *)
  Accent 0x0060 0x0041;      (* latin capital letter a with grave *)
  Accent 0x00b4 0x0041;      (* latin capital letter a with acute *)
  Accent 0x005e 0x0041;      (* latin capital letter a with circumflex *)
  Accent 0x02dc 0x0041;      (* latin capital letter a with tilde *)
  Accent 0x00a8 0x0041;      (* latin capital letter a with diaeresis *)
  Accent 0x02da 0x0041;      (* latin capital letter a with ring above *)
  Sequence [0x0041; 0x0045]; (* latin capital letter ae *)
  Accent 0x00b8 0x0043;      (* latin capital letter c with cedilla *)
  Accent 0x0060 0x0045;      (* latin capital letter e with grave *)
  Accent 0x00b4 0x0045;      (* latin capital letter e with acute *)
  Accent 0x005e 0x0045;      (* latin capital letter e with circumflex *)
  Accent 0x00a8 0x0045;      (* latin capital letter e with diaeresis *)
  Accent 0x0060 0x0049;      (* latin capital letter i with grave *)
  Accent 0x00b4 0x0049;      (* latin capital letter i with acute *)
  Accent 0x005e 0x0049;      (* latin capital letter i with circumflex *)
  Accent 0x00a8 0x0049;      (* latin capital letter i with diaeresis *)
  Undef;                     (* latin capital letter eth *)
  Accent 0x02dc 0x004e;      (* latin capital letter n with tilde *)
  Accent 0x0060 0x004f;      (* latin capital letter o with grave *)
  Accent 0x00b4 0x004f;      (* latin capital letter o with acute *)
  Accent 0x005e 0x004f;      (* latin capital letter o with circumflex *)
  Accent 0x02dc 0x004f;      (* latin capital letter o with tilde *)
  Accent 0x00a8 0x004f;      (* latin capital letter o with diaeresis *)
  Undef;                     (* multiplication sign *)
  Undef;                     (* latin capital letter o with stroke *)
  Accent 0x0060 0x0055;      (* latin capital letter u with grave *)
  Accent 0x00b4 0x0055;      (* latin capital letter u with acute *)
  Accent 0x005e 0x0055;      (* latin capital letter u with circumflex *)
  Accent 0x00a8 0x0055;      (* latin capital letter u with diaeresis *)
  Accent 0x00b4 0x0059;      (* latin capital letter y with acute *)
  Undef;                     (* latin capital letter thorn *)
  Undef;                     (* latin small letter sharp s *)
  Accent 0x0060 0x0061;      (* latin small letter a with grave *)
  Accent 0x00b4 0x0061;      (* latin small letter a with acute *)
  Accent 0x005e 0x0061;      (* latin small letter a with circumflex *)
  Accent 0x02dc 0x0061;      (* latin small letter a with tilde *)
  Accent 0x00a8 0x0061;      (* latin small letter a with diaeresis *)
  Accent 0x02da 0x0061;      (* latin small letter a with ring above *)
  Sequence [0x0061; 0x0065]; (* latin small letter ae *)
  Accent 0x00b8 0x0063;      (* latin small letter c with cedilla *)
  Accent 0x0060 0x0065;      (* latin small letter e with grave *)
  Accent 0x00b4 0x0065;      (* latin small letter e with acute *)
  Accent 0x005e 0x0065;      (* latin small letter e with circumflex *)
  Accent 0x00a8 0x0065;      (* latin small letter e with diaeresis *)
  Accent 0x0060 0x0131;      (* latin small letter i with grave *)
  Accent 0x00b4 0x0131;      (* latin small letter i with acute *)
  Accent 0x005e 0x0131;      (* latin small letter i with circumflex *)
  Accent 0x00a8 0x0131;      (* latin small letter i with diaeresis *)
  Undef;                     (* latin small letter eth *)
  Accent 0x02dc 0x006e;      (* latin small letter n with tilde *)
  Accent 0x0060 0x006f;      (* latin small letter o with grave *)
  Accent 0x00b4 0x006f;      (* latin small letter o with acute *)
  Accent 0x005e 0x006f;      (* latin small letter o with circumflex *)
  Accent 0x02dc 0x006f;      (* latin small letter o with tilde *)
  Accent 0x00a8 0x006f;      (* latin small letter o with diaeresis *)
  Undef;                     (* division sign *)
  Undef;                     (* latin small letter o with stroke *)
  Accent 0x0060 0x0075;      (* latin small letter u with grave *)
  Accent 0x00b4 0x0075;      (* latin small letter u with acute *)
  Accent 0x005e 0x0075;      (* latin small letter u with circumflex *)
  Accent 0x00a8 0x0075;      (* latin small letter u with diaeresis *)
  Accent 0x00b4 0x0079;      (* latin small letter y with acute *)
  Undef;                     (* latin small letter thorn *)
  Accent 0x00a8 0x0079       (* latin small letter y with diaeresis *)
|];

value fake_01 =
[|
  Accent 0x00af 0x0041;       (* latin capital letter a with macron *)
  Accent 0x00af 0x0061;       (* latin small letter a with macron *)
  Accent 0x02d8 0x0041;       (* latin capital letter a with breve *)
  Accent 0x02d8 0x0061;       (* latin small letter a with breve *)
  Accent 0x02db 0x0041;       (* latin capital letter a with ogonek *)
  Accent 0x02db 0x0061;       (* latin small letter a with ogonek *)
  Accent 0x00b4 0x0043;       (* latin capital letter c with acute *)
  Accent 0x00b4 0x0063;       (* latin small letter c with acute *)
  Accent 0x005e 0x0043;       (* latin capital letter c with circumflex *)
  Accent 0x005e 0x0063;       (* latin small letter c with circumflex *)
  Accent 0x02d9 0x0043;       (* latin capital letter c with dot above *)
  Accent 0x02d9 0x0063;       (* latin small letter c with dot above *)
  Accent 0x02c7 0x0043;       (* latin capital letter c with caron *)
  Accent 0x02c7 0x0063;       (* latin small letter c with caron *)
  Accent 0x02c7 0x0044;       (* latin capital letter d with caron *)
  Accent 0x02c7 0x0064;       (* latin small letter d with caron *)
  Undef;                      (* latin capital letter d with stroke *)
  Undef;                      (* latin small letter d with stroke *)
  Accent 0x00af 0x0045;       (* latin capital letter e with macron *)
  Accent 0x00af 0x0065;       (* latin small letter e with macron *)
  Accent 0x02d8 0x0045;       (* latin capital letter e with breve *)
  Accent 0x02d8 0x0065;       (* latin small letter e with breve *)
  Accent 0x02d9 0x0045;       (* latin capital letter e with dot above *)
  Accent 0x02d9 0x0065;       (* latin small letter e with dot above *)
  Accent 0x02db 0x0045;       (* latin capital letter e with ogonek *)
  Accent 0x02db 0x0065;       (* latin small letter e with ogonek *)
  Accent 0x02c7 0x0045;       (* latin capital letter e with caron *)
  Accent 0x02c7 0x0065;       (* latin small letter e with caron *)
  Accent 0x005e 0x0047;       (* latin capital letter g with circumflex *)
  Accent 0x005e 0x0067;       (* latin small letter g with circumflex *)
  Accent 0x02d8 0x0047;       (* latin capital letter g with breve *)
  Accent 0x02d8 0x0067;       (* latin small letter g with breve *)
  Accent 0x02d9 0x0047;       (* latin capital letter g with dot above *)
  Accent 0x02d9 0x0067;       (* latin small letter g with dot above *)
  Accent 0x00b8 0x0047;       (* latin capital letter g with cedilla *)
  Accent 0x00b8 0x0067;       (* latin small letter g with cedilla *)
  Accent 0x005e 0x0048;       (* latin capital letter h with circumflex *)
  Accent 0x005e 0x0068;       (* latin small letter h with circumflex *)
  Undef;                      (* latin capital letter h with stroke *)
  Undef;                      (* latin small letter h with stroke *)
  Accent 0x02dc 0x0049;       (* latin capital letter i with tilde *)
  Accent 0x02dc 0x0131;       (* latin small letter i with tilde *)
  Accent 0x00af 0x0049;       (* latin capital letter i with macron *)
  Accent 0x00af 0x0131;       (* latin small letter i with macron *)
  Accent 0x02d8 0x0049;       (* latin capital letter i with breve *)
  Accent 0x02d8 0x0131;       (* latin small letter i with breve *)
  Accent 0x02db 0x0049;       (* latin capital letter i with ogonek *)
  Accent 0x02db 0x0131;       (* latin small letter i with ogonek *)
  Accent 0x02d9 0x0049;       (* latin capital letter i with dot above *)
  Undef;                      (* latin small letter dotless i *)
  Sequence [0x0049; 0x004a];  (* latin capital ligature ij *)
  Sequence [0x0069; 0x006a];  (* latin small ligature ij *)
  Accent 0x005e 0x004a;       (* latin capital letter j with circumflex *)
  Accent 0x005e 0x0237;       (* latin small letter j with circumflex *)
  Accent 0x00b8 0x004b;       (* latin capital letter k with cedilla *)
  Accent 0x00b8 0x006b;       (* latin small letter k with cedilla *)
  Undef;                      (* latin small letter kra *)
  Accent 0x00b4 0x004c;       (* latin capital letter l with acute *)
  Accent 0x00b4 0x006c;       (* latin small letter l with acute *)
  Accent 0x00b8 0x004c;       (* latin capital letter l with cedilla *)
  Accent 0x00b8 0x006c;       (* latin small letter l with cedilla *)
  Accent 0x02c7 0x004c;       (* latin capital letter l with caron *)
  Accent 0x02c7 0x006c;       (* latin small letter l with caron *)
  Undef;                      (* latin capital letter l with middle dot *)
  Undef;                      (* latin small letter l with middle dot *)
  Undef;                      (* latin capital letter l with stroke *)
  Undef;                      (* latin small letter l with stroke *)
  Accent 0x00b4 0x004e;       (* latin capital letter n with acute *)
  Accent 0x00b4 0x006e;       (* latin small letter n with acute *)
  Accent 0x00b8 0x004e;       (* latin capital letter n with cedilla *)
  Accent 0x00b8 0x006e;       (* latin small letter n with cedilla *)
  Accent 0x02c7 0x004e;       (* latin capital letter n with caron *)
  Accent 0x02c7 0x006e;       (* latin small letter n with caron *)
  Sequence [0x0027; 0x006e];  (* latin small letter n preceded by apostrophe *)
  Undef;                      (* latin capital letter eng *)
  Undef;                      (* latin small letter eng *)
  Accent 0x00af 0x004f;       (* latin capital letter o with macron *)
  Accent 0x00af 0x006f;       (* latin small letter o with macron *)
  Accent 0x02d8 0x004f;       (* latin capital letter o with breve *)
  Accent 0x02d8 0x006f;       (* latin small letter o with breve *)
  Accent 0x02dd 0x004f;       (* latin capital letter o with double acute *)
  Accent 0x02dd 0x006f;       (* latin small letter o with double acute *)
  Sequence [0x004f; 0x0045];  (* latin capital ligature oe *)
  Sequence [0x006f; 0x0065];  (* latin small ligature oe *)
  Accent 0x00b4 0x0052;       (* latin capital letter r with acute *)
  Accent 0x00b4 0x0072;       (* latin small letter r with acute *)
  Accent 0x00b8 0x0052;       (* latin capital letter r with cedilla *)
  Accent 0x00b8 0x0072;       (* latin small letter r with cedilla *)
  Accent 0x02c7 0x0052;       (* latin capital letter r with caron *)
  Accent 0x02c7 0x0072;       (* latin small letter r with caron *)
  Accent 0x00b4 0x0053;       (* latin capital letter s with acute *)
  Accent 0x00b4 0x0073;       (* latin small letter s with acute *)
  Accent 0x005e 0x0053;       (* latin capital letter s with circumflex *)
  Accent 0x005e 0x0073;       (* latin small letter s with circumflex *)
  Accent 0x00b8 0x0053;       (* latin capital letter s with cedilla *)
  Accent 0x00b8 0x0073;       (* latin small letter s with cedilla *)
  Accent 0x02c7 0x0053;       (* latin capital letter s with caron *)
  Accent 0x02c7 0x0073;       (* latin small letter s with caron *)
  Accent 0x00b8 0x0054;       (* latin capital letter t with cedilla *)
  Accent 0x00b8 0x0074;       (* latin small letter t with cedilla *)
  Accent 0x02c7 0x0054;       (* latin capital letter t with caron *)
  Accent 0x02c7 0x0074;       (* latin small letter t with caron *)
  Undef;                      (* latin capital letter t with stroke *)
  Undef;                      (* latin small letter t with stroke *)
  Accent 0x02dc 0x0055;       (* latin capital letter u with tilde *)
  Accent 0x02dc 0x0075;       (* latin small letter u with tilde *)
  Accent 0x00af 0x0055;       (* latin capital letter u with macron *)
  Accent 0x00af 0x0075;       (* latin small letter u with macron *)
  Accent 0x02d8 0x0055;       (* latin capital letter u with breve *)
  Accent 0x02d8 0x0075;       (* latin small letter u with breve *)
  Accent 0x02da 0x0055;       (* latin capital letter u with ring above *)
  Accent 0x02da 0x0075;       (* latin small letter u with ring above *)
  Accent 0x02dd 0x0055;       (* latin capital letter u with double acute *)
  Accent 0x02dd 0x0075;       (* latin small letter u with double acute *)
  Accent 0x02db 0x0055;       (* latin capital letter u with ogonek *)
  Accent 0x02db 0x0075;       (* latin small letter u with ogonek *)
  Accent 0x005e 0x0057;       (* latin capital letter w with circumflex *)
  Accent 0x005e 0x0077;       (* latin small letter w with circumflex *)
  Accent 0x005e 0x0059;       (* latin capital letter y with circumflex *)
  Accent 0x005e 0x0079;       (* latin small letter y with circumflex *)
  Accent 0x00a8 0x0059;       (* latin capital letter y with diaeresis *)
  Accent 0x00b4 0x005a;       (* latin capital letter z with acute *)
  Accent 0x00b4 0x007a;       (* latin small letter z with acute *)
  Accent 0x02d9 0x005a;       (* latin capital letter z with dot above *)
  Accent 0x02d9 0x007a;       (* latin small letter z with dot above *)
  Accent 0x02c7 0x005a;       (* latin capital letter z with caron *)
  Accent 0x02c7 0x007a;       (* latin small letter z with caron *)
  Undef;                      (* latin small letter long s *)
  Undef;                      (* latin small letter b with stroke *)
  Undef;                      (* latin capital letter b with hook *)
  Undef;                      (* latin capital letter b with topbar *)
  Undef;                      (* latin small letter b with topbar *)
  Undef;                      (* latin capital letter tone six *)
  Undef;                      (* latin small letter tone six *)
  Undef;                      (* latin capital letter open o *)
  Undef;                      (* latin capital letter c with hook *)
  Undef;                      (* latin small letter c with hook *)
  Undef;                      (* latin capital letter african d *)
  Undef;                      (* latin capital letter d with hook *)
  Undef;                      (* latin capital letter d with topbar *)
  Undef;                      (* latin small letter d with topbar *)
  Undef;                      (* latin small letter turned delta *)
  Undef;                      (* latin capital letter reversed e *)
  Undef;                      (* latin capital letter schwa *)
  Undef;                      (* latin capital letter open e *)
  Undef;                      (* latin capital letter f with hook *)
  Undef;                      (* latin small letter f with hook *)
  Undef;                      (* latin capital letter g with hook *)
  Undef;                      (* latin capital letter gamma *)
  Undef;                      (* latin small letter hv *)
  Undef;                      (* latin capital letter iota *)
  Undef;                      (* latin capital letter i with stroke *)
  Undef;                      (* latin capital letter k with hook *)
  Undef;                      (* latin small letter k with hook *)
  Undef;                      (* latin small letter l with bar *)
  Undef;                      (* latin small letter lambda with stroke *)
  Undef;                      (* latin capital letter turned m *)
  Undef;                      (* latin capital letter n with left hook *)
  Undef;                      (* latin small letter n with long right leg *)
  Undef;                      (* latin capital letter o with middle tilde *)
  Undef;                      (* latin capital letter o with horn *)
  Undef;                      (* latin small letter o with horn *)
  Undef;                      (* latin capital letter oi *)
  Undef;                      (* latin small letter oi *)
  Undef;                      (* latin capital letter p with hook *)
  Undef;                      (* latin small letter p with hook *)
  Undef;                      (* latin letter yr *)
  Undef;                      (* latin capital letter tone two *)
  Undef;                      (* latin small letter tone two *)
  Undef;                      (* latin capital letter esh *)
  Undef;                      (* latin letter reversed esh loop *)
  Undef;                      (* latin small letter t with palatal hook *)
  Undef;                      (* latin capital letter t with hook *)
  Undef;                      (* latin small letter t with hook *)
  Undef;                      (* latin capital letter t with retroflex hook *)
  Undef;                      (* latin capital letter u with horn *)
  Undef;                      (* latin small letter u with horn *)
  Undef;                      (* latin capital letter upsilon *)
  Undef;                      (* latin capital letter v with hook *)
  Undef;                      (* latin capital letter y with hook *)
  Undef;                      (* latin small letter y with hook *)
  Undef;                      (* latin capital letter z with stroke *)
  Undef;                      (* latin small letter z with stroke *)
  Undef;                      (* latin capital letter ezh *)
  Undef;                      (* latin capital letter ezh reversed *)
  Undef;                      (* latin small letter ezh reversed *)
  Undef;                      (* latin small letter ezh with tail *)
  Undef;                      (* latin letter two with stroke *)
  Undef;                      (* latin capital letter tone five *)
  Undef;                      (* latin small letter tone five *)
  Undef;                      (* latin letter inverted glottal stop with stroke *)
  Undef;                      (* latin letter wynn *)
  Undef;                      (* latin letter dental click *)
  Undef;                      (* latin letter lateral click *)
  Undef;                      (* latin letter alveolar click *)
  Undef;                      (* latin letter retroflex click *)
  Sequence [0x0044; 0x017d];  (* latin capital letter dz with caron *)
  Sequence [0x0044; 0x017e];  (* latin capital letter d with small letter z with caron *)
  Sequence [0x0064; 0x017e];  (* latin small letter dz with caron *)
  Sequence [0x004c; 0x004a];  (* latin capital letter lj *)
  Sequence [0x004c; 0x006a];  (* latin capital letter l with small letter j *)
  Sequence [0x006c; 0x006a];  (* latin small letter lj *)
  Sequence [0x004e; 0x004a];  (* latin capital letter nj *)
  Sequence [0x004e; 0x006a];  (* latin capital letter n with small letter j *)
  Sequence [0x006e; 0x006a];  (* latin small letter nj *)
  Accent 0x02c7 0x0041;       (* latin capital letter a with caron *)
  Accent 0x02c7 0x0061;       (* latin small letter a with caron *)
  Accent 0x02c7 0x0049;       (* latin capital letter i with caron *)
  Accent 0x02c7 0x0131;       (* latin small letter i with caron *)
  Accent 0x02c7 0x004f;       (* latin capital letter o with caron *)
  Accent 0x02c7 0x006f;       (* latin small letter o with caron *)
  Accent 0x02c7 0x0055;       (* latin capital letter u with caron *)
  Accent 0x02c7 0x0075;       (* latin small letter u with caron *)
  Undef;                      (* latin capital letter u with diaeresis and macron *)
  Undef;                      (* latin small letter u with diaeresis and macron *)
  Undef;                      (* latin capital letter u with diaeresis and acute *)
  Undef;                      (* latin small letter u with diaeresis and acute *)
  Undef;                      (* latin capital letter u with diaeresis and caron *)
  Undef;                      (* latin small letter u with diaeresis and caron *)
  Undef;                      (* latin capital letter u with diaeresis and grave *)
  Undef;                      (* latin small letter u with diaeresis and grave *)
  Undef;                      (* latin small letter turned e *)
  Undef;                      (* latin capital letter a with diaeresis and macron *)
  Undef;                      (* latin small letter a with diaeresis and macron *)
  Undef;                      (* latin capital letter a with dot above and macron *)
  Undef;                      (* latin small letter a with dot above and macron *)
  Accent 0x00af 0x00c6;       (* latin capital letter ae with macron *)
  Accent 0x00af 0x00e6;       (* latin small letter ae with macron *)
  Undef;                      (* latin capital letter g with stroke *)
  Undef;                      (* latin small letter g with stroke *)
  Accent 0x02c7 0x0047;       (* latin capital letter g with caron *)
  Accent 0x02c7 0x0067;       (* latin small letter g with caron *)
  Accent 0x02c7 0x004b;       (* latin capital letter k with caron *)
  Accent 0x02c7 0x006b;       (* latin small letter k with caron *)
  Accent 0x02db 0x004f;       (* latin capital letter o with ogonek *)
  Accent 0x02db 0x006f;       (* latin small letter o with ogonek *)
  Undef;                      (* latin capital letter o with ogonek and macron *)
  Undef;                      (* latin small letter o with ogonek and macron *)
  Undef;                      (* latin capital letter ezh with caron *)
  Undef;                      (* latin small letter ezh with caron *)
  Accent 0x02c7 0x0237;       (* latin small letter j with caron *)
  Undef;                      (* latin capital letter dz *)
  Undef;                      (* latin capital letter d with small letter z *)
  Undef;                      (* latin small letter dz *)
  Accent 0x00b4 0x0047;       (* latin capital letter g with acute *)
  Accent 0x00b4 0x0067;       (* latin small letter g with acute *)
  Undef;                      (* latin capital letter hwair *)
  Undef;                      (* latin capital letter wynn *)
  Accent 0x0060 0x004e;       (* latin capital letter n with grave *)
  Accent 0x0060 0x006e;       (* latin small letter n with grave *)
  Undef;                      (* latin capital letter a with ring above and acute *)
  Undef;                      (* latin small letter a with ring above and acute *)
  Accent 0x00b4 0x00c6;       (* latin capital letter ae with acute *)
  Accent 0x00b4 0x00e6;       (* latin small letter ae with acute *)
  Accent 0x00b4 0x00d8;       (* latin capital letter o with stroke and acute *)
  Accent 0x00b4 0x00f8        (* latin small letter o with stroke and acute *)
|];

value fake_02 =
[|
  Undef;                (* latin capital letter a with double grave *)
  Undef;                (* latin small letter a with double grave *)
  Undef;                (* latin capital letter a with inverted breve *)
  Undef;                (* latin small letter a with inverted breve *)
  Undef;                (* latin capital letter e with double grave *)
  Undef;                (* latin small letter e with double grave *)
  Undef;                (* latin capital letter e with inverted breve *)
  Undef;                (* latin small letter e with inverted breve *)
  Undef;                (* latin capital letter i with double grave *)
  Undef;                (* latin small letter i with double grave *)
  Undef;                (* latin capital letter i with inverted breve *)
  Undef;                (* latin small letter i with inverted breve *)
  Undef;                (* latin capital letter o with double grave *)
  Undef;                (* latin small letter o with double grave *)
  Undef;                (* latin capital letter o with inverted breve *)
  Undef;                (* latin small letter o with inverted breve *)
  Undef;                (* latin capital letter r with double grave *)
  Undef;                (* latin small letter r with double grave *)
  Undef;                (* latin capital letter r with inverted breve *)
  Undef;                (* latin small letter r with inverted breve *)
  Undef;                (* latin capital letter u with double grave *)
  Undef;                (* latin small letter u with double grave *)
  Undef;                (* latin capital letter u with inverted breve *)
  Undef;                (* latin small letter u with inverted breve *)
  Undef;                (* latin capital letter s with comma below *)
  Undef;                (* latin small letter s with comma below *)
  Undef;                (* latin capital letter t with comma below *)
  Undef;                (* latin small letter t with comma below *)
  Undef;                (* latin capital letter yogh *)
  Undef;                (* latin small letter yogh *)
  Accent 0x02c7 0x0048; (* latin capital letter h with caron *)
  Accent 0x02c7 0x0068; (* latin small letter h with caron *)
  Undef;                (* latin capital letter n with long right leg *)
  Undef;                (* latin small letter d with curl *)
  Undef;                (* latin capital letter ou *)
  Undef;                (* latin small letter ou *)
  Undef;                (* latin capital letter z with hook *)
  Undef;                (* latin small letter z with hook *)
  Accent 0x02d9 0x0041; (* latin capital letter a with dot above *)
  Accent 0x02d9 0x0061; (* latin small letter a with dot above *)
  Accent 0x00b8 0x0045; (* latin capital letter e with cedilla *)
  Accent 0x00b8 0x0065; (* latin small letter e with cedilla *)
  Undef;                (* latin capital letter o with diaeresis and macron *)
  Undef;                (* latin small letter o with diaeresis and macron *)
  Undef;                (* latin capital letter o with tilde and macron *)
  Undef;                (* latin small letter o with tilde and macron *)
  Accent 0x02d9 0x004f; (* latin capital letter o with dot above *)
  Accent 0x02d9 0x006f; (* latin small letter o with dot above *)
  Undef;                (* latin capital letter o with dot above and macron *)
  Undef;                (* latin small letter o with dot above and macron *)
  Accent 0x00af 0x0059; (* latin capital letter y with macron *)
  Accent 0x00af 0x0079; (* latin small letter y with macron *)
  Undef;                (* latin small letter l with curl *)
  Undef;                (* latin small letter n with curl *)
  Undef;                (* latin small letter t with curl *)
  Undef;                (* latin small letter dotless j *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;               (* latin small letter turned a *)
  Undef;               (* latin small letter alpha *)
  Undef;               (* latin small letter turned alpha *)
  Undef;               (* latin small letter b with hook *)
  Undef;               (* latin small letter open o *)
  Undef;               (* latin small letter c with curl *)
  Undef;               (* latin small letter d with tail *)
  Undef;               (* latin small letter d with hook *)
  Undef;               (* latin small letter reversed e *)
  Undef;               (* latin small letter schwa *)
  Undef;               (* latin small letter schwa with hook *)
  Undef;               (* latin small letter open e *)
  Undef;               (* latin small letter reversed open e *)
  Undef;               (* latin small letter reversed open e with hook *)
  Undef;               (* latin small letter closed reversed open e *)
  Undef;               (* latin small letter dotless j with stroke *)
  Undef;               (* latin small letter g with hook *)
  Undef;               (* latin small letter script g *)
  Undef;               (* latin letter small capital g *)
  Undef;               (* latin small letter gamma *)
  Undef;               (* latin small letter rams horn *)
  Undef;               (* latin small letter turned h *)
  Undef;               (* latin small letter h with hook *)
  Undef;               (* latin small letter heng with hook *)
  Undef;               (* latin small letter i with stroke *)
  Undef;               (* latin small letter iota *)
  Undef;               (* latin letter small capital i *)
  Undef;               (* latin small letter l with middle tilde *)
  Undef;               (* latin small letter l with belt *)
  Undef;               (* latin small letter l with retroflex hook *)
  Undef;               (* latin small letter lezh *)
  Undef;               (* latin small letter turned m *)
  Undef;               (* latin small letter turned m with long leg *)
  Undef;               (* latin small letter m with hook *)
  Undef;               (* latin small letter n with left hook *)
  Undef;               (* latin small letter n with retroflex hook *)
  Undef;               (* latin letter small capital n *)
  Undef;               (* latin small letter barred o *)
  Undef;               (* latin letter small capital oe *)
  Undef;               (* latin small letter closed omega *)
  Undef;               (* latin small letter phi *)
  Undef;               (* latin small letter turned r *)
  Undef;               (* latin small letter turned r with long leg *)
  Undef;               (* latin small letter turned r with hook *)
  Undef;               (* latin small letter r with long leg *)
  Undef;               (* latin small letter r with tail *)
  Undef;               (* latin small letter r with fishhook *)
  Undef;               (* latin small letter reversed r with fishhook *)
  Undef;               (* latin letter small capital r *)
  Undef;               (* latin letter small capital inverted r *)
  Undef;               (* latin small letter s with hook *)
  Undef;               (* latin small letter esh *)
  Undef;               (* latin small letter dotless j with stroke and hook *)
  Undef;               (* latin small letter squat reversed esh *)
  Undef;               (* latin small letter esh with curl *)
  Undef;               (* latin small letter turned t *)
  Undef;               (* latin small letter t with retroflex hook *)
  Undef;               (* latin small letter u bar *)
  Undef;               (* latin small letter upsilon *)
  Undef;               (* latin small letter v with hook *)
  Undef;               (* latin small letter turned v *)
  Undef;               (* latin small letter turned w *)
  Undef;               (* latin small letter turned y *)
  Undef;               (* latin letter small capital y *)
  Undef;               (* latin small letter z with retroflex hook *)
  Undef;               (* latin small letter z with curl *)
  Undef;               (* latin small letter ezh *)
  Undef;               (* latin small letter ezh with curl *)
  Undef;               (* latin letter glottal stop *)
  Undef;               (* latin letter pharyngeal voiced fricative *)
  Undef;               (* latin letter inverted glottal stop *)
  Undef;               (* latin letter stretched c *)
  Undef;               (* latin letter bilabial click *)
  Undef;               (* latin letter small capital b *)
  Undef;               (* latin small letter closed open e *)
  Undef;               (* latin letter small capital g with hook *)
  Undef;               (* latin letter small capital h *)
  Undef;               (* latin small letter j with crossed-tail *)
  Undef;               (* latin small letter turned k *)
  Undef;               (* latin letter small capital l *)
  Undef;               (* latin small letter q with hook *)
  Undef;               (* latin letter glottal stop with stroke *)
  Undef;               (* latin letter reversed glottal stop with stroke *)
  Undef;               (* latin small letter dz digraph *)
  Undef;               (* latin small letter dezh digraph *)
  Undef;               (* latin small letter dz digraph with curl *)
  Undef;               (* latin small letter ts digraph *)
  Undef;               (* latin small letter tesh digraph *)
  Undef;               (* latin small letter tc digraph with curl *)
  Undef;               (* latin small letter feng digraph *)
  Undef;               (* latin small letter ls digraph *)
  Undef;               (* latin small letter lz digraph *)
  Undef;               (* latin letter bilabial percussive *)
  Undef;               (* latin letter bidental percussive *)
  Undef;               (* latin small letter turned h with fishhook  *)
  Undef;               (* latin small letter turned h with fishhook and tail *)
  Undef;               (* modifier letter small h *)
  Undef;               (* modifier letter small h with hook *)
  Undef;               (* modifier letter small j *)
  Undef;               (* modifier letter small r *)
  Undef;               (* modifier letter small turned r *)
  Undef;               (* modifier letter small turned r with hook *)
  Undef;               (* modifier letter small capital inverted r *)
  Undef;               (* modifier letter small w *)
  Undef;               (* modifier letter small y *)
  Undef;               (* modifier letter prime *)
  Undef;               (* modifier letter double prime *)
  Undef;               (* modifier letter turned comma *)
  Undef;               (* modifier letter apostrophe *)
  Undef;               (* modifier letter reversed comma *)
  Undef;               (* modifier letter right half ring *)
  Undef;               (* modifier letter left half ring *)
  Undef;               (* modifier letter glottal stop *)
  Undef;               (* modifier letter reversed glottal stop *)
  Undef;               (* modifier letter left arrowhead *)
  Undef;               (* modifier letter right arrowhead *)
  Undef;               (* modifier letter up arrowhead *)
  Undef;               (* modifier letter down arrowhead *)
  Undef;               (* modifier letter circumflex accent *)
  Undef;               (* caron *)
  Undef;               (* modifier letter vertical line *)
  Undef;               (* modifier letter macron *)
  Undef;               (* modifier letter acute accent *)
  Undef;               (* modifier letter grave accent *)
  Undef;               (* modifier letter low vertical line *)
  Undef;               (* modifier letter low macron *)
  Undef;               (* modifier letter low grave accent *)
  Undef;               (* modifier letter low acute accent *)
  Undef;               (* modifier letter triangular colon *)
  Undef;               (* modifier letter half triangular colon *)
  Undef;               (* modifier letter centred right half ring *)
  Undef;               (* modifier letter centred left half ring *)
  Undef;               (* modifier letter up tack *)
  Undef;               (* modifier letter down tack *)
  Undef;               (* modifier letter plus sign *)
  Undef;               (* modifier letter minus sign *)
  Undef;               (* breve *)
  Undef;               (* dot above *)
  Undef;               (* ring above *)
  Undef;               (* ogonek *)
  Undef;               (* small tilde *)
  Undef;               (* double acute accent *)
  Undef;               (* modifier letter rhotic hook *)
  Undef;               (* modifier letter cross accent *)
  Undef;               (* modifier letter small gamma *)
  Undef;               (* modifier letter small l *)
  Undef;               (* modifier letter small s *)
  Undef;               (* modifier letter small x *)
  Undef;               (* modifier letter small reversed glottal stop *)
  Undef;               (* modifier letter extra-high tone bar *)
  Undef;               (* modifier letter high tone bar *)
  Undef;               (* modifier letter mid tone bar *)
  Undef;               (* modifier letter low tone bar *)
  Undef;               (* modifier letter extra-low tone bar *)
  Undef;               (* modifier letter yin departing tone mark *)
  Undef;               (* modifier letter yang departing tone mark *)
  Undef;               (* modifier letter voicing *)
  Undef;               (* modifier letter unaspirated *)
  Undef;               (* modifier letter double apostrophe *)
  Undef;               (* modifier letter low down arrowhead *)
  Undef;               (* modifier letter low up arrowhead *)
  Undef;               (* modifier letter low left arrowhead *)
  Undef;               (* modifier letter low right arrowhead *)
  Undef;               (* modifier letter low ring *)
  Undef;               (* modifier letter middle grave accent *)
  Undef;               (* modifier letter middle double grave accent *)
  Undef;               (* modifier letter middle double acute accent *)
  Undef;               (* modifier letter low tilde *)
  Undef;               (* modifier letter raised colon *)
  Undef;               (* modifier letter begin high tone *)
  Undef;               (* modifier letter end high tone *)
  Undef;               (* modifier letter begin low tone *)
  Undef;               (* modifier letter end low tone *)
  Undef;               (* modifier letter shelf *)
  Undef;               (* modifier letter open shelf *)
  Undef                (* modifier letter low left arrow *)
|];

value fake_03 =
[|
  Undef;       (* combining grave accent *)
  Undef;       (* combining acute accent *)
  Undef;       (* combining circumflex accent *)
  Undef;       (* combining tilde *)
  Undef;       (* combining macron *)
  Undef;       (* combining overline *)
  Undef;       (* combining breve *)
  Undef;       (* combining dot above *)
  Undef;       (* combining diaeresis *)
  Undef;       (* combining hook above *)
  Undef;       (* combining ring above *)
  Undef;       (* combining double acute accent *)
  Undef;       (* combining caron *)
  Undef;       (* combining vertical line above *)
  Undef;       (* combining double vertical line above *)
  Undef;       (* combining double grave accent *)
  Undef;       (* combining candrabindu *)
  Undef;       (* combining inverted breve *)
  Undef;       (* combining turned comma above *)
  Undef;       (* combining comma above *)
  Undef;       (* combining reversed comma above *)
  Undef;       (* combining comma above right *)
  Undef;       (* combining grave accent below *)
  Undef;       (* combining acute accent below *)
  Undef;       (* combining left tack below *)
  Undef;       (* combining right tack below *)
  Undef;       (* combining left angle above *)
  Undef;       (* combining horn *)
  Undef;       (* combining left half ring below *)
  Undef;       (* combining up tack below *)
  Undef;       (* combining down tack below *)
  Undef;       (* combining plus sign below *)
  Undef;       (* combining minus sign below *)
  Undef;       (* combining palatalized hook below *)
  Undef;       (* combining retroflex hook below *)
  Undef;       (* combining dot below *)
  Undef;       (* combining diaeresis below *)
  Undef;       (* combining ring below *)
  Undef;       (* combining comma below *)
  Undef;       (* combining cedilla *)
  Undef;       (* combining ogonek *)
  Undef;       (* combining vertical line below *)
  Undef;       (* combining bridge below *)
  Undef;       (* combining inverted double arch below *)
  Undef;       (* combining caron below *)
  Undef;       (* combining circumflex accent below *)
  Undef;       (* combining breve below *)
  Undef;       (* combining inverted breve below *)
  Undef;       (* combining tilde below *)
  Undef;       (* combining macron below *)
  Undef;       (* combining low line *)
  Undef;       (* combining double low line *)
  Undef;       (* combining tilde overlay *)
  Undef;       (* combining short stroke overlay *)
  Undef;       (* combining long stroke overlay *)
  Undef;       (* combining short solidus overlay *)
  Undef;       (* combining long solidus overlay *)
  Undef;       (* combining right half ring below *)
  Undef;       (* combining inverted bridge below *)
  Undef;       (* combining square below *)
  Undef;       (* combining seagull below *)
  Undef;       (* combining x above *)
  Undef;       (* combining vertical tilde *)
  Undef;       (* combining double overline *)
  Undef;       (* combining grave tone mark *)
  Undef;       (* combining acute tone mark *)
  Undef;       (* combining greek perispomeni *)
  Undef;       (* combining greek koronis *)
  Undef;       (* combining greek dialytika tonos *)
  Undef;       (* combining greek ypogegrammeni *)
  Undef;       (* combining bridge above *)
  Undef;       (* combining equals sign below *)
  Undef;       (* combining double vertical line below *)
  Undef;       (* combining left angle below *)
  Undef;       (* combining not tilde above *)
  Undef;       (* combining homothetic above *)
  Undef;       (* combining almost equal to above *)
  Undef;       (* combining left right arrow below *)
  Undef;       (* combining upwards arrow below *)
  Undef;       (* combining grapheme joiner *)
  Undef;       (* combining right arrowhead above *)
  Undef;       (* combining left half ring above *)
  Undef;       (* combining fermata *)
  Undef;       (* combining x below *)
  Undef;       (* combining left arrowhead below *)
  Undef;       (* combining right arrowhead below *)
  Undef;       (* combining right arrowhead and up arrowhead below *)
  Undef;       (* combining right half ring above *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;       (* combining double breve *)
  Undef;       (* combining double macron *)
  Undef;       (* combining double macron below *)
  Undef;       (* combining double tilde *)
  Undef;       (* combining double inverted breve *)
  Undef;       (* combining double rightwards arrow below *)
  Undef;       (* combining latin small letter a *)
  Undef;       (* combining latin small letter e *)
  Undef;       (* combining latin small letter i *)
  Undef;       (* combining latin small letter o *)
  Undef;       (* combining latin small letter u *)
  Undef;       (* combining latin small letter c *)
  Undef;       (* combining latin small letter d *)
  Undef;       (* combining latin small letter h *)
  Undef;       (* combining latin small letter m *)
  Undef;       (* combining latin small letter r *)
  Undef;       (* combining latin small letter t *)
  Undef;       (* combining latin small letter v *)
  Undef;       (* combining latin small letter x *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;                     (* greek numeral sign *)
  Undef;                     (* greek lower numeral sign *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;                     (* greek ypogegrammeni *)
  Undef;
  Undef;
  Undef;
  Simple 0x003b;             (* greek question mark -> semicolon*)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;                     (* greek tonos *)
  Undef;                     (* greek dialytika tonos *)
  Sequence [0x0384; 0x0391]; (* greek capital letter alpha with tonos *)
  Simple 0x00b7;             (* greek ano teleia -> middle dot *)
  Sequence [0x0384; 0x0395]; (* greek capital letter epsilon with tonos *)
  Sequence [0x0384; 0x0397]; (* greek capital letter eta with tonos *)
  Sequence [0x0384; 0x0399]; (* greek capital letter iota with tonos *)
  Undef;
  Sequence [0x0384; 0x039f]; (* greek capital letter omicron with tonos *)
  Undef;
  Sequence [0x0384; 0x03a5]; (* greek capital letter upsilon with tonos *)
  Sequence [0x0384; 0x03a9]; (* greek capital letter omega with tonos *)
  Accent 0x0385 0x03b9;      (* greek small letter iota with dialytika and tonos *)
  Simple 0x0041;             (* greek capital letter alpha *)
  Simple 0x0042;             (* greek capital letter beta *)
  Undef;                     (* greek capital letter gamma *)
  Undef;                     (* greek capital letter delta *)
  Simple 0x0045;             (* greek capital letter epsilon *)
  Simple 0x005a;             (* greek capital letter zeta *)
  Simple 0x0048;             (* greek capital letter eta *)
  Undef;                     (* greek capital letter theta *)
  Simple 0x0049;             (* greek capital letter iota *)
  Simple 0x004b;             (* greek capital letter kappa *)
  Undef;                     (* greek capital letter lamda *)
  Simple 0x004d;             (* greek capital letter mu *)
  Simple 0x004e;             (* greek capital letter nu *)
  Undef;                     (* greek capital letter xi *)
  Simple 0x004f;             (* greek capital letter omicron *)
  Undef;                     (* greek capital letter pi *)
  Simple 0x0050;             (* greek capital letter rho *)
  Undef;
  Undef;                     (* greek capital letter sigma *)
  Simple 0x0054;             (* greek capital letter tau *)
  Simple 0x0059;             (* greek capital letter upsilon *)
  Undef;                     (* greek capital letter phi *)
  Simple 0x0058;             (* greek capital letter chi *)
  Undef;                     (* greek capital letter psi *)
  Undef;                     (* greek capital letter omega *)
  Simple 0x00cf;             (* greek capital letter iota with dialytika *)
  Simple 0x0178;             (* greek capital letter upsilon with dialytika *)
  Accent 0x0384 0x03b1;      (* greek small letter alpha with tonos *)
  Accent 0x0384 0x03b5;      (* greek small letter epsilon with tonos *)
  Accent 0x0384 0x03b7;      (* greek small letter eta with tonos *)
  Accent 0x0384 0x03b9;      (* greek small letter iota with tonos *)
  Accent 0x0385 0x03c5;      (* greek small letter upsilon with dialytika and tonos *)
  Undef;                     (* greek small letter alpha *)
  Undef;                     (* greek small letter beta *)
  Undef;                     (* greek small letter gamma *)
  Undef;                     (* greek small letter delta *)
  Undef;                     (* greek small letter epsilon *)
  Undef;                     (* greek small letter zeta *)
  Undef;                     (* greek small letter eta *)
  Undef;                     (* greek small letter theta *)
  Undef;                     (* greek small letter iota *)
  Undef;                     (* greek small letter kappa *)
  Undef;                     (* greek small letter lamda *)
  Simple 0x00b5;             (* greek small letter mu -> micro *)
  Undef;                     (* greek small letter nu *)
  Undef;                     (* greek small letter xi *)
  Simple 0x006f;             (* greek small letter omicron -> latin small letter o *)
  Undef;                     (* greek small letter pi *)
  Undef;                     (* greek small letter rho *)
  Undef;                     (* greek small letter final sigma *)
  Undef;                     (* greek small letter sigma *)
  Undef;                     (* greek small letter tau *)
  Undef;                     (* greek small letter upsilon *)
  Undef;                     (* greek small letter phi *)
  Undef;                     (* greek small letter chi *)
  Undef;                     (* greek small letter psi *)
  Undef;                     (* greek small letter omega *)
  Accent 0x00a8 0x03b9;      (* greek small letter iota with dialytika *)
  Accent 0x00a8 0x03c5;      (* greek small letter upsilon with dialytika *)
  Accent 0x0384 0x03bf;      (* greek small letter omicron with tonos *)
  Accent 0x0384 0x03c5;      (* greek small letter upsilon with tonos *)
  Accent 0x0384 0x03c9;      (* greek small letter omega with tonos *)
  Undef;
  Undef;                     (* greek beta symbol *)
  Undef;                     (* greek theta symbol *)
  Undef;                     (* greek upsilon with hook symbol *)
  Undef;                     (* greek upsilon with acute and hook symbol *)
  Undef;                     (* greek upsilon with diaeresis and hook symbol *)
  Undef;                     (* greek phi symbol *)
  Undef;                     (* greek pi symbol *)
  Undef;                     (* greek kai symbol *)
  Undef;                     (* greek letter archaic koppa *)
  Undef;                     (* greek small letter archaic koppa *)
  Undef;                     (* greek letter stigma *)
  Undef;                     (* greek small letter stigma *)
  Undef;                     (* greek letter digamma *)
  Undef;                     (* greek small letter digamma *)
  Undef;                     (* greek letter koppa *)
  Undef;                     (* greek small letter koppa *)
  Undef;                     (* greek letter sampi *)
  Undef;                     (* greek small letter sampi *)
  Undef;                     (* coptic capital letter shei *)
  Undef;                     (* coptic small letter shei *)
  Undef;                     (* coptic capital letter fei *)
  Undef;                     (* coptic small letter fei *)
  Undef;                     (* coptic capital letter khei *)
  Undef;                     (* coptic small letter khei *)
  Undef;                     (* coptic capital letter hori *)
  Undef;                     (* coptic small letter hori *)
  Undef;                     (* coptic capital letter gangia *)
  Undef;                     (* coptic small letter gangia *)
  Undef;                     (* coptic capital letter shima *)
  Undef;                     (* coptic small letter shima *)
  Undef;                     (* coptic capital letter dei *)
  Undef;                     (* coptic small letter dei *)
  Undef;                     (* greek kappa symbol *)
  Undef;                     (* greek rho symbol *)
  Undef;                     (* greek lunate sigma symbol *)
  Undef;                     (* greek letter yot *)
  Undef;                     (* greek capital theta symbol *)
  Undef;                     (* greek lunate epsilon symbol *)
  Undef;                     (* greek reversed lunate epsilon symbol *)
  Undef;                     (* greek capital letter sho *)
  Undef;                     (* greek small letter sho *)
  Undef;                     (* greek capital lunate sigma symbol *)
  Undef;                     (* greek capital letter san *)
  Undef;                     (* greek small letter san *)
  Undef;
  Undef;
  Undef;
  Undef
|];

value fake_1d =
[|
  Undef;       (* latin letter small capital a *)
  Undef;       (* latin letter small capital ae *)
  Undef;       (* latin small letter turned ae *)
  Undef;       (* latin letter small capital barred b *)
  Undef;       (* latin letter small capital c *)
  Undef;       (* latin letter small capital d *)
  Undef;       (* latin letter small capital eth *)
  Undef;       (* latin letter small capital e *)
  Undef;       (* latin small letter turned open e *)
  Undef;       (* latin small letter turned i *)
  Undef;       (* latin letter small capital j *)
  Undef;       (* latin letter small capital k *)
  Undef;       (* latin letter small capital l with stroke *)
  Undef;       (* latin letter small capital m *)
  Undef;       (* latin letter small capital reversed n *)
  Undef;       (* latin letter small capital o *)
  Undef;       (* latin letter small capital open o *)
  Undef;       (* latin small letter sideways o *)
  Undef;       (* latin small letter sideways open o *)
  Undef;       (* latin small letter sideways o with stroke *)
  Undef;       (* latin small letter turned oe *)
  Undef;       (* latin letter small capital ou *)
  Undef;       (* latin small letter top half o *)
  Undef;       (* latin small letter bottom half o *)
  Undef;       (* latin letter small capital p *)
  Undef;       (* latin letter small capital reversed r *)
  Undef;       (* latin letter small capital turned r *)
  Undef;       (* latin letter small capital t *)
  Undef;       (* latin letter small capital u *)
  Undef;       (* latin small letter sideways u *)
  Undef;       (* latin small letter sideways diaeresized u *)
  Undef;       (* latin small letter sideways turned m *)
  Undef;       (* latin letter small capital v *)
  Undef;       (* latin letter small capital w *)
  Undef;       (* latin letter small capital z *)
  Undef;       (* latin letter small capital ezh *)
  Undef;       (* latin letter voiced laryngeal spirant *)
  Undef;       (* latin letter ain *)
  Undef;       (* greek letter small capital gamma *)
  Undef;       (* greek letter small capital lamda *)
  Undef;       (* greek letter small capital pi *)
  Undef;       (* greek letter small capital rho *)
  Undef;       (* greek letter small capital psi *)
  Undef;       (* cyrillic letter small capital el *)
  Undef;       (* modifier letter capital a *)
  Undef;       (* modifier letter capital ae *)
  Undef;       (* modifier letter capital b *)
  Undef;       (* modifier letter capital barred b *)
  Undef;       (* modifier letter capital d *)
  Undef;       (* modifier letter capital e *)
  Undef;       (* modifier letter capital reversed e *)
  Undef;       (* modifier letter capital g *)
  Undef;       (* modifier letter capital h *)
  Undef;       (* modifier letter capital i *)
  Undef;       (* modifier letter capital j *)
  Undef;       (* modifier letter capital k *)
  Undef;       (* modifier letter capital l *)
  Undef;       (* modifier letter capital m *)
  Undef;       (* modifier letter capital n *)
  Undef;       (* modifier letter capital reversed n *)
  Undef;       (* modifier letter capital o *)
  Undef;       (* modifier letter capital ou *)
  Undef;       (* modifier letter capital p *)
  Undef;       (* modifier letter capital r *)
  Undef;       (* modifier letter capital t *)
  Undef;       (* modifier letter capital u *)
  Undef;       (* modifier letter capital w *)
  Undef;       (* modifier letter small a *)
  Undef;       (* modifier letter small turned a *)
  Undef;       (* modifier letter small alpha *)
  Undef;       (* modifier letter small turned ae *)
  Undef;       (* modifier letter small b *)
  Undef;       (* modifier letter small d *)
  Undef;       (* modifier letter small e *)
  Undef;       (* modifier letter small schwa *)
  Undef;       (* modifier letter small open e *)
  Undef;       (* modifier letter small turned open e *)
  Undef;       (* modifier letter small g *)
  Undef;       (* modifier letter small turned i *)
  Undef;       (* modifier letter small k *)
  Undef;       (* modifier letter small m *)
  Undef;       (* modifier letter small eng *)
  Undef;       (* modifier letter small o *)
  Undef;       (* modifier letter small open o *)
  Undef;       (* modifier letter small top half o *)
  Undef;       (* modifier letter small bottom half o *)
  Undef;       (* modifier letter small p *)
  Undef;       (* modifier letter small t *)
  Undef;       (* modifier letter small u *)
  Undef;       (* modifier letter small sideways u *)
  Undef;       (* modifier letter small turned m *)
  Undef;       (* modifier letter small v *)
  Undef;       (* modifier letter small ain *)
  Undef;       (* modifier letter small beta *)
  Undef;       (* modifier letter small greek gamma *)
  Undef;       (* modifier letter small delta *)
  Undef;       (* modifier letter small greek phi *)
  Undef;       (* modifier letter small chi *)
  Undef;       (* latin subscript small letter i *)
  Undef;       (* latin subscript small letter r *)
  Undef;       (* latin subscript small letter u *)
  Undef;       (* latin subscript small letter v *)
  Undef;       (* greek subscript small letter beta *)
  Undef;       (* greek subscript small letter gamma *)
  Undef;       (* greek subscript small letter rho *)
  Undef;       (* greek subscript small letter phi *)
  Undef;       (* greek subscript small letter chi *)
  Undef;       (* latin small letter ue *)
  Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value fake_1e =
[|
  Undef;                (* latin capital letter a with ring below *)
  Undef;                (* latin small letter a with ring below *)
  Accent 0x02d9 0x0042; (* latin capital letter b with dot above *)
  Accent 0x02d9 0x0062; (* latin small letter b with dot above *)
  Undef;                (* latin capital letter b with dot below *)
  Undef;                (* latin small letter b with dot below *)
  Undef;                (* latin capital letter b with line below *)
  Undef;                (* latin small letter b with line below *)
  Undef;                (* latin capital letter c with cedilla and acute *)
  Undef;                (* latin small letter c with cedilla and acute *)
  Accent 0x02d9 0x0044; (* latin capital letter d with dot above *)
  Accent 0x02d9 0x0064; (* latin small letter d with dot above *)
  Undef;                (* latin capital letter d with dot below *)
  Undef;                (* latin small letter d with dot below *)
  Undef;                (* latin capital letter d with line below *)
  Undef;                (* latin small letter d with line below *)
  Accent 0x00b8 0x0044; (* latin capital letter d with cedilla *)
  Accent 0x00b8 0x0064; (* latin small letter d with cedilla *)
  Undef;                (* latin capital letter d with circumflex below *)
  Undef;                (* latin small letter d with circumflex below *)
  Undef;                (* latin capital letter e with macron and grave *)
  Undef;                (* latin small letter e with macron and grave *)
  Undef;                (* latin capital letter e with macron and acute *)
  Undef;                (* latin small letter e with macron and acute *)
  Undef;                (* latin capital letter e with circumflex below *)
  Undef;                (* latin small letter e with circumflex below *)
  Undef;                (* latin capital letter e with tilde below *)
  Undef;                (* latin small letter e with tilde below *)
  Undef;                (* latin capital letter e with cedilla and breve *)
  Undef;                (* latin small letter e with cedilla and breve *)
  Accent 0x02d9 0x0046; (* latin capital letter f with dot above *)
  Accent 0x02d9 0x0066; (* latin small letter f with dot above *)
  Accent 0x00af 0x0047; (* latin capital letter g with macron *)
  Accent 0x00af 0x0067; (* latin small letter g with macron *)
  Accent 0x02d9 0x0048; (* latin capital letter h with dot above *)
  Accent 0x02d9 0x0068; (* latin small letter h with dot above *)
  Undef;                (* latin capital letter h with dot below *)
  Undef;                (* latin small letter h with dot below *)
  Accent 0x00a8 0x0048; (* latin capital letter h with diaeresis *)
  Accent 0x00a8 0x0068; (* latin small letter h with diaeresis *)
  Accent 0x00b8 0x0048; (* latin capital letter h with cedilla *)
  Accent 0x00b8 0x0068; (* latin small letter h with cedilla *)
  Undef;                (* latin capital letter h with breve below *)
  Undef;                (* latin small letter h with breve below *)
  Undef;                (* latin capital letter i with tilde below *)
  Undef;                (* latin small letter i with tilde below *)
  Undef;                (* latin capital letter i with diaeresis and acute *)
  Undef;                (* latin small letter i with diaeresis and acute *)
  Accent 0x00b4 0x004b; (* latin capital letter k with acute *)
  Accent 0x00b4 0x006b; (* latin small letter k with acute *)
  Undef;                (* latin capital letter k with dot below *)
  Undef;                (* latin small letter k with dot below *)
  Undef;                (* latin capital letter k with line below *)
  Undef;                (* latin small letter k with line below *)
  Undef;                (* latin capital letter l with dot below *)
  Undef;                (* latin small letter l with dot below *)
  Undef;                (* latin capital letter l with dot below and macron *)
  Undef;                (* latin small letter l with dot below and macron *)
  Undef;                (* latin capital letter l with line below *)
  Undef;                (* latin small letter l with line below *)
  Undef;                (* latin capital letter l with circumflex below *)
  Undef;                (* latin small letter l with circumflex below *)
  Accent 0x00b4 0x004d; (* latin capital letter m with acute *)
  Accent 0x00b4 0x006d; (* latin small letter m with acute *)
  Accent 0x02d9 0x004d; (* latin capital letter m with dot above *)
  Accent 0x02d9 0x006d; (* latin small letter m with dot above *)
  Undef;                (* latin capital letter m with dot below *)
  Undef;                (* latin small letter m with dot below *)
  Accent 0x02d9 0x004e; (* latin capital letter n with dot above *)
  Accent 0x02d9 0x006e; (* latin small letter n with dot above *)
  Undef;                (* latin capital letter n with dot below *)
  Undef;                (* latin small letter n with dot below *)
  Undef;                (* latin capital letter n with line below *)
  Undef;                (* latin small letter n with line below *)
  Undef;                (* latin capital letter n with circumflex below *)
  Undef;                (* latin small letter n with circumflex below *)
  Undef;                (* latin capital letter o with tilde and acute *)
  Undef;                (* latin small letter o with tilde and acute *)
  Undef;                (* latin capital letter o with tilde and diaeresis *)
  Undef;                (* latin small letter o with tilde and diaeresis *)
  Undef;                (* latin capital letter o with macron and grave *)
  Undef;                (* latin small letter o with macron and grave *)
  Undef;                (* latin capital letter o with macron and acute *)
  Undef;                (* latin small letter o with macron and acute *)
  Accent 0x00b4 0x0050; (* latin capital letter p with acute *)
  Accent 0x00b4 0x0070; (* latin small letter p with acute *)
  Accent 0x02d9 0x0050; (* latin capital letter p with dot above *)
  Accent 0x02d9 0x0070; (* latin small letter p with dot above *)
  Accent 0x02d9 0x0052; (* latin capital letter r with dot above *)
  Accent 0x02d9 0x0072; (* latin small letter r with dot above *)
  Undef;                (* latin capital letter r with dot below *)
  Undef;                (* latin small letter r with dot below *)
  Undef;                (* latin capital letter r with dot below and macron *)
  Undef;                (* latin small letter r with dot below and macron *)
  Undef;                (* latin capital letter r with line below *)
  Undef;                (* latin small letter r with line below *)
  Accent 0x02d9 0x0053; (* latin capital letter s with dot above *)
  Accent 0x02d9 0x0073; (* latin small letter s with dot above *)
  Undef;                (* latin capital letter s with dot below *)
  Undef;                (* latin small letter s with dot below *)
  Undef;                (* latin capital letter s with acute and dot above *)
  Undef;                (* latin small letter s with acute and dot above *)
  Undef;                (* latin capital letter s with caron and dot above *)
  Undef;                (* latin small letter s with caron and dot above *)
  Undef;                (* latin capital letter s with dot below and dot above *)
  Undef;                (* latin small letter s with dot below and dot above *)
  Accent 0x02d9 0x0054; (* latin capital letter t with dot above *)
  Accent 0x02d9 0x0074; (* latin small letter t with dot above *)
  Undef;                (* latin capital letter t with dot below *)
  Undef;                (* latin small letter t with dot below *)
  Undef;                (* latin capital letter t with line below *)
  Undef;                (* latin small letter t with line below *)
  Undef;                (* latin capital letter t with circumflex below *)
  Undef;                (* latin small letter t with circumflex below *)
  Undef;                (* latin capital letter u with diaeresis below *)
  Undef;                (* latin small letter u with diaeresis below *)
  Undef;                (* latin capital letter u with tilde below *)
  Undef;                (* latin small letter u with tilde below *)
  Undef;                (* latin capital letter u with circumflex below *)
  Undef;                (* latin small letter u with circumflex below *)
  Undef;                (* latin capital letter u with tilde and acute *)
  Undef;                (* latin small letter u with tilde and acute *)
  Undef;                (* latin capital letter u with macron and diaeresis *)
  Undef;                (* latin small letter u with macron and diaeresis *)
  Accent 0x02dc 0x0056; (* latin capital letter v with tilde *)
  Accent 0x02dc 0x0076; (* latin small letter v with tilde *)
  Undef;                (* latin capital letter v with dot below *)
  Undef;                (* latin small letter v with dot below *)
  Accent 0x0060 0x0057; (* latin capital letter w with grave *)
  Accent 0x0060 0x0077; (* latin small letter w with grave *)
  Accent 0x00b4 0x0057; (* latin capital letter w with acute *)
  Accent 0x00b4 0x0077; (* latin small letter w with acute *)
  Accent 0x00a8 0x0057; (* latin capital letter w with diaeresis *)
  Accent 0x00a8 0x0077; (* latin small letter w with diaeresis *)
  Accent 0x02d9 0x0057; (* latin capital letter w with dot above *)
  Accent 0x02d9 0x0077; (* latin small letter w with dot above *)
  Undef;                (* latin capital letter w with dot below *)
  Undef;                (* latin small letter w with dot below *)
  Accent 0x02d9 0x0058; (* latin capital letter x with dot above *)
  Accent 0x02d9 0x0078; (* latin small letter x with dot above *)
  Accent 0x00a8 0x0058; (* latin capital letter x with diaeresis *)
  Accent 0x00a8 0x0078; (* latin small letter x with diaeresis *)
  Accent 0x02d9 0x0059; (* latin capital letter y with dot above *)
  Accent 0x02d9 0x0079; (* latin small letter y with dot above *)
  Accent 0x005e 0x005a; (* latin capital letter z with circumflex *)
  Accent 0x005e 0x007a; (* latin small letter z with circumflex *)
  Undef;                (* latin capital letter z with dot below *)
  Undef;                (* latin small letter z with dot below *)
  Undef;                (* latin capital letter z with line below *)
  Undef;                (* latin small letter z with line below *)
  Undef;                (* latin small letter h with line below *)
  Accent 0x00a8 0x0074; (* latin small letter t with diaeresis *)
  Accent 0x02da 0x0077; (* latin small letter w with ring above *)
  Accent 0x02da 0x0079; (* latin small letter y with ring above *)
  Undef;                (* latin small letter a with right half ring *)
  Accent 0x02d9 0x017f; (* latin small letter long s with dot above *)
  Undef; Undef; Undef; Undef;
  Undef;                (* latin capital letter a with dot below *)
  Undef;                (* latin small letter a with dot below *)
  Undef;                (* latin capital letter a with hook above *)
  Undef;                (* latin small letter a with hook above *)
  Undef;                (* latin capital letter a with circumflex and acute *)
  Undef;                (* latin small letter a with circumflex and acute *)
  Undef;                (* latin capital letter a with circumflex and grave *)
  Undef;                (* latin small letter a with circumflex and grave *)
  Undef;                (* latin capital letter a with circumflex and hook above *)
  Undef;                (* latin small letter a with circumflex and hook above *)
  Undef;                (* latin capital letter a with circumflex and tilde *)
  Undef;                (* latin small letter a with circumflex and tilde *)
  Undef;                (* latin capital letter a with circumflex and dot below *)
  Undef;                (* latin small letter a with circumflex and dot below *)
  Undef;                (* latin capital letter a with breve and acute *)
  Undef;                (* latin small letter a with breve and acute *)
  Undef;                (* latin capital letter a with breve and grave *)
  Undef;                (* latin small letter a with breve and grave *)
  Undef;                (* latin capital letter a with breve and hook above *)
  Undef;                (* latin small letter a with breve and hook above *)
  Undef;                (* latin capital letter a with breve and tilde *)
  Undef;                (* latin small letter a with breve and tilde *)
  Undef;                (* latin capital letter a with breve and dot below *)
  Undef;                (* latin small letter a with breve and dot below *)
  Undef;                (* latin capital letter e with dot below *)
  Undef;                (* latin small letter e with dot below *)
  Undef;                (* latin capital letter e with hook above *)
  Undef;                (* latin small letter e with hook above *)
  Accent 0x02dc 0x0045; (* latin capital letter e with tilde *)
  Accent 0x02dc 0x0065; (* latin small letter e with tilde *)
  Undef;                (* latin capital letter e with circumflex and acute *)
  Undef;                (* latin small letter e with circumflex and acute *)
  Undef;                (* latin capital letter e with circumflex and grave *)
  Undef;                (* latin small letter e with circumflex and grave *)
  Undef;                (* latin capital letter e with circumflex and hook above *)
  Undef;                (* latin small letter e with circumflex and hook above *)
  Undef;                (* latin capital letter e with circumflex and tilde *)
  Undef;                (* latin small letter e with circumflex and tilde *)
  Undef;                (* latin capital letter e with circumflex and dot below *)
  Undef;                (* latin small letter e with circumflex and dot below *)
  Undef;                (* latin capital letter i with hook above *)
  Undef;                (* latin small letter i with hook above *)
  Undef;                (* latin capital letter i with dot below *)
  Undef;                (* latin small letter i with dot below *)
  Undef;                (* latin capital letter o with dot below *)
  Undef;                (* latin small letter o with dot below *)
  Undef;                (* latin capital letter o with hook above *)
  Undef;                (* latin small letter o with hook above *)
  Undef;                (* latin capital letter o with circumflex and acute *)
  Undef;                (* latin small letter o with circumflex and acute *)
  Undef;                (* latin capital letter o with circumflex and grave *)
  Undef;                (* latin small letter o with circumflex and grave *)
  Undef;                (* latin capital letter o with circumflex and hook above *)
  Undef;                (* latin small letter o with circumflex and hook above *)
  Undef;                (* latin capital letter o with circumflex and tilde *)
  Undef;                (* latin small letter o with circumflex and tilde *)
  Undef;                (* latin capital letter o with circumflex and dot below *)
  Undef;                (* latin small letter o with circumflex and dot below *)
  Undef;                (* latin capital letter o with horn and acute *)
  Undef;                (* latin small letter o with horn and acute *)
  Undef;                (* latin capital letter o with horn and grave *)
  Undef;                (* latin small letter o with horn and grave *)
  Undef;                (* latin capital letter o with horn and hook above *)
  Undef;                (* latin small letter o with horn and hook above *)
  Undef;                (* latin capital letter o with horn and tilde *)
  Undef;                (* latin small letter o with horn and tilde *)
  Undef;                (* latin capital letter o with horn and dot below *)
  Undef;                (* latin small letter o with horn and dot below *)
  Undef;                (* latin capital letter u with dot below *)
  Undef;                (* latin small letter u with dot below *)
  Undef;                (* latin capital letter u with hook above *)
  Undef;                (* latin small letter u with hook above *)
  Undef;                (* latin capital letter u with horn and acute *)
  Undef;                (* latin small letter u with horn and acute *)
  Undef;                (* latin capital letter u with horn and grave *)
  Undef;                (* latin small letter u with horn and grave *)
  Undef;                (* latin capital letter u with horn and hook above *)
  Undef;                (* latin small letter u with horn and hook above *)
  Undef;                (* latin capital letter u with horn and tilde *)
  Undef;                (* latin small letter u with horn and tilde *)
  Undef;                (* latin capital letter u with horn and dot below *)
  Undef;                (* latin small letter u with horn and dot below *)
  Accent 0x0060 0x0059; (* latin capital letter y with grave *)
  Accent 0x0060 0x0079; (* latin small letter y with grave *)
  Undef;                (* latin capital letter y with dot below *)
  Undef;                (* latin small letter y with dot below *)
  Undef;                (* latin capital letter y with hook above *)
  Undef;                (* latin small letter y with hook above *)
  Accent 0x02dc 0x0059; (* latin capital letter y with tilde *)
  Accent 0x02dc 0x0079; (* latin small letter y with tilde *)
  Undef; Undef; Undef; Undef; Undef; Undef
|];

value fake_20 =
[|
  Undef;         (* en quad *)
  Undef;         (* em quad *)
  Undef;         (* en space *)
  Undef;         (* em space *)
  Undef;         (* three-per-em space *)
  Undef;         (* four-per-em space *)
  Undef;         (* six-per-em space *)
  Undef;         (* figure space *)
  Undef;         (* punctuation space *)
  Undef;         (* thin space *)
  Undef;         (* hair space *)
  Undef;         (* zero width space *)
  Undef;         (* zero width non-joiner *)
  Undef;         (* zero width joiner *)
  Undef;         (* left-to-right mark *)
  Undef;         (* right-to-left mark *)
  Simple 0x002d; (* hyphen -> minus-hyphen *)
  Simple 0x002d; (* non-breaking hyphen -> minus-hyphen *)
  Undef;         (* figure dash *)
  Undef;         (* en dash *)
  Undef;         (* em dash *)
  Undef;         (* horizontal bar *)
  Undef;         (* double vertical line *)
  Undef;         (* double low line *)
  Undef;         (* left single quotation mark *)
  Simple 0x0027; (* right single quotation mark -> apostrophe *)
  Simple 0x002c; (* single low-9 quotation mark -> comma *)
  Undef;                     (* single high-reversed-9 quotation mark *)
  Sequence [0x2018; 0x2018]; (* left double quotation mark *)
  Sequence [0x2019; 0x2019]; (* right double quotation mark *)
  Sequence [0x201a; 0x201a]; (* double low-9 quotation mark *)
  Sequence [0x201b; 0x201b]; (* double high-reversed-9 quotation mark *)
  Undef;                     (* dagger *)
  Undef;                     (* double dagger *)
  Undef;                     (* bullet *)
  Undef;                     (* triangular bullet *)
  Simple 0x002e;             (* one dot leader -> full stop *)
  Sequence [0x002e; 0x002e]; (* two dot leader *)
  Sequence [0x002e; 0x002e; 0x002e]; (* horizontal ellipsis *)
  Undef;       (* hyphenation point *)
  Undef;       (* line separator *)
  Undef;       (* paragraph separator *)
  Undef;       (* left-to-right embedding *)
  Undef;       (* right-to-left embedding *)
  Undef;       (* pop directional formatting *)
  Undef;       (* left-to-right override *)
  Undef;       (* right-to-left override *)
  Undef;       (* narrow no-break space *)
  Undef;       (* per mille sign *)
  Undef;       (* per ten thousand sign *)
  Undef;       (* prime *)
  Sequence [0x2032; 0x2032];         (* double prime *)
  Sequence [0x2032; 0x2032; 0x2032]; (* triple prime *)
  Undef;                             (* reversed prime *)
  Sequence [0x2035; 0x2035];         (* reversed double prime *)
  Sequence [0x2035; 0x2035; 0x2035]; (* reversed triple prime *)
  Undef;                             (* caret *)
  Undef;                             (* single left-pointing angle quotation mark *)
  Undef;                             (* single right-pointing angle quotation mark *)
  Undef;                             (* reference mark *)
  Sequence [0x0021; 0x0021];         (* double exclamation mark *)
  Undef;                             (* interrobang *)
  Undef;                             (* overline *)
  Undef;                             (* undertie *)
  Undef;                             (* character tie *)
  Undef;                             (* caret insertion point *)
  Undef;                             (* asterism *)
  Undef;                             (* hyphen bullet *)
  Undef;                             (* fraction slash *)
  Undef;                             (* left square bracket with quill *)
  Undef;                             (* right square bracket with quill *)
  Sequence [0x0022; 0x0022];         (* double question mark *)
  Sequence [0x0022; 0x0021];         (* question exclamation mark *)
  Sequence [0x0021; 0x0022];         (* exclamation question mark *)
  Undef;                             (* tironian sign et *)
  Undef;                             (* reversed pilcrow sign *)
  Undef;                             (* black leftwards bullet *)
  Undef;                             (* black rightwards bullet *)
  Undef;                             (* low asterisk *)
  Undef;                             (* reversed semicolon *)
  Undef;                             (* close up *)
  Undef;                             (* two asterisks aligned vertically *)
  Undef;                             (* commercial minus sign *)
  Undef;                             (* swung dash *)
  Undef;                             (* inverted undertie *)
  Undef;
  Undef;
  Sequence [0x2032; 0x2032; 0x2032; 0x2032]; (* quadruple prime *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;       (* medium mathematical space *)
  Undef;       (* word joiner *)
  Undef;       (* function application *)
  Undef;       (* invisible times *)
  Undef;       (* invisible separator *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;       (* inhibit symmetric swapping *)
  Undef;       (* activate symmetric swapping *)
  Undef;       (* inhibit arabic form shaping *)
  Undef;       (* activate arabic form shaping *)
  Undef;       (* national digit shapes *)
  Undef;       (* nominal digit shapes *)
  Undef;       (* superscript zero *)
  Undef;       (* superscript latin small letter i *)
  Undef;
  Undef;
  Undef;       (* superscript four *)
  Undef;       (* superscript five *)
  Undef;       (* superscript six *)
  Undef;       (* superscript seven *)
  Undef;       (* superscript eight *)
  Undef;       (* superscript nine *)
  Undef;       (* superscript plus sign *)
  Undef;       (* superscript minus *)
  Undef;       (* superscript equals sign *)
  Undef;       (* superscript left parenthesis *)
  Undef;       (* superscript right parenthesis *)
  Undef;       (* superscript latin small letter n *)
  Undef;       (* subscript zero *)
  Undef;       (* subscript one *)
  Undef;       (* subscript two *)
  Undef;       (* subscript three *)
  Undef;       (* subscript four *)
  Undef;       (* subscript five *)
  Undef;       (* subscript six *)
  Undef;       (* subscript seven *)
  Undef;       (* subscript eight *)
  Undef;       (* subscript nine *)
  Undef;       (* subscript plus sign *)
  Undef;       (* subscript minus *)
  Undef;       (* subscript equals sign *)
  Undef;       (* subscript left parenthesis *)
  Undef;       (* subscript right parenthesis *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;       (* euro-currency sign *)
  Undef;       (* colon sign *)
  Undef;       (* cruzeiro sign *)
  Undef;       (* french franc sign *)
  Undef;       (* lira sign *)
  Undef;       (* mill sign *)
  Undef;       (* naira sign *)
  Sequence [0x0050; 0x0074; 0x0073]; (* peseta sign *)
  Sequence [0x0052; 0x0073];         (* rupee sign *)
  Undef;       (* won sign *)
  Undef;       (* new sheqel sign *)
  Undef;       (* dong sign *)
  Undef;       (* euro sign *)
  Undef;       (* kip sign *)
  Undef;       (* tugrik sign *)
  Undef;       (* drachma sign *)
  Undef;       (* german penny sign *)
  Undef;       (* peso sign *)
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef;       (* combining left harpoon above *)
  Undef;       (* combining right harpoon above *)
  Undef;       (* combining long vertical line overlay *)
  Undef;       (* combining short vertical line overlay *)
  Undef;       (* combining anticlockwise arrow above *)
  Undef;       (* combining clockwise arrow above *)
  Undef;       (* combining left arrow above *)
  Undef;       (* combining right arrow above *)
  Undef;       (* combining ring overlay *)
  Undef;       (* combining clockwise ring overlay *)
  Undef;       (* combining anticlockwise ring overlay *)
  Undef;       (* combining three dots above *)
  Undef;       (* combining four dots above *)
  Undef;       (* combining enclosing circle *)
  Undef;       (* combining enclosing square *)
  Undef;       (* combining enclosing diamond *)
  Undef;       (* combining enclosing circle backslash *)
  Undef;       (* combining left right arrow above *)
  Undef;       (* combining enclosing screen *)
  Undef;       (* combining enclosing keycap *)
  Undef;       (* combining enclosing upward pointing triangle *)
  Undef;       (* combining reverse solidus overlay *)
  Undef;       (* combining double vertical stroke overlay *)
  Undef;       (* combining annuity symbol *)
  Undef;       (* combining triple underdot *)
  Undef;       (* combining wide bridge above *)
  Undef;       (* combining leftwards arrow overlay *)
  Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value fake_21 =
[|
  Undef;                     (* account of *)
  Undef;                     (* addressed to the subject *)
  Undef;                     (* double-struck capital c *)
  Sequence [0x00b0; 0x0043]; (* degree celsius *)
  Undef;                     (* centre line symbol *)
  Undef;                     (* care of *)
  Undef;                     (* cada una *)
  Undef;                     (* euler constant *)
  Undef;                     (* scruple *)
  Sequence [0x00b0; 0x0046]; (* degree fahrenheit *)
  Undef;                     (* script small g *)
  Undef;                     (* script capital h *)
  Undef;                     (* black-letter capital h *)
  Undef;                     (* double-struck capital h *)
  Undef;                     (* planck constant *)
  Undef;                     (* planck constant over two pi *)
  Undef;                     (* script capital i *)
  Undef;                     (* black-letter capital i *)
  Undef;                     (* script capital l *)
  Undef;                     (* script small l *)
  Undef;                     (* l b bar symbol *)
  Undef;                     (* double-struck capital n *)
  Undef;                     (* numero sign *)
  Undef;                     (* sound recording copyright *)
  Undef;                     (* script capital p *)
  Undef;                     (* double-struck capital p *)
  Undef;                     (* double-struck capital q *)
  Undef;                     (* script capital r *)
  Undef;                     (* black-letter capital r *)
  Undef;                     (* double-struck capital r *)
  Undef;                     (* prescription take *)
  Undef;                     (* response *)
  Undef;                     (* service mark *)
  Undef;                     (* telephone sign *)
  Undef;                     (* trade mark sign *)
  Undef;                     (* versicle *)
  Undef;                     (* double-struck capital z *)
  Undef;                     (* ounce sign *)
  Undef;                     (* ohm sign *)
  Undef;                     (* inverted ohm sign *)
  Undef;                     (* black-letter capital z *)
  Undef;                     (* turned greek small letter iota *)
  Simple 0x004b;             (* kelvin sign *)
  Simple 0x00c5;             (* angstrom sign *)
  Undef;                     (* script capital b *)
  Undef;                     (* black-letter capital c *)
  Undef;                     (* estimated symbol *)
  Undef;                     (* script small e *)
  Undef;                     (* script capital e *)
  Undef;                     (* script capital f *)
  Undef;                     (* turned capital f *)
  Undef;                     (* script capital m *)
  Undef;                     (* script small o *)
  Undef;                     (* alef symbol *)
  Undef;                     (* bet symbol *)
  Undef;                     (* gimel symbol *)
  Undef;                     (* dalet symbol *)
  Undef;                     (* information source *)
  Undef;                     (* rotated capital q *)
  Undef;                     (* facsimile sign *)
  Undef;
  Undef;       (* double-struck small gamma *)
  Undef;       (* double-struck capital gamma *)
  Undef;       (* double-struck capital pi *)
  Undef;       (* double-struck n-ary summation *)
  Undef;       (* turned sans-serif capital g *)
  Undef;       (* turned sans-serif capital l *)
  Undef;       (* reversed sans-serif capital l *)
  Undef;       (* turned sans-serif capital y *)
  Undef;       (* double-struck italic capital d *)
  Undef;       (* double-struck italic small d *)
  Undef;       (* double-struck italic small e *)
  Undef;       (* double-struck italic small i *)
  Undef;       (* double-struck italic small j *)
  Undef;       (* property line *)
  Undef;       (* turned ampersand *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Sequence [0x00b9; 0x2044; 0x2083]; (* vulgar fraction one third *)
  Sequence [0x00b2; 0x2044; 0x2083]; (* vulgar fraction two thirds *)
  Sequence [0x00b9; 0x2044; 0x2085]; (* vulgar fraction one fifth *)
  Sequence [0x00b2; 0x2044; 0x2085]; (* vulgar fraction two fifths *)
  Sequence [0x00b3; 0x2044; 0x2085]; (* vulgar fraction three fifths *)
  Sequence [0x2074; 0x2044; 0x2085]; (* vulgar fraction four fifths *)
  Sequence [0x00b9; 0x2044; 0x2086]; (* vulgar fraction one sixth *)
  Sequence [0x2075; 0x2044; 0x2086]; (* vulgar fraction five sixths *)
  Sequence [0x00b9; 0x2044; 0x2088]; (* vulgar fraction one eighth *)
  Sequence [0x00b3; 0x2044; 0x2088]; (* vulgar fraction three eighths *)
  Sequence [0x2075; 0x2044; 0x2088]; (* vulgar fraction five eighths *)
  Sequence [0x2077; 0x2044; 0x2088]; (* vulgar fraction seven eighths *)
  Sequence [0x00b9; 0x2044];         (* fraction numerator one *)
  Simple 0x0049;                     (* roman numeral one *)
  Sequence [0x0049; 0x0049];         (* roman numeral two *)
  Sequence [0x0049; 0x0049; 0x0049]; (* roman numeral three *)
  Sequence [0x0049; 0x0056];         (* roman numeral four *)
  Simple 0x0056;                     (* roman numeral five *)
  Sequence [0x0056; 0x0049];         (* roman numeral six *)
  Sequence [0x0056; 0x0049; 0x0049]; (* roman numeral seven *)
  Sequence [0x0056; 0x0049; 0x0049; 0x0049]; (* roman numeral eight *)
  Sequence [0x0049; 0x0058];         (* roman numeral nine *)
  Simple 0x0058;                     (* roman numeral ten *)
  Sequence [0x0058; 0x0049];         (* roman numeral eleven *)
  Sequence [0x0058; 0x0049; 0x0094]; (* roman numeral twelve *)
  Simple 0x004c;                     (* roman numeral fifty *)
  Simple 0x0043;                     (* roman numeral one hundred *)
  Simple 0x0044;                     (* roman numeral five hundred *)
  Simple 0x004d;                     (* roman numeral one thousand *)
  Simple 0x0069;                     (* small roman numeral one *)
  Sequence [0x0069; 0x0069];         (* small roman numeral two *)
  Sequence [0x0069; 0x0069; 0x0069]; (* small roman numeral three *)
  Sequence [0x0069; 0x0076];         (* small roman numeral four *)
  Simple 0x0076;                     (* small roman numeral five *)
  Sequence [0x0076; 0x0069];         (* small roman numeral six *)
  Sequence [0x0076; 0x0069; 0x0069]; (* small roman numeral seven *)
  Sequence [0x0076; 0x0069; 0x0069; 0x0069]; (* small roman numeral eight *)
  Sequence [0x0069; 0x0078];         (* small roman numeral nine *)
  Simple 0x0078;                     (* small roman numeral ten *)
  Sequence [0x0078; 0x0069];         (* small roman numeral eleven *)
  Sequence [0x0078; 0x0069; 0x0064]; (* small roman numeral twelve *)
  Simple 0x006c;                     (* small roman numeral fifty *)
  Simple 0x0063;                     (* small roman numeral one hundred *)
  Simple 0x0064;                     (* small roman numeral five hundred *)
  Simple 0x006d;                     (* small roman numeral one thousand *)
  Undef;                             (* roman numeral one thousand c d *)
  Undef;                             (* roman numeral five thousand *)
  Undef;                             (* roman numeral ten thousand *)
  Undef;                             (* roman numeral reversed one hundred *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;       (* leftwards arrow *)
  Undef;       (* upwards arrow *)
  Undef;       (* rightwards arrow *)
  Undef;       (* downwards arrow *)
  Undef;       (* left right arrow *)
  Undef;       (* up down arrow *)
  Undef;       (* north west arrow *)
  Undef;       (* north east arrow *)
  Undef;       (* south east arrow *)
  Undef;       (* south west arrow *)
  Undef;       (* leftwards arrow with stroke *)
  Undef;       (* rightwards arrow with stroke *)
  Undef;       (* leftwards wave arrow *)
  Undef;       (* rightwards wave arrow *)
  Undef;       (* leftwards two headed arrow *)
  Undef;       (* upwards two headed arrow *)
  Undef;       (* rightwards two headed arrow *)
  Undef;       (* downwards two headed arrow *)
  Undef;       (* leftwards arrow with tail *)
  Undef;       (* rightwards arrow with tail *)
  Undef;       (* leftwards arrow from bar *)
  Undef;       (* upwards arrow from bar *)
  Undef;       (* rightwards arrow from bar *)
  Undef;       (* downwards arrow from bar *)
  Undef;       (* up down arrow with base *)
  Undef;       (* leftwards arrow with hook *)
  Undef;       (* rightwards arrow with hook *)
  Undef;       (* leftwards arrow with loop *)
  Undef;       (* rightwards arrow with loop *)
  Undef;       (* left right wave arrow *)
  Undef;       (* left right arrow with stroke *)
  Undef;       (* downwards zigzag arrow *)
  Undef;       (* upwards arrow with tip leftwards *)
  Undef;       (* upwards arrow with tip rightwards *)
  Undef;       (* downwards arrow with tip leftwards *)
  Undef;       (* downwards arrow with tip rightwards *)
  Undef;       (* rightwards arrow with corner downwards *)
  Undef;       (* downwards arrow with corner leftwards *)
  Undef;       (* anticlockwise top semicircle arrow *)
  Undef;       (* clockwise top semicircle arrow *)
  Undef;       (* north west arrow to long bar *)
  Undef;       (* leftwards arrow to bar over rightwards arrow to bar *)
  Undef;       (* anticlockwise open circle arrow *)
  Undef;       (* clockwise open circle arrow *)
  Undef;       (* leftwards harpoon with barb upwards *)
  Undef;       (* leftwards harpoon with barb downwards *)
  Undef;       (* upwards harpoon with barb rightwards *)
  Undef;       (* upwards harpoon with barb leftwards *)
  Undef;       (* rightwards harpoon with barb upwards *)
  Undef;       (* rightwards harpoon with barb downwards *)
  Undef;       (* downwards harpoon with barb rightwards *)
  Undef;       (* downwards harpoon with barb leftwards *)
  Undef;       (* rightwards arrow over leftwards arrow *)
  Undef;       (* upwards arrow leftwards of downwards arrow *)
  Undef;       (* leftwards arrow over rightwards arrow *)
  Undef;       (* leftwards paired arrows *)
  Undef;       (* upwards paired arrows *)
  Undef;       (* rightwards paired arrows *)
  Undef;       (* downwards paired arrows *)
  Undef;       (* leftwards harpoon over rightwards harpoon *)
  Undef;       (* rightwards harpoon over leftwards harpoon *)
  Undef;       (* leftwards double arrow with stroke *)
  Undef;       (* left right double arrow with stroke *)
  Undef;       (* rightwards double arrow with stroke *)
  Undef;       (* leftwards double arrow *)
  Undef;       (* upwards double arrow *)
  Undef;       (* rightwards double arrow *)
  Undef;       (* downwards double arrow *)
  Undef;       (* left right double arrow *)
  Undef;       (* up down double arrow *)
  Undef;       (* north west double arrow *)
  Undef;       (* north east double arrow *)
  Undef;       (* south east double arrow *)
  Undef;       (* south west double arrow *)
  Undef;       (* leftwards triple arrow *)
  Undef;       (* rightwards triple arrow *)
  Undef;       (* leftwards squiggle arrow *)
  Undef;       (* rightwards squiggle arrow *)
  Undef;       (* upwards arrow with double stroke *)
  Undef;       (* downwards arrow with double stroke *)
  Undef;       (* leftwards dashed arrow *)
  Undef;       (* upwards dashed arrow *)
  Undef;       (* rightwards dashed arrow *)
  Undef;       (* downwards dashed arrow *)
  Undef;       (* leftwards arrow to bar *)
  Undef;       (* rightwards arrow to bar *)
  Undef;       (* leftwards white arrow *)
  Undef;       (* upwards white arrow *)
  Undef;       (* rightwards white arrow *)
  Undef;       (* downwards white arrow *)
  Undef;       (* upwards white arrow from bar *)
  Undef;       (* upwards white arrow on pedestal *)
  Undef;       (* upwards white arrow on pedestal with horizontal bar *)
  Undef;       (* upwards white arrow on pedestal with vertical bar *)
  Undef;       (* upwards white double arrow *)
  Undef;       (* upwards white double arrow on pedestal *)
  Undef;       (* rightwards white arrow from wall *)
  Undef;       (* north west arrow to corner *)
  Undef;       (* south east arrow to corner *)
  Undef;       (* up down white arrow *)
  Undef;       (* right arrow with small circle *)
  Undef;       (* downwards arrow leftwards of upwards arrow *)
  Undef;       (* three rightwards arrows *)
  Undef;       (* leftwards arrow with vertical stroke *)
  Undef;       (* rightwards arrow with vertical stroke *)
  Undef;       (* left right arrow with vertical stroke *)
  Undef;       (* leftwards arrow with double vertical stroke *)
  Undef;       (* rightwards arrow with double vertical stroke *)
  Undef;       (* left right arrow with double vertical stroke *)
  Undef;       (* leftwards open-headed arrow *)
  Undef;       (* rightwards open-headed arrow *)
  Undef        (* left right open-headed arrow *)
|];

value fake_22 =
[|
  Undef;         (* for all *)
  Undef;         (* complement *)
  Undef;         (* partial differential *)
  Undef;         (* there exists *)
  Undef;         (* there does not exist *)
  Undef;         (* empty set *)
  Undef;         (* increment *)
  Undef;         (* nabla *)
  Undef;         (* element of *)
  Undef;         (* not an element of *)
  Undef;         (* small element of *)
  Undef;         (* contains as member *)
  Undef;         (* does not contain as member *)
  Undef;         (* small contains as member *)
  Undef;         (* end of proof *)
  Simple 0x03a0; (* n-ary product -> greek capital letter pi *)
  Undef;         (* n-ary coproduct *)
  Simple 0x03a3; (* n-ary summation -> greek capital letter sigma *)
  Undef;         (* minus sign *)
  Undef;         (* minus-or-plus sign *)
  Undef;         (* dot plus *)
  Simple 0x002f; (* division slash -> solidus *)
  Undef;         (* set minus *)
  Undef;         (* asterisk operator *)
  Undef;         (* ring operator *)
  Undef;         (* bullet operator *)
  Undef;         (* square root *)
  Undef;         (* cube root *)
  Undef;         (* fourth root *)
  Undef;         (* proportional to *)
  Undef;         (* infinity *)
  Undef;         (* right angle *)
  Undef;         (* angle *)
  Undef;         (* measured angle *)
  Undef;         (* spherical angle *)
  Simple 0x007c;             (* divides -> vertical line *)
  Undef;                     (* does not divide *)
  Sequence [0x007c; 0x007c]; (* parallel to *)
  Undef;                     (* not parallel to *)
  Undef;         (* logical and *)
  Undef;         (* logical or *)
  Undef;         (* intersection *)
  Undef;         (* union *)
  Undef;         (* integral *)
  Undef;         (* double integral *)
  Undef;         (* triple integral *)
  Undef;         (* contour integral *)
  Undef;         (* surface integral *)
  Undef;         (* volume integral *)
  Undef;         (* clockwise integral *)
  Undef;         (* clockwise contour integral *)
  Undef;         (* anticlockwise contour integral *)
  Undef;         (* therefore *)
  Undef;         (* because *)
  Undef;         (* ratio *)
  Undef;         (* proportion *)
  Undef;         (* dot minus *)
  Undef;         (* excess *)
  Undef;         (* geometric proportion *)
  Undef;         (* homothetic *)
  Undef;         (* tilde operator *)
  Undef;         (* reversed tilde *)
  Undef;         (* inverted lazy s *)
  Undef;         (* sine wave *)
  Undef;         (* wreath product *)
  Undef;         (* not tilde *)
  Undef;         (* minus tilde *)
  Undef;         (* asymptotically equal to *)
  Undef;         (* not asymptotically equal to *)
  Undef;         (* approximately equal to *)
  Undef;         (* approximately but not actually equal to *)
  Undef;         (* neither approximately nor actually equal to *)
  Undef;         (* almost equal to *)
  Undef;         (* not almost equal to *)
  Undef;         (* almost equal or equal to *)
  Undef;         (* triple tilde *)
  Undef;         (* all equal to *)
  Undef;         (* equivalent to *)
  Undef;         (* geometrically equivalent to *)
  Undef;         (* difference between *)
  Undef;         (* approaches the limit *)
  Undef;         (* geometrically equal to *)
  Undef;         (* approximately equal to or the image of *)
  Undef;         (* image of or approximately equal to *)
  Undef;         (* colon equals *)
  Undef;         (* equals colon *)
  Undef;         (* ring in equal to *)
  Undef;         (* ring equal to *)
  Undef;         (* corresponds to *)
  Undef;         (* estimates *)
  Undef;         (* equiangular to *)
  Undef;         (* star equals *)
  Undef;         (* delta equal to *)
  Undef;         (* equal to by definition *)
  Undef;         (* measured by *)
  Undef;         (* questioned equal to *)
  Undef;         (* not equal to *)
  Undef;         (* identical to *)
  Undef;         (* not identical to *)
  Undef;         (* strictly equivalent to *)
  Undef;         (* less-than or equal to *)
  Undef;         (* greater-than or equal to *)
  Undef;         (* less-than over equal to *)
  Undef;         (* greater-than over equal to *)
  Undef;         (* less-than but not equal to *)
  Undef;         (* greater-than but not equal to *)
  Undef;         (* much less-than *)
  Undef;         (* much greater-than *)
  Undef;         (* between *)
  Undef;         (* not equivalent to *)
  Undef;         (* not less-than *)
  Undef;         (* not greater-than *)
  Undef;         (* neither less-than nor equal to *)
  Undef;         (* neither greater-than nor equal to *)
  Undef;         (* less-than or equivalent to *)
  Undef;         (* greater-than or equivalent to *)
  Undef;         (* neither less-than nor equivalent to *)
  Undef;         (* neither greater-than nor equivalent to *)
  Undef;         (* less-than or greater-than *)
  Undef;         (* greater-than or less-than *)
  Undef;         (* neither less-than nor greater-than *)
  Undef;         (* neither greater-than nor less-than *)
  Undef;         (* precedes *)
  Undef;         (* succeeds *)
  Undef;         (* precedes or equal to *)
  Undef;         (* succeeds or equal to *)
  Undef;         (* precedes or equivalent to *)
  Undef;         (* succeeds or equivalent to *)
  Undef;         (* does not precede *)
  Undef;         (* does not succeed *)
  Undef;         (* subset of *)
  Undef;         (* superset of *)
  Undef;         (* not a subset of *)
  Undef;         (* not a superset of *)
  Undef;         (* subset of or equal to *)
  Undef;         (* superset of or equal to *)
  Undef;         (* neither a subset of nor equal to *)
  Undef;         (* neither a superset of nor equal to *)
  Undef;         (* subset of with not equal to *)
  Undef;         (* superset of with not equal to *)
  Undef;         (* multiset *)
  Undef;         (* multiset multiplication *)
  Undef;         (* multiset union *)
  Undef;         (* square image of *)
  Undef;         (* square original of *)
  Undef;         (* square image of or equal to *)
  Undef;         (* square original of or equal to *)
  Undef;         (* square cap *)
  Undef;         (* square cup *)
  Undef;         (* circled plus *)
  Undef;         (* circled minus *)
  Undef;         (* circled times *)
  Undef;         (* circled division slash *)
  Undef;         (* circled dot operator *)
  Undef;         (* circled ring operator *)
  Undef;         (* circled asterisk operator *)
  Undef;         (* circled equals *)
  Undef;         (* circled dash *)
  Undef;         (* squared plus *)
  Undef;         (* squared minus *)
  Undef;         (* squared times *)
  Undef;         (* squared dot operator *)
  Undef;         (* right tack *)
  Undef;         (* left tack *)
  Undef;         (* down tack *)
  Undef;         (* up tack *)
  Undef;         (* assertion *)
  Undef;         (* models *)
  Undef;         (* true *)
  Undef;         (* forces *)
  Undef;         (* triple vertical bar right turnstile *)
  Undef;         (* double vertical bar double right turnstile *)
  Undef;         (* does not prove *)
  Undef;         (* not true *)
  Undef;         (* does not force *)
  Undef;         (* negated double vertical bar double right turnstile *)
  Undef;         (* precedes under relation *)
  Undef;         (* succeeds under relation *)
  Undef;         (* normal subgroup of *)
  Undef;         (* contains as normal subgroup *)
  Undef;         (* normal subgroup of or equal to *)
  Undef;         (* contains as normal subgroup or equal to *)
  Undef;         (* original of *)
  Undef;         (* image of *)
  Undef;         (* multimap *)
  Undef;         (* hermitian conjugate matrix *)
  Undef;         (* intercalate *)
  Undef;         (* xor *)
  Undef;         (* nand *)
  Undef;         (* nor *)
  Undef;         (* right angle with arc *)
  Undef;         (* right triangle *)
  Undef;         (* n-ary logical and *)
  Undef;         (* n-ary logical or *)
  Undef;         (* n-ary intersection *)
  Undef;         (* n-ary union *)
  Undef;         (* diamond operator *)
  Undef;         (* dot operator *)
  Undef;         (* star operator *)
  Undef;         (* division times *)
  Undef;         (* bowtie *)
  Undef;         (* left normal factor semidirect product *)
  Undef;         (* right normal factor semidirect product *)
  Undef;         (* left semidirect product *)
  Undef;         (* right semidirect product *)
  Undef;         (* reversed tilde equals *)
  Undef;         (* curly logical or *)
  Undef;         (* curly logical and *)
  Undef;         (* double subset *)
  Undef;         (* double superset *)
  Undef;         (* double intersection *)
  Undef;         (* double union *)
  Undef;         (* pitchfork *)
  Undef;         (* equal and parallel to *)
  Undef;         (* less-than with dot *)
  Undef;         (* greater-than with dot *)
  Undef;         (* very much less-than *)
  Undef;         (* very much greater-than *)
  Undef;         (* less-than equal to or greater-than *)
  Undef;         (* greater-than equal to or less-than *)
  Undef;         (* equal to or less-than *)
  Undef;         (* equal to or greater-than *)
  Undef;         (* equal to or precedes *)
  Undef;         (* equal to or succeeds *)
  Undef;         (* does not precede or equal *)
  Undef;         (* does not succeed or equal *)
  Undef;         (* not square image of or equal to *)
  Undef;         (* not square original of or equal to *)
  Undef;         (* square image of or not equal to *)
  Undef;         (* square original of or not equal to *)
  Undef;         (* less-than but not equivalent to *)
  Undef;         (* greater-than but not equivalent to *)
  Undef;         (* precedes but not equivalent to *)
  Undef;         (* succeeds but not equivalent to *)
  Undef;         (* not normal subgroup of *)
  Undef;         (* does not contain as normal subgroup *)
  Undef;         (* not normal subgroup of or equal to *)
  Undef;         (* does not contain as normal subgroup or equal *)
  Undef;         (* vertical ellipsis *)
  Undef;         (* midline horizontal ellipsis *)
  Undef;         (* up right diagonal ellipsis *)
  Undef;         (* down right diagonal ellipsis *)
  Undef;         (* element of with long horizontal stroke *)
  Undef;         (* element of with vertical bar at end of horizontal stroke *)
  Undef;         (* small element of with vertical bar at end of horizontal stroke *)
  Undef;         (* element of with dot above *)
  Undef;         (* element of with overbar *)
  Undef;         (* small element of with overbar *)
  Undef;         (* element of with underbar *)
  Undef;         (* element of with two horizontal strokes *)
  Undef;         (* contains with long horizontal stroke *)
  Undef;         (* contains with vertical bar at end of horizontal stroke *)
  Undef;         (* small contains with vertical bar at end of horizontal stroke *)
  Undef;         (* contains with overbar *)
  Undef;         (* small contains with overbar *)
  Undef          (* z notation bag membership *)
|];

value fake_23 =
[|
  Undef;         (* diameter sign *)
  Undef;         (* electric arrow *)
  Undef;         (* house *)
  Undef;         (* up arrowhead *)
  Undef;         (* down arrowhead *)
  Undef;         (* projective *)
  Undef;         (* perspective *)
  Undef;         (* wavy line *)
  Undef;         (* left ceiling *)
  Undef;         (* right ceiling *)
  Undef;         (* left floor *)
  Undef;         (* right floor *)
  Undef;         (* bottom right crop *)
  Undef;         (* bottom left crop *)
  Undef;         (* top right crop *)
  Undef;         (* top left crop *)
  Undef;         (* reversed not sign *)
  Undef;         (* square lozenge *)
  Undef;         (* arc *)
  Undef;         (* segment *)
  Undef;         (* sector *)
  Undef;         (* telephone recorder *)
  Undef;         (* position indicator *)
  Undef;         (* viewdata square *)
  Undef;         (* place of interest sign *)
  Undef;         (* turned not sign *)
  Undef;         (* watch *)
  Undef;         (* hourglass *)
  Undef;         (* top left corner *)
  Undef;         (* top right corner *)
  Undef;         (* bottom left corner *)
  Undef;         (* bottom right corner *)
  Undef;         (* top half integral *)
  Undef;         (* bottom half integral *)
  Undef;         (* frown *)
  Undef;         (* smile *)
  Undef;         (* up arrowhead between two horizontal bars *)
  Undef;         (* option key *)
  Undef;         (* erase to the right *)
  Undef;         (* x in a rectangle box *)
  Undef;         (* keyboard *)
  Undef;         (* left-pointing angle bracket *)
  Undef;         (* right-pointing angle bracket *)
  Undef;         (* erase to the left *)
  Undef;         (* benzene ring *)
  Undef;         (* cylindricity *)
  Undef;         (* all around-profile *)
  Undef;         (* symmetry *)
  Undef;         (* total runout *)
  Undef;         (* dimension origin *)
  Undef;         (* conical taper *)
  Undef;         (* slope *)
  Undef;         (* counterbore *)
  Undef;         (* countersink *)
  Undef;         (* apl functional symbol i-beam *)
  Undef;         (* apl functional symbol squish quad *)
  Undef;         (* apl functional symbol quad equal *)
  Undef;         (* apl functional symbol quad divide *)
  Undef;         (* apl functional symbol quad diamond *)
  Undef;         (* apl functional symbol quad jot *)
  Undef;         (* apl functional symbol quad circle *)
  Undef;         (* apl functional symbol circle stile *)
  Undef;         (* apl functional symbol circle jot *)
  Undef;         (* apl functional symbol slash bar *)
  Undef;         (* apl functional symbol backslash bar *)
  Undef;         (* apl functional symbol quad slash *)
  Undef;         (* apl functional symbol quad backslash *)
  Undef;         (* apl functional symbol quad less-than *)
  Undef;         (* apl functional symbol quad greater-than *)
  Undef;         (* apl functional symbol leftwards vane *)
  Undef;         (* apl functional symbol rightwards vane *)
  Undef;         (* apl functional symbol quad leftwards arrow *)
  Undef;         (* apl functional symbol quad rightwards arrow *)
  Undef;         (* apl functional symbol circle backslash *)
  Undef;         (* apl functional symbol down tack underbar *)
  Undef;         (* apl functional symbol delta stile *)
  Undef;         (* apl functional symbol quad down caret *)
  Undef;         (* apl functional symbol quad delta *)
  Undef;         (* apl functional symbol down tack jot *)
  Undef;         (* apl functional symbol upwards vane *)
  Undef;         (* apl functional symbol quad upwards arrow *)
  Undef;         (* apl functional symbol up tack overbar *)
  Undef;         (* apl functional symbol del stile *)
  Undef;         (* apl functional symbol quad up caret *)
  Undef;         (* apl functional symbol quad del *)
  Undef;         (* apl functional symbol up tack jot *)
  Undef;         (* apl functional symbol downwards vane *)
  Undef;         (* apl functional symbol quad downwards arrow *)
  Undef;         (* apl functional symbol quote underbar *)
  Undef;         (* apl functional symbol delta underbar *)
  Undef;         (* apl functional symbol diamond underbar *)
  Undef;         (* apl functional symbol jot underbar *)
  Undef;         (* apl functional symbol circle underbar *)
  Undef;         (* apl functional symbol up shoe jot *)
  Undef;         (* apl functional symbol quote quad *)
  Undef;         (* apl functional symbol circle star *)
  Undef;         (* apl functional symbol quad colon *)
  Undef;         (* apl functional symbol up tack diaeresis *)
  Undef;         (* apl functional symbol del diaeresis *)
  Undef;         (* apl functional symbol star diaeresis *)
  Undef;         (* apl functional symbol jot diaeresis *)
  Undef;         (* apl functional symbol circle diaeresis *)
  Undef;         (* apl functional symbol down shoe stile *)
  Undef;         (* apl functional symbol left shoe stile *)
  Undef;         (* apl functional symbol tilde diaeresis *)
  Undef;         (* apl functional symbol greater-than diaeresis *)
  Undef;         (* apl functional symbol comma bar *)
  Undef;         (* apl functional symbol del tilde *)
  Undef;         (* apl functional symbol zilde *)
  Undef;         (* apl functional symbol stile tilde *)
  Undef;         (* apl functional symbol semicolon underbar *)
  Undef;         (* apl functional symbol quad not equal *)
  Undef;         (* apl functional symbol quad question *)
  Undef;         (* apl functional symbol down caret tilde *)
  Undef;         (* apl functional symbol up caret tilde *)
  Simple 0x03b9; (* apl functional symbol iota *)
  Simple 0x03c1; (* apl functional symbol rho *)
  Simple 0x03c9; (* apl functional symbol omega *)
  Undef;         (* apl functional symbol alpha underbar *)
  Undef;         (* apl functional symbol epsilon underbar *)
  Undef;         (* apl functional symbol iota underbar *)
  Undef;         (* apl functional symbol omega underbar *)
  Simple 0x03b1; (* apl functional symbol alpha *)
  Undef;         (* not check mark *)
  Undef;         (* right angle with downwards zigzag arrow *)
  Undef;         (* shouldered open box *)
  Undef;         (* bell symbol *)
  Undef;         (* vertical line with middle dot *)
  Undef;         (* insertion symbol *)
  Undef;         (* continuous underline symbol *)
  Undef;         (* discontinuous underline symbol *)
  Undef;         (* emphasis symbol *)
  Undef;         (* composition symbol *)
  Undef;         (* white square with centre vertical line *)
  Undef;         (* enter symbol *)
  Undef;         (* alternative key symbol *)
  Undef;         (* helm symbol *)
  Undef;         (* circled horizontal bar with notch *)
  Undef;         (* circled triangle down *)
  Undef;         (* broken circle with northwest arrow *)
  Undef;         (* undo symbol *)
  Undef;         (* monostable symbol *)
  Undef;         (* hysteresis symbol *)
  Undef;         (* open-circuit-output h-type symbol *)
  Undef;         (* open-circuit-output l-type symbol *)
  Undef;         (* passive-pull-down-output symbol *)
  Undef;         (* passive-pull-up-output symbol *)
  Undef;         (* direct current symbol form two *)
  Undef;         (* software-function symbol *)
  Undef;         (* apl functional symbol quad *)
  Undef;         (* decimal separator key symbol *)
  Undef;         (* previous page *)
  Undef;         (* next page *)
  Undef;         (* print screen symbol *)
  Undef;         (* clear screen symbol *)
  Undef;         (* left parenthesis upper hook *)
  Undef;         (* left parenthesis extension *)
  Undef;         (* left parenthesis lower hook *)
  Undef;         (* right parenthesis upper hook *)
  Undef;         (* right parenthesis extension *)
  Undef;         (* right parenthesis lower hook *)
  Undef;         (* left square bracket upper corner *)
  Undef;         (* left square bracket extension *)
  Undef;         (* left square bracket lower corner *)
  Undef;         (* right square bracket upper corner *)
  Undef;         (* right square bracket extension *)
  Undef;         (* right square bracket lower corner *)
  Undef;         (* left curly bracket upper hook *)
  Undef;         (* left curly bracket middle piece *)
  Undef;         (* left curly bracket lower hook *)
  Undef;         (* curly bracket extension *)
  Undef;         (* right curly bracket upper hook *)
  Undef;         (* right curly bracket middle piece *)
  Undef;         (* right curly bracket lower hook *)
  Undef;         (* integral extension *)
  Undef;         (* horizontal line extension *)
  Undef;         (* upper left or lower right curly bracket section *)
  Undef;         (* upper right or lower left curly bracket section *)
  Undef;         (* summation top *)
  Undef;         (* summation bottom *)
  Undef;         (* top square bracket *)
  Undef;         (* bottom square bracket *)
  Undef;         (* bottom square bracket over top square bracket *)
  Undef;         (* radical symbol bottom *)
  Undef;         (* left vertical box line *)
  Undef;         (* right vertical box line *)
  Undef;         (* horizontal scan line-1 *)
  Undef;         (* horizontal scan line-3 *)
  Undef;         (* horizontal scan line-7 *)
  Undef;         (* horizontal scan line-9 *)
  Undef;         (* dentistry symbol light vertical and top right *)
  Undef;         (* dentistry symbol light vertical and bottom right *)
  Undef;         (* dentistry symbol light vertical with circle *)
  Undef;         (* dentistry symbol light down and horizontal with circle *)
  Undef;         (* dentistry symbol light up and horizontal with circle *)
  Undef;         (* dentistry symbol light vertical with triangle *)
  Undef;         (* dentistry symbol light down and horizontal with triangle *)
  Undef;         (* dentistry symbol light up and horizontal with triangle *)
  Undef;         (* dentistry symbol light vertical and wave *)
  Undef;         (* dentistry symbol light down and horizontal with wave *)
  Undef;         (* dentistry symbol light up and horizontal with wave *)
  Undef;         (* dentistry symbol light down and horizontal *)
  Undef;         (* dentistry symbol light up and horizontal *)
  Undef;         (* dentistry symbol light vertical and top left *)
  Undef;         (* dentistry symbol light vertical and bottom left *)
  Undef;         (* square foot *)
  Undef;         (* return symbol *)
  Undef;         (* eject symbol *)
  Undef;         (* vertical line extension *)
  Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef
|];

value fake_fb =
[|
  Sequence [0x0066; 0x0066];         (* latin small ligature ff *)
  Sequence [0x0066; 0x0069];         (* latin small ligature fi *)
  Sequence [0x0066; 0x006c];         (* latin small ligature fl *)
  Sequence [0x0066; 0x0066; 0x0069]; (* latin small ligature ffi *)
  Sequence [0x0066; 0x0066; 0x006c]; (* latin small ligature ffl *)
  Sequence [0x017f; 0x0074];         (* latin small ligature long s t *)
  Sequence [0x0073; 0x0074];         (* latin small ligature st *)
  Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;       (* armenian small ligature men now *)
  Undef;       (* armenian small ligature men ech *)
  Undef;       (* armenian small ligature men ini *)
  Undef;       (* armenian small ligature vew now *)
  Undef;       (* armenian small ligature men xeh *)
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;
  Undef;       (* hebrew letter yod with hiriq *)
  Undef;       (* hebrew point judeo-spanish varika *)
  Undef;       (* hebrew ligature yiddish yod yod patah *)
  Undef;       (* hebrew letter alternative ayin *)
  Undef;       (* hebrew letter wide alef *)
  Undef;       (* hebrew letter wide dalet *)
  Undef;       (* hebrew letter wide he *)
  Undef;       (* hebrew letter wide kaf *)
  Undef;       (* hebrew letter wide lamed *)
  Undef;       (* hebrew letter wide final mem *)
  Undef;       (* hebrew letter wide resh *)
  Undef;       (* hebrew letter wide tav *)
  Undef;       (* hebrew letter alternative plus sign *)
  Undef;       (* hebrew letter shin with shin dot *)
  Undef;       (* hebrew letter shin with sin dot *)
  Undef;       (* hebrew letter shin with dagesh and shin dot *)
  Undef;       (* hebrew letter shin with dagesh and sin dot *)
  Undef;       (* hebrew letter alef with patah *)
  Undef;       (* hebrew letter alef with qamats *)
  Undef;       (* hebrew letter alef with mapiq *)
  Undef;       (* hebrew letter bet with dagesh *)
  Undef;       (* hebrew letter gimel with dagesh *)
  Undef;       (* hebrew letter dalet with dagesh *)
  Undef;       (* hebrew letter he with mapiq *)
  Undef;       (* hebrew letter vav with dagesh *)
  Undef;       (* hebrew letter zayin with dagesh *)
  Undef;
  Undef;       (* hebrew letter tet with dagesh *)
  Undef;       (* hebrew letter yod with dagesh *)
  Undef;       (* hebrew letter final kaf with dagesh *)
  Undef;       (* hebrew letter kaf with dagesh *)
  Undef;       (* hebrew letter lamed with dagesh *)
  Undef;
  Undef;       (* hebrew letter mem with dagesh *)
  Undef;
  Undef;       (* hebrew letter nun with dagesh *)
  Undef;       (* hebrew letter samekh with dagesh *)
  Undef;
  Undef;       (* hebrew letter final pe with dagesh *)
  Undef;       (* hebrew letter pe with dagesh *)
  Undef;
  Undef;       (* hebrew letter tsadi with dagesh *)
  Undef;       (* hebrew letter qof with dagesh *)
  Undef;       (* hebrew letter resh with dagesh *)
  Undef;       (* hebrew letter shin with dagesh *)
  Undef;       (* hebrew letter tav with dagesh *)
  Undef;       (* hebrew letter vav with holam *)
  Undef;       (* hebrew letter bet with rafe *)
  Undef;       (* hebrew letter kaf with rafe *)
  Undef;       (* hebrew letter pe with rafe *)
  Undef;       (* hebrew ligature alef lamed *)
  Undef;       (* arabic letter alef wasla isolated form *)
  Undef;       (* arabic letter alef wasla final form *)
  Undef;       (* arabic letter beeh isolated form *)
  Undef;       (* arabic letter beeh final form *)
  Undef;       (* arabic letter beeh initial form *)
  Undef;       (* arabic letter beeh medial form *)
  Undef;       (* arabic letter peh isolated form *)
  Undef;       (* arabic letter peh final form *)
  Undef;       (* arabic letter peh initial form *)
  Undef;       (* arabic letter peh medial form *)
  Undef;       (* arabic letter beheh isolated form *)
  Undef;       (* arabic letter beheh final form *)
  Undef;       (* arabic letter beheh initial form *)
  Undef;       (* arabic letter beheh medial form *)
  Undef;       (* arabic letter tteheh isolated form *)
  Undef;       (* arabic letter tteheh final form *)
  Undef;       (* arabic letter tteheh initial form *)
  Undef;       (* arabic letter tteheh medial form *)
  Undef;       (* arabic letter teheh isolated form *)
  Undef;       (* arabic letter teheh final form *)
  Undef;       (* arabic letter teheh initial form *)
  Undef;       (* arabic letter teheh medial form *)
  Undef;       (* arabic letter tteh isolated form *)
  Undef;       (* arabic letter tteh final form *)
  Undef;       (* arabic letter tteh initial form *)
  Undef;       (* arabic letter tteh medial form *)
  Undef;       (* arabic letter veh isolated form *)
  Undef;       (* arabic letter veh final form *)
  Undef;       (* arabic letter veh initial form *)
  Undef;       (* arabic letter veh medial form *)
  Undef;       (* arabic letter peheh isolated form *)
  Undef;       (* arabic letter peheh final form *)
  Undef;       (* arabic letter peheh initial form *)
  Undef;       (* arabic letter peheh medial form *)
  Undef;       (* arabic letter dyeh isolated form *)
  Undef;       (* arabic letter dyeh final form *)
  Undef;       (* arabic letter dyeh initial form *)
  Undef;       (* arabic letter dyeh medial form *)
  Undef;       (* arabic letter nyeh isolated form *)
  Undef;       (* arabic letter nyeh final form *)
  Undef;       (* arabic letter nyeh initial form *)
  Undef;       (* arabic letter nyeh medial form *)
  Undef;       (* arabic letter tcheh isolated form *)
  Undef;       (* arabic letter tcheh final form *)
  Undef;       (* arabic letter tcheh initial form *)
  Undef;       (* arabic letter tcheh medial form *)
  Undef;       (* arabic letter tcheheh isolated form *)
  Undef;       (* arabic letter tcheheh final form *)
  Undef;       (* arabic letter tcheheh initial form *)
  Undef;       (* arabic letter tcheheh medial form *)
  Undef;       (* arabic letter ddahal isolated form *)
  Undef;       (* arabic letter ddahal final form *)
  Undef;       (* arabic letter dahal isolated form *)
  Undef;       (* arabic letter dahal final form *)
  Undef;       (* arabic letter dul isolated form *)
  Undef;       (* arabic letter dul final form *)
  Undef;       (* arabic letter ddal isolated form *)
  Undef;       (* arabic letter ddal final form *)
  Undef;       (* arabic letter jeh isolated form *)
  Undef;       (* arabic letter jeh final form *)
  Undef;       (* arabic letter rreh isolated form *)
  Undef;       (* arabic letter rreh final form *)
  Undef;       (* arabic letter keheh isolated form *)
  Undef;       (* arabic letter keheh final form *)
  Undef;       (* arabic letter keheh initial form *)
  Undef;       (* arabic letter keheh medial form *)
  Undef;       (* arabic letter gaf isolated form *)
  Undef;       (* arabic letter gaf final form *)
  Undef;       (* arabic letter gaf initial form *)
  Undef;       (* arabic letter gaf medial form *)
  Undef;       (* arabic letter gueh isolated form *)
  Undef;       (* arabic letter gueh final form *)
  Undef;       (* arabic letter gueh initial form *)
  Undef;       (* arabic letter gueh medial form *)
  Undef;       (* arabic letter ngoeh isolated form *)
  Undef;       (* arabic letter ngoeh final form *)
  Undef;       (* arabic letter ngoeh initial form *)
  Undef;       (* arabic letter ngoeh medial form *)
  Undef;       (* arabic letter noon ghunna isolated form *)
  Undef;       (* arabic letter noon ghunna final form *)
  Undef;       (* arabic letter rnoon isolated form *)
  Undef;       (* arabic letter rnoon final form *)
  Undef;       (* arabic letter rnoon initial form *)
  Undef;       (* arabic letter rnoon medial form *)
  Undef;       (* arabic letter heh with yeh above isolated form *)
  Undef;       (* arabic letter heh with yeh above final form *)
  Undef;       (* arabic letter heh goal isolated form *)
  Undef;       (* arabic letter heh goal final form *)
  Undef;       (* arabic letter heh goal initial form *)
  Undef;       (* arabic letter heh goal medial form *)
  Undef;       (* arabic letter heh doachashmee isolated form *)
  Undef;       (* arabic letter heh doachashmee final form *)
  Undef;       (* arabic letter heh doachashmee initial form *)
  Undef;       (* arabic letter heh doachashmee medial form *)
  Undef;       (* arabic letter yeh barree isolated form *)
  Undef;       (* arabic letter yeh barree final form *)
  Undef;       (* arabic letter yeh barree with hamza above isolated form *)
  Undef;       (* arabic letter yeh barree with hamza above final form *)
  Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef; Undef; Undef; Undef; Undef; Undef;
  Undef; Undef; Undef;
  Undef;       (* arabic letter ng isolated form *)
  Undef;       (* arabic letter ng final form *)
  Undef;       (* arabic letter ng initial form *)
  Undef;       (* arabic letter ng medial form *)
  Undef;       (* arabic letter u isolated form *)
  Undef;       (* arabic letter u final form *)
  Undef;       (* arabic letter oe isolated form *)
  Undef;       (* arabic letter oe final form *)
  Undef;       (* arabic letter yu isolated form *)
  Undef;       (* arabic letter yu final form *)
  Undef;       (* arabic letter u with hamza above isolated form *)
  Undef;       (* arabic letter ve isolated form *)
  Undef;       (* arabic letter ve final form *)
  Undef;       (* arabic letter kirghiz oe isolated form *)
  Undef;       (* arabic letter kirghiz oe final form *)
  Undef;       (* arabic letter kirghiz yu isolated form *)
  Undef;       (* arabic letter kirghiz yu final form *)
  Undef;       (* arabic letter e isolated form *)
  Undef;       (* arabic letter e final form *)
  Undef;       (* arabic letter e initial form *)
  Undef;       (* arabic letter e medial form *)
  Undef;       (* arabic letter uighur kazakh kirghiz alef maksura initial form *)
  Undef;       (* arabic letter uighur kazakh kirghiz alef maksura medial form *)
  Undef;       (* arabic ligature yeh with hamza above with alef isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with alef final form *)
  Undef;       (* arabic ligature yeh with hamza above with ae isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with ae final form *)
  Undef;       (* arabic ligature yeh with hamza above with waw isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with waw final form *)
  Undef;       (* arabic ligature yeh with hamza above with u isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with u final form *)
  Undef;       (* arabic ligature yeh with hamza above with oe isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with oe final form *)
  Undef;       (* arabic ligature yeh with hamza above with yu isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with yu final form *)
  Undef;       (* arabic ligature yeh with hamza above with e isolated form *)
  Undef;       (* arabic ligature yeh with hamza above with e final form *)
  Undef;       (* arabic ligature yeh with hamza above with e initial form *)
  Undef;       (* arabic ligature uighur kirghiz yeh with hamza above with alef maksura isolated form *)
  Undef;       (* arabic ligature uighur kirghiz yeh with hamza above with alef maksura final form *)
  Undef;       (* arabic ligature uighur kirghiz yeh with hamza above with alef maksura initial form *)
  Undef;       (* arabic letter farsi yeh isolated form *)
  Undef;       (* arabic letter farsi yeh final form *)
  Undef;       (* arabic letter farsi yeh initial form *)
  Undef        (* arabic letter farsi yeh medial form *)
|];

value fake = Charmap.build
[|
  fake_00;     fake_01;     fake_02;     fake_03;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; fake_1d;     fake_1e;     empty_table;
  fake_20;     fake_21;     fake_22;     fake_23;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; empty_table;
  empty_table; empty_table; empty_table; fake_fb;
  empty_table; empty_table; empty_table; empty_table
|];

value ot1_to_uc =
[|
  [|0x0393|]; [|0x0394|]; [|0x0398|]; [|0x039b|]; [|0x039e|]; [|0x03a0|]; [|0x03a3|]; [|0x03a5|];
  [|0x03a6|]; [|0x03a8|]; [|0x03a9|]; [|0x66; 0x66|]; [|0x66; 0x69|]; [|0x66; 0x6c|]; [|0x66; 0x66; 0x69|]; [|0x66; 0x66; 0x6c|];
  [|0x0131|]; [|0x0237|]; [|0x0060|]; [|0x00b4|]; [|0x02c7|]; [|0x02d8|]; [|0x00af|]; [|0x02da|];
  [|0x00b8|]; [|0x00df|]; [|0x00e6|]; [|0x0153|]; [|0x00f8|]; [|0x00c6|]; [|0x0152|]; [|0x00d8|];
  [||];       [|0x0021|]; [|0x201d|]; [|0x0023|]; [|0x0024|]; [|0x0025|]; [|0x0026|]; [|0x2019|];
  [|0x0028|]; [|0x0029|]; [|0x002a|]; [|0x002b|]; [|0x002c|]; [|0x002d|]; [|0x002e|]; [|0x002f|];
  [|0x0030|]; [|0x0031|]; [|0x0032|]; [|0x0033|]; [|0x0034|]; [|0x0035|]; [|0x0036|]; [|0x0037|];
  [|0x0038|]; [|0x0039|]; [|0x003a|]; [|0x003b|]; [|0x00a1|]; [|0x003d|]; [|0x00bf|]; [|0x003f|];
  [|0x0040|]; [|0x0041|]; [|0x0042|]; [|0x0043|]; [|0x0044|]; [|0x0045|]; [|0x0046|]; [|0x0047|];
  [|0x0048|]; [|0x0049|]; [|0x004a|]; [|0x004b|]; [|0x004c|]; [|0x004d|]; [|0x004e|]; [|0x004f|];
  [|0x0050|]; [|0x0051|]; [|0x0052|]; [|0x0053|]; [|0x0054|]; [|0x0055|]; [|0x0056|]; [|0x0057|];
  [|0x0058|]; [|0x0059|]; [|0x005a|]; [|0x005b|]; [|0x201c|]; [|0x005d|]; [|0x005e|]; [|0x02d9|];
  [|0x2018|]; [|0x0061|]; [|0x0062|]; [|0x0063|]; [|0x0064|]; [|0x0065|]; [|0x0066|]; [|0x0067|];
  [|0x0068|]; [|0x0069|]; [|0x006a|]; [|0x006b|]; [|0x006c|]; [|0x006d|]; [|0x006e|]; [|0x006f|];
  [|0x0070|]; [|0x0071|]; [|0x0072|]; [|0x0073|]; [|0x0074|]; [|0x0075|]; [|0x0076|]; [|0x0077|];
  [|0x0078|]; [|0x0079|]; [|0x007a|]; [|0x2013|]; [|0x2014|]; [|0x02dd|]; [|0x007e|]; [|0x00a8|]
|];

value t1_to_uc  =
[|
  [|0x0060|]; [|0x00b4|]; [|0x005e|]; [|0x007E|]; [|0x00a8|]; [|0x02dd|]; [|0x02da|]; [|0x02c7|];
  [|0x02d8|]; [|0x00af|]; [|0x02d9|]; [|0x00b8|]; [|0x02db|]; [|0x201a|]; [|0x2039|]; [|0x203a|];
  [|0x201c|]; [|0x201d|]; [|0x201e|]; [|0x00ab|]; [|0x00bb|]; [|0x2013|]; [|0x2014|]; [||];
  [|0x2080|]; [|0x0131|]; [|0x0237|]; [|0x66; 0x66|]; [|0x66; 0x69|]; [|0x66; 0x6c|]; [|0x66; 0x66; 0x69|]; [|0x66; 0x66; 0x6c|];
  [|0x0020|]; [|0x0021|]; [|0x0022|]; [|0x0023|]; [|0x0024|]; [|0x0025|]; [|0x0026|]; [|0x2019|];
  [|0x0028|]; [|0x0029|]; [|0x002a|]; [|0x002b|]; [|0x002c|]; [|0x002d|]; [|0x002e|]; [|0x002f|];
  [|0x0030|]; [|0x0031|]; [|0x0032|]; [|0x0033|]; [|0x0034|]; [|0x0035|]; [|0x0036|]; [|0x0037|];
  [|0x0038|]; [|0x0039|]; [|0x003a|]; [|0x003b|]; [|0x003c|]; [|0x003d|]; [|0x003e|]; [|0x003f|];
  [|0x0040|]; [|0x0041|]; [|0x0042|]; [|0x0043|]; [|0x0044|]; [|0x0045|]; [|0x0046|]; [|0x0047|];
  [|0x0048|]; [|0x0049|]; [|0x004a|]; [|0x004b|]; [|0x004c|]; [|0x004d|]; [|0x004e|]; [|0x004f|];
  [|0x0050|]; [|0x0051|]; [|0x0052|]; [|0x0053|]; [|0x0054|]; [|0x0055|]; [|0x0056|]; [|0x0057|];
  [|0x0058|]; [|0x0059|]; [|0x005a|]; [|0x005b|]; [|0x005c|]; [|0x005d|]; [|0x005e|]; [|0x005f|];
  [|0x0060|]; [|0x0061|]; [|0x0062|]; [|0x0063|]; [|0x0064|]; [|0x0065|]; [|0x0066|]; [|0x0067|];
  [|0x0068|]; [|0x0069|]; [|0x006a|]; [|0x006b|]; [|0x006c|]; [|0x006d|]; [|0x006e|]; [|0x006f|];
  [|0x0070|]; [|0x0071|]; [|0x0072|]; [|0x0073|]; [|0x0074|]; [|0x0075|]; [|0x0076|]; [|0x0077|];
  [|0x0078|]; [|0x0079|]; [|0x007a|]; [|0x007b|]; [|0x007c|]; [|0x007d|]; [|0x007e|]; [|0x002d|];
  [|0x0102|]; [|0x0104|]; [|0x0106|]; [|0x010c|]; [|0x010e|]; [|0x011a|]; [|0x0118|]; [|0x011e|];
  [|0x0139|]; [|0x013d|]; [|0x0141|]; [|0x0143|]; [|0x0147|]; [|0x014a|]; [|0x0150|]; [|0x0154|];
  [|0x0158|]; [|0x015a|]; [|0x0160|]; [|0x015e|]; [|0x0164|]; [|0x0162|]; [|0x0170|]; [|0x016e|];
  [|0x0178|]; [|0x0179|]; [|0x017d|]; [|0x017b|]; [|0x0132|]; [|0x0130|]; [|0x0111|]; [|0x00a7|];
  [|0x0103|]; [|0x0105|]; [|0x0107|]; [|0x010d|]; [|0x010f|]; [|0x011b|]; [|0x0119|]; [|0x011f|];
  [|0x013a|]; [|0x013e|]; [|0x0142|]; [|0x0144|]; [|0x0148|]; [|0x014b|]; [|0x0151|]; [|0x0155|];
  [|0x0159|]; [|0x015b|]; [|0x0161|]; [|0x015f|]; [|0x0165|]; [|0x0163|]; [|0x0171|]; [|0x016f|];
  [|0x00ff|]; [|0x017a|]; [|0x017e|]; [|0x017c|]; [|0x0133|]; [|0x00a1|]; [|0x00bf|]; [|0x00a3|];
  [|0x00c0|]; [|0x00c1|]; [|0x00c2|]; [|0x00c3|]; [|0x00c4|]; [|0x00c5|]; [|0x00c6|]; [|0x00c7|];
  [|0x00c8|]; [|0x00c9|]; [|0x00ca|]; [|0x00cb|]; [|0x00cc|]; [|0x00cd|]; [|0x00ce|]; [|0x00cf|];
  [|0x00d0|]; [|0x00d1|]; [|0x00d2|]; [|0x00d3|]; [|0x00d4|]; [|0x00d5|]; [|0x00d6|]; [|0x0152|];
  [|0x00d8|]; [|0x00d9|]; [|0x00da|]; [|0x00db|]; [|0x00dc|]; [|0x00dd|]; [|0x00de|]; [|0x53; 0x53|];
  [|0x00e0|]; [|0x00e1|]; [|0x00e2|]; [|0x00e3|]; [|0x00e4|]; [|0x00e5|]; [|0x00e6|]; [|0x00e7|];
  [|0x00e8|]; [|0x00e9|]; [|0x00ea|]; [|0x00eb|]; [|0x00ec|]; [|0x00ed|]; [|0x00ee|]; [|0x00ef|];
  [|0x00f0|]; [|0x00f1|]; [|0x00f2|]; [|0x00f3|]; [|0x00f4|]; [|0x00f5|]; [|0x00f6|]; [|0x0153|];
  [|0x00f8|]; [|0x00f9|]; [|0x00fa|]; [|0x00fb|]; [|0x00fc|]; [|0x00fd|]; [|0x00fe|]; [|0x00df|]
|];

value ott_to_uc =
[|
  [|0x0393|]; [|0x0394|]; [|0x0398|]; [|0x039b|]; [|0x039e|]; [|0x03a0|]; [|0x03a3|]; [|0x03a5|];
  [|0x03a6|]; [|0x03a8|]; [|0x03a9|]; [|0x2191|]; [|0x2193|]; [|0x0027|]; [|0x00a1|]; [|0x00bf|];
  [|0x0131|]; [|0x0237|]; [|0x0060|]; [|0x00b4|]; [|0x02c7|]; [|0x02d8|]; [|0x00af|]; [|0x02da|];
  [|0x00b8|]; [|0x00df|]; [|0x00e6|]; [|0x0153|]; [|0x00f8|]; [|0x00c6|]; [|0x0152|]; [|0x00d8|];
  [|0x2420|]; [|0x0021|]; [|0x201d|]; [|0x0023|]; [|0x0024|]; [|0x0025|]; [|0x0026|]; [|0x2019|];
  [|0x0028|]; [|0x0029|]; [|0x002a|]; [|0x002b|]; [|0x002c|]; [|0x002d|]; [|0x002e|]; [|0x002f|];
  [|0x0030|]; [|0x0031|]; [|0x0032|]; [|0x0033|]; [|0x0034|]; [|0x0035|]; [|0x0036|]; [|0x0037|];
  [|0x0038|]; [|0x0039|]; [|0x003a|]; [|0x003b|]; [|0x003c|]; [|0x003d|]; [|0x003e|]; [|0x003f|];
  [|0x0040|]; [|0x0041|]; [|0x0042|]; [|0x0043|]; [|0x0044|]; [|0x0045|]; [|0x0046|]; [|0x0047|];
  [|0x0048|]; [|0x0049|]; [|0x004a|]; [|0x004b|]; [|0x004c|]; [|0x004d|]; [|0x004e|]; [|0x004f|];
  [|0x0050|]; [|0x0051|]; [|0x0052|]; [|0x0053|]; [|0x0054|]; [|0x0055|]; [|0x0056|]; [|0x0057|];
  [|0x0058|]; [|0x0059|]; [|0x005a|]; [|0x005b|]; [|0x005c|]; [|0x005d|]; [|0x005e|]; [|0x005f|];
  [|0x2018|]; [|0x0061|]; [|0x0062|]; [|0x0063|]; [|0x0064|]; [|0x0065|]; [|0x0066|]; [|0x0067|];
  [|0x0068|]; [|0x0069|]; [|0x006a|]; [|0x006b|]; [|0x006c|]; [|0x006d|]; [|0x006e|]; [|0x006f|];
  [|0x0070|]; [|0x0071|]; [|0x0072|]; [|0x0073|]; [|0x0074|]; [|0x0075|]; [|0x0076|]; [|0x0077|];
  [|0x0078|]; [|0x0079|]; [|0x007a|]; [|0x007b|]; [|0x007c|]; [|0x007d|]; [|0x007e|]; [|0x00a8|]
|];

value oms_to_uc =
[|
  [|0x2212|]; [|0x00b7|]; [|0x00D7|]; [|0x204e|]; [|0x00f7|]; [|0x22c4|]; [|0x00b1|]; [|0x2213|];
  [|0x2295|]; [|0x2296|]; [|0x2297|]; [|0x2298|]; [|0x2299|]; [|0x0027|]; [|0x25e7|]; [|0x2022|];
  [||]; [|0x2261|]; [|0x2286|]; [|0x2287|]; [|0x2264|]; [|0x2265|]; [|0x227c|]; [|0x227d|];
  [||]; [|0x2245|]; [|0x2282|]; [|0x2283|]; [|0x226b|]; [|0x226a|]; [|0x227a|]; [|0x227b|];
  [|0x2190|]; [|0x2192|]; [|0x2191|]; [|0x2193|]; [|0x2194|]; [|0x2197|]; [|0x2198|]; [||];
  [|0x21d0|]; [|0x21d2|]; [|0x21d1|]; [|0x21d3|]; [|0x21d4|]; [|0x2196|]; [|0x2199|]; [|0x221d|];
  [|0x2032|]; [|0x221e|]; [|0x2208|]; [|0x220b|]; [|0x2206|]; [|0x2207|]; [|0x2215|]; [||];
  [|0x2200|]; [|0x2203|]; [|0x00ac|]; [|0x2205|]; [|0x211c|]; [|0x2111|]; [|0x22a4|]; [|0x22a5|];
  [|0xfb21|]; [|0x0041|]; [|0x0042|]; [|0x0043|]; [|0x0044|]; [|0x0045|]; [|0x0046|]; [|0x0047|];
  [|0x0048|]; [|0x0049|]; [|0x004a|]; [|0x004b|]; [|0x004c|]; [|0x004d|]; [|0x004e|]; [|0x004f|];
  [|0x0050|]; [|0x0051|]; [|0x0052|]; [|0x0053|]; [|0x0054|]; [|0x0055|]; [|0x0056|]; [|0x0057|];
  [|0x0058|]; [|0x0059|]; [|0x005a|]; [|0x222a|]; [|0x2229|]; [|0x228e|]; [|0x2227|]; [|0x2228|];
  [|0x22a2|]; [|0x22a3|]; [|0x230a|]; [|0x230b|]; [|0x2308|]; [|0x2309|]; [|0x007b|]; [|0x007d|];
  [|0x2329|]; [|0x232a|]; [|0x007c|]; [|0x2016|]; [|0x2195|]; [|0x21d5|]; [|0x005c|]; [|0x2240|];
  [|0x221a|]; [|0x2a3f|]; [|0x2207|]; [|0x222b|]; [|0x2294|]; [|0x2293|]; [|0x2291|]; [|0x2292|];
  [|0x00a7|]; [|0x2020|]; [|0x2021|]; [|0x00b6|]; [|0x2663|]; [|0x2662|]; [|0x2661|]; [|0x2660|]
|];

value oml_to_uc =
[|
  [|0x0393|]; [|0x0394|]; [|0x0398|]; [|0x039b|]; [|0x039e|]; [|0x03a0|]; [|0x03a3|]; [|0x03a5|];
  [|0x03a6|]; [|0x03a8|]; [|0x03a9|]; [|0x03b1|]; [|0x03b2|]; [|0x03b3|]; [|0x03b4|]; [|0x03b5|];
  [|0x03b6|]; [|0x03b7|]; [|0x03b8|]; [|0x03b9|]; [|0x03ba|]; [|0x03bb|]; [|0x03bc|]; [|0x03bd|];
  [|0x03be|]; [|0x03c0|]; [|0x03c1|]; [|0x03c3|]; [|0x03c4|]; [|0x03c5|]; [|0x03d5|]; [|0x03c7|];
  [|0x03c8|]; [|0x03c9|]; [|0x03f5|]; [|0x03d1|]; [|0x03d6|]; [|0x03f1|]; [|0x03c2|]; [|0x03c6|];
  [|0x21bc|]; [|0x21bd|]; [|0x21c0|]; [|0x21c1|]; [||]; [||]; [|0x22b3|]; [|0x22b2|];
  [|0x0030|]; [|0x0031|]; [|0x0032|]; [|0x0033|]; [|0x0034|]; [|0x0035|]; [|0x0036|]; [|0x0037|];
  [|0x0038|]; [|0x0039|]; [|0x002e|]; [|0x002c|]; [|0x003c|]; [|0x002f|]; [|0x003e|]; [|0x204e|];
  [|0x2202|]; [|0x0041|]; [|0x0042|]; [|0x0043|]; [|0x0044|]; [|0x0045|]; [|0x0046|]; [|0x0047|];
  [|0x0048|]; [|0x0049|]; [|0x004a|]; [|0x004b|]; [|0x004c|]; [|0x004d|]; [|0x004e|]; [|0x004f|];
  [|0x0050|]; [|0x0051|]; [|0x0052|]; [|0x0053|]; [|0x0054|]; [|0x0055|]; [|0x0056|]; [|0x0057|];
  [|0x0058|]; [|0x0059|]; [|0x005a|]; [|0x266d|]; [|0x255e|]; [|0x266f|]; [|0x2323|]; [|0x2322|];
  [|0x006c|]; [|0x0061|]; [|0x0062|]; [|0x0063|]; [|0x0064|]; [|0x0065|]; [|0x0066|]; [|0x0067|];
  [|0x0068|]; [|0x0069|]; [|0x006a|]; [|0x006b|]; [|0x006c|]; [|0x006d|]; [|0x006e|]; [|0x006f|];
  [|0x0070|]; [|0x0071|]; [|0x0072|]; [|0x0073|]; [|0x0074|]; [|0x0075|]; [|0x0076|]; [|0x0077|];
  [|0x0078|]; [|0x0079|]; [|0x007a|]; [|0x0131|]; [||]; [||]; [||]; [||]
|];

value fake_encoding map = do
{
  let cm = Charmap.create Undef;

  for g = 0 to Array.length map - 1 do
  {
    match map.(g) with
    [ [|c|] -> Charmap.set cm c (Simple g)
    | _     -> ()
    ]
  };

  cm
};

(*
  |charmap_encoding <map> <char>| is the value of the |get_glyph| member of fonts with
  explicit encoding table.
*)

value charmap_encoding map char = do
{
  match Charmap.lookup map char with
  [ Undef -> match Charmap.lookup fake char with
    [ Undef       -> Undef
    | Simple c    -> Charmap.lookup map c
    | Accent a b  -> match (Charmap.lookup map a, Charmap.lookup map b) with
      [ (Simple x, Simple y) -> Accent x y
      | _                    -> Undef
      ]
    | Sequence cs -> match
                       List.fold_right
                         (fun c (def, gs) ->
                           if not def then
                             (False, [])
                           else match Charmap.lookup map c with
                           [ Simple g -> (True, [g :: gs])
                           | _        -> (False, [])
                           ])
                         cs
                         (True, [])
        with
        [ (True, gs) -> Sequence gs
        | (False, _) -> Undef
        ]
    | Extendable _ _ _ _
    | Border _           -> assert False    (* there are no such entries in |fake| *)
    ]
  | g -> g
  ]
};

value array_decoding map glyph = do
{
  let lookup g =
    if g > 0 && g < Array.length map then
      Array.to_list map.(g)
    else
      [];

  Array.of_list (decode glyph)

  where rec decode glyph = match glyph with
  [ Undef              -> []
  | Border _           -> []
  | Simple g           -> lookup g
  | Accent a g         -> lookup a @ lookup g
  | Sequence gs        -> List.concat (List.map lookup gs)
  | Extendable t m b _ -> decode t @ decode m @ decode b
  ]
};

(* |raw_encoding <char>| is the value of the |get_glyph| member of fonts with raw encoding. *)

value raw_encoding _ = Undef;
value raw_decoding _ = [||];

(*
value raw_encoding char = Simple char;

value rec raw_decoding glyph = match glyph with
[ Undef              -> []
| Border _           -> []
| Simple g           -> [g]
| Accent a g         -> [a; g]
| Sequence gs        -> gs
| Extendable t m b _ -> raw_decoding t
                      @ raw_decoding m
                      @ raw_decoding b
];
*)

(* vim:set foldmarker=[\|,\|] foldmethod=marker: *)
