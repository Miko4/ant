
(* Interface to the kpathsea library. *)

type file_type =
[= `PK
|  `Source
|  `TeX
|  `TFM
|  `Type1
|  `TrueType
|  `OpenType
];

value pk_format       =  1;
value tfm_format      =  3;
value afm_format      =  4;
value tex_format      = 26;
value type1_format    = 32;
value truetype_format = 36;
value opentype_format = 47;

external init_prog           : string -> int -> string -> string -> unit = "kpse_init_prog_wrapper";
external set_program_name    : string -> string -> unit                  = "kpse_set_program_name_wrapper";
external set_program_enabled : int -> int -> int -> unit                 = "kpse_set_program_enabled_wrapper";
external kpse_find_file      : string -> int -> bool -> string           = "kpse_find_file_wrapper";
external kpse_find_glyph     : string -> int -> int -> string            = "kpse_find_glyph_wrapper";

value init progname dpi mode = do
{
  set_program_name progname "ant";
  init_prog "ANT" dpi mode "cmr10";
  set_program_enabled pk_format  1 1;
  set_program_enabled tfm_format 1 1;
  set_program_enabled tex_format 0 1
};

value find_file name file_type must_exists = match file_type with
[ `PK       -> kpse_find_file name pk_format       must_exists
| `TFM      -> kpse_find_file name tfm_format      must_exists
| `TeX      -> kpse_find_file name tex_format      must_exists
| `Type1    -> kpse_find_file name type1_format    must_exists
| `TrueType -> kpse_find_file name truetype_format must_exists
| `OpenType -> kpse_find_file name opentype_format must_exists
| _         -> name
];

value find_glyph name dpi = kpse_find_glyph name dpi pk_format;

