
type file_type =
[= `PK
|  `Source
|  `TeX
|  `TFM
|  `Type1
|  `TrueType
|  `OpenType
];

value init       : string -> int -> string -> unit;
value find_file  : string -> file_type -> bool -> string;
value find_glyph : string -> int -> string;

