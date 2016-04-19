
open XNum;
open Unicode.Types;

type format =
[ Bitmap
| Bmp
| PostScript
| PDF
];

type bitmap_format =
[ Indexed of string
| RGB
| RGBA
| CMYK
];

type bitmap =
{
  bm_format   : bitmap_format;
  bm_width    : int;
  bm_height   : int;
  bm_depth    : int;
  bm_scanline : int -> string
};

value get_dimensions   : string -> int -> (format * num * num * num);
value get_bounding_box : string -> option (int * int * int * int);

value read_bitmap : string -> bitmap;

