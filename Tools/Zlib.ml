
type zstream;

external set_input    : zstream -> string -> unit      = "zlib_set_input";
external get_output   : zstream -> string              = "zlib_get_output";
external avail_input  : zstream -> int                 = "zlib_avail_input";
external deflate_init : int -> int -> zstream          = "zlib_deflate_init";
external deflate      : zstream -> int -> unit         = "zlib_deflate";
external deflate_end  : zstream -> unit                = "zlib_deflate_end";
external inflate_init : int -> zstream                 = "zlib_inflate_init";
external inflate      : zstream -> int -> unit         = "zlib_inflate";
external inflate_end  : zstream -> unit                = "zlib_inflate_end";
external compress     : string -> string -> int        = "zlib_compress";
external compress2    : string -> string -> int -> int = "zlib_compress2";
external uncompress   : string -> string -> int        = "zlib_uncompress";
external adler32      : int32 -> string -> int32       = "zlib_adler32";
external crc32        : int32 -> string -> int32       = "zlib_crc32";

value no_flush   = 0;
value sync_flush = 2;
value full_flush = 3;
value finish     = 4;

