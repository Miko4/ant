
type io +'a =
{
  (* general *)
  io_free          : unit -> unit;
  (* reading *)
  io_read_char     : unit -> char;
  io_read_string   : int -> string;
  io_eof           : unit -> bool;
  (* writing *)
  io_write_char    : char -> unit;
  io_write_string  : string -> unit;
  io_bytes_written : unit -> int;
  (* random access *)
  io_size          : unit -> int;
  io_pos           : unit -> int;
  io_seek          : int -> unit;
};

type io_r   = [= `IO_Read];
type io_w   = [= `IO_Write];
type io_s   = [= `IO_Seek];
type io_rs  = [= io_r | io_s];
type io_ws  = [= io_w | io_s];
type io_rw  = [= io_r | io_w];
type io_rws = [= io_r | io_w | io_s];

type istream   = io io_r;
type irstream  = io io_rs;
type ostream   = io io_w;
type orstream  = io io_ws;
type iostream  = io io_rw;
type iorstream = io io_rws;

value not_implemented _ = assert False;

value io_make fr rc rs eof wc ws bw sz pos sk =
{
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
};

value io_make_read fr rc rs eof =
{
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = not_implemented;
  io_write_string  = not_implemented;
  io_bytes_written = not_implemented;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
};

value io_make_write fr wc ws bw =
{
  io_free          = fr;
  io_read_char     = not_implemented;
  io_read_string   = not_implemented;
  io_eof           = not_implemented;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
};

value io_make_read_seek fr rc rs eof sz pos sk =
{
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = not_implemented;
  io_write_string  = not_implemented;
  io_bytes_written = not_implemented;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
};

value io_make_write_seek fr wc ws bw sz pos sk =
{
  io_free          = fr;
  io_read_char     = not_implemented;
  io_read_string   = not_implemented;
  io_eof           = not_implemented;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = sz;
  io_pos           = pos;
  io_seek          = sk;
};

value io_make_read_write fr rc rs eof wc ws bw =
{
  io_free          = fr;
  io_read_char     = rc;
  io_read_string   = rs;
  io_eof           = eof;
  io_write_char    = wc;
  io_write_string  = ws;
  io_bytes_written = bw;
  io_size          = not_implemented;
  io_pos           = not_implemented;
  io_seek          = not_implemented;
};

value io_free          cs     = cs.io_free          ();
value io_read_char     cs     = cs.io_read_char     ();
value io_read_string   cs len = cs.io_read_string  len;
value io_eof           cs     = cs.io_eof           ();
value io_write_char    cs chr = cs.io_write_char   chr;
value io_write_string  cs str = cs.io_write_string str;
value io_bytes_written cs     = cs.io_bytes_written ();
value io_size          cs     = cs.io_size          ();
value io_pos           cs     = cs.io_pos           ();
value io_seek          cs off = cs.io_seek         off;

value io_coerce_i   cs = (cs :> istream);
value io_coerce_o   cs = (cs :> ostream);
value io_coerce_ir  cs = (cs :> irstream);
value io_coerce_or  cs = (cs :> orstream);
value io_coerce_io  cs = (cs :> iostream);
value io_coerce_ior cs = (cs :> iorstream);

