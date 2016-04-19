
open Runtime;
open Logging;

type output_format = [ DVI | XDVI | PDF | PS | SVG ];

type job =
{
  time               : Unix.tm;
  argv               : array string;
  output_format      : output_format;
  source_specials    : bool;
  jobname            : string;
  input_file         : string;
  output_file        : string;
  src_special_file   : string;
  log_file           : string;
  src_special_stream : IO.ostream
};

value empty =
{
  time               = Unix.localtime (Unix.time ());
  argv               = Sys.argv;
  output_format      = PDF;
  source_specials    = False;
  jobname            = "";
  input_file         = "";
  output_file        = "";
  src_special_file   = "";
  log_file           = "";
  src_special_stream = IO.coerce_o (IO.make_buffer_stream 10)
};

value create name fmt src_spec = do
{
  let basename = do
  {
    try
      String.sub name 0 (String.rindex name '.')
    with
    [ Not_found -> name ]
  };

  KPathSea.init Sys.argv.(0) !FontMetric.default_bitmap_resolution !FontMetric.default_mf_mode;
  FreeType.ft_init_freetype ();

  log_open (basename ^ ".log");

  {
    time               = Unix.localtime (Unix.time ());
    argv               = Sys.argv;
    output_format      = fmt;
    source_specials    = src_spec;
    jobname            = basename;
    input_file         = name;
    output_file        = match fmt with
                         [ DVI  -> basename ^ ".dvi"
                         | XDVI -> basename ^ ".xdvi"
                         | PDF  -> basename ^ ".pdf"
                         | PS   -> basename ^ ".ps"
                         | SVG  -> basename ^ ".svg"
                         ];
    src_special_file   = basename ^ ".pdfsync";
    log_file           = basename ^ ".log";
    src_special_stream = if fmt <> PDF || not src_spec then
                           IO.coerce_o (IO.make_buffer_stream 10)
                         else do
                         {
                           let os = IO.make_out_stream (basename ^ ".pdfsync");
                           IO.write_string os basename;
                           IO.write_string os "\nversion 0\n";
                           os
                         }
  }
};

