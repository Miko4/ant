
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

value empty  : job;

value create : string -> output_format -> bool -> job;

