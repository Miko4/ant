
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open FontMetric;
open Typesetting;
open Box;

type font_definition =
{
  fd_name         : uc_string;
  fd_family       : uc_string;
  fd_series       : uc_string;
  fd_shape        : uc_string;
  fd_min_size     : num;
  fd_max_size     : num;
  fd_loaded_sizes : mutable list (num * font_metric);
  fd_data         : font_load_params
};

type font =
{
  f_font_def : font_definition;
  f_metric   : font_metric;
  f_size     : num
};

type font_table;

value get_font     : font_table -> uc_string -> uc_string -> uc_string -> num -> option font;
value declare_font : font_table -> uc_string -> uc_string -> uc_string ->
                     uc_string -> (num * num) -> font_load_params -> font_table;

value initialise_font_table : unit -> font_table;

value make_virtual_font : string -> num -> array box -> array num -> list ((int * int) * GlyphMetric.lig_kern) ->
                          array num -> font_metric;

