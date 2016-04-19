
open XNum;
open Runtime;
open Unicode.Types;

value tracing_page_breaks : ref bool;

type area_params =
{
  galley      : uc_string;
  top_skip    : num;
  bottom_skip : num;
  min_size    : num;
  grid_size   : num            (* if non-zero then all y coordinates are rounded  *)
};

value contents_from_galley : area_params -> PageLayout.area_contents_function;

