
open Unicode.Types;

type glyph =
{
  g_glyph  : int;
  g_width  : int;
  g_height : int;
  g_depth  : int;
  g_hdpp   : float;
  g_vdpp   : float;
  g_min_x  : int;
  g_min_y  : int;
  g_max_x  : int;
  g_max_y  : int;
  g_bitmap : Bitmap.bitmap
};

value empty_glyph =
{
  g_glyph  = -1;
  g_width  = 0;
  g_height = 0;
  g_depth  = 0;
  g_hdpp   = 1.0;
  g_vdpp   = 1.0;
  g_min_x  = 0;
  g_min_y  = 0;
  g_max_x  = -1;
  g_max_y  = -1;
  g_bitmap = Bitmap.make 0 0
};

value make c w h d hdpp vdpp (min_x, min_y) (max_x, max_y) =
{
  g_glyph  = c;
  g_width  = w;
  g_height = h;
  g_depth  = d;
  g_hdpp   = hdpp;
  g_vdpp   = vdpp;
  g_min_x  = min_x;
  g_min_y  = min_y;
  g_max_x  = max_x;
  g_max_y  = max_y;
  g_bitmap = Bitmap.make (max_x - min_x + 1) (max_y - min_y + 1)
};

value point g x y = do
{
  Bitmap.point g.g_bitmap (x - g.g_min_x) (g.g_max_y - y)
};

value set_point g x y = do
{
  Bitmap.set_point g.g_bitmap (x - g.g_min_x) (g.g_max_y - y)
};

value unset_point g x y = do
{
  Bitmap.unset_point g.g_bitmap (x - g.g_min_x) (g.g_max_y - y)
};

value set_line g x1 x2 y = do
{
  Bitmap.set_line g.g_bitmap (x1 - g.g_min_x) (x2 - g.g_min_x) (g.g_max_y - y)
};

value unset_line g x1 x2 y = do
{
  Bitmap.unset_line g.g_bitmap (x1 - g.g_min_x) (x2 - g.g_min_x) (g.g_max_y - y)
};

value copy_line g y1 y2 = do
{
  Bitmap.copy_line g.g_bitmap (g.g_max_y - y1) (g.g_max_y - y2)
};

value print io g black white end_line = do
{
  Bitmap.print io g.g_bitmap black white end_line
};

