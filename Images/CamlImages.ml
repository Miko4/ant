
module Images  = Images;
module Ps      = Ps;
module Index8  = Index8;
module Index16 = Index16;
module Color   = Color;
module Rgb24   = Rgb24;
module Rgba32  = Rgba32;
module Cmyk32  = Cmyk32;

value get_index8_colourmap x i = do
{
  let cm = x.Index8.colormap;
  (cm.Color.map.(i).Color.r,
   cm.Color.map.(i).Color.g,
   cm.Color.map.(i).Color.b)
};

