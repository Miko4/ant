
open XNum;
open FontMetric;

value read_pk_font : font_metric -> int -> option ((num * num * num) * array GlyphBitmap.glyph);

