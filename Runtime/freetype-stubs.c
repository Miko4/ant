#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include FT_TYPE1_TABLES_H

static FT_Library library;

CAMLprim value Wrapper_FT_Init_FreeType ()
{
  CAMLparam0();

  if (FT_Init_FreeType(&library))
    failwith("FT_Init_FreeType");

  CAMLreturn(Val_unit);
};

/* faces */

void finalize_face(value face)
{
  FT_Done_Face(*(FT_Face *)Data_custom_val(face));
};

static struct custom_operations face_ops =
{
  "freetype face",
  finalize_face,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value Wrapper_FT_New_Face(value filename)
{
  CAMLparam1(filename);
  CAMLlocal1(block);
  FT_Face *face;

  block = alloc_custom(&face_ops, sizeof(FT_Face), 0, 1);

  face = (FT_Face *)Data_custom_val(block);

  if (FT_New_Face(library, String_val(filename), 0, face))
    failwith("FT_New_Face");

  CAMLreturn(block);
};

CAMLprim value Wrapper_FT_Attach_File(value face, value filename)
{
  CAMLparam2(face, filename);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  CAMLreturn(Val_int(FT_Attach_File(f, String_val(filename))));
};

CAMLprim value Wrapper_FT_Set_Char_Size(value face, value size, value dpi)
{
  CAMLparam3(face, size, dpi);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  if (FT_Set_Char_Size(f, 0, Int_val(size), Int_val(dpi), Int_val(dpi)))
    failwith("FT_Set_Char_Size");

  CAMLreturn(Val_unit);
};

CAMLprim value Wrapper_FT_Get_Char_Index(value face, value charcode)
{
  CAMLparam2(face, charcode);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  CAMLreturn(Val_int(FT_Get_Char_Index(f, Int_val(charcode))));
};

CAMLprim value Wrapper_FT_Get_First_Char(value face)
{
  CAMLparam1(face);
  CAMLlocal1(block);
  FT_Face f;
  FT_ULong char_code;
  FT_UInt  glyph;

  f = *(FT_Face *)Data_custom_val(face);

  char_code = FT_Get_First_Char (f, &glyph);

  block = alloc_tuple(2);

  Store_field(block, 0, Val_int(char_code));
  Store_field(block, 1, Val_int(glyph));

  CAMLreturn(block);
};

CAMLprim value Wrapper_FT_Get_Next_Char(value face, value glyph)
{
  CAMLparam2(face, glyph);
  CAMLlocal1(block);
  FT_Face f;
  FT_ULong char_code;
  FT_UInt  g;

  f = *(FT_Face *)Data_custom_val(face);

  char_code = Int_val(Field(glyph, 0));
  g         = Int_val(Field(glyph, 1));

  char_code = FT_Get_Next_Char (f, char_code, &g);

  block = alloc_tuple(2);

  Store_field(block, 0, Val_int(char_code));
  Store_field(block, 1, Val_int(g));

  CAMLreturn(block);
};

CAMLprim value Wrapper_FT_Load_Glyph(value face, value glyph_index, value load_flags)
{
  CAMLparam3(face, glyph_index, load_flags);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  if (FT_Load_Glyph(f, Int_val(glyph_index), Int_val(load_flags)))
    failwith("FT_Load_Glyph");

  CAMLreturn(Val_unit);
};

CAMLprim value Wrapper_FT_Get_Kerning(value face, value left, value right, value mode)
{
  CAMLparam4(face, left, right, mode);
  CAMLlocal1(block);
  FT_Face   f;
  FT_Vector kern;

  f = *(FT_Face *)Data_custom_val(face);

  if (FT_Get_Kerning(f, Int_val(left), Int_val(right), Int_val(mode), &kern))
    failwith("FT_Get_Kern");

  block = alloc_tuple(2);

  Store_field(block, 0, Val_int(kern.x));
  Store_field(block, 1, Val_int(kern.y));

  CAMLreturn(block);
};

CAMLprim value Wrapper_FT_Get_Postscript_Name(value face)
{
  CAMLparam1(face);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  CAMLreturn(copy_string(FT_Get_Postscript_Name(f)));
};

CAMLprim value face_num_glyphs(value face)
{
  CAMLparam1(face);
  FT_Face f;

  f = *(FT_Face *)Data_custom_val(face);

  CAMLreturn(Val_int(f->num_glyphs));
};

CAMLprim value face_glyph(value face)
{
  CAMLparam1(face);
  CAMLlocal1(block);
  FT_Face      f;
  FT_GlyphSlot *g;

  f = *(FT_Face *)Data_custom_val(face);

  block = alloc(sizeof(FT_GlyphSlot), Abstract_tag);

  g = (FT_GlyphSlot *)Data_custom_val(block);

  *g = f->glyph;

  CAMLreturn(block);
};

CAMLprim value face_metrics(value face)
{
  CAMLparam1(face);
  CAMLlocal1(block);
  FT_Face      f;

  f = *(FT_Face *)Data_custom_val(face);

  block = alloc_tuple(6);

  Store_field(block, 0, Val_int(f->units_per_EM));
  Store_field(block, 1, Val_int(f->ascender));
  Store_field(block, 2, Val_int(f->descender));
  Store_field(block, 3, Val_int(f->height));
  Store_field(block, 4, Val_int(f->underline_position));
  Store_field(block, 5, Val_int(f->underline_thickness));

  CAMLreturn(block);
};

CAMLprim value glyph_metrics(value glyph)
{
  CAMLparam1(glyph);
  CAMLlocal1(block);
  FT_GlyphSlot g;

  g = *(FT_GlyphSlot *)Data_custom_val(glyph);

  block = alloc_tuple(5);

  Store_field(block, 0, Val_int(g->metrics.width));
  Store_field(block, 1, Val_int(g->metrics.height));
  Store_field(block, 2, Val_int(g->metrics.horiBearingX));
  Store_field(block, 3, Val_int(g->metrics.horiBearingY));
  Store_field(block, 4, Val_int(g->metrics.horiAdvance));

  CAMLreturn(block);
};

CAMLprim value get_glyph_name(value f, value glyph)
{
  CAMLparam2(f, glyph);
  FT_Face face;
  static char buffer[256];

  face = *(FT_Face *)Data_custom_val(f);

  if (FT_Get_Glyph_Name(face, Int_val(glyph), buffer, 256))
    failwith("get_glyph_name");

  CAMLreturn(copy_string(buffer));
};

CAMLprim value glyph_to_bitmap(value glyph)
{
  CAMLparam1(glyph);
  CAMLlocal2(block, buffer);
  FT_GlyphSlot   slot;
  FT_Glyph       g;
  FT_BitmapGlyph bm;
  size_t         pitch;
  size_t         new_pitch;
  int i;

  slot = *(FT_GlyphSlot *)Data_custom_val(glyph);

  if (FT_Get_Glyph(slot, &g))
    failwith("glyph_to_bitmap");

  if (g->format != FT_GLYPH_FORMAT_BITMAP)
  {
    if (FT_Glyph_To_Bitmap(&g, FT_RENDER_MODE_MONO, 0, 1))
    {
      FT_Done_Glyph(g);
      failwith("glyph_to_bitmap");
    }
  }

  bm = (FT_BitmapGlyph)g;

  pitch     = abs(bm->bitmap.pitch);
  new_pitch = (bm->bitmap.width + 7) / 8;

  block  = alloc_tuple(6);
  buffer = alloc_string(bm->bitmap.rows * new_pitch);

  if (bm->bitmap.pitch >= 0)
  {
    for (i = 0; i < bm->bitmap.rows; i++)
      memcpy(String_val(buffer) + i * new_pitch,
             bm->bitmap.buffer + i * pitch,
             new_pitch);
  }
  else
  {
    for (i = 0; i < bm->bitmap.rows; i++)
      memcpy(String_val(buffer) + i * new_pitch,
             bm->bitmap.buffer + (bm->bitmap.rows - i) * pitch,
             new_pitch);
  }

  Store_field(block, 0, Val_int(bm->left));
  Store_field(block, 1, Val_int(bm->top));
  Store_field(block, 2, Val_int(bm->bitmap.rows));
  Store_field(block, 3, Val_int(bm->bitmap.width));
  Store_field(block, 4, Val_int(new_pitch));
  Store_field(block, 5, buffer);

  FT_Done_Glyph(g);

  CAMLreturn(block);
};

CAMLprim value is_sfnt(value f)
{
  CAMLparam1(f);
  FT_Face face;

  face = *(FT_Face *)Data_custom_val(f);

  CAMLreturn(Val_bool(FT_IS_SFNT(face) != 0));
};

/* Hack: In order to find out whether the font is PostScript,
 * we call FT_Get_PS_Font_Private and check for an error. */

CAMLprim value is_postscript(value f)
{
  CAMLparam1(f);
  FT_Face face;
  PS_PrivateRec p;

  face = *(FT_Face *)Data_custom_val(f);

  CAMLreturn(Val_bool(!FT_IS_SFNT(face) &&
                      (FT_Get_PS_Font_Private(face, &p) == 0)));
};

CAMLprim value has_ps_glyph_names(value f)
{
  CAMLparam1(f);
  FT_Face face;

  face = *(FT_Face *)Data_custom_val(f);

  CAMLreturn(Val_bool(FT_Has_PS_Glyph_Names(face) != 0));
};

