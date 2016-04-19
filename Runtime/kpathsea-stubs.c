
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdio.h>
#include <kpathsea/c-auto.h>
#include <kpathsea/progname.h>
#include <kpathsea/proginit.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/tex-glyph.h>

CAMLprim value kpse_init_prog_wrapper(value name, value dpi, value mode, value alt_font)
{
  CAMLparam4(name, dpi, mode, alt_font);

  kpse_init_prog(String_val(name), Int_val(dpi), String_val(mode), String_val(alt_font));

  CAMLreturn(Val_unit);
}

CAMLprim value kpse_set_program_name_wrapper(value argv0, value progname)
{
  CAMLparam2(argv0, progname);

  kpse_set_program_name(String_val(argv0), String_val(progname));

  putenv("SELFAUTOLOC="    SELFAUTOLOC);
  putenv("SELFAUTODIR="    SELFAUTODIR);
  putenv("SELFAUTOPARENT=" SELFAUTOPARENT);

  CAMLreturn(Val_unit);
}

CAMLprim value kpse_set_program_enabled_wrapper(value fmt, value val, value level)
{
  CAMLparam3(fmt, val, level);

  kpse_set_program_enabled(Int_val(fmt), Int_val(val), Int_val(level));

  CAMLreturn(Val_unit);
}

CAMLprim value kpse_find_file_wrapper(value name, value format, value must_exists)
{
  CAMLparam3(name, format, must_exists);

  char *filename = kpse_find_file(String_val(name), Int_val(format), Bool_val(must_exists));

  if (!filename)
    filename = "";

  CAMLreturn(copy_string(filename));
}

CAMLprim value kpse_find_glyph_wrapper(value name, value dpi, value format)
{
  CAMLparam3(name, dpi, format);

  kpse_glyph_file_type glyph_file;

  char *filename = kpse_find_glyph(String_val(name), Int_val(dpi), Int_val(format), &glyph_file);

  if (!filename)
    filename = "";

  CAMLreturn(copy_string(filename));
}

