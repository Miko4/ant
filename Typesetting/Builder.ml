
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open GlyphMetric;
open FontMetric;
open Dim;
open Box;

type builder_interface =
{
  add_char  : uc_char -> unit;
  add_break : num -> bool -> list (char_item box box)
                          -> list (char_item box box)
                          -> list (char_item box box) -> unit;
  add_kern  : num -> num -> unit;
  add_box   : box -> unit;
  add_cmd   : box -> unit;

  set_font          : font_metric -> glyph_composer -> unit;
  set_hyphen_params : JustHyph.hyphen_params -> unit
};

type builder 'a = (builder_interface * (unit -> 'a));

value add_char  builder chr                      = builder.add_char chr;
value add_break builder penalty hyph pre post no = builder.add_break penalty hyph pre post no;
value add_kern  builder x y                      = builder.add_kern x y;
value add_box   builder box                      = builder.add_box  box;
value add_cmd   builder cmd                      = builder.add_cmd  cmd;
value set_font  builder font composer            = builder.set_font font composer;
value set_hyphen_params builder hyphen_params    = builder.set_hyphen_params hyphen_params;

value add_char_list builder chars = List.iter (fun c -> add_char builder c) chars;
value add_box_list  builder boxes = List.iter (add_box builder) boxes;
value add_cmd_list  builder boxes = List.iter (add_cmd builder) boxes;

(* A builder which throws away all boxes. *)

value void_builder =
{
  add_char  = fun _         -> ();
  add_break = fun _ _ _ _ _ -> ();
  add_kern  = fun _ _       -> ();
  add_box   = fun _         -> ();
  add_cmd   = fun _         -> ();
  set_font  = fun _ _       -> ();
  set_hyphen_params = fun _ -> ()
};

(* A simple builder which is just a wrapper around ListBuilder. *)

type simple_builder =
{
  boxes    : ListBuilder.builder box;
  font     : mutable font_metric;
  composer : mutable glyph_composer
};

value sb_add_char sb chr = do
{
  ListBuilder.add sb.boxes (new_char_box chr sb.font);
};

value sb_add_break sb p h pre post no = do
{
  ListBuilder.add sb.boxes
    (new_break_box
      p h
      (Array.to_list (Array.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer pre)))
      (Array.to_list (Array.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer post)))
      (Array.to_list (Array.map extended_item_to_box (JustHyph.convert_to_glyphs sb.font sb.composer no))))
  (* FIX: Do you really want to apply an arbitrary composer here,
     or should we take the identity function? *)
};

value sb_add_kern sb x y = do
{
  ListBuilder.add sb.boxes (new_kern_box x y)
};

value sb_add_box sb box = do
{
  ListBuilder.add sb.boxes box
};

value sb_set_font sb font composer = do
{
  sb.font     := font;
  sb.composer := composer   (* FIX: Should we allow changing the composer? *)
};

value sb_set_hyphen_params _ = ();

value sb_get sb () = do
{
  ListBuilder.get sb.boxes
};

value simple_builder font composer = do
{
  let b =
    {
      boxes    = ListBuilder.make ();
      font     = font;
      composer = composer (* FIX: should we fix this to  fun x -> x  ? *)
    };

  ({
      add_char  = sb_add_char  b;
      add_break = sb_add_break b;
      add_kern  = sb_add_kern  b;
      add_box   = sb_add_box   b;
      add_cmd   = sb_add_box   b;
      set_font  = sb_set_font  b;

      set_hyphen_params = sb_set_hyphen_params
    },
    (sb_get b)
  )
};

