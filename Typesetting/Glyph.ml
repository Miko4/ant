
open XNum;
open Runtime;
open Unicode.Types;
open Dim;
open Substitute;
open GlyphMetric;
open FontMetric;
open Box;

(*
  |attach_accent <acc-font> <acc> <chr-font> <chr>| attaches the character <acc> of <acc-font> to
  <chr> as accent.
*)

value attach_accent acc_font acc chr_font chr = do
{
  let acc_gm = get_glyph_metric acc_font acc;
  let chr_gm = get_glyph_metric chr_font chr;

  let (pos_x, pos_y) = accent_position acc_font acc_gm chr_font chr_gm;
  let height = max_num chr_gm.gm_height (acc_gm.gm_height +/ pos_y);
  let depth  = max_num chr_gm.gm_depth  (acc_gm.gm_depth  -/ pos_y);

  new_compound_box
    (fixed_dim chr_gm.gm_width)
    (fixed_dim height)
    (fixed_dim depth)
    [Graphic.PutBox dim_zero          dim_zero          (new_glyph_box chr chr_font);
     Graphic.PutBox (fixed_dim pos_x) (fixed_dim pos_y) (new_glyph_box acc acc_font)]
};

(*
  |vertical_extendable <height> <font> <top> <mid> <bot> <rep>| assembles an extendable glyph
  of the given height from <top>, <mid>, <bot>, and <rep>.
*)

value vertical_extendable height font top mid bot rep = do
{
  let top_gm = get_glyph_metric font top;
  let mid_gm = get_glyph_metric font mid;
  let bot_gm = get_glyph_metric font bot;
  let rep_gm = get_glyph_metric font rep;

  let total_height gm = gm.gm_height +/ gm.gm_depth;

  let top_height = total_height top_gm;
  let mid_height = total_height mid_gm;
  let bot_height = total_height bot_gm;
  let rep_height = total_height rep_gm;
  let min_height = top_height +/ mid_height +/ bot_height;
  let num_reps   = if rep_height = num_zero then
                     0
                   else do
                   {
                     let delta = height -/ min_height;
                     let num   = int_of_num (ceiling_num (delta // rep_height));

                     if mid = Undef then
                       num
                     else
                      (num + 1) / 2
                   };
  let rep_list  = XList.repeat num_reps (new_glyph_box rep font);

  (VBox.make
    ( (if top <> Undef then [new_glyph_box top font] else [])
    @ rep_list
    @ (if mid <> Undef then [new_glyph_box mid font :: rep_list] else [])
    @ (if bot <> Undef then [new_glyph_box bot font] else [])
    )
  )
};

(*
  |horizontal_extendable <width> <font> <left> <mid> <right> <rep>| assembles an extendable glyph
  of the given width from <left>, <mid>, <right>, and <rep>.
*)

value horizontal_extendable width font left mid right rep = do
{
  let left_gm  = get_glyph_metric font left;
  let mid_gm   = get_glyph_metric font mid;
  let right_gm = get_glyph_metric font right;
  let rep_gm   = get_glyph_metric font rep;

  let left_width  = left_gm.gm_width;
  let mid_width   = mid_gm.gm_width;
  let right_width = right_gm.gm_width;
  let rep_width   = rep_gm.gm_width;
  let min_width   = left_width +/ mid_width +/ right_width;
  let num_reps    = if rep_width = num_zero then
                      0
                    else do
                    {
                      let delta = width -/ min_width;
                      let num   = int_of_num (ceiling_num (delta // rep_width));

                      if mid = Undef then
                        num
                      else
                       (num + 1) / 2
                    };
  let rep_list    = XList.repeat num_reps (new_glyph_box rep font);

  (HBox.make HBox.LR
    ( (if left  <> Undef then [new_glyph_box left  font] else [])
    @ rep_list
    @ (if mid   <> Undef then [new_glyph_box mid   font] else [])
    @ rep_list
    @ (if right <> Undef then [new_glyph_box right font] else [])
    )
  )
};
