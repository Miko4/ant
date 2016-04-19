
open XNum;
open Runtime;
open Unicode.Types;
open Logging;

value calc_momentum weight base glyph = do
{
  iter_x glyph.Glyph.min_x num_zero

  where rec iter_x x sum = do
  {
    if x > glyph.Glyph.max_x then
      sum
    else
      iter_y glyph.Glyph.min_y 0

      where rec iter_y y n = do
      {
        if y > glyph.Glyph.max_y then
          if x <= base then
            iter_x (x + 1) (sum -/ num_of_int n */ weight (base - x))
          else
            iter_x (x + 1) (sum +/ num_of_int n */ weight (x - base))
        else
          if Glyph.point glyph x y then
            iter_y (y + 1) (n + 1)
          else
            iter_y (y + 1) n
      }
  }
};

value calc_center weight glyph = do
{
  (* binary search for the center *)

  iter glyph.Glyph.min_x glyph.Glyph.max_x

  where rec iter left right = do
  {
    if right <= left + 1 then
      if calc_momentum weight left glyph +/ calc_momentum weight right glyph <=/ num_zero then
        left
      else
        right
    else do
    {
      let m = (left + right) / 2;

      if calc_momentum weight m glyph <=/ num_zero then
        iter left m
      else
        iter m right
    }
  }
};

(*
;(define (calc-center weight inv-weight glyph)
;  (let ((width  (+ 1 (- (glyph:max-x glyph) (glyph:min-x glyph))))
;        (height (+ 1 (- (glyph:max-y glyph) (glyph:min-y glyph)))))
;    (let iter-columns ((x (glyph:min-x glyph)) (sum 0))
;      (if (> x (glyph:max-x glyph))
;          (+ (inv-weight (/ sum ( * width height)))
;             (glyph:min-x glyph))
;          (let iter-lines ((y (glyph:min-y glyph)) (n 0))
;            (if (> y (glyph:max-y glyph))
;                (iter-columns (+ x 1) (+ sum ( * n (weight (- x (glyph:min-x glyph))))))
;                (if (glyph:point glyph x y)
;                  (iter-lines (+ y 1) (+ n 1))
;                  (iter-lines (+ y 1) n)
;                )
;            )
;          )
;      )
;    )
;  )
;)
*)

value calc_left weight inv_weight center glyph = do
{
  let width  = glyph.Glyph.max_x - glyph.Glyph.min_x + 1;
  let height = glyph.Glyph.max_y - glyph.Glyph.min_y + 1;

  iter_x (center - 1) num_zero

  where rec iter_x x sum = do
  {
    if x < glyph.Glyph.min_x then
      center - inv_weight (sum // num_of_int (width * height))
    else
      iter_y glyph.Glyph.min_y 0

      where rec iter_y y n = do
      {
        if y > glyph.Glyph.max_y then
          iter_x (x - 1) (sum +/ num_of_int n */ weight (center - x))
        else
          if Glyph.point glyph x y then
            iter_y (y + 1) (n + 1)
          else
            iter_y (y + 1) n
      }
  }
};

value calc_right weight inv_weight center glyph = do
{
  let width  = glyph.Glyph.max_x - glyph.Glyph.min_x + 1;
  let height = glyph.Glyph.max_y - glyph.Glyph.min_y + 1;

  iter_x (center - 1) num_zero

  where rec iter_x x sum = do
  {
    if x < glyph.Glyph.min_x then
      center + inv_weight (sum // num_of_int (width * height))
    else
      iter_y glyph.Glyph.min_y 0

      where rec iter_y y n = do
      {
        if y > glyph.Glyph.max_y then
          iter_x (x - 1) (sum +/ num_of_int n */ weight (x - center))
        else
          if Glyph.point glyph x y then
            iter_y (y + 1) (n + 1)
          else
            iter_y (y + 1) n
      }
  }
};

(*
  |calc_left_border <glyph> and |calc_right_border <glyph>| return a list containing for each line
  the number of white pixels at the left and right border of the <glyph>.
*)

value calc_left_border glyph = do
{
  iter_y glyph.Glyph.min_y []

  where rec iter_y y border = do
  {
    if y > glyph.Glyph.max_y then
      List.rev border
    else
      iter_x glyph.Glyph.min_x

      where rec iter_x x = do
      {
        if x > glyph.Glyph.max_x || Glyph.point glyph x y then
          iter_y (y + 1) [x :: border]
        else
          iter_x (x + 1)
      }
  }
};

value calc_right_border glyph = do
{
  let width = glyph.Glyph.width - 1;

  iter_y glyph.Glyph.min_y []

  where rec iter_y y border = do
  {
    if y > glyph.Glyph.max_y then
      List.rev border
    else
      iter_x glyph.Glyph.max_x

      where rec iter_x x = do
      {
        if x < glyph.Glyph.min_x || Glyph.point glyph x y then
          iter_y (y + 1) [width - x :: border]
        else
          iter_x (x - 1)
      }
  }
};

(* |calc_projection <max-x> <border>| counts the number of black pixels in each column. *)

value calc_projection max_x border = do
{
  iter max_x []

  where rec iter x result = do
  {
    let num = List.length (List.filter (fun z -> z <= x) border);

    if num = 0 then
      (x + 1, result)
    else
      iter (x - 1) [num :: result]
  }
};

(*
  |calc_protuding_area <weight> <projection>| calculates the area of black pixels to the outside of
  each column. The <weight> parameter is a function which provides a weight for a pixel depending on
  the amount it protrudes. |calc_border_diff <delta> <projection>| calculates, for each~$i$, the differences
  between the $(i + \delta)$-th and the $i$-th element of <projection>.
*)

value calc_protuding_area weight projection = do
{
  iter [] projection

  where rec iter out p = match p with
  [ []      -> []
  | [n::ns] -> iter_sum num_zero 0 [n :: out]

               where rec iter_sum sum x l = match l with
               [ []      -> [sum :: iter [n :: out] ns]
               | [m::ms] -> iter_sum (sum +/ weight x m) (x+1) ms
               ]
  ]
};

value calc_border_diff delta projection = do
{
  iter (XList.repeat delta 0) projection

  where rec iter prev l = match l with
  [ []      -> []
  | [n::ns] -> [n - List.nth prev (delta - 1)
                 :: iter [n :: prev] ns]
  ]
};

(*
  |calc_kern <hppp> <vppp> <glyph> <border>| determines the position of the border of a glyph.
  It depends on (1)~the amount of black pixels protruding the border and (2)~the occurance of long
  vertical contours.
*)

value calc_kern hppp vppp glyph border = do
{
  let find_pos cmp_gt threshold l = do
  {
    iter 0 l

    where rec iter pos l = match l with
    [ []      -> pos
    | [n::ns] -> if cmp_gt n threshold then
                   pos
                 else
                   iter (pos+1) ns
    ]
  };

  let (first, numbers) = calc_projection glyph.Glyph.width border;
  let area_threshold   = hppp */ vppp;
  let area_weight x n  = num_of_int n */ num_of_int (x + 1) // hppp;
  let area             = calc_protuding_area area_weight numbers;
  let area_pos         = find_pos gt_num area_threshold area;
  let diff_order       = hppp // num_of_int 8;
  let diff_threshold   = int_of_num (round_num (num_of_int 2 */ vppp));
  let diff             = calc_border_diff (int_of_num (round_num diff_order)) numbers;
  let diff_pos         = find_pos \> diff_threshold diff;

  first + min area_pos diff_pos
};

value calc_left_kern  hppp vppp glyph = calc_kern hppp vppp glyph (calc_left_border  glyph);
value calc_right_kern hppp vppp glyph = calc_kern hppp vppp glyph (calc_right_border glyph);

(*
  |output_lrcodes <left-kern> <right-kern> <glyphs>| outputs a list of |\char`\\lpcode| and
  |\char`\\rpcode| commands for pdf\TeX.
*)

value output_lrcodes left_kern right_kern glyphs = do
{
  let std_l = List.nth left_kern  77;
  let std_r = List.nth right_kern 77;
  let std_g = List.nth glyphs     77;

  iter left_kern right_kern glyphs

  where rec iter lk rk glyphs = match glyphs with
  [ []      -> ()
  | [g::gs] -> do
    {
      let l = List.hd lk;
      let r = List.hd rk;

      log_string "\\lpcode\\font ";
      log_int    g.Glyph.char;
      log_string "=";
      log_int    (int_of_num (round_num (num_of_int 1000 */ num_of_int (l - std_l)
                                          // num_of_int g.Glyph.width)));
      log_string "\\rpcode\\currentfont";
      log_int    g.Glyph.char;
      log_string "=";
      log_int    (int_of_num (round_num (num_of_int 1000 */ num_of_int (r - std_r)
                                          // num_of_int g.Glyph.width)));
      log_string "\n";

      iter (List.tl lk) (List.tl rk) gs
    }
  ]
};

value output_margin_test left_kern right_kern glyphs = do
{
  let output_line l r g = do
  {
    log_string "\\hbox to \\linewidth{\\hbox to 1mm{}\\kern ";
    log_num    (num_of_ints (~-l) 600);
    log_string "in \\char";
    log_int    g.Glyph.char;
    log_string "x\\,\\leaders\\hbox{\\,i\\,}\\hfill x\\char";
    log_int    g.Glyph.char;
    log_string "\\kern ";
    log_num    (num_of_ints (~-r) 600);
    log_string "in\\hbox to 1mm{}}\n"
  };

  log_string "\\documentclass[a4paper,10pt]{article}\n";
  log_string "\\usepackage{geometry,multicol}\n";
  log_string "%\\geometry{left=1cm, textwidth=2cm, top=1cm, bottom=1cm}\n";
  log_string "\\parindent0pt\n";
  log_string "\\begin{document}%\n";
  log_string "\\begin{multicols}{5}%\n";

  let fill_l = List.nth left_kern  77;
  let fill_r = List.nth right_kern 77;
  let fill_g = List.nth glyphs     77;

  iter left_kern right_kern glyphs

  where rec iter lk rk glyphs = match glyphs with
  [ []      -> log_string "\\end{multicols}\n\\end{document}\n"
  | [g::gs] -> do
    {
      output_line (List.hd lk) (List.hd rk) g;
      output_line fill_l fill_r fill_g;
      iter (List.tl lk) (List.tl rk) gs
    }
  ]
};

value rec draw_chart l = match l with
[ []      -> log_string "\\par\\vskip 5mm\\hskip 0mm "
| [x::xs] -> do
  {
    log_string "\\vrule width 2mm depth 0mm height ";
    log_num    (num_of_int 2 */ x);
    log_string "mm\n";
    draw_chart xs
  }
];

value draw_glyph glyph = do
{
  log_string "\\vbox{%\n\\hbox{";
  log_uni_string
    (Glyph.print
      glyph
      (string_to_list "\\1")
      (string_to_list "\\0")
      (string_to_list "}%\n\\hbox{")
    );
  log_string "}}\n"
};

value output_glyphs lk rk glyphs = do
{
  let draw_line x g = do
  {
    log_string "  \\hbox to 0pt{\\hskip ";
    log_num    (num_of_ints 13 10 */ num_of_int (x - g.Glyph.min_x));
    log_string "mm\\vrule width 0.1mm height 0pt depth ";
    log_num    (num_of_ints 13 10 */ num_of_int (g.Glyph.max_y - g.Glyph.min_y + 1));
    log_string "mm\\hss}%\n"
  };

  let pow4 x = num_of_int x */ num_of_int x */ num_of_int x */ num_of_int x;

  log_string "\\documentclass[a4paper,10pt]{article}\n";
  log_string "\\usepackage{geometry}\n";
  log_string "\\geometry{left=1cm, right=1cm, top=1cm, bottom=1cm}\n";
  log_string "\\parindent0pt\n";
  log_string "\\begin{document}\n";
  log_string "\\baselineskip=0pt\n";
  log_string "\\lineskip=0pt\n";
  log_string "\\lineskiplimit=0pt\n";
  log_string "\\def\\1{\\vbox to 1.3mm{\\vss\\hbox to 1.3mm{\\hss\\tiny$\\bullet$\\hss}\\vss}}\n";
  log_string "\\def\\0{\\vbox to 1.3mm{\\hbox to 1.3mm{\\hfil}\\vfil}}\n";

  iter lk rk glyphs

  where rec iter lk rk glyphs = match glyphs with
  [ []      -> ()
  | [g::gs] -> do
    {
      log_string "\n\n(";
      log_int    g.Glyph.width;
      log_string ", ";
      log_int    g.Glyph.height;
      log_string ", ";
      log_int    g.Glyph.depth;
      log_string ", ";
      log_int    g.Glyph.min_x;
      log_string ", ";
      log_int    g.Glyph.max_x;
      log_string ", ";
      log_int    g.Glyph.min_y;
      log_string ", ";
      log_int    g.Glyph.max_y;
      log_string ")\n\n";
      log_string "\\vbox{\\vbox to 0pt{%\n";
      log_string "  \\vbox to 0pt{}%\n";
      log_string "  \\hbox{";
      draw_line (List.hd lk)                 g;
      draw_line (g.Glyph.width - List.hd rk) g;
      draw_line (calc_center pow4 g)         g;
      log_string "}\\vss}%\n";
      draw_glyph g;
      log_string "}\\vfill\n";

      iter (List.tl lk) (List.tl rk) gs
    }
  ];

  log_string "\\end{document}\n"
};

value main () = do
{
  if Array.length Sys.argv <> 3 then do
  {
    print_string "USAGE: kerning <font> <size> <dpi>\n"
  }
  else do
  {
    Job.start_job Sys.argv.(1);

    let font = Font.load_font Sys.argv.(1) (num_of_string Sys.argv.(2));

    match FontPK.read_pk_font font (int_of_string Sys.argv.(3)) with
    [ None                           -> log_error "Can't load font!"
    | Some ((hppp, vppp, _), glyphs) -> do
      {
        let left_kern  = XList.map (fun g -> calc_left_kern  hppp vppp g) glyphs;
        let right_kern = XList.map (fun g -> calc_right_kern hppp vppp g) glyphs;

(*        output_lrcodes     left_kern right_kern glyphs  *)
(*        output_margin_test left_kern right_kern glyphs  *)
        output_glyphs      left_kern right_kern glyphs
      }
    ]
  }
};

main ();

