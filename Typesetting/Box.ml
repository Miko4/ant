
open XNum;
open Unicode.Types;
open Runtime;
open Substitute;
open GlyphMetric;
open FontMetric;
open Logging;
open Dim;
open Graphic;

type index_position = [ LeftIndex | RightIndex | VertIndex ];

type math_code =
[ NoMath    | Ordinary    | BinOp
| Relation  | Operator    | Punct
| Open      | Close       | Inner
| SubScript | SuperScript | IndexPosition of index_position
];

type page_info =
{
  pi_width     : num;
  pi_height    : num;
  pi_page_no   : int;
  pi_old_marks : list (uc_string * uc_string);
  pi_new_marks : list (uc_string * uc_string)
};

(* basic box *)

type basic_box 'a =
{
  b_width    : dim;
  b_height   : dim;
  b_depth    : dim;
  b_contents : 'a
};

(* contents of a box *)

type box = basic_box contents_type

and contents_type =
[ EmptyBox
| CharBox of glyph_desc and font_metric
| GlueBox of bool and bool
| RuleBox
| ImageBox of string and LoadImage.format
| CompBox of list gfx_cmd
| MathBox of math_code and box
| BreakBox of num and bool and list box and list box and list box
| CommandBox of box_cmd
| ProcBox of page_info -> (num * num) -> box -> list gfx_cmd
]

and box_cmd =
[= `GfxCmd of gfx_cmd
|  `ParCmd of par_cmd
|  `PageCmd of page_cmd
|  `Special of simple_cmd
]

and gfx_cmd = graphic_command dim box

and par_cmd =
[ VInsert of bool and (list box)
| CallParFunction of int -> unit
]

and floating = (uc_string * list extended_glyph_item)

and page_cmd =
[ SetNextLayout of uc_string
| SetMark of uc_string and uc_string
| CallPageFunction of page_info -> (num * num) -> unit
| Float of floating
]

and simple_glyph_item   = JustHyph.simple_glyph_item   box box
and extended_glyph_item = JustHyph.extended_glyph_item box box;

type glyph_composer     = Substitute.glyph_composer font_metric box box;

(* |log_box <box>| prints <box> in human readable form. *)

value rec log_box box = match box.b_contents with
[ CharBox g f -> log_uc_string (FontMetric.get_unicode f g)
| GlueBox i _ ->
    if i then
      ()
    else
      log_string " "
| BreakBox _ h _ _ _ ->
    if h then
      log_string "-"
    else
      log_string "|"
| CompBox _    -> log_string "[]"
| RuleBox      -> log_string "[--]"
| ImageBox f _ -> do { log_string "<"; log_string f; log_string ">" }
| EmptyBox     -> ()
| MathBox _ b  -> log_box b
| CommandBox _ -> log_string "[!]"
| ProcBox    _ -> log_string "[*]"
];

(* debugging aides *)

(* |dump_box <box>| prints <box> in debugging form. *)

value rec long_dump_box box = do
{
  let dump_dims box = do
  {
    log_string "(";
    log_dim box.b_width;
    log_string " x ";
    log_dim box.b_height;
    log_string " + ";
    log_dim box.b_depth;
    log_string ")"
  };

  match box.b_contents with
  [ CharBox c _ -> do
    {
      log_string "\n(char-box ";
      dump_dims box;
      log_string " ";

      match c with
      [ Simple g -> log_int g
      | Undef    -> log_string "<undef>"
      | _        -> log_string "<comp>"
      ]
    }
  | GlueBox i d -> do
    {
      log_string "\n(glue-box ";
      dump_dims box;
      log_string (if i then " i " else " - ");
      log_string (if d then "d)" else "-)")
    }
  | BreakBox _ h _ _ _ -> do
    {
      log_string "\n(break-box ";
      dump_dims box;
      if h then
        log_string " -)"
      else
        log_string " |)"
    }
  | CompBox c    -> do
    {
      log_string "\n(comp-box ";
      dump_dims box;
      List.iter
        (fun cmd -> match cmd with
         [ Graphic.PutBox _ _ b -> long_dump_box b
         | _                    -> ()
         ])
        c;
      log_string ")"
    }
  | RuleBox      -> log_string "\n(rule-box)"
  | ImageBox f _ -> do
    {
      log_string "\n(image-box ";
      dump_dims box;
      log_string " ";
      log_string f;
      log_string ")"
    }
  | EmptyBox     -> log_string "\n(empty-box)"
  | MathBox _ b  -> do
    {
      log_string "\n(math-box ";
      dump_dims box;
      log_string " ";
      long_dump_box b;
      log_string ")" 
    }
  | CommandBox _ -> log_string "\n(cmd-box)"
  | ProcBox    _ -> log_string "\n(proc-box)"
  ]
};

value short_dump_box box = match box.b_contents with
[ BreakBox _ _ _ _ _  -> log_string "|"
| GlueBox True _      -> log_string "/"
| GlueBox False True  -> log_string "_"
| GlueBox False False -> log_string "~"
| CharBox c f         -> log_uc_string (FontMetric.get_unicode f c)
| _                   -> log_string "!"
];

value short_dump_box_list boxes = do
{
  log_string ">";
  List.iter short_dump_box boxes;
  log_string "<\n"
};

value empty_box =
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = EmptyBox
};

value is_emtpy_box box = (box = empty_box);

(*
  |is_real_box <box>| tests whether <box> contains typeset material or meta information.
*)

value is_real_box box = match box.b_contents with
[ EmptyBox
| CommandBox _
| BreakBox _ _ _ _ [] -> False  (* FIX: remove no-break element of BreakBox *)
| BreakBox _ _ _ _ _
| CharBox _ _
| GlueBox _ _
| RuleBox
| ImageBox _ _
| CompBox _
| ProcBox _
| MathBox _ _         -> True
];

(*
  A character box contains one character. The contents is a list consisting of the number of the
  glyph, the font, and a list of the original characters which may be more than one in case of ligatures.
*)

value new_glyph_box glyph font = do
{
  let gm = FontMetric.get_glyph_metric font glyph;
  {
    b_width    = fixed_dim gm.gm_width;
    b_height   = fixed_dim gm.gm_height;
    b_depth    = fixed_dim gm.gm_depth;
    b_contents = CharBox glyph font
  }
};

value new_char_box char font = do
{
  new_glyph_box (FontMetric.get_glyph font char) font
};

value is_char_box box = match box.b_contents with
[ CharBox _ _ -> True
| _           -> False
];

(*
  A glue box is an empty box. The <implicit> flag marks glue which is inserted by the layout procedures,
  e.g., kerning. <discardable> indicates that the glue-box is to be removed if a line-break occures
  immediately before it. A rule box basically is a black glue-box.
*)

value new_glue_box width height implicit discardable =
{
  b_width    = width;
  b_height   = height;
  b_depth    = dim_zero;
  b_contents = GlueBox implicit discardable
};

value new_kern_box x y = do
{
  (* FIX: support vertical kerning *)

  new_glue_box (fixed_dim x) dim_zero True True
};

value is_glue_box box = match box.b_contents with
[ GlueBox _ _ -> True
| _           -> False
];

value is_implicit_glue box = match box.b_contents with
[ GlueBox i _ -> i
| _           -> False
];

value is_discardable_glue box = match box.b_contents with
[ GlueBox _ d -> d
| _           -> False
];

value new_rule_box width height depth =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = RuleBox
};

value is_rule_box box = match box.b_contents with
[ RuleBox -> True
| _       -> False
];

(*
  Compound boxes contain a list of tuples (<x>, <y>, <box>).
*)

value new_compound_box width height depth contents =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = CompBox contents
};

value is_compound_box box = match box.b_contents with
[ CompBox _ -> True
| _         -> False
];

(* |wrap_in_compound_box <box>| puts <box> into a compound-box of the same size. *)

value wrap_in_compound_box box = do
{
  if is_compound_box box then
    box
  else
    new_compound_box box.b_width box.b_height box.b_depth [PutBox dim_zero dim_zero box]
};

(*
  |add_to_compound_box <box> <gfx-cmds>| adds a list of graphics commands to <box>. If <box> is not
  a compound box then it is wrapped in one.
*)

value add_to_compound_box box gfx_cmds = do
{
  match box.b_contents with
  [ CompBox cmds -> new_compound_box box.b_width box.b_height box.b_depth (gfx_cmds @ cmds)
  | _            -> new_compound_box
                      box.b_width box.b_height box.b_depth
                      [PutBox dim_zero dim_zero box :: gfx_cmds]
  ]
};

(*
  |scale_box_horiz <box> <ratio>| and |scale_box_vert <box> <ratio>| scale <box> horizontally and
  vertically by <ratio>. The variants |scale_box_horiz_upto <box> <ratio>| and
  |scale_box_vert_upto <box> <ratio>| respect the maximal and minimal dimensions of <box>.
*)

value rec scale_box_horiz box ratio = do
{
  let scale_graphics cmd = match cmd with
  [ PutBox x y b -> PutBox (dim_scale x ratio) y (scale_box_horiz b ratio)
  | _            -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        (dim_scale box.b_width ratio)
        box.b_height
        box.b_depth
        (List.map scale_graphics contents)
  | _ -> { (box) with b_width = dim_scale box.b_width ratio }
  ]
};

value rec scale_box_horiz_upto box ratio = do
{
  let scale_graphics cmd = match cmd with
  [ PutBox x y b -> PutBox (dim_scale_upto x ratio) y (scale_box_horiz_upto b ratio)
  | _            -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        (dim_scale_upto box.b_width ratio)
        box.b_height
        box.b_depth
        (List.map scale_graphics contents)
  | _ -> { (box) with b_width = dim_scale_upto box.b_width ratio }
  ]
};

value rec scale_box_vert box ratio = do
{
  let scale_graphics cmd = match cmd with
  [ PutBox x y b -> PutBox x (dim_scale y ratio) (scale_box_vert b ratio)
  | _            -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        box.b_width
        (dim_scale box.b_height ratio)
        (dim_scale box.b_depth  ratio)
        (List.map scale_graphics contents)
  | _ -> {
           (box)

           with

           b_height = dim_scale box.b_height ratio;
           b_depth  = dim_scale box.b_depth  ratio
         }
  ]
};

value rec scale_box_vert_upto box ratio = do
{
  let scale_graphics cmd = match cmd with
  [ PutBox x y b -> PutBox x (dim_scale_upto y ratio) (scale_box_vert_upto b ratio)
  | _            -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        box.b_width
        (dim_scale_upto box.b_height ratio)
        (dim_scale_upto box.b_depth  ratio)
        (List.map scale_graphics contents)
  | _ -> {
           (box)

           with

           b_height = dim_scale_upto box.b_height ratio;
           b_depth  = dim_scale_upto box.b_depth  ratio
         }
  ]
};

value rec resize_box_horiz box width = do
{
  let ratio     = adjustment_ratio box.b_width width;
  let new_width = (dim_shift_base box.b_width (width -/ box.b_width.d_base));

  let scale_graphics cmd = match cmd with
  [ PutBox x y b -> PutBox (dim_scale x ratio) y (scale_box_horiz b ratio)
  | _            -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        new_width
        box.b_height
        box.b_depth
        (List.map scale_graphics contents)
  | _ -> { (box) with b_width = new_width }
  ]
};

value rec resize_box_vert box height depth = do
{
  let height_ratio = adjustment_ratio box.b_height height;
  let depth_ratio  = adjustment_ratio box.b_depth  depth;
  let new_height   = (dim_shift_base box.b_height (height -/ box.b_height.d_base));
  let new_depth    = (dim_shift_base box.b_depth  (depth  -/ box.b_depth.d_base));

  let scale_graphics cmd = match cmd with
  [ PutBox x y b ->
      if y.d_base >/ num_zero then do
      {
        if y.d_base -/ b.b_depth.d_base >=/ num_zero then
          PutBox x (dim_scale y height_ratio) (scale_box_vert b height_ratio)
        else
          PutBox x (dim_scale y height_ratio)
                   (resize_box_vert b
                     (dim_scale b.b_height height_ratio).d_base
                     ((dim_scale y height_ratio).d_base +/
                      (dim_scale
                        (dim_shift_base b.b_depth (minus_num y.d_base))
                        depth_ratio).d_base))
      }
      else if y.d_base </ num_zero then do
      {
        if y.d_base +/ b.b_height.d_base <=/ num_zero then
          PutBox x (dim_scale y depth_ratio) (scale_box_vert b depth_ratio)
        else
          PutBox x (dim_scale y height_ratio)
                   (resize_box_vert b
                     ((dim_scale
                        (dim_shift_base b.b_height y.d_base)
                        height_ratio).d_base -/
                      (dim_scale y depth_ratio).d_base)
                     (dim_scale b.b_depth depth_ratio).d_base)
      }
      else
        PutBox x dim_zero
                 (resize_box_vert b
                   (dim_scale b.b_height height_ratio).d_base
                   (dim_scale b.b_depth  depth_ratio).d_base)
  | _ -> cmd
  ];

  match box.b_contents with
  [ CompBox contents ->
      new_compound_box
        box.b_width
        new_height
        new_depth
        (List.map scale_graphics contents)
  | _ -> {
           (box)

           with

           b_height = new_height;
           b_depth  = new_depth
         }
  ]
};

value rec resize_box_horiz_upto box width = do
{
  resize_box_horiz box (min width (dim_max_value box.b_width))
};

value rec resize_box_vert_upto box height depth = do
{
  resize_box_vert box
    (min height (dim_max_value box.b_height))
    (min depth  (dim_max_value box.b_depth))
};

(* |shift_compound_vert <box> <amount>| shifts the contents of <box> upwards by <amount>. *)

value shift_compound_vert box amount = match box.b_contents with
[ CompBox contents -> do
  {
    let shift = fixed_dim amount;

    let shift_gfx cmd = match cmd with
    [ PutBox x y b -> PutBox x (dim_add y shift) b
    | _            -> cmd
    ];

    new_compound_box
      box.b_width
      (dim_add box.b_height shift)
      (dim_sub box.b_depth  shift)
      (List.map shift_gfx contents)
  }
| _ -> raise (Invalid_argument "Box.shift_comound_vert applied to non-compound box!")
];

(*
  |make_phantom <box>| creates an empty box of the same size as <box>. |make_hphantom| and |make_vphantom|
  return boxes of the same width, resp. height and depth, but where the other dimension is zero.
*)

value make_phantom box = do
{
  new_compound_box box.b_width box.b_height box.b_depth []
};

value make_hphantom box = do
{
  new_compound_box box.b_width dim_zero dim_zero []
};

value make_vphantom box = do
{
  new_compound_box dim_zero box.b_height box.b_depth []
};

(*
  Image boxes contain a reference to an external image file.
*)

value new_image_box width height file format =
{
  b_width    = fixed_dim width;
  b_height   = fixed_dim height;
  b_depth    = dim_zero;
  b_contents = ImageBox file format
};

(*
  Math boxes are wrappers containing additional information needed to typeset math formulae.
  <code> denotes the type and <box> contains the original box.
*)

value new_math_box code box =
{
  b_width    = box.b_width;
  b_height   = box.b_height;
  b_depth    = box.b_depth;
  b_contents = MathBox code box
};

value is_math_box box = match box.b_contents with
[ MathBox _ _ -> True
| _           -> False
];

value math_box_code box = match box.b_contents with
[ MathBox c _ -> c
| _           -> NoMath
];

(*
  A break box indicates a point where a line-break can occure. <penalty> is the penalty for a break at this
  position, <hyph> indicates whether the break point was cased by a hyphenation. If <penalty> is
  |-infinite| the line is always broken at this position. <pre_break>, <post_break>, and <no_break>
  are lists of boxes which are inserted before and after a break, or if no break occures.
*)

value new_break_box penalty hyph pre_break post_break no_break =
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = BreakBox penalty hyph pre_break post_break no_break
};

value forced_break_box = new_break_box (minus_num infinite) False [] [] [];

value break_box_penalty box = match box.b_contents with
[ BreakBox x _ _ _ _ -> x
| _                  -> raise (Invalid_argument "Box.break_box_penalty applied to non-break box!")
];

value break_box_hyph box = match box.b_contents with
[ BreakBox _ x _ _ _ -> x
| _                  -> raise (Invalid_argument "Box.break_box_hyph applied to non-break box!")
];

value break_box_pre box = match box.b_contents with
[ BreakBox _ _ x _ _ -> x
| _                  -> raise (Invalid_argument "Box.break_box_pre applied to non-break box!")
];

value break_box_post box = match box.b_contents with
[ BreakBox _ _ _ x _ -> x
| _                  -> raise (Invalid_argument "Box.break_box_post applied to non-break box!")
];

value break_box_no box = match box.b_contents with
[ BreakBox _ _ _ _ x -> x
| _                  -> raise (Invalid_argument "Box.break_box_no applied to non-break box!")
];

value new_command_box cmd =
{
  b_width    = dim_zero;
  b_height   = dim_zero;
  b_depth    = dim_zero;
  b_contents = CommandBox cmd
};

value new_proc_box width height depth f =
{
  b_width    = width;
  b_height   = height;
  b_depth    = depth;
  b_contents = ProcBox f
};

(* |item_to_box <item>| converts a glyph_item into a box. *)

value rec simple_item_to_box item = match item with
[ `Glyph (g,f)             -> new_glyph_box g f
| `Kern (x,y)              -> new_kern_box x y
| `Break (p,h,pre,post,no) -> new_break_box p h
                                (Array.to_list (Array.map simple_item_to_box pre))
                                (Array.to_list (Array.map simple_item_to_box post))
                                (Array.to_list (Array.map simple_item_to_box no))
| `Command c               -> c
| `Box b                   -> b
];

value rec extended_item_to_box item = match item with
[ `Glyph (g,(f,_))         -> new_glyph_box g f
| `Kern (x,y)              -> new_kern_box x y
| `Break (p,h,pre,post,no) -> new_break_box p h
                                (Array.to_list (Array.map extended_item_to_box pre))
                                (Array.to_list (Array.map extended_item_to_box post))
                                (Array.to_list (Array.map extended_item_to_box no))
| `Command c               -> c
| `Box b                   -> b
];

(*
  |draw_box <page_info> <x> <y> <box>| converts <box> into a |simple_box|.
  While doing so it executes all `CallFunction commands and it expands all procedure boxes.
*)

value rec draw_box page_info x y box = match box.b_contents with
[ EmptyBox           -> Empty
| BreakBox _ _ _ _ _ -> Empty
| GlueBox _ _        -> Empty
| CharBox c f        -> FontMetric.draw_glyph f c
| RuleBox            -> Group
                          [PutBox
                             num_zero
                             box.b_depth.d_base
                             (Rule
                               box.b_width.d_base
                               (box.b_height.d_base +/ box.b_depth.d_base))
                          ]
| ImageBox file fmt  -> Image box.b_width.d_base box.b_height.d_base file fmt
| MathBox _ b        -> draw_box page_info x y b
| CompBox cmds       -> do
  {
    let rec get_bg_colour cmds = match cmds with
    [ []                    -> []
    | [SetBgColour c :: _ ] -> [SetColour c;
                                PutBox
                                  num_zero
                                  box.b_depth.d_base
                                  (Rule
                                    box.b_width.d_base
                                    (box.b_height.d_base +/ box.b_depth.d_base))
                               ]
    | [_ :: cs]             -> get_bg_colour cs
    ];

    Group
      (get_bg_colour cmds
       @ draw_gfx_cmds page_info x y cmds)
  }
| ProcBox f -> do
  {
    Group
      (draw_gfx_cmds page_info x y (f page_info (x, y) box))
  }
| CommandBox c -> match c with
  [ `Special cmd -> Command cmd
  | `PageCmd cmd -> match cmd with
    [ CallPageFunction f-> do
      {
        f page_info (x, y);

        Empty
      }
    | _ -> Empty
    ]
  | _ -> Empty
  ]
]
and draw_gfx_cmds page_info x y cmds = match cmds with
[ [] -> []
| [PutBox dx dy b :: cs] -> match draw_box page_info (x +/ dx.d_base) (y +/ dy.d_base) b with
  [ Empty                 -> draw_gfx_cmds page_info x y cs
  | box                   -> [PutBox dx.d_base dy.d_base box
                              :: draw_gfx_cmds page_info x y cs]
  ]
| [Draw pc p     :: cs]   -> [Draw pc (convert_path p)
                              :: draw_gfx_cmds page_info x y cs]
| [SetColour c   :: cs]   -> [SetColour c
                              :: draw_gfx_cmds page_info x y cs]
| [SetAlpha  a   :: cs]   -> [SetAlpha a
                              :: draw_gfx_cmds page_info x y cs]
| [SetLineWidth w  :: cs] -> [SetLineWidth w
                              :: draw_gfx_cmds page_info x y cs]
| [SetLineCap c    :: cs] -> [SetLineCap c
                              :: draw_gfx_cmds page_info x y cs]
| [SetLineJoin j   :: cs] -> [SetLineJoin j
                              :: draw_gfx_cmds page_info x y cs]
| [SetMiterLimit l :: cs] -> [SetMiterLimit l
                              :: draw_gfx_cmds page_info x y cs]
| [SetBgColour _ :: cs]   -> draw_gfx_cmds page_info x y cs
]
and convert_path p = do
{
  List.map
    (fun (ax,ay,bx,by,cx,cy,dx,dy) ->
      (ax.d_base, ay.d_base, bx.d_base, by.d_base,
       cx.d_base, cy.d_base, dx.d_base, dy.d_base))
    p
};

(*
  |discard_glue <boxes>| removes glue- and break-boxes from the start of the list <boxes>
  and collects all command-boxes encountered. It is usually called after a line- or page-break.
*)

value rec discard_glue boxes = do
{
  iter [] boxes

  where rec iter cmds boxes = match boxes with
  [ []      -> (List.rev cmds, [])
  | [b::bs] -> match b.b_contents with
    [ GlueBox _ disc      -> if disc then
                               iter cmds bs
                             else
                               (List.rev cmds, boxes)
    | BreakBox _ _ _ _ [] -> iter cmds bs   (* only break boxes where no-break is empty *)
    | EmptyBox            -> iter cmds bs
    | _                   -> do
      {
        if is_real_box b then
          (List.rev cmds, boxes)
        else
          iter [b :: cmds] bs
      }
    ]
  ]
};

value remove_breaks boxes = do
{
  let result = ListBuilder.make ();

  iter boxes

  where rec iter boxes = match boxes with
  [ []      -> ListBuilder.get result
  | [b::bs] -> do
    {
      match b.b_contents with
      [ BreakBox _ _ _ _ n -> ListBuilder.add_list result n
      | _                  -> ListBuilder.add      result b
      ];
      iter bs
    }
  ]
};

