
open XNum;
open Unicode.Types;
open Runtime;
open Dim;
open Substitute;
open GlyphMetric;
open FontMetric;
open Box;

value rec discard_glue items = do
{
  iter [] items

  where rec iter cmds items = match items with
  [ []      -> (List.rev cmds, [])
  | [i::is] -> match i with
    [ `Glyph _              -> (List.rev cmds, items)
    | `Kern _               -> iter cmds is
    | `Break (_,_,_,_,[||]) -> iter cmds is   (* only break boxes where no-break is empty *)
    | `Break _              -> (List.rev cmds, items)
    | `Command b | `Box b   -> match b.b_contents with
      [ GlueBox _ disc      -> if disc then
                                 iter cmds is
                               else
                                 (List.rev cmds, items)
      | BreakBox _ _ _ _ [] -> iter cmds is   (* only break boxes where no-break is empty *)
      | EmptyBox            -> iter cmds is
      | _                   -> do
        {
          if is_real_box b then
            (List.rev cmds, items)
          else
            iter [i :: cmds] is
        }
      ]
    ]
  ]
};

value rec discard_glue_array from_pos to_pos items = do
{
  iter [] from_pos

  where rec iter cmds pos = do
  {
    if pos > to_pos then
      (List.rev cmds, pos)
    else match items.(pos) with
    [ `Glyph _              -> (List.rev cmds, pos)
    | `Kern _               -> iter cmds (pos+1)
    | `Break (_,_,_,_,[||]) -> iter cmds (pos+1)   (* only break boxes where no-break is empty *)
    | `Break _              -> (List.rev cmds, pos)
    | `Command b | `Box b   -> match b.b_contents with
      [ GlueBox _ disc      -> if disc then
                                 iter cmds (pos+1)
                               else
                                 (List.rev cmds, pos)
      | BreakBox _ _ _ _ [] -> iter cmds (pos+1)   (* only break boxes where no-break is empty *)
      | EmptyBox            -> iter cmds (pos+1)
      | _                   -> do
        {
          if is_real_box b then
            (List.rev cmds, pos)
          else
            iter [items.(pos) :: cmds] (pos+1)
        }
      ]
    ]
  }
};

value rec box_add_lig_kern boxes = do
{
  let insert_lig lig_char keep_first keep_second b1 b2 bs = do
  {
    match (b1.b_contents, b2.b_contents) with
    [ (CharBox _ font, CharBox _ _) -> do
      {
          (if keep_first  then [b1] else [])
        @ [new_glyph_box (Simple lig_char) font]
        @ (if keep_second then [b2] else [])
        @ bs
      }
    | _ -> assert False
    ]
  };

  let collect_commands boxes = do
  {
    let cmds = ListBuilder.make ();

    iter boxes

    where rec iter boxes = match boxes with
    [ []      -> (ListBuilder.get cmds, [])
    | [b::bs] -> do
      {
        if is_real_box b then
          (ListBuilder.get cmds, boxes)
        else do
        {
          ListBuilder.add cmds b;
          iter bs
        }
      }
    ]
  };

  let process_lig_kern b1 boxes = match collect_commands boxes with
  [ (cmds, [])                 -> [b1 :: cmds]
  | (cmds, ([b2::rest] as bs)) -> match (b1.b_contents, b2.b_contents) with
    [ (CharBox c1 f1, CharBox c2 f2) -> do
      {
        if f1 == f2 then
          match get_lig_kern f1 c1 c2 with
          [ NoLigKern -> [b1 :: cmds @ box_add_lig_kern bs]
          | Kern k    -> [b1; new_kern_box k num_zero
                           :: cmds @ box_add_lig_kern boxes]
          | Ligature c s k1 k2 -> do
            {
              let new_boxes = insert_lig c k1 k2 b1 b2 rest;

              XList.take s new_boxes @ box_add_lig_kern (XList.drop s new_boxes)
            }
          ]
        else
          [b1 :: cmds @ box_add_lig_kern bs]
      }
    | _ -> [b1 :: cmds @ box_add_lig_kern bs]
    ]
  ];

  match boxes with
  [ []      -> []
  | [b::bs] -> if is_char_box b then
                 process_lig_kern b bs
               else
                 [b :: box_add_lig_kern bs]
  ]
};

type composer 'result 'cmd =
{
  result        : ListBuilder.builder 'result;
  current       : ListBuilder.builder (char_item box 'cmd);
  end_word      : composer 'result 'cmd -> unit;
  process_box   : composer 'result 'cmd -> box -> 'result;
  font          : mutable font_metric;
  composer      : mutable Substitute.glyph_composer font_metric box 'cmd;
  hyphen_params : mutable JustHyph.hyphen_params
};

value set_font composer font glyph_composer = do
{
  if composer.composer != glyph_composer || composer.font != font then do
  {
    composer.end_word composer;

    composer.font     := font;
    composer.composer := glyph_composer
  }
  else ()
};

value set_hyphen_params composer hyphen_params = do
{
  composer.hyphen_params := hyphen_params
};

value add_char composer chr = do
{
  ListBuilder.add composer.current (`Char chr)
};

value add_break composer p h pre post no = do
{
  ListBuilder.add composer.current (`Break (p,h,pre,post,no))
};

value ignore_break _ _ _ _ _ _ = ();

value add_kern composer x y = do
{
  ListBuilder.add composer.current (`Kern (x,y))
};

value add_cmd composer cmd = do
{
  ListBuilder.add composer.current (`Command cmd)
};

value add_box composer box = do
{
  composer.end_word composer;

  ListBuilder.add composer.result (composer.process_box composer box)
};

value get_contents composer () = do
{
  composer.end_word composer;

  ListBuilder.get composer.result
};

value process_box_to_box _ box  = box;
value process_box_to_item _ box = `Box box;

value end_word_just_hyph composer = do
{
  match ListBuilder.get composer.current with
  [ []   -> ()
  | word -> do
    {
      let items = JustHyph.convert_to_glyphs_and_add_breaks composer.hyphen_params composer.font composer.composer word;

      ListBuilder.add_list
        composer.result
        (List.map simple_item_to_box (JustHyph.add_lig_kern True items))
    }
  ]
};

value end_word_ligature composer = do
{
  match ListBuilder.get composer.current with
  [ []   -> ()
  | word -> do
    {
      ListBuilder.add_list
        composer.result
        (List.map simple_item_to_box
          (JustHyph.add_lig_kern False
            (Array.to_list
              (JustHyph.convert_to_glyphs composer.font composer.composer word))))
    }
  ]
};

value end_word_hyph_only composer = do
{
  match ListBuilder.get composer.current with
  [ []   -> ()
  | word -> do
    {
      let items = JustHyph.convert_to_glyphs_and_add_breaks composer.hyphen_params composer.font composer.composer word;

      ListBuilder.add_list composer.result items
    }
  ]
};

value end_word_char_item composer = do
{
  ListBuilder.add_list composer.result (ListBuilder.get composer.current)
};

value end_word_glyph_item composer = match ListBuilder.get composer.current with
[ []   -> ()
| word -> do
  {
    ListBuilder.add_list
      composer.result
      (composer.composer
        (Array.fold_right
          (fun i is -> [JustHyph.strip_composer i :: is])
          (JustHyph.convert_to_glyphs composer.font composer.composer word)
          []))
  }
];

value make_composer add_break composer = do
{
  ({
     Builder.add_char          = add_char  composer;
     Builder.add_break         = add_break composer;
     Builder.add_kern          = add_kern  composer;
     Builder.add_box           = add_box   composer;
     Builder.add_cmd           = add_cmd   composer;
     Builder.set_font          = set_font  composer;
     Builder.set_hyphen_params = set_hyphen_params composer
   },
   (get_contents composer)
  )
};

value just_hyph_builder font composer params = do
{
  make_composer add_break
    {
      result        = ListBuilder.make ();
      current       = ListBuilder.make ();
      end_word      = end_word_just_hyph;
      process_box   = process_box_to_box;
      font          = font;
      composer      = composer;
      hyphen_params = params
    }
};

value ligature_builder font composer params = do
{
  make_composer add_break
    {
      result        = ListBuilder.make ();
      current       = ListBuilder.make ();
      end_word      = end_word_ligature;
      process_box   = process_box_to_box;
      font          = font;
      composer      = composer;
      hyphen_params = params
    }
};

value hyph_only_builder font composer params = do
{
  make_composer add_break
    {
      result        = ListBuilder.make ();
      current       = ListBuilder.make ();
      end_word      = end_word_hyph_only;
      process_box   = process_box_to_item;
      font          = font;
      composer      = composer;
      hyphen_params = params
    }
};

value char_item_builder font composer params = do
{
  make_composer add_break
    {
      result        = ListBuilder.make ();
      current       = ListBuilder.make ();
      end_word      = end_word_char_item;
      process_box   = process_box_to_item;
      font          = font;
      composer      = composer;
      hyphen_params = params
    }
};

value glyph_item_builder font composer params = do
{
  make_composer add_break
    {
      result        = ListBuilder.make ();
      current       = ListBuilder.make ();
      end_word      = end_word_glyph_item;
      process_box   = process_box_to_item;
      font          = font;
      composer      = composer;
      hyphen_params = params
    }
};

