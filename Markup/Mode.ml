
open XNum;
open Runtime;
open Logging;
open Typesetting;
open Engine;
open ParseState;

value pdf_spec_num        = ref 0;
value pdf_page            = ref (-1);
value pdf_src_spec_stream = ref (IO.coerce_o (IO.make_buffer_stream 10));
value src_spec_enabled    = ref False;

value init_source_specials job = do
{
  !pdf_spec_num        := 0;
  !pdf_page            := -1;
  !src_spec_enabled    := job.Job.source_specials;
  !pdf_src_spec_stream := job.Job.src_special_stream;
};

value insert_source_special ps = do
{
  let ((file, line, col) as loc) = location ps;

  if file <> "" then do
  {
    let dvi_spec = Printf.sprintf "src:%d:%d %s" line col file;

    let pdf_spec pi (x,y) = do
    {
      let x_pos = int_of_float (float_of_num x *. 65536.0);
      let y_pos = int_of_float (float_of_num y *. 65536.0);

      if pi.Box.pi_page_no <> !pdf_page then do
      {
        IO.printf !pdf_src_spec_stream "s %d\n" pi.Box.pi_page_no;
        !pdf_page := pi.Box.pi_page_no
      }
      else ();

      IO.printf !pdf_src_spec_stream "(%s\nl %d %d\np %d %d %d\n)\n" file !pdf_spec_num line !pdf_spec_num x_pos y_pos;

      !pdf_spec_num := !pdf_spec_num + 1
    };

    add_node ps (Node.CommandBox loc (`Special (`DVI_Special dvi_spec)));
    add_node ps (Node.CommandBox loc (`PageCmd (Box.CallPageFunction pdf_spec)))
  }
  else ()
};

(* |begin_paragraph| starts a new paragraph, |end_paragraph| adds the current paragraph to the page. *)

value begin_paragraph ps = do
{
  open_node_list ps `Paragraph;

  if !src_spec_enabled then
    insert_source_special ps
  else ()
};

value end_paragraph ps = do
{
  match close_node_list ps `Paragraph with
  [ []    -> ()
  | nodes -> add_node ps (Node.Paragraph (location ps) nodes)
  ]
};

(* |begin_math| starts a math formula, |end_math| adds it to the current paragraph. *)

value begin_math ps = do
{
  open_node_list ps `Math
};

value end_math ps = do
{
  match close_node_list ps `Math with
  [ []    -> ()
  | nodes -> add_node ps (Node.Math (location ps) nodes)
  ]
};

value begin_hbox ps = do
{
  open_node_list ps `HBox
};

value end_hbox ps = do
{
  match close_node_list ps `HBox with
  [ []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps) `Default nodes)
  ]
};

value end_lrbox ps = do
{
  match close_node_list ps `LRBox with
  [ []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps) `LR nodes)
  ]
};

value end_rlbox ps = do
{
  match close_node_list ps `RLBox with
  [ []    -> ()
  | nodes -> add_node ps (Node.HBox (location ps) `RL nodes)
  ]
};

(* |ensure_par_mode| enters paragraph mode if the system is in document mode. *)

value ensure_par_mode ps = match current_mode ps with
[ `Galley                    -> begin_paragraph ps
| `VBox                      -> begin_hbox ps
| `Paragraph | `Math | `HBox | `LRBox | `RLBox -> ()
| m                          -> log_error (location ps)
                                  ("You can't start a paragraph in "
                                 ^ ParseState.mode_to_string m
                                 ^ " mode!")
];

value leave_par_mode ps = match current_mode ps with
[ `Preamble
| `Galley
| `VBox      -> ()
| `Paragraph -> end_paragraph ps
| `Math      -> do
                {
                  end_math ps;
                  end_paragraph ps
                }
| `HBox      -> end_hbox  ps
| `LRBox     -> end_lrbox ps
| `RLBox     -> end_rlbox ps
| m          -> log_error (location ps)
                  ("Mode "
                 ^ ParseState.mode_to_string m
                 ^ " at end of paragraph!")
];

(* |set_mode <mode>| sets the current mode and returns |True| on success. *)

value set_mode ps mode = do
{
  let set_mode_par ps = match current_mode ps with
  [ `Galley    -> do
                  {
                    begin_paragraph ps;
                    True
                  }
  | `Paragraph -> True
  | `Math      -> do
                  {
                    end_math ps;
                    True
                  }
  | _          -> False
  ];

  let set_mode_math ps = match current_mode ps with
  [ `Galley    -> do
                  {
                    begin_paragraph ps;
                    begin_math ps;
                    True
                  }
  | `Paragraph -> do
                  {
                    begin_math ps;
                    True
                  }
  | `Math      -> True
  | `HBox | `LRBox | `RLBox -> do
                  {
                    begin_math ps;
                    True
                  }
  | _          -> False
  ];

  match mode with
  [ `Galley    -> do
                  {
                    leave_par_mode ps;
                    True
                  }
  | `Paragraph -> set_mode_par ps
  | `Math      -> set_mode_math ps
  | _          -> False
  ]
};

