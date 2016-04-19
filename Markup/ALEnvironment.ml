
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ALCoding;
open ALDim;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* Opaque type for environments *)

value apply_env env x = match !x with
[ Types.Symbol s -> do
  {
(*    if s = sym_ then
    else *)
      Types.runtime_error "invalid argument"
  }
| _ -> Types.runtime_error "invalid argument"
];

value cmp_env e1 e2 = e1 == e2;

value (env_wrapper, env_unwrapper) = Opaque.declare_type "environment" apply_env cmp_env cmp_env;

value wrap_env env = Types.Opaque (env_wrapper env);

value unwrap_env = decode_opaque "environment" env_unwrapper;

value wrap_env_cmd name f loc env = do
{
  wrap_env (f (decode_location name loc) (unwrap_env name env))
};

value decode_env_cmd name f loc env = do
{
  try
    unwrap_env name (Machine.evaluate_function f [ref (encode_location loc); ref (wrap_env env)])
  with
  [ VM.Types.Syntax_error loc msg -> do { log_warn loc (UString.to_string (Array.to_list msg)); env }
  | VM.Types.Runtime_error msg    -> do { log_warn loc (UString.to_string (Array.to_list msg)); env }
  ]
};

value encode_env_cmd name cmd = Types.Primitive2 (wrap_env_cmd name cmd);

(* skip args *)

value encode_skip_arg s = do
{
  let f e = do
  {
    let env = unwrap_env "<unnamed>" e;

    Types.Number (s env)
  };

  Types.Primitive1 f
};

value decode_skip_arg name s = match !s with
[ Types.Number a -> (fun _ -> a)
| _ -> do
  {
    let f = Machine.evaluate_function s;

    (fun env -> do
      {
        let x = f [ref (wrap_env env)];

        Machine.decode_num name x
      })
  }
];

(* dim args *)

value encode_dim_arg d = do
{
  let f e = do
  {
    let env = unwrap_env "<unnamed>" e;

    wrap_dim (d env)
  };

  Types.Primitive1 f
};

value decode_dim_arg name d = match !d with
[ Types.Number a -> (fun _ -> Dim.fixed_dim a)
| _ -> do
  {
    let f = Machine.evaluate_function d;

    (fun env -> do
      {
        let x = f [ref (wrap_env env)];

        match !x with
        [ Types.Number a -> Dim.fixed_dim a
        | _              -> unwrap_dim name x
        ]
      })
  }
];

value lookup_skip name dict key = lookup (decode_skip_arg name) dict key;
value lookup_dim  name dict key = lookup (decode_dim_arg  name) dict key;

(* primitives *)

value env_quad x env = do
{
  let e = unwrap_env "env_quad" env;
  let s = Machine.decode_num "env_quad" x;

  Types.Number (Evaluate.const_em s e)
};

value env_x_height x env = do
{
  let e = unwrap_env "env_x_height" env;
  let s = Machine.decode_num "env_x_height" x;

  Types.Number (Evaluate.const_ex s e)
};

value env_math_unit x env = do
{
  let e = unwrap_env "env_math_unit" env;
  let s = Machine.decode_num "env_math_unit" x;

  Types.Number (Evaluate.const_mu s e)
};

value prim_new_galley name width = do
{
  let n = decode_uc_string   "new_galley" name;
  let w = Machine.decode_num "new_galley" width;

  encode_env_cmd "new_galley" (Environment.new_galley n w)
};

value prim_select_galley name = do
{
  let n = decode_uc_string "select_galley" name;

  encode_env_cmd "select_galley" (Environment.select_galley n)
};
(*
value prim_set_galley         : Galley.galley box_cmd -> env_cmd;
value prim_galley_set_leading : (box -> box -> Galley.line_params box_cmd -> dim) -> env_cmd;

value prim_set_par_shape res shape = do
{
  let s env line = do
  {
    let result = Machine.decode_function
                   "<par-shape>"
                   shape
                   [ref (wrap_env env); ref (Types.Number (num_of_int line))];

    Machine.evaluate result;

    match !result with
    [ Types.Tuple [|l; r|] -> (Machine.decode_num "<par-shape>" l,
                               Machine.decode_num "<par-shape>" r)
    | _ -> Types.runtime_error ("<par-shape>: pair expected but got " ^ Types.type_name !result)
    ]
  };

  !res := encode_env_cmd "galley_set_par_shape"
            (Environment.set_par_params
              (None, None, None, None, None, Some s, None, None))
};

value prim_galley_set_post_process_line : (environment -> list box -> list box) -> env_cmd;
*)

value prim_set_colour col = do
{
  let c = decode_colour "set_colour" col;

  encode_env_cmd "set_colour" (Environment.set_colour c)
};

(*
value prim_adjust_graphics_state : environment -> environment -> list box;
*)

value prim_new_page_layout args = match args with
[ [name; width; height] -> do
  {
    let n = decode_uc_string   "new_page_layout" name;
    let w = Machine.decode_num "new_page_layout" width;
    let h = Machine.decode_num "new_page_layout" height;

    encode_env_cmd "new_page_layout" (Environment.new_page_layout n w h)
  }
| _ -> assert False
];

value prim_select_page_layout name = do
{
  let n = decode_uc_string "select_page_layout" name;

  encode_env_cmd "select_page_layout" (Environment.select_page_layout n)
};

(*
value prim_set_page_layout : PageLayout.page_layout box_cmd -> env_cmd;
value prim_add_pages       : int -> list box -> env_cmd;
*)

(*
value prim_set_font def = do
{
  Machine.evaluate def;

  match !def with
  [ Types.Tuple [|family; series; shape; size; script_lang|] -> do
    {
      let fam = decode_option "set_font" decode_uc_string     family;
      let ser = decode_option "set_font" decode_uc_string     series;
      let sha = decode_option "set_font" decode_uc_string     shape;
      let siz = decode_option "set_font" Machine.decode_num size;

      encode_env_cmd "set_font" (Environment.set_font (fam, ser, sha, siz))
    }
  | _ -> Types.runtime_error "set_font: invalid argument"
  ]
};
*)

(*
value prim_get_math_font args = match args with
[ [env; style; family] -> do
  {
    let e = unwrap_env        "get_math_font" env;
    let s = decode_math_style "get_math_font" style;
    let f = decode_int        "get_math_font" family;

    (* FIX *)
    encode_font_metric (Environment.get_math_font e s f)
  }
| _ -> assert False
];
*)

value prim_set_math_font def = match !def with
[ Types.Tuple [|math_family; family; series; shape; text_size; script_size; script2_size|] -> do
  {
    let mf  = decode_option "set_math_font" decode_int           math_family;
    let fam = decode_option "set_math_font" decode_uc_string     family;
    let ser = decode_option "set_math_font" decode_uc_string     series;
    let sha = decode_option "set_math_font" decode_uc_string     shape;
    let ts  = decode_option "set_math_font" Machine.decode_num text_size;
    let ss  = decode_option "set_math_font" Machine.decode_num script_size;
    let s2s = decode_option "set_math_font" Machine.decode_num script2_size;

    encode_env_cmd "set_math_font" (Environment.set_math_font (mf, fam, ser, sha, ts, ss, s2s))
  }
| _ -> Types.runtime_error "set_math_font: invalid argument"
];

value prim_adapt_fonts_to_math_style =
  encode_env_cmd "adapt_fonts_to_math_style" Environment.adapt_fonts_to_math_style;

value decode_par_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_num name d sym_Measure,
   lookup_dim name d sym_ParIndent,
   lookup_dim name d sym_ParFillSkip,
   lookup_dim name d sym_LeftSkip,
   lookup_dim name d sym_RightSkip,
   None, (* FIX: lookup_ name d sym_ParShape  *)
   lookup_dim name d sym_ParSkip,
   None, (* FIX: lookup_ name d sym_PreBreak  *)
   None, (* FIX: lookup_ name d sym_PostBreak *)
   None) (* FIX: lookup_ name d sym_PostProcessLine *)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_par_params params = do
{
  encode_env_cmd "set_par_params"
    (Environment.set_par_params (decode_par_params "set_par_params" params))
};

value prim_set_current_par_params params = do
{
  encode_env_cmd "set_current_par_params"
    (Environment.set_current_par_params (decode_par_params "set_current_par_params" params))
};

value leading_map =
   SymbolMap.add sym_Fixed    Galley.leading_fixed
  (SymbolMap.add sym_Register Galley.leading_register
  (SymbolMap.add sym_TeX      Galley.leading_TeX
  (SymbolMap.add sym_Skyline  Galley.leading_skyline
    SymbolMap.empty)));

value decode_leading name leading = match leading with
[ None     -> None
| Some sym -> do
  {
    try
      Some (SymbolMap.find sym leading_map)
    with
    [ Not_found ->
        Types.runtime_error
          (name
           ^ ": unknown leading `"
           ^ UString.to_string (Array.to_list (Machine.symbol_to_string sym))
           ^ "'.")
    ]
  }
];

value decode_line_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_dim     name d sym_BaselineSkip,
   lookup_skip    name d sym_LineSkipLimit,
   lookup_dim     name d sym_LineSkip,
   decode_leading name (lookup_symbol name d sym_Leading),
   None)  (* FIX: lookup_ name d sym_ClubWidowPenalty *)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_line_params params = do
{
  encode_env_cmd "set_line_params"
    (Environment.set_line_params (decode_line_params "set_line_params" params))
};

value prim_set_current_line_params params = do
{
  encode_env_cmd "set_current_line_params"
    (Environment.set_current_line_params (decode_line_params "set_current_line_params" params))
};

value decode_line_break_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_num  name d sym_PreTolerance,
   lookup_num  name d sym_Tolerance,
   lookup_int  name d sym_Looseness,
   lookup_num  name d sym_LinePenalty,
   lookup_num  name d sym_AdjDemerits,
   lookup_num  name d sym_DoubleHyphenDemerits,
   lookup_num  name d sym_FinalHyphenDemerits,
   lookup_skip name d sym_EmergencyStretch,
   lookup_num  name d sym_RiverDemerits,
   lookup_skip name d sym_RiverThreshold,
   lookup_bool name d sym_SimpleBreaking)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_line_break_params params = do
{
  encode_env_cmd "set_line_break_params"
    (Environment.set_line_break_params (decode_line_break_params "set_line_break_params" params))
};

value prim_set_current_line_break_params params = do
{
  encode_env_cmd "set_current_line_break_params"
    (Environment.set_current_line_break_params (decode_line_break_params "set_current_line_break_params" params))
};

value decode_hyphen_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_string name d sym_HyphenTable,
   lookup_num    name d sym_HyphenPenalty,
   lookup_num    name d sym_ExHyphenPenalty,
   lookup_int    name d sym_LeftHyphenMin,
   lookup_int    name d sym_RightHyphenMin,
   lookup_string name d sym_ScriptLang)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_hyphen_params params = do
{
  encode_env_cmd "set_hyphen_params"
    (Environment.set_hyphen_params (decode_hyphen_params "set_hyphen_params" params))
};

value prim_set_current_hyphen_params params = do
{
  encode_env_cmd "set_current_hyphen_params"
    (Environment.set_current_hyphen_params (decode_hyphen_params "set_current_hyphen_params" params))
};

value decode_space_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_num  name d sym_SpaceFactor,
   lookup_dim  name d sym_SpaceSkip,
   lookup_dim  name d sym_XSpaceSkip,
   lookup_bool name d sym_VictorianSpacing)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_space_params params = do
{
  encode_env_cmd "set_space_params"
    (Environment.set_space_params (decode_space_params "set_space_params" params))
};

value prim_set_current_space_params params = do
{
  encode_env_cmd "set_current_space_params"
    (Environment.set_current_space_params (decode_space_params "set_current_space_params" params))
};

value decode_math_params name params = match !params with
[ Types.Dictionary d ->
  (lookup_dim  name d sym_ThinMathSkip,
   lookup_dim  name d sym_MedMathSkip,
   lookup_dim  name d sym_ThickMathSkip,
   lookup_dim  name d sym_ScriptSpace,
   lookup_num  name d sym_RelPenalty,
   lookup_num  name d sym_BinOpPenalty,
   lookup_num  name d sym_DelimiterFactor,
   lookup_skip name d sym_DelimiterShortfall,
   lookup_dim  name d sym_NullDelimiterSpace)
| _ -> Types.runtime_error (name ^ ": invalid argument")
];

value prim_set_math_params params = do
{
  encode_env_cmd "set_math_params"
    (Environment.set_math_params (decode_math_params "set_math_params" params))
};

value prim_set_current_math_params params = do
{
  encode_env_cmd "set_current_math_params"
    (Environment.set_current_math_params (decode_math_params "set_current_math_params" params))
};


value prim_get_space_factor env char = do
{
  let e = unwrap_env  "get_space_factor" env;
  let c = decode_char "get_space_factor" char;

  Types.Number (Environment.get_space_factor e c)
};

value prim_adjust_space_factor char = do
{
  let x = decode_char "adjust_space_factor" char;

  encode_env_cmd "adjust_space_factor" (Environment.adjust_space_factor x)
};

