
open XNum;
open Maps;
open Unicode.Types;

module Tag =
struct

type tag = int32;

value compose_tag high low = do
{
  Int32.logor
    (Int32.shift_left (Int32.of_int high) 16)
    (Int32.of_int low)
};

value make_tag str = do
{
  if String.length str <> 4 then
    invalid_arg "make_tag: 4 characters expected"
  else do
  {
    let d0 = int_of_char str.[0];
    let d1 = int_of_char str.[1];
    let d2 = int_of_char str.[2];
    let d3 = int_of_char str.[3];

    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag: invalid character"
  }
};

value make_tag_uc str = do
{
  if Array.length str <> 4 then
    invalid_arg "make_tag_uc: 4 characters expected"
  else do
  {
    let d0 = str.(0);
    let d1 = str.(1);
    let d2 = str.(2);
    let d3 = str.(3);

    if d0 >= 32 && d0 <= 126
    && d1 >= 32 && d1 <= 126
    && d2 >= 32 && d2 <= 126
    && d3 >= 32 && d3 <= 126 then
      compose_tag ((d0 lsl 8) lor d1)
                  ((d2 lsl 8) lor d3)
    else
      invalid_arg "make_tag_uc: invalid character"
  }
};

value tag_to_string tag = do
{
  let str = String.make 4 ' ';

  let x = Int32.to_int (Int32.logand (Int32.shift_right tag 16)
                                     (Int32.of_int 0xffff));
  let y = Int32.to_int (Int32.logand tag (Int32.of_int 0xffff));

  str.[0] := char_of_int ((x lsr 8) land 0xff);
  str.[1] := char_of_int (x land 0xff);
  str.[2] := char_of_int ((y lsr 8) land 0xff);
  str.[3] := char_of_int (y land 0xff);

  str
};

value empty_tag    = make_tag "    ";
value latn_tag     = make_tag "latn";
value dflt_tag     = make_tag "dflt";
value size_tag     = make_tag "size";
(*
value vkrn_tag     = make_tag "vkrn";
*)

value ttcf_tag = make_tag "ttcf";
value true_tag = make_tag "true";
value otto_tag = make_tag "OTTO";
value cff_tag  = make_tag "CFF ";
value cmap_tag = make_tag "cmap";
value glyf_tag = make_tag "glyf";
value gdef_tag = make_tag "GDEF";
value gpos_tag = make_tag "GPOS";
value gsub_tag = make_tag "GSUB";
(*
value bdat_tag = make_tag "bdat";
value ebdt_tag = make_tag "EBDT";
value bloc_tag = make_tag "bloc";
value eblc_tag = make_tag "EBLC";
value bhed_tag = make_tag "bhed";
*)
value head_tag = make_tag "head";
value hhea_tag = make_tag "hhea";
value hmtx_tag = make_tag "hmtx";
value kern_tag = make_tag "kern";
value loca_tag = make_tag "loca";
value maxp_tag = make_tag "maxp";
value name_tag = make_tag "name";
value post_tag = make_tag "post";
value os2_tag  = make_tag "OS/2";
(*
value vhea_tag = make_tag "vhea";
value vmtx_tag = make_tag "vmtx";
value vorg_tag = make_tag "VORG";
value acnt_tag = make_tag "acnt";
value feat_tag = make_tag "feat";
value lcar_tag = make_tag "lcar";
value mort_tag = make_tag "mort";
value morx_tag = make_tag "morx";
value opbd_tag = make_tag "opbd";
value prop_tag = make_tag "prop";
*)
value cvt_tag  = make_tag "cvt ";
value prep_tag = make_tag "prep";
value fpgm_tag = make_tag "fpgm";
(*
value gvar_tag = make_tag "gvar";
value fvar_tag = make_tag "fvar";
value avar_tag = make_tag "avar";
value cvar_tag = make_tag "cvar";
*)

module OrderedTag = struct type t = tag; value compare = (compare : tag -> tag -> int); end;
module TagMap = Map.Make(OrderedTag);
module TagSet = Set.Make(OrderedTag);

value init_map num get_key get_value = do
{
  iter 0 IntMap.empty

  where rec iter i map = do
  {
    if i >= num then
      map
    else
      iter (i+1) (IntMap.add (get_key i) (get_value i) map)
  }
};

end;

module Table =
struct

type table = string;

value read_u8 table off = int_of_char (table.[off]);

value read_u16 table off = do
{
  let x = read_u8 table off;
  let y = read_u8 table (off+1);

  0x100 * x + y
};

value read_u24 table off = do
{
  let x = read_u8 table off;
  let y = read_u8 table (off+1);
  let z = read_u8 table (off+2);

  0x10000 * x + 0x100 * y + z
};

value read_u32 table off = do
{
  let x = read_u16 table off;
  let y = read_u16 table (off+2);

  Tag.compose_tag x y
};

value read_i8 table off = do
{
  let x = read_u8 table off;

  if x > 0x7f then
    x - 0x100
  else
    x
};

value read_i16 table off = do
{
  let x = read_u16 table off;

  if x > 0x7fff then
    x - 0x10000
  else
    x
};

value read_u16_array table off len = do
{
  Array.init len
    (fun i -> read_u16 table (off + 2 * i))
};

end;

module OTF_Pos_Subst =
struct

open Table;
open Tag;

type feature =
{
  f_tag         : tag;
  f_params      : int;
  f_lookups     : array int;
  f_script_list : TagMap.t tag
};

value flags_right_to_left        = 0x0001;
value flags_ignore_base_glyphs   = 0x0002;
value flags_ignore_ligatures     = 0x0004;
value flags_ignore_marks         = 0x0008;
value flags_mark_attachment_type = 0xff00;

type lang_sys =
{
  ls_required : option feature;
  ls_features : array feature
};

type positioning =
{
  p_x_off     : int;
  p_y_off     : int;
  p_h_adv_off : int;
  p_v_adv_off : int
};

type pos_rule_record =
{
  prr_seq_idx : int;
  prr_lookup  : lookup
}

and pos_subst_rule 'a =
{
  psr_data    : 'a;
  psr_lookups : array pos_rule_record
}

and pos_subst_command =
[ NoCommand
| Position             of IntMap.t positioning
| CursiveAnchors       of IntMap.t (int * int) and IntMap.t (int * int)
| MarkToBaseAnchors    of array (int * int * int * int) and array (int * array (int * int))
| MarkToLigAnchors     of array (int * int * int * int) and array (int * array (array (int * int)))
| MarkToMarkAnchors    of array (int * int * int * int) and array (int * array (int * int))
| Kern                 of IntMap.t (IntMap.t (positioning * positioning))
| KernClass            of int and IntMap.t int and IntMap.t int and array positioning and array positioning
| Substitution         of IntMap.t int
| Multiple             of IntMap.t (array int)
| Alternate            of IntMap.t (array int)
| Ligature             of DynUCTrie.t int
| ContextGlyphPos      of DynUCTrie.t (array pos_rule_record)
| ContextGlyphSubst    of DynUCTrie.t (array pos_rule_record)
| ContextClassPos      of IntMap.t int and array (pos_subst_rule (array int))
| ContextClassSubst    of IntMap.t int and array (pos_subst_rule (array int))
| ContextCoveragePos   of array (pos_subst_rule (array (array int)))
| ContextCoverageSubst of array (pos_subst_rule (array (array int)))
| ChainGlyphPos        of (array (pos_subst_rule (array int * array int * array int)))
| ChainGlyphSubst      of (array (pos_subst_rule (array int * array int * array int)))
| ChainClassPos        of IntMap.t int and IntMap.t int and IntMap.t int and
                          array (pos_subst_rule (array int * array int * array int))
| ChainClassSubst      of IntMap.t int and IntMap.t int and IntMap.t int and
                          array (pos_subst_rule (array int * array int * array int))
| ChainCoveragePos     of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| ChainCoverageSubst   of array (pos_subst_rule (array (array int) * array (array int) * array (array int)))
| ReverseSubst         of array int and array int and array (array int) and array (array int)
]

and lookup =
{
  l_flags    : int;
  l_commands : array pos_subst_command
};

type size_params =
{
  design_size         : int;
  font_style_id       : int;
  font_style_name     : int;
  design_range_bottom : int;
  design_range_top    : int
};

type pos_subst_table =
{
  t_scripts : TagMap.t (TagMap.t lang_sys);
  t_lookups : array lookup;
  t_size    : option size_params
};

value empty_pos_subst =
{
  t_scripts = TagMap.empty;
  t_lookups = [||];
  t_size    = None
};

value make_pos_subst_table scripts lookups size =
{
  t_scripts = scripts;
  t_lookups = lookups;
  t_size    = size
};

(* access method *)

value get_lookups ps_table script lang_sys features = do
{
  let mark_feature_lookups lookups feature = do
  {
    Array.iter
      (fun l -> lookups.(l) := True)
      feature.f_lookups
  };
  let mark_lang_sys_lookups lookups lang_sys features = do
  {
    match lang_sys.ls_required with
    [ None   -> ()
    | Some f -> mark_feature_lookups lookups f
    ];

    Array.iter
      (fun f -> if TagSet.mem f.f_tag features then
                  mark_feature_lookups lookups f
                else ())
      lang_sys.ls_features
  };

  let num_lookups = Array.length ps_table.t_lookups;
  let marks       = Array.make num_lookups False;

  let s  = TagMap.find script ps_table.t_scripts;

  let ls = try
             TagMap.find lang_sys s
           with
           [ Not_found -> TagMap.find dflt_tag s ];

  mark_lang_sys_lookups marks ls features;

  let (_, active_lookups) =
    Array.fold_right
      (fun l (i, lu) -> do
        {
          if marks.(i) then
            (i-1, [l :: lu])
          else
            (i-1, lu)
        })
      ps_table.t_lookups
      (num_lookups-1, []);

  active_lookups
};

(* parsing routines *)

value parse_size_params table table_off =
{
  design_size         = read_u16 table table_off;
  font_style_id       = read_u16 table (table_off+2);
  font_style_name     = read_u16 table (table_off+4);
  design_range_bottom = read_u16 table (table_off+6);
  design_range_top    = read_u16 table (table_off+8)
};

value parse_features table table_off = do
{
  let num_features = read_u16 table table_off;

  let read_feature i = do
  {
    let f_off  = table_off + 6 * i + 2;
    let tag    = read_u32 table f_off;
    let offset = read_u16 table (f_off + 4);

    let params      = read_u16       table (table_off + offset);
    let num_lookups = read_u16       table (table_off + offset + 2);
    let lookups     = read_u16_array table (table_off + offset + 4) num_lookups;

    {
      f_tag         = tag;
      f_params      = params;
      f_lookups     = lookups;
      f_script_list = TagMap.empty
    }
  };

  Array.init num_features read_feature;
};

value process_lang_sys table table_off (required_script_list, script_list) script lang = do
{
  let f = read_u16 table (table_off + 2);
  let n = read_u16 table (table_off + 4);

  if f = 0xffff then
    ()
  else
    required_script_list.(f) := TagMap.add script lang required_script_list.(f);

  for i = 0 to n-1 do
  {
    let f = read_u16 table (table_off + 2 * i + 6);

    script_list.(f) := TagMap.add script lang script_list.(f)
  }
};

value parse_scripts table table_off features = do
{
  let read_lang_sys table_off = do
  {
    let f = read_u16 table (table_off + 2);
    let n = read_u16 table (table_off + 4);

    {
      ls_required =
          if f = 0xffff then
            None
          else
            Some features.(f);
      ls_features =
          Array.init
            n
            (fun i -> do
              {
                let f = read_u16 table (table_off + 2 * i + 6);

                features.(f)
              })
    }
  };
  let read_script i scripts = do
  {
    let script_tag = read_u32 table (table_off + 6 * i + 2);
    let script_off = read_u16 table (table_off + 6 * i + 6);

    let off          = table_off + script_off;
    let def_lang_sys = read_u16 table off;
    let num_langs    = read_u16 table (off + 2);

    let ls =
      if def_lang_sys <> 0 then
        TagMap.add dflt_tag (read_lang_sys (off + def_lang_sys)) TagMap.empty
      else
        TagMap.empty;

    iter 0 ls

    where rec iter i ls = do
    {
      if i >= num_langs then
        TagMap.add script_tag ls scripts
      else do
      {
        let t = read_u32 table (off + 6 * i + 4);
        let o = read_u16 table (off + 6 * i + 8);

        iter (i+1) (TagMap.add t (read_lang_sys (off + o)) ls)
      }
    }
  };

  let num_scripts = read_u16 table table_off;

  iter 0 TagMap.empty

  where rec iter i scripts = do
  {
    if i >= num_scripts then
      scripts
    else
      iter (i+1) (read_script i scripts)
  }
};

value get_coverage_table table table_off = do
{
  match read_u16 table table_off with
  [ 1 -> do
    {
      let num = read_u16 table (table_off + 2);

      read_u16_array table (table_off + 4) num
    }
  | 2 -> do
    {
      let rel_num = read_u16 table (table_off + 2);

      iter 0 0 []

      where rec iter i num_glyphs intervals = do
      {
        if i >= rel_num then do
        {
          let glyphs = Array.make num_glyphs 0;

          List.iter
            (fun (first, last, ind) -> do
              {
                for i = 0 to last - first do
                {
                  glyphs.(ind + i) := first + i
                }
              })
            intervals;

          glyphs
        }
        else do
        {
          let first = read_u16 table (table_off + 6 * i + 4);
          let last  = read_u16 table (table_off + 6 * i + 6);
          let ind   = read_u16 table (table_off + 6 * i + 8);

          iter (i+1) (max num_glyphs (ind + last - first + 1))
                     [(first, last, ind) :: intervals]
        }
      }
    }
  | _ -> raise (Failure "invalid coverage table")
  ]
};

value parse_lookups table table_off read = do
{
  let num_lookups    = read_u16 table table_off;
  let lookup_offsets = read_u16_array table (table_off + 2) num_lookups;

  let rec read_lookup l = do
  {
    let off    = table_off + lookup_offsets.(l);
    let l_type = read_u16 table off;
    let flags  = read_u16 table (off + 2);
    let num    = read_u16 table (off + 4);

    let offsets = read_u16_array table (off + 6) num;

    let commands = Array.init num (fun i -> read read_lookup table (off + offsets.(i)) l_type);
    {
      l_flags    = flags;
      l_commands = commands
    }
  };

  Array.init num_lookups read_lookup
};

value read_class_table table table_off = do
{
  match read_u16 table table_off with
  [ 1 -> do
    {
      let start = read_u16 table (table_off + 2);
      let num   = read_u16 table (table_off + 4);

      init_map num
        (fun i -> start + i)
        (fun i -> read_u16 table (table_off + 2 * i + 6))
    }
  | 2 -> do
    {
      let num = read_u16 table (table_off + 2);

      iter 0 IntMap.empty

      where rec iter i ct = do
      {
        if i >= num then
          ct
        else do
        {
          let first = read_u16 table (table_off + 6 * i + 4);
          let last  = read_u16 table (table_off + 6 * i + 6);
          let cls   = read_u16 table (table_off + 6 * i + 8);

          add first ct

          where rec add k ct = do
          {
            if k > last then
              iter (i+1) ct
            else
              add (k+1) (IntMap.add k cls ct)
          }
        }
      }
    }
  | _ -> raise (Failure "unknown class table format")
  ]
};

value read_pos_rule_record table table_off read_lookup num = do
{
  Array.init num
    (fun i -> { prr_seq_idx =              read_u16 table (table_off + 4 * i);
                prr_lookup  = read_lookup (read_u16 table (table_off + 4 * i + 2))})
};

value read_context_subrule table table_off read_lookup first i = do
{
  let off = table_off + read_u16 table (table_off + 2 * i + 2);

  let num1 = read_u16 table off;
  let num2 = read_u16 table (off + 2);

  let first_array =
    Array.init num1
      (fun i -> do
        {
          if i = 0 then
            first
          else
            read_u16 table (off + 2 * i + 2)
        });

  let sl_off = off + 2 * num1 + 2;

  (first_array,
   read_pos_rule_record table sl_off read_lookup num2)
};

value read_context_1 _read read_lookup table table_off is_gpos = do
{
  let read_rule glyphs i = do
  {
    let off = table_off + read_u16 table (table_off + 2 * i + 6);

    if off = table_off then
      [| |]
    else do
    {
      let num_sub = read_u16 table off;

      Array.init num_sub (read_context_subrule table off read_lookup glyphs.(i))
    }
  };

  let coverage  = read_u16 table (table_off + 2);
  let num_rules = read_u16 table (table_off + 4);

  let glyphs = get_coverage_table table (table_off + coverage);

  let rules = Array.init num_rules (read_rule glyphs);

  iter 0 0 DynUCTrie.empty

  where rec iter i k trie = do
  {
    if i >= Array.length rules then do
    {
      if is_gpos then
        ContextGlyphPos   trie
      else
        ContextGlyphSubst trie
    }
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 trie
    else do
    {
      let (glyphs, lookups) = rules.(i).(k);

      iter i (k+1) (DynUCTrie.add_string glyphs lookups trie)
    }
  }
};

value read_context_2 _read read_lookup table table_off is_gpos = do
{
  let read_rule table_off i = do
  {
    let off = table_off + read_u16 table (table_off + 2 * i);

    if off = table_off then
      [| |]
    else do
    {
      let num_sub = read_u16 table off;

      Array.init num_sub (read_context_subrule table off read_lookup i)
    }
  };

(*  let coverage = read_u16 table (table_off + 2); *)
  let class_off = read_u16 table (table_off + 4);
  let num_rules = read_u16 table (table_off + 6);

(*  let glyphs    = get_coverage_table table (table_off + coverage); *)
  let class_table = read_class_table table (table_off + class_off);

  let rules = Array.init num_rules (read_rule (table_off + 8));

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules;
  let ps_rules = Array.make num_psr { psr_data = [||]; psr_lookups = [||] };

  iter 0 0 0

  where rec iter i k n = do
  {
    if i >= Array.length rules then do
    {
      if is_gpos then
        ContextClassPos   class_table ps_rules
      else
        ContextClassSubst class_table ps_rules
    }
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else do
    {
      let (classes, lookups) = rules.(i).(k);

      ps_rules.(n) :=
        {
          psr_data    = classes;
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    }
  }
};

value read_context_3 _read read_lookup table table_off is_gpos = do
{
  let num_glyphs = read_u16 table (table_off + 2);
  let num_sl     = read_u16 table (table_off + 4);

  let coverage   = read_u16_array table (table_off + 6) num_glyphs;

  let sl = read_pos_rule_record table (table_off + 2 * num_glyphs + 6) read_lookup num_sl;

  let glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      coverage;

  let ps_rules =
    [| {
         psr_data    = glyphs;
         psr_lookups = sl
       } |];

  if is_gpos then
    ContextCoveragePos   ps_rules
  else
    ContextCoverageSubst ps_rules
};

value rec read_context read read_lookup table table_off is_gpos = do
{
  match read_u16 table table_off with
  [ 1 -> read_context_1 read read_lookup table table_off is_gpos
  | 2 -> read_context_2 read read_lookup table table_off is_gpos
  | 3 -> read_context_3 read read_lookup table table_off is_gpos
  | _ -> NoCommand
  ]
};

value read_chaining_subrule table table_off read_lookup first i = do
{
  let b_off   = table_off + read_u16 table (table_off + 2 * i + 2);
  let b_num   = read_u16 table b_off;
  let b_array = read_u16_array table (b_off + 2) b_num;

  let i_off = b_off + 2 * b_num + 2;
  let i_num = read_u16 table i_off;

  let i_array =
    Array.init i_num
      (fun i -> do
        {
          if i = 0 then
            first
          else
            read_u16 table (i_off + 2 * i)
        });

  let a_off = i_off + 2 * i_num + 2;
  let a_num = read_u16 table a_off;

  let a_array = read_u16_array table (a_off + 2) a_num;

  let p_off = a_off + 2 * a_num + 2;
  let num_p = read_u16 table p_off;

  (b_array, i_array, a_array,
   read_pos_rule_record table (p_off + 2) read_lookup num_p)
};

value read_chaining_1 _read read_lookup table table_off is_gpos = do
{
  let read_rule glyphs i = do
  {
    let off = table_off + read_u16 table (table_off + 2 * i + 6);

    if off = table_off then
      [| |]
    else do
    {
      let num_sub = read_u16 table off;

      Array.init num_sub (read_chaining_subrule table off read_lookup glyphs.(i))
    }
  };

  let coverage  = read_u16 table (table_off + 2);
  let num_rules = read_u16 table (table_off + 4);

  let glyphs = get_coverage_table table (table_off + coverage);

  let rules = Array.init num_rules (read_rule glyphs);

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules;
  let ps_rules = Array.make num_psr { psr_data = ([||], [||], [||]); psr_lookups = [||] };

  iter 0 0 0

  where rec iter i k n = do
  {
    if i >= Array.length rules then do
    {
      if is_gpos then
        ChainGlyphPos   ps_rules
      else
        ChainGlyphSubst ps_rules
    }
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else do
    {
      let (b,g,f, lookups) = rules.(i).(k);

      ps_rules.(n) :=
        {
          psr_data    = (b,g,f);
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    }
  }
};

value read_chaining_2 _read read_lookup table table_off is_gpos = do
{
  let read_rule table_off i = do
  {
    let off = table_off + read_u16 table (table_off + 2 * i);

    if off = table_off then
      [| |]
    else do
    {
      let num_sub = read_u16 table off;

      Array.init num_sub (read_chaining_subrule table off read_lookup i)
    }
  };

(*  let coverage  = read_u16 table (table_off +  2); *)
  let b_class_off = read_u16 table (table_off +  4);
  let g_class_off = read_u16 table (table_off +  6);
  let f_class_off = read_u16 table (table_off +  8);
  let num_rules   = read_u16 table (table_off + 10);

(*  let glyphs  = get_coverage_table table (table_off + coverage); *)
  let b_classes = read_class_table table (table_off + b_class_off);
  let g_classes = read_class_table table (table_off + g_class_off);
  let f_classes = read_class_table table (table_off + f_class_off);

  let rules = Array.init num_rules (read_rule (table_off + 12));

  let num_psr =
    Array.fold_left
      (fun n sr -> n + Array.length sr)
      0
      rules;
  let ps_rules = Array.make num_psr { psr_data = ([||], [||], [||]); psr_lookups = [||] };

  iter 0 0 0

  where rec iter i k n = do
  {
    if i >= Array.length rules then do
    {
      if is_gpos then
        ChainClassPos   b_classes g_classes f_classes ps_rules
      else
        ChainClassSubst b_classes g_classes f_classes ps_rules
    }
    else if k >= Array.length rules.(i) then
      iter (i+1) 0 n
    else do
    {
      let (b,g,f, lookups) = rules.(i).(k);

      ps_rules.(n) :=
        {
          psr_data    = (b,g,f);
          psr_lookups = lookups
        };

      iter i (k+1) (n+1)
    }
  }
};

value read_chaining_3 _read read_lookup table table_off is_gpos = do
{
  let num_b      = read_u16 table (table_off + 2);
  let b_coverage = read_u16_array table (table_off + 4) num_b;
  let n_off      = table_off + 2 * num_b + 4;
  let num_n      = read_u16 table n_off;
  let n_coverage = read_u16_array table (n_off + 2) num_n;
  let f_off      = n_off + 2 * num_n + 2;
  let num_f      = read_u16 table f_off;
  let f_coverage = read_u16_array table (f_off + 2) num_f;
  let sl_off     = f_off + 2 * num_f + 2;
  let num_sl     = read_u16 table sl_off;

  let sl = read_pos_rule_record table (sl_off + 2) read_lookup num_sl;

  let b_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      b_coverage;
  let n_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      n_coverage;
  let f_glyphs =
    Array.map
      (fun off -> get_coverage_table table (table_off + off))
      f_coverage;

  let ps_rules =
    [| {
         psr_data    = (b_glyphs, n_glyphs, f_glyphs);
         psr_lookups = sl
       } |];

  if is_gpos then
    ChainCoveragePos   ps_rules
  else
    ChainCoverageSubst ps_rules
};

value rec read_chaining read read_lookup table table_off is_gpos = do
{
  match read_u16 table table_off with
  [ 1 -> read_chaining_1 read read_lookup table table_off is_gpos
  | 2 -> read_chaining_2 read read_lookup table table_off is_gpos
  | 3 -> read_chaining_3 read read_lookup table table_off is_gpos
  | _ -> NoCommand
  ]
};

value read_extension read read_lookup table table_off = do
{
  let lu_type = read_u16 table (table_off + 2);
  let offset  = read_u16 table (table_off + 4);

  read read_lookup table (table_off + offset) lu_type
};

value parse_table table read = do
{
  let script_offset  = read_u16 table 4;
  let feature_offset = read_u16 table 6;
  let lookup_offset  = read_u16 table 8;

  let features = parse_features table feature_offset;
  let scripts  = parse_scripts  table script_offset features;
  let lookups  = parse_lookups  table lookup_offset read;

  let rec get_size f = do
  {
    if f >= Array.length features then
      None
    else if features.(f).f_tag = size_tag then
      Some (parse_size_params table (feature_offset + features.(f).f_params))
    else
      get_size (f+1)
  };
  let size = get_size 0;

  make_pos_subst_table scripts lookups size
};

end;

module GPOS =
struct

open Table;
open Tag;
open OTF_Pos_Subst;

type value_record =
{
  x_placement : int;
  y_placement : int;
  x_advance   : int;
  y_advance   : int
};

value value_record_len vf = do
{
  let bit i = if vf land i <> 0 then 2 else 0;

    bit 0x01 + bit 0x02 + bit 0x04 + bit 0x08
  + bit 0x10 + bit 0x20 + bit 0x40 + bit 0x80
};

value read_value_record table table_off vf = do
{
  let read (t,o) bit = if bit = 0 then ((t,o), 0) else ((t, o+2), read_i16 t o);

  let (t1, x_placement) = read (table, table_off) (vf land 0x01);
  let (t2, y_placement) = read t1                 (vf land 0x02);
  let (t3, x_advance)   = read t2                 (vf land 0x04);
  let (_,  y_advance)   = read t3                 (vf land 0x08);

  {
    x_placement = x_placement;
    y_placement = y_placement;
    x_advance   = x_advance;
    y_advance   = y_advance
  }
};

value read_value_records table table_off num vf = do
{
  let len = value_record_len vf;

  Array.init num
    (fun i -> read_value_record table (table_off + i * len) vf)
};

value value_record_to_positioning vr =
{
  p_x_off     = vr.x_placement;
  p_y_off     = vr.y_placement;
  p_h_adv_off = vr.x_advance;
  p_v_adv_off = vr.y_advance
};

value gpos_simple_pos table table_off = do
{
  let format = read_u16 table table_off;

  if format <> 1 && format <> 2 then
    NoCommand
  else do
  {
    let coverage     = read_u16 table (table_off + 2);
    let value_format = read_u16 table (table_off + 4);

    if value_format land 0xf = 0 then
      NoCommand
    else do
    {
      let num_records =
        if format = 1 then
          1
        else
          read_u16 table (table_off + 6);
      let record_off =
        if format = 1 then
          table_off + 6
        else
          table_off + 8;
      let value_records = read_value_records table record_off num_records value_format;
      let glyphs        = get_coverage_table table (table_off + coverage);

      Position
        (init_map (Array.length glyphs)
          (fun i -> glyphs.(i))
          (fun i -> value_record_to_positioning
                      value_records.(if format = 1 then 0 else i)))
    }
  }
};

value gpos_kern table table_off = do
{
  let format = read_u16 table table_off;

  if format <> 1 && format <> 2 then
    NoCommand
  else do
  {
    let coverage = read_u16 table (table_off + 2);
    let vf1      = read_u16 table (table_off + 4);
    let vf2      = read_u16 table (table_off + 6);
    let vr1_len  = value_record_len vf1;
    let vr2_len  = value_record_len vf2;

    match format with
    [ 1 -> do
      {
        let num = read_u16 table (table_off + 8);

        let offsets = read_u16_array table (table_off + 10) num;
        let glyphs  = get_coverage_table table (table_off + coverage);

        let record_len = vr1_len + vr2_len + 2;

        let num_pairs = Array.map
                          (fun off -> read_u16 table (table_off + off))
                          offsets;

        let read_sub_table i = do
        {
          let off = table_off + offsets.(i);

          init_map num_pairs.(i)
            (fun k -> read_u16 table (off + k * record_len + 2))
            (fun k -> do
              {
                let vr1 = read_value_record table (off + k * record_len + 4)           vf1;
                let vr2 = read_value_record table (off + k * record_len + vr1_len + 4) vf2;

                (value_record_to_positioning vr1,
                 value_record_to_positioning vr2)
               })
        };

        Kern
          (init_map num
            (fun i -> glyphs.(i))
            read_sub_table)
      }
    | 2 -> do
      {
        let class1_off = read_u16 table (table_off +  8);
        let class2_off = read_u16 table (table_off + 10);
        let class1_num = read_u16 table (table_off + 12);
        let class2_num = read_u16 table (table_off + 14);

        let class1 = read_class_table table (table_off + class1_off);
        let class2 = read_class_table table (table_off + class2_off);

        let kern_val1 = Array.make (class1_num * class2_num) { p_x_off = 0; p_y_off = 0; p_h_adv_off = 0; p_v_adv_off = 0 };
        let kern_val2 = Array.make (class1_num * class2_num) { p_x_off = 0; p_y_off = 0; p_h_adv_off = 0; p_v_adv_off = 0 };

        for i = 0 to class1_num * class2_num - 1 do
        {
          let off = table_off + i * (vr1_len + vr2_len) + 16;

          kern_val1.(i) := value_record_to_positioning (read_value_record table off             vf1);
          kern_val2.(i) := value_record_to_positioning (read_value_record table (off + vr1_len) vf2)
        };

        KernClass class2_num class1 class2 kern_val1 kern_val2
      }
    | _ -> assert False
    ]
  }
};

value gpos_cursive table table_off = do
{
  if read_u16 table table_off <> 1 then
    NoCommand
  else do
  {
    let coverage = read_u16 table (table_off + 2);
    let num      = read_u16 table (table_off + 4);

    let offsets =
      Array.init num
        (fun i ->
          (read_u16 table (table_off + 4 * i + 6),
           read_u16 table (table_off + 4 * i + 8)));

    let glyphs = get_coverage_table table (table_off + coverage);

    iter 0 IntMap.empty IntMap.empty

    where rec iter i entry_map exit_map = do
    {
      if i >= num then
        CursiveAnchors entry_map exit_map
      else do
      {
        let (entry, exit) = offsets.(i);

        let new_entry =
          if entry = 0 then
            entry_map
          else
            IntMap.add
              glyphs.(i)
              (read_u16 table (table_off + entry + 2),
               read_u16 table (table_off + entry + 4))
             entry_map;
        let new_exit =
          if exit = 0 then
            exit_map
          else
            IntMap.add
              glyphs.(i)
              (read_u16 table (table_off + exit + 2),
               read_u16 table (table_off + exit + 4))
             exit_map;

        iter (i+1) new_entry new_exit
      }
    }
  }
};

value read_marks table table_off mark_glyphs = do
{
  let num = read_u16 table table_off;

  let offsets =
    Array.init num
      (fun i ->
          (read_u16 table (table_off + 4 * i + 2),
           read_u16 table (table_off + 4 * i + 4)));

  iter 0 []

  where rec iter i marks = do
  {
    if i >= num then
      Array.of_list marks
    else do
    {
      let g = mark_glyphs.(i);

      let (cls, off) = offsets.(i);

      if off <> 0 then
        iter (i+1)
             [(g, cls, read_u16 table (table_off + off + 2),
                       read_u16 table (table_off + off + 4))
              :: marks]
      else
        iter (i+1) marks
    }
  }
};

value read_bases table table_off base_glyphs num_classes = do
{
  let num = read_u16 table table_off;

  let offsets = read_u16_array table (table_off + 2) num;

  let read_base i = do
  {
    let anchors =
      Array.init num_classes
        (fun k -> do
          {
            let off = offsets.(i * num_classes + k);

            (read_u16 table (table_off + off + 2),
             read_u16 table (table_off + off + 4))
          });

    (base_glyphs.(i), anchors)
  };

  Array.init num (fun i -> read_base i)
};

value read_ligs table table_off base_glyphs num_classes = do
{
  let num = read_u16 table table_off;

  let loffsets = read_u16_array table (table_off + 2) num;

  let read_lig i = do
  {
    let off = table_off + loffsets.(i);

    let num_comp = read_u16 table off;
    let aoffsets = read_u16_array table (off + 2) (num_classes * num_comp);

    let anchors =
      Array.init num_comp
        (fun k -> Array.init num_classes
          (fun c -> do
            {
              let aoff = aoffsets.(k * num_classes + c);

              (read_u16 table (off + aoff + 2),
               read_u16 table (off + aoff + 4))
            }));

    (base_glyphs.(i), anchors)
  };

  Array.init num (fun i -> read_lig i)
};

value gpos_mark read_base table table_off = do
{
  let mark_coverage = read_u16 table (table_off +  2);
  let base_coverage = read_u16 table (table_off +  4);
  let num_classes   = read_u16 table (table_off +  6);
  let mark_offset   = read_u16 table (table_off +  8);
  let base_offset   = read_u16 table (table_off + 10);

  let mark_glyphs = get_coverage_table table (table_off + mark_coverage);
  let base_glyphs = get_coverage_table table (table_off + base_coverage);

  let marks = read_marks table (table_off + mark_offset) mark_glyphs;
  let bases = read_base  table (table_off + base_offset) base_glyphs num_classes;

  (marks, bases)
};

value rec read_gpos allow_extension read_lookup table table_off l_type = match l_type with
[ 1 -> gpos_simple_pos table table_off
| 2 -> gpos_kern table table_off
| 3 -> gpos_cursive table table_off
| 4 -> let (m,b) = gpos_mark read_bases  table table_off in MarkToBaseAnchors m b
| 5 -> let (m,b) = gpos_mark read_ligs   table table_off in MarkToLigAnchors  m b
| 6 -> let (m,b) = gpos_mark read_bases  table table_off in MarkToMarkAnchors m b
| 7 -> read_context  (read_gpos True read_lookup) read_lookup table table_off True
| 8 -> read_chaining (read_gpos True read_lookup) read_lookup table table_off True
| 9 -> if allow_extension then
         read_extension (read_gpos False) read_lookup table table_off
       else
         raise (Failure "corrupt extension table")
| _ -> raise (Failure "invalid gpos table")
];

value parse_gpos_table table = parse_table table (read_gpos True);

end;

module GSUB =
struct

open Table;
open Tag;
open OTF_Pos_Subst;

value gsub_simple_subst table table_off = do
{
  let format = read_u16 table table_off;

  if format <> 1 && format <> 2 then
    NoCommand
  else do
  {
    let coverage    = read_u16 table (table_off + 2);
    let from_glyphs = get_coverage_table table (table_off + coverage);

    if format = 1 then do
    {
      let delta = read_u16 table (table_off + 4);

      Substitution
        (init_map (Array.length from_glyphs)
          (fun i -> from_glyphs.(i))
          (fun i -> from_glyphs.(i) + delta))
    }
    else do
    {
      let num = read_u16 table (table_off + 4);

      let to_glyphs = read_u16_array table (table_off + 6) num;

      Substitution
        (init_map (Array.length from_glyphs)
          (fun i -> from_glyphs.(i))
          (fun i -> to_glyphs.(i)))
    }
  }
};

value gsub_multiple table table_off = do
{
  if read_u16 table table_off <> 1 then
    IntMap.empty
  else do
  {
    let coverage = read_u16 table (table_off + 2);
    let num_off  = read_u16 table (table_off + 4);
    let offsets  = read_u16_array table (table_off + 6) num_off;
    let glyphs   = get_coverage_table table (table_off + coverage);

    init_map (Array.length glyphs)
      (fun i -> glyphs.(i))
      (fun i -> do
        {
          let off        = table_off + offsets.(i);
          let num_glyphs = read_u16 table off;
          let to_glyphs  = read_u16_array table (off + 2) num_glyphs;

          to_glyphs
        })
  }
};

value gsub_ligature table table_off = do
{
  let coverage = read_u16 table (table_off + 2);
  let num      = read_u16 table (table_off + 4);
  let offsets  = read_u16_array table (table_off + 6) num;
  let glyphs   = get_coverage_table table (table_off + coverage);

  iter_i 0 DynUCTrie.empty

  where rec iter_i i trie = do
  {
    if i >= num then
      Ligature trie
    else do
    {
      let off         = table_off + offsets.(i);
      let num_ligs    = read_u16 table off;
      let lig_offsets = read_u16_array table (off + 2) num_ligs;

      iter_k 0 trie

      where rec iter_k k trie = do
      {
        if k >= num_ligs then
          iter_i (i+1) trie
        else do
        {
          let loff         = off + lig_offsets.(k);
          let lig          = read_u16 table loff;
          let num_lig_comp = read_u16 table (loff + 2);

          let lig_glyphs =
            Array.init num_lig_comp
              (fun l -> if l = 0 then
                          glyphs.(i)
                        else
                          read_u16 table (loff + 2 * l + 2));

          iter_k (k+1) (DynUCTrie.add_string lig_glyphs lig trie)
        }
      }
    }
  }
};

value gsub_reverse_chain table table_off = do
{
  if read_u16 table table_off <> 1 then
    NoCommand
  else do
  {
    let coverage = read_u16 table (table_off + 2);
    let glyphs   = get_coverage_table table (table_off + coverage);

    let num_b      = read_u16 table (table_off + 4);
    let b_coverage = read_u16_array table (table_off + 6) num_b;
    let f_off      = table_off + 2 * num_b + 6;
    let num_f      = read_u16 table f_off;
    let f_coverage = read_u16_array table (f_off + 2) num_f;
    let s_off      = f_off + 2 * num_f + 2;
    let num_s      = read_u16 table s_off;
    let s_glyphs   = read_u16_array table (s_off + 2) num_s;

    ReverseSubst glyphs s_glyphs
      (Array.map (fun off -> get_coverage_table table (table_off + off)) b_coverage)
      (Array.map (fun off -> get_coverage_table table (table_off + off)) f_coverage)
  }
};

value rec read_gsub allow_extension read_lookup table table_off l_type = match l_type with
[ 1 -> gsub_simple_subst table table_off
| 2 -> Multiple  (gsub_multiple table table_off)
| 3 -> Alternate (gsub_multiple table table_off)
| 4 -> gsub_ligature                              table table_off
| 5 -> read_context  (read_gsub True) read_lookup table table_off False
| 6 -> read_chaining (read_gsub True) read_lookup table table_off False
| 7 -> if allow_extension then
         read_extension (read_gsub False) read_lookup table table_off
       else
         raise (Failure "corrupt extension table")
| 8 -> gsub_reverse_chain table table_off
| _ -> raise (Failure "invalid gsub table")
];

value parse_gsub_table table = parse_table table (read_gsub True);

end;

type glyph_metric =
{
  g_adv_width : int;
  g_min_x     : int;
  g_min_y     : int;
  g_max_x     : int;
  g_max_y     : int
};

type otf_font =
{
  otf_units_per_em : int;
  otf_glyphs       : array glyph_metric;
  otf_gpos         : option OTF_Pos_Subst.pos_subst_table;
  otf_gsub         : option OTF_Pos_Subst.pos_subst_table
};

module Parse_Simple_Tables =
struct

open Table;

value parse_head table = do
{
  let units_per_em        = read_u16 table 18;
  let index_to_loc_format = read_u16 table 50;

  (units_per_em, index_to_loc_format <> 0)
};

value parse_hhea table = do
{
(*  let ascent        = read_u16 table  4;
  let descent       = read_u16 table  6;
  let leading       = read_u16 table  8;*)
  let num_h_metrics = read_u16 table 34;

  num_h_metrics
};

value parse_maxp table = do
{
  let num_glyphs = read_u16 table 4;

  num_glyphs
};

value parse_hmtx table num_glyphs num_h_metrics = do
{
  let width = Array.make num_glyphs 0;

  for i = 0 to num_h_metrics - 1 do
  {
    width.(i) := read_u16 table (4 * i)
  };

  if num_h_metrics < num_glyphs then
    for i = num_h_metrics to num_glyphs - 1 do
    {
      width.(i) := width.(num_h_metrics - 1)
    }
  else ();

  width
};

value parse_loca table num_glyphs index_to_loc_format = do
{
  if index_to_loc_format then
    Array.init num_glyphs
      (fun i -> Int32.to_int (read_u32 table (4 * i)))
  else
    Array.init num_glyphs
      (fun i -> 2 * read_u16 table (2 * i))
};

value parse_glyf table glyph_locations = do
{
  Array.init
    (Array.length glyph_locations)
    (fun i -> do
      {
        let off = glyph_locations.(i);

        (read_u16 table (off+2),
         read_u16 table (off+4),
         read_u16 table (off+6),
         read_u16 table (off+8))
      })
};

end;

(*
module CFF =
struct

open Table;

value parse_cff table = do
{
  if read_u8 table 0 <> 1 then
    raise (Failure "wrong CFF version")
  else do
  {
    let header_len  = read_u8 table 2;
    let offset_size = read_u8 table 3;

    ()
  }
};

end;
*)

module Parse_OTF =
struct

open Tag;

value read_tag is = do
{
  let x = IO.read_be_u16 is;
  let y = IO.read_be_u16 is;

  compose_tag x y
};

value read_tables is = do
{
  let version = read_tag is;

  if version = ttcf_tag then
    raise (Failure "font collections not supported")
  else if version <> Int32.of_int 0x10000 &&
          version <> Int32.of_int 0x20000 &&
          version <> otto_tag then
    raise (Failure "unknown font type")
  else do
  {
    let num_tables = IO.read_be_u16 is;

    IO.skip is 6;

    let table_list =
      Array.init num_tables
        (fun _ -> do
          {
            let tag = read_tag is;

            IO.skip is 4;

            let offset = int_of_num (IO.read_be_u32 is);
            let length = int_of_num (IO.read_be_u32 is);

            (tag, offset, length)
          });

    iter 0 TagMap.empty

    where rec iter i tables = do
    {
      if i >= num_tables then
        tables
      else do
      {
        let (tag, off, len) = table_list.(i);

        IO.seek is off;

        iter (i+1) (TagMap.add tag (IO.read_string is len) tables)
      }
    }
  }
};

value parse_cff_font tables num_glyphs units_per_em advance_widths gpos gsub = do
{
  let glyphs =
    Array.init num_glyphs
      (fun i -> do
        {
          let (x0,y0,x1,y1) = (0,0,0,0); (*bboxes.(i)*)

          {
            g_adv_width = advance_widths.(i);
            g_min_x     = x0;
            g_min_y     = y0;
            g_max_x     = x1;
            g_max_y     = y1
          }
        });

  let font =
  {
    otf_units_per_em = units_per_em;
    otf_glyphs       = glyphs;
    otf_gpos         = gpos;
    otf_gsub         = gsub
  };

  (* not implemented *)

  font
};

value parse_ttf_font tables num_glyphs units_per_em index_to_loc_format advance_widths gpos gsub = do
{
  let loca_table = TagMap.find loca_tag tables;
  let glyf_table = TagMap.find loca_tag tables;

  let locations = Parse_Simple_Tables.parse_loca loca_table num_glyphs index_to_loc_format;
  let bboxes    = Parse_Simple_Tables.parse_glyf glyf_table locations;

  let glyphs =
    Array.init num_glyphs
      (fun i -> do
        {
          let (x0,y0,x1,y1) = bboxes.(i);

          {
            g_adv_width = advance_widths.(i);
            g_min_x     = x0;
            g_min_y     = y0;
            g_max_x     = x1;
            g_max_y     = y1
          }
        });

  (* FIX: read kern table, if present *)

  let font =
  {
    otf_units_per_em = units_per_em;
    otf_glyphs       = glyphs;
    otf_gpos         = gpos;
    otf_gsub         = gsub
  };

  font
};

value read_pos_subst tables = do
{
  let gpos_table =
    try
      Some (GPOS.parse_gpos_table (TagMap.find gpos_tag tables))
    with [ Not_found -> None ];
  let gsub_table =
    try
      Some (GSUB.parse_gsub_table (TagMap.find gsub_tag tables))
    with [ Not_found -> None ];

  (gpos_table, gsub_table)
};

(* only return the pos_subst data *)

value parse_pos_subst tables = do
{
  try
    read_pos_subst tables
  with
  [ Not_found -> (None,None) ]
};

(* load the whole font *)

value parse_font is = do
{
  let tables = read_tables is;

  try do
  {
    let head_table = TagMap.find head_tag tables;
    let hhea_table = TagMap.find hhea_tag tables;
    let maxp_table = TagMap.find maxp_tag tables;
    let hmtx_table = TagMap.find hmtx_tag tables;

    let (upm, itlf)    = Parse_Simple_Tables.parse_head head_table;
    let num_h_metrics  = Parse_Simple_Tables.parse_hhea hhea_table;
    let num_glyphs     = Parse_Simple_Tables.parse_maxp maxp_table;
    let advance_widths = Parse_Simple_Tables.parse_hmtx hmtx_table num_glyphs num_h_metrics;

    let (gpos, gsub)   = read_pos_subst tables;

    let font =
      if TagMap.mem glyf_tag tables then
        parse_ttf_font tables num_glyphs upm itlf advance_widths gpos gsub
      else
        parse_cff_font tables num_glyphs upm advance_widths gpos gsub;

    font
  }
  with
  [ Not_found -> raise (Failure "font lacks required tables") ]
};

end;

module WriteOTF =
struct

open Tag;

value check_sum str = do
{
  let get i = do
  {
    if i >= String.length str then
      0
    else
      int_of_char (String.unsafe_get str i)
  };

  iter 0 Int32.zero

  where rec iter pos chk = do
  {
    if pos >= String.length str then
      chk
    else do
    {
      let a = get pos;
      let b = get (pos+1);
      let c = get (pos+2);
      let d = get (pos+3);

      let x = compose_tag ((a lsl 8) + b) ((c lsl 8) + d);

      iter (pos + 4) (Int32.add chk x)
    }
  }
};

value write_tag os tag = do
{
  let x = Int32.to_int (Int32.logand (Int32.shift_right tag 16) (Int32.of_int 0xffff));
  let y = Int32.to_int (Int32.logand tag (Int32.of_int 0xffff));

  IO.write_be_u16 os x;
  IO.write_be_u16 os y
};

value write_tables os tables ttf = do
{
  (* create preamble *)

  let preamble = IO.make_buffer_stream 0x1000;

  if ttf then
    write_tag preamble (Int32.of_int 0x10000)
  else
    write_tag preamble otto_tag;

  let num_tables = Array.length tables;
  let num_bits   = iter 0 num_tables
    where rec iter x i = do
    {
      if 1 lsr i > x then
        i - 1
      else
        iter x (i+1)
    };

  IO.write_be_u16 preamble num_tables;
  IO.write_be_u16 preamble (16 * (1 lsl num_bits));
  IO.write_be_u16 preamble num_bits;
  IO.write_be_u16 preamble (16 * num_tables - num_bits);

  let table_offsets = Array.make num_tables 0;
  let _ = Array.fold_left
            (fun (i,off) (_,table) -> do
              {
                let len     = String.length table;
                let new_off = if len land 3 = 0 then
                                off + len
                              else
                                off + len + 4 - (len land 3);

                table_offsets.(i) := off;

                (i+1, new_off)
              })
            (0, 12 + 16 * num_tables)
            tables;

  let chk = ref Int32.zero;

  for i = 0 to num_tables - 1 do
  {
    let (tag,tab) = tables.(i);

    let c = check_sum tab;

    !chk := Int32.add !chk c;

    write_tag       preamble tag;
    write_tag       preamble c;
    IO.write_be_u32 preamble (num_of_int table_offsets.(i));
    IO.write_be_u32 preamble (num_of_int (String.length tab))
  };

  (* write preamble *)

  let p = IO.to_string preamble;

  IO.write_string os p;

  !chk = Int32.add !chk (check_sum p);

  (* Fix check sum in head table. *)
  (* We assume that the head table is the first element of <tables>! *)

  let (_, head) = tables.(0);

  let h = IO.from_string head;
  IO.seek h 8;
  write_tag h (Int32.sub (compose_tag 0xb1b0 0xafba) !chk);

  tables.(0) := (head_tag, IO.to_string h);

  (* write tables *)

  for i = 0 to num_tables - 1 do
  {
    let (_,tab) = tables.(i);

    IO.write_string os tab;

    (* padding *)

    if String.length tab land 3 <> 0 then
      for n = String.length tab land 3 to 3 do
      {
        IO.write_byte os 0
      }
    else ()
  }
};

value make_head_table font = do
{
  (* Just copy the table but set the checksum to 0.*)

  let head = TagMap.find head_tag font;

  let s = IO.from_string head;

  IO.seek s 8;
  IO.write_be_u32 s num_zero;

  IO.to_string s
};

value make_hhea_table font encoding = do
{
  let hhea = TagMap.find hhea_tag font;

  (* Just change the number of metrics. *)

  let s = IO.from_string hhea;

  IO.seek s 34;
  IO.write_be_u16 s (Array.length encoding);

  IO.to_string s
};

value make_hmtx_table font encoding = do
{
  let hhea = IO.make_string_stream (TagMap.find hhea_tag font);
  let maxp = IO.make_string_stream (TagMap.find maxp_tag font);
  let hmtx = IO.make_string_stream (TagMap.find hmtx_tag font);

  IO.seek hhea 34;
  IO.seek maxp 4;

  let num_metrics = IO.read_be_u16 hhea;
  let num_glyphs  = IO.read_be_u16 maxp;

  let width        = Array.make num_glyphs 0;
  let side_bearing = Array.make num_glyphs 0;

  let os = IO.make_buffer_stream (4 * Array.length encoding);

  for i = 0 to num_metrics - 1 do
  {
    width.(i)        := IO.read_be_u16 hmtx;
    side_bearing.(i) := IO.read_be_i16 hmtx
  };

  for i = num_metrics to num_glyphs - 1 do
  {
    width.(i) := width.(num_metrics - 1);
    side_bearing.(i) := IO.read_be_i16 hmtx
  };

  for i = 0 to Array.length encoding - 1 do
  {
    IO.write_be_u16 os width.(encoding.(i));
    IO.write_be_i16 os side_bearing.(encoding.(i))
  };

  IO.to_string os
};

value make_maxp_table font encoding = do
{
  let maxp = TagMap.find maxp_tag font;

  (* Just change the number of glyphs. *)

  let s = IO.from_string maxp;

  IO.seek s 4;
  IO.write_be_u16 s (Array.length encoding);

  IO.to_string s
};

value make_cmap_table encoding = do
{
  let cmap = IO.make_buffer_stream 0x100;

  IO.write_be_u16 cmap 0;
  IO.write_be_u16 cmap 1;

  IO.write_be_u16 cmap 1;
  IO.write_be_u16 cmap 0;
  IO.write_be_u32 cmap (num_of_int 12);

  IO.write_be_u16 cmap 0;
  IO.write_be_u16 cmap (6 + 256);
  IO.write_be_u16 cmap 0;

  for i = 0 to 255 do
  {
    if i < Array.length encoding then
      IO.write_be_u8 cmap i
    else
      IO.write_be_u8 cmap 0
  };

  IO.to_string cmap
};

value make_name_table font = do
{
  TagMap.find name_tag font
};

value make_os2_table  font = do
{
  TagMap.find os2_tag font
};

value make_post_table font = do
{
  let post = IO.make_string_stream (TagMap.find post_tag font);

  IO.seek post 4;

  let os = IO.make_buffer_stream 0x80;

  IO.write_be_u16 os 3;
  IO.write_be_u16 os 0;
  IO.write_string os (IO.read_string post 28);

  IO.to_string os
};

value make_cvt_table  font = do
{
  TagMap.find cvt_tag font
};

value make_fpgm_table font = do
{
  TagMap.find fpgm_tag font
};

value make_prep_table font = do
{
  TagMap.find prep_tag font
};

value make_glyf_loca_tables font encoding = do
{
  let head = TagMap.find head_tag font;
  let glyf = TagMap.find glyf_tag font;
  let loca = TagMap.find loca_tag font;

  let loca_fmt = (head.[50] <> '\000' || head.[51] <> '\000');

  let igs = IO.make_string_stream glyf;
  let ils = IO.make_string_stream loca;
  let ogs = IO.make_buffer_stream (String.length glyf);
  let ols = IO.make_buffer_stream (4 * Array.length encoding);

  let get_loca = if loca_fmt then
                   (fun n -> do
                     {
                       IO.seek ils (4 * n);
                       int_of_num (IO.read_be_u32 ils)
                     })
                 else
                   (fun n -> do
                     {
                       IO.seek ils (2 * n);
                       2 * IO.read_be_u16 ils
                     });
  let set_loca = if loca_fmt then
                   (fun off -> IO.write_be_u32 ols (num_of_int off))
                 else
                   (fun off -> IO.write_be_u16 ols (off / 2));

  for i = 0 to Array.length encoding - 1 do
  {
    let off  = get_loca encoding.(i);
    let next = get_loca (encoding.(i) + 1);

    set_loca (IO.bytes_written ogs);

    IO.seek igs off;
    IO.write_string ogs (IO.read_string igs (next - off));
  };
  set_loca (IO.bytes_written ogs);

  (IO.to_string ogs, IO.to_string ols)
};

value make_cff_table font encoding = do
{
  ""
};

value write_ttf_subset stream font encoding = do
{
  let cmap = make_cmap_table encoding;
  let head = make_head_table font;
  let hhea = make_hhea_table font encoding;
  let hmtx = make_hmtx_table font encoding;
  let maxp = make_maxp_table font encoding;
  let name = make_name_table font;
  let os2  = make_os2_table  font;
  let post = make_post_table font;
  let cvt  = make_cvt_table  font;
  let prep = make_prep_table font;
  let fpgm = make_fpgm_table font;

  let (glyf, loca) = make_glyf_loca_tables font encoding;

  write_tables stream
    [|
      (head_tag, head);
      (hhea_tag, hhea);
      (maxp_tag, maxp);
      (os2_tag,  os2);
      (hmtx_tag, hmtx);
      (fpgm_tag, fpgm);
      (prep_tag, prep);
      (cvt_tag,  cvt);
      (loca_tag, loca);
      (glyf_tag, glyf);
      (post_tag, post);
      (name_tag, name);
      (cmap_tag, cmap)
    |]
    True
};

value write_cff_subset stream font encoding = do
{
  let cmap = make_cmap_table encoding;
  let head = make_head_table font;
  let hhea = make_hhea_table font encoding;
  let hmtx = make_hmtx_table font encoding;
  let maxp = make_maxp_table font encoding;
  let name = make_name_table font;
  let os2  = make_os2_table  font;
  let post = make_post_table font;
  let cff  = make_cff_table  font encoding;

  write_tables stream
    [|
      (head_tag, head);
      (hhea_tag, hhea);
      (maxp_tag, maxp);
      (os2_tag,  os2);
      (name_tag, name);
      (cmap_tag, cmap);
      (post_tag, post);
      (cff_tag,  cff);
      (hmtx_tag, hmtx)
    |]
    False
};

end;

type otf_tables = Tag.TagMap.t string;

value read_font_tables name = do
{
  Parse_OTF.read_tables (IO.make_rand_in_stream name)
};

value get_pos_subst font = do
{
  Parse_OTF.parse_pos_subst font
};

value is_cff font = do
{
  Tag.TagMap.mem Tag.cff_tag font
};

value get_cff font = do
{
  Tag.TagMap.find Tag.cff_tag font
};

value read_font name = do
{
  Parse_OTF.parse_font (IO.make_rand_in_stream name)
};

value write_subset stream font encoding = do
{
  if Tag.TagMap.mem Tag.glyf_tag font then
    WriteOTF.write_ttf_subset stream font encoding
  else
    WriteOTF.write_cff_subset stream font encoding
};

