
open XNum;
open Runtime;
open Logging;
open VM;
open Typesetting;
open Engine;
open ParseState;

module UString   = Unicode.UString;
module SymbolMap = Unicode.SymbolTable.SymbolMap;

(* symbols *)

value decl_sym str = Machine.string_to_symbol (UString.uc_string_of_ascii str);

value sym_Accent               = decl_sym "Accent";
value sym_AddToGalley          = decl_sym "AddToGalley";
value sym_AdjDemerits          = decl_sym "AdjDemerits";
value sym_Adjustments          = decl_sym "Adjustments";
value sym_Alignment            = decl_sym "Alignment";
value sym_AutoLigatures        = decl_sym "AutoLigatures";
value sym_Base                 = decl_sym "Base";
value sym_BaselineSkip         = decl_sym "BaselineSkip";
value sym_BeginGroup           = decl_sym "BeginGroup";
value sym_Bevel                = decl_sym "Bevel";
value sym_BinOp                = decl_sym "BinOp";
value sym_BinOpPenalty         = decl_sym "BinOpPenalty";
value sym_Bitmap               = decl_sym "Bitmap";
value sym_Bmp                  = decl_sym "Bmp";
value sym_Bool                 = decl_sym "Bool";
value sym_BorderKern           = decl_sym "BorderKern";
value sym_Bottom               = decl_sym "Bottom";
value sym_BottomSkip           = decl_sym "BottomSkip";
value sym_Break                = decl_sym "Break";
value sym_Butt                 = decl_sym "Butt";
value sym_Circle               = decl_sym "Circle";
value sym_Clip                 = decl_sym "Clip";
value sym_Close                = decl_sym "Close";
value sym_ClubPenalty          = decl_sym "ClubPenalty";
value sym_CMYK                 = decl_sym "CMYK";
value sym_Command              = decl_sym "Command";
value sym_CommandBox           = decl_sym "CommandBox";
value sym_CrampedDisplay       = decl_sym "CrampedDisplay";
value sym_CrampedScript        = decl_sym "CrampedScript";
value sym_CrampedScript2       = decl_sym "CrampedScriptScript";
value sym_CrampedText          = decl_sym "CrampedText";
value sym_Default              = decl_sym "Default";
value sym_DelimiterFactor      = decl_sym "DelimiterFactor";
value sym_DelimiterShortfall   = decl_sym "DelimiterShortfall";
value sym_Direct               = decl_sym "Direct";
value sym_Display              = decl_sym "Display";
value sym_DoubleHyphenDemerits = decl_sym "DoubleHyphenDemerits";
value sym_Dpi                  = decl_sym "Dpi";
value sym_EmergencyStretch     = decl_sym "EmergencyStretch";
value sym_Encoding             = decl_sym "Encoding";
value sym_EndGroup             = decl_sym "EndGroup";
value sym_ExHyphenPenalty      = decl_sym "ExHyphenPenalty";
value sym_Family               = decl_sym "Family";
value sym_Fill                 = decl_sym "Fill";
value sym_FinalHyphenDemerits  = decl_sym "FinalHyphenDemerits";
value sym_Fixed                = decl_sym "Fixed";
value sym_Float                = decl_sym "Float";
value sym_FloatSep             = decl_sym "FloatSep";
value sym_Footnote             = decl_sym "Footnote";
value sym_Fraction             = decl_sym "Fraction";
value sym_Galley               = decl_sym "Galley";
value sym_GfxCommand           = decl_sym "GfxCommand";
value sym_Glyph                = decl_sym "Glyph";
value sym_Glue                 = decl_sym "Glue";
value sym_Grey                 = decl_sym "Grey";
value sym_GridSize             = decl_sym "GridSize";
value sym_HBox                 = decl_sym "HBox";
value sym_HBoxSpread           = decl_sym "HBoxSpread";
value sym_HBoxTo               = decl_sym "HBoxTo";
value sym_Height               = decl_sym "Height";
value sym_HLeaders             = decl_sym "HLeaders";
value sym_Hyphen               = decl_sym "Hyphen";
value sym_HyphenGlyph          = decl_sym "HyphenGlyph";
value sym_HyphenParams         = decl_sym "HyphenParams";
value sym_HyphenPenalty        = decl_sym "HyphenPenalty";
value sym_HyphenTable          = decl_sym "HyphenTable";
value sym_Image                = decl_sym "Image";
value sym_IndexPosition        = decl_sym "IndexPosition";
value sym_Inner                = decl_sym "Inner";
value sym_Kern                 = decl_sym "Kern";
value sym_Leading              = decl_sym "Leading";
value sym_Left                 = decl_sym "Left";
value sym_LeftHyphenMin        = decl_sym "LeftHyphenMin";
value sym_LeftRight            = decl_sym "LeftRight";
value sym_LeftSkip             = decl_sym "LeftSkip";
value sym_Letter               = decl_sym "Letter";
value sym_LetterSpacing        = decl_sym "LetterSpacing";
value sym_Ligature             = decl_sym "Ligature";
value sym_LineBreakParams      = decl_sym "LineBreakParams";
value sym_LineParams           = decl_sym "LineParams";
value sym_LinePenalty          = decl_sym "LinePenalty";
value sym_LineSkip             = decl_sym "LineSkip";
value sym_LineSkipLimit        = decl_sym "LineSkipLimit";
value sym_Looseness            = decl_sym "Looseness";
value sym_LR                   = decl_sym "LR";
value sym_LRBox                = decl_sym "LRBox";
value sym_Mandantory           = decl_sym "Mandantory";
value sym_Math                 = decl_sym "Math";
value sym_MathAccent           = decl_sym "MathAccent";
value sym_MathChar             = decl_sym "MathChar";
value sym_MathCode             = decl_sym "MathCode";
value sym_MathFamily           = decl_sym "MathFamily";
value sym_MathParams           = decl_sym "MathParams";
value sym_MathStyle            = decl_sym "MathStyle";
value sym_Measure              = decl_sym "Measure";
value sym_MedMathSkip          = decl_sym "MedMathSkip";
value sym_MinSize              = decl_sym "MinSize";
value sym_Miter                = decl_sym "Miter";
value sym_ModifyGalleyGlue     = decl_sym "ModifyGalleyGlue";
value sym_Name                 = decl_sym "Name";
value sym_NewArea              = decl_sym "NewArea";
value sym_NewGalley            = decl_sym "NewGalley";
value sym_NewLayout            = decl_sym "NewLayout";
value sym_NewMarks             = decl_sym "NewMarks";
value sym_NoMath               = decl_sym "NoMath";
value sym_None                 = decl_sym "None";
value sym_NullDelimiterSpace   = decl_sym "NullDelimiterSpace";
value sym_OldMarks             = decl_sym "OldMarks";
value sym_Open                 = decl_sym "Open";
value sym_Operator             = decl_sym "Operator";
value sym_Optional             = decl_sym "Optional";
value sym_Ordinary             = decl_sym "Ordinary";
value sym_Overline             = decl_sym "Overline";
value sym_PageNo               = decl_sym "PageNo";
value sym_Paragraph            = decl_sym "Paragraph";
value sym_ParIndent            = decl_sym "ParIndent";
value sym_ParFillSkip          = decl_sym "ParFillSkip";
value sym_ParParams            = decl_sym "ParParams";
value sym_ParShape             = decl_sym "ParShape";
value sym_ParSkip              = decl_sym "ParSkip";
value sym_PDF                  = decl_sym "PDF";
value sym_Phantom              = decl_sym "Phantom";
value sym_PostProcessLine      = decl_sym "PostProcessLine";
value sym_PostScript           = decl_sym "PostScript";
value sym_Preamble             = decl_sym "Preamble";
value sym_PreTolerance         = decl_sym "PreTolerance";
value sym_Punct                = decl_sym "Punct";
value sym_PutBox               = decl_sym "PutBox";
value sym_PutGalleyInVBox      = decl_sym "PutGalleyInVBox";
value sym_Register             = decl_sym "Register";
value sym_Relation             = decl_sym "Relation";
value sym_RelPenalty           = decl_sym "RelPenalty";
value sym_RGB                  = decl_sym "RGB";
value sym_Right                = decl_sym "Right";
value sym_RightHyphenMin       = decl_sym "RightHyphenMin";
value sym_RightSkip            = decl_sym "RightSkip";
value sym_RiverDemerits        = decl_sym "RiverDemerits";
value sym_RiverThreshold       = decl_sym "RiverThreshold";
value sym_RL                   = decl_sym "RL";
value sym_RLBox                = decl_sym "RLBox";
value sym_Root                 = decl_sym "Root";
value sym_Round                = decl_sym "Round";
value sym_Rule                 = decl_sym "Rule";
value sym_Scale                = decl_sym "Scale";
value sym_Script               = decl_sym "Script";
value sym_Script2              = decl_sym "ScriptScript";
value sym_ScriptLang           = decl_sym "ScriptLang";
value sym_ScriptSpace          = decl_sym "ScriptSpace";
value sym_ScriptSize           = decl_sym "ScriptSize";
value sym_Script2Size          = decl_sym "ScriptScriptSize";
value sym_Separator            = decl_sym "Separator";
value sym_Series               = decl_sym "Series";
value sym_SetAlpha             = decl_sym "SetAlpha";
value sym_SetBgColour          = decl_sym "SetBgColour";
value sym_SetColour            = decl_sym "SetColour";
value sym_SetFont              = decl_sym "SetFont";
value sym_SetLineCap           = decl_sym "SetLineCap";
value sym_SetLineJoin          = decl_sym "SetLineJoin";
value sym_SetLineWidth         = decl_sym "SetLineWidth";
value sym_SetMiterLimit        = decl_sym "SetMiterLimit";
value sym_Shape                = decl_sym "Shape";
value sym_ShipOut              = decl_sym "ShipOut";
value sym_Shrink               = decl_sym "Shrink";
value sym_ShrinkFactor         = decl_sym "ShrinkFactor";
value sym_ShrinkOrder          = decl_sym "ShrinkOrder";
value sym_SimpleBreaking       = decl_sym "SimpleBreaking";
value sym_Size                 = decl_sym "Size";
value sym_Skew                 = decl_sym "Skew";
value sym_SkewGlyph            = decl_sym "SkewGlyph";
value sym_Skyline              = decl_sym "Skyline";
value sym_Space                = decl_sym "Space";
value sym_SpaceFactor          = decl_sym "SpaceFactor";
value sym_SpaceParams          = decl_sym "SpaceParams";
value sym_SpaceSkip            = decl_sym "SpaceSkip";
value sym_Square               = decl_sym "Square";
value sym_Stretch              = decl_sym "Stretch";
value sym_StretchFactor        = decl_sym "StretchFactor";
value sym_StretchOrder         = decl_sym "StretchOrder";
value sym_Stroke               = decl_sym "Stroke";
value sym_SubScript            = decl_sym "SubScript";
value sym_SuperScript          = decl_sym "SuperScript";
value sym_Table                = decl_sym "Table";
value sym_TableEntry           = decl_sym "TableEntry";
value sym_TeX                  = decl_sym "TeX";
value sym_Text                 = decl_sym "Text";
value sym_TextSize             = decl_sym "TextSize";
value sym_ThickMathSkip        = decl_sym "ThickMathSkip";
value sym_ThinMathSkip         = decl_sym "ThinMathSkip";
value sym_Tolerance            = decl_sym "Tolerance";
value sym_Top                  = decl_sym "Top";
value sym_TopSkip              = decl_sym "TopSkip";
value sym_Underline            = decl_sym "Underline";
value sym_VBox                 = decl_sym "VBox";
value sym_VBoxSpread           = decl_sym "VBoxSpread";
value sym_VBoxTo               = decl_sym "VBoxTo";
value sym_Vert                 = decl_sym "Vert";
value sym_VictorianSpacing     = decl_sym "VictorianSpacing";
value sym_VInsert              = decl_sym "VInsert";
value sym_WidowPenalty         = decl_sym "WidowPenalty";
value sym_Width                = decl_sym "Width";
value sym_XSpaceSkip           = decl_sym "XspaceSkip";

(* wrappers for various types *******************************************************************************)

value decode_symbol name x = match !x with
[ Types.Symbol s -> s
| _              -> Types.runtime_error (name ^ ": symbol expected but got " ^ Types.type_name !x)
];

value decode_int name x = do
{
  let n = Machine.decode_num name x;

  if is_integer_num n then
    int_of_num n
  else
    Types.runtime_error (name ^ ": integer expected but got arbitrary number")
};

value decode_bool name x = match !x with
[ Types.Bool b -> b
| _            -> Types.runtime_error (name ^ ": bool expected but got " ^ Types.type_name !x)
];

value decode_char name x = match !x with
[ Types.Char c -> c
| _            -> Types.runtime_error (name ^ ": character expected but got " ^ Types.type_name !x)
];

value decode_option name decode x = match !x with
[ Types.Symbol s when s = sym_None -> None
| _                                -> Some (decode name x)
];

value decode_uc_string name x = do
{
  Array.of_list (Machine.decode_string name x)
};

value decode_tuple name x = match !x with
[ Types.Tuple z -> z
| _             -> Types.runtime_error (name ^ ": tuple expected but got " ^ Types.type_name !x)
];

(* dictionaries *)

value decode_dict name x = match !x with
[ Types.Dictionary d -> d
| _ -> Types.runtime_error (name ^ ": dictionary expected but got " ^ Types.type_name !x)
];

value lookup decode dict key = do
{
  try
    Some (decode (SymbolMap.find key dict))
  with
  [ Not_found -> None ]
};

value lookup_string name dict key = lookup (decode_uc_string    name) dict key;
value lookup_bool   name dict key = lookup (decode_bool         name) dict key;
value lookup_int    name dict key = lookup (decode_int          name) dict key;
value lookup_num    name dict key = lookup (Machine.decode_num  name) dict key;
value lookup_symbol name dict key = lookup (decode_symbol       name) dict key;
value lookup_dict   name dict key = lookup (decode_dict         name) dict key;
value lookup_tuple  name dict key = lookup (decode_tuple        name) dict key;
value lookup_list   name dict key = lookup (Machine.decode_list name) dict key;

(* generic unwrapper for opaque types *)

value decode_opaque type_name unwrapper name x = match !x with
[ Types.Opaque y -> try unwrapper y with
                    [ Opaque.Type_error -> Types.runtime_error (name ^ ": " ^ type_name ^ " expected but got " ^ Types.type_name !x) ]
| _ -> Types.runtime_error (name ^ ": " ^ type_name ^ " expected but got " ^ Types.type_name !x)
];

(* locations *)

value encode_location (file, line, col) =
  Types.Tuple
    [|ref (Machine.uc_string_to_char_list (UString.uc_string_of_ascii file));
      ref (Types.Number (num_of_int line));
      ref (Types.Number (num_of_int col))|];

value decode_location name loc = match !loc with
[ Types.Tuple [|file; line; col|] -> do
  {
    (UString.to_string (Machine.decode_string name file),
     decode_int name line,
     decode_int name col)
  }
| _ -> Types.runtime_error (name ^ ": invalid location")
];

(* index position *)

value encode_index_position s = match s with
[ Box.LeftIndex  -> Types.Symbol sym_Left
| Box.RightIndex -> Types.Symbol sym_Right
| Box.VertIndex  -> Types.Symbol sym_Vert
];

value decode_index_position name s = match !s with
[ Types.Symbol s -> do
  {
    if s = sym_Left then
      Box.LeftIndex
    else if s = sym_Right then
      Box.RightIndex
    else if s = sym_Vert then
      Box.VertIndex
    else
      Types.runtime_error (name ^ ": invalid math style")
  }
| _ -> Types.runtime_error (name ^ ": invalid math style")
];

(* math-code *)

value encode_math_code c = match c with
[ Box.NoMath          -> Types.Symbol sym_NoMath
| Box.Ordinary        -> Types.Symbol sym_Ordinary
| Box.BinOp           -> Types.Symbol sym_BinOp
| Box.Relation        -> Types.Symbol sym_Relation
| Box.Operator        -> Types.Symbol sym_Operator
| Box.Punct           -> Types.Symbol sym_Punct
| Box.Open            -> Types.Symbol sym_Open
| Box.Close           -> Types.Symbol sym_Close
| Box.Inner           -> Types.Symbol sym_Inner
| Box.SubScript       -> Types.Symbol sym_SubScript
| Box.SuperScript     -> Types.Symbol sym_SuperScript
| Box.IndexPosition p -> Types.Tuple [|ref (Types.Symbol sym_IndexPosition);
                                       ref (encode_index_position p)|]
];

value decode_math_code name c = match !c with
[ Types.Symbol s -> do
  {
    if s = sym_NoMath then
      Box.NoMath
    else if s = sym_Ordinary then
      Box.Ordinary
    else if s = sym_BinOp then
      Box.BinOp
    else if s = sym_Relation then
      Box.Relation
    else if s = sym_Operator then
      Box.Operator
    else if s = sym_Punct then
      Box.Punct
    else if s = sym_Open then
      Box.Open
    else if s = sym_Close then
      Box.Close
    else if s = sym_Inner then
      Box.Inner
    else if s = sym_SubScript then
      Box.SubScript
    else if s = sym_SuperScript then
      Box.SuperScript
    else
      Types.runtime_error (name ^ ": invalid math code")
  }
| Types.Tuple [|a; b|] -> match !a with
      [ Types.Symbol s -> do
        {
          if s = sym_IndexPosition then
            Box.IndexPosition (decode_index_position name b)
          else
            Types.runtime_error (name ^ ": invalid math code")
        }
      | _ -> Types.runtime_error (name ^ ": invalid math code")
      ]
| _ -> Types.runtime_error (name ^ ": invalid math code")
];

(* math-style *)

value encode_math_style s = match s with
[ MathLayout.Display        -> Types.Symbol sym_Display
| MathLayout.CrampedDisplay -> Types.Symbol sym_CrampedDisplay
| MathLayout.Text           -> Types.Symbol sym_Text
| MathLayout.CrampedText    -> Types.Symbol sym_CrampedText
| MathLayout.Script         -> Types.Symbol sym_Script
| MathLayout.CrampedScript  -> Types.Symbol sym_CrampedScript
| MathLayout.Script2        -> Types.Symbol sym_Script2
| MathLayout.CrampedScript2 -> Types.Symbol sym_CrampedScript2
];

value decode_math_style name s = match !s with
[ Types.Symbol s -> do
  {
    if s = sym_Display then
      MathLayout.Display
    else if s = sym_CrampedDisplay then
      MathLayout.CrampedDisplay
    else if s = sym_Text then
      MathLayout.Text
    else if s = sym_CrampedText then
      MathLayout.CrampedText
    else if s = sym_Script then
      MathLayout.Script
    else if s = sym_CrampedScript then
      MathLayout.CrampedScript
    else if s = sym_Script2 then
      MathLayout.Script2
    else if s = sym_CrampedScript2 then
      MathLayout.CrampedScript2
    else
      Types.runtime_error (name ^ ": invalid math style")
  }
| _ -> Types.runtime_error (name ^ ": invalid math style")
];

(* modes *)

value encode_mode m = match m with
[ `Preamble  -> Types.Symbol sym_Preamble
| `Galley    -> Types.Symbol sym_Galley
| `Paragraph -> Types.Symbol sym_Paragraph
| `Math      -> Types.Symbol sym_Math
| `HBox      -> Types.Symbol sym_HBox
| `LRBox     -> Types.Symbol sym_LRBox
| `RLBox     -> Types.Symbol sym_RLBox
| `VBox      -> Types.Symbol sym_VBox
| `Table     -> Types.Symbol sym_Table
];

value decode_mode name m = match !m with
[ Types.Symbol s -> do
  {
    if s = sym_Preamble then
      `Preamble
    else if s = sym_Galley then
      `Galley
    else if s = sym_Paragraph then
      `Paragraph
    else if s = sym_Math then
      `Math
    else if s = sym_HBox then
      `HBox
    else if s = sym_LRBox then
      `LRBox
    else if s = sym_RLBox then
      `RLBox
    else if s = sym_VBox then
      `VBox
    else if s = sym_Table then
      `Table
    else
      Types.runtime_error (name ^ ": invalid mode")
  }
| _ -> Types.runtime_error (name ^ ": invalid mode")
];

value encode_hbox_dir d = match d with
[ `LR      -> Types.Symbol sym_LR
| `RL      -> Types.Symbol sym_RL
| `Default -> Types.Symbol sym_Default
];

value decode_hbox_dir name d = match !d with
[ Types.Symbol s -> do
  {
    if s = sym_LR then
      `LR
    else if s = sym_RL then
      `RL
    else if s = sym_Default then
      `Default
    else
      Types.runtime_error (name ^ ": invalid direction")
  }
| _ -> Types.runtime_error (name ^ ": invalid direction")
];

(* colours *)

value encode_colour col = match col with
[ Graphic.Grey x       -> Types.Tuple [| ref (Types.Symbol sym_Grey);
                                         ref (Types.Number x) |]
| Graphic.RGB r g b    -> Types.Tuple [| ref (Types.Symbol sym_RGB);
                                         ref (Types.Number r);
                                         ref (Types.Number g);
                                         ref (Types.Number b) |]
| Graphic.CMYK c m y k -> Types.Tuple [| ref (Types.Symbol sym_CMYK);
                                         ref (Types.Number c);
                                         ref (Types.Number m);
                                         ref (Types.Number y);
                                         ref (Types.Number k) |]
];

value decode_colour name col = match !col with
[ Types.Tuple xs -> match !(xs.(0)) with
    [ Types.Symbol s -> do
      {
        if s = sym_Grey then match xs with
          [ [|_; x|] -> do
            {
              Graphic.Grey (Machine.decode_num name x)
            }
          | _ -> Types.runtime_error (name ^ ": colour expected")
          ]
        else if s = sym_RGB then match xs with
          [ [|_; r; g; b|] -> do
            {
              Graphic.RGB
                (Machine.decode_num name r)
                (Machine.decode_num name g)
                (Machine.decode_num name b)
            }
          | _ -> Types.runtime_error (name ^ ": colour expected")
          ]
        else if s = sym_CMYK then match xs with
          [ [|_; c; m; y; k|] -> do
            {
              Graphic.CMYK
                (Machine.decode_num name c)
                (Machine.decode_num name m)
                (Machine.decode_num name y)
                (Machine.decode_num name k)
            }
          | _ -> Types.runtime_error (name ^ ": colour expected")
          ]
        else
          Types.runtime_error (name ^ ": colour expected")
      }
    | _ -> Types.runtime_error (name ^ ": colour expected")
    ]
| _ -> Types.runtime_error (name ^ ": colour expected")
];

