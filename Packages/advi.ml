
open XNum;
open Runtime;
open Unicode;
open Unicode.Types;
open Logging;
open Markup;
open ParseState;

value advi_special ps cmd = do
{
  add_node ps (`CommandBox (location ps, `Special (`DVI_Special ("advi: " ^ cmd))))
};

(* |\adviwait [time]| *)

value advi_wait ps = do
{
  match Parser.read_optional ps.input_stream [] with
  [ [] -> advi_special ps "pause"
  | x  -> advi_special ps ("wait sec=" ^ UString.to_string x)
  ]
};

(* |\advibeginrecording * {tag}| *)

value advi_begin_recording ps = do
{
  let play = if Parser.read_bool ps.input_stream then
               " play"
             else
               "";
  let tag  = Macro.expand_string ps (Parser.read_argument ps.input_stream);

  advi_special ps ("proc=" ^ (UString.to_string tag) ^ " record=start" ^ play)
};

(* |\adviendrecording| *)

value advi_end_recording ps = do
{
  advi_special ps "proc record=end"
};

(* |\adviplay {tag}| *)

value advi_play ps = do
{
  let tag = Macro.expand_string ps (Parser.read_argument ps.input_stream);

  advi_special ps ("proc=" ^ (UString.to_string tag) ^ " play")
};

(* |\adviembed {name} {mode} {width} {height} {cmd}| *)

value advi_embed ps = do
{
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let mode   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let width  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let height = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let cmd    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("embed name=\"" ^ name ^ "\" mode=" ^ mode ^
                   " width=" ^ width ^ " height=" ^ height ^ " command=\"" ^ cmd ^ "\"")
};

(* |\advikillembed {name} {signal}| *)

value advi_kill_embed ps = do
{
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let signal = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("kill name=\"" ^ name ^ "\" signal=\"" ^ signal ^ "\"")
};

(* |\advisetbg {name} {signal}| *)

value advi_set_bg ps = do
{
  let colour = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let image  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let alpha  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let blend  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let fit    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("setbg " ^ colour ^ " " ^ image ^ " " ^ alpha ^ " " ^ blend ^ " " ^ fit)
};

value advi_eps_transparent ps = do
{
  advi_special ps "epstransparent push true"
};

value advi_eps_white ps = do
{
  advi_special ps "epstransparent push false"
};

value advi_reset_eps_transparent ps = do
{
  advi_special ps "epstransparent pop"
};

value advi_set_alpha ps = do
{
  let alpha = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("alpha push " ^ alpha)
};

value advi_reset_alpha ps = do
{
  advi_special ps "alpha pop"
};

value advi_set_blend ps = do
{
  let blend = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("blend push " ^ blend)
};

value advi_reset_blend ps = do
{
  advi_special ps "blend pop"
};

value advi_transition ps = do
{
  let mode    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let from    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let steps   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let start_x = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let start_y = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let stop_x  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let stop_y  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let genpath = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("trans " ^ mode ^ " " ^ from ^ " " ^ steps ^ " " ^
                   start_x ^ " " ^ start_y ^ " " ^ stop_x ^ " " ^ stop_y ^ " " ^ genpath)
};

value advi_trans_box_save ps = do
{
  let width  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let height = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let depth  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("transbox save width=" ^ width ^ " height=" ^ height ^ " depth=" ^ depth)
};

value advi_trans_box_go ps = do
{
  let mode    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let from    = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let steps   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let start_x = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let start_y = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let stop_x  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let stop_y  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let genpath = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("transbox go " ^ mode ^ " " ^ from ^ " " ^ steps ^ " " ^
                   start_x ^ " " ^ start_y ^ " " ^ stop_x ^ " " ^ stop_y ^ " " ^ genpath)
};

value advi_move_to ps = do
{
  advi_special ps "moveto"
};

value advi_begin_anchor ps = do
{
  let mode = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let tag  = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  if mode = "over" then
    advi_special ps ("html:<a advi=\"" ^ tag ^ "\">")
  else if mode = "click" then
    advi_special ps ("html:<a hdvi=\"" ^ tag ^ "\">")
  else
    log_warn (location ps) ("Incorrect anchor mode " ^ mode ^ ".")
};

value advi_end_anchor ps = do
{
  advi_special ps "html:</a>"
};

value advi_edit ps = do
{
  let comm   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let name   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let line   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let file   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let unit   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let x      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let y      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let w      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let h      = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let move   = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));
  let resize = UString.to_string (Macro.expand_string ps (Parser.read_argument ps.input_stream));

  advi_special ps ("edit comm=\"" ^ comm ^ "\" name=\"" ^ name ^ "\" line=" ^ line ^
                   " file=" ^ file ^ " unit=" ^ unit ^
                   " x=" ^ x ^ " y=" ^ y ^ " w=" ^ w ^ " h=" ^ h ^
                   " move=" ^ move ^ " resize=" ^ resize)
};

value init_commands ps = do
{
  let def_cmd name cmd = do
  {
    define_command ps (UString.of_ascii name) { execute = cmd; expand = Macro.noexpand }
  };

  def_cmd "\\adviwait"                advi_wait;
  def_cmd "\\advibeginrecording"      advi_begin_recording;
  def_cmd "\\adviendrecording"        advi_end_recording;
  def_cmd "\\adviplay"                advi_play;
  def_cmd "\\adviembed"               advi_embed;
  def_cmd "\\advikillembed"           advi_kill_embed;
  def_cmd "\\advisetbg"               advi_set_bg;
  def_cmd "\\adviepstransparent"      advi_eps_transparent;
  def_cmd "\\adviepswhite"            advi_eps_white;
  def_cmd "\\adviresetepstransparent" advi_reset_eps_transparent;
  def_cmd "\\advisetalpha"            advi_set_alpha;
  def_cmd "\\adviresetalpha"          advi_reset_alpha;
  def_cmd "\\advisetblend"            advi_set_blend;
  def_cmd "\\adviresetblend"          advi_reset_blend;
  def_cmd "\\advitransition"          advi_transition;
  def_cmd "\\advitransboxsave"        advi_trans_box_save;
  def_cmd "\\advitransboxgo"          advi_trans_box_go;
  def_cmd "\\advimoveto"              advi_move_to;
  def_cmd "\\advibeginanchor"         advi_begin_anchor;
  def_cmd "\\adviendanchor"           advi_end_anchor;
  def_cmd "\\adviedit"                advi_edit;
};

do
{
  Run.register_parse_state_hook init_commands
};

