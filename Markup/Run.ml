
open Runtime;
open Engine;

(* hooks for loaded modules *)

value hook_parse_state = ref [];
value hook_initialise  = ref [];

value register_parse_state_hook h = do
{
  !hook_parse_state := [h :: !hook_parse_state]
};

value register_init_hook h = do
{
  !hook_initialise := [h :: !hook_initialise]
};

value call_parse_state_hooks ps = do
{
  List.iter (fun h -> h ps) !hook_parse_state
};

value call_init_hooks () = do
{
  List.iter (fun h -> h ()) !hook_initialise
};

(* routines to start ant *)

value initialise job = do
{
  Mode.init_source_specials job;
  Fonts.initialise_font_table ();
  call_init_hooks ();
};

value parse_document ps = do
{
  Primitives.initialise ps;
  call_parse_state_hooks ps;

  ParseState.run_parser ps `Preamble
};

value parse_file job name = do
{
  let ps = ParseState.create job;

  UCStream.include_file ps.ParseState.input_stream name;

  (parse_document ps, ps)
};

value parse_string job str = do
{
  let ps = ParseState.create job;

  UCStream.insert_string ps.ParseState.input_stream str;

  (parse_document ps, ps)
};

