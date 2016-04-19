
open XNum;
open Runtime;
open Unicode.Types;
open Logging;
open Dim;
open Markup;
open ParseState;

value warn_unknown loc sequence = do
{
  log_warn loc "command \"";
  log_uc_list sequence;
  log_string " unknown!"
};

value add_zero_skip ps = do
{
  add_node ps (`Glue (location ps, fun _ -> dim_zero, fun _ -> dim_zero, False, False))
};

value add_discretionary ps penalty pre post no = do
{
  let loc = location ps;

  add_node
    ps
    (`Break
       (loc, penalty, False,
        (List.map (fun c -> `Letter (loc, c)) pre),
        (List.map (fun c -> `Letter (loc, c)) post),
        (List.map (fun c -> `Letter (loc, c)) no)))
};

value add_break ps = do
{
  add_discretionary ps None [] [] []
};

value double_consonant ps char = do
{
  let c = UCStream.next_char ps.input_stream;

  if c = char then
    add_discretionary ps None [c; c; 45] [] [c]
  else
    warn_unknown (location ps) [char; c]
};

(*
  Commands for german typsetting:

    "a   ->   ä
    "A   ->   Ä
    "e   ->   ë
    "E   ->   Ë
    "i   ->   ï
    "I   ->   Ï
    "o   ->   ö
    "O   ->   Ö
    "s   ->   ß
    "S   ->   SS
    "u   ->   ü
    "U   ->   Ü
    "z   ->   ß
    "Z   ->   SZ
    "`   ->   double quotes left on the baseline
    "'   ->   double quotes right
    "<   ->   «
    ">   ->   »
    "-   ->   
    ""   ->   
    "|   ->   
    "~   ->   
    "=   ->   
    "ck  ->   ck  which is hyphenated as k-k
    "CK  ->   CK  which is hyphenated as K-K
    "ff  ->   ff  which is hyphenated as ff-f
    "FF  ->   FF  which is hyphenated as FF-F
    "ll  ->   ll  which is hyphenated as ll-l
    "LL  ->   LL  which is hyphenated as LL-L
    "mm  ->   mm  which is hyphenated as mm-m
    "MM  ->   MM  which is hyphenated as MM-M
    "nn  ->   nn  which is hyphenated as nn-n
    "NN  ->   NN  which is hyphenated as NN-N
    "pp  ->   pp  which is hyphenated as pp-p
    "PP  ->   PP  which is hyphenated as PP-P
    "rr  ->   rr  which is hyphenated as rr-r
    "RR  ->   RR  which is hyphenated as RR-R
    "tt  ->   tt  which is hyphenated as tt-t
    "TT  ->   TT  which is hyphenated as TT-T

*)

value german_execute ps = do
{
  let loc = location ps;

  match UCStream.pop ps.input_stream with
  [  97 -> add_node ps (`Letter (loc, 228))    (* ""a *)
  |  65 -> add_node ps (`Letter (loc, 196))    (* ""A *)
  | 101 -> add_node ps (`Letter (loc, 235))    (* ""e *)
  |  69 -> add_node ps (`Letter (loc, 203))    (* ""E *)
  | 105 -> add_node ps (`Letter (loc, 239))    (* ""i *)
  |  73 -> add_node ps (`Letter (loc, 207))    (* ""I *)
  | 111 -> add_node ps (`Letter (loc, 246))    (* ""o *)
  |  79 -> add_node ps (`Letter (loc, 214))    (* ""O *)
  | 117 -> add_node ps (`Letter (loc, 252))    (* ""u *)
  |  85 -> add_node ps (`Letter (loc, 220))    (* ""U *)
  | 115 -> add_node ps (`Letter (loc, 223))    (* ""s *)
  |  83 -> do                                  (* ""S *)
           {
             add_node ps (`Letter (loc, 83));
             add_node ps (`Letter (loc, 83))
           }
  | 122 -> add_node ps (`Letter (loc, 223))    (* ""z *)
  |  90 -> do                                  (* ""Z *)
           {
             add_node ps (`Letter (loc, 83));
             add_node ps (`Letter (loc, 90))
           }
  |  96 -> add_node ps (`Letter (loc, 0x201e)) (* ""` *)
  |  39 -> add_node ps (`Letter (loc, 0x201d)) (* ""' *)
  |  60 -> add_node ps (`Letter (loc, 0x2039)) (* ""< *)
  |  62 -> add_node ps (`Letter (loc, 0x203a)) (* ""> *)
  |  45 -> do                                  (* ""- *)
           {
             add_discretionary ps None [45] [] [];
             add_zero_skip ps
           }
  | 124 -> do                                  (* ""| *)
           {
             add_discretionary ps None [45] [] [];
             add_zero_skip ps
           }
  |  34 -> add_break ps                        (* "" *)
  |  61 -> do                                  (* ""= *)
           {
             add_node ps (`Letter (loc, 45));
             add_break ps
           }
  | 126 -> add_node ps (`HBox (loc, [(`Letter (loc, 45))])) (* ""~ *)
  |  99 -> match UCStream.next_char ps.input_stream with    (* ""ck *)
           [ 107 -> add_discretionary ps None [107; 45] [] [99]
           | _   -> warn_unknown loc [99; UCStream.next_char ps.input_stream]
           ]
  |  67 -> match UCStream.next_char ps.input_stream with    (* ""CK *)
           [ 75 -> add_discretionary ps None [75; 45] [] [67]
           | _  -> warn_unknown loc [99; UCStream.next_char ps.input_stream]
           ]
  | 102  -> double_consonant ps 102       (* ""ff *)
  |  70  -> double_consonant ps  70       (* ""FF *)
  | 108  -> double_consonant ps 108       (* ""ll *)
  |  76  -> double_consonant ps  76       (* ""LL *)
  | 109  -> double_consonant ps 109       (* ""mm *)
  |  77  -> double_consonant ps  77       (* ""MM *)
  | 110  -> double_consonant ps 110       (* ""nn *)
  |  78  -> double_consonant ps  78       (* ""NN *)
  | 112  -> double_consonant ps 112       (* ""pp *)
  |  80  -> double_consonant ps  80       (* ""PP *)
  | 114  -> double_consonant ps 114       (* ""rr *)
  |  82  -> double_consonant ps  82       (* ""RR *)
  | 116  -> double_consonant ps 116       (* ""tt *)
  |  84  -> double_consonant ps  84       (* ""TT *)
  | c    -> warn_unknown loc [c]
  ];
};

value german_expand tok stream = do
{
  match UCStream.pop stream with
  [  97 -> [228]       (* ""a *)
  |  65 -> [196]       (* ""A *)
  | 101 -> [235]       (* ""e *)
  |  69 -> [203]       (* ""E *)
  | 105 -> [239]       (* ""i *)
  |  73 -> [207]       (* ""I *)
  | 111 -> [246]       (* ""o *)
  |  79 -> [214]       (* ""O *)
  | 117 -> [252]       (* ""u *)
  |  85 -> [220]       (* ""U *)
  | 115 -> [223]       (* ""s *)
  |  83 -> [83; 83]    (* ""S *)
  | 122 -> [223]       (* ""z *)
  |  90 -> [83; 90]    (* ""Z *)
  |  96 -> [0x201e]    (* ""` *)
  |  39 -> [0x201d]    (* ""' *)
  |  60 -> [0x2039]    (* ""< *)
  |  62 -> [0x203a]    (* ""> *)
  |  45 -> [45]        (* ""- *)
  | 124 -> []          (* ""| *)
  |  34 -> []          (* "" *)
  |  61 -> [45]        (* ""= *)
  | 126 -> [45]        (* ""~ *)
  |  99 -> [99]        (* ""ck *)
  |  67 -> [67]        (* ""CK *)
  | 102 -> [102]       (* ""ff *)
  |  70 -> [ 70]       (* ""FF *)
  | 108 -> [108]       (* ""ll *)
  |  76 -> [ 76]       (* ""LL *)
  | 109 -> [109]       (* ""mm *)
  |  77 -> [ 77]       (* ""MM *)
  | 110 -> [110]       (* ""nn *)
  |  78 -> [ 78]       (* ""NN *)
  | 112 -> [112]       (* ""pp *)
  |  80 -> [ 80]       (* ""PP *)
  | 114 -> [114]       (* ""rr *)
  |  82 -> [ 82]       (* ""RR *)
  | 116 -> [116]       (* ""tt *)
  |  84 -> [ 84]       (* ""TT *)
  | c   -> do { warn_unknown (UCStream.location stream) [c]; [c] }
  ];
};

value init_commands ps = do
{
  ParseState.define_pattern ps [34]
    {
      ParseState.execute = german_execute;
      ParseState.expand  = (fun ps tok ->
                              german_expand tok ps.input_stream @ Macro.expand ps)
    }
};

do
{
  Run.register_parse_state_hook init_commands
};

(* vim:set fenc=utf8: *)
