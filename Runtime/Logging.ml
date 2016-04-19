
open XNum;
open Unicode;
open Unicode.Types;

value log_file = ref None;

value log_open file = do
{
  !log_file := Some (open_out file)
};

value log_string str = do
{
  print_string str;
  flush stdout;

  match !log_file with
  [ None    -> ()
  | Some oc -> output_string oc str
  ]
};

value log_uc_list   str = log_string (UString.to_string str);
value log_uc_string str = log_string (UString.to_string (Array.to_list str));

value log_int x = log_string (string_of_int x);
value log_num x = log_string (string_of_float (float_of_num x));

value log_info (file, line, col) msg = do
{
  if file <> "" then do
  {
    log_string "\nIn file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  }
  else do
  {
    log_string "\n";
    log_string msg
  }
};

value log_warn (file, line, col) msg = do
{
  if file <> "" then do
  {
    log_string "\nWarning, in file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  }
  else do
  {
    log_string "\nWarning: ";
    log_string msg
  }
};

value log_error (file, line, col) msg = do
{
  if file <> "" then do
  {
    log_string "\nError, in file ";
    log_string file;
    log_string ", line ";
    log_int line;
    log_string ", column ";
    log_int col;
    log_string ":\n";
    log_string msg
  }
  else do
  {
    log_string "\nError: ";
    log_string msg
  }
};

