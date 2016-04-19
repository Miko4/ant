open Camlp4;

module Id = struct
  value name    = "pa_extensions";
  value version = "";
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "~-"
    [ [ "!"; x = expr -> <:expr< $x$.val >> ] ];
  END;
end;

let module M = Register.OCamlSyntaxExtension Id Make in ();

