
open Types;

type charmap 'a;

value create : 'a -> charmap 'a;
value build  : array (array 'a) -> charmap 'a;
value lookup : charmap 'a -> uc_char -> 'a;
value set    : charmap 'a -> uc_char -> 'a -> unit;
value copy   : charmap 'a -> charmap 'a;

