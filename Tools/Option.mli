
value maybe :       'b -> ('a -> 'b) -> option 'a -> 'b;
value is_some :     option 'a -> bool;
value is_none :     option 'a -> bool;
value from_some :   option 'a -> 'a;
value from_option : 'a -> option 'a -> 'a;
value compare :     'a -> option 'a -> bool;
value compareq :    'a -> option 'a -> bool;
value to_list :     option 'a -> list 'a;
value from_list :   list 'a -> option 'a;
value concat :      list (option 'a) -> list 'a;
value map :         ('a -> option 'b) -> list 'a -> list 'b;
