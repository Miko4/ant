
open XNum;

type compare_result = [ Eq | Lt | Gt ];

type compare 'a = 'a -> 'a -> compare_result;

type lin_form 'a = private
{
  const   : num;
  terms   : list (num * 'a);
  compare : compare 'a
};

value lin_zero          : compare 'a -> lin_form 'a;
value of_num            : compare 'a -> num -> lin_form 'a;
value of_unknown        : compare 'a -> 'a -> lin_form 'a;
value of_scaled_unknown : compare 'a -> num -> 'a -> lin_form 'a;
value of_terms          : compare 'a -> list (num * 'a) -> lin_form 'a;
value is_constant       : lin_form 'a -> bool;
value coefficient       : lin_form 'a -> 'a -> num;
value remove_first_term : lin_form 'a -> lin_form 'a;
value add               : lin_form 'a -> lin_form 'a -> lin_form 'a;
value add_const         : lin_form 'a -> num -> lin_form 'a;
value add_unknown       : lin_form 'a -> num -> 'a -> lin_form 'a;
value sub               : lin_form 'a -> lin_form 'a -> lin_form 'a;
value lin_comb          : num -> lin_form 'a -> num -> lin_form 'a -> lin_form 'a;
value scale             : num -> lin_form 'a -> lin_form 'a;
value map               : compare 'b -> ('a -> 'b) -> lin_form 'a -> lin_form 'b;

