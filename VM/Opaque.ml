
exception Type_error;

type opaque 'a =
{
  data      : !'b . 'b;
  type_info : type_info 'a
}
and type_info 'a =
{
  name    : string;
  apply   : opaque 'a -> 'a -> 'a;
  compare : opaque 'a -> opaque 'a -> bool;
  unify   : opaque 'a -> opaque 'a -> bool
};

value type_name x = x.type_info.name;

value same_type x y = (x.type_info == y.type_info);
value apply     x y = x.type_info.apply   x y;
value compare   x y = x.type_info.compare x y;
value unify     x y = x.type_info.unify   x y;

value declare_type name apply cmp unify = do
{
  let rec wrap x =
    {
      data      = Obj.magic x;
      type_info = ti
    }
  and unwrap x = do
  {
    if x.type_info == ti then
      x.data
    else
      raise Type_error
  }
  and ti =
    {
      name    = name;
      apply   = (fun x y -> apply (unwrap x) y);
      compare = (fun x y -> cmp   (unwrap x) (unwrap y));
      unify   = (fun x y -> unify (unwrap x) (unwrap y))
    };

  (wrap, unwrap)
};

