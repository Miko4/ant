
include List;

value rec take n l = do
{
  if n = 0 then
    []
  else match l with
  [ []      -> []
  | [x::xs] -> [x :: take (n-1) xs]
  ]
};

value rec drop n l = do
{
  if n = 0 then
    l
  else match l with
  [ []      -> []
  | [_::xs] -> drop (n-1) xs
  ]
};

value rec last l = match l with
[ []      -> failwith "XList.last: list empty!"
| [x]     -> x
| [_::xs] -> last xs
];

value rec split_at n l = do
{
  if n = 0 then
    ([], l)
  else match l with
  [ []      -> ([], [])
  | [x::xs] -> let (a, b) = split_at (n-1) x in
               ([x :: a], b)
  ]
};

value rec take_while p l = match l with
[ []      -> []
| [x::xs] -> if p x then
               [x :: take_while p xs]
             else
               []
];

value rec drop_while p l = match l with
[ []      -> []
| [x::xs] -> if p x then
               drop_while p xs
             else
               l
];

value rec span p l = match l with
[ []      -> ([], [])
| [x::xs] -> if p x then
               let (a, b) = span p xs in
               ([x :: a], b)
             else
               ([], l)
];

value rec break p l = match l with
[ []      -> ([], [])
| [x::xs] -> if p x then
               ([], l)
             else
               let (a, b) = span p xs in
               ([x :: a], b)
];

value rec repeat n x = do
{
  iter n

  where rec iter n =
    if n <= 0 then
      []
    else
      [x :: iter (n-1)]
};

value rev_to_array x = match x with
[ []      -> [| |]
| [y::ys] -> do
  {
    let len = List.length x;
    let arr = Array.make len y;

    iter (len - 2) ys

    where rec iter i x = match x with
    [ []      -> arr
    | [y::ys] -> do
      {
        arr.(i) := y;
        iter (i-1) ys
      }
    ]
  }
];

value rec append_sub_array arr from_pos to_pos tail = do
{
  iter tail to_pos

  where rec iter tail to_pos = do
  {
    if from_pos > to_pos then
      tail
    else
      iter [arr.(to_pos) :: tail] (to_pos-1)
  }
};

value rec from_sub_array arr from_pos to_pos = append_sub_array arr from_pos to_pos [];

