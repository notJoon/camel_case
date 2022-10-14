let rec factorial (n: int) : int = 
  if n = 0 then 1
  else n * factorial (n - 1)

let rec pow (x: int) (y: int) : int =
  if y = 0 then 1
  else x * pow x (y-1)

let rec even n =
  n = 0 || odd (n-1)

and odd n =
  n <> 0 && even (n-1);;

(** annonymous function.
   e.g. same as [let inc x = x + 1] *)
let inc = fun x -> x + 1 