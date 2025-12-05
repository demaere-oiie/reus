type re =
  | Trm of string
  | Alt of re * re
  | Seq of re * re
  | Leq of re

let split xs =
  let m = (List.length xs) / 2 in
  (List.take m xs, List.drop m xs)

let rec tfold f xs =
  match xs with
  | []      -> raise (Failure "nullary")
  | [x]     -> x
  | _ :: _  -> let (p,s) = split xs in
               f (tfold f p) (tfold f s)

let trm s = Trm s
let alts  = tfold (fun x y -> Alt (x,y))
let seqs  = tfold (fun x y -> Seq (x,y))
let leq x = Leq x

let rec ways n r =
  match r with
  | Trm _     -> if n = 1 then 1 else 0
  | Alt (x,y) -> (ways n x) + (ways n y)
  | Seq (x,y) -> sum n (fun i -> (ways i x) * (ways (n-i) y)) 0
  | Leq x     -> sum n (fun i -> ways i x) 0
and sum n f a = if n < 0 then a else sum (n-1) f (a + f n)

let rec kth n k r =
  match r with
  | Trm s     -> if n = 1 then s else "*barf*"
  | Alt (x,y) -> let lefts = ways n x in
                 if k < lefts then (kth n  k        x)
                              else (kth n (k-lefts) y)
  | Seq (x,y) -> kth_seq n k (x,y) 0
  | Leq x     -> kth_leq n k x 0 
and kth_seq n k (x,y) m =
  let more = (ways m x)*(ways (n-m) y) in
  if k < more then let d = ways m x in
                   (kth m (k mod d) x) ^ " " ^ (kth (n-m) (k / d) y)
              else (kth_seq n (k-more) (x,y) (m+1))
and kth_leq n k x m =
  let more = (ways m x) in
  if k < more then kth m k x
              else kth_leq n (k-more) x (m+1)

let pick n r =
  let k = Random.int @@ ways n r in
  kth n k r
