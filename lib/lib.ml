type re =
  | Trm of string
  | Alt of re * re
  | Seq of re * re
  | Leq of re
  | Ref of string * (int array)

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
let ref s = Ref (s, Array.make 32 (-1))

let rec ways n r v =
  match r with
  | Trm _     -> if n = 1 then 1 else 0
  | Alt (x,y) -> (ways n x v) + (ways n y v)
  | Seq (x,y) -> sum n (fun i -> (ways i x v) * (ways (n-i) y v)) 0
  | Leq x     -> sum n (fun i -> ways (n-i) x v) 0
  | Ref (s,a) -> let c = Array.get a n in
                 if c <> -1 then c else
                 (let u = ways n (List.assoc s v) v in
                  Array.set a n u; u)
and sum n f a = if n < 1 then a else sum (n-1) f (a + f n)

let rec kth n k r v =
  match r with
  | Trm s     -> if n = 1 then s else "*barf*"
  | Alt (x,y) -> let lefts = ways n x v in
                 if k < lefts then (kth n  k        x v)
                              else (kth n (k-lefts) y v)
  | Seq (x,y) -> kth_seq n k (x,y) v 0
  | Leq x     -> kth_leq n k x v 0
  | Ref (s,_) -> kth n k (List.assoc s v) v
and kth_seq n k (x,y) v m =
  let more = (ways m x v)*(ways (n-m) y v) in
  if k < more then let d = ways m x v in
                   (kth m (k mod d) x v) ^ " " ^ (kth (n-m) (k / d) y v)
              else (kth_seq n (k-more) (x,y) v (m+1))
and kth_leq n k x v m =
  let more = (ways m x v) in
  if k < more then kth m k x v
              else kth_leq n (k-more) x v (m+1)

let pick n v r =
  let k = Random.int @@ ways n r v in
  kth n k r v
