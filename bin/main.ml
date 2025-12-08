open Reus.Lib

let () = 
  let () = Random.self_init() in
  let digits = List.map trm ["0";"1";"2";"3"] in
  let alphas = List.map trm ["a";"b";"c"] in
  print_endline @@ pick 8 [] @@ leq @@ seqs [
    alts [Seq(seqs digits, trm "a"); Seq(trm "0", seqs alphas)];
    trm ":";
    alts digits;
    alts alphas];
  let xpr = seqs [ alts [ alts digits; ref "sum" ];
                   trm "*";
                   alts [ alts digits; ref "sum" ] ] in
  let sum = seqs [ ref "xpr"; trm "+"; ref "xpr" ] in
  print_endline @@ pick 20 [("xpr",xpr);("sum",sum)] @@ leq @@ xpr
