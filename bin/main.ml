open Reus.Lib

let () =
  let () = Random.self_init() in
  let digits = List.map trm ["0";"1";"2";"3"] in
  let alphas = List.map trm ["a";"b";"c"] in
  print_endline @@ pick 8 @@ leq @@ seqs [
    alts [Seq(seqs digits, trm "a"); Seq(trm "0", seqs alphas)];
    trm ":";
    alts digits;
    alts alphas];
  let lf  = alts [
alts alphas; alts alphas; alts alphas; alts alphas;
alts alphas; alts alphas; alts alphas; alts alphas;
                   seqs [ trm "("; ref "sum"; trm ")" ] ] in
  let pd  = alts [ lf; seqs [lf; trm "*"; lf] ] in
  let sm  = alts [ pd; seqs [pd; trm "+"; pd] ] in
  print_endline @@ gpick 20 [("",leq sm);("sum",sm)]
