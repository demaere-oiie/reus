# REUS

## Regular Expression Uniform Selectionk

To select uniformly from regular expressions, invoke the `Make` functor with `max_n` set to 0:

```
module M = Make (struct
  let max_n = 0
end)
```

Then you will work with regular expressions of type `re`:

- `Trm` of string
- `Alt` of re * re
- `Seq` of re * re

to build up a regular expression for the data space from which you will be selecting uniformly.

Then use `pick n` to select a string of `n` terminals.

### `Leq` node

You can use a `Leq` node to consider not just `n` terminal strings, but `n` *or fewer* terminal strings.

### example

```
  let open M in
  let digits = List.map trm [ "0"; "1"; "2"; "3" ] in
  let alphas = List.map trm [ "a"; "b"; "c" ] in
  print_endline @@ pick 8 @@ leq
  @@ seqs
       [
         alts [ Seq (seqs digits, trm "a"); Seq (trm "0", seqs alphas) ];
         trm ":";
         alts digits;
         alts alphas;
       ];
```

## Context Free Grammar Uniform Selection

To select uniformly from context free grammars, invoke the `Make` functor with `max_n` set to the longest string of terminals you will generate:

```
module G = Make (struct
  let max_n = 32
end)
```

Then use `gpick n [productions...]` to select a string of `n` terminals.

### `Ref` node

Build up grammars as in the regular expression case, but you may use a `Ref` node to recur into grammar productions.

### example

```
  let open G in
  let lf = alts [ alts alphas; seqs [ trm "("; ref "sum"; trm ")" ] ] in
  let pd = alts [ lf; seqs [ lf; trm "*"; lf ] ] in
  let sm = alts [ pd; seqs [ pd; trm "+"; pd ] ] in
  print_endline @@ gpick 20 [ ("", leq sm); ("sum", sm) ]
```
