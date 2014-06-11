(*
Compute paths(x, x) and \sum_i{paths(x, i)}
*)

open Batteries

module S = Set.Make(String)
module M = Map.Make(String)

let extend k v m =
  M.modify_def S.empty k (S.add v) m

let increment k m = M.modify_def 0 k ((+) 1) m

let tab = Str.regexp_string "\t"

let rec build_graph ic g =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> g
  | Some l ->
    match Str.split tab l with
    | [source; target] -> build_graph ic (extend source target g)
    | _ -> build_graph ic g

let rec build_tgraph ic g =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> g
  | Some l ->
    match Str.split tab l with
    | [source; target] -> build_graph ic (extend target source g)
    | _ -> build_graph ic g

let rec paths g hops dst src =
  if hops = 1 then
    if try S.mem dst (M.find src g) with Not_found -> false then 1 else 0
  else
    S.fold ((+) % paths g (hops-1) dst)
      (try M.find src g with Not_found -> S.empty)
      0

let optionalize f x y = match x, y with
| None, None -> None
| Some z, None | None, Some z -> Some z
| Some x, Some y -> Some (f x y)

let fst x y = x

let rec neighbours g hops src =
  if hops = 1 then
    S.fold increment (try M.find src g with Not_found -> S.empty) M.empty
  else
    S.fold (M.merge (fst % optionalize @@ (+)) % neighbours g (hops-1))
      (try M.find src g with Not_found -> S.empty)
      M.empty

let run links out =
  let t0 = Unix.gettimeofday () in
  let ic = open_in links in
  let graph = build_graph ic M.empty in
  close_in ic;
  let t1 = Unix.gettimeofday () in
  prerr_endline ("graph: OK ("^string_of_float (t1 -. t0)^"s)");
  let auto = M.mapi (fun i _ -> float (paths graph 2 i i)) graph in
  let t2 = Unix.gettimeofday () in
  prerr_endline ("auto: OK ("^string_of_float (t2 -. t1)^"s)");
  let rec sum g x =
    let a = try M.find x auto with Not_found -> 0. in
    M.fold
      (fun k v acc ->
        acc +. (float v) /. (try M.find k auto +. a with Not_found -> a))
      (neighbours g 2 x)
      0.
  in
  let sums = M.mapi (fun x _ -> sum graph x) auto in
  let t3 = Unix.gettimeofday () in
  prerr_endline ("sums: OK ("^string_of_float (t3 -. t2)^"s)");
  let ic = open_in links in
  let tgraph = build_tgraph ic M.empty in
  close_in ic;
  let t4 = Unix.gettimeofday () in
  prerr_endline ("tgraph: OK ("^string_of_float (t4 -. t3)^"s)");
  let tsums = M.mapi (fun x _ -> sum tgraph x) auto in
  let t5 = Unix.gettimeofday () in
  prerr_endline ("tsums: OK ("^string_of_float (t5 -. t4)^"s)");
  let oc, close =
    if out = "-" then stdout, ignore else
    let oc = open_out out in oc, fun () -> close_out oc
  in
  M.iter
    (fun k v ->
      output_string oc
        (k^"\t"^string_of_float (M.find k auto)^"\t"^string_of_float v^"\n"))
    (M.merge (fst % optionalize @@ (+.)) sums tsums);
  close ();
  let t6 = Unix.gettimeofday () in
  prerr_endline ("output: OK ("^string_of_float (t6 -. t5)^"s)");
  let tot = int_of_float (t6 -. t0) in
  prerr_endline
    ("Total: "^string_of_int (tot / 3600)^"h"^
               string_of_int (tot mod 60 / 60)^"m"^
               string_of_int (tot mod 3600)^"s")

let () =
  if Array.length Sys.argv = 3 then run Sys.argv.(1) Sys.argv.(2)
  else print_endline "usage: similarity links out"
