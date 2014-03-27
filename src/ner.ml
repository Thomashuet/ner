open Batteries

let word = Str.regexp "[a-zA-Z\128-\255]*"

let split =
  let get = function Str.Text s | Str.Delim s -> s in
  List.map get % Str.full_split word

module S = Set.Make(String)
module M = Map.Make(String)

let find default k m =
  if M.mem k m then M.find k m
  else default

let extend k v m =
  M.add k (S.add v (find S.empty k m)) m

let tab = Str.regexp_string "\t"

let rec build_graph ic g =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> g
  | Some l ->
    match Str.split tab l with
    | [source; target] -> build_graph ic (extend source target g)
    | _ -> build_graph ic g

let rec build_dic ic dic =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> dic
  | Some l ->
    match Str.split tab l with
    | [name; entity; count] ->
      build_dic ic (M.add name (M.add entity count (find M.empty name dic)) dic)
    | _ -> build_dic ic dic

let run names links =
  let ic, close =
    if names = "-" then stdin, ignore else
    let ic = open_in names in ic, fun () -> close_in ic
  in
  let dic = build_dic ic M.empty in
  close ();
  let ic, close =
    if links = "-" then stdin, ignore else
    let ic = open_in links in ic, fun () -> close_in ic
  in
  let graph = build_graph ic M.empty in
  close ();
  ()


let () =
  if Array.length Sys.argv = 3 then run Sys.argv.(1) Sys.argv.(2)
  else print_endline "usage: ner names links"
