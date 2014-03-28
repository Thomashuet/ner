open Batteries

let word = Str.regexp "[a-zA-Z\128-\255]*"

let split =
  let get = function Str.Text s | Str.Delim s -> s in
  List.map get % Str.full_split word

module S = Set.Make(String)
module M = Map.Make(String)
module T = Trie.Make(M)

let extend k v m =
  M.modify_def S.empty k (S.add v) m

let tab = Str.regexp_string "\t"

let rec build_graph ic g =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> g
  | Some l ->
    match Str.split tab l with
    | [source; target] -> build_graph ic (extend source target g)
    | _ -> build_graph ic g

let rec build_trie ic trie =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> trie
  | Some l ->
    match Str.split tab l with
    | [name; entity; count] ->
      build_trie ic
        (T.modify_def M.empty (split name) (M.add entity (float_of_string count)) trie)
    | _ -> build_trie ic trie

let ner trie graph text =
  let length = List.fold_left (+) 0 % List.map String.length in
  let rec get_candidates pos = function
  | [] -> []
  | h :: t as l -> try
    let matched, entities, tail = T.find_longest l trie in
    let size = length matched in
    (List.map
      (fun (entity, weight) -> pos, pos + size, entity, weight)
      (M.bindings entities)) :: get_candidates (pos + size) tail
  with Not_found -> get_candidates (pos + String.length h) t
  in
  let disambiguate acc candidates =
    let conflicts (a, b, _, _) (c, d, _, _) = b > c && d > a in
    [(* TODO *)]
  in
  disambiguate [] (get_candidates 0 (split text))

let run names links =
  let ic, close =
    if names = "-" then stdin, ignore else
    let ic = open_in names in ic, fun () -> close_in ic
  in
  let trie = build_trie ic T.empty in
  close ();
  let ic, close =
    if links = "-" then stdin, ignore else
    let ic = open_in links in ic, fun () -> close_in ic
  in
  let graph = build_graph ic M.empty in
  close ();
  let extract = ner trie graph in
  ()

let () =
  if Array.length Sys.argv = 3 then run Sys.argv.(1) Sys.argv.(2)
  else print_endline "usage: ner names links"
