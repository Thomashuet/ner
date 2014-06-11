open Batteries

open CamomileLibraryDefault.Camomile
module CM = CaseMap.Make(UTF8)

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

let valid name = UTF8.length name > 1 && name <> CM.lowercase name

let rec build_trie ic trie =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> trie
  | Some l ->
    match Str.split tab l with
    | [name; entity; count] when valid name ->
      build_trie ic
        (T.modify_def M.empty (split name)
          (M.add entity (float_of_string count))
          trie)
    | _ -> build_trie ic trie

let rec build_means ic m =
  let loption = try Some (input_line ic) with End_of_file -> None in
  match loption with
  | None -> m
  | Some l ->
    match Str.split tab l with
    | [page; auto; mean] ->
      let auto = float_of_string auto in
      if auto > 0. then
	build_means ic (M.add page (auto, float_of_string mean) m)
      else build_means ic m
    | _ -> build_means ic m

(* count paths of length `hops` in `g` between `dst` and `src` *)
let rec paths g hops dst src =
  if hops = 1 then
    if try S.mem dst (M.find src g) with Not_found -> false then 1 else 0
  else
    S.fold ((+) % paths g (hops-1) dst)
      (try M.find src g with Not_found -> S.empty)
      0

let is_other s =
  String.length s > 8 && String.sub s 0 8 = "<Other>:"

let relatedness avg m g x y =
  if x = y then 1. else
  let find k = try M.find k m with Not_found -> 0., 0. in
  match is_other x, is_other y with
  | true, true -> avg
  | true, false -> snd (find y)
  | false, true -> snd (find x)
  | false, false ->
    float (paths g 2 x y + paths g 2 y x) /.
    max (fst (find x) +. fst (find y)) 1.

let ner trie avg means graph text =
  (* memoise relatedness *)
  let rec rel =
    let h = Hashtbl.create 42 in
    fun x y ->
      if x > y then rel y x else
      try Hashtbl.find h (x, y) with
      | Not_found ->
        let ans = relatedness avg means graph x y in
        (Hashtbl.add h (x, y) ans; ans)
  in
  let length = List.fold_left (+) 0 % List.map UTF8.length in
  let rec get_candidates pos = function
  | [] -> []
  | h :: t as l -> try
    let matched, entities, tail = T.find_longest l trie in
    let size = length matched in
    (List.map
      (fun (entity, weight) -> pos, pos + size, entity, (0., weight))
      (M.bindings entities)) @ get_candidates (pos + size) tail
  with Not_found -> get_candidates (pos + UTF8.length h) t
  in
  let sort = List.sort (fun (_, _, _, a) (_, _, _, b) -> compare a b) in
  let conflicts (a, b, _, _) (c, d, _, _) = b > c && d > a in
  let (++) (c, p) r = c +. p *. r, p in
  let (--) (c, p) r = c -. p *. r, p in
  let rec remove (_, _, ex, _ as x) = function
  | [] -> []
  | h :: t when h = x -> remove x t
  | (l, r, e, w) :: t ->
    (l, r, e, w -- rel e ex) :: remove x t
  in
  let rec product (l1, r1, e1, w1) = function
  | [] -> (l1, r1, e1, w1), []
  | (l2, r2, e2, w2) :: t ->
    let r = rel e1 e2 in
    let h, t = product (l1, r1, e1, w1 ++ r) t in
    h, (l2, r2, e2, w2 ++ r) :: t
  in
  let rec consistency = function
  | [] -> []
  | h :: t ->
    let h, t = product h t in
    h :: consistency t
  in
  let rec disambiguate = function
  | [] -> []
  | h :: t ->
    if List.exists (conflicts h) t then disambiguate % sort % remove h @@ t
    else h :: disambiguate t
  in
  disambiguate % sort % consistency % get_candidates 0 % split @@ text

let run port names links similarity =
  (* build trie from file `names` *)
  let ic, close =
    if names = "-" then stdin, ignore else
    let ic = open_in names in ic, fun () -> close_in ic
  in
  let trie = build_trie ic T.empty in
  close ();
  (* build graph from file `links` *)
  let ic, close =
    if links = "-" then stdin, ignore else
    let ic = open_in links in ic, fun () -> close_in ic
  in
  let graph = build_graph ic M.empty in
  close ();
  (* build average distance to x from file `similarity` *)
  let ic, close =
    if similarity = "-" then stdin, ignore else
    let ic = open_in similarity in ic, fun () -> close_in ic
  in
  let means = build_means ic M.empty in
  close ();
  (* compute average distance between 2 nodes *)
  let tot = M.fold (fun _ -> (+.) % snd) means 0. in
  let n = float (M.cardinal means) in
  let avg = tot /. n /. n in
  let means = M.map (fun (x, y) -> x, y /. n) means in
  let extract = ner trie avg means graph in
  let json l =
    let rec json = function
    | [] -> ""
    | (start_pos, end_pos, entity, _) :: t when entity <> "<Other>" ->
      "{\"start\":" ^ string_of_int start_pos ^
      ",\"end\":" ^ string_of_int end_pos ^
      ",\"entity\":\"" ^ entity ^
      "\"}," ^ json t
    | _ :: t -> json t
    in
    "["^ json l ^"]\n"
  in
  print_endline "ready";
  Server.run (json % extract) port

let () =
  if Array.length Sys.argv = 5 then
    run (int_of_string Sys.argv.(4)) Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
  else print_endline "usage: ner names links similarity port"
