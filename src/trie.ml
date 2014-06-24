open Batteries

module Make (M : Map.S) = struct

  type 'a t = Node of 'a option * 'a t M.t

  type key = M.key

  let empty = Node(None, M.empty)

  let is_empty = function
  | Node(None, m) -> M.is_empty m
  | _ -> false

  let read_key k = function
  | Node(_, m) -> M.find k m

  let get_root = function
  | Node(Some v, _) -> v
  | _ -> raise Not_found

  let rec find_ l trie = match l with
  | [] -> get_root trie
  | h :: t -> find_ t (read_key h trie)

  let rec find ?default l t = try
    find_ l t
  with Not_found ->
    match default with
    | None -> raise Not_found
    | Some d -> d

  let mem l t =
    try find_ l t; true
    with Not_found -> false

  let rec add l v t = match l, t with
  | [], Node(_, m) -> Node(Some v, m)
  | h :: t, Node(o, m) ->
    Node(o, M.modify_def empty h (add t v) m)

  let cons h (t, a, b) = h :: t, a, b

  let rec find_longest l t = match l, t with
  | [], Node(Some v, _) -> [], v, []
  | [], Node(None, _) -> raise Not_found
  | h :: t, Node(Some v, m) ->
    (try cons h (find_longest t (M.find h m)) with Not_found -> [], v, h :: t)
  | h :: t, Node(None, m) -> cons h (find_longest t (M.find h m))

  let rec find_all l t = match l, t with
  | [], Node(Some v, _) -> [[], v, []]
  | [], Node(None, _) -> []
  | h :: t, Node(Some v, m) ->
    (try List.map (cons h) (find_all t (M.find h m)) with Not_found -> [[], v, t])
  | h :: t, Node(None, m) ->
    (try List.map (cons h) (find_all t (M.find h m)) with Not_found -> [])

  let rec modify_def def l f t = match l, t with
  | [], Node(None, m) -> Node(Some (f def), m)
  | [], Node(Some v, m) -> Node(Some (f v), m)
  | h :: t, Node(o, m) ->
    Node(o, M.modify_def empty h (modify_def def t f) m)

  let rec map f = function
  | Node(None, m) -> Node(None, M.map (map f) m)
  | Node(Some v, m) -> Node(Some (f v), M.map (map f) m)

end
