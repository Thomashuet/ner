module Make :
  functor (M : Batteries.Map.S) ->
    sig
      type 'a t
      type key = M.key
      val empty : 'a t
      val is_empty : 'a t -> bool
      val read_key : M.key -> 'a t -> 'a t
      val get_root : 'a t -> 'a
      val find : ?default:'a -> M.key list -> 'a t -> 'a
      val mem : M.key list -> 'a t -> bool
      val add : M.key list -> 'a -> 'a t -> 'a t
      val find_longest : M.key list -> 'a t -> M.key list * 'a * M.key list
      val find_all : M.key list -> 'a t -> (M.key list * 'a * M.key list) list
      val modify_def : 'a -> M.key list -> ('a -> 'a) -> 'a t -> 'a t
      val map : ('a -> 'b) -> 'a t -> 'b t
    end
