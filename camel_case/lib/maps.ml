[@@@ocaml.text 
"Hash tables implement the map data abstraction. 
A map binds key to values."]
module type Maps = sig 
  (** [('k, 'v) t] is the type of maps that bind keys of type
      ['k] to values of type ['v]. *)
  type ('k, 'v) t 
  
  (** [insert k v m] is the same map as [m], but with an additional
      binding from [k] to [v].  If [k] was already bound in [m],
      that binding is replaced by the binding to [v] in the new map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  
  (** [find k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not. *)
  val find : 'k -> ('k, 'v) t -> 'v option
  
  (** [remove k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  
  (** [empty] is the empty map. *)
  val empty : ('k, 'v) t 
  
  (** [of_list lst] is a map containing the same binding as
      association list [lst]. 
      requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t
  
  (** [binds m] is an association list containing the same
      bindings as [m]. There are no duplicates in the list. *)
  val binds : ('k, 'v) t -> ('k * 'v) list 

end

module AssocListMap : Maps = struct
  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map {k1 : v1, k2 : v2,
      ..., kn : vn}. If a key appears more than once in the list, then in the
      map it is bound to the left-most occurrence in the list. For example,
      [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
      the empty map.
      RI: none. *)
  type ('k, 'v) t = ('k * 'v) list

    let insert k v m = (k, v) :: m

    let find = List.assoc_opt

    let remove k lst = List.filter (fun (k', _) -> k <> k') lst
    
    let empty = []

    let of_list lst = lst
    
    (** [keys m] is a list of the keys in [m],
        without any duplicates.
        Efficiency: O(n log n) *)
    let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

    (** [bind m k] is [(k, v)], where [v] is the value that [k]
        binds in [m].
        Requires: [k] is a key in [m]
        Efficiency: O(n). *)
    let bind m k = (k, List.assoc k m)

    let binds m = List.map (bind m) (keys m)
end

[@@@ocaml.text 
"Mutable maps are maps whose bindings may be mutated.

An array can be used to represent a mutable map whose 
key to a value is stored by using key as an index into an array.

Since the arrays have fixed size, the implmenter now needs to know
he capacity of the table whenever an empty table is created."]
module type DirectAddressMap = sig
  type 'v t

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v] in the new map. Requires: [k] is in bounds for [m]. *)
  val insert : int -> 'v -> 'v t -> unit

  (** [find k m] is [Some v] if [k] is bound to [v] in [m], and [None]
      if not. Requires: [k] is in bounds for [m]. *)
  val find : int -> 'v t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], then the map is unchanged. Requires: [k] is in
      bounds for [m]. *)
  val remove : int -> 'v t -> unit 

  (** [create c] creates a map with capacity [c]. Keys [0] through [c-1]
      are _in bounds_ for the map. *)
  val create : int -> 'v t

  (** [of_list c lst] is a map containing the same bindings as
      association list [lst] and with capacity [c]. Requires: [lst] does
      not contain any duplicate keys, and every key in [lst] is in
      bounds for capacity [c]. *)
  val of_list : int -> (int * 'v) list -> 'v t

  (** [bindings m] is an association list containing the same bindings
      as [m]. There are no duplicate keys in the list. *)
  val binds : 'v t -> (int * 'v) list
end

module ArrayMap : DirectAddressMap = struct
  (** AF: [[|Some v0; Some v1; ... |]] represents {0 : v0, 1 : v1, ...}.
      If element [i] of the array is instead [None], then [i] is not
      bound in the map.
      RI: None. *)
  
  [@@@ocaml.text " [insert], [find] and [remove] operations
    are constant time. but that comes at the expense of forcing keys to be integers"]
  type 'v t = 'v option array

  let insert k v a = a.(k) <- Some v 

  let find k a = a.(k)

  let remove k a = a.(k) <- None

  let create c = Array.make c None 

  let of_list c lst =
    let a = create c in 
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  let binds a =
    let bs = ref [] in 
    let add_bind k v =
      match v with None -> () | Some v -> bs := (k, v) :: !bs
    in 
    Array.iteri add_bind a;
    !bs 

end

module type TableMap = sig 
  (** [('k, 'v) t] is the type of mutable table-based maps that bind
      keys of type ['k] to values of type ['v].*)
  type ('k, 'v) t 

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was
      already bound in [m], that binding is replaced by the binding to
      [v]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  
  (** [find k m] is [Some v] if [m] binds [k] to [v], and [None] if [m]
      does not bind [k]. *)
  val find : 'k -> ('k, 'v) t -> 'v option 

  (** [remove k m] mutates [m] to remove any binding of [k]. If [k] was
      not bound in [m], the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [binds m] is an association list containing the same bindings
      as [m]. *)
  val binds : ('k, 'v) t -> ('k * 'v) list 

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not
      contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t 

end

[@@@ocaml.text "what if the output of the hash is not within
  the bounds of the array: It's easy to solve. if [a] is the length
  of the array then computing [(hash k) mod a] will return an index that
  is within bounds."]

[@@@ocaml.text "and what todo if the hash function is not injective, not one-to-one.
  then multiple keys could [collide] and need to be stored at the same index
in the array. just allow it."]

[@@@ocaml.text "how to dealing with collisons: [chaining] and [probing]
  former is just store multiple bindings at each array index. latter is a
  simple way to find an empty localtion is to search ahead through the array
  indices with a fixed stride."]

module HashMap : TableMap = struct
  (** The [hash] is used to determine which bucket a key goes into. 
      The [size] is used to keep track on the number of bindings currently
      in the table. The [buckets] array has element that are association list, 
      which store the bindings. *)
    type ('k, 'v) t = {
      hash : 'k -> int;
      mutable size : int;
      mutable buckets : ('k * 'v) list array;
    }
    
    (** [capacity tab] is the number of buckets in [tab].
      Efficiency: O(1) *)
    let capacity {buckets} =
      Array.length buckets

    (** [load_factor tab] is the load factor of [tab], i.e., the number of
      bindings divided by the number of buckets. *)
    let load_factor tab =
      float_of_int tab.size /. float_of_int (capacity tab)
    
    let create hash n =
      {hash; size = 0; buckets = Array.make n []}
    
    (** [index k tab] is the index at which key [k] should be stored in the
      buckets of [tab].
      Efficiency: O(1) *)
    let index k tab =
      (tab.hash k) mod (capacity tab)

    (** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
      and does not resize the table, regardless of what happens to the
      load factor.
      Efficiency: expected O(L) *)
    let insert_no_resize k v tab =
      let b = index k tab in
      let old_bucket = tab.buckets.(b) in
      tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
      if not (List.mem_assoc k old_bucket) then
        tab.size <- tab.size + 1;
      ()

    (** [rehash tab new_capacity] replaces the buckets array of [tab] with a new
      array of size [new_capacity], and re-inserts all the bindings of [tab]
      into the new array.  The keys are re-hashed, so the bindings will
      likely land in different buckets.
      Efficiency: O(n), where n is the number of bindings. *)
    let rehash tab new_capacity =
      let rehash_bind (k, v) =
        insert_no_resize k v tab
      in 

      let rehash_bucket bucket =
        List.iter rehash_bind bucket
      in

      let old_buckets = tab.buckets in 
      tab.buckets <- Array.make new_capacity [];
      tab.size <- 0;

      (* [rehash_binding] is called by [rehash_bucket] once for every binding *)
      Array.iter rehash_bucket old_buckets
    
    (* [resize_if_needed tab] resizes and rehashes [tab] if the load factor
      is too big or too small.  Load factors are allowed to range from
      1/2 to 2. *)
    let resize_if_needed tab =
      let lf = load_factor tab in 
      if lf > 2.0 then
        rehash tab (capacity tab * 2)
      else if lf < 0.5 then 
        rehash tab (capacity tab / 2)
      else ()
    
    let insert k v tab =
      insert_no_resize k v tab;
      resize_if_needed tab
    
    let find k tab =
      List.assoc_opt k tab.buckets.(index k tab)
    

  (** [remove_no_resize k tab] removes [k] from [tab] and does not trigger
      a resize, regardless of what happens to the load factor.
      Efficiency: expected O(L) *)
    let remove_no_resize k tab =
      let b = index k tab in 
      let old_bucket = tab.buckets.(b) in 
      tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
      if List.mem_assoc k old_bucket then 
        tab.size <- tab.size - 1;
      ()
    
    let remove k tab =
      remove_no_resize k tab;
      resize_if_needed tab 
    
    let binds tab = 
      Array.fold_left
        (fun acc bucket ->
          List.fold_left
            (fun acc (k, v) -> (k, v) :: acc)
            acc bucket)
        [] tab.buckets
      
    let of_list hash lst =
      let m = create hash (List.length lst) in 
      List.iter (fun (k, v) -> insert k v m) lst;
      m
end