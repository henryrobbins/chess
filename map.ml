(** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map
    {k1:v1, k2:v2, ..., kn:vn}. If a key appears more than once in the list,
    then in the map it is bound to the left-most occurrence in the list. For
    example, [[(k, v1); (k, v2)]] represents the map {k:v1}.  The empty list
    represents the empty map.
    RI: none. *)
type ('k, 'v) t = ('k * 'v) list

let insert k v m = (k, v) :: m

let find = List.assoc

let remove k lst = List.filter (fun (k',_) -> k <> k') lst

let empty = []

let of_list lst = lst

(** [keys m] is a set-like list of the keys in [m]. *)
let keys m =
    m |> List.map fst |> List.sort_uniq Stdlib.compare

(** [binding m k] is [(k, v)], where [v] is the value that [k]
    binds in [m].
    Requires: [k] is a key in [m]. *)
let binding m k =
    (k, List.assoc k m)

let bindings m =
    List.map (binding m) (keys m)