(** [('k, 'v) t] is the type of maps that bind keys of type
    ['k] to values of type ['v]. *)
type ('k, 'v) t

(** [insert k v m] is the same map as [m], but with an additional
    binding from [k] to [v].  If [k] was already bound in [m],
    that binding is replaced by the binding to [v] in the new map. *)
val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

(** [find k m] is [v] where [k] is bound to [v] in [m].
    Requires: [k] is a valid key in [m]. *)
val find : 'k -> ('k, 'v) t -> 'v

(** [remove k m] is the same map as [m], but without any binding of [k].
    If [k] was not bound in [m], then the map is unchanged. *)
val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

(** [empty] is the empty map *)
val empty : ('k, 'v) t

(** [of_list lst] is a map containing the same bindings as
    association list [lst].
    Requires: [lst] does not contain any duplicate keys. *)
val of_list : ('k * 'v) list -> ('k, 'v) t

(** [bindings m] is an association list containing the same
    bindings as [m]. There are no duplicate keys in the list. *)
val bindings : ('k, 'v) t -> ('k * 'v) list
