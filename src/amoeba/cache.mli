val mod_cache_ver : float
type ('a, 'b) cache_entry = { cache_key : 'a; cache_data : 'b; } 
and ('a, 'b)
  t = {
  mutable cache_size : int;
  mutable cache_head : int;
  mutable cache_hit : int;
  mutable cache_miss : int;
  mutable cache_table : ('a, 'b) cache_entry option array;
} 
val create : size:int -> ('a, 'b) t
val add : cache:('a, 'b) t -> key:'a -> data:'b -> unit
val lookup : cache:('a, 'b) t -> key:'a -> 'b
val invalidate : cache:('a, 'b) t -> key:'a -> unit
