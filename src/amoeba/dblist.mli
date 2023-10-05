val mod_dblist_ver : float

type 'a dblist_node = {
  mutable dbl_data : 'a;
  mutable dbl_prev : 'a dblist_node;
  mutable dbl_next : 'a dblist_node;
} 
and 'a dblist = {
  mutable dbl_nodes : int;
  mutable dbl_head : 'a dblist_node option;
} 
exception List_empty
val create : unit -> 'a dblist
val iter : f:('a -> 'b) -> dl:'a dblist -> unit
val to_array : dl:'a dblist -> 'a array
val insert_head : dblist:'a dblist -> node:'a -> unit
val insert_tail : dblist:'a dblist -> node:'a -> unit
val find : f:('a -> bool) -> dl:'a dblist -> 'a dblist_node
val find_data : f:('a -> bool) -> dl:'a dblist -> 'a 
val remove : dl:'a dblist -> node:'a dblist_node -> unit
val remove_data : dl:'a dblist -> node:'a -> unit
val enqueue_head : dl:'a dblist -> 'a dblist_node
val enqueue_tail : dl:'a dblist -> 'a dblist_node
