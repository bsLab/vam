
let mod_dblist_ver = 1.0

(*
** Double linked circular lists
*)

type 'a dblist_node = {
    mutable dbl_data : 'a;
    mutable dbl_prev : 'a dblist_node;
    mutable dbl_next : 'a dblist_node; 
}

type 'a dblist = {
    mutable dbl_nodes : int;
    mutable dbl_head : 'a dblist_node option;
}


exception List_empty

(*
** Create a new db list. The first element is required.
*)

let create () =
    {
        dbl_nodes = 0;
        dbl_head  = None;        
    }

(*
** Iterate the list and apply for each node teh function f.
*)

let iter ~f ~dl = 
    match dl.dbl_head with
    | None      -> ();
    | Some hd   ->
        begin
            let cur = ref hd in
            for n = 1 to dl.dbl_nodes
            do
                f !cur.dbl_data;
                cur := !cur.dbl_next;
            done;
        end 
    
(*
** Convert the dblist to an array.
*)

let to_array ~dl = 
    match dl.dbl_head with
    | None      -> [||];
    | Some hd   ->
        begin
            let cur = ref hd in
            let ar = Array.create dl.dbl_nodes !cur.dbl_data in
            cur := !cur.dbl_next;
            
            for n = 1 to dl.dbl_nodes-1
            do
                ar.(n) <- !cur.dbl_data;
                cur := !cur.dbl_next;
            done;
            ar
        end 
    

(*
** Insert a new node before the head node.
*)

let insert_head ~dblist:db ~node =
    match db.dbl_head with
    | None ->
        begin
            (*
            ** Empty list. Simple.
            *)
            let rec dbn = {
                dbl_data = node;
                dbl_prev = dbn;
                dbl_next = dbn;
            } in 
            db.dbl_head  <- Some dbn;
            db.dbl_nodes <- db.dbl_nodes + 1;
        end;
    | Some dh ->
        begin
            let tail = dh.dbl_prev in
    
            let dbn = {
                dbl_data = node;
                dbl_prev = tail;
                dbl_next = dh;
            } in                
            dh.dbl_prev <- dbn;
            dh.dbl_next <- if (dh.dbl_next = dh) then
                                dbn         (* old head == tail *)
                           else
                                dh.dbl_next;     

            db.dbl_head <- Some dbn;
            db.dbl_nodes <- db.dbl_nodes + 1;
            
        end

(*
** Insert a new node after the tail node.
*)

let insert_tail ~dblist:db ~node =
    match db.dbl_head with
    | None ->
        begin
            (*
            ** Empty list. Simple.
            *)
            let rec dbn = {
                dbl_data = node;
                dbl_prev = dbn;
                dbl_next = dbn;
            } in 
            db.dbl_head  <- Some dbn;
            db.dbl_nodes <- db.dbl_nodes + 1;
        end;
    | Some dh ->
        begin
            let tail = dh.dbl_prev in
    
            let dbn = {
                dbl_data = node;
                dbl_prev = tail;
                dbl_next = dh;
            } in                
            tail.dbl_next <- dbn;
            dh.dbl_prev <- dbn;
            db.dbl_nodes <- db.dbl_nodes + 1;
        end

(*
** Find a node and return the node structure. The user specified
** function f:'a -> bool  must select the right one.
*)

 
let find ~f ~dl =

    match dl.dbl_head with
    | None      -> raise List_empty;
    | Some hd   ->
        begin
            let node = ref hd in
            let cur = ref hd in

            (try
                for n = 1 to dl.dbl_nodes
                do
                    if (f !cur.dbl_data = true) then
                    begin
                        node := !cur;
                        raise Exit;
                    end;
                    cur := !cur.dbl_next;
                done;
                raise Not_found;
            with
                | Exit  -> !node;
            );
        end 

let find_data ~f ~dl =
    let dln = find ~f ~dl in
    dln.dbl_data

(*
** Remove a node.
*)

let remove ~dl ~node =
    match dl.dbl_head with
    | None      -> raise List_empty;
    | Some dh   ->
        if (node == dh) then
        begin
            (*
            ** It's the head.
            *)
            if (dl.dbl_nodes = 1) then
            begin
                dl.dbl_head <- None;
                dl.dbl_nodes <- dl.dbl_nodes - 1;
            end
            else
            begin
                let next = dh.dbl_next in
                let prev = dh.dbl_prev in
                dl.dbl_head <- Some next;
                next.dbl_prev <- prev;
                prev.dbl_next <- next;
                dl.dbl_nodes <- dl.dbl_nodes - 1;
            end
        end
        else
        begin
            (*
            ** Not the head
            *)
            node.dbl_prev.dbl_next <- node.dbl_next;
            node.dbl_next.dbl_prev <- node.dbl_prev;            
            dl.dbl_nodes <- dl.dbl_nodes - 1;
        end
        
(*
** Remove a node ascosiated with data.
*)

let remove_data ~dl ~node =
    let node = find ~f:(fun d -> if d = node then true else false) 
                    ~dl in
    
    match dl.dbl_head with
    | None      -> raise List_empty;
    | Some dh   ->
        if (node == dh) then
        begin
            (*
            ** It's the head.
            *)
            if (dl.dbl_nodes = 1) then
            begin
                dl.dbl_head <- None;
                dl.dbl_nodes <- dl.dbl_nodes - 1;
            end
            else
            begin
                let next = dh.dbl_next in
                let prev = dh.dbl_prev in
                dl.dbl_head <- Some next;
                next.dbl_prev <- prev;
                prev.dbl_next <- next;
                dl.dbl_nodes <- dl.dbl_nodes - 1;
            end
        end
        else
        begin
            (*
            ** Not the head
            *)
            node.dbl_prev.dbl_next <- node.dbl_next;
            node.dbl_next.dbl_prev <- node.dbl_prev;            
            dl.dbl_nodes <- dl.dbl_nodes - 1;
        end
        

(*
** Return the head and remove this node from the list.
*)

let enqueue_head ~dl =
    match dl.dbl_head with
    | None      -> raise List_empty;
    | Some dh   ->
        if (dl.dbl_nodes = 1) then
        begin
            dl.dbl_head <- None;
            dl.dbl_nodes <- dl.dbl_nodes - 1;
            dh
        end
        else
        begin
            let next = dh.dbl_next in
            let prev = dh.dbl_prev in
            dl.dbl_head <- Some next;
            next.dbl_prev <- prev;
            prev.dbl_next <- next;
            dl.dbl_nodes <- dl.dbl_nodes - 1;
            dh
        end
        

(*
** Return the tail and remove this node from the list.
*)

let enqueue_tail ~dl =
    match dl.dbl_head with
    | None      -> raise List_empty;
    | Some dh   ->
        if (dl.dbl_nodes = 1) then
        begin
            dl.dbl_head <- None;
            dl.dbl_nodes <- dl.dbl_nodes - 1;
            dh
        end
        else
        begin
            let dt = dh.dbl_prev in
            let next = dt.dbl_next in
            let prev = dt.dbl_prev in
            next.dbl_prev <- prev;
            prev.dbl_next <- next;
            dl.dbl_nodes <- dl.dbl_nodes - 1;
            dt
        end
        

                