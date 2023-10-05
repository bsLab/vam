(*
**  THIS SOFTWARE IS OWNED AND COPYRIGHTED BY
**
**    ###     ####   ####               #         ##         #####
**    #  #    #      #                 #         # #        #     #
**    #   #   #      #                #         #  #       #      #
**    #   #   #      #               #         #   #      #      #   
**    ####    ####   ####  ####     #         ######     ########
**    #   #      #      #          #         #     #    #      #
**    #   #      #      #         #         #      #   #       #
**    #  #       #      #        #         #       #  #       #
**    ###     ####   ####       ######### #        # #########
**
**    Stefan Bosse (c) 2003
**   
**  THIS SOFTWARE MAY NOT BE COPIED, EXTRACTED, MODIFIED, OR 
**  OTHERWISE USED IN A CONTEXT OUTSIDE OF THE VAM SYSTEM.
** 
*)

(*
** Free cluster list management. This is a core feature
** of the filesystem!
**
**  Free and used cluster units:
**
**      addr: blocks
**      size: blocks (!)
**
** 0. Init:
**      free_create
**
** 1. File Creation:
**      free_new or free_match [known final file size]
**
** 2. File size grow:
**      free_append     [reserve enough space for further modifications
**                       size > currently needed size]
**
** 3. File was committed:
**      free_merge      [return not needed disk space]
**
** 4. File deletion:
**      free_merge
**
** 
*)

let version = "1.06"
(* #define DEBUG *)

(*
** Cluster flags (determines divide mode)
*)

type cluster_flag = 
    | Cluster_FREE          (* The cluster is indeed free       *)
    | Cluster_USED          (* The cluster is currently used    *)

(*                                
** The left side of the free cluster follows a currently used cluster
** with an uncommitted file. In the case, the used cluster must be 
** expandend, the beginning of this free cluster is needed. Afte the
** file was committed, the RESERVED flag must be cleared to allow
** the efficient bottom divide mode again.
*)

    | Cluster_RESERVED 


(*
** Free list structure.
**
** The free cluster list is diveded in sublists with a limited
** size range. New entries are always put on the head of the
** list, therefore a FIFO order is achieved.
**
*)

type free_block = {
    mutable fb_addr: int;
    mutable fb_size: int;
    mutable fb_flag: cluster_flag;
}

let nilfb = {fb_addr=0;fb_size=0;fb_flag=Cluster_FREE}

(*
** Free block list management. Normally, the body list is emtpy,
** and the tail list contains the free_block list.
*)

type free_block_list = {
    mutable body: free_block list;  (* temporarily list *)
    mutable tail: free_block list;  (* the real list    *)
    mutable biggest: free_block;
}

type free_blocks = {
    mutable fbs_array: free_block_list array;
    mutable fbs_range: (int * int) array;
    mutable fbs_num: int;
}

(*
** Divide Mode (see below)
*)

type free_divide_mode = 
    | Free_Bottom
    | Free_Half
    | Free_Top

open Printf

let print_free_list fl =
    let flar = fl.fbs_array in
    let srar = fl.fbs_range in
    let nmax = fl.fbs_num - 1 in

    print_string "Free cluster list:"; print_newline ();
    Array.iter (fun fcl ->
        List.iter (fun fc ->
                print_string (sprintf "addr=%8d size=%8d\n" 
                                  fc.fb_addr fc.fb_size);

            ) fcl.tail;
    ) flar;
    print_newline ()
    

(*
** Find the biggest cluster in a freelist.
*)

let find_biggest fl =
    let big = ref nilfb in
    List.iter (fun fb -> if fb.fb_size > !big.fb_size then
                         begin
                            big := fb;
                         end;             
        ) fl.tail;
    fl.biggest <- !big


(*
** Divide a free cluster (addr,size') in at least a cluster
** with newsize<=size' and if there are remains in one ore two 
** free clusters right or left and right from the middle of the
** original cluster depending of the flags bottom.
**
** Mode Free_Bottom
**
** Before (addr,size)
** ----------------------------------------------------
**
** After:
** ++++++++XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
**
** +: Extracted cluster
** X: new free cluster
**
** Mode Free_Half
**
** Before (addr,size)
** ----------------------------------------------------
**
** After:
** XXXXXXXXXXXXXXXXXXXXXXXXXX++++++++YYYYYYYYYYYYYYYYYY
**
** +: Extracted  cluster
** X,Y: new free clusters
**
**
** This scheme is used because on file creation the file server
** not know the final size of a file. Following modify requests
** will increase the file size, and there must be a great probability
** for a free cluster following the current file end.
**
**
** Mode Free_Top
**
** Before (addr,size)
** ----------------------------------------------------
**
** After:
** XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX++++++++
**
** +: Extracted cluster
** X: new free cluster
**
** Only useable if the final size of file is known (for example
** with a afs_CREATE request and the afs_COMMIT/SAFETY flag set).
**
**
**
** Args:
**  old: original free cluster (addr,size')
**  size: desired size from the free cluster [blocks]
**  mode: 
**
**
** Return:
**  new:    (addr,size,flag=USED)
**  left:   (addr'',size'',flag=FREE)  
**  right:  (addr''',size''',flag=RESERVED)
**
**
*)



let _free_divide ~old ~size ~mode =

    if (size < old.fb_size) then
    begin
        match mode with
        | Free_Bottom ->
        begin
            {fb_addr=old.fb_addr; 
             fb_size=size;
             fb_flag=Cluster_USED},
            nilfb,
            {fb_addr=old.fb_addr+size;
             fb_size=old.fb_size-size;
             fb_flag=Cluster_RESERVED}
        end;
        | Free_Half ->
        begin
            let os2 = old.fb_size/2  in

            if (size < os2) then
                {fb_addr=old.fb_addr+os2;
                 fb_size=size;
                 fb_flag=Cluster_USED},
                {fb_addr=old.fb_addr;
                 fb_size=os2;
                 fb_flag=old.fb_flag},      (* !!! *)
                {fb_addr=old.fb_addr+os2+size;
                 fb_size=old.fb_size-os2-size;
                 fb_flag=Cluster_RESERVED}
            else
                {fb_addr=old.fb_addr;
                 fb_size=os2;
                 fb_flag=Cluster_USED},
                nilfb,
                {fb_addr=old.fb_addr+os2;
                 fb_size=old.fb_size-os2;
                 fb_flag=Cluster_RESERVED}
        end;
        | Free_Top ->
        begin
            {fb_addr=old.fb_addr+old.fb_size-size; 
             fb_size=size;
             fb_flag=Cluster_USED},
            {fb_addr=old.fb_addr;
             fb_size=old.fb_size-size;
             fb_flag=old.fb_flag},          (* !!! *)
            nilfb
        end
    end
    else if (size = old.fb_size) then
    begin
        old,
        nilfb,
        nilfb    
    end 
    else
    begin
        nilfb,
        nilfb,
        nilfb
    end


let free_divide ~old ~size ~mode =
#ifdef DEBUG
    Db.Pr.sddd 1 "free_divide [od,os,size->]" old.fb_addr old.fb_size size;
#endif    
    let n,l,r = _free_divide old size mode in
#ifdef DEBUG
    Db.Pr.sdd 1 "free_divide [->nd,ns]" n.fb_addr n.fb_size;
    Db.Pr.sdd 1 "free_divide [->ld,ls]" l.fb_addr l.fb_size;
    Db.Pr.sdd 1 "free_divide [->rd,rs]" r.fb_addr r.fb_size;
#endif    
    n,l,r


(*
** Find a free cluster (X) with start address 'addr' and remove it
** from the free list. It's assumed that this cluster
** is somewhere in the front of a free list. Therefore, all 
** free cluster lists are searched in a round-robinson style.
**
**      L : AAAAAA XXXX BBBB ->
**      L': AAAAAA      BBBB ->
**
**      XXXX
**
*)


let free_find ~fl ~addr =
    let flar = fl.fbs_array in
    let nmax = fl.fbs_num in
    let found = ref nilfb in
    let empty = ref 0 in
    let mask = ((1 lsl nmax) - 1) in

#ifdef DEBUG
    Db.Pr.sd 1 "free_find [addr]" addr;
    print_free_list fl;
#endif

    let rec iter n =
        match flar.(n).tail with
        | hd::tl -> 
        begin
            if (hd.fb_addr <> addr) then
            begin
                flar.(n).body <- flar.(n).body @ [hd];
                flar.(n).tail <- tl;
                let next = if (n+1) = nmax then
                              0
                           else
                              (n+1)
                in
                iter next;
            end 
            else
            begin
                (*
                ** Success.
                *)
                found := hd;
                flar.(n).tail <- flar.(n).body @ tl;
                flar.(n).body <- [];

                if (!found = flar.(n).biggest) then
                    find_biggest flar.(n);
            end;
        end;
        | [] ->   
        begin
            empty := !empty lor (1 lsl n);
            let next = if (n+1) = nmax then
                              0
                           else
                              (n+1)
            in
            if (!empty land mask <> mask) then
                iter next;
        end;
    in
    iter 0;
    (*
    ** Move the body lists to the front of the tail lists.
    *)
    for i = 0 to nmax-1
    do
        flar.(i).tail <- flar.(i).body @ flar.(i).tail;
        flar.(i).body <- [];
    done;
#ifdef DEBUG
    Db.Pr.sd 1 "free_find [found: size]" (!found.fb_size);
    print_free_list fl;
#endif
    !found
    


(*
** Insert a free cluster (X) in the appropiate free cluster list.
** This cluster is inserted on the head of the appropiate list 
** (FIFO order).
**
**      L:   AAAAAA BBBBBB
**
**      XXXXXX ->
**
**      L':  XXXXXX AAAAAA BBBBBB
**
*)

let free_insert ~fl ~newc =
    let flar = fl.fbs_array in
    let srar = fl.fbs_range in
    let nmax = fl.fbs_num - 1 in
    let size = newc.fb_size in

#ifdef DEBUG
    Db.Pr.sdd 1 "free_insert [new addr,size]" newc.fb_addr newc.fb_size;
    print_free_list fl;
#endif


    let rec iter n =
        let low,high = srar.(n) in
        if (size >= low && size < high) then
        begin
            flar.(n).tail <- [newc] @ flar.(n).tail;
            if (newc.fb_size > flar.(n).biggest.fb_size) then
                flar.(n).biggest <- newc;
        end 
        else
        begin
            if (n > 0) then
                iter (n-1)
            else
                failwith ("free_insert: inconsistent free list/size="^
                           (string_of_int size));
        end
    in
    if (newc <> nilfb) then iter nmax

(*
** Try to merge a free cluster (X) (previously allocated
** with free_new or free_append) after a file creation
** with an already existing one from the free list. 
** The new cluster is flagged with Cluster_FREE! The merged cluster
** (if any) or the cluster X is inserted in the free list again.
**
**      L:  AAAA XXXXX             BBBBB ->
**
**      XXXXXX +
**            YYYYYYYYYY =
**      ZZZZZZZZZZZZZZZZ                 ->
**
**      L': AAAA ZZZZZZZZZZZZZZZZ BBBBB
*)

let free_merge ~fl ~newc =

#ifdef DEBUG
    Db.Pr.sdd 1 "free_merge [new addr,size]" newc.fb_addr newc.fb_size;
    print_free_list fl;
#endif

    if (newc <> nilfb) then
    begin
        newc.fb_flag <- Cluster_FREE;
        let addr = newc.fb_addr + newc.fb_size in
        let size = newc.fb_size in
    
        let fb = free_find fl addr in
        if (fb <> nilfb) then
            free_insert fl {newc with fb_size = size + fb.fb_size}
        else
            free_insert fl newc
    end
 


(*
** Search the freecluster list for a cluster with
** start address addr. Take the front of the cluster
** with the desired size, and insert the rest in the list again.
** Return the extracted cluster.
**
**  Units:
**      addr,size : blocks
**
**      L:   AAAA XXXX BBBBB ->
**
**      L':  AAAA   XX BBBBB ->
**
**      XX
**
**
*)

let free_append ~fl ~addr ~size =
    let fb = free_find fl addr in

#ifdef DEBUG
    Db.Pr.sdd 1 "free_append [addr,size]" addr size;
#endif

    if (fb.fb_size >= size) then
    begin
        let newc,left,right = free_divide fb size Free_Bottom in 
        free_insert fl right;
        newc
    end
    else 
    begin
#ifdef DEBUG
        print_free_list fl;
#endif
        free_insert fl fb;
        nilfb
    end



(*
** Get a free cluster with specified size somewhere in the file system. 
** A file was created. The biggest cluster in a freelist is
** taken and splitted to the desired size and the rest. Searched is
** from the largest free clusters down to the smallest possible.
**
** Units:
**  size: blocks
**
*)

let free_new ~fl ~size =
    let flar = fl.fbs_array in
    let srar = fl.fbs_range in
    let nmax = fl.fbs_num - 1 in


    let rec iter n =
        let low,high = srar.(n) in
#ifdef DEBUG
        Db.Pr.sdd 1 "free_new: [search low,high...]" low high;
#endif
        if (flar.(n).tail <> []) then
        begin
            (*
            ** Find the biggest cluster in this list!
            *)
            let rec find () = 
                if (flar.(n).biggest.fb_size >= size) then
                begin
                  match flar.(n).tail with
                  | hd::tl ->
                  begin
                    if (hd = flar.(n).biggest) then
                    begin
                        flar.(n).tail <- flar.(n).body @ tl;
                        flar.(n).body <- [];
                        find_biggest flar.(n);
#ifdef DEBUG
                    Db.Pr.sddd 1 "free_new: [od,os,size]"
                                 hd.fb_addr hd.fb_size size;
                                    
#endif
                        hd
                    end
                    else
                    begin
                        flar.(n).body <- flar.(n).body @ [hd];
                        flar.(n).tail <- tl;
                        find ();
                    end;
                  end;
                  | [] -> nilfb;
                end
                else
                    nilfb
            in
            let found = find () in
            if (found <> nilfb) then
                found
            else
            begin
                if (n > 0 && size < low) then
                    iter (n-1)
                else
                    nilfb
            end
        end 
        else if (size < low) then
        begin
            if (n > 0) then
                iter (n-1)
            else
                nilfb
        end
        else
            nilfb
    in
    let cl = iter nmax in
    (*
    ** Move the body lists to the front of the tail lists.
    *)
    for i = 0 to nmax-1
    do
        flar.(i).tail <- flar.(i).body @ flar.(i).tail;
        flar.(i).body <- [];
    done;

    let mode = if (cl.fb_flag = Cluster_RESERVED) then
                    Free_Half
                 else
                    Free_Bottom
    in
    let newc,left,right = free_divide cl size mode in

    free_insert fl right;
    if (mode=Free_Half) then
        free_insert fl left;
    newc


(*
** Get a free cluster with specified size somewhere in the file system. 
** A file was created. The best matching cluster in a freelist is
** taken and splitted to the desired size and the rest. The free cluster
** is taken from the best matching frecluster list.
** Only usefull in the case the final size of a file is already
** known (e.g. small files transfered with a single request).
**
** Units:
**  size: blocks
**
*)

let free_match ~fl ~size =
    let flar = fl.fbs_array in
    let srar = fl.fbs_range in
    let nmax = fl.fbs_num - 1 in

    let rec iter n =
        let low,high = srar.(n) in
        if (size <= high && flar.(n).tail <> []) then
        begin
            (*
            ** Find the best matching cluster!
            *)

            let found = ref nilfb in
            List.iter ( fun fb ->
                if (fb.fb_size >= size &&  
                    (!found.fb_size > fb.fb_size ||
                     !found = nilfb)) then
                                        found := fb;
            ) flar.(n).tail;        
            if (!found = nilfb) then
            begin
                if (n < nmax) then
                    iter (n+1)
                else
                    nilfb
            end
            else
            begin
                (*
                ** Remove the found freecluster from the freelist.
                *)
                flar.(n).body <- [];
                List.iter ( fun fb ->
                        if (fb <> !found) then
                            flar.(n).body <- flar.(n).body @ [fb];
                ) flar.(n).tail;
                flar.(n).tail <- flar.(n).body;
                flar.(n).body <- [];
                if(!found = flar.(n).biggest) then
                    find_biggest flar.(n);
                !found;
            end;
        end 
        else 
        begin
            if (n < nmax) then
                iter (n+1)
            else
                nilfb
        end;
    in

    (*
    ** Start searching in the freecluster list with the smallest
    ** cluster sizes!
    *)
    let cl = iter 0 in
    if (cl <> nilfb) then
    begin
        let newc,left,right = free_divide cl size Free_Top in
        free_insert fl left;
        newc
    end
    else
        nilfb    

(*
** Release a reserved cluster with the start address 'addr'.
** It' assumed that this cluster is somewhere in the front of a free list. 
** Therefore, all free cluster lists are searched in a round-robinson style.
** After file creation is finished, normally the free_merge
** function do this job in a more efficient way.
**
**      L:  AAAA XXXXXX(RES)  BBB ->
**      L': AAAA XXXXXX(FREE) BBB
** 
*)

let free_release ~fl ~addr =
    let flar = fl.fbs_array in
    let nmax = fl.fbs_num in
    let found = ref nilfb in
    let empty = ref 0 in
    let mask = ((1 lsl nmax) - 1) in
    let rec iter n =
        match flar.(n).tail with
        | hd::tl -> 
        begin
            if (hd.fb_addr <> addr) then
            begin
                flar.(n).body <- flar.(n).body @ [hd];
                flar.(n).tail <- tl;
                let next = if (n+1) = nmax then
                              0
                           else
                              (n+1)
                in
                iter next;
            end 
            else
            begin
                (*
                ** Success.
                *)
                hd.fb_flag <- Cluster_FREE;
            end;
        end;
        | [] ->   
        begin
            empty := !empty lor (1 lsl n);
            let next = if (n+1) = nmax then
                              0
                           else
                              (n+1)
            in
            if (!empty land mask <> mask) then
                iter next;
        end;
    in
    iter 0;
    (*
    ** Move the body list to the front of the tail list.
    *)
    for i = 0 to nmax-1
    do
        flar.(i).tail <- flar.(i).body @ flar.(i).tail;
        flar.(i).body <- [];
    done

(*
** Create a free clusterlist object handler. The size range is divided
** linearely in n equidistant ranges (decades of tenth). 
*)

let free_create ~n ~size =         
    let srar = Array.create n (0,0) in
    let flar = Array.create n {body=[];tail=[];biggest=nilfb} in
    for i = 0 to n-1
    do
        flar.(i) <- {body=[];tail=[];biggest=nilfb};
    done;

    let d = ref 1 in
    for i = 0 to n-2
    do
        srar.(i) <- (!d,!d*10);
        d := !d * 10;
    done;
    if (size > !d*10) then
        srar.(n-1) <- (!d,size)
    else
        srar.(n-1) <- (!d,!d*10);
    {
        fbs_array = flar;
        fbs_range = srar;
        fbs_num = n;
    }


(*
** Compact the freelist (merge contigous clusters). Simply said,
** but hard to perform.
*)

let free_compact fl =
    (*
    ** First merge all sublists to one huge list.
    *)

    let fl' = ref [] in
    for i = 0 to fl.fbs_num-1
    do
        fl' := !fl' @ fl.fbs_array.(i).tail;
    done;

    (*
    ** Now resort the freelist with increasing disk address
    ** order.
    *)
    
    let fl'' = Sort.list (fun f1 f2 ->
                    (f1.fb_addr < f2.fb_addr)
                ) !fl' in

    (*
    ** Try to merge contigous clusters.
    *)

    let merged = ref [] in
    let lastfb = ref {fb_addr=0;fb_size=0;fb_flag=Cluster_FREE} in

    List.iter (fun fs ->
            if ((!lastfb.fb_addr + !lastfb.fb_size) <> fs.fb_addr) then
            begin
                if ((!lastfb.fb_addr + !lastfb.fb_size) <> 0) then
                begin
                    merged := !merged @ [!lastfb];
                end;
                lastfb := fs;
            end
            else
            begin
                lastfb := {fb_addr = !lastfb.fb_addr;
                           fb_size = fs.fb_size + !lastfb.fb_size;
                           fb_flag = !lastfb.fb_flag};
            end
        ) fl'';
    merged := !merged @ [!lastfb];

    (*
    ** Now rebuild the freelist.
    *)
    let flar = Array.create fl.fbs_num {body=[];tail=[];biggest=nilfb} in
    for i = 0 to fl.fbs_num-1
    do
        flar.(i) <- {body=[];tail=[];biggest=nilfb};
    done;
    let fl''' = {fbs_array = flar;
                 fbs_range = fl.fbs_range;
                 fbs_num   = fl.fbs_num} in 
    List.iter ( fun fb ->
#ifdef DEBUG 
                    Db.Pr.sdd 1 "[daddr,dsize]" fb.fb_addr fb.fb_size;
#endif
                    free_insert fl''' fb ) !merged;
    (*
    ** Huhhh, all work is done.
    *)
    fl'''       
    

(*
** Return the number of free clusters, the total free space and
** an array containing (low,high,num) free list statistics.
*)

let free_info fl =
    let flar = fl.fbs_array in
    let srar = fl.fbs_range in
    let nmax = fl.fbs_num in
    let totn = ref 0 in
    let tots = ref 0 in
    let tota = Array.create nmax (0,0,0) in
    for i = 0 to nmax-1
    do
        totn := !totn + (List.length flar.(i).tail);
        let count = ref 0 in
        List.iter (fun fb ->
            tots := !tots + fb.fb_size;
            incr count;
            ) flar.(i).tail;
        let low,high = srar.(i) in
        tota.(i) <- (low,high,!count);
    done;
#ifdef DEBUG
    print_free_list fl;
#endif
    !totn,!tots,tota
