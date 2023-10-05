(* 
** This file is part of the FIREBALL AMOEBA System.
**
** Written by:
**		Stefan Bosse
**		sbosse@physik.uni-bremen.de
**
** Last modified:
**              29/09/2002
**
** Changes:
**
**
**
** FIREBALL AMOEBA is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as 
** published by the Free Software Foundation; version 2.
**
** The FIREBALL AMOEBA is distributed in the hope that it will be usefull,
** but WITHOUT ANY WARRANTY; without even implied warranty of 
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
** General Public License for more details.
**
*) 

open StdLabels


(*
** Cache module. Provides a fixed table cache. The fixed table
** is treated like a circular buffer, therefore if fille up, the oldest
** entry is overwritten.
** Seraching is done from the newest entry downto the oldest. 
** Assumption: the cache is mostly filled up.
** It's possible to invalidate (remove) a cache entry.
*)

let mod_cache_ver = 1.0

type ('a,'b) cache_entry =
{
    cache_key   : 'a;
    cache_data  : 'b;    
}

type ('a,'b) t = 
{
    mutable cache_size      :   int;
    mutable cache_head      :   int;
    mutable cache_hit       :   int;
    mutable cache_miss      :   int;
    mutable cache_table     :   (('a,'b) cache_entry) option array;
}


let create ~size:n =
    {
        cache_size      = n;
        cache_table     = Array.create n None;
        cache_head      = -1;    
        cache_hit       = 0;
        cache_miss      = 0;
    }

    
let add ~cache:cache ~key:key ~data:data =
    (*
    ** Use the next free cache slot.
    *)

    cache.cache_head <- cache.cache_head + 1;
    if (cache.cache_head = cache.cache_size) then
        cache.cache_head <- 0;
    cache.cache_table.(cache.cache_head) <- Some {
                                        cache_key = key;    
                                        cache_data = data;
                                      }


let lookup ~cache:cache ~key:key =

    if (cache.cache_head = -1) then
    begin
        cache.cache_miss <- cache.cache_miss + 1;
        raise Not_found;
    end;
    let size = cache.cache_size in

    (*
    ** Assumption: the cache is mostly filled with valid entries.
    ** -> Search from head down to (head-size), with respect to wrap arounds.
    *)
    let rec iter n count =
        if (cache.cache_table.(n) <> None) then
        begin
            (* 
            ** Cache slot not empty 
            *)
            let ce = match (cache.cache_table.(n)) with
                     | Some ce  -> ce; 
                     | _        -> failwith "Programming error";
            in
            if (ce.cache_key = key) then
            begin
                cache.cache_hit <- cache.cache_hit + 1;
                ce.cache_data
            end
            else
            if(count <> size) then
            begin
                let next = if (n = 0) then
                                size-1
                            else
                                n - 1
                in
                iter next (count+1);
            end
            else
            begin
                cache.cache_miss <- cache.cache_miss + 1;
                raise Not_found; 
            end
        end
        else
        begin
            (*
            ** Cache slot empty. Skip it.
            *)
            if(count <> size) then
            begin
                let next = if (n = 0) then
                                size-1
                            else
                                n - 1
                in
                iter next (count+1);
            end 
            else
            begin
                cache.cache_miss <- cache.cache_miss + 1;
                raise Not_found;
            end;
        end
    in
    iter (cache.cache_head) 0 

let invalidate ~cache:cache ~key:key =
    if (cache.cache_head = -1) then
        raise Not_found;

    let size = cache.cache_size in

    (*
    ** Assumption: the cache is mostly filled with valid entries.
    ** -> Search from head down to (head-size), with respect to wrap arounds.
    *)
    let rec iter n count =
        if (cache.cache_table.(n) <> None) then
        begin
            (* 
            ** Cache slot not empty 
            *)
            let ce = match (cache.cache_table.(n)) with
                     | Some ce  -> ce; 
                     | _        -> failwith "Programming error";
            in
            if (ce.cache_key = key) then
            begin
                cache.cache_table.(n) <- None;
            end
            else
            if(count <> size) then
            begin
                let next = if (n = 0) then
                                size-1
                            else
                                n - 1
                in
                iter next (count+1);
            end
            else
                raise Not_found; 
        end
        else
        begin
            (*
            ** Cache slot empty. Skip it.
            *)
            if(count <> size) then
            begin
                let next = if (n = 0) then
                                size-1
                            else
                                n - 1
                in
                iter next (count+1);
            end 
            else
                raise Not_found;
        end
    in
    iter (cache.cache_head) 0 
    

     