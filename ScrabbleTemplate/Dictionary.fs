// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

    open System.Collections.Generic
    type Dict =
        | Node of (bool * Map<char,Dict>) 

    let empty () = Node (false,Map.empty)
    
    let insert (st:string) (Node (bool0,map0)) = 
        let rec go =
            function
            | ([], Node(_,map0)) -> Node (true,map0)
            | (x::x1,(Node (bool1,map1))) ->
                match Map.tryFind x map1 with
                | Some (Node (bool2,map2)) -> Node(bool1,Map.add x (go (x1,Node(bool2,map2))) map1)
                | None -> Node(bool1,Map.add x (go (x1,empty())) map1)
        go (Seq.toList st,Node(bool0,map0))

    let lookup (st:string) (Node (bool0,map0)) = 
        let rec go = 
            function
            |([],(Node (_,_))) -> false
            |(x::x1,(Node (_,map1))) when List.isEmpty x1 ->
                match Map.tryFind x map1 with
                | Some (Node (bool1,_))  -> bool1
                | None                -> false
            |(x::xs,(Node (_,map1))) ->
                match Map.tryFind x map1 with
                | Some (Node (bool1,map2)) -> go (xs,Node(bool1,map2))
                | None                -> false
        go (Seq.toList st,(Node (bool0,map0)))
    let step c (Node(bool0,map0)) =
       match Map.tryFind c map0 with
       | Some (Node(bool1,map1)) -> Some (bool1,Node(bool1,map1))
       | None -> None
        
    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    //let step : char -> Dict -> (bool * Dict) option = fun _ _ -> failwith "Not implemented"
