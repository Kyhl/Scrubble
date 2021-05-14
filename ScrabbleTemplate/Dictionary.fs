module internal Dictionary

    open System.Collections.Generic
    type Dict =
        | Node of (bool * Map<char,Dict>) 
    //let EdgeMap (Node(b,m)) = m
        
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

    
    let step c (Node(bool0,map0)) =
       match Map.tryFind c map0 with
       | Some (Node(bool1,map1)) -> Some (bool1,Node(bool1,map1))
       | None -> None
