// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = 
        | M of Map<'a, uint32>
        override this.ToString() = 
            match this with
                | M(m: Map<'a, uint32>) -> 
                    let asList: list<'a* uint32> = Map.toList m
                    let strings = Array.create (asList.Length * 2 - 1) ""
                    asList
                    |> List.iteri (fun i element ->
                        let key = element |> fst
                        let value = element |> snd
                        // let toAdd = $"({key}, #{value})" // noget virker ikke
                        let toAdd = sprintf "(%A, #%A)" key (value |> int)
                        strings.[i * 2] <- toAdd
                        if i <> asList.Length - 1 then
                            strings.[i * 2 + 1] <- ", "
                    )
                    Array.fold (fun a b -> a + b) "" strings
                    |> fun a ->
                        sprintf "{%s}" a

    let empty = M(Map.empty)
    let isEmpty (M(s)) = Map.isEmpty(s)
    let size (M(s)) = Map.fold (fun a k v -> a+v) 0u s
    let contains a (M(s))= Map.containsKey a s
    let numItems a (M(s)) = defaultArg(Map.tryFind a s) 0u
    let add a n (M(s)) = M(Map.add a ((defaultArg(Map.tryFind a s)0u) + n) s)
    let addSingle a (M(s)) = M(Map.add a ((defaultArg(Map.tryFind a s)0u) + 1u) s)
    let remove a n (M(s)) = M(if(n<(defaultArg(Map.tryFind a s) 0u)) then Map.add a ((defaultArg(Map.tryFind a s) 0u)-n) s else Map.remove a s)
    let removeSingle a (M(s)) = M(if((defaultArg(Map.tryFind a s) 0u)>1u) then Map.add a ((defaultArg(Map.tryFind a s) 0u)-1u) s else Map.remove a s)
    let fold f acc (M(s)) = Map.fold f acc s
    let foldBack f (M(s)) acc  = Map.foldBack f s acc
    let map f (M(s)) = Map.fold (fun a k v -> add (f(k)) v a) empty s
    let ofList lst = List.fold (fun a k -> addSingle k a) empty lst
    let toList (M(s)) = Map.fold (fun a k v -> a@[for i in 1u..v do yield k] ) List.Empty s
    let union (M(m)) (M(k: Map<'a,uint32>)) =
        let leftJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> max i value)   
            )
        leftJoin m k
        |> leftJoin k
        |> M

  
    let sum (M(m)) (M(k: Map<'a,uint32>)) =
        let leftJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> i + value)   
            )
        let outerJoin left (right: Map<'a,uint32>) =
            left
            |> Map.map (fun key value -> 
                defaultArg (right.TryFind(key)) 0u
                |> (fun i -> 
                    if i > 0u then
                        i
                    else
                        value
                )   
            )
        leftJoin m k
        |> outerJoin k
        |> M

    let subtract ((M(m))) ((M(k))) =
        let keysToRemove = k |> Map.toList 
        let rec updateMap (resultMap: Map<'a, uint32>) (i: int) =
            match i with 
                | -1 -> resultMap
                | n -> 
                    let amountToRemove = k.Item(keysToRemove.[n] |> fst)
                    let amountPresent = defaultArg (m.TryFind(keysToRemove.[n] |> fst)) 0u
                    if amountToRemove >= amountPresent then
                        updateMap (resultMap.Remove(keysToRemove.[n] |> fst)) (n - 1)
                    else
                        updateMap (resultMap.Add (keysToRemove.[n] |> fst, (amountPresent - amountToRemove))) (n - 1)     
        updateMap m (keysToRemove.Length - 1)
        |> M


    let intersection (M(m)) (M(k)) =
        let keysToCheck = k |> Map.toList
        let rec updateMap (resultMap: Map<'a, uint32>) (i: int) =
            match i with
                | -1 -> resultMap
                | n ->
                    let mValue = defaultArg (m.TryFind(keysToCheck.[n] |> fst)) 0u
                    let kValue = keysToCheck.[n] |> snd
                    if mValue > 0u then
                        updateMap (resultMap.Add((keysToCheck.[n] |> fst), (min mValue kValue))) (n - 1)
                    else
                        updateMap resultMap (n - 1)
        updateMap Map.empty (keysToCheck.Length - 1)
        |> M
     
