namespace itiswhatitis

open System.Linq
open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

// This bot was developed by drawing inspiration from the article https://www.cs.cmu.edu/afs/cs/academic/class/15451-s06/www/lectures/scrabble.pdf


// The RegEx module is only used to parse human input. It is not used for the final product.
module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module internal State = 
    type state = {
        board           : Parser.board
        dict            : Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>
        numPlayers      : uint32
        playerTurn      : uint32
        FF              : MultiSet.MultiSet<uint32>
        playedTiles     : Map<coord,(uint32*(char*int))>
        scores          : Map<uint32,int>
        tilesLeft       : int
        tiles           : Map<uint32, tile>
     
    }
    let mkState b d pn h np pt t = {board = b; dict = d;  playerNumber = pn; hand = h; numPlayers = np; playerTurn = pt; FF = MultiSet.empty; playedTiles = Map.empty; tilesLeft = 100;scores = Map.empty; tiles = t}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayers st    = st.numPlayers
    let playerTurn st    = st.playerTurn
    
    let state (st:state) = st
module internal MoveLogic =
    open Dictionary
    let onBoard (st:State.state) (c:coord) =
        match st.board.squares c with
        |Some _ -> true
        |None -> false
    let isEmpty (playedTiles : Map<coord,uint32*(char*int)>) (c:coord) =
        if(Map.containsKey c playedTiles) then
            false
        else
            true
    let isFilled (playedTiles : Map<coord,uint32*(char*int)>) (c:coord) =
        if not (Map.containsKey c playedTiles) then
            false
        else
            true 
    let nextCoord (c:coord) (hori:bool) =
        if hori then
            ((fst c)+1,(snd c))
        else
            ((fst c),(snd c)+1)
    let prevCoord (c:coord) (hori:bool) =
        if hori then
            ((fst c)-1,(snd c))
        else
            ((fst c),(snd c)-1)
    let prev (c:coord) (hori:bool) (times:int) =
        if hori then
            ((fst c)-times,(snd c)),hori
        else
            ((fst c),(snd c)-times),hori
            
            
    let checkString (pw:string) (dict:(Dict)) =
        //Splits the string up into a char array, and loops through it to update our current dictionary
        let rec split (sDict:(bool*Dict)) word=
            match word with 
            | c1 :: c2 ->
                match step c1 (snd sDict) with
                // if chars remain in the word and it matches a word in our dict, continue.
                | Some d -> split d c2
                //if no words that match in dictionary, return none
                | None -> None
            | [] -> Some sDict
        split (false,dict) (Seq.toList pw)
    let nextCross (c:coord) (hori:bool) =
        if hori then
            ((fst c),(snd c)-1)
        else
            ((fst c)-1,(snd c))
    let prevCross (c:coord) (hori:bool) =
        if hori then
            ((fst c),(snd c)+1)
        else
            ((fst c)+1,(snd c))
    let findAnchors (st:State.state) (playedTiles : Map<coord,uint32*(char*int)>) (hori:bool)=
        
        Map.fold (fun acc x y ->
            let nextCo = (nextCoord x hori)
            let prevCo = (prevCoord x hori)
            let nextCr = (nextCross x hori)
            let prevCr = (prevCross x hori)
            let help = Set.ofList [
                        (nextCo,((isEmpty playedTiles nextCo) && (onBoard st nextCo)));
                        (prevCo,((isEmpty playedTiles prevCo) && (onBoard st prevCo)));
                      //  (nextCr,((isEmpty playedTiles nextCr) && (onBoard st nextCr)));
                       // (prevCr,((isEmpty playedTiles prevCr) && (onBoard st prevCr)));
                         ]
            let newList = Set.fold (fun acc1 (c,b) ->
                if(b) then
                    Set.add (c) acc1
                else
                    acc1
                            ) Set.empty help 
                
            Set.union newList acc
               ) Set.empty playedTiles
    //List.fold (fun acc (x,y) -> MultiSet.add x y acc) st.hand newTiles
    let findNewAnchors (c:coord)(handSize:int)=
        
        let anchors = Set.ofList([for i=0 to handSize do yield! [(prev c true i);(prev c false i)]])
        anchors 
    

    let cCheck (st:State.state) hori acc =
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let totalMap =  Map.ofList (Map.toList acc @ Map.toList st.playedTiles)
        //let anc = findAnchors st acc hori
        if(Map.isEmpty acc) then 
            (Map.add (0,0) (Seq.toList alphabet) Map.empty) 
        else
            List.fold (fun acc2 (coord,_) ->
                let rec getPrevLetters cPos prevLetters =
                    match Map.tryFind (prevCross cPos hori) acc with
                    | Some (id,(char,value)) -> getPrevLetters (prevCross cPos hori) (string char+ prevLetters )
                    | None -> prevLetters
//                    if (isFilled acc (prevCross cPos hori )) then
//                        getPrevLetters (prevCross cPos hori) ((string (fst(snd(Map.find cPos acc)))) + prevLetters)
//                    else
//                        prevLetters
                let prevLetters = getPrevLetters coord ""

                let rec getNextLetters cPos nextLetters =
                    match Map.tryFind (nextCross cPos hori) acc with
                    | Some (id,(char,value)) -> getNextLetters (nextCross cPos hori) (nextLetters+ string char)
                    | None -> nextLetters
//                    if (isFilled acc (nextCross cPos hori)) then
//                        getNextLetters (nextCross cPos hori) (nextLetters+(string (fst(snd(Map.find cPos acc))))) 
//                    else
//                        nextLetters
                        
                let nextLetters = getNextLetters coord ""
              
                Map.add coord (  
                    if (prevLetters.Length = 0) && (nextLetters.Length = 0)
                    then
                        Seq.toList alphabet
                    else
                        List.filter (fun l -> 
                            let completeWord = prevLetters + (string l) + nextLetters
                            match (checkString completeWord st.dict) with
                            | Some (bool,dict) -> bool
                            | None -> false 
                              
                        )(Seq.toList alphabet)
                ) acc2
            ) Map.empty (Map.toList acc)
    let coordCheck (acc: (coord*(uint32*(char*int)) )list ) hori =
        if(hori) then 
            let alt = fst (List.max acc)
            let newList = List.filter (fun x -> if (fst x) = alt then true else false) acc 
            if newList.Length = acc.Length then true else false 
        else
            let alt = snd (List.max acc) 
            let newList = List.filter (fun x -> if (snd x) = alt then true else false) acc 
            if newList.Length = acc.Length then true else false 
    let orderCheck (acc: (coord*(uint32*(char*int)) )list ) hori =
        let newList = List.map fst acc
        let listArray = Array.ofList newList
        let rec aux index =
            if(listArray.Length = 2) then
                if(nextCoord listArray.[index] hori) = listArray.[index+1] then
                    true
                else
                    false
            else 
            if(index = (listArray.Length-1)) then
                true
            elif (nextCoord listArray.[index] hori ) = listArray.[index+1] then
                aux (index+1) 
            else
                false 
        aux 0
    let extendRight (st:State.state) hand pw (dict:bool*Dict) pt coord anchorEmpty hori (acc:(coord*(uint32*(char*int)))list)   =
        let mutable moveList = List.empty
        
        //Right-going aux
        let rec aux (st:State.state) hand1 pw (dict1:bool*Dict) nCoord anchEmpty playedTiles (acc1:(coord*(uint32*(char*int)))list) =
            //Check if the word is actually a word
            let realWord = lookup pw st.dict
            if (Map.tryFind nCoord st.playedTiles).IsNone && (Map.tryFind nCoord playedTiles).IsNone && (not anchEmpty) && realWord && (fst dict1) then   
                let allowedLetters = cCheck st hori (Map.ofList ((Map.toList playedTiles) @ acc1))
                let moveGen = 
                         (List.filter
                            (fun (co, (i, (ch, v))) ->
                                if(onBoard st co) && (not (Map.containsKey co st.playedTiles)) then
                                    match Map.tryFind co allowedLetters with
                                    | Some charList -> (List.contains ch charList) 
                                    | None -> false
                                else
                                    false
                            )acc1)
                let cond = (moveGen.Length = acc1.Length)
                if ((cond) || (Map.isEmpty st.playedTiles))  then
                    moveList <- (acc1,pw) :: moveList
            if (onBoard st nCoord) then 
                match Map.tryFind nCoord st.playedTiles with
                | Some (id,(char,value)) ->
                    match step char (snd dict1) with
                    | Some d3 -> aux st hand1 (pw + (string char)) d3 (nextCoord nCoord hori) false (Map.add nCoord (id,(char,value)) playedTiles) acc1 
                    | None -> ()
                | None -> 
                    match checkString pw (st.dict) with
                    | Some dict2 ->
                        for i in (toList hand1) do
                            let tile = (Map.find i st.tiles)
                            let newHand = remove i 1u hand1
                            for (char,value) in tile do
                                
                                match step char (snd dict2) with
                                | Some (dict3) ->
                                    let change = (nCoord,(i,(char,value)))
                                    
                                    aux st newHand (pw+(string char)) dict3 (nextCoord nCoord hori) false playedTiles (change::acc1)   
                                | None -> ()            
                    | None -> ()
        do aux st hand pw dict coord anchorEmpty pt acc
        List.distinct moveList
    //Something not right here!!!!
                                                
    let leftPart (st: State.state) hand pw aCoord limit hori =
        //List of valid moves, gets changed throughout the course of the function. 
        let mutable moveList = List.Empty
        //aux that finds the left-going moves depending on the anchor.
        let rec aux (st:State.state) hand pw sdict limit acc =
            let newMove = (extendRight st hand pw sdict st.playedTiles aCoord true hori acc) 
            moveList <- newMove@moveList 
            if (limit > 0u) then
                match checkString pw st.dict with
                | Some dict ->
                    for i in (toList hand) do
                        let tile = (Map.find i st.tiles)
                        let newHand = remove i 1u hand
                        for c,v in tile do
                            match step c (snd dict) with
                            | Some d ->
                                let newMap = List.map (fun (c,t) -> (prevCoord c hori,t)) acc
                                //if(fst d )  then moveList <- (acc,pw)::moveList
                                aux st newHand (pw+(string c)  ) d (limit-1u) (((prevCoord aCoord hori ),(i,(c,v)))::newMap)
                            //If no words could be found, end the search
                            | None -> ()
                | None -> ()
            else
                ()
                
        do aux st hand pw (false,st.dict) limit []
        List.distinct moveList
                    
    let findAll (st:State.state) (hori:bool) (c:coord) hand =
//        let anchorTiles =
//            (Set.fold (fun acc dir ->
//            Set.union (Map.fold (fun acc1 co x -> Set.union (Set.ofList[for i=1 to int (size hand) do prev co dir i]) acc1) (findAnchors st st.playedTiles dir) (st.playedTiles)) acc
//            )
//            Set.empty
//            (Set.ofList [true;false;]))
        let anchorTiles = (findAnchors st st.playedTiles hori)
//        let anchorTiles = Map.fold (fun acc1 co x -> Set.union (Set.ofList[for i=1 to int (size hand) do prev co hori i]) acc1) startTiles (st.playedTiles)
       // let roughTiles = Set.union (findNewAnchors c (int (size st.hand))) startTiles
        //let anchorTiles = findNewAnchors c (int (size st.hand))
        //let anchorTiles = Set.filter (fun (c,b) -> if (Map.containsKey (prevCross c b) st.playedTiles) || (not (onBoard st c)) then false else true)roughTiles
        let mutable moveList = List.empty
        
        
        let buildPrePart hori (aCoord:coord) =
            //How many tiles are free to the left
            let rec getMoveLimit moveLimit scanCoord =
                if (Map.tryFind (prevCoord scanCoord hori) st.playedTiles)
                    .IsNone
                    && (moveLimit <= (size st.hand))
                    //&& (not (Set.contains ((prevCoord scanCoord hori)) anchorTiles))
                then
                    getMoveLimit (moveLimit + 1u) (prevCoord scanCoord hori)
                else
                    moveLimit
            
                
            let moveLimit = getMoveLimit 0u aCoord
            leftPart st hand "" c moveLimit hori
             
        //Aux function that scans the position to the left of the current tile.    
        let rec aux (aCoord:coord) (preCoord:coord) pw hori acc =
            
           
            //let totalMap = Map.ofList((Map.toList st.playedTiles)@acc)
//            let buildTiles c limit =
//                let aux limit c acc=
//                    List.fold
//                    (fun acc (co,(id,(char,value))) -> )
//                    if(limit >0)
//                    then
//                aux (size hand) c acc
            //We need to find the starting tile on the board.
            match Map.tryFind preCoord st.playedTiles with
            | Some (id,(c,v)) ->
                //let prev = 
                aux aCoord (prevCoord preCoord hori) (string c + pw) hori ((preCoord,(id,(c,v)))::acc)
            //No more full tiles to the left.
            | None ->
                    match (checkString pw st.dict) with
                    | Some res when (pw.Length>0)-> (extendRight st hand pw res (Map.ofList acc) aCoord true hori [])
                    | _ -> buildPrePart hori aCoord
        
            

        if Map.isEmpty (st.playedTiles) then
            moveList <- extendRight st hand "" (false,st.dict) st.playedTiles st.board.center true hori [] @moveList          
        else 
            let newMove =
                (Set.fold (fun acc1 c ->
                    (aux c (prevCoord c hori) "" hori []))
                []
                anchorTiles)
     
            moveList <- newMove @ moveList
        moveList <- aux c (prevCoord c hori) "" hori [] @ moveList
        List.distinct moveList 
        
                        
          
    let move (st:State.state)=
        if(st.playedTiles.IsEmpty) then
           let rightList = findAll st true st.board.center st.hand
           let wordList = (findAll st false st.board.center st.hand)@rightList
           let finalList = List.filter (fun (move,word) ->
                            match checkString word st.dict with
                            | Some (bool,dict) -> bool
                            | None -> false
                            ) wordList
           let mappedList = List.map (fun (move,word) -> move) finalList
           (SMPlay (List.max mappedList))
        else
            let rightList = Map.fold (fun acc k v -> acc@(findAll st true k st.hand)) List.empty st.playedTiles
            let wordList =  Map.fold (fun acc k v -> acc@(findAll st false k st.hand)) rightList st.playedTiles
            let finalList = List.filter (fun (move,word) ->
                            match checkString word st.dict with
                            | Some (bool,dict) when word.Length>0 -> bool
                            | _ -> false
                            ) wordList
            let mappedList = List.map (fun (move,word) -> (move,(List.length move))) finalList
            if(wordList.IsEmpty) || (fst (List.max mappedList) = []) then (SMChange (toList st.hand))
            else 
                (SMPlay (fst(List.maxBy snd mappedList)))
module Scrabble =
    open System.Threading
    open MoveLogic
   
    let playGame cstream pieces (st : State.state) =
          
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            if st.playerTurn = st.playerNumber then 
                let move = move (State.state st)
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                send cstream (move)
            
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let passTurnState playerId =
                let newTurn = 
                    if playerId = st.numPlayers then
                        1u
                    else
                        st.playerNumber + 1u
                let st' = {st with playerTurn = newTurn}
                st'

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removed = List.fold (fun acc (_,(y,_)) -> removeSingle y acc) st.hand ms
                let added = List.fold (fun acc (x,y) -> addSingle x acc) removed newPieces
                let played = List.fold (fun acc (c,t) -> Map.add c t acc) st.playedTiles ms
                let newTurn = 
                    let rec foundPlayer playerNumber =
                        if(playerNumber = st.numPlayers) then 
                            if (MultiSet.contains 1u st.FF) then
                                foundPlayer 1u
                            else
                                1u 
                        else 
                            if (MultiSet.contains (playerNumber + 1u) st.FF) then
                                foundPlayer st.playerTurn+1u
                            else
                                st.playerTurn + 1u

                    foundPlayer st.playerNumber
                let tileCount = st.tilesLeft - (List.length ms)
                let newScore = Map.add st.playerNumber points st.scores
                let st' = {st with hand =  added; playerTurn = newTurn; playedTiles = played; tilesLeft = tileCount; scores = newScore}
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newTurn = if(pid = st.numPlayers) then 1u else st.playerTurn+1u
                let played = List.fold (fun acc (c,t) -> Map.add c t acc) st.playedTiles ms
                let tileCount = st.tilesLeft - (List.length ms)
                let newScore = Map.add pid points st.scores
                let st' = {st with playerTurn = newTurn; playedTiles = played; tilesLeft = tileCount; scores = newScore}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                aux (passTurnState pid)
            | RCM (CMGameOver _) -> ()
            | RCM (CMForfeit pid) ->
                let st' = {st with FF = MultiSet.addSingle pid st.FF}
                aux st'
            | RCM (CMChange (playerId, numberOfTiles)) -> 
                aux (passTurnState playerId)
            | RCM (CMChangeSuccess newTiles) -> 
                let newHand = List.fold (fun multiSet (tile, count) -> MultiSet.add tile count multiSet) MultiSet.empty newTiles
                let st' = {st with hand = newHand}
                aux st'
            | RCM (CMTimeout playerId) -> 
                aux (passTurnState playerId)
            | RCM (CMPassed playerId) ->
                aux (passTurnState playerId) 
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP
        
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn tiles)
        