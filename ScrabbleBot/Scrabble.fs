namespace itiswhatitis

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad




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
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module internal State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board           : Parser.board
        dict            : ScrabbleUtil.Dictionary.Dict
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
module MoveLogic =
    open Dictionary
                            //if(fst(d)) then aux (d) hand c2 cWord bWord                
//                        | Some d ->  MultiSet.fold (fun acc x y ->
//                                        match Dictionary.step (fst(Set.minElement(Map.find x (st.tiles)))) (snd(d)) with
//                                        | Some d ->                                           
//                                            let current =
//                                                if hori then
//                                                    (cWord@[((fst(c2)+1,snd(c2)),(x,(Set.minElement(Map.find x st.tiles))))])
//                                                else
//                                                    (cWord@[((fst(c2),snd(c2)-1),(x,(Set.minElement(Map.find x st.tiles))))])
//                                                                                                
//                                            if(fst(d)) then
//                                                aux d (MultiSet.removeSingle x hand) c2 current cWord
//                                            else
//                                                aux d (MultiSet.removeSingle x hand) c2 current bWord
//                                        
//                                        | None -> bWord
//                                        ) cWord hand
    let getLetter (st:State.state) (id:uint32) =
       fst (Set.minElement(Map.find id st.tiles))
    let nextCoord (c:coord) (hori:bool) =
        if hori then
            ((fst c)+1,(snd c))
        else
            ((fst c),(snd c)+1)
    let longestWord (word1: (coord * (uint32 * (char * int))) list) (word2: (coord * (uint32 * (char * int))) list) =
        if(word1.Length>word2.Length) then word1
        else word2 
    let moveGen (st:State.state) (c:coord) (hori:bool) =
        let rec aux (dict:(bool*Dict)) (hand:MultiSet<uint32>) (c2:coord) (currentWord: (coord * (uint32 * (char * int))) list) (bestWord: (coord * (uint32 * (char * int))) list)  =
            match Map.tryFind c2 st.playedTiles with
            | Some c ->
                match step (fst (snd c)) (snd dict) with
                | Some d -> aux d hand (nextCoord c2 hori) currentWord bestWord       
                | None -> currentWord
            | None ->
                    fold (fun acc tileId tileCount ->
                    match step (fst(Set.minElement(Map.find tileId (st.tiles)))) (snd(dict)) with
                    | Some d ->
                        let current = ((c2,(tileId,(Set.minElement(Map.find tileId st.tiles))))::acc)
                        if(fst(d)) then
                            printf "Current bestword %A \n" (longestWord current bestWord)
                            aux d (removeSingle tileId hand) (nextCoord c2 hori) current (longestWord current bestWord)
                        else
                            aux d (removeSingle tileId hand) (nextCoord c2 hori) current bestWord
                    | None ->
                        //printf "Current list of letters: %A \n" (current)
                        //printf "Acc: %A \n" (acc) 
                        
                        //aux (false,st.dict) st.hand c2 List.empty bestWord 
                        acc
                    )bestWord hand
                    
                    //printf "Best move %A" (bestWord)
           
        aux (false,st.dict) st.hand c List.empty List.empty //[((0,0),(20u,('T',1)));((1,0),(15u,('O',1)));]
    let move (st:State.state)=
        if(Map.isEmpty st.playedTiles) then
           //printf "State : %A \n" st
           let wordList = moveGen st st.board.center true
           //printf "Move generated: %A \n" (wordList) 
           wordList
        else
            Map.fold (fun acc k v -> moveGen st k true) List.empty st.playedTiles
            
        
//    let handContain (st:State.state) c =
//        match List.tryFindBack (fun x -> (fst(Set.minElement(Map.find x st.tiles))) = c) (MultiSet.toList st.hand) with
//        | Some s -> true
//        | None -> false
//    let isWord c d =
//        match step c d with
//        | Some S -> true
//        | None -> false
//    let PlayMove = failwith "Not Implemented"
//    let SaveMove (co:coord) (ch:char) (id:uint32) (p:int) =
//        (string (fst(co))) + " " + (string (snd(co))) + " " + (string id) + (string ch) + (string p)
//    
//    let rec rightSide (st:State.state) (pw:string) (dic:(bool*Dict)) (coord:coord) (currentMove: (coord * (uint32 * (char * int))) list)=
//        if not (Map.containsKey coord st.playedTiles) then
//            if (fst(dic)) then
//                currentMove
//            else     
//                 for letter in (MultiSet.toList st.hand) do
//                    let cLetter = getLetter st letter
//                    if(isWord (cLetter) (snd(dic))) then
//                        rightSide st (pw+(string cLetter)) (defaultArg(Dictionary.step cLetter (snd(dic))) dic) (fst(coord)+1,snd(coord)) (currentMove+(SaveMove coord cLetter letter (snd (Set.minElement(Map.find letter st.tiles)))))
//        else
//            rightSide st (pw+(string (fst(Map.find coord st.playedTiles)))) (defaultArg(Dictionary.step (fst(Map.find coord st.playedTiles)) (snd(dic))) dic) (fst(coord)+1,snd(coord)) currentMove
//    let rec lPartial (st:State.state) (pw:string)  (dict:(bool*Dict)) (c:coord) limit =
//        rightSide st pw dict (fst(Seq.head(Map.toSeq st.playedTiles)))
//        if limit > 0 then
//            fold (fun acc tileId tileCount ->
//                    match step (fst(Set.minElement(Map.find tileId (st.tiles)))) (snd(dict)) with
//                    | Some d ->
//                        let current = ((c,(tileId,(Set.minElement(Map.find tileId st.tiles))))::acc)
//                        current
//                    | None ->
//                        //printf "Current list of letters: %A \n" (current)
//                        //printf "Acc: %A \n" (acc) 
//                        
//                        //aux (false,st.dict) st.hand c2 List.empty bestWord 
//                        acc
//                    ) bestWord hand 
//        lPartial st "" (false,st.dict)

module WebMove =
    open Dictionary
    let onBoard (st:State.state) (c:coord) =
        match st.board.squares c with
        |Some s -> true
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
    let checkString (pw:string) (dict:(bool*Dict)) =
        //Splits the string up into a char array, and loops through it to update our current dictionary
        let rec split (dict:(bool*Dict))=
            function
            | [] -> Some dict
            | c1 :: c2 ->
                match step c1 (snd(dict)) with
                // if chars remain in the word and it matches a word in our dict, continue.
                | Some s -> split s c2
                //if no words that match in dictionary, return none
                | None -> None 
        split dict (Seq.toList pw)
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
    let findAnchors (playedTiles : Map<coord,uint32*(char*int)>) (hori:bool)=
        if(Map.isEmpty playedTiles) then
            [(0,0);]
        else        
        
        Map.fold (fun acc x y ->
            if (isEmpty playedTiles (nextCoord x hori))
               || (isEmpty playedTiles (prevCoord x hori))
               || (isEmpty playedTiles (nextCross x hori))
               || (isEmpty playedTiles (prevCross x hori))
               then
                   x::acc
               else
                   acc
               ) List.empty playedTiles
    //List.fold (fun acc (x,y) -> MultiSet.add x y acc) st.hand newTiles
    let cCheck (st:State.state) hori acc =
        let anchors = findAnchors st.playedTiles hori
        let mutable newAcc = acc
        
        for coord in anchors do
            let mutable prevLetters = ""
            let mutable cPos = coord
            while (isFilled st.playedTiles (prevCross cPos hori )) do
                cPos <- prevCross cPos hori 
                prevLetters <-(string (fst(snd(Map.find cPos st.playedTiles)))) + prevLetters
            let mutable nextLetters = ""
            let mutable cPos = coord
            while (isFilled st.playedTiles (nextCross cPos hori)) do
                cPos <- nextCross cPos hori 
                nextLetters <-nextLetters+(string (fst(snd(Map.find cPos st.playedTiles))))  
            
            newAcc <- Map.add coord (  
                if (prevLetters.Length = 0) && (nextLetters.Length = 0)
                then
                    Seq.toList "abcdefghijklmnopqrstuvwxyz"
                else
                    let mutable letterList = List.empty
                    for l in (Seq.toList "abcdefghijklmnopqrstuvwxyz") do
                        let completeWord = prevLetters +  (string l) + nextLetters
                        if (Dictionary.lookup completeWord st.dict) then
                            letterList <- (l::letterList)
                   
                    letterList
                ) newAcc
        newAcc
  
  
    let rec extendRight (st:State.state) hand pw dict nCoord anchorEmpty hori (acc: (coord*(uint32*(char*int)) )list ) :(coord*(uint32*(char*int)) )list list  =
        let mutable moveList = List.empty
        let mutable adj = Map.empty
        let actualHand = fold (fun acc x y -> (x,(Map.find x st.tiles))::acc) List.empty hand
        //Right-going aux
        let rec aux (st:State.state) hand pw dict nCoord anchors playedTiles (acc1: (coord*(uint32*(char*int)) )list ) =
       
            //Check if the word is actually a word
            if (lookup pw dict) && (Map.tryFind nCoord st.playedTiles).IsNone && (not anchorEmpty) then
                let allowedLetters = cCheck st hori playedTiles
                
                let moveGen =
                         (List.filter
                            (fun (co, (i, (ch, v))) ->
                                if(onBoard st co) then
                                    match Map.tryFind co allowedLetters with
                                    | Some cl -> (List.contains ch cl)
                                    | None -> false
                                else
                                    false
                            )acc1)
                //if(moveGen.Length = moveList.Length) then
                let adjTiles = Map.ofList ((Map.toList playedTiles)@(Map.toList allowedLetters))
                adj <- adjTiles
                moveList <- moveGen :: moveList
            if (onBoard st nCoord) then 
                match Map.tryFind nCoord st.playedTiles with
                | Some c -> ()
                | None -> 
                    match checkString pw (false,st.dict) with
                    | Some dict ->
                        
                        for (i,t) in actualHand do
                            for (char,int) in t do
                                let newHand = (MultiSet.removeSingle i hand)
                                match step char (snd(dict)) with
                                | Some (b,d) -> aux st newHand (pw+(string char)) d (nextCoord nCoord hori) Map.empty adj ((nCoord,(i,(char,int)))::acc1)
                                | None -> ()
                                
                    | None -> ()
        do aux st hand pw dict nCoord Map.empty adj acc
        moveList
        
    
                                
                                
                        
                    
            
    let leftPart (st: State.state) hand pw aCoord limit hori =
        //List of valid moves, gets changed throughout the course of the function. 
        let mutable moveList = List.empty
        let actualHand = fold (fun acc x y -> (x,(Map.find x st.tiles))::acc) List.empty hand
        //aux that finds the left-going moves depending on the anchor.
        let rec aux (st:State.state) hand pw limit (acc:(coord*(uint32*(char*int))) list ) =
            moveList <- (extendRight st hand pw st.dict aCoord false hori acc) @ moveList
            if limit > 0u then
                match checkString pw (false,st.dict) with
                | Some l1 ->
                    for i,t in actualHand do
                        for c,v in t do
                        match step c (snd l1) with
                        | Some _ -> aux st (removeSingle i hand) (pw + string c) (limit-1u) (((prevCoord aCoord hori),(i,(c,v)))::((List.map (fun (c,t) -> (prevCoord c hori,t))) acc))
                        
                        //If no words could be found, end the search
                        | None -> ()
                | None -> ()
            else ()
        aux st hand pw limit []
        moveList
                    
    let findAll (st:State.state) (hori:bool) (c:coord) hand =
        let anchorTiles = (findAnchors st.playedTiles hori)
          
            
        let mutable moveList = List.empty
        
        let buildPrePart hori (aCoord:coord) =
            //How many tiles are free to the left
            let mutable moveLimit = 0u
            
            //Position currently scanned, default is the anchorpos
            let mutable scanCoord = aCoord
            
            while (Map.tryFind (prevCoord scanCoord hori) st.playedTiles)
                      .IsNone
                      && (moveLimit <= (size st.hand)*2u)
                      && (not (List.contains (prevCoord scanCoord hori) anchorTiles))
                      do
                          moveLimit <- moveLimit + 1u
                          scanCoord <- prevCoord scanCoord hori
            leftPart st hand "" c moveLimit hori
        //Aux function that scans the position to the left of the current tile.    
        let rec aux (aCoord:coord) (preCoord:coord) pw hori acc =
            //We need to find the starting tile on the board.
            match Map.tryFind preCoord st.playedTiles with
            | Some (id,(c,v)) ->
                let prev = (prevCoord aCoord hori)
                aux aCoord prev (string c + pw) hori ((prev,(id,(c,v)))::acc)
            //No more full tiles to the left.
            | None ->
                if((checkString pw (false,st.dict)).IsNone) && pw.Length > 0 then
                    extendRight st hand pw st.dict aCoord false hori acc
                else
                    buildPrePart hori aCoord
        if List.isEmpty anchorTiles then
            printf "MoveList %A" (moveList)
            moveList <- extendRight st hand  "" st.dict st.board.center false hori []
            
        else
            for hori in [true;false] do
                for aCoord in anchorTiles do
                    let newMove = aux aCoord (prevCoord aCoord hori) "" hori []
                    
                    //Extra check to make sure that no duplicates are inserted.
                    let dMoves = List.filter (fun m -> not (List.contains m moveList)) newMove
                    moveList <- dMoves @ moveList
        moveList
                        
                
//        let rec aux (playedTiles : Map<coord,(char*int)>) hand cWord (c0:coord) =
//            
//            
//            
//            List.fold (fun acc x ->
//                if (isFilled playedTiles (prevCoord x hori)) then
//                    let scanCord = (prevCoord x hori) 
//                    let pw = cWord + (string (Map.find scanCord playedTiles))
//                   while loopcond do
//                        scanCord <- prevCoord scanCord hori
//                        pw <- (string (fst(Map.find scanCord st.playedTiles))) + pw
//                       loopcond <- not (isEmpty scanCord hori) 
//                    match Dictionary.step  dict
//                    if(wordSearch) then aux playedTiles hand pw (prevCoord scanCord hori) else acc
//                else
//                    acc
//                    ) List.empty anchors
//        aux st.playedTiles st.hand "" c 
//    let max ((l:(coord*(int32*(char*int))) list ) (l2:(coord*(int32*(char*int))) list list)) =
//        let aux l1 l2 =
//            
//        if l1.Length > l2.Length then
//            l1
//        else
//            l2
    let moveh (st:State.state)=
        if(Map.isEmpty st.playedTiles) then
           //printf "State : %A \n" st
           let rightList = findAll st true st.board.center st.hand
           let wordList = (findAll st false st.board.center st.hand)@rightList
           //printf "Move generated: %A \n" (wordList) 
           List.max wordList
        else
            let rightList = Map.fold (fun acc k v -> findAll st true k st.hand) List.empty st.playedTiles
            let finalList = (Map.fold (fun acc k v -> findAll st false k st.hand) List.empty st.playedTiles)@rightList
            List.max finalList
            
            
            
//    let rec crossCheck (st:State.state)=
//        Map.fold (fun acc x y ->
//            x ) st.dict st.playedTiles
        
        
module Scrabble =
    open System.Threading
    open MoveLogic
    open WebMove
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
           
            //let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input
            let m = moveh (State.state st)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) m) // keep the debug lines. They are useful.
            send cstream (SMPlay m)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) m) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removed = List.fold (fun acc (_,(y,_)) -> MultiSet.removeSingle y acc) st.hand ms
                let added = List.fold (fun acc (x,y) -> MultiSet.addSingle x acc) removed newPieces
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
                
                let newTurn = if(pid = st.numPlayers) then 1u else st.playerTurn+1u
                let st' = {st with playerTurn = newTurn}
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMForfeit pid) ->
                let st' = {st with FF = MultiSet.addSingle pid st.FF}
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
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
        