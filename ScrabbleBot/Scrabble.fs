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
        playedTiles     : Map<coord,(char*int)>
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
            ((fst c),(snd c)-1)
    let longestWord (word1: (coord * (uint32 * (char * int))) list) (word2: (coord * (uint32 * (char * int))) list) =
        if(word1.Length>word2.Length) then word1
        else word2 
    let moveGen (st:State.state) (c:coord) (hori:bool) =
        let rec aux (dict:(bool*Dict)) (hand:MultiSet<uint32>) (c2:coord) (currentWord: (coord * (uint32 * (char * int))) list) (bestWord: (coord * (uint32 * (char * int))) list)  =
            match Map.tryFind c2 st.playedTiles with
            | Some c ->
                match step (fst c) (snd dict) with
                | Some d -> aux d hand (nextCoord c2 hori) currentWord bestWord       
                | None -> bestWord
            | None ->
                    fold (fun acc tileId tileCount ->
                    match step (fst(Set.minElement(Map.find tileId (st.tiles)))) (snd(dict)) with
                    | Some d ->
                        let current = ((c2,(tileId,(Set.minElement(Map.find tileId st.tiles))))::acc)
                        if(fst(d)) then
                            aux d (removeSingle tileId hand) (nextCoord c2 hori) current (longestWord current bestWord)
                        else
                            aux d (removeSingle tileId hand) (nextCoord c2 hori) current bestWord
                    | None ->
                        //printf "Current list of letters: %A \n" (current)
                        //printf "Acc: %A \n" (acc) 
                        //printf "Current bestmove: %A \n" (bestWord)
                        //aux (false,st.dict) st.hand c2 List.empty bestWord 
                        acc
                    ) bestWord hand
                    //printf "Best move %A" (bestWord)
           
        aux (false,st.dict) st.hand c List.empty List.empty //[((0,0),(20u,('T',1)));((1,0),(15u,('O',1)));]
    let move (st:State.state)=
        if(Map.isEmpty st.playedTiles) then
           // printf "State : %A \n" st
            printf "Move generated: %A \n" (moveGen st (0,0) true) 
            moveGen st st.board.center true
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
//    let rec lPartial (st:State.state) (pw:string)  (dic:(bool*Dict)) limit =
//        rightSide st pw dic (fst(Seq.head(Map.toSeq st.playedTiles)))
//        if limit > 0 then
//            for letter in (MultiSet.toList st.hand) do
//                let cLetter = getLetter st letter
//                if(isWord (cLetter) (snd(dic))) then
//                    lPartial st (pw+(string cLetter)) (defaultArg(Dictionary.step cLetter (snd(dic))) dic) (limit-1) 
//        lPartial st "" (false,st.dict)


module Scrabble =
    open System.Threading
    open MoveLogic
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =

            let passTurnState playerId =
                let newTurn = 
                    if playerId = st.numPlayers then
                        1u
                    else
                        playerId + 1u
                let st' = {st with playerTurn = newTurn}
                st'

            Print.printHand pieces (State.hand st)
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input = MoveLogic.lPartial
            //let input =  System.Console.ReadLine()
            //let move = RegEx.parseMove input
            
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) (move (State.state st))) // keep the debug lines. They are useful.
            send cstream (SMPlay (MoveLogic.move st))

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) (move (State.state st))) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let removed = List.fold (fun acc (_,(y,_)) -> MultiSet.removeSingle y acc) st.hand ms
                let added = List.fold (fun acc (x,y) -> MultiSet.addSingle x acc) removed newPieces
                let played = List.fold (fun acc (x,(_,y)) -> Map.add x y acc) st.playedTiles ms
//                let newTurn = 
//                    let rec foundPlayer playerNumber =
//                        if(playerNumber = st.numPlayers) then 
//                            if (MultiSet.contains 1u st.FF) then
//                                foundPlayer 1u
//                            else
//                                1u 
//                        else 
//                            if (MultiSet.contains (playerNumber + 1u) st.FF) then
//                                foundPlayer st.playerTurn+1u
//                            else
//                                st.playerTurn + 1u
//
//                    foundPlayer st.playerNumber
                let tileCount = st.tilesLeft - (List.length ms)
                let newScore = Map.add st.playerNumber points st.scores
                let st' = {st with hand =  added;  playedTiles = played; tilesLeft = tileCount; scores = newScore}
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newTurn = if(pid = st.numPlayers) then 1u else st.playerTurn+1u
                let played = List.fold (fun acc (x,(_,y)) -> Map.add x y acc) st.playedTiles ms
                let tileCount = st.tilesLeft - (List.length ms)
                let newScore = Map.add pid points st.scores
                let st' = {st with playerTurn = newTurn; playedTiles = played; tilesLeft = tileCount; scores = newScore}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                aux (passTurnState pid)
            | RCM (CMGameOver _) -> ()
            | RCM (CMForfeit pid) ->
            // TODO should turn be updated here?
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
        