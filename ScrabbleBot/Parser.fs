// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    // Example parser combinator library. Use for CodeJudge.
    //open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"
    
    // Assignment 7.2
   
    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter        = satisfy System.Char.IsLetter
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    // Assignment 7.3

    let (.>*>.) a b = a .>> spaces .>>. b
    let (.>*>) a b  = a.>> spaces .>> b
    let (>*>.) a b  = a>>. spaces >>. b

    // Assignment 7.4

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let tparenthesise p = pchar '{' >*>. p .>*> pchar '}'
    let apostro p = pchar ''' >>. p .>> pchar '''

    // Assignment 7.5
    
    let punder = pchar '_'
    let implode = fun (cs:char list) -> List.foldBack (fun c s ->(string c)+s) cs ""
    
    let pid = (pletter <|> punder) .>>. many (punder <|> palphanumeric) |>> (fun (x,y) -> x::y) |>> implode 
    
    // Assignment 7.6

    let unop op a = op >*>. a
    
    // Assignment 7.7

    let binop op a b = a .>*> op .>*>. b  

    // Assignment 7.8

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse;]
    
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse;]
    let CTIParse = unop pCharToInt (parenthesise CharParse) |>> CharToInt <?> "CharToInt"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "String"
    let pvParse  = pPointValue>*>. parenthesise TermParse  |>> PV <?> "pointValue"
    let WLParse  = pstring "WL" >*>. parenthesise TermParse |>> (fun _  -> WL) <?> "wordLength"
    let NegParse = pchar '-' >*>. pint32 |>> (fun x -> Mul ((N -1),(N x))) <?> "-"
    let ParParse = parenthesise TermParse
    do aref := choice [NegParse;pvParse; NParse; CTIParse;WLParse;ParParse;VParse;]

    let AexpParse = TermParse 

    // Assignment 7.9
    let CexpParse = CharParse

    let CParse = apostro anyChar |>> C
    let CVParse = pCharValue >*>. parenthesise TermParse|>> CV <?> "charValue"
    let TUParse = pToUpper >*>. parenthesise CharParse   |>> ToUpper <?> "toUpper"
    let TLParse = pToLower >*>. parenthesise CharParse   |>> ToLower <?> "toLower"
    let ITCParse = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    let APParse = apostro CharParse
    do cref := choice [TUParse;TLParse;ITCParse;CVParse;CParse;]

    

    // Assignment 7.10

    let xParse, xref = createParserForwardedToRef<bExp>()
    let yParse, yref = createParserForwardedToRef<bExp>()
    let zParse, zref = createParserForwardedToRef<bExp>()
    
    let conParse = binop (pstring @"/\") yParse xParse |>> (fun (x,y) -> x.&&.y) <?> @"/\"
    let disParse = binop (pstring @"\/") yParse xParse |>> (fun (x,y) -> x.||.y) <?> @"\/" 
    
    do xref := choice [conParse;disParse;yParse;]
    
    let equalParse = binop (pchar '=') AtomParse AtomParse |>> (fun (x,y) -> x.=.y) <?> "="    
    let nequalParse = binop (pstring "<>") AtomParse AtomParse |>> (fun (x,y) -> x.<>.y) <?> "<>"    
    let lesserParse = binop (pchar '<') AtomParse AtomParse |>> (fun (x,y) -> x.<.y) <?> "<"    
    let greaterParse = binop (pchar '>') AtomParse AtomParse |>> (fun (x,y) -> x.>.y) <?> ">"    
    let lesserOrEqualParse = binop (pstring "<=") AtomParse AtomParse |>> (fun (x,y) -> x.<=.y) <?> "<="    
    let greaterOrEqualParse = binop (pstring ">=") AtomParse AtomParse |>> (fun (x,y) -> x.>=.y) <?> ">="

    do yref := choice [equalParse;nequalParse;lesserParse;lesserOrEqualParse;greaterParse;greaterOrEqualParse;zParse]
    
    let notParse = pchar '~' >>. xParse |>> Not <?> "~"    
    let isLetterParse = pIsLetter >*>. CexpParse |>> IsLetter <?> "isLetter"    
    let isDigitParse = pIsDigit >*>. CexpParse |>> IsDigit <?> "isDigit"    
    let tParse = pTrue |>> (fun _-> TT) <?> "TT"    
    let fParse = pFalse |>> (fun _ -> FF) <?> "FF"
    
    let parenParse = parenthesise xParse  
    
    do zref := choice [tParse;fParse;notParse;isLetterParse;isDigitParse;parenParse;]

    let BexpParse = xParse
    // Assignment 7.11
    
    let stmParse,sref = createParserForwardedToRef<stm>()
    let stateParse,stateref = createParserForwardedToRef<stm>()

    let seqParse = stateParse .>*> pchar ';' .>*>. stmParse|>> (fun (x,y) -> Seq(x,y)) <?> "Seq"
    
    do sref := choice [seqParse;stateParse;]
    
    let declareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let assParse = binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
    let ITEParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. tparenthesise stmParse .>*> pelse .>*>. tparenthesise stmParse |>> (fun ((x,y),z) -> ITE(x,y,z)) <?> "IfThenElse"
    let ITParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. tparenthesise stmParse |>> (fun (a, b) -> ITE (a, b, Skip)) <?> "IfThen"
    let SkipParse = pstring "Skip" |>> (fun _ -> Skip) <?> "Skip"
    let WhileParse = pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. tparenthesise stmParse |>> (fun (a, b) -> While (a, b)) <?> "While"
    
    do stateref := choice [assParse;declareParse;SkipParse;ITEParse;ITParse;WhileParse;]
    
    let stmntParse = stmParse

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun sqp = Map.map (fun x y -> stmntToSquareFun (getSuccess(run stmntParse y))) sqp

    let parseBoardFun s m = stmntToBoardFun (getSuccess(run stmntParse s)) m

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) : board =
        let m = Map.map (fun x y -> parseSquareFun y) bp.squares;
        {
            center = bp.center;
            defaultSquare = Map.find bp.usedSquare m ;
            squares = parseBoardFun bp.prog m;
        }