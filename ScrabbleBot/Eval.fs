// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1);] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= fun x-> b >>= fun y -> ret (x+y)
    let sub a b = a >>= fun x-> b >>= fun y -> ret (x-y)
    let mul a b = a >>= fun x-> b >>= fun y -> ret (x*y)       
    let div a b = a >>= fun x-> b >>= fun y -> if y <> 0 then  ret (x/y) else fail DivisionByZero     

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N a -> ret a
        | V v -> lookup v
        | WL -> wordLength
        | PV a -> arithEval a >>= fun x -> pointValue x
        | Add (a1,a2) -> add (arithEval a1) (arithEval a2)
        | Sub (s1,s2) -> sub (arithEval s1) (arithEval s2)
        | Mul (m1,m2) -> mul (arithEval m1) (arithEval m2)
        | Div (d1,d2) -> div (arithEval d1) (arithEval d2)
        | Mod (mod1,mod2) -> 
                arithEval mod1 >>= fun x -> 
                arithEval mod2 >>= fun y -> 
                if y <> 0 then ret (x%y) else fail DivisionByZero
        | CharToInt c -> charEval c >>= fun ci -> ret (int ci) 
    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | ToUpper c -> charEval c >>= fun tu -> ret (System.Char.ToUpper (tu))
        | ToLower c -> charEval c >>= fun tl -> ret (System.Char.ToLower (tl))
        | CV c -> arithEval c >>= fun cv -> characterValue cv
        | IntToChar c -> arithEval c >>= fun ic -> ret (ic |> char)      

    let rec boolEval b : SM<bool> = 
      match b with
      | TT -> ret true
      | FF -> ret false
      | AEq (x,y) -> arithEval x >>= fun c1 -> arithEval y >>= fun c2 -> ret (c1=c2)
      | ALt (x,y) -> arithEval x >>= fun c1 -> arithEval y >>= fun c2 -> ret (c1<c2)
      | Not b -> boolEval b >>= fun n -> ret (not n)
      | Conj (a,b) -> boolEval a >>= fun x -> boolEval b >>= fun y -> ret (x&&y)
      | IsLetter c -> charEval c >>= fun x -> ret(System.Char.IsLetter(x))
      | IsDigit c -> charEval c >>= fun x -> ret(System.Char.IsDigit(x))

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stm : SM<unit> = 
      match stm with
      | Skip -> ret()
      | Declare d -> declare d
      | Ass(x,a) -> arithEval a >>= fun y -> update x y
      | Seq(stm1,stm2) -> stmntEval stm1 >>>= stmntEval stm2
      | ITE(guard,stm1,stm2) -> boolEval guard >>= fun y -> if(y) then push >>>= stmntEval stm1 >>>= pop  else push >>>= stmntEval stm2 >>>= pop
      | While(guard, stm) -> boolEval guard >>= fun bool -> 
        if(bool) then 
            push >>>= stmntEval stm >>= fun evl -> stmntEval (While(guard,stm))>>>=pop 
           else 
            push >>>= ret () >>>= pop

(* Part 3 (Optional) *)

    // type StateBuilder() =

    //     member this.Bind(f, x)    = f >>= x
    //     member this.Return(x)     = ret x
    //     member this.ReturnFrom(x) = x
    //     member this.Delay(f)      = f ()
    //     member this.Combine(a, b) = a >>= (fun _ -> b)
        
    // let prog = new StateBuilder()

    // let arithEval2 a = failwith "Not implemented"
    // let charEval2 c = failwith "Not implemented"
    // let rec boolEval2 b = failwith "Not implemented"

    // let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> int // This has changed from Assignment 6

    // Refactor your implementation from Assignment 6 to remove the Result type and return 0 on failure.
    // Details are in the assignment.
    let stmntToSquareFun stm =          
        fun (w: word) (pos: int) (acc: int) ->              
        mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]             
        |> fun s -> evalSM s (stmntEval stm >>>= lookup "_result_")
        |> function 
                | Success a -> a
                | Failure e -> 0

    type coord = int * int

    type boardFun = coord -> squareFun option

    // Refactor your implementation from Assignment 6 to remove the Result type and return None on failure.
    // Also, make sure the funciton is polymorphic on the return type and the lookup map.
    // Details in the assignment.
    let stmntToBoardFun stm squares = 
        fun f ->
            mkState [("_x_"),fst f;("_y_",snd f);("_result_",0)] [] ["_x_";"_y_";"_result"]
            |> fun s -> evalSM s (stmntEval stm >>>= lookup "_result_")
            |> function 
                | Success a -> (Map.tryFind a squares)
                | Failure e -> None