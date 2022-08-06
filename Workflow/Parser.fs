module internal rec Workflow.Parser

(*
STATES
   0 { BinaryOperator -> ·TMinus [] | ValueExpression -> ValueExpressionBinOp· [TComma TParenClose] | ValueExpressionBinOp -> ValueExpressionBinOp· BinaryOperator ValueExpressionId [] }
   1 { BinaryOperator -> TMinus· [TId] }
   2 { Document -> ·States TBreak Initial TBreak Transitions [] | States -> ·TStates TColon TBlockOpen StateDefinitionList TBlockClose [] }
   3 { Document -> States· TBreak Initial TBreak Transitions [] }
   4 { Document -> States TBreak· Initial TBreak Transitions [] | Initial -> ·TInitial TColon TBlockOpen StateConstruction TBlockClose [] }
   5 { Document -> States TBreak Initial· TBreak Transitions [] }
   6 { Document -> States TBreak Initial TBreak· Transitions [] | Transitions -> ·TTransitions TColon TBlockOpen TransitionList TBlockClose [] }
   7 { Document -> States TBreak Initial TBreak Transitions· [$] }
   8 { Initial -> TInitial· TColon TBlockOpen StateConstruction TBlockClose [] }
   9 { Initial -> TInitial TColon· TBlockOpen StateConstruction TBlockClose [] }
   10 { Initial -> TInitial TColon TBlockOpen· StateConstruction TBlockClose [] | StateConstruction -> ·TId StateConstructionArguments [] }
   11 { Initial -> TInitial TColon TBlockOpen StateConstruction· TBlockClose [] }
   12 { Initial -> TInitial TColon TBlockOpen StateConstruction TBlockClose· [TBreak] }
   13 { StateConstruction -> ·TId StateConstructionArguments [] | TransitionCase -> StatePattern TArrow· StateConstruction [] }
   14 { StateConstruction -> TId· StateConstructionArguments [] | StateConstructionArguments -> · [TBlockClose TBreak] | StateConstructionArguments -> ·TParenOpen StateConstructionArgumentList TParenClose [] }
   15 { StateConstruction -> TId StateConstructionArguments· [TBlockClose TBreak] }
   16 { StateConstructionArgumentList -> ·StateConstructionArgumentList TComma ValueExpression [] | StateConstructionArgumentList -> ·ValueExpression [] | StateConstructionArguments -> TParenOpen· StateConstructionArgumentList TParenClose [] | ValueExpression -> ·ValueExpressionBinOp [] | ValueExpressionBinOp -> ·ValueExpressionBinOp BinaryOperator ValueExpressionId [] | ValueExpressionBinOp -> ·ValueExpressionId [] | ValueExpressionId -> ·TId [] }
   17 { StateConstructionArgumentList -> StateConstructionArgumentList· TComma ValueExpression [] | StateConstructionArguments -> TParenOpen StateConstructionArgumentList· TParenClose [] }
   18 { StateConstructionArgumentList -> StateConstructionArgumentList TComma· ValueExpression [] | ValueExpression -> ·ValueExpressionBinOp [] | ValueExpressionBinOp -> ·ValueExpressionBinOp BinaryOperator ValueExpressionId [] | ValueExpressionBinOp -> ·ValueExpressionId [] | ValueExpressionId -> ·TId [] }
   19 { StateConstructionArgumentList -> StateConstructionArgumentList TComma ValueExpression· [TComma TParenClose] }
   20 { StateConstructionArgumentList -> ValueExpression· [TComma TParenClose] }
   21 { StateConstructionArguments -> TParenOpen StateConstructionArgumentList TParenClose· [TBlockClose TBreak] }
   22 { StateDefinition -> ·TId StateDefinitionParameters [] | StateDefinitionList -> ·StateDefinition [] | StateDefinitionList -> ·StateDefinitionList TBreak StateDefinition [] | States -> TStates TColon TBlockOpen· StateDefinitionList TBlockClose [] }
   23 { StateDefinition -> ·TId StateDefinitionParameters [] | StateDefinitionList -> StateDefinitionList TBreak· StateDefinition [] }
   24 { StateDefinition -> TId· StateDefinitionParameters [] | StateDefinitionParameters -> · [TBlockClose TBreak] | StateDefinitionParameters -> ·TParenOpen StateDefinitionParameterList TParenClose [] }
   25 { StateDefinition -> TId StateDefinitionParameters· [TBlockClose TBreak] }
   26 { StateDefinitionList -> StateDefinition· [TBlockClose TBreak] }
   27 { StateDefinitionList -> StateDefinitionList· TBreak StateDefinition [] | States -> TStates TColon TBlockOpen StateDefinitionList· TBlockClose [] }
   28 { StateDefinitionList -> StateDefinitionList TBreak StateDefinition· [TBlockClose TBreak] }
   29 { StateDefinitionParameterList -> ·StateDefinitionParameterList TComma TId [] | StateDefinitionParameterList -> ·TId [] | StateDefinitionParameters -> TParenOpen· StateDefinitionParameterList TParenClose [] }
   30 { StateDefinitionParameterList -> StateDefinitionParameterList· TComma TId [] | StateDefinitionParameters -> TParenOpen StateDefinitionParameterList· TParenClose [] }
   31 { StateDefinitionParameterList -> StateDefinitionParameterList TComma· TId [] }
   32 { StateDefinitionParameterList -> StateDefinitionParameterList TComma TId· [TComma TParenClose] }
   33 { StateDefinitionParameterList -> TId· [TComma TParenClose] }
   34 { StateDefinitionParameters -> TParenOpen StateDefinitionParameterList TParenClose· [TBlockClose TBreak] }
   35 { StatePattern -> ·TId StatePatternArguments [] | Transition -> TOn TQuotedString TBlockOpen· TransitionCaseList TBlockClose [] | TransitionCase -> ·StatePattern TArrow StateConstruction [] | TransitionCaseList -> ·TransitionCase [] | TransitionCaseList -> ·TransitionCaseList TBreak TransitionCase [] }
   36 { StatePattern -> ·TId StatePatternArguments [] | TransitionCase -> ·StatePattern TArrow StateConstruction [] | TransitionCaseList -> TransitionCaseList TBreak· TransitionCase [] }
   37 { StatePattern -> TId· StatePatternArguments [] | StatePatternArguments -> · [TArrow] | StatePatternArguments -> ·TParenOpen StatePatternArgumentList TParenClose [] }
   38 { StatePattern -> TId StatePatternArguments· [TArrow] }
   39 { StatePatternArgument -> ·TAsterisk TId [] | StatePatternArgument -> ·TId [] | StatePatternArgument -> ·TUnderscore [] | StatePatternArgumentList -> ·StatePatternArgument [] | StatePatternArgumentList -> ·StatePatternArgumentList TComma StatePatternArgument [] | StatePatternArguments -> TParenOpen· StatePatternArgumentList TParenClose [] }
   40 { StatePatternArgument -> ·TAsterisk TId [] | StatePatternArgument -> ·TId [] | StatePatternArgument -> ·TUnderscore [] | StatePatternArgumentList -> StatePatternArgumentList TComma· StatePatternArgument [] }
   41 { StatePatternArgument -> TAsterisk· TId [] }
   42 { StatePatternArgument -> TAsterisk TId· [TComma TParenClose] }
   43 { StatePatternArgument -> TId· [TComma TParenClose] }
   44 { StatePatternArgument -> TUnderscore· [TComma TParenClose] }
   45 { StatePatternArgumentList -> StatePatternArgument· [TComma TParenClose] }
   46 { StatePatternArgumentList -> StatePatternArgumentList· TComma StatePatternArgument [] | StatePatternArguments -> TParenOpen StatePatternArgumentList· TParenClose [] }
   47 { StatePatternArgumentList -> StatePatternArgumentList TComma StatePatternArgument· [TComma TParenClose] }
   48 { StatePatternArguments -> TParenOpen StatePatternArgumentList TParenClose· [TArrow] }
   49 { States -> TStates· TColon TBlockOpen StateDefinitionList TBlockClose [] }
   50 { States -> TStates TColon· TBlockOpen StateDefinitionList TBlockClose [] }
   51 { States -> TStates TColon TBlockOpen StateDefinitionList TBlockClose· [TBreak] }
   52 { Transition -> ·TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose [] | TransitionList -> ·Transition [] | TransitionList -> ·TransitionList TBreak Transition [] | Transitions -> TTransitions TColon TBlockOpen· TransitionList TBlockClose [] }
   53 { Transition -> ·TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose [] | TransitionList -> TransitionList TBreak· Transition [] }
   54 { Transition -> TOn· TQuotedString TBlockOpen TransitionCaseList TBlockClose [] }
   55 { Transition -> TOn TQuotedString· TBlockOpen TransitionCaseList TBlockClose [] }
   56 { Transition -> TOn TQuotedString TBlockOpen TransitionCaseList· TBlockClose [] | TransitionCaseList -> TransitionCaseList· TBreak TransitionCase [] }
   57 { Transition -> TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose· [TBlockClose TBreak] }
   58 { TransitionCase -> StatePattern· TArrow StateConstruction [] }
   59 { TransitionCase -> StatePattern TArrow StateConstruction· [TBlockClose TBreak] }
   60 { TransitionCaseList -> TransitionCase· [TBlockClose TBreak] }
   61 { TransitionCaseList -> TransitionCaseList TBreak TransitionCase· [TBlockClose TBreak] }
   62 { TransitionList -> Transition· [TBlockClose TBreak] }
   63 { TransitionList -> TransitionList· TBreak Transition [] | Transitions -> TTransitions TColon TBlockOpen TransitionList· TBlockClose [] }
   64 { TransitionList -> TransitionList TBreak Transition· [TBlockClose TBreak] }
   65 { Transitions -> TTransitions· TColon TBlockOpen TransitionList TBlockClose [] }
   66 { Transitions -> TTransitions TColon· TBlockOpen TransitionList TBlockClose [] }
   67 { Transitions -> TTransitions TColon TBlockOpen TransitionList TBlockClose· [$] }
   68 { ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator· ValueExpressionId [] | ValueExpressionId -> ·TId [] }
   69 { ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator ValueExpressionId· [TComma TMinus TParenClose] }
   70 { ValueExpressionBinOp -> ValueExpressionId· [TComma TMinus TParenClose] }
   71 { ValueExpressionId -> TId· [TComma TMinus TParenClose] }

PRODUCTIONS
   ValueExpression -> ValueExpressionBinOp
   BinaryOperator -> TMinus
   Initial -> TInitial TColon TBlockOpen StateConstruction TBlockClose
   StateConstructionArguments -> 
   StateConstruction -> TId StateConstructionArguments
   StateConstructionArgumentList -> StateConstructionArgumentList TComma ValueExpression
   StateConstructionArgumentList -> ValueExpression
   StateConstructionArguments -> TParenOpen StateConstructionArgumentList TParenClose
   StateDefinitionParameters -> 
   StateDefinition -> TId StateDefinitionParameters
   StateDefinitionList -> StateDefinition
   StateDefinitionList -> StateDefinitionList TBreak StateDefinition
   StateDefinitionParameterList -> StateDefinitionParameterList TComma TId
   StateDefinitionParameterList -> TId
   StateDefinitionParameters -> TParenOpen StateDefinitionParameterList TParenClose
   StatePatternArguments -> 
   StatePattern -> TId StatePatternArguments
   StatePatternArgument -> TAsterisk TId
   StatePatternArgument -> TId
   StatePatternArgument -> TUnderscore
   StatePatternArgumentList -> StatePatternArgument
   StatePatternArgumentList -> StatePatternArgumentList TComma StatePatternArgument
   StatePatternArguments -> TParenOpen StatePatternArgumentList TParenClose
   States -> TStates TColon TBlockOpen StateDefinitionList TBlockClose
   Transition -> TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose
   TransitionCase -> StatePattern TArrow StateConstruction
   TransitionCaseList -> TransitionCase
   TransitionCaseList -> TransitionCaseList TBreak TransitionCase
   TransitionList -> Transition
   TransitionList -> TransitionList TBreak Transition
   Transitions -> TTransitions TColon TBlockOpen TransitionList TBlockClose
   ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator ValueExpressionId
   ValueExpressionBinOp -> ValueExpressionId
   ValueExpressionId -> TId

ACTION
   State Lookahead     Action
   0     TComma        reduce (ValueExpression -> ValueExpressionBinOp)
   0     TMinus        shift (1)
   0     TParenClose   reduce (ValueExpression -> ValueExpressionBinOp)
   1     TId           reduce (BinaryOperator -> TMinus)
   2     TStates       shift (49)
   3     TBreak        shift (4)
   4     TInitial      shift (8)
   5     TBreak        shift (6)
   6     TTransitions  shift (65)
   7     $             accept
   8     TColon        shift (9)
   9     TBlockOpen    shift (10)
   10    TId           shift (14)
   11    TBlockClose   shift (12)
   12    TBreak        reduce (Initial -> TInitial TColon TBlockOpen StateConstruction TBlockClose)
   13    TId           shift (14)
   14    TBlockClose   reduce (StateConstructionArguments -> )
   14    TBreak        reduce (StateConstructionArguments -> )
   14    TParenOpen    shift (16)
   15    TBlockClose   reduce (StateConstruction -> TId StateConstructionArguments)
   15    TBreak        reduce (StateConstruction -> TId StateConstructionArguments)
   16    TId           shift (71)
   17    TComma        shift (18)
   17    TParenClose   shift (21)
   18    TId           shift (71)
   19    TComma        reduce (StateConstructionArgumentList -> StateConstructionArgumentList TComma ValueExpression)
   19    TParenClose   reduce (StateConstructionArgumentList -> StateConstructionArgumentList TComma ValueExpression)
   20    TComma        reduce (StateConstructionArgumentList -> ValueExpression)
   20    TParenClose   reduce (StateConstructionArgumentList -> ValueExpression)
   21    TBlockClose   reduce (StateConstructionArguments -> TParenOpen StateConstructionArgumentList TParenClose)
   21    TBreak        reduce (StateConstructionArguments -> TParenOpen StateConstructionArgumentList TParenClose)
   22    TId           shift (24)
   23    TId           shift (24)
   24    TBlockClose   reduce (StateDefinitionParameters -> )
   24    TBreak        reduce (StateDefinitionParameters -> )
   24    TParenOpen    shift (29)
   25    TBlockClose   reduce (StateDefinition -> TId StateDefinitionParameters)
   25    TBreak        reduce (StateDefinition -> TId StateDefinitionParameters)
   26    TBlockClose   reduce (StateDefinitionList -> StateDefinition)
   26    TBreak        reduce (StateDefinitionList -> StateDefinition)
   27    TBlockClose   shift (51)
   27    TBreak        shift (23)
   28    TBlockClose   reduce (StateDefinitionList -> StateDefinitionList TBreak StateDefinition)
   28    TBreak        reduce (StateDefinitionList -> StateDefinitionList TBreak StateDefinition)
   29    TId           shift (33)
   30    TComma        shift (31)
   30    TParenClose   shift (34)
   31    TId           shift (32)
   32    TComma        reduce (StateDefinitionParameterList -> StateDefinitionParameterList TComma TId)
   32    TParenClose   reduce (StateDefinitionParameterList -> StateDefinitionParameterList TComma TId)
   33    TComma        reduce (StateDefinitionParameterList -> TId)
   33    TParenClose   reduce (StateDefinitionParameterList -> TId)
   34    TBlockClose   reduce (StateDefinitionParameters -> TParenOpen StateDefinitionParameterList TParenClose)
   34    TBreak        reduce (StateDefinitionParameters -> TParenOpen StateDefinitionParameterList TParenClose)
   35    TId           shift (37)
   36    TId           shift (37)
   37    TArrow        reduce (StatePatternArguments -> )
   37    TParenOpen    shift (39)
   38    TArrow        reduce (StatePattern -> TId StatePatternArguments)
   39    TAsterisk     shift (41)
   39    TId           shift (43)
   39    TUnderscore   shift (44)
   40    TAsterisk     shift (41)
   40    TId           shift (43)
   40    TUnderscore   shift (44)
   41    TId           shift (42)
   42    TComma        reduce (StatePatternArgument -> TAsterisk TId)
   42    TParenClose   reduce (StatePatternArgument -> TAsterisk TId)
   43    TComma        reduce (StatePatternArgument -> TId)
   43    TParenClose   reduce (StatePatternArgument -> TId)
   44    TComma        reduce (StatePatternArgument -> TUnderscore)
   44    TParenClose   reduce (StatePatternArgument -> TUnderscore)
   45    TComma        reduce (StatePatternArgumentList -> StatePatternArgument)
   45    TParenClose   reduce (StatePatternArgumentList -> StatePatternArgument)
   46    TComma        shift (40)
   46    TParenClose   shift (48)
   47    TComma        reduce (StatePatternArgumentList -> StatePatternArgumentList TComma StatePatternArgument)
   47    TParenClose   reduce (StatePatternArgumentList -> StatePatternArgumentList TComma StatePatternArgument)
   48    TArrow        reduce (StatePatternArguments -> TParenOpen StatePatternArgumentList TParenClose)
   49    TColon        shift (50)
   50    TBlockOpen    shift (22)
   51    TBreak        reduce (States -> TStates TColon TBlockOpen StateDefinitionList TBlockClose)
   52    TOn           shift (54)
   53    TOn           shift (54)
   54    TQuotedString shift (55)
   55    TBlockOpen    shift (35)
   56    TBlockClose   shift (57)
   56    TBreak        shift (36)
   57    TBlockClose   reduce (Transition -> TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose)
   57    TBreak        reduce (Transition -> TOn TQuotedString TBlockOpen TransitionCaseList TBlockClose)
   58    TArrow        shift (13)
   59    TBlockClose   reduce (TransitionCase -> StatePattern TArrow StateConstruction)
   59    TBreak        reduce (TransitionCase -> StatePattern TArrow StateConstruction)
   60    TBlockClose   reduce (TransitionCaseList -> TransitionCase)
   60    TBreak        reduce (TransitionCaseList -> TransitionCase)
   61    TBlockClose   reduce (TransitionCaseList -> TransitionCaseList TBreak TransitionCase)
   61    TBreak        reduce (TransitionCaseList -> TransitionCaseList TBreak TransitionCase)
   62    TBlockClose   reduce (TransitionList -> Transition)
   62    TBreak        reduce (TransitionList -> Transition)
   63    TBlockClose   shift (67)
   63    TBreak        shift (53)
   64    TBlockClose   reduce (TransitionList -> TransitionList TBreak Transition)
   64    TBreak        reduce (TransitionList -> TransitionList TBreak Transition)
   65    TColon        shift (66)
   66    TBlockOpen    shift (52)
   67    $             reduce (Transitions -> TTransitions TColon TBlockOpen TransitionList TBlockClose)
   68    TId           shift (71)
   69    TComma        reduce (ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator ValueExpressionId)
   69    TMinus        reduce (ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator ValueExpressionId)
   69    TParenClose   reduce (ValueExpressionBinOp -> ValueExpressionBinOp BinaryOperator ValueExpressionId)
   70    TComma        reduce (ValueExpressionBinOp -> ValueExpressionId)
   70    TMinus        reduce (ValueExpressionBinOp -> ValueExpressionId)
   70    TParenClose   reduce (ValueExpressionBinOp -> ValueExpressionId)
   71    TComma        reduce (ValueExpressionId -> TId)
   71    TMinus        reduce (ValueExpressionId -> TId)
   71    TParenClose   reduce (ValueExpressionId -> TId)

GOTO
   Source state Symbol                        Destination state
   0            BinaryOperator                68
   2            States                        3
   4            Initial                       5
   6            Transitions                   7
   10           StateConstruction             11
   13           StateConstruction             59
   14           StateConstructionArguments    15
   16           StateConstructionArgumentList 17
   16           ValueExpression               20
   16           ValueExpressionBinOp          0
   16           ValueExpressionId             70
   18           ValueExpression               19
   18           ValueExpressionBinOp          0
   18           ValueExpressionId             70
   22           StateDefinition               26
   22           StateDefinitionList           27
   23           StateDefinition               28
   24           StateDefinitionParameters     25
   29           StateDefinitionParameterList  30
   35           StatePattern                  58
   35           TransitionCase                60
   35           TransitionCaseList            56
   36           StatePattern                  58
   36           TransitionCase                61
   37           StatePatternArguments         38
   39           StatePatternArgument          45
   39           StatePatternArgumentList      46
   40           StatePatternArgument          47
   52           Transition                    62
   52           TransitionList                63
   53           Transition                    64
   68           ValueExpressionId             69

*)

type TArrow = unit
type TAsterisk = unit
type TBlockClose = unit
type TBlockOpen = unit
type TBreak = unit
type TColon = unit
type TComma = unit
type TId = string
type TInitial = unit
type TInvalidToken = string
type TMinus = unit
type TOn = unit
type TParenClose = unit
type TParenOpen = unit
type TQuotedString = string
type TStates = unit
type TTransitions = unit
type TUnderscore = unit

type BinaryOperator =
    | Minus of TMinus

type Document =
    | Document of States * TBreak * Initial * TBreak * Transitions

type Initial =
    | Initial of TInitial * TColon * TBlockOpen * StateConstruction * TBlockClose

type StateConstruction =
    | StateConstruction of TId * StateConstructionArguments

type StateConstructionArgumentList =
    | Append of StateConstructionArgumentList * TComma * ValueExpression
    | Head of ValueExpression

type StateConstructionArguments =
    | Arguments of TParenOpen * StateConstructionArgumentList * TParenClose
    | NoArguments

type StateDefinition =
    | StateDefinition of TId * StateDefinitionParameters

type StateDefinitionList =
    | Append of StateDefinitionList * TBreak * StateDefinition
    | Head of StateDefinition

type StateDefinitionParameterList =
    | Append of StateDefinitionParameterList * TComma * TId
    | Head of TId

type StateDefinitionParameters =
    | NoParameters
    | Parameters of TParenOpen * StateDefinitionParameterList * TParenClose

type StatePattern =
    | StatePattern of TId * StatePatternArguments

type StatePatternArgument =
    | Bind of TAsterisk * TId
    | Id of TId
    | Underscore of TUnderscore

type StatePatternArgumentList =
    | Append of StatePatternArgumentList * TComma * StatePatternArgument
    | Head of StatePatternArgument

type StatePatternArguments =
    | Arguments of TParenOpen * StatePatternArgumentList * TParenClose
    | NoArguments

type States =
    | States of TStates * TColon * TBlockOpen * StateDefinitionList * TBlockClose

type Transition =
    | Transition of TOn * TQuotedString * TBlockOpen * TransitionCaseList * TBlockClose

type TransitionCase =
    | TransitionCase of StatePattern * TArrow * StateConstruction

type TransitionCaseList =
    | Append of TransitionCaseList * TBreak * TransitionCase
    | Head of TransitionCase

type TransitionList =
    | Append of TransitionList * TBreak * Transition
    | Head of Transition

type Transitions =
    | Transitions of TTransitions * TColon * TBlockOpen * TransitionList * TBlockClose

type ValueExpression =
    | ValueExpression of ValueExpressionBinOp

type ValueExpressionBinOp =
    | BinaryOp of ValueExpressionBinOp * BinaryOperator * ValueExpressionId
    | FallThrough of ValueExpressionId

type ValueExpressionId =
    | ValueExpressionId of TId

type InputItem =
    | TArrow of TArrow
    | TAsterisk of TAsterisk
    | TBlockClose of TBlockClose
    | TBlockOpen of TBlockOpen
    | TBreak of TBreak
    | TColon of TColon
    | TComma of TComma
    | TId of TId
    | TInitial of TInitial
    | TInvalidToken of TInvalidToken
    | TMinus of TMinus
    | TOn of TOn
    | TParenClose of TParenClose
    | TParenOpen of TParenOpen
    | TQuotedString of TQuotedString
    | TStates of TStates
    | TTransitions of TTransitions
    | TUnderscore of TUnderscore

type Unexpected =
    | EndOfStream
    | InputItem of InputItem

type ExpectedItem =
    | EndOfStream
    | TArrow
    | TAsterisk
    | TBlockClose
    | TBlockOpen
    | TBreak
    | TColon
    | TComma
    | TId
    | TInitial
    | TInvalidToken
    | TMinus
    | TOn
    | TParenClose
    | TParenOpen
    | TQuotedString
    | TStates
    | TTransitions
    | TUnderscore

type ParseError = {
    unexpected : Unexpected
    expected : list<ExpectedItem>
}

let parse (input: #seq<InputItem>) : Result<Document, ParseError> =
    use inputEnumerator = input.GetEnumerator()
    let lhsStack = System.Collections.Stack(50)
    let stateStack = System.Collections.Generic.Stack<int>(50)
    let mutable result = Unchecked.defaultof<Document>
    let mutable accepted = false
    let mutable expected = Unchecked.defaultof<list<ExpectedItem>>

    stateStack.Push(2)

    let mutable lookahead, lookaheadIsEof =
        if inputEnumerator.MoveNext()
        then (inputEnumerator.Current, false)
        else (Unchecked.defaultof<InputItem>, true)

    let mutable keepGoing = true
    while keepGoing do
        match stateStack.Peek() with
        | 0 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpressionBinOp
                let reductionResult = ValueExpression.ValueExpression arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 20
                    | 18 -> 19
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TMinus x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(1)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpressionBinOp
                let reductionResult = ValueExpression.ValueExpression arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 20
                    | 18 -> 19
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TMinus; ExpectedItem.TParenClose]
                keepGoing <- false
        | 1 ->
            match lookahead with
            | InputItem.TId _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TMinus
                let reductionResult = BinaryOperator.Minus arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 0 -> 68
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 2 ->
            match lookahead with
            | InputItem.TStates x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(49)
            | _ ->
                // error
                expected <- [ExpectedItem.TStates]
                keepGoing <- false
        | 3 ->
            match lookahead with
            | InputItem.TBreak x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(4)
            | _ ->
                // error
                expected <- [ExpectedItem.TBreak]
                keepGoing <- false
        | 4 ->
            match lookahead with
            | InputItem.TInitial x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(8)
            | _ ->
                // error
                expected <- [ExpectedItem.TInitial]
                keepGoing <- false
        | 5 ->
            match lookahead with
            | InputItem.TBreak x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(6)
            | _ ->
                // error
                expected <- [ExpectedItem.TBreak]
                keepGoing <- false
        | 6 ->
            match lookahead with
            | InputItem.TTransitions x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(65)
            | _ ->
                // error
                expected <- [ExpectedItem.TTransitions]
                keepGoing <- false
        | 7 ->
            match lookahead with
            | _ when lookaheadIsEof ->
                // accept
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> Transitions
                let arg4 = lhsStack.Pop() :?> TBreak
                let arg3 = lhsStack.Pop() :?> Initial
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> States
                let reductionResult = Document.Document (arg1, arg2, arg3, arg4, arg5)
                result <- reductionResult
                accepted <- true
                keepGoing <- false
            | _ ->
                // error
                expected <- [ExpectedItem.EndOfStream]
                keepGoing <- false
        | 8 ->
            match lookahead with
            | InputItem.TColon x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(9)
            | _ ->
                // error
                expected <- [ExpectedItem.TColon]
                keepGoing <- false
        | 9 ->
            match lookahead with
            | InputItem.TBlockOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(10)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockOpen]
                keepGoing <- false
        | 10 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(14)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 11 ->
            match lookahead with
            | InputItem.TBlockClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(12)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose]
                keepGoing <- false
        | 12 ->
            match lookahead with
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> TBlockClose
                let arg4 = lhsStack.Pop() :?> StateConstruction
                let arg3 = lhsStack.Pop() :?> TBlockOpen
                let arg2 = lhsStack.Pop() :?> TColon
                let arg1 = lhsStack.Pop() :?> TInitial
                let reductionResult = Initial.Initial (arg1, arg2, arg3, arg4, arg5)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 4 -> 5
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBreak]
                keepGoing <- false
        | 13 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(14)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 14 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                let reductionResult = StateConstructionArguments.NoArguments
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 14 -> 15
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                let reductionResult = StateConstructionArguments.NoArguments
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 14 -> 15
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(16)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak; ExpectedItem.TParenOpen]
                keepGoing <- false
        | 15 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> StateConstructionArguments
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateConstruction.StateConstruction (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 10 -> 11
                    | 13 -> 59
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> StateConstructionArguments
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateConstruction.StateConstruction (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 10 -> 11
                    | 13 -> 59
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 16 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(71)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 17 ->
            match lookahead with
            | InputItem.TComma x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(18)
            | InputItem.TParenClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(21)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 18 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(71)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 19 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> ValueExpression
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StateConstructionArgumentList
                let reductionResult = StateConstructionArgumentList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 17
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> ValueExpression
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StateConstructionArgumentList
                let reductionResult = StateConstructionArgumentList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 17
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 20 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpression
                let reductionResult = StateConstructionArgumentList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 17
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpression
                let reductionResult = StateConstructionArgumentList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 17
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 21 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TParenClose
                let arg2 = lhsStack.Pop() :?> StateConstructionArgumentList
                let arg1 = lhsStack.Pop() :?> TParenOpen
                let reductionResult = StateConstructionArguments.Arguments (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 14 -> 15
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TParenClose
                let arg2 = lhsStack.Pop() :?> StateConstructionArgumentList
                let arg1 = lhsStack.Pop() :?> TParenOpen
                let reductionResult = StateConstructionArguments.Arguments (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 14 -> 15
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 22 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(24)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 23 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(24)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 24 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                let reductionResult = StateDefinitionParameters.NoParameters
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 24 -> 25
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                let reductionResult = StateDefinitionParameters.NoParameters
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 24 -> 25
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(29)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak; ExpectedItem.TParenOpen]
                keepGoing <- false
        | 25 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> StateDefinitionParameters
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateDefinition.StateDefinition (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 26
                    | 23 -> 28
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> StateDefinitionParameters
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateDefinition.StateDefinition (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 26
                    | 23 -> 28
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 26 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> StateDefinition
                let reductionResult = StateDefinitionList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 27
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> StateDefinition
                let reductionResult = StateDefinitionList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 27
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 27 ->
            match lookahead with
            | InputItem.TBlockClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(51)
            | InputItem.TBreak x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(23)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 28 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StateDefinition
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> StateDefinitionList
                let reductionResult = StateDefinitionList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 27
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StateDefinition
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> StateDefinitionList
                let reductionResult = StateDefinitionList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 22 -> 27
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 29 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(33)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 30 ->
            match lookahead with
            | InputItem.TComma x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(31)
            | InputItem.TParenClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(34)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 31 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(32)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 32 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TId
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StateDefinitionParameterList
                let reductionResult = StateDefinitionParameterList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 29 -> 30
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TId
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StateDefinitionParameterList
                let reductionResult = StateDefinitionParameterList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 29 -> 30
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 33 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateDefinitionParameterList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 29 -> 30
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StateDefinitionParameterList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 29 -> 30
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 34 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TParenClose
                let arg2 = lhsStack.Pop() :?> StateDefinitionParameterList
                let arg1 = lhsStack.Pop() :?> TParenOpen
                let reductionResult = StateDefinitionParameters.Parameters (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 24 -> 25
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TParenClose
                let arg2 = lhsStack.Pop() :?> StateDefinitionParameterList
                let arg1 = lhsStack.Pop() :?> TParenOpen
                let reductionResult = StateDefinitionParameters.Parameters (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 24 -> 25
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 35 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(37)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 36 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(37)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 37 ->
            match lookahead with
            | InputItem.TArrow _ ->
                // reduce
                let reductionResult = StatePatternArguments.NoArguments
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 37 -> 38
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(39)
            | _ ->
                // error
                expected <- [ExpectedItem.TArrow; ExpectedItem.TParenOpen]
                keepGoing <- false
        | 38 ->
            match lookahead with
            | InputItem.TArrow _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> StatePatternArguments
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StatePattern.StatePattern (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 58
                    | 36 -> 58
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TArrow]
                keepGoing <- false
        | 39 ->
            match lookahead with
            | InputItem.TAsterisk x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(41)
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(43)
            | InputItem.TUnderscore x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(44)
            | _ ->
                // error
                expected <- [ExpectedItem.TAsterisk; ExpectedItem.TId; ExpectedItem.TUnderscore]
                keepGoing <- false
        | 40 ->
            match lookahead with
            | InputItem.TAsterisk x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(41)
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(43)
            | InputItem.TUnderscore x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(44)
            | _ ->
                // error
                expected <- [ExpectedItem.TAsterisk; ExpectedItem.TId; ExpectedItem.TUnderscore]
                keepGoing <- false
        | 41 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(42)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 42 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> TId
                let arg1 = lhsStack.Pop() :?> TAsterisk
                let reductionResult = StatePatternArgument.Bind (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg2 = lhsStack.Pop() :?> TId
                let arg1 = lhsStack.Pop() :?> TAsterisk
                let reductionResult = StatePatternArgument.Bind (arg1, arg2)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 43 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StatePatternArgument.Id arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = StatePatternArgument.Id arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 44 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TUnderscore
                let reductionResult = StatePatternArgument.Underscore arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TUnderscore
                let reductionResult = StatePatternArgument.Underscore arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 45
                    | 40 -> 47
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 45 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> StatePatternArgument
                let reductionResult = StatePatternArgumentList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 46
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> StatePatternArgument
                let reductionResult = StatePatternArgumentList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 46
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 46 ->
            match lookahead with
            | InputItem.TComma x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(40)
            | InputItem.TParenClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(48)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 47 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StatePatternArgument
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StatePatternArgumentList
                let reductionResult = StatePatternArgumentList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 46
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StatePatternArgument
                let arg2 = lhsStack.Pop() :?> TComma
                let arg1 = lhsStack.Pop() :?> StatePatternArgumentList
                let reductionResult = StatePatternArgumentList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 39 -> 46
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TParenClose]
                keepGoing <- false
        | 48 ->
            match lookahead with
            | InputItem.TArrow _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TParenClose
                let arg2 = lhsStack.Pop() :?> StatePatternArgumentList
                let arg1 = lhsStack.Pop() :?> TParenOpen
                let reductionResult = StatePatternArguments.Arguments (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 37 -> 38
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TArrow]
                keepGoing <- false
        | 49 ->
            match lookahead with
            | InputItem.TColon x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(50)
            | _ ->
                // error
                expected <- [ExpectedItem.TColon]
                keepGoing <- false
        | 50 ->
            match lookahead with
            | InputItem.TBlockOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(22)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockOpen]
                keepGoing <- false
        | 51 ->
            match lookahead with
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> TBlockClose
                let arg4 = lhsStack.Pop() :?> StateDefinitionList
                let arg3 = lhsStack.Pop() :?> TBlockOpen
                let arg2 = lhsStack.Pop() :?> TColon
                let arg1 = lhsStack.Pop() :?> TStates
                let reductionResult = States.States (arg1, arg2, arg3, arg4, arg5)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 2 -> 3
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBreak]
                keepGoing <- false
        | 52 ->
            match lookahead with
            | InputItem.TOn x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(54)
            | _ ->
                // error
                expected <- [ExpectedItem.TOn]
                keepGoing <- false
        | 53 ->
            match lookahead with
            | InputItem.TOn x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(54)
            | _ ->
                // error
                expected <- [ExpectedItem.TOn]
                keepGoing <- false
        | 54 ->
            match lookahead with
            | InputItem.TQuotedString x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(55)
            | _ ->
                // error
                expected <- [ExpectedItem.TQuotedString]
                keepGoing <- false
        | 55 ->
            match lookahead with
            | InputItem.TBlockOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(35)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockOpen]
                keepGoing <- false
        | 56 ->
            match lookahead with
            | InputItem.TBlockClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(57)
            | InputItem.TBreak x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(36)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 57 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> TBlockClose
                let arg4 = lhsStack.Pop() :?> TransitionCaseList
                let arg3 = lhsStack.Pop() :?> TBlockOpen
                let arg2 = lhsStack.Pop() :?> TQuotedString
                let arg1 = lhsStack.Pop() :?> TOn
                let reductionResult = Transition.Transition (arg1, arg2, arg3, arg4, arg5)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 62
                    | 53 -> 64
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> TBlockClose
                let arg4 = lhsStack.Pop() :?> TransitionCaseList
                let arg3 = lhsStack.Pop() :?> TBlockOpen
                let arg2 = lhsStack.Pop() :?> TQuotedString
                let arg1 = lhsStack.Pop() :?> TOn
                let reductionResult = Transition.Transition (arg1, arg2, arg3, arg4, arg5)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 62
                    | 53 -> 64
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 58 ->
            match lookahead with
            | InputItem.TArrow x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(13)
            | _ ->
                // error
                expected <- [ExpectedItem.TArrow]
                keepGoing <- false
        | 59 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StateConstruction
                let arg2 = lhsStack.Pop() :?> TArrow
                let arg1 = lhsStack.Pop() :?> StatePattern
                let reductionResult = TransitionCase.TransitionCase (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 60
                    | 36 -> 61
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> StateConstruction
                let arg2 = lhsStack.Pop() :?> TArrow
                let arg1 = lhsStack.Pop() :?> StatePattern
                let reductionResult = TransitionCase.TransitionCase (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 60
                    | 36 -> 61
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 60 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TransitionCase
                let reductionResult = TransitionCaseList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 56
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TransitionCase
                let reductionResult = TransitionCaseList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 56
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 61 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TransitionCase
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> TransitionCaseList
                let reductionResult = TransitionCaseList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 56
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> TransitionCase
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> TransitionCaseList
                let reductionResult = TransitionCaseList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 35 -> 56
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 62 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> Transition
                let reductionResult = TransitionList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 63
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> Transition
                let reductionResult = TransitionList.Head arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 63
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 63 ->
            match lookahead with
            | InputItem.TBlockClose x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(67)
            | InputItem.TBreak x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(53)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 64 ->
            match lookahead with
            | InputItem.TBlockClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> Transition
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> TransitionList
                let reductionResult = TransitionList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 63
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TBreak _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> Transition
                let arg2 = lhsStack.Pop() :?> TBreak
                let arg1 = lhsStack.Pop() :?> TransitionList
                let reductionResult = TransitionList.Append (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 52 -> 63
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockClose; ExpectedItem.TBreak]
                keepGoing <- false
        | 65 ->
            match lookahead with
            | InputItem.TColon x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(66)
            | _ ->
                // error
                expected <- [ExpectedItem.TColon]
                keepGoing <- false
        | 66 ->
            match lookahead with
            | InputItem.TBlockOpen x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(52)
            | _ ->
                // error
                expected <- [ExpectedItem.TBlockOpen]
                keepGoing <- false
        | 67 ->
            match lookahead with
            | _ when lookaheadIsEof ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg5 = lhsStack.Pop() :?> TBlockClose
                let arg4 = lhsStack.Pop() :?> TransitionList
                let arg3 = lhsStack.Pop() :?> TBlockOpen
                let arg2 = lhsStack.Pop() :?> TColon
                let arg1 = lhsStack.Pop() :?> TTransitions
                let reductionResult = Transitions.Transitions (arg1, arg2, arg3, arg4, arg5)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 6 -> 7
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.EndOfStream]
                keepGoing <- false
        | 68 ->
            match lookahead with
            | InputItem.TId x ->
                // shift
                lhsStack.Push(x)
                if inputEnumerator.MoveNext() then
                    lookahead <- inputEnumerator.Current
                else
                    lookaheadIsEof <- true
                stateStack.Push(71)
            | _ ->
                // error
                expected <- [ExpectedItem.TId]
                keepGoing <- false
        | 69 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> ValueExpressionId
                let arg2 = lhsStack.Pop() :?> BinaryOperator
                let arg1 = lhsStack.Pop() :?> ValueExpressionBinOp
                let reductionResult = ValueExpressionBinOp.BinaryOp (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TMinus _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> ValueExpressionId
                let arg2 = lhsStack.Pop() :?> BinaryOperator
                let arg1 = lhsStack.Pop() :?> ValueExpressionBinOp
                let reductionResult = ValueExpressionBinOp.BinaryOp (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                let arg3 = lhsStack.Pop() :?> ValueExpressionId
                let arg2 = lhsStack.Pop() :?> BinaryOperator
                let arg1 = lhsStack.Pop() :?> ValueExpressionBinOp
                let reductionResult = ValueExpressionBinOp.BinaryOp (arg1, arg2, arg3)
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TMinus; ExpectedItem.TParenClose]
                keepGoing <- false
        | 70 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpressionId
                let reductionResult = ValueExpressionBinOp.FallThrough arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TMinus _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpressionId
                let reductionResult = ValueExpressionBinOp.FallThrough arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> ValueExpressionId
                let reductionResult = ValueExpressionBinOp.FallThrough arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 0
                    | 18 -> 0
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TMinus; ExpectedItem.TParenClose]
                keepGoing <- false
        | 71 ->
            match lookahead with
            | InputItem.TComma _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = ValueExpressionId.ValueExpressionId arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 70
                    | 18 -> 70
                    | 68 -> 69
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TMinus _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = ValueExpressionId.ValueExpressionId arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 70
                    | 18 -> 70
                    | 68 -> 69
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | InputItem.TParenClose _ ->
                // reduce
                stateStack.Pop() |> ignore
                let arg1 = lhsStack.Pop() :?> TId
                let reductionResult = ValueExpressionId.ValueExpressionId arg1
                lhsStack.Push(reductionResult)
                let nextState =
                    match stateStack.Peek() with
                    | 16 -> 70
                    | 18 -> 70
                    | 68 -> 69
                    | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."
                stateStack.Push(nextState)
            | _ ->
                // error
                expected <- [ExpectedItem.TComma; ExpectedItem.TMinus; ExpectedItem.TParenClose]
                keepGoing <- false
        | _ -> failwith "Parser is in an invalid state. This is a bug in the parser generator."

    if accepted
    then Ok result
    else Error {
        unexpected = if lookaheadIsEof then Unexpected.EndOfStream else Unexpected.InputItem lookahead
        expected = expected
    }
