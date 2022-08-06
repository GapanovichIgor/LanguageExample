module internal Workflow.Ast

type State =
    { name: string
      parameters: string list }

type StatePatternArgument =
    | Discard
    | MatchId of string
    | BindId of string

type StatePattern =
    { name: string
      arguments: StatePatternArgument list }

type BinaryOperator = | Minus

type ValueExpression =
    | Id of string
    | BinaryOperation of ValueExpression * BinaryOperator * ValueExpression

type StateConstruction =
    { name: string
      arguments: ValueExpression list }

type TransitionCase =
    { sourceStatePattern: StatePattern
      newStateExpression: StateConstruction }

type Transition =
    { action: string
      cases: TransitionCase list }

type Document =
    { states: State Set
      initial: StateConstruction
      transitions: Transition Set }

let createFromParseTree (p: Parser.Document) =
    let convertStateDefinition (stateDef: Parser.StateDefinition) =
        match stateDef with
        | Parser.StateDefinition (name, parameters) ->
            let rec convertParameterList parameters =
                match parameters with
                | Parser.StateDefinitionParameterList.Head paramName -> [ paramName ]
                | Parser.StateDefinitionParameterList.Append (head, _, paramName) ->
                    paramName :: convertParameterList head

            let parameters =
                match parameters with
                | Parser.StateDefinitionParameters.NoParameters -> []
                | Parser.StateDefinitionParameters.Parameters (_, parameterList, _) ->
                    convertParameterList parameterList

            { State.name = name
              parameters = parameters }

    let convertStateList stateList =
        let rec convertStateList stateList =
            match stateList with
            | Parser.StateDefinitionList.Head stateDefinition -> [ convertStateDefinition stateDefinition ]
            | Parser.StateDefinitionList.Append (stateList, _, stateDefinition) ->
                (convertStateDefinition stateDefinition)
                :: convertStateList stateList

        convertStateList stateList |> Set.ofList

    let convertStatePatternArgument argument =
        match argument with
        | Parser.StatePatternArgument.Underscore _ -> StatePatternArgument.Discard
        | Parser.StatePatternArgument.Id name -> StatePatternArgument.MatchId name
        | Parser.StatePatternArgument.Bind(_, name) -> StatePatternArgument.BindId name

    let convertStatePatternArgumentList argumentList =
        let rec convertStatePatternArgumentList argumentList =
            match argumentList with
            | Parser.StatePatternArgumentList.Head statePatternArgument ->
                [ convertStatePatternArgument statePatternArgument ]
            | Parser.StatePatternArgumentList.Append (argumentList, _, statePatternArgument) ->
                (convertStatePatternArgument statePatternArgument)
                :: convertStatePatternArgumentList argumentList

        convertStatePatternArgumentList argumentList
        |> List.rev

    let convertBinaryOperator operator =
        match operator with
        | Parser.BinaryOperator.Minus _ -> BinaryOperator.Minus

    let convertIdValueExpression expr =
        match expr with
        | Parser.ValueExpressionId id -> ValueExpression.Id id

    let rec convertBinaryOperatorValueExpression expr =
        match expr with
        | Parser.ValueExpressionBinOp.BinaryOp (binOp, operator, id) ->
            let a =
                convertBinaryOperatorValueExpression binOp

            let op = convertBinaryOperator operator
            let b = convertIdValueExpression id
            ValueExpression.BinaryOperation(a, op, b)
        | Parser.ValueExpressionBinOp.FallThrough valueExpressionId -> convertIdValueExpression valueExpressionId

    let rec convertValueExpression expr =
        match expr with
        | Parser.ValueExpression binOp -> convertBinaryOperatorValueExpression binOp

    let convertStateConstructionArgumentList argumentList =
        let rec convertStateConstructionArgumentList argumentList =
            match argumentList with
            | Parser.StateConstructionArgumentList.Head valueExpression -> [ convertValueExpression valueExpression ]
            | Parser.StateConstructionArgumentList.Append (argumentList, _, valueExpression) ->
                (convertValueExpression valueExpression)
                :: convertStateConstructionArgumentList argumentList

        convertStateConstructionArgumentList argumentList
        |> List.rev

    let convertStateConstruction stateConstruction =
        match stateConstruction with
        | Parser.StateConstruction (name, arguments) ->
            let arguments =
                match arguments with
                | Parser.StateConstructionArguments.NoArguments -> []
                | Parser.StateConstructionArguments.Arguments (_, argumentList, _) ->
                    convertStateConstructionArgumentList argumentList

            { StateConstruction.name = name
              arguments = arguments }

    let convertTransitionCase transitionCase =
        match transitionCase with
        | Parser.TransitionCase (sourceStatePattern, _, newStateConstruction) ->
            let sourceStatePattern =
                match sourceStatePattern with
                | Parser.StatePattern (name, arguments) ->
                    let arguments =
                        match arguments with
                        | Parser.StatePatternArguments.NoArguments -> []
                        | Parser.StatePatternArguments.Arguments (_, argumentList, _) ->
                            convertStatePatternArgumentList argumentList

                    { StatePattern.name = name
                      arguments = arguments }

            let newStateExpression =
                convertStateConstruction newStateConstruction

            { TransitionCase.sourceStatePattern = sourceStatePattern
              newStateExpression = newStateExpression }

    let convertTransitionCaseList transitionCaseList =
        let rec convertTransitionCaseList transitionCaseList =
            match transitionCaseList with
            | Parser.TransitionCaseList.Head transitionCase -> [ convertTransitionCase transitionCase ]
            | Parser.TransitionCaseList.Append (transitionCaseList, _, transitionCase) ->
                (convertTransitionCase transitionCase)
                :: convertTransitionCaseList transitionCaseList

        convertTransitionCaseList transitionCaseList
        |> List.rev

    let convertTransition (transition: Parser.Transition) =
        match transition with
        | Parser.Transition (_, action, _, transitionCaseList, _) ->
            { Transition.action = action
              cases = convertTransitionCaseList transitionCaseList }

    let convertTransitionList transitionList =
        let rec convertTransitionList transitionList =
            match transitionList with
            | Parser.TransitionList.Head transition -> [ convertTransition transition ]
            | Parser.TransitionList.Append (transitionList, _, transition) ->
                (convertTransition transition)
                :: convertTransitionList transitionList

        convertTransitionList transitionList |> Set.ofList

    match p with
    | Parser.Document (states, _, initial, _, transitions) ->
        let states =
            match states with
            | Parser.States (_, _, _, stateList, _) -> convertStateList stateList

        let initial =
            match initial with
            | Parser.Initial (_, _, _, stateConstruction, _) -> convertStateConstruction stateConstruction

        let transitions =
            match transitions with
            | Parser.Transitions (_, _, _, transitionList, _) -> convertTransitionList transitionList

        { Document.states = states
          initial = initial
          transitions = transitions }
