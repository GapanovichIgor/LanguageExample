module internal Workflow.Runtime

open System
open Workflow.Ast

type Runtime(ast: Document) =
    do
        if ast.initial.arguments.Length > 0 then
            failwith "Arguments for initial state are not supported yet"

    let mutable stateName = ast.initial.name
    let mutable data = []

    let matchAction (pattern: string) (input: string) =
        let patternSegments = pattern.Split([| '{'; '}' |]) |> List.ofArray
        let inputSegments = input.Split([| '{'; '}' |]) |> List.ofArray

        let rec matchSegments patternSegments inputSegments bindings =
            match patternSegments, inputSegments with
            | [], [] -> Some bindings
            | [pText], [iText] when pText = iText -> Some bindings
            | pText :: varName :: pRest, iText :: varValue :: iRest when pText = iText -> matchSegments pRest iRest ((varName, varValue) :: bindings)
            | _, _ -> None

        matchSegments patternSegments inputSegments []

    let matchStatePattern (action: string) (actionData: Map<string, string>) (pattern : StatePattern) =
        if pattern.name <> stateName then None else

        if pattern.arguments.Length <> data.Length then None else

        let rec matchArgs data args bindings =
            match data, args with
            | [], _ :: _
            | _ :: _, [] -> None
            | [], [] -> Some bindings
            | dataItem :: dataRest, arg :: argRest ->
                match arg with
                | Discard -> matchArgs dataRest argRest bindings
                | MatchId id ->
                    match actionData.TryFind(id) with
                    | None -> failwith $"Missing value '{id}' for state '{stateName}' on action '{action}'"
                    | Some value ->
                        if value <> dataItem then
                            None
                        else
                            matchArgs dataRest argRest bindings
                | BindId id ->
                    let bindings = (id, dataItem) :: bindings
                    matchArgs dataRest argRest bindings

        matchArgs data pattern.arguments []

    let (|List|_|) (str: string) =
        let elements =
            str.Split(',')
            |> Seq.map (fun s -> s.Trim())
            |> List.ofSeq

        if elements.Length > 1
        then Some elements
        else None

    let (|Number|_|) (str: string) =
        match Int32.TryParse(str) with
        | true, number -> Some number
        | false, _ -> None

    let rec evaluateExpression (data: Map<string, string>) (expression: ValueExpression) =
        match expression with
        | Id id ->
            match data.TryFind(id) with
            | Some value -> value
            | None -> failwith $"Missing value for '{id}'"
        | BinaryOperation(a, op, b) ->
            let a = evaluateExpression data a
            let b = evaluateExpression data b
            match op with
            | Minus ->
                match a, b with
                | List aList, b ->
                    aList
                    |> List.filter (fun item -> item <> b)
                    |> String.concat ", "
                | Number aNumber, Number bNumber ->
                    aNumber - bNumber
                    |> string
                | _ -> failwith "Invalid expression"

    member _.CurrentState =
        let dataStr =
            if data.Length > 0 then
                data
                |> String.concat ", "
                |> sprintf "(%s)"
            else
                String.Empty
        $"{stateName}{dataStr}"

    member _.Action(action: string) =
        let transition =
            ast.transitions
            |> Seq.choose (fun tr ->
                let bindigs = matchAction tr.action action
                bindigs |> Option.map (fun bindings -> (tr, bindings)))
            |> Seq.tryExactlyOne

        match transition with
        | None -> failwith $"The action '{action}' does not match any transition"
        | Some (transition, actionData) ->

        let actionData = actionData |> Map.ofSeq

        let case =
            transition.cases
            |> Seq.choose (fun case ->
                let bindings = case.sourceStatePattern |> matchStatePattern action actionData
                bindings |> Option.map (fun bindings -> (case, bindings)))
            |> Seq.tryHead

        match case with
        | None -> failwith $"No matching transition from state '{stateName}' on action '{action}'"
        | Some (case, patternData) ->

        let patternData = patternData |> Map.ofSeq

        let combinedData =
            patternData
            |> Map.fold (fun map k v -> map |> Map.add k v) actionData

        let newData =
            case.newStateExpression.arguments
            |> List.map (evaluateExpression combinedData)

        stateName <- case.newStateExpression.name
        data <- newData
