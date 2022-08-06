module internal Workflow.Token

open System
open System.IO
open HnkParserCombinator
open HnkParserCombinator.Primitives
open HnkParserCombinator.Composition
open HnkParserCombinator.CharPrimitives

type Token = Parser.InputItem

type private TokenParser = CharParser<IndentationBasedLanguageKit.State, PrimitiveError<char>, Token>

let private isWhiteSpace char = char = ' '

let private constToken str t =
    constString str
    >> ParseResult.constValue (t ())

let private pStates: TokenParser =
    constToken "states" Token.TStates

let private pTransitions: TokenParser =
    constToken "transitions" Token.TTransitions

let private pInitial: TokenParser =
    constToken "initial" Token.TInitial

let private pColon: TokenParser =
    constToken ":" Token.TColon

let private pParenOpen: TokenParser =
    constToken "(" Token.TParenOpen

let private pParenClose: TokenParser =
    constToken ")" Token.TParenClose

let private pOn: TokenParser =
    constToken "on" Token.TOn

let private pArrow: TokenParser =
    constToken "->" Token.TArrow

let private pUnderscore: TokenParser =
    constToken "_" Token.TUnderscore

let private pComma: TokenParser =
    constToken "," Token.TComma

let private pMinus: TokenParser =
    constToken "-" Token.TMinus

let private pAsterisk: TokenParser =
    constToken "*" Token.TAsterisk

let private pId: TokenParser =
    oneOrMoreCond Char.IsLetter
    >> ParseResult.mapValue (String >> Token.TId)

let private pQuotedString: TokenParser =
    skipOne '"' >>. zeroOrMoreCond (fun c -> c <> '"')
    .>> skipOne '"'
    >> ParseResult.mapValue (String >> Token.TQuotedString)

let private pInvalidToken: TokenParser =
    oneOrMoreCond (fun c -> not (isWhiteSpace c) && c <> '\n' && c <> '\r')
    >> ParseResult.mapValue (String >> Token.TInvalidToken)

let tokenize (stream: Stream) =
    let reader = new StreamReader(stream)

    let readChar () =
        match reader.Read() with
        | -1 -> None
        | c -> Some(char c)

    let tape = Tape(4096, readChar)

    let parseToken =
        chooseFirstLongest [ pStates
                             pTransitions
                             pInitial
                             pColon
                             pParenOpen
                             pParenClose
                             pOn
                             pArrow
                             pUnderscore
                             pComma
                             pMinus
                             pAsterisk
                             pId
                             pQuotedString ]
        |> orElse pInvalidToken

    let parseDocument =
        IndentationBasedLanguageKit.simpleParseDocument
            { parseToken = parseToken
              blockOpenToken = Token.TBlockOpen()
              newLineDelimiter = Token.TBreak()
              blockCloseToken = Token.TBlockClose()
              isWhiteSpace = isWhiteSpace }

    match parseDocument tape with
    | Error e -> Error e
    | Ok result -> Ok result.value
