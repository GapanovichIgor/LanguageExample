
open System
open System.IO
open Workflow

type private InputItem = Parser.InputItem

let private workflowDefinitionFileStream = File.OpenRead "WorkflowDefinition.txt"

let private tokens = Token.tokenize workflowDefinitionFileStream

match tokens with
| Error e -> failwith (string e)
| Ok tokens ->

let parseTree = Parser.parse tokens

match parseTree with
| Error e -> failwith (string e)
| Ok parseTree ->

let ast = Ast.createFromParseTree parseTree

let runtime = Runtime.Runtime(ast)

Console.WriteLine($"State: {runtime.CurrentState}")

let doAction action =
    Console.WriteLine($"Action: {action}")
    runtime.Action(action)
    Console.WriteLine($"State: {runtime.CurrentState}")

doAction "request approval from {Bob, Jack}"
doAction "approved by {Jack}"
doAction "request change {Consider rewording}"
doAction "request approval from {Bob, Jack}"
doAction "approved by {Jack}"
doAction "approved by {Bob}"