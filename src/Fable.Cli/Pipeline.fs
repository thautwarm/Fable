module Fable.Cli.Pipeline

open System
open Fable
open Fable.AST
open Fable.Transforms

let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) =
    match com.Options.Language with
    | JavaScript | TypeScript -> Js.compileFile com cliArgs dedupTargetDir outPath
    | Python -> Python.compileFile com cliArgs dedupTargetDir outPath
    | Php -> Php.compileFile com outPath
    | Dart -> Dart.compileFile com cliArgs dedupTargetDir outPath
