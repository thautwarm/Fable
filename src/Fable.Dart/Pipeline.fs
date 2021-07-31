module Fable.Dart.Printer

type DartWriter(com: Compiler, cliArgs: CliArgs, dedupTargetDir, targetPath: string) =
    let sourcePath = com.CurrentFile
    let fileExt = cliArgs.CompilerOptions.FileExtension
    let targetDir = Path.GetDirectoryName(targetPath)
    let stream = new IO.StreamWriter(targetPath)
    interface DartPrinter.Writer with
        member _.Write(str) =
            stream.WriteAsync(str) |> Async.AwaitTask
        member _.EscapeStringLiteral(str) =
            // TODO: Check if this works for Dart
            Web.HttpUtility.JavaScriptStringEncode(str)
        member _.MakeImportPath(path) =
            let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
            let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
            if path.EndsWith(".fs") then
                let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                File.changeFsExtension isInFableHiddenDir path fileExt
            else path
        member _.AddLog(msg, severity, ?range) =
            com.AddLog(msg, severity, ?range=range, fileName=com.CurrentFile)
        member _.Dispose() = stream.Dispose()

let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
    let _imports, fable =
        FSharp2Fable.Compiler.transformFile com
        |> FableTransforms.transformFile com
        |> Fable2Statements.Compiler.transformFile com

    use writer = new DartWriter(com, cliArgs, dedupTargetDir, outPath)
    do! DartPrinter.run writer fable
}
