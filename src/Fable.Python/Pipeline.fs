module Fable.Python.Pipeline

type PythonFileWriter(sourcePath: string, targetPath: string, cliArgs: CliArgs, dedupTargetDir) =
    let fileExt = ".py"
    let targetDir = Path.GetDirectoryName(targetPath)
    // PEP8: Modules should have short, all-lowercase names
    let fileName = Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(targetPath))
    let fileName = Naming.applyCaseRule Core.CaseRules.SnakeCase fileName
    // Note that Python modules cannot contain dots or it will be impossible to import them
    let targetPath = Path.Combine(targetDir, fileName + fileExt)

    let stream = new IO.StreamWriter(targetPath)

    interface PythonPrinter.Writer with
        member _.Write(str) =
            stream.WriteAsync(str) |> Async.AwaitTask
        member _.MakeImportPath(path) =
            let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
            let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
            if path.EndsWith(".fs") then
                let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                File.changeFsExtension isInFableHiddenDir path "" // Remove file extension
            else path
        member _.Dispose() = stream.Dispose()

let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
    let python =
        FSharp2Fable.Compiler.transformFile com
        |> FableTransforms.transformFile com
        |> Fable2Python.Compiler.transformFile com

    let map = { new PythonPrinter.SourceMapGenerator with
                    member _.AddMapping(_,_,_,_,_) = () }
    let writer = new PythonFileWriter(com.CurrentFile, outPath, cliArgs, dedupTargetDir)
    do! PythonPrinter.run writer map python
}
