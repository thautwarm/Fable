module Fable.Javascript.Pipeline

type BabelWriter(cliArgs: CliArgs, dedupTargetDir, sourcePath: string, targetPath: string) =
    // In imports *.ts extensions have to be converted to *.js extensions instead
    let fileExt =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        if fileExt.EndsWith(".ts") then Path.replaceExtension ".js" fileExt else fileExt
    let targetDir = Path.GetDirectoryName(targetPath)
    let stream = new IO.StreamWriter(targetPath)
    let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))
    interface BabelPrinter.Writer with
        member _.Write(str) =
            stream.WriteAsync(str) |> Async.AwaitTask
        member _.EscapeJsStringLiteral(str) =
            Web.HttpUtility.JavaScriptStringEncode(str)
        member _.MakeImportPath(path) =
            let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
            let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
            if path.EndsWith(".fs") then
                let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                File.changeFsExtension isInFableHiddenDir path fileExt
            else path
        member _.Dispose() = stream.Dispose()
        member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
            if cliArgs.SourceMaps then
                let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
                let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
                mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=name)
    member _.SourceMap =
        mapGenerator.Force().toJSON()

let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
    let babel =
        FSharp2Fable.Compiler.transformFile com
        |> FableTransforms.transformFile com
        |> Fable2Babel.Compiler.transformFile com

    let! sourceMap = async {
        use writer = new BabelWriter(cliArgs, dedupTargetDir, com.CurrentFile, outPath)
        do! BabelPrinter.run writer babel
        return if cliArgs.SourceMaps then Some writer.SourceMap else None
    }

    match sourceMap with
    | Some sourceMap ->
        let mapPath = outPath + ".map"
        do! IO.File.AppendAllLinesAsync(outPath, [$"//# sourceMappingURL={IO.Path.GetFileName(mapPath)}"]) |> Async.AwaitTask
        use fs = IO.File.Open(mapPath, IO.FileMode.Create)
        do! sourceMap.SerializeAsync(fs) |> Async.AwaitTask
    | None -> ()
}
