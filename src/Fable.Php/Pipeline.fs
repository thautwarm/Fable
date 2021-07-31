module Fable.Php.Pipeline

let compileFile (com: Compiler) (outPath: string) = async {
    let php =
        FSharp2Fable.Compiler.transformFile com
        |> FableTransforms.transformFile com
        |> Fable2Php.transformFile com

    use w = new IO.StreamWriter(outPath)
    let ctx = PhpPrinter.Output.Writer.create w
    PhpPrinter.Output.writeFile ctx php
    w.Flush()
}
