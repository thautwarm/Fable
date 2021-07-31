module rec Fable.Transforms.Fable2Statements

open Fable
open Fable.AST
open Fable.AST.Statements
open System.Collections.Generic
open Fable.Transforms.AST
open Fable.Transforms.FSharp2Fable

type ReturnStrategy =
    | ReturnExpr
    | ReturnUnit
    | Assign of Expression
    | Target of Fable.Ident

type Import =
  { Selector: string
    LocalIdent: string option
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type Context =
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string> }

type IStatementsCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Import list
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> Fable.Ident list * Choice<Expression, Statement list>
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Util =
    let (|TransformExpr|) (com: IStatementsCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let makeNull t =
        Value(Null t, None)

    let makeUntypedNull() =
        makeNull Fable.Any

    let addErrorAndReturnNull (com: Compiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        makeUntypedNull()

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgument && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    let getUniqueNameInRootScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name)
            || ctx.UsedNames.DeclarationScopes.Contains(name))
        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.CurrentDeclarationScope.Contains(name))
        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: Compiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds = discardUnitArg args |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds
            member _.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Value _ | Fable.Import _  | Fable.IdentExpr _
        | Fable.Lambda _ | Fable.Delegate _ | Fable.ObjectExpr _
        | Fable.Call _ | Fable.CurriedApply _ | Fable.Operation _
        | Fable.Get _ | Fable.Test _ | Fable.Curry _ -> false

        | Fable.TypeCast(e,_) -> isStatement ctx preferStatement e

        // TODO: These depend on language target
        | Fable.Set _ -> true
        | Fable.Throw _ -> false

        | Fable.TryCatch _
        | Fable.Sequential _ | Fable.Let _ | Fable.LetRec _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        // TODO: If IsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i,_,_) -> i.IsStatement

        | Fable.DecisionTreeSuccess(targetIndex,_, _) ->
            getDecisionTarget ctx targetIndex
            |> snd |> isStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isStatement ctx false)) targets

        | Fable.IfThenElse(_,thenExpr,elseExpr,_) ->
            preferStatement || isStatement ctx false thenExpr || isStatement ctx false elseExpr

//    let optimizeTailCall (com: IStatementsCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
//        let rec checkCrossRefs tempVars allArgs = function
//            | [] -> tempVars
//            | (argId, _arg)::rest ->
//                let found = allArgs |> List.exists (FableTransforms.deepExists (function
//                    | Fable.IdentExpr i -> argId = i.Name
//                    | _ -> false))
//                let tempVars =
//                    if found then
//                        let tempVarName = getUniqueNameInDeclarationScope ctx (argId + "_tmp")
//                        Map.add argId tempVarName tempVars
//                    else tempVars
//                checkCrossRefs tempVars allArgs rest
//        ctx.OptimizeTailCall()
//        let zippedArgs = List.zip tc.Args args
//        let tempVars = checkCrossRefs Map.empty args zippedArgs
//        let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)
//        [|
//            // First declare temp variables
//            for (KeyValue(argId, tempVar)) in tempVars do
//                yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
//            // Then assign argument expressions to the original argument identifiers
//            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
//            for (argId, arg) in zippedArgs do
//                let arg = FableTransforms.replaceValues tempVarReplacements arg
//                let arg = com.TransformAsExpr(ctx, arg)
//                yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
//            yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
//        |]

//    let transformImport (com: IStatementsCompiler) ctx r (selector: string) (path: string) =
//        let selector, parts =
//            let parts = Array.toList(selector.Split('.'))
//            parts.Head, parts.Tail
//        com.GetImportExpr(ctx, selector, path, r)
//        |> getParts parts

//    let transformCallArgs (com: IStatementsCompiler) ctx hasSpread args =
//        match args with
//        | []
//        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
//        | args when hasSpread ->
//            match List.rev args with
//            | [] -> []
//            | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
//                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
//                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
//            | last::rest ->
//                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
//                rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
//        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr strategy expr: Statement =
        match strategy with
        | None | Some ReturnUnit -> ExpressionStatement expr
        | Some ReturnExpr -> Return expr
        | Some(Assign left) -> Set(left, ValueSet, left.Type, expr, None)
        | Some(Target left) -> Set(IdentExpr left, ValueSet, left.Type, expr, None)

//    let transformCall (com: IStatementsCompiler) ctx range callee (callInfo: Fable.CallInfo) =
//        let callee = com.TransformAsExpr(ctx, callee)
//        let args = transformCallArgs com ctx callInfo.HasSpread callInfo.Args
//        match callInfo.ThisArg with
//        | Some(TransformExpr com ctx thisArg) -> callFunction range callee (thisArg::args)
//        | None when callInfo.IsConstructor -> Expression.newExpression(callee, List.toArray args, ?loc=range)
//        | None -> callFunction range callee args

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IStatementsCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues
        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr)::bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements)
            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccessAsExpr (com: IStatementsCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)

//    let transformDecisionTreeSuccessAsStatements (com: IStatementsCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement[] =
//        match returnStrategy with
//        | Some(Target targetId) as target ->
//            let idents, _ = getDecisionTarget ctx targetIndex
//            let assignments =
//                matchTargetIdentAndValues idents boundValues
//                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
//                    assign None (identAsExpr id) value |> ExpressionStatement)
//            let targetAssignment = assign None (targetId |> Expression.Identifier) (ofInt targetIndex) |> ExpressionStatement
//            Array.append [|targetAssignment|] assignments
//        | ret ->
//            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
//            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
//            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqualStrict, expr, right), _, _) ->
                Some(expr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, None), None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | _ -> false
        let rec checkInner cases evalExpr = function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _)
                                    when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                    Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr -> checkInner ((caseExpr, targetIndex, boundValues)::cases) evalExpr treeExpr
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            match checkInner [caseExpr, targetIndex, boundValues] evalExpr treeExpr with
            | Some(evalExpr, cases, defaultCase) ->
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let transformDecisionTreeAsExpr (com: IStatementsCompiler) (ctx: Context) targets expr =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) (defaultIndex, defaultBoundValues) =
        cases
        |> List.groupBy (fun (_,idx,boundValues) ->
            // Try to group cases with some target index and empty bound values
            // If bound values are non-empty use also a non-empty Guid to prevent grouping
            if List.isEmpty boundValues
            then idx, System.Guid.Empty
            else idx, System.Guid.NewGuid())
        |> List.map (fun ((idx,_), cases) ->
            let caseExprs = cases |> List.map Tuple3.item1
            // If there are multiple cases, it means boundValues are empty
            // (see `groupBy` above), so it doesn't mind which one we take as reference
            let boundValues = cases |> List.head |> Tuple3.item3
            caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t))
        |> function
            | [] -> []
            // Check if the last case can also be grouped with the default branch, see #2357
            | cases when List.isEmpty defaultBoundValues ->
                match List.splitLast cases with
                | cases, (_, Fable.DecisionTreeSuccess(idx, [], _))
                    when idx = defaultIndex -> cases
                | _ -> cases
            | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int,int>) = function
            | [] -> targetRefs
            | expr::exprs ->
                match expr with
                // We shouldn't actually see this, but shortcircuit just in case
                | Fable.DecisionTree _ ->
                    findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx,_,_) ->
                    let count =
                        Map.tryFind idx targetRefs
                        |> Option.defaultValue 0
                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = FableTransforms.getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)
        findSuccess Map.empty [expr] |> Seq.choose (fun kv ->
            if kv.Value > 1 then Some kv.Key else None) |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
//    let transformDecisionTreeWithTwoSwitches (com: IStatementsCompiler) ctx returnStrategy
//                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
//        // Declare target and bound idents
//        let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent
//        let multiVarDecl =
//            let boundIdents = targets |> List.collect (fun (idents,_) ->
//                idents |> List.map (fun id -> typedIdent com ctx id, None))
//            multiVarDeclaration Let ((typedIdent com ctx targetId, None)::boundIdents)
//        // Transform targets as switch
//        let switch2 =
//            // TODO: Declare the last case as the default case?
//            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
//            transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
//        // Transform decision tree
//        let targetAssign = Target(ident targetId)
//        let ctx = { ctx with DecisionTargets = targets }
//        match transformDecisionTreeAsSwitch treeExpr with
//        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
//            let cases = groupSwitchCases (Fable.Number(Int32, None)) cases (defaultIndex, defaultBoundValues)
//            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number(Int32, None))
//            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
//            [|multiVarDecl; switch1; switch2|]
//        | None ->
//            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
//            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

//    let transformDecisionTreeAsStatements (com: IStatementsCompiler) (ctx: Context) returnStrategy
//                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
//        // If some targets are referenced multiple times, hoist bound idents,
//        // resolve the decision index and compile the targets as a switch
//        let targetsWithMultiRefs =
//            if com.Options.Language = TypeScript then [] // no hoisting when compiled with types
//            else getTargetsWithMultipleReferences treeExpr
//        match targetsWithMultiRefs with
//        | [] ->
//            let ctx = { ctx with DecisionTargets = targets }
//            match transformDecisionTreeAsSwitch treeExpr with
//            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
//                let t = treeExpr.Type
//                let cases = cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->

    let transformValue (com: IStatementsCompiler) (ctx: Context) r value: Expression =
        let makeValue k = Value(k, r)

        match value with
//        | Fable.BaseValue(None,_) -> Super(None)
//        | Fable.BaseValue(Some boundIdent,_) -> identAsExpr boundIdent
//        | Fable.TypeInfo t -> transformTypeInfo com ctx r Map.empty t
        | Fable.ThisValue t -> ThisValue t |> makeValue
        | Fable.Null t -> Null t |> makeValue
        | Fable.UnitConstant -> Null Fable.Unit |> makeValue
        | Fable.CharConstant v -> CharConstant v |> makeValue
        | Fable.StringConstant v -> StringConstant v |> makeValue
        | Fable.BoolConstant v -> BoolConstant v |> makeValue
        | Fable.NumberConstant(v, k, uom) -> NumberConstant(v, k, uom) |> makeValue
        | _ -> addErrorAndReturnNull com None $"TODO: value %A{value}"

    let transformOperation com ctx range typ opKind: Expression =
        let makeOp k = Operation(k, typ, range)

        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) -> Unary(op, expr) |> makeOp
        | Fable.Binary(op, TransformExpr com ctx left, TransformExpr com ctx right) -> Binary(op, left, right) |> makeOp
        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) -> Logical(op, left, right) |> makeOp

    let rec transformAsExpr (com: IStatementsCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.Value(kind, r) -> let k = transformValue com ctx r kind in k

        | Fable.IdentExpr id -> IdentExpr id

        | Fable.Operation(kind, typ, r) -> transformOperation com ctx r typ kind

        | Fable.IfThenElse(TransformExpr com ctx guardExpr,
                           TransformExpr com ctx thenExpr,
                           TransformExpr com ctx elseExpr, r) ->
            Operation(Ternary(guardExpr, thenExpr, elseExpr), expr.Type, r)

        | _ -> addErrorAndReturnNull com None $"TODO: expression %A{expr}"

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IStatementsCompiler) ctx ret expr: Statement list =
        com.TransformAsStatements(ctx, ret, expr)

    let rec transformIfStatement (com: IStatementsCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        match com.TransformAsExpr(ctx, guardExpr) with
        | Value(BoolConstant value, _) ->
            if value then com.TransformAsStatements(ctx, ret, thenStmnt)
            else com.TransformAsStatements(ctx, ret, elseStmnt)
        | guardExpr ->
            let thenStmnt = transformBlock com ctx ret thenStmnt
            match elseStmnt with
            | Fable.Value(Fable.UnitConstant, _) ->
                [IfThenElse(guardExpr, thenStmnt, [], r)]
            | _ ->
                let elseStmnt = com.TransformAsStatements(ctx, ret, elseStmnt)
                [IfThenElse(guardExpr, thenStmnt, elseStmnt, r)]

    let rec transformAsStatements (com: IStatementsCompiler) ctx returnStrategy (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.Throw(TransformExpr com ctx e, isRethrow, t, r) ->
            [Throw(e, isRethrow, t, r) |> ExpressionStatement]

        | Fable.Value(kind, r) ->
            [transformValue com ctx r kind |> resolveExpr returnStrategy]

        | Fable.IdentExpr id ->
            [IdentExpr id |> resolveExpr returnStrategy]

        | Fable.Operation(kind, typ, r) ->
            [transformOperation com ctx r typ kind |> resolveExpr returnStrategy]

//                | Fable.IfThenElse(TransformExpr com ctx guardExpr,
//                           TransformExpr com ctx thenExpr,
//                           TransformExpr com ctx elseExpr, r) ->

        | Fable.Set(TransformExpr com ctx expr, kind, typ, TransformExpr com ctx value, r) ->
            let kind =
                match kind with
                | Fable.ExprSet(TransformExpr com ctx expr) -> ExprSet expr
                | Fable.FieldSet field -> FieldSet field
                | Fable.ValueSet -> ValueSet
            [Set(expr, kind, typ, value, r)]

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            let asStatement =
                match returnStrategy with
                | None | Some ReturnUnit -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) -> (isStatement ctx false thenExpr) || (isStatement ctx false elseExpr)
                | Some ReturnExpr ->
                    Option.isSome ctx.TailCallOpportunity
                    || (isStatement ctx false thenExpr) || (isStatement ctx false elseExpr)
            if asStatement then
                transformIfStatement com ctx r returnStrategy guardExpr thenExpr elseExpr
            else
                let guardExpr' = transformAsExpr com ctx guardExpr
                let thenExpr' = transformAsExpr com ctx thenExpr
                let elseExpr' = transformAsExpr com ctx elseExpr
                [Operation(Ternary(guardExpr', thenExpr', elseExpr'), expr.Type, r) |> resolveExpr returnStrategy]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                com.TransformAsStatements(ctx, ret, statement))
            |> List.concat

        | _ -> [addErrorAndReturnNull com None $"TODO: statement %A{expr}" |> resolveExpr returnStrategy]

    let rec transformDeclaration (com: IStatementsCompiler) ctx decl =
        let withCurrentScope (ctx: Context) (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with

        | Fable.MemberDeclaration memb ->
            withCurrentScope ctx memb.UsedNames <| fun ctx ->
//                if memb.Info.IsValue then // TODO
                {
                    Name = memb.Name
                    FullDisplayName = memb.FullDisplayName
                    Args = memb.Args
                    Body = transformAsStatements com ctx (Some ReturnExpr) memb.Body
                    Type = memb.Body.Type
                    Info = memb.Info
                    UsedNames = memb.UsedNames
                }
                |> MemberDeclaration
                |> List.singleton

        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        // TODO
        | Fable.ActionDeclaration _
        | Fable.ClassDeclaration _ -> []

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then None
        else
            match selector with
            | "*" | "default" -> Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            |> getUniqueNameInRootScope ctx
            |> Some

module Compiler =
    open Util

    type StatementsCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string,Import>()

        interface IStatementsCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            // TODO: the returned expression should be typed
            member _.GetImportExpr(ctx, selector, path, r) =
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> makeIdent localIdent |> IdentExpr
                    | None -> makeUntypedNull()
                | false, _ ->
                    let localId = getIdentForImport ctx path selector
                    let i =
                      { Selector =
                            if selector = Naming.placeholder then
                                     "`importMember` must be assigned to a variable"
                                     |> addError com [] r; selector
                            else selector
                        Path = path
                        LocalIdent = localId }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> makeIdent localId |> IdentExpr // TODO: type
                    | None -> makeUntypedNull()
            member _.GetAllImports() = imports.Values |> Seq.toList
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = failwith "todo" //transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = failwith "todo" //transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.ProjectFile = com.ProjectFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = StatementsCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IStatementsCompiler
        let declScopes =
            let hs = HashSet()
            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
          { File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = Unchecked.defaultof<_> }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        com.GetAllImports(), rootDecls
