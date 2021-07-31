module rec Fable.AST.Statements

open Fable.AST.Fable

type OperationKind =
    | Logical of operator: LogicalOperator * left: Expression * right: Expression
    | Unary of operator: UnaryOperator * operand: Expression
    | Binary of operator: BinaryOperator * left: Expression * right: Expression
    | Ternary of guardExpr: Expression * thenExpr: Expression * elseExpr: Expression

type ValueKind =
    | ThisValue of typ: Type
//    | BaseValue of boundIdent: Ident option * typ: Type
    | Null of typ: Type
    | BoolConstant of value: bool
    | CharConstant of value: char
    | StringConstant of value: string
    | NumberConstant of value: float * kind: NumberKind * uom: string option
    member this.Type =
        match this with
        | ThisValue t
//        | BaseValue(_,t) -> t
        | Null t -> t
        | BoolConstant _ -> Boolean
        | CharConstant _ -> Char
        | StringConstant _ -> String
        | NumberConstant (_, kind, uom) -> Number(kind, uom)

type SetKind =
    | ExprSet of Expression
    | FieldSet of fieldName: string
    | ValueSet

type Expression =
    | IdentExpr of ident: Ident
    | Value of kind: ValueKind * range: SourceLocation option
//    | AnonymousFunction of args: Ident list * body: Expression * name: string option
//    | Call of callee: Expression * info: CallInfo * typ: Type * range: SourceLocation option
    | Operation of kind: OperationKind * typ: Type * range: SourceLocation option
    // Throw can be a expression or a statement depending on the language
    | Throw of expr: Expression * isRethrow: bool * typ: Type * range: SourceLocation option
    member this.Type =
        match this with
        | Value (kind, _) -> kind.Type
        | IdentExpr id -> id.Type
        | Operation (_, t, _)
        | Throw(_,_,t,_) -> t
//        | Call(_,_,t,_)

type Statement =
    | Return of Expression
    | ExpressionStatement of Expression
    | Break of label: string option
    | Set of Expression * kind: SetKind * typ: Type * value: Expression * range: SourceLocation option
    | IfThenElse of guardStat: Expression * thenStatements: Statement list * elseStatements: Statement list * range: SourceLocation option
//    | IdentDeclaration of ident: Ident * value: Expression
//    | Switch of Expression * cases: (Expression * Statement list) list * defaultCase: Statement list * range: SourceLocation option
//    | WhileLoop of guard: Expression * body: Expression * label: string option * range: SourceLocation option
//    | ForLoop of ident: Ident * start: Expression * limit: Expression * body: Expression * isUp: bool * range: SourceLocation option
//    | TryCatch of body: Expression * catch: (Ident * Expression) option * finalizer: Expression option * range: SourceLocation option

type MemberDecl = {
    Name: string
    FullDisplayName: string
    Args: Ident list
    Body: Statement list
    Type: Type
    Info: MemberInfo
    UsedNames: Set<string>
}

type Declaration =
    | MemberDeclaration of MemberDecl
