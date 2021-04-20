# parsita-based pseudocode syntax parser

from stmt import *
from parsita import *
from parsita.util import constant

def compose(f, g): return lambda x: f(g(x))

op_map = {
    "+": "add",
    "-": "sub",
    "*": "mul",
    "%": "mod",
    "≤": "leq",
    "≥": "geq",
    "<": "lt",
    ">": "gt"
}

def map_expr(x):
    start, end = x
    if end == None: return start
    opname = end[0]
    if opname == "index":
        return Index(start, end[1])
    elif opname == "call":
        return Call(start, end[1:])
    else:
        return Op([start, end[1]], op_map.get(end[0], end[0]))

def map_unop_expr(x):
    return Op(x[1], x[0])

def map_if(x):
    def map_ifc(x):
        if x == None:
            return [], []
        elif x[0] == "ELSE":
            return [], x[1]
        else:
            elif_cases, else_cases = map_ifc(x[4])
            return [(x[1], x[3])] + elif_cases, else_cases
    elif_cases, else_case = map_ifc(x[4])
    return If([
        (x[1], x[3]),
    ] + elif_cases, else_case)

def map_subroutine(x):
    return Function(x[3], x[5], x[1])

def aliases(name, aliases):
    p = lit(name)
    for alias in aliases:
        p |= (lit(alias) > (lambda _: name))
    return p

class PseudocodeParser(TextParsers):
    ε = lit("") > (lambda _: None)
    IntLit = reg("\-?[0-9]+") > compose(IntLit, int)
    StrLit = "'" >> reg("[^']*") << "'" > StrLit # TODO escapes (not in "spec" but could be needed)
    FloatLit = reg("\-?[0-9]+\.[0-9]+") > compose(FloatLit, float)
    ArrayLit = "[" >> repsep(Expr, ",") << "]" > ArrayLit
    Identifier = reg("[a-zA-Z_]+[a-zA-Z_0-9]*")
    VarUse = Identifier > Var
    BracketedExpr = "(" >> Expr << ")"
    UnaryOperator = lit("NOT")
    Start = FloatLit | StrLit | IntLit | ArrayLit | BracketedExpr | (UnaryOperator & Expr > map_unop_expr) | VarUse
    # avoid left recursion problems by not doing left recursion
    # AQA pseudocode does not appear to have a notion of "operator precedence", simplifying parsing logic nicely
    Index = "[" >> Expr << "]" > (lambda x: ["index", x])
    Call = "(" >> repsep(Expr, ",") << ")" > (lambda x: ["call"] + x)
    BinaryOperator = aliases("≤", ["<="]) | aliases("≠", ["!="]) | aliases("≥", [">="]) | lit("DIV") | lit("MOD") | lit("AND") | lit("OR") | reg("[+/*\-=<>]")
    End = ((BinaryOperator & Expr) | Index | Call) | ε
    Expr = (Start & End) > map_expr

    AssignmentOp = lit("<-") | lit("←")
    Assignment = (Identifier > Var) & AssignmentOp & Expr > (lambda l: Assignment(l[0], l[2]))
    IndexAssignment = ((Start & Index) > map_expr) & AssignmentOp & Expr > (lambda l: Assignment(l[0], l[2]))
    ConstAssignment = lit("constant") & Identifier & AssignmentOp & Expr > (lambda l: Constant(l[1], l[3]))
    RepeatUntil = lit("REPEAT") & Statements & lit("UNTIL") & Expr > (lambda l: Repeat(l[1], l[3]))
    While = lit("WHILE") & Expr & Statements & lit("ENDWHILE") > (lambda l: While(l[2], l[1]))
    For = lit("FOR") & Identifier & AssignmentOp & Expr & lit("TO") & Expr & Statements & lit("ENDFOR") > (lambda l: For(l[1], l[3], l[5], l[6]))
    IfContinuation = (lit("ELSE IF") & Expr & lit("THEN") & Statements & IfContinuation) | (lit("ELSE") & Statements) | ε
    If = lit("IF") & Expr & lit("THEN") & Statements & IfContinuation & lit("ENDIF") > map_if
    Return = lit("RETURN") >> Expr > Return
    Statement = Return | For | If | While | RepeatUntil | Assignment | ConstAssignment | IndexAssignment | ((Start & Call) > map_expr)
    Statements = rep(Statement)
    Subroutine = lit("SUBROUTINE") & Identifier & lit("(") & repsep(Identifier, lit(",")) & lit(")") & Statements & lit("ENDSUBROUTINE") > map_subroutine
    TopLevel = Subroutine | Assignment | ConstAssignment
    Program = rep(TopLevel)

parse = PseudocodeParser.Program.parse
if __name__ == "__main__":
    x = parse("1+2+3 != 6 AND NOT 4 AND x + y + [1,2, 3][5]")
    if isinstance(x, Failure):
        print(x.message)
    else:
        print(x.value)
    x = PseudocodeParser.Program.parse("""
    SUBROUTINE thing(a, b, c)
        x ← 4
        y ← apify(a,b,c)
        RETURN LEN(a)
    ENDSUBROUTINE
    x ← 4
    FOR I <- 1 TO 3
        WHILE 4
            REPEAT bees ← 4 UNTIL 5
            IF i < 1248124 THEN
                REPEAT a ← b UNTIL b
            ENDIF
            IF i < 4 THEN
                y ← 4
            ELSE
                y ← 5
            ENDIF
            IF i < 9 THEN
                y ← 4
            ELSE IF i < 10 THEN
                y ← 5
            ELSE IF i < 11 THEN
                y ← 6
            ENDIF
        ENDWHILE
    ENDFOR
    """.strip())
    if isinstance(x, Failure):
        print(x.message)
    else:
        print(x.value)
        print("\n".join(map(lambda x: x.show(), x.value)))