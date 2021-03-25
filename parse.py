# parsita-based pseudocode syntax parser

from stmt import *
from parsita import *
from parsita.util import constant

def compose(f, g): return lambda x: f(g(x))

def map_expr(x):
    start, end = x
    if end == "": return start
    return Op([start, end[1]], end[0])

def map_unop_expr(x):
    return Op(x[1], x[0])

def aliases(name, aliases):
    p = lit(name)
    for alias in aliases:
        p |= (lit(alias) > (lambda _: name))
    return p

class PseudocodeParser(TextParsers):
    ε = lit("")
    IntLit = reg("\-?[0-9]+") > compose(IntLit, int)
    StrLit = "'" >> reg("[^']*") << "'" > StrLit # TODO escapes (not in "spec" but could be needed)
    FloatLit = reg("\-?[0-9]+\.[0-9]+") > compose(FloatLit, float)
    ArrayLit = "[" >> repsep(Expr, ",") << "]" > ArrayLit
    Identifier = reg("[a-zA-Z_]+[a-zA-Z_0-9]*") > Var
    BracketedExpr = "(" >> Expr << ")"
    UnaryOperator = lit("NOT")
    Start = FloatLit | StrLit | IntLit | ArrayLit | BracketedExpr | (UnaryOperator & Expr > map_unop_expr) | Identifier
    # avoid left recursion problems by not doing left recursion
    # AQA pseudocode does not appear to have a notion of "operator precedence", simplifying parsing logic nicely
    Index = "[" >> Expr << "]" > (lambda x: ["[]", x])
    BinaryOperator = aliases("≤", ["<="]) | aliases("≠", ["!="]) | aliases("≥", [">="]) | lit("DIV") | lit("MOD") | lit("AND") | lit("OR") | reg("[+/*\-=<>]")
    End = ((BinaryOperator & Expr) | Index) | ε
    Expr = (Start & End) > map_expr

    AssignmentOp = lit("<-") | lit("←")
    Assignment = Identifier & AssignmentOp & Expr
    ConstAssignment = lit("constant") & Assignment
    RepeatUntil = lit("REPEAT") & Program & lit("UNTIL") & Expr
    While = lit("WHILE") & Expr & Program & lit("ENDWHILE")
    For = lit("FOR") & Identifier & AssignmentOp & Expr & lit("TO") & Expr & Program & lit("ENDFOR")
    IfBody = (lit("ELSE IF") & Expr & lit("THEN") & Program & IfBody) | (lit("ELSE") & Program) | ε
    If = lit("IF") & Expr & lit("THEN") & Program & IfBody & lit("ENDIF")
    Statement = For | If | While | RepeatUntil | Assignment | ConstAssignment
    Program = rep(Statement)

parse = PseudocodeParser.Expr.parse
x = parse("1+2+3 != 6 AND NOT 4 AND x + y + [1,2, 3][5]")
if isinstance(x, Failure):
    print(x.message)
else:
    print(x.value)
x = PseudocodeParser.Program.parse("""
FOR I <- 1 TO 3
    WHILE 4
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
        ENDIF
    ENDWHILE
ENDFOR
""".strip())
if isinstance(x, Failure):
    print(x.message)
else:
    print(x.value)