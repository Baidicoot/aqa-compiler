from stmt import *
from typing import *
import enum

# `compile.py` is divided into two stages:
#
# - Flattening, where high-level constructs within functions,
#   such as loops, are converted to jumps, and expressions are
#   converted to series of assignments to temporary variables.
#   Constant folding operations occur here.
#
# - Register allocation, where variables are assigned registers,
#   and function calls are converted to jumps and stack operations.

# ARM Calling Convention:
# r0-3 used to pass arguments       (caller-saved)
# r4-12 used for local variables    (callee-saved)

VarNames = dict[str, str]
ConstState = dict[str, int]

def tmpNames():
    x = 0
    while True:
        yield str(x)
        x += 1

fresh = (lambda g : lambda n : "__" + n + "_" + g.__next__())(tmpNames())

BOOL_NEG = {
    "leq": "gt",
    "geq": "lt",
    "eq": "ne",
    "ne": "eq"
}

BOOL_OPS = ["and","or","not","lt","gt","leq","geq","eq","ne"]

INT_FOLD_OPS = {
    "add": lambda x, y : x + y,
    "sub": lambda x, y : x - y,
    "mul": lambda x, y : x * y,
    "div": lambda x, y : x // y,
    "mod": lambda x, y : x % y,
}

def constantFold(exp: Exp) -> Exp:
    match exp:
        case Op([l,r],("add" | "sub" | "mul" | "div" | "mod")):
            l_ = constantFold(l)
            r_ = constantFold(r)
            match (l_,r_):
                case (IntLit(lv),IntLit(rv)):
                    return IntLit(INT_FOLD_OPS[exp.op](lv,rv))
                case (l__,r__):
                    return Op([l__,r__], exp.op)
        case exp:
            return exp

def flattenExp(exp: Exp, renames: VarNames, outvar: str = None) -> tuple[list[Stmt],Val]:
    exp = constantFold(exp)
    match exp:
        case Var(var=v):
            if outvar:
                return ([Assignment(Var(outvar),Var(renames[v]))],Var(outvar))
            return ([],Var(renames[v]))
        case Op(args=a,op=o):
            stmts = []
            vals = []
            tmp = outvar if outvar else fresh("tmp")
            for a in a:
                stmt, val = flattenExp(a,renames)
                stmts.extend(stmt)
                vals.append(val)
            stmts.append(Assignment(Var(tmp),Op(args=vals,op=o)))
            return (stmts,Var(tmp))
        case Index(a,o):
            stmts, v = flattenExp(o,renames)
            stmts_, v_ = flattenExp(a,renames)
            tmp = outvar if outvar else fresh("tmp")
            return (stmts + stmts_ + [Assignment(Var(tmp),Index(v_,v))],Var(tmp))
        case Call(f,args):
            stmts, f_ = flattenExp(f,renames)
            stmts.append(Op([],"slr"))
            for a in reversed(args[4:]):
                stmt, val = flattenExp(a,renames)
                stmts.extend(stmt)
                match val:
                    case Var(v):
                        stmts.append(Op([Var(v)],"push"))
                    case val:
                        tmp = fresh("tmp")
                        stmts.extend([Assignment(Var(tmp),val),Op([Var(tmp)],"push")])
            # buffer variables needed to simplify register allocation
            regargs = []
            for a in args[:4]:
                v = fresh("tmp")
                stmt, val = flattenExp(a,renames,outvar=v)
                stmts.extend(stmt)
                regargs.append(val)
            ret = fresh("tmp")
            tmp = outvar if outvar else fresh("tmp")
            stmts.append(Assignment(Var(ret),Call(f_,regargs)))
            stmts.append(Assignment(Var(tmp),Var(ret)))
            if len(args) > 4:
                stmts.append(Op([IntLit(len(args) - 4)],"drop"))
            stmts.append(Op([],"rlr"))
            return (stmts,Var(tmp))
        case lit:
            if outvar:
                return ([Assignment(Var(outvar),lit)],Var(outvar))
            return ([],lit)

def flattenCondJmp(exp: Exp, lbl: str, renames: VarNames) -> list[Stmt]:
    match exp:
        case Op(args=a,op=op):
            match op:
                case "and":
                    l, r = a
                    l_true = fresh("and")
                    fail = fresh("and")
                    l_cse = flattenCondJmp(l,l_true,renames)
                    r_cse = flattenCondJmp(r,lbl,renames)
                    return l_cse + [GotoCond(fail,""),Label(l_true)] + r_cse + [Label(fail)]
                case "or":
                    l, r = a
                    l_cse = flattenCondJmp(l,lbl,renames)
                    r_cse = flattenCondJmp(r,lbl,renames)
                    return l_cse + r_cse
                case "not":
                    match a[0]:
                        case Op(args=a,op=op):
                            if op == "not":
                                return flattenCondJmp(a[0],lbl,renames)
                            elif op in BOOL_NEG:
                                return flattenCondJmp(Op(a,BOOL_NEG[op]),lbl,renames)
                            elif op in BOOL_OPS:
                                avoided = fresh("not")
                                n_cse = flattenCondJmp(Op(a,op),avoided,renames)
                                return n_cse + [GotoCond(lbl,""),Label(avoided)]
                    return flattenCondJmp(Op([a[0],IntLit(0)],"eq"),lbl,renames)
                case ("lt" | "gt" | "eq" | "ne"):
                    l, r = a
                    stmts_l, lv = flattenExp(l,renames)
                    stmts_r, rv = flattenExp(r,renames)
                    return stmts_l + stmts_r + [Op([lv,rv],"cmp"),GotoCond(lbl,op)]
                case ("leq" | "geq"):
                    l, r = a
                    stmts_l, lv = flattenExp(l,renames)
                    stmts_r, rv = flattenExp(r,renames)
                    return stmts_l + stmts_r + flattenCondJmp(Op([Op([lv,rv],op)],"not"),lbl,renames)
                case op:
                    return flattenCondJmp(Op([exp,IntLit(0)],"ne"),lbl,renames)
        case exp:
            return flattenCondJmp(Op([exp,IntLit(0)],"ne"),lbl,renames)

def flattenStmt(stmt: Stmt, renames: VarNames) -> list[Stmt]:
    match stmt:
        case Assignment(Index(a,o),e):
            stmts = []
            idx = None
            match o:
                case IntLit(i):
                    stmts, a_ = flattenExp(a,renames)
                    av = None
                    match a_:
                        case Var(v):
                            av = v
                        case a_:
                            av = fresh("tmp")
                            stmts.append(Assignment(Var(av),a_))
                    idx = Index(Var(av),IntLit(i))
                case o:
                    stmts, i_ = flattenExp(Op([a,o],"add"),renames)
                    iv = None
                    match i_:
                        case Var(v):
                            iv = v
                        case i_:
                            iv = fresh("tmp")
                            stmts.append(Assignment(Var(iv),i_))
                    idx = Index(Var(iv),IntLit(0))
            stmts_, e_ = flattenExp(e,renames)
            stmts.extend(stmts_)
            ev = None
            match e_:
                case Var(v):
                    ev = v
                case e_:
                    ev = fresh("tmp")
                    stmts.append(Assignment(Var(ev),e_))
            stmts.append(Assignment(idx,Var(ev)))
            return stmts
        case Assignment(Var(v),e):
            if v not in renames:
                renames[v] = fresh("user_" + v)
            stmts, v_ = flattenExp(e,renames,outvar=renames[v])
            return stmts
        case While(stmts=stmts,cond=cond):
            loop_start = fresh("loop")
            loop_cmp = fresh("cmp")
            stmts_ = flattenCondJmp(cond,loop_start,renames)
            stmts__ = flattenStmts(stmts,renames.copy())
            return [GotoCond(loop_cmp,""),Label(loop_start)] + stmts__ + [Label(loop_cmp)] + stmts_
        case Repeat(stmts=stmts,cond=cond):
            loop_start = fresh("loop")
            stmts_ = flattenCondJmp(Op([cond],"not"),loop_start,renames)
            stmts__ = flattenStmts(stmts,renames.copy())
            return [Label(loop_start)] + stmts__ + stmts_
        case For(var=iv,start=s,end=e,stmts=stmts):
            stmts_,e_ = flattenExp(e,renames)
            match e_:
                case Var(ev):
                    renames[ev] = ev
            stmts__ = flattenStmt(Assignment(Var(iv),s),renames)
            loop = flattenStmt(
                While(stmts + [Assignment(Var(iv),Op([Var(iv),IntLit(1)],"add"))],
                    Op([Var(iv),e_],"lt")),
                renames)
            return stmts_ + stmts__ + loop
        case If(cases=cases,dflt=dflt):
            branches = [fresh("elif") for _ in cases]
            end = fresh("endif")
            sel = []
            stmts = []
            for b,(cond,s) in zip(branches,cases):
                stmts_ = flattenCondJmp(Op([cond],"not"),b,renames)
                stmts__ = flattenStmts(s,renames.copy())
                stmts.extend(stmts_ + [GotoCond(end,"")] + stmts__ + [Label(b)])
            stmts.extend(dflt)
            stmts.append(Label(end))
            return stmts
        case Return(v):
            tmp = fresh("tmp")
            stmts,v_ = flattenExp(v,renames,outvar=tmp)
            stmts.append(Return(v_))
            return stmts
        case Exp():
            return flattenExp(stmt,renames)[0]
        case stmt:
            return [stmt]

def flattenStmts(stmts: list[Stmt],renames: VarNames) -> list[Stmt]:
    stmts_ = []
    for s in map(lambda s : flattenStmt(s,renames),stmts):
        stmts_.extend(s)
    return stmts_

def lifetimesExp(e: Exp,i: int,d: dict[str,tuple[int,int]], c: dict[str,Register], outvar: str = None):
    match e:
        case Op(args=a):
            for a in a:
                match a:
                    case Var(var=n):
                        d[n] = (d[n][0],i)
        case Var(var=n):
            d[n] = (d[n][0],i)
        case Index(a,o):
            lifetimesExp(a,i,d)
            lifetimesExp(o,i,d)
        case Call(f,a):
            for vi,v in enumerate(a):
                c[v.var] = vi
                d[v.var] = (d[v.var][0],i)
            for bi in range(len(a), 4):
                blocker = fresh("blk")
                c[blocker] = bi
                d[blocker] = (i,i)
            if outvar:
                d[outvar] = 0
            lifetimesExp(f,i,d,c)

def lifetimes(stmts: list[Stmt], d: dict[str,tuple[int,int]] = {}, c: dict[str,Register] = {}) -> tuple[dict[str,tuple[int,int]],dict[str,Register]]:
    l = {}
    for i,s in enumerate(stmts):
        match s:
            case Label(lbl):
                l[lbl] = i
            case Assignment(assigns=v,assignexp=e):
                lifetimesExp(e,i,d,c)
                match v:
                    case Var(var=n):
                        if n not in d:
                            d[n] = (i,i)
                        else:
                            d[n] = (d[n][0],i)
                    case v:
                        lifetimesExp(v,i,d,c)
            case GotoCond(lbl=lbl):
                if lbl in l:
                    for n,(s,e) in d.items():
                        if s <= l[lbl] <= e:
                            d[n] = (s,i)
            case Return(Var(v)):
                c[v] = 0
                d[n] = (d[n][0],i)
            case Exp():
                lifetimesExp(s,i,d,c)
    return (d,c)

def interference(d: dict[str,tuple[int,int]]) -> dict[str, list[str]]:
    i = {}
    for v,(s,e) in d.items():
        i[v] = []
        for v_,(s_,e_) in d.items():
            if v == v_:
                continue
            if s <= s_ < e or s_ <= s < e_:
                i[v].append(v_)
    return i

def colorAlloc(stmts: list[Stmt], forced: dict[str,Register]={}) -> tuple[list[Stmt],dict[str,Register]]:
    while True:
        inf,a = lifetimes(stmts,c=forced)
        inf = interference(inf)
        rm,f = [],True
        lft = [x for x in list(inf) if x not in a]
        while f:
            f = False
            for n in lft:
                if n not in rm and n not in a and sum(map(lambda x: x not in rm,inf[n])) < 13:
                    f = True
                    lft.remove(n)
                    rm.append(n)
        if len(lft) > 0:
            # need to do a spill
            ERROR("spill required")
            break
        while len(rm) > 0:
            n = rm.pop()
            r = 0
            for i in range(13):
                if all(map(lambda x: a[x] != i if x in a else True,inf[n])):
                    r = i
                    break
            a[n] = r
        return (stmts,a)

def generateAsmOp(e: Exp, regs: dict[str,Register]) -> AsmOp:
    match e:
        case Var(v):
            return Reg(regs[v])
        case Index(a,o):
            if isinstance(a,Var):
                return RegOffset(regs[a.var],generateAsmOp(o,regs))
        case Lit():
            return AsmLit(e)

def generateAssign(lhs: Addr, rhs: Exp, regs: dict[str,Register],) -> [Asm]:
    match lhs:
        case Var(lhs):
            match rhs:
                case Op(args=a,op=op):
                    if op == "pop":
                        return [MonOp("pop",[Reg(regs[lhs])])]
                    elif op == "local":
                        return [LdStrOp("ldr",regs[lhs],Local(a[0].val))]
                    elif len(a) == 2:
                        l, r = a
                        o = regs[lhs]
                        return [BinOp(op,generateAsmOp(l,regs),generateAsmOp(r,regs),o)]
                case Index(Var(v),IntLit(n)):
                    return [LdStrOp("ldr",regs[lhs],Indirect(regs[v],n))]
                case Call(f,a):
                    return [MonOp("call",generateAsmOp(f,regs))]
                case e:
                    match generateAsmOp(e,regs):
                        case Reg(r):
                            if r == regs[lhs]:
                                return []
                            else:
                                return [UnOp("mov",Reg(r),regs[lhs])]
                        case e:
                            return [UnOp("mov",e,regs[lhs])]
        case Index(Var(v),IntLit(n)):
            match rhs:
                case Var(rhs):
                    return [LdStrOp("str",regs[rhs],Indirect(regs[v],n))]

def generateAsm(stmts: list[Stmt], regs: dict[str,Register]) -> [Asm]:
    out = []
    for stmt in stmts:
        match stmt:
            case Assignment(assigns=a,assignexp=e):
                out.extend(generateAssign(a,e,regs))
            case GotoCond(l,c):
                out.extend([Branch(c,l)])
            case Label(l):
                out.append(LabelDec(l))
            case Return():
                out.append(NullOp("ret"))
            case Op(args,op):
                if op == "cmp":
                    l,r = args
                    out.append(UnOp("cmp",generateAsmOp(r,regs),regs[l.var]))
                elif op == "push":
                    a = args[0]
                    out.append(MonOp("push",generateAsmOp(a,regs)))
                elif op == "drop":
                    out.append(MonOp("drop",AsmLit(args[0])))
                elif op == "slr" or op == "rlr":
                    out.append(NullOp(op))
    return out

def compileFunc(f: Function) -> [Asm]:
    renames = {}
    forced = {}
    stmts = [Label(f.name)]
    for i,a in enumerate(f.args):
        renames[a] = fresh("arg_" + a)
        if i < 4:
            forced[renames[a]] = i
        else:
            stmts.append(Assignment(Var(a),Op([IntLit(i-4)],"local")))
    stmts = stmts + f.stmts
    flt = flattenStmts(stmts,renames)
    flt,regs = colorAlloc(flt,forced)
    return generateAsm(flt,regs)

def compileProgram(entry: str, decls: list[Function]) -> [Asm]:
    asm = []
    for f in decls:
        if f.name == entry:
            asm = compileFunc(f) + [NullOp("hlt")] + asm
        else:
            asm.extend(compileFunc(f))
    return asm

def dispAsm(asm: [Asm]) -> str:
    out = ""
    for a in asm:
        out += a.generate(GenCfg("arm")) + "\n"
    return out

def compile(prg: list[Stmt],args: list[str] = []) -> str:
    renames = {}
    for a in args:
        renames[a] = fresh("arg_" + a)
    flt = flattenStmts(prg,renames)
    flt,regs = colorAlloc(flt)
    return dispAsm(generateAsm(flt,regs))

program = [
    Function(["a","b","c","d","x","y"],[
        While([
            Assignment(Var("x"),Op([Var("x"),IntLit(1)],"sub")),
            Assignment(Var("y"),Op([Var("y"),IntLit(2)],"add"))
        ],Op([Var("x"),IntLit(0)],"ne")),
        Return(Var("y"))
    ],"addDouble"),
    Function([],[
        Assignment(Var("x"),IntLit(5)),
        Assignment(Var("y"),IntLit(2)),
        Assignment(Var("z"),Call(SymbLit("addDouble"),[IntLit(0),IntLit(0),IntLit(0),IntLit(0),Var("x"),Var("y")]))
    ],"main")
]

print(shows(program))
print("\ncompiles to:\n")
print(dispAsm(compileProgram("main",program)))