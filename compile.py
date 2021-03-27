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

def resolveName(name: str, renames: VarNames, symb: list[str]) -> Exp:
    if name in renames:
        return Var(renames[name])
    elif name in symb:
        return SymbLit(name)
    else:
        ERROR("unknown identifier '" + name + "'")

def flattenExp(exp: Exp, renames: VarNames, symb: list[str], outvar: str = None) -> tuple[list[Stmt],Val]:
    exp = constantFold(exp)
    match exp:
        case Var(var=v):
            val = resolveName(v,renames,symb)
            if outvar:
                return ([Assignment(Var(outvar),val)],Var(outvar))
            else:
                return ([],val)
        case Op(args=a,op=o):
            stmts = []
            vals = []
            tmp = outvar if outvar else fresh("tmp")
            for a in a:
                stmt, val = flattenExp(a,renames,symb)
                stmts.extend(stmt)
                vals.append(val)
            stmts.append(Assignment(Var(tmp),Op(args=vals,op=o)))
            return (stmts,Var(tmp))
        case Index(a,o):
            stmts, v = flattenExp(o,renames,symb)
            stmts_, v_ = flattenExp(a,renames,symb)
            tmp = outvar if outvar else fresh("tmp")
            return (stmts + stmts_ + [Assignment(Var(tmp),Index(v_,v))],Var(tmp))
        case Call(f,args):
            stmts, f_ = flattenExp(f,renames,symb)
            stmts.append(Op([],"slr"))
            for a in reversed(args[4:]):
                stmt, val = flattenExp(a,renames,symb)
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
                stmt, val = flattenExp(a,renames,symb,outvar=v)
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

def flattenCondJmp(exp: Exp, lbl: str, renames: VarNames,symb: list[str]) -> list[Stmt]:
    match exp:
        case Op(args=a,op=op):
            match op:
                case "and":
                    l, r = a
                    l_true = fresh("and")
                    fail = fresh("and")
                    l_cse = flattenCondJmp(l,l_true,renames,symb)
                    r_cse = flattenCondJmp(r,lbl,renames,symb)
                    return l_cse + [GotoCond(fail,""),Label(l_true)] + r_cse + [Label(fail)]
                case "or":
                    l, r = a
                    l_cse = flattenCondJmp(l,lbl,renames,symb)
                    r_cse = flattenCondJmp(r,lbl,renames,symb)
                    return l_cse + r_cse
                case "not":
                    match a[0]:
                        case Op(args=a,op=op):
                            if op == "not":
                                return flattenCondJmp(a[0],lbl,renames,symb)
                            elif op in BOOL_NEG:
                                return flattenCondJmp(Op(a,BOOL_NEG[op]),lbl,renames,symb)
                            elif op in BOOL_OPS:
                                avoided = fresh("B")
                                n_cse = flattenCondJmp(Op(a,op),avoided,renames,symb)
                                return n_cse + [GotoCond(lbl,""),Label(avoided)]
                    return flattenCondJmp(Op([a[0],IntLit(0)],"eq"),lbl,renames,symb)
                case ("lt" | "gt" | "eq" | "ne"):
                    l, r = a
                    stmts_l, lv = flattenExp(l,renames,symb)
                    stmts_r, rv = flattenExp(r,renames,symb)
                    return stmts_l + stmts_r + [Op([lv,rv],"cmp"),GotoCond(lbl,op)]
                case ("leq" | "geq"):
                    return flattenCondJmp(Op([Op([a[0],a[1]],BOOL_NEG[op])],"not"),lbl,renames,symb)
                case op:
                    return flattenCondJmp(Op([exp,IntLit(0)],"ne"),lbl,renames,symb)
        case exp:
            return flattenCondJmp(Op([exp,IntLit(0)],"ne"),lbl,renames,symb)

def flattenStmt(stmt: Stmt, renames: VarNames, symb: list[str]) -> list[Stmt]:
    match stmt:
        case Assignment(Index(a,o),e):
            stmts = []
            idx = None
            match o:
                case IntLit(i):
                    stmts, a_ = flattenExp(a,renames,symb)
                    av = None
                    match a_:
                        case Var(v):
                            av = v
                        case a_:
                            av = fresh("tmp")
                            stmts.append(Assignment(Var(av),a_))
                    idx = Index(Var(av),IntLit(i))
                case o:
                    stmts, i_ = flattenExp(Op([a,o],"add"),renames,symb)
                    iv = None
                    match i_:
                        case Var(v):
                            iv = v
                        case i_:
                            iv = fresh("tmp")
                            stmts.append(Assignment(Var(iv),i_))
                    idx = Index(Var(iv),IntLit(0))
            stmts_, e_ = flattenExp(e,renames,symb)
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
            stmts, v_ = flattenExp(e,renames,symb,outvar=renames[v])
            return stmts
        case While(stmts=stmts,cond=cond):
            loop_start = fresh("L")
            loop_cmp = fresh("C")
            stmts_ = flattenCondJmp(cond,loop_start,renames,symb)
            stmts__ = flattenStmts(stmts,renames.copy(),symb)
            return [GotoCond(loop_cmp,""),Label(loop_start)] + stmts__ + [Label(loop_cmp)] + stmts_
        case Repeat(stmts=stmts,cond=cond):
            loop_start = fresh("L")
            stmts_ = flattenCondJmp(Op([cond],"not"),loop_start,renames,symb)
            stmts__ = flattenStmts(stmts,renames.copy(),symb)
            return [Label(loop_start)] + stmts__ + stmts_
        case For(var=iv,start=s,end=e,stmts=stmts):
            stmts_,e_ = flattenExp(e,renames,symb)
            match e_:
                case Var(ev):
                    renames[ev] = ev
            stmts__ = flattenStmt(Assignment(Var(iv),s),renames,symb)
            loop = flattenStmt(
                While(stmts + [Assignment(Var(iv),Op([Var(iv),IntLit(1)],"add"))],
                    Op([Var(iv),e_],"lt")),
                renames,symb)
            return stmts_ + stmts__ + loop
        case If(cases=cases,dflt=dflt):
            branches = [fresh("C") for _ in cases]
            end = fresh("B")
            sel = []
            stmts = []
            for b,(cond,s) in zip(branches,cases):
                stmts_ = flattenCondJmp(Op([cond],"not"),b,renames,symb)
                stmts__ = flattenStmts(s,renames.copy(),symb)
                stmts.extend(stmts_ + stmts__ + [GotoCond(end,""),Label(b)])
            stmts.extend(dflt)
            stmts.append(Label(end))
            return stmts
        case Return(v):
            tmp = fresh("tmp")
            stmts,v_ = flattenExp(v,renames,symb,outvar=tmp)
            stmts.append(Return(v_))
            return stmts
        case Exp():
            return flattenExp(stmt,renames,symb)[0]
        case stmt:
            return [stmt]

def flattenStmts(stmts: list[Stmt],renames: VarNames, symb: list[str]) -> list[Stmt]:
    stmts_ = []
    for s in map(lambda s : flattenStmt(s,renames,symb),stmts):
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
            lifetimesExp(a,i,d,c)
            lifetimesExp(o,i,d,c)
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

def colorAlloc(stmts: list[Stmt], forced: dict[str,Register]={}, lf: dict[str,tuple[int,int]] = {}) -> tuple[list[Stmt],dict[str,Register]]:
    while True:
        inf,a = lifetimes(stmts,d=lf.copy(),c=forced)
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

def generateAssign(lhs: Addr, rhs: Exp, regs: dict[str,Register],stk: int) -> [Asm]:
    match lhs:
        case Var(lhs):
            match rhs:
                case Op(args=a,op=op):
                    if op == "pop":
                        return [MonOp("pop",Reg(regs[lhs]))]
                    elif op == "local":
                        return [LdStrOp("ldr",regs[lhs],Local(a[0].val + stk))]
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

def generateAsm(stmts: list[Stmt], regs: dict[str,Register],stk: int = 0) -> [Asm]:
    out = []
    for stmt in stmts:
        match stmt:
            case Assignment(assigns=a,assignexp=e):
                out.extend(generateAssign(a,e,regs,stk))
            case GotoCond(l,c):
                out.extend([Branch(c,l)])
            case Label(l):
                out.append(LabelDec(l))
            case Return():
                pass # `ret` automatically appended
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

def saveRegs(stmts: list[Stmt],regs: dict[str,Register],name: str,entry: bool = False) -> list[Asm]:
    tosave = list(set([r for r in regs.values() if r > 3 and not entry]))
    asm = [LabelDec(name)]
    for r in tosave:
        asm.append(MonOp("push",Reg(r)))
    asm.extend(generateAsm(stmts,regs,len(tosave)))
    for r in reversed(tosave):
        asm.append(MonOp("pop",Reg(r)))
    asm.append(NullOp("hlt" if entry else "ret"))
    return asm

def compileFunc(f: Function,symb: list[str],entry: bool) -> list[Asm]:
    renames,forced,lf = {},{},{}
    stmts = []
    for i,a in enumerate(f.args):
        renames[a] = fresh("arg_" + a)
        if i < 4:
            forced[renames[a]] = i
            lf[renames[a]] = (-1,-1)
        else:
            stmts.append(Assignment(Var(a),Op([IntLit(i-4)],"local")))
    stmts = stmts + f.stmts
    flt = flattenStmts(stmts,renames,symb)
    flt,regs = colorAlloc(flt,forced,lf)
    return saveRegs(flt,regs,f.name,entry)

def compileProgram(entry: str, decls: list[Function]) -> list[Asm]:
    symb = [f.name for f in decls]
    asm = []
    for f in decls:
        if f.name == entry:
            asm = compileFunc(f,symb,True) + asm
        else:
            asm.extend(compileFunc(f,symb,False))
    return asm

def dispAsm(asm: [Asm],target: str) -> str:
    out = ""
    for a in asm:
        out += a.generate(GenCfg(target)) + "\n"
    return out

if __name__ == "__main__":
    import sys
    import parse
    if len(sys.argv) == 1:
        ERROR("please specify an input file")
    elif len(sys.argv) == 2:
        ERROR("please specify a target")
    with open(sys.argv[1]) as fp:
        source = fp.read()
        program = parse.parse(source).value
        entry = sys.argv[3] if len(sys.argv) > 3 else "main"
        asm = compileProgram(entry,program)
        print(dispAsm(asm,sys.argv[2]))