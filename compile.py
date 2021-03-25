from stmt import *
from typing import *
import enum

VarNames = dict[str, str]
ConstState = dict[str, int]

def tmpNames():
    x = 0
    while True:
        yield str(x)
        x += 1

fresh = (lambda g : lambda n : "__" + n + "_" + g.__next__())(tmpNames())

BOOLNEG = {
    "leq": "ge",
    "geq": "le",
    "eq": "neq",
    "neq": "eq"
}

BOOLOPS = ["and","or","not","le","ge","leq","geq","eq","ne"]

def flattenExp(exp: Exp, renames: VarNames, outvar: str = None) -> tuple[list[Stmt],Val]:
    match exp:
        case Var(var=v):
            if outvar:
                return ([Assignment(outvar,Var(renames[v]))],Var(outvar))
            return ([],Var(renames[v]))
        case Op(args=a,op=o):
            stmts = []
            vals = []
            for a in a:
                stmt, val = flattenExp(a,renames)
                stmts.extend(stmt)
                vals.append(val)
            tmp = outvar if outvar else fresh("tmp")
            stmts.append(Assignment(Var(tmp),Op(args=vals,op=o)))
            return (stmts,Var(tmp))
        case IntLit():
            if outvar:
                return ([Assignment(Var(outvar),exp)],Var(outvar))
            return ([],exp)
        case Index(a,o):
            stmts, v = flattenExp(o,renames)
            stmts_, v_ = flattenExp(a,renames)
            tmp = outvar if outvar else fresh("tmp")
            return (stmts + stmts_ + [Assignment(Var(tmp),Index(v_,v))],Var(tmp))

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
                            if op in BOOLNEG:
                                return flattenCondJmp(Op(a,BOOLNEG[op]),lbl,renames)
                            elif op in BOOLOPS:
                                avoided = fresh("not")
                                n_cse = flattenCondJmp(Op(a,op),avoided,renames)
                                return n_cse + [GotoCOnd(lbl,""),Label(avoided)]
                    return flattenCondJmp(Op([a[0],IntLit(0)],"eq"),lbl,renames)
                case ("le" | "ge" | "eq" | "neq"):
                    l, r = a
                    stmts_l, lv = flattenExp(l,renames)
                    stmts_r, rv = flattenExp(r,renames)
                    return stmts_l + stmts_r + [Op([lv,rv],"cmp"),GotoCond(lbl,op)]
                case ("leq" | "geq"):
                    l, r = a
                    stmts_l, lv = flattenExp(l,renames)
                    stmts_r, rv = flattenExp(r,renames)
                    return stmts_l + stmts_r + flattenCondJmp(Op([Op([lv,rv],nop)],"not"),lbl,renames)
                case op:
                    return flattenCondJmp(Op([exp,IntLit(0)],"neq"),lbl,renames)
        case exp:
            return flattenCondJmp(Op([exp,IntLit(0)],"neq"),lbl,renames)

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
            loop_break = fresh("break")
            stmts_ = flattenCondJmp(Op([cond],"not"),loop_break,renames)
            stmts__ = flattenStmts(stmts,renames.copy())
            return [Label(loop_start)] + stmts_ + stmts__ + [GotoCond(loop_start,""),Label(loop_break)]
        case If(cases=cases):
            branches = [fresh("branch") for _ in cases]
            sel = []
            stmts = []
            for b,(cond,s) in zip(branches,cases):
                stmts_ = flattenCondJmp(Op([cond],"not"),b,renames)
                stmts__ = flattenStmts(s,renames.copy())
                stmts.extend(stmts_ + stmts__ + [Label(b)])
            return stmts
        case stmt:
            return [stmt]

def flattenStmts(stmts: list[Stmt],renames) -> list[Stmt]:
    stmts_ = []
    for s in map(lambda s : flattenStmt(s,renames),stmts):
        stmts_.extend(s)
    return stmts_

def lieftimesExp(e: Exp,i: int,d: dict[str,tuple[int,int]]):
    match e:
        case Op(args=a):
            for a in a:
                match a:
                    case Var(var=n):
                        d[n] = (d[n][0],i)
        case Var(var=n):
            d[n] = (d[n][0],i)
        case Index(a,o):
            lieftimesExp(a,i,d)
            lieftimesExp(o,i,d)

def lifetimes(stmts: list[Stmt]) -> dict[str,tuple[int,int]]:
    d = {}
    l = {}
    for i,s in enumerate(stmts):
        match s:
            case Label(lbl):
                l[lbl] = i
            case Assignment(assigns=v,assignexp=e):
                lieftimesExp(e,i,d)
                match v:
                    case Var(var=n):
                        if n not in d:
                            d[n] = (i,i)
                        else:
                            d[n] = (d[n][0],i)
                    case v:
                        lifetimesExp(v,i,d)
            
            case GotoCond(lbl=lbl):
                if lbl in l:
                    for n,(s,e) in d.items():
                        if s <= l[lbl] <= e:
                            d[n] = (s,i)
    return d

def interference(d: dict[str,tuple[int,int]]) -> dict[str, list[str]]:
    i = {}
    for v,(s,e) in d.items():
        i[v] = []
        for v_,(s_,e_) in d.items():
            if v == v_:
                continue
            if s <= s_ <= e or s <= e_ <= e or s_ <= s <= e_ or s_ <= e <= e_:
                i[v].append(v_)
    return i

def greedy(v: list[str], g: dict[str,list[str]]) -> dict[str, Register]:
    a = {}
    q = v
    while len(q) > 0:
        n = q.pop()
        for i in range(14):
            if i == 13:
                print("could not allocate registers!")
                exit()
            if all(map(lambda n : a.get(n) != i,g[n])):
                a[n] = i
                break
    return a

def generateAsmOp(e: Exp, regs: dict[str,Register]) -> AsmOp:
    match e:
        case Var(v):
            return Reg(regs[v])
        case Index(a,o):
            if isinstance(a,Var):
                return RegOffset(regs[a.var],generateAsmOp(o,regs))
        case IntLit(v):
            return IntLitAsm(v)

def generateAssign(lhs: Addr, rhs: Exp, regs: dict[str,Register]) -> [Asm]:
    match lhs:
        case Var(lhs):
            match rhs:
                case Op(args=a,op=op):
                    if len(a) == 2:
                        l, r = a
                        o = regs[lhs]
                        return [BinOp(op,generateAsmOp(l,regs),generateAsmOp(r,regs),o)]
                case Index(Var(v),IntLit(n)):
                    return [LdStrOp("ldr",regs[lhs],Indirect(regs[v],n))]
                case e:
                    return [UnOp("mov",generateAsmOp(e,regs),regs[lhs])]
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
                out.append(Misc(l+":"))
            case Op(args,op):
                if op == "cmp":
                    l,r = args
                    out.append(UnOp("cmp",generateAsmOp(r,regs),regs[l.var]))
    return out

def dispAsm(asm: [Asm]) -> str:
    out = ""
    for a in asm:
        out += a.generate() + "\n"
    return out

def compile(prg: list[Stmt]) -> str:
    flt = flattenStmts(prg,{})
    lf = lifetimes(flt)
    return dispAsm(generateAsm(flt,greedy(list(lf),interference(lf))))

program = [
    Assignment(Var("x"),IntLit(5)),
    Assignment(Var("y"),IntLit(0)),
    While([
        Assignment(Var("x"),Op([Var("x"),IntLit(1)],"sub")),
        Assignment(Var("y"),Op([Var("y"),IntLit(2)],"add"))
    ],Op([Var("x"),IntLit(0)],"neq"))
]

print(compile(program))