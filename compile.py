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

VarRenames = dict[str, str]
Constants = dict[str, Lit]
StaticAlloc = int

@dataclasses.dataclass
class CompilerState:
    frames: list[VarRenames]
    symbols: list[str]
    consts: Constants
    stalloc: StaticAlloc

    def newframe(self):
        self.frames.append({})
    
    def popframe(self):
        self.frames.pop()
    
    def lookupVar(self,var: str) -> str | type(None):
        for i in range(len(self.frames)-1,-1,-1):
            if var in self.frames[i]:
                return self.frames[i][var]
        return None
    
    def lookupConst(self,const: str) -> Lit | type(None):
        if const in self.consts:
            return self.consts[const]
        return None
    
    def allocArray(self,size: int):
        self.stalloc -= size
        return self.stalloc
    
    def isSymb(self,var: str) -> bool:
        return var in self.symbols
        
    def assignVar(self,var: str,new: str):
        self.frames[-1][var] = new

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

# new expression simplifier
# 1st pass: constant fold + constant insert + name resolution
# 2nd pass: expression flattener

def resolveName(name: str, state: CompilerState) -> Exp:
    if state.lookupVar(name) != None:
        return Var(state.lookupVar(name))
    elif state.isSymb(name):
        return SymbLit(name)
    elif state.lookupConst(name) != None:
        return state.lookupConst(name)
    else:
        ERROR("unknown identifier '" + name + "'")

def constantFold(exp: Exp,state: CompilerState) -> Exp:
    match exp:
        case Var(v):
            return resolveName(v,state)
        case Op(exps,op):
            exps_ = [constantFold(e,state) for e in exps]
            if op in INT_FOLD_OPS:
                match exps_:
                    case [IntLit(lv),IntLit(rv)]:
                        return IntLit(INT_FOLD_OPS[op](lv,rv))
            return Op(exps_,op)
        case Call(fn,exps):
            fn_ = constantFold(fn,state)
            exps_ = [constantFold(e,state) for e in exps]
            return Call(fn_,exps_)
        case Index(ptr,off):
            ptr_ = constantFold(ptr,state)
            off_ = constantFold(off,state)
            return Index(ptr_,off_)
        case exp:
            return exp

def constantFold_(exp: Exp) -> Exp:
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

def flattenExp(exp: Exp,state: CompilerState, static: bool = False, outvar: str = None) -> tuple[list[Stmt],Exp]:
    match exp:
        case ArrayLit(exps):
            subexps = [flattenExp(e,state,static) for e in exps]
            out = []
            addr = None
            if static:
                addr = IntLit(state.allocArray(len(exps)))
            else:
                ERROR("no dynamic allocations at present")
            for index,(inits,exp) in enumerate(subexps):
                out.extend(flattenAssignments(Assignment(Index(addr,IntLit(index)),exp),state))
            return (out,addr)
        case Op(args,op):
            tmp = outvar if outvar else fresh("tmp")
            stmts,vals = [list(x) for x in zip(*[flattenExp(e,state,static) for e in args])]
            return (stmts + [Assignment(Var(tmp),Op(vals,op))],Var(tmp))
        case Index(ptr,off):
            stmts, v = flattenExp(ptr,state,static)
            stmts_, v_ = flattenExp(off,state,static)
            tmp = outvar if outvar else fresh("tmp")
            return (stmts + stmts_ + [Assignment(Var(tmp),Index(v,v_))],Var(tmp))
        case Call(f,args):
            stmts, f_ = flattenExp(f,state,static)
            stmts.append(Op([],"slr"))
            for a in reversed(args[4:]):
                stmt,val = flattenExp(a,state,static)
                stmts.extend(stmt)
                match val:
                    case Var(v):
                        stmts.append(Op([Var(v)],"push"))
                    case val:
                        tmp = fresh("tmp")
                        stmts.extend([Assignment(Var(tmp),val),Op([Var(tmp)],"push")])
            # argument variables to simplify register allocation
            regargs = []
            for a in args[:4]:
                v = fresh("tmp")
                stmt, val = flattenExp(a,state,static,outvar=v)
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

def flattenExp_(exp: Exp, state: CompilerState, outvar: str = None) -> tuple[list[Stmt],Exp]:
    exp = constantFold(exp)
    match exp:
        case Var(var=v):
            val = resolveName(v,state)
            if outvar:
                return ([Assignment(Var(outvar),val)],Var(outvar))
            else:
                return ([],val)
        case Op(args=a,op=o):
            stmts = []
            vals = []
            tmp = outvar if outvar else fresh("tmp")
            for a in a:
                stmt, val = flattenExp(a,state)
                stmts.extend(stmt)
                vals.append(val)
            stmts.append(Assignment(Var(tmp),Op(args=vals,op=o)))
            return (stmts,Var(tmp))
        case Index(a,o):
            stmts, v = flattenExp(o,state)
            stmts_, v_ = flattenExp(a,state)
            tmp = outvar if outvar else fresh("tmp")
            return (stmts + stmts_ + [Assignment(Var(tmp),Index(v_,v))],Var(tmp))
        case Call(f,args):
            stmts, f_ = flattenExp(f,state)
            stmts.append(Op([],"slr"))
            for a in reversed(args[4:]):
                stmt, val = flattenExp(a,state)
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
                stmt, val = flattenExp(a,state,outvar=v)
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

def simplifyExp(exp: Exp,state: CompilerState, static: bool = False, outvar: str = None) -> tuple[list[Stmt],Exp]:
    exp_ = constantFold(exp,state)
    return flattenExp(exp_,state,static,outvar)

def flattenBoolExp(exp: Exp, lbl: str, state: CompilerState) -> list[Stmt]:
    match exp:
        case Op(args=a,op=op):
            match op:
                case "and":
                    l, r = a
                    l_true = fresh("and")
                    fail = fresh("and")
                    l_cse = flattenBoolExp(l,l_true,state)
                    r_cse = flattenBoolExp(r,lbl,state)
                    return l_cse + [GotoCond(fail,""),Label(l_true)] + r_cse + [Label(fail)]
                case "or":
                    l, r = a
                    l_cse = flattenBoolExp(l,lbl,state)
                    r_cse = flattenBoolExp(r,lbl,state)
                    return l_cse + r_cse
                case "not":
                    match a[0]:
                        case Op(args=a,op=op):
                            if op == "not":
                                return flattenBoolExp(a[0],lbl,state)
                            elif op in BOOL_NEG:
                                return flattenBoolExp(Op(a,BOOL_NEG[op]),lbl,state)
                            elif op in BOOL_OPS:
                                avoided = fresh("B")
                                n_cse = flattenBoolExp(Op(a,op),avoided,state)
                                return n_cse + [GotoCond(lbl,""),Label(avoided)]
                    return flattenBoolExp(Op([a[0],IntLit(0)],"eq"),lbl,state)
                case ("lt" | "gt" | "eq" | "ne"):
                    l, r = a
                    stmts_l, lv = simplifyExp(l,state)
                    stmts_r, rv = simplifyExp(r,state)
                    return stmts_l + stmts_r + [Op([lv,rv],"cmp"),GotoCond(lbl,op)]
                case ("leq" | "geq"):
                    return flattenBoolExp(Op([Op([a[0],a[1]],BOOL_NEG[op])],"not"),lbl,state)
                case op:
                    return flattenBoolExp(Op([exp,IntLit(0)],"ne"),lbl,state)
        case exp:
            return flattenBoolExp(Op([exp,IntLit(0)],"ne"),lbl,state)

def flattenStmt(stmt: Stmt, state: CompilerState) -> list[Stmt]:
    match stmt:
        case Assignment(Var(v),e):
            if state.lookupVar(v) == None:
                state.assignVar(v,fresh("user_" + v))
            stmts, v_ = simplifyExp(e,state,outvar=state.lookupVar(v))
            return stmts
        case Assignment(a,e):
            stmts, a_ = simplifyExp(a,state)
            stmts_,e_ = simplifyExp(e,state)
            return (stmts + stmts_ + [Assignment(a_,e_)])
        case While(stmts=stmts,cond=cond):
            loop_start = fresh("L")
            loop_cmp = fresh("C")
            stmts_ = flattenBoolExp(cond,loop_start,state)
            state.newframe()
            stmts__ = flattenStmts(stmts,state)
            state.popframe()
            return [GotoCond(loop_cmp,""),Label(loop_start)] + stmts__ + [Label(loop_cmp)] + stmts_
        case Repeat(stmts=stmts,cond=cond):
            loop_start = fresh("L")
            stmts_ = flattenBoolExp(Op([cond],"not"),loop_start,state)
            state.newframe()
            stmts__ = flattenStmts(stmts,state)
            state.popframe()
            return [Label(loop_start)] + stmts__ + stmts_
        case For(var=iv,start=s,end=e,stmts=stmts):
            stmts_,e_ = simplifyExp(e,state)
            match e_:
                case Var(ev):
                    state.assignVar(ev,ev)
            stmts__ = flattenStmt(Assignment(Var(iv),s),state)
            loop = flattenStmt(
                While(stmts + [Assignment(Var(iv),Op([Var(iv),IntLit(1)],"add"))],
                    Op([Var(iv),e_],"lt")),
                state)
            return stmts_ + stmts__ + loop
        case If(cases=cases,dflt=dflt):
            branches = [fresh("C") for _ in cases]
            end = fresh("B")
            sel = []
            stmts = []
            for b,(cond,s) in zip(branches,cases):
                stmts_ = flattenBoolExp(Op([cond],"not"),b,state)
                state.newframe()
                stmts__ = flattenStmts(s,state)
                state.popframe()
                stmts.extend(stmts_ + stmts__ + [GotoCond(end,""),Label(b)])
            stmts.extend(dflt)
            stmts.append(Label(end))
            return stmts
        case Return(v):
            tmp = fresh("tmp")
            stmts,v_ = simplifyExp(v,state,outvar=tmp)
            stmts.append(Return(v_))
            return stmts
        case Exp():
            return simplifyExp(stmt,state)[0]
        case stmt:
            return [stmt]

def flattenAssignments(stmt: Stmt, state: CompilerState) -> list[Stmt]:
    match stmt:
        case Assignment(Index(a,o),e):
            rhs = None
            out = []
            match e:
                case Var(v):
                    rhs = e
                case e:
                    rhs = Var(fresh("tmp"))
                    out.append(Assignment(rhs,e))
            match (a,o):
                case (IntLit(a),Var(v)):
                    out.append(Assignment(Index(Var(v),IntLit(a)),rhs))
                case (Var(a),Var(o)):
                    tmp = fresh("tmp")
                    out.extend([
                        Assignment(Var(tmp),Op([Var(a),Var(v)],"add")),
                        Assignment(Index(Var(tmp),IntLit(0)),rhs)])
                case (a,o):
                    out.append(Assignment(Index(a,o),rhs))
            return out
        case stmt:
            return [stmt]

def flattenStmts(stmts: list[Stmt],state: CompilerState) -> list[Stmt]:
    return [l for s in stmts for r in flattenStmt(s,state) for l in flattenAssignments(r,state)]

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
                c[outvar] = 0
            lifetimesExp(f,i,d,c)

def lifetimes(stmts: list[Stmt], d: dict[str,tuple[int,int]] = {}, c: dict[str,Register] = {}) -> tuple[dict[str,tuple[int,int]],dict[str,Register]]:
    l = {}
    for i,s in enumerate(stmts):
        match s:
            case Label(lbl):
                l[lbl] = i
            case Assignment(assigns=v,assignexp=e):
                match v:
                    case Var(var=n):
                        lifetimesExp(e,i,d,c,outvar=n)
                        if n not in d:
                            d[n] = (i,i)
                        else:
                            d[n] = (d[n][0],i)
                    case v:
                        lifetimesExp(e,i,d,c)
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
        case Index(IntLit(i),IntLit(o)):
            match rhs:
                case Var(rhs):
                    return [LdStrOp("str",regs[rhs],Direct(i+o))]

def generateAsm(stmts: list[Stmt], regs: dict[str,Register],name: str,entry: bool) -> [Asm]:
    tosave = list(set([r for r in regs.values() if r > 3 and not entry]))
    out = [LabelDec(name)]
    for r in tosave:
        out.append(MonOp("push",Reg(r)))
    stk = len(tosave)
    returned = False
    for stmt in stmts:
        returned = False
        match stmt:
            case Assignment(assigns=a,assignexp=e):
                out.extend(generateAssign(a,e,regs,stk))
            case GotoCond(l,c):
                out.extend([Branch(c,l)])
            case Label(l):
                out.append(LabelDec(l))
            case Return():
                returned = True
                if entry:
                    out.append(NullOp("hlt"))
                else:
                    if stk > 0:
                        out.append(MonOp("drop",AsmLit(IntLit(stk))))
                        stk = 0
                    out.append(NullOp("ret"))
            case Op(args,op):
                if op == "cmp":
                    l,r = args
                    out.append(UnOp("cmp",generateAsmOp(r,regs),regs[l.var]))
                elif op == "push":
                    a = args[0]
                    out.append(MonOp("push",generateAsmOp(a,regs)))
                    stk += 1
                elif op == "drop":
                    stk -= args[0].val
                    out.append(MonOp("drop",AsmLit(args[0])))
                elif (op == "slr" or op == "rlr") and not entry:
                    stk += 1 if op == "slr" else -1
                    out.append(NullOp(op))
    if not returned:
        if stk - len(tosave) > 0:
            out.append(MonOp("drop",AsmLit(IntLit(stk - len(tosave)))))
        for r in reversed(tosave):
            out.append(MonOp("pop",Reg(r)))
        out.append(NullOp("hlt" if entry else "ret"))
    return out

def compileFunc(f: Function,state: CompilerState,entry: bool) -> list[Asm]:
    forced,lf = {},{}
    stmts = []
    state.newframe()
    for i,a in enumerate(f.args):
        state.assignVar(a,fresh("arg_" + a))
        if i < 4:
            forced[state.lookupVar(a)] = i
            lf[state.lookupVar(a)] = (-1,-1)
        else:
            stmts.append(Assignment(Var(a),Op([IntLit(i-4)],"local")))
    state.popframe()
    stmts = stmts + f.stmts
    flt = flattenStmts(stmts,state)
    flt,regs = colorAlloc(flt,forced,lf)
    return generateAsm(flt,regs,f.name,entry)

def partitionTLS(decls: list[Stmt]) -> tuple[list[Constant], list[Function]]:
    consts = []
    fns = []
    for s in decls:
        match s:
            case Constant():
                consts.append(s)
            case Function():
                fns.append(s)
    return (consts,fns)

def evalTLConstants(state: CompilerState, consts: list[Constant]) -> list[Stmt]:
    stmts = []
    for c in consts:
        stmts_,exp = simplifyExp(c.assignval,state,static=True)
        if isinstance(exp,Lit):
            state.consts[c.assigns] = exp
            stmts.extend([l for s in stmts_ for l in flattenAssignments(s,state)])
        else:
            ERROR("non-constant expression '" + c.assignval.show() + "' assigned to constant '" + c.assigns + "'")
    return stmts

def compileProgram(entry: str, decls: list[Function]) -> list[Asm]:
    consts,fns = partitionTLS(decls)
    symbols = [f.name for f in fns]
    state = CompilerState([{}],symbols,{},0)
    init = evalTLConstants(state,consts)
    asm = []
    for f in fns:
        if f.name == entry:
            init,regs = colorAlloc(init)
            initAsm = generateAsm(init,regs,"init",True)[:-1]
            mainAsm = compileFunc(f,state,True)
            asm = initAsm + mainAsm + asm
        else:
            asm.extend(compileFunc(f,state,False))
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