import dataclasses
import enum
from typing import *

INDENT = 2

def ERROR(msg):
    print("FATAL ERROR:",msg)
    exit(1)

@dataclasses.dataclass
class GenCfg:
    target: str

@dataclasses.dataclass
class Function:
    args: list[str]
    stmts: list[Stmt]
    name: str

    def show(self,i: int = 0) -> str:
        return "SUBROUTINE " + self.name + "(" + ", ".join(self.args) + ")\n" + shows(self.stmts,i+INDENT) + "\nEND"

class Stmt:
    def show(self,i: int = 0) -> str:
        return "ERROR INVALID STATEMENT"

class Exp(Stmt):
    pass

class Addr(Exp):
    pass

class Lit(Addr):
    pass

class Register(enum.Enum):
    R0 = 0
    R1 = 1
    R2 = 2
    R3 = 3
    R4 = 4
    R5 = 5
    R6 = 6
    R7 = 7
    R8 = 8
    R9 = 9
    R10 = 10
    R11 = 11
    R12 = 12
    PC = 13
    LR = 14
    SP = 15

def generateReg(r,cfg: GenCfg) -> str:
    if r == 13:
        if cfg.target == "aqa":
            ERROR("the program counter is unavailable in AQA assembly")
        return "pc"
    elif r == 14:
        if cfg.target == "aqa":
            ERROR("the link register is unavailable in AQA assembly")
        return "lr"
    elif r == 15:
        if cfg.target == "aqa":
            ERROR("the stack is unavailable in AQA assembly")
        return "sp"
    else:
        return "r" + str(r)

@dataclasses.dataclass
class RegisterAllocationFailure(Exception):
    failed: str
    assigned: dict[str, Register]
    graph: dict[str, list[str]]

def shows(ls, i: int = 0) -> str:
    return "\n".join(map(lambda x : (" " * i) + x.show(i),ls))

@dataclasses.dataclass
class Assignment(Stmt):
    assigns: Addr
    assignexp: Exp

    def show(self,_=0):
        return self.assigns.show() + " <- " + self.assignexp.show()

@dataclasses.dataclass
class Constant(Stmt):
    assigns: Addr
    assignval: Exp

    def show(self,_=0):
        return "constant " + self.assigns.show() + " <- " + self.assignval.show()

@dataclasses.dataclass
class While(Stmt):
    stmts: list[Stmt]
    cond: Exp

    def show(self,i=0):
        return "WHILE " + self.cond.show() + "\n" + shows(self.stmts,i+INDENT) + "\n" + " " * i + "END"

@dataclasses.dataclass
class For(Stmt):
    var: str
    start: Exp
    end: Exp
    stmts: list[Stmt]

    def show(self,i=0):
        return "FOR " + self.var + " <- " + self.start.show() + " TO " + self.end.show() + "\n" + shows(self.stmts,i+INDENT) + "\n" + " " * i + "END"

@dataclasses.dataclass
class Repeat(Stmt):
    stmts: list[Stmt]
    cond: Exp

    def show(self,i=0):
        return "REPEAT\n" + shows(self.stmts,i+INDENT) + "\n" + " " * i + "UNTIL " + self.cond.show()

@dataclasses.dataclass
class If(Stmt):
    cases: list[tuple[Exp,list[Stmt]]]
    dflt: list[Stmt]

    def show(self,_=0):
        pass

@dataclasses.dataclass
class Label(Stmt):
    lbl: str

    def show(self,_=0):
        return self.lbl + ":"

@dataclasses.dataclass
class GotoCond(Stmt):
    lbl: str
    cond: str

    def show(self,i=0):
        return "b" + self.cond + " " + self.lbl

@dataclasses.dataclass
class Op(Exp):
    args: list[Exp]
    op: str

    def show(self,_=0):
        return self.op + " " + ", ".join(map(lambda x : x.show(), self.args))

@dataclasses.dataclass
class IntLit(Lit):
    val: int

    def show(self,_=0):
        return str(self.val)

@dataclasses.dataclass
class StrLit(Lit):
    val: str

    def show(self,_=0):
        return "\"" + self.val + "\""

@dataclasses.dataclass
class SymbLit(Lit):
    symb: str

    def show(self,_=0):
        return self.symb

@dataclasses.dataclass
class FloatLit(Lit):
    val: float

    def show(self,_=0):
        return str(self.val)

@dataclasses.dataclass
class ArrayLit(Lit):
    data: list[Exp]

    def show(self,_=0):
        return "[" + ", ".join(map(lambda x: x.show(0),self.data)) + "]"

@dataclasses.dataclass
class Call(Exp):
    func: Exp
    args: list[Exp]

    def show(self,_=0):
        return self.func.show() + "(" + ", ".join(map(lambda x: x.show(),self.args)) + ")"

@dataclasses.dataclass
class Return(Stmt):
    val: Exp

    def show(self,_=0):
        return "RETURN " + self.val.show()

@dataclasses.dataclass
class Var(Addr):
    var: str

    def show(self,_=0):
        return self.var

@dataclasses.dataclass
class Index(Addr):
    ptr: Addr
    offset: Exp

    def show(self,_=0):
        return self.ptr.show() + "[" + self.offset.show() + "]"

class Asm:
    def generate(self,cfg: GenCfg) -> str:
        return ""

class AsmOp:
    def generate(self,cfg: GenCfg) -> str:
        return ""

class AsmLoc:
    def generate(self,cfg: GenCfg) -> str:
        return ""

@dataclasses.dataclass
class LabelDec(Asm):
    lbl: str

    def generate(self,cfg: GenCfg):
        return self.lbl + ":"

@dataclasses.dataclass
class BinOp(Asm):
    op: str
    l: AsmOp
    r: AsmOp
    o: Register

    def generate(self,cfg: GenCfg):
        return self.op + " " + generateReg(self.o,cfg) + ", " + self.l.generate(cfg) + ", " + self.r.generate(cfg)

@dataclasses.dataclass
class UnOp(Asm):
    op: str
    src: AsmOp
    dst: Register

    def generate(self,cfg: GenCfg):
        return self.op + " " + generateReg(self.dst,cfg) + ", " + self.src.generate(cfg)

@dataclasses.dataclass
class LdStrOp(Asm):
    op: str
    reg: Register
    loc: AsmLoc

    def generate(self,cfg: GenCfg):
        return self.op + " " + generateReg(self.reg,cfg) + ", " + self.loc.generate(cfg)

@dataclasses.dataclass
class Branch(Asm):
    cond: str
    lbl: str

    def generate(self,cfg: GenCfg):
        return "b" + self.cond + " " + self.lbl

@dataclasses.dataclass
class NullOp(Asm):
    op: str

    def generate(self,cfg: GenCfg):
        if self.op == "ret" and cfg.target == "aqa":
            ERROR("the stack is unavailable in AQA assembly")
        elif self.op == "slr":
            return "push {lr}"
        elif self.op == "rlr":
            return "pop {lr}"
        return self.op

@dataclasses.dataclass
class MonOp(Asm):
    op: str
    arg: AsmOp

    def generate(self,cfg: GenCfg):
        if self.op == "ib":
            if cfg.target == "aqa":
                ERROR("indirect branches are unavailable in AQA assembly")
            return "mov pc, " + self.arg.generate(cfg)
        elif self.op == "call":
            if cfg.target == "aqa":
                ERROR("procedure calls are unavailable in AQA assembly")
            return "mov lr, pc\nmov pc, " + self.arg.generate(cfg)
        elif self.op == "pop" or self.op == "push":
            if cfg.target == "aqa":
                ERROR("the stack is unavailable in AQA assembly")
            return self.op + " {" + self.arg.generate(cfg) + "}"
        elif self.op == "drop":
            if cfg.target == "aqa":
                ERROR("the stack is unavailable in AQA assembly")
            return "add sp, sp, #" + str(4 * self.arg.val.val)
        return self.op + " " + self.arg.generate(cfg)

@dataclasses.dataclass
class Reg(AsmOp):
    reg: Register

    def generate(self,cfg: GenCfg):
        return generateReg(self.reg,cfg)

@dataclasses.dataclass
class AsmLit(AsmOp):
    val: Lit

    def generate(self,cfg: GenCfg):
        return "#" + self.val.show()

@dataclasses.dataclass
class Direct(AsmLoc):
    addr: int
    
    def generate(self,cfg: GenCfg):
        return str(self.addr)

@dataclasses.dataclass
class Indirect(AsmLoc):
    reg: Register
    off: int
    
    def generate(self,cfg: GenCfg):
        return "[" + self.reg.generate(cfg) + " + " + str(self.off*4) + "]"

@dataclasses.dataclass
class Local(AsmLoc):
    off: int

    def generate(self,cfg: GenCfg):
        if cfg.target == "aqa":
            ERROR("the stack is unavailable in AQA assembly")
        return "[sp + " + str(self.off*4) + "]"

@dataclasses.dataclass
class LabelLoc(AsmLoc):
    lbl: str

    def generate(self,cfg: GenCfg):
        return self.lbl