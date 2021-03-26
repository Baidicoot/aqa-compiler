import dataclasses
import enum
from typing import *

INDENT = 2

class Stmt:
    def show(self,i: int = 0) -> str:
        return "ERROR INVALID STATEMENT"

class Exp(Stmt):
    pass

class Addr(Exp):
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

def showRegister(r: Register) -> str:
    return "r" + str(r)

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
class Allocate(Stmt):
    data: list[Exp]
    var: Addr

    def show(self,_=0):
        return self.var.show() + " <- [" + ", ".join(map(lambda x: x.show(0),self.data)) + "]"

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
class IntLit(Exp):
    val: int

    def show(self,_=0):
        return str(self.val)

@dataclasses.dataclass
class StrLit(Exp):
    val: str

@dataclasses.dataclass
class FloatLit(Exp):
    val: float

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
    def generate(self) -> str:
        return ""

class AsmOp:
    def generate(self) -> str:
        return ""

class AsmLoc:
    def generate(self) -> str:
        return ""

@dataclasses.dataclass
class BinOp(Asm):
    op: str
    l: AsmOp
    r: AsmOp
    o: Register

    def generate(self):
        return self.op + " " + showRegister(self.o) + ", " + self.l.generate() + ", " + self.r.generate()

@dataclasses.dataclass
class UnOp(Asm):
    op: str
    src: AsmOp
    dst: Register

    def generate(self):
        return self.op + " " + showRegister(self.dst) + ", " + self.src.generate()

@dataclasses.dataclass
class LdStrOp(Asm):
    op: str
    reg: Register
    loc: AsmLoc

    def generate(self):
        return self.op + " " + showRegister(self.reg) + ", " + self.loc.generate()

@dataclasses.dataclass
class Branch(Asm):
    cond: str
    lbl: str

    def generate(self):
        return "b" + self.cond + " " + self.lbl

@dataclasses.dataclass
class Misc(Asm):
    asm: str
    
    def generate(self):
        return self.asm

@dataclasses.dataclass
class Reg(AsmOp):
    reg: Register

    def generate(self):
        return showRegister(self.reg)

@dataclasses.dataclass
class IntLitAsm(AsmOp):
    val: int

    def generate(self):
        return "#" + str(self.val)

@dataclasses.dataclass
class Direct(AsmLoc):
    addr: int
    
    def generate(self):
        return str(self.addr)

@dataclasses.dataclass
class Indirect(AsmLoc):
    reg: Register
    off: int
    
    def generate(self):
        return "[" + showRegister(self.reg) + " + " + str(self.off) + "]"