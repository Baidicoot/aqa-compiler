import dataclasses
import enum
from typing import *

class Stmt:
    pass

class Exp(Stmt):
    pass

class Val(Exp):
    pass

class Addr(Val):
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

@dataclasses.dataclass
class Assignment(Stmt):
    assigns: Addr
    assignexp: Exp

@dataclasses.dataclass
class Constant(Stmt):
    assigns: Addr
    assignval: Exp

@dataclasses.dataclass
class While(Stmt):
    stmts: list[Stmt]
    cond: Exp

@dataclasses.dataclass
class Label(Stmt):
    lbl: str

@dataclasses.dataclass
class Label(Stmt):
    label: str

@dataclasses.dataclass
class GotoCond(Stmt):
    lbl: str
    cond: str

@dataclasses.dataclass
class If(Stmt):
    cases: list[tuple[Exp,list[Stmt]]]

@dataclasses.dataclass
class Break(Stmt):
    pass

@dataclasses.dataclass
class Op(Exp):
    args: list[Exp]
    op: str

@dataclasses.dataclass
class IntLit(Val):
    val: int

@dataclasses.dataclass
class Var(Addr):
    var: str

@dataclasses.dataclass
class Index(Addr):
    ptr: Addr
    offset: Exp

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