current operation - no parsing, compiler internals
```python
program = [
    Assignment(Var("x"),IntLit(5)),
    Assignment(Var("y"),IntLit(0)),
    While([
        Assignment(Var("x"),Op([Var("x"),IntLit(1)],"sub")),
        Assignment(Var("y"),Op([Var("y"),IntLit(2)],"add"))
    ],Op([Var("x"),IntLit(0)],"neq"))
]
```
compiles to:
```x86asm
mov r1, #5
mov r0, #0
__loop_2:
cmp r1, #0
beq __break_3
sub r1, r1, #1
add r0, r0, #2
b __loop_2
__break_3:
```