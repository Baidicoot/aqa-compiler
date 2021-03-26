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
b __cmp_3
__loop_2:
sub r1, r1, #1
add r0, r0, #2
__cmp_3:
cmp r1, #0
bneq __loop_2
```