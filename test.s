main:
push {lr}
mov r0, #5
push {r0}
mov r0, #1
mov r1, #2
mov r2, #3
mov r3, #4
mov lr, pc
mov pc, #thing
add sp, sp, #4
pop {lr}
hlt
thing:
ldr r0, [sp + 0]
add r0, r3, r0
add r0, r2, r0
ret

