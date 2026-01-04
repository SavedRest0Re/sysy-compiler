  .text
  .global main
main:
  li t0, 1
  sw t0, 0(sp)

  lw t0, 0(sp)
  sw t0, 4(sp)

  lw t0, 4(sp)
  li t1, 1
  add t2, t0, t1
  sw t2, 8(sp)
  
  lw a0, 8(sp)
  addi sp, sp, 16
  ret
