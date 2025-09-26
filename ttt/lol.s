# Very simple assembly program that will cause Spike to terminate gracefully.

.text
.global _start; _start:

addi gp, x0, 1

# Spin until Spike terminates the simulation.
1: j 1b

# Expose tohost and fromhost to Spike so we can communicate with it.
.data
.align 6; .global tohost;   tohost:   .dword 0
.align 6; .global fromhost; fromhost: .dword 0

