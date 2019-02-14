# Find the greatest common divisor of values in memory locations 0 and 1,
# put result to the register R1
Instruction (Set R0 0)
Instruction (Store R0 255)
Instruction (Load R0 1)
# Test register R0 for being zero by subtracting zero
Instruction (Sub R0 255)
# Halt if register R0 contains zero, loop otherwise
Instruction (JumpZero 6)
Instruction (Load R0 0)
Instruction (Mod R0 1)
Instruction (Load R1 1)
Instruction (Store R0 1)
Instruction (Store R1 0)
Instruction (Jump (-8))
Instruction Halt