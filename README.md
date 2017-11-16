# Subtractor

A model of a minimalistic processor architecture with a formal verification backend.

Inspired by [OISCs](https://en.wikipedia.org/wiki/One_instruction_set_computer).

The instruction set has only 6 commands:

* `ld Register MemoryAddress` loads a value to a register from a given memory address.
* `ld_si Register SImm8` loads a 8-bit signed immediate value to a register.
* `st Register MemoryAddress` stores a value from a register to a given memory address.
* `sub Register MemoryAddress` subtracts a value located in the memory from one in a register.
* `jmpi SImm10` performs an unconditional jump (*to be upgraded to conditional*).
* `halt` stops the machine operation.
