# Subtractor

A model of a minimalistic processor architecture with a formal verification backend.

Inspired by [OISCs](https://en.wikipedia.org/wiki/One_instruction_set_computer).

## Project organization

Subtractor is a very simple architecture. It has 4 general purpose registers, 2 flags and 7 commands.

The project is split into following modules:

* `Subtractor.Types` module contains the Haskell types encoding the domain entities: memory, registers, instructions, etc.

* `Subtractor.State` module contains the representation of the Subtrator's state.

* `Subtractor.Assembly` module contains an embedded monadic combinator library designed to build Subtractor programs using Haskell's `do`-notation.

* `Subtractor.Semantics` module contains the semantics of the instructions, i.e. how does each instruction transforms the machine state.

## Instruction set

The instruction set has 7 commands defined in `Subtractor.Assembly`:

* `load Register MemoryAddress` loads a value to a register from a given memory address.
* `loadMI Register MemoryAddress` loads a value to a register from a given memory address using the memory indirect access mode.
* `set Register SImm8` loads an 8-bit signed immediate value to a register.
* `store Register MemoryAddress` stores a value from a register to a given memory address.
* `subtract Register MemoryAddress` subtracts a value located in the memory from one in a register.
* `jump SImm10` performs an unconditional jump.
* `jumpZero SImm10` performs a jump if `Zero` flag is set.
* `halt` stops the machine operation.

These commands later get desugared into constructors of `Subtractor.Types.Instruction` data type.



