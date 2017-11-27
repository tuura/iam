# Inglorious Adding Machine

Hi there! I am an Inglorious Adding Machine --- a model of a minimalistic processor
architecture with a formal verification backend.

My design was inspired by [OISCs](https://en.wikipedia.org/wiki/One_instruction_set_computer).

## Project organization

IAM is a very simple architecture. It has 4 general purpose registers, 2 flags and 7 commands.

The project is split into following modules:

* `Iam.Types` module contains the Haskell types encoding the domain entities: memory, registers, instructions, etc.

* `Iam.State` module contains the representation of the Subtrator's state.

* `Iam.Assembly` module contains an embedded monadic combinator library designed to build IAM programs using Haskell's `do`-notation.

* `Iam.Semantics` module contains the semantics of the instructions, i.e. how does each instruction transforms the machine state.

## Instruction set

The instruction set has 7 commands defined in `Iam.Assembly`:

* `load Register MemoryAddress` loads a value to a register from a given memory address.
* `loadMI Register MemoryAddress` loads a value to a register from a given memory address using the memory indirect access mode.
* `set Register SImm8` loads an 8-bit signed immediate value to a register.
* `store Register MemoryAddress` stores a value from a register to a given memory address.
* `add Register MemoryAddress` adds a value located in the memory to one in a register.
* `jump SImm10` performs an unconditional jump.
* `jumpZero SImm10` performs a jump if `Zero` flag is set.
* `halt` stops the machine operation.

These commands later get desugared into constructors of `Iam.Types.Instruction` data type.



