# LambdaVM
LambdaVM is a virtual computer with a CPU and a virtual RAM, all written as a single untyped lambda calculus term.
Machine code is written in the [ELVM](https://github.com/shinh/elvm) assembly set
with an arbitrary number of user-definable registers (the original ELVM has a fixed register set).
With the help of ELVM, you can compile interactive C code including `printf` and `scanf` to LambdaVM's assembly.
Code compiles to a single standalone lambda calculus term, which runs on the terminal via 5 supported lambda calculus interpreters that handle I/O.

The memory space's address size is arbitrary configurable, so it can run as 8-bit, 16-bit, or even 64-bit.

## Usage
