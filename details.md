## Implementation Details
Here I will explain further implementation details of LambdaVM.

### Binary Tree Structure for the RAM and the Registers
The binary tree structure is the same as my other project [LambdaLisp](https://github.com/woodrush/lambdalisp), a Lisp interpreter written as an untyped lambda calculus term.
Please see my [blog post for LambdaLisp](https://woodrush.github.io/blog/lambdalisp.html#virtual-heap-memory-ram) for details on the RAM data structure.

As the same with LambdaLisp, the registers share the same binary tree structure as the RAM, except registers can have a variable-length address, while the RAM has a fixed-length address.


### Eliminating the Need for a Program Counter
The core `eval` function keeps track of the current list of instructions in the current tag, and the current list of tags.
When the current tag list becomes empty, it fetches a new tag from the current list of tags,
and starts executing the first instruction from the fetched tag.
Remember that each tag is a list of instructions, and the list of tags represent the rest of the program.
LambdaVM stops its execution when no more tags are left in the current list of tags.
By this design, LambdaVM does not need to keep track of an explicit program counter, since the current instruction and the next tag can always be fetched from the current tag and current list.


### Shared Data Structures for the ROM and the RAM
The ROM and the RAM share the same binary tree structure.
When an uninitialized ROM/RAM address is referenced, the lookup function returns a list filled with bits of 0s, `zero = (list 0 0 0 ... 0)`.

Interpreted as a RAM lookup call, `zero` is naturally interpreted as the integer zero.
Interpreted as a ROM lookup call, `zero` is interpreted as a list of N empty tags with `N = iobitsize + suppbitsize`.
Here, a tag is a list of instructions representing each jumpable tag,
and in this case every jumpable tag is empty.
These empty tags are skipped, and eventually the list of tags becomes empty, exiting the execution of LambdaVM.
This way, when an uninitialized ROM address is referenced, LambdaVM exits the program, which is the expected behavior since the VM was commanded to jump to a nonexistent program counter.

By this design, the ROM and RAM are both able to share the same tree lookup function, where an empty reference returns the value `zero = (list 0 0 0 ... 0)`.
This design helps making the lambda calculus term size of LambdaVM to be small.
