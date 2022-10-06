# LambdaVM - A Programmable Virtual CPU Written as an Untyped Lambda Calculus Term

![Lambda calculus diagram for LambdaVM.](bin/lambdavm-diagram.png)

LambdaVM is a programmable virtual CPU written as a closed untyped lambda calculus term.
It supports an extended version of the [ELVM](https://github.com/shinh/elvm) instruction set and architecture written by [Shinichiro Hamaji](https://github.com/shinh).
Using LambdaVM, you can enjoy assembly programming to write programs in untyped lambda calculus.


LambdaVM supports 8 instructions including standard I/O and virtual memory operations, and has an arbitrarily configurable ROM/RAM address size and word size, and an arbitrarily configurable number of registers.

Despite its rather rich capability, LambdaVM's lambda term is quite small. Here is its entire lambda term written in plaintext:

```text
LambdaVM = \x.\y.\z.\a.\b.((\c.((\d.((\e.((\f.((\g.((\h.(a ((\i.(i (d (\j.\k.(k 
(\l.\m.\n.\o.(o k (j m))) k)) a) (\j.(i z (d (\k.\l.\m.\n.\o.\p.((\q.((\r.((\s.(
n (\t.\u.\v.\w.v) (\t.t) (\t.\u.\v.u) (\t.\u.u) (o (\t.\u.\v.(o (k l m) p)) o) (
n (\t.\u.((\v.(t (\w.\A.\B.((\C.(A (C B) (s B C))) (\C.\D.(w (D ((\E.(m (\F.\G.\
H.(E (y (\I.\J.(J (\K.\L.K) I)) F) G)) (E c m))) (\E.\F.(r B E (k l F u o)))) (\
E.(E (y (\F.(F (\G.\H.H))) C) (v p))) A) (D (\E.\F.\G.\H.((\I.(F (I G) (s G I)))
 (s H (\I.\J.(E (e I C) (q J) (v p))))))) (D (\E.\F.((\G.(f (\H.\I.I) (E (s F e 
C)) G G (\H.(r F)))) c)) v) (q C) (h l C (r D) v) (s D (g l C) k m u o p) (D (\E
.\F.(s E (f F F) C (\G.(r E)))) v) (r D C v))))))) (k l m u o)))))) (h p))) (g p
))) (\q.(h j q (\r.(r (k l m) p))))))))))) (\i.\j.(d (\k.\l.\m.\n.(l (\o.\p.\q.(
m (\r.\s.\t.(k l s (\u.\v.(k v s (\w.(n (\A.(A u w)))))))) (l n))) (n l l))) i c
 (\k.\l.(j k)))) b) (\i.\j.j))) (d (\h.\i.\j.\k.(i (\l.\m.\n.(j (\o.\p.\q.(o (h 
l) (h m) p k)) (k i))) (k c)))))) (d (\g.\h.\i.\j.\k.(i (\l.\m.\n.((\o.(h (\p.\q
.\r.(l (h o) (o q p))) (o (\p.\q.q) (\p.\q.q)))) (\o.(g o m j (\p.\q.(l (k (\r.(
r p q))) (k (\r.(r q p))))))))) (k j)))))) (d (\f.\g.\h.\i.\j.\k.(i (\l.\m.\n.(j
 (\o.\p.(f g h m p (\q.\r.((\s.((\t.((\u.((\v.(t s q (v (\w.\A.w)) (v (\w.\A.A))
)) (t q (q (\v.\w.w) (\v.\w.v)) (u (\v.\w.v)) (u (\v.\w.w))))) (\u.\v.(k v (\w.(
w u r)))))) (\t.\u.(l (s t u) (s u t))))) (h o (o (\s.\t.t) (\s.\t.s))))))))) (k
 g i)))))) (d (\e.\f.\g.(f (\h.\i.\j.(g (\k.\l.((\m.(h (k m (\n.\o.\p.o)) (k (\n
.\o.\p.p) m))) (e i l))))) (\h.\i.\j.h)))))) (\d.((\e.(d (e e))) (\e.(d (e e))))
))) ((\c.(y c (x c (\d.\e.e)))) (\c.\d.(d (\e.\f.e) c))))
```

Shown here is a lambda calculus term featuring a RAM unit with 8 instructions including I/O and memory operations.
With LambdaVM, you can write an assembly program that runs on this virtual machine.

The image on the top of this repo is LambdaVM's [lambda calculus diagram](http://tromp.github.io/cl/diagrams.html).

Various designs for LambdaVM are borrowed from [Kunihiko Sakamoto](https://github.com/irori)'s [UnlambdaVM](https://irori.hatenablog.com/entry/elvm-unlambda-part2) (in Japanese), with many modifications. Details are described later.

LambdaVM is built using [LambdaCraft](https://github.com/woodrush/lambdacraft), a Common Lisp DSL that I wrote for building large lambda calculus programs, also used to build [LambdaLisp](https://github.com/woodrush/lambdalisp), a Lisp interpreter implemented in untyped lambda calculus.


## Overview
### Lambda Calculus as a Programming Language
Using LambdaVM, you can write lambda calculus programs in assembly style.
But what does it mean to write programs in lambda calculus?

Lambda calculus terms can be interpreted as programs, by interpreting it as a program that takes an input string and returns an output string.
Characters and bytes are encoded as a list of bits with $0 = \lambda x. \lambda y.x$, $1 = \lambda x. \lambda y.y$,
and lists are encoded in the [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding) with ${\rm cons} = \lambda x.\lambda y.\lambda f.(f x y)$, ${\rm nil} = \lambda x.\lambda y.y$.
This way, _everything_ in the computation process, even including integers, is expressed as pure lambda terms,
without the need of introducing any non-lambda type object whatsoever.

Various lambda calculus interpreters automatically handle this I/O format so that it runs on the terminal - standard input is encoded into lambda terms, and the output lambda term is decoded and shown on the terminal.
Using these interpreters, lambda calculus programs can be run on the terminal just like any other terminal utility with I/O.

A thorough explanation of programming in lambda calculus is described in [my blog post](https://woodrush.github.io/blog/lambdalisp.html) about [LambdaLisp](https://github.com/woodrush/lambdalisp), a Lisp interpreter written as an untyped lambda calculus term.


### Programming Languages Based on Lambda Calculus
There are various programming languages that use this I/O strategy. Examples are:

- [Binary lambda calculus](https://tromp.github.io/cl/Binary_lambda_calculus.html)
- [Universal Lambda](https://esolangs.org/wiki/Universal_Lambda)
- [Lazy K](https://tromp.github.io/cl/lazy-k.html)

Seen as a programming language, these languages are purely functional languages with lazy evaluation.
The "purely" part is emphasized even more than languages such as Haskell, since no primitive data types such as integers exist except for lambdas exist in these languages.
Each of these languages have a different I/O encoding and a different notation for expressing lambda calculus terms.
The I/O encoding can be absorbed by using a [wrapper](https://github.com/woodrush/lambdavm/blob/main/src/blc-clamb-wrapper.cl),
and the notation can be adapted by using a [compiler](https://github.com/woodrush/lambdacraft) to write lambdas in the expected format.

Using LambdaVM, you can write programs for these languages.
A FizzBuzz program written in LambdaVM assembly looks like [./examples/fizzbuzz.cl](https://github.com/woodrush/lambdavm/blob/main/examples/fizzbuzz.cl).

Compiled programs can be run on the terminal using various interpreters, including:

- SectorLambda, the [521-byte lambda calculus interpreter](https://justine.lol/lambda/) written by Justine Tunney
- The [IOCCC](https://www.ioccc.org/) 2012 ["Most functional"](https://www.ioccc.org/2012/tromp/hint.html) interpreter written by John Tromp
  (the [source](https://www.ioccc.org/2012/tromp/tromp.c) is in the shape of a λ)
- Universal Lambda interpreter [clamb](https://github.com/irori/clamb) and Lazy K interpreter [lazyk](https://github.com/irori/lazyk) written by Kunihiko Sakamoto


### Compiling C to Lambda Calculus
I have integrated LambdaVM into ELVM to implement ELVM's lambda calculus backend.
Using this backend, you can even compile interactive C code to LambdaVM's assembly.
Since ELVM implements its [own libc](https://github.com/shinh/elvm/tree/master/libc), you can even `#include <stdio.h>` and use library functions such as `printf` and `scanf`.

This is entirely contained in the realm of the ELVM repository. Please see the documentation for ELVM for details.
Instead of compiling C to assembly, you can enjoy hand-assembling programs for lambda calculus using the examples in this repository.



## Features
- Instruction set:
  - `mov` `load` `store` `addsub` `cmp` `jmpcmp` `jmp` `putchar` `getchar` `exit`
  - The behavior of each instruction is mostly the same as ELVM. Please see [elvm.md](https://github.com/shinh/elvm/blob/master/ELVM.md) from the ELVM repo for details.
  - The lambda calculus data structure for each instruction is described later.
- ROM/RAM address size and word size:
  - Pre-configurable to an arbitrary integer
- I/O bit size:
  - Pre-configurable to an arbitrary integer
- Registers:
  - Word size is pre-configurable to an arbitrary integer
  - Number of registers is arbitrarily pre-configurable


## Usage
### Hand-Assembling Your Own LambdaVM Programs
You can hand-assemble your own LambdaVM programs using [LambdaCraft](https://github.com/woodrush/lambdacraft),
a Common Lisp DSL I wrote for building lambda calculus programs, also used to build [LambdaLisp](https://github.com/woodrush/lambdalisp).

The [examples](https://github.com/woodrush/lambdavm/tree/main/examples) directory in this repo contains 3 example LambdaVM assembly programs:

- [fizzbuzz.cl](https://github.com/woodrush/lambdavm/blob/main/examples/fizzbuzz.cl): Prints the FizzBuzz sequence in unary.
- [rot13.cl](https://github.com/woodrush/lambdavm/blob/main/examples/rot13.cl): Encodes/decodes standard input to/from the [ROT13](https://en.wikipedia.org/wiki/ROT13) cipher.
- [yes.cl](https://github.com/woodrush/lambdavm/blob/main/examples/yes.cl): The Unix `yes` command, printing infinite lines of `y`.

Here is what the beginning of the assembly listing for rot13.cl looks like:

```lisp
(def-lazy asm (list
  ;; Initialization (PC == 0)
  (list
    ;; Store 26/2 = 13 at reg-B
    (mov reg-B "N")
    (sub reg-B "A")
  )
  ;; tag-main (PC == 1)
  (list
    (getc reg-A)

    ;; Exit at EOF
    (jmpcmp reg-A == EOF -> tag-exit)

    ;; "a" <= reg-A < "n" : add 13
    (mov reg-C reg-A)
    (cmp reg-C >= "a")
    (mov reg-D reg-A)
    (cmp reg-D < "n")
    (add reg-C reg-D)
    (jmpcmp reg-C == int-2 -> tag-plus13)
...
```

As shown here, the assembly is written as Common Lisp macros.
These listings can be compiled by running *.cl on a Common Lisp interpreter such as SBCL.

Since these programs are based on LambdaCraft and LambdaCraft runs on LambdaLisp,
it is expected that these programs run on LambdaLisp as well, although it takes a lot of time compared to fast interpreters such as SBCL.

Please use these example programs as a template for hand-assembling your own LambdaVM programs.



## Specifications
LambdaVM is written as the following lambda calculus term:

$$
{\rm LambdaVM} = \lambda.{\rm iobitsize} ~ \lambda.{\rm suppbitsize} ~ \lambda.{\rm proglist} ~ \lambda.{\rm memlist} ~ \lambda.{\rm stdin} ~ \cdots
$$

- The first 2 arguments ${\rm iobitsize}$ and ${\rm suppbitsize}$ are configuration parameters specifying the CPU's I/O word size and RAM word size.
- ${\rm proglist}$ represents the assembly listing to be executed.
- ${\rm memlist}$ represents the memory initialization state. Unspecified memory regions are initialized to 0.
- ${\rm stdin}$ is the input string provided by the interpreter.

By applying the first 4 arguments except ${\rm stdin}$ to ${\rm LambdaVM}$, the combined lambda term
$({\rm LambdaVM} ~ {\rm iobitsize} ~ {\rm suppbitsize} ~ {\rm proglist} ~ {\rm memlist})$ behaves as a lambda calculus program that accepts a string
${\rm stdin}$, processes it, and returns some string.


### Implementation Design
Various designs for LambdaVM are borrowed from [Kunihiko Sakamoto](https://github.com/irori)'s [UnlambdaVM](https://irori.hatenablog.com/entry/elvm-unlambda-part2) (in Japanese):

- Using a binary tree structure to represent the RAM
- Using a list of lists of instructions to represent the program

LambdaVM has the following differences:
- While Unlambda is a strictly evaluated language, LambdaVM assumes a lazily evaluated language.
  While UnlambdaVM is written in direct style using function applications to mutate the VM's global state,
  LambdaVM is written using continuation-passing style to handle monadic I/O.
- The binary tree structure is modified so that an empty tree can be initialized with `nil = \x.\y.y`.


### Standard Input and Output
By loading the program and initialization configurations to LambdaVM, the resulting lambda calculus term $({\rm LambdaVM} ~ {\rm iobitsize} ~ {\rm suppbitsize} ~ {\rm proglist} ~ {\rm memlist})$ behaves as a function that accepts a string as an input and outputs a string.

Here, characters and bytes are encoded as a list of bits with $0 = \lambda x. \lambda y.x$, $1 = \lambda x. \lambda y.y$,
and lists are encoded in the [Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding) with ${\rm cons} = \lambda x.\lambda y.\lambda f.(f x y)$, ${\rm nil} = \lambda x.\lambda y.y$.

The variable ${\rm stdin}$, the last argument of ${\rm LambdaVM}$, is expected to be provided by the interpreter in this format.

Various lambda calculus languages use a slightly different I/O encoding, such as using [Church numerals](https://en.wikipedia.org/wiki/Church_encoding) to express each character.
Such differences can be absorbed by using a [wrapper](https://github.com/woodrush/lambdavm/blob/main/src/blc-clamb-wrapper.cl) that adapts the program to its surrounding environment.
This is discussed later in the [Adaptation to Different Languages](#adaptation-to-different-languages) section.


### iobitsize and suppbitsize
`iobitsize` and `suppbitsize` are integers encoded as lambda terms in the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding).

`iobitsize` specifies the number of bits used for the input string.
In the [IOCCC](https://www.ioccc.org/) 2012 ["Most functional"](https://www.ioccc.org/2012/tromp/hint.html) interpreter,
`iobitsize == 8` since it uses 8 bits for encoding the I/O.

`suppbitsize` represents the additional number of bits added to `iobitsize` to make the machine's RAM and register word size.
The word size becomes `iobitsize + suppbitsize`.

In ELVM, the machine word size is 24 and the I/O bit size is 8, so `iobitsize` and `suppbitsize` are set to 8 and 16, respectively.

### proglist
`proglist` is represented as a list of lists, where each sublist is a _tag_ containing a list of instructions.
The instruction format is described later.
The beginning of each list represents a tag that can be jumped to using the `jmp` or `jmpcmp` instructions.
When the `jmp` or `jmpcmp` instruction is run, the program proceeds to the beginning of the specified tag.

### memlist
`memlist` is represented as a list of N-bit unsigned integers with the machine's word size, where each integer is represented as a list of bits with
$0 = \lambda x. \lambda y.x$ and $1 = \lambda x. \lambda y.y$.
The elements of each list are assigned to contiguous RAM addresses startting from the address zero.
The rest of the memory is initiliazed with the integer zero.

## Instruction Structure
Each instruction ${\rm inst}$ is a 4-tuple containing 4 arguments:

$$
{\rm inst} = {\rm cons4} ~ {\rm insttag} ~ {\rm srcisimm} ~ {\rm src} ~ {\rm opt}
$$

- ${\rm cons4}$ is a 4-tuple constructor, ${\rm cons4} = \lambda x. \lambda y. \lambda z. \lambda w. \lambda f. (f x y z w)$.
- ${\rm insttag}$ is an 8-tuple specifying the instruction.
- ${\rm srcisimm}$ is a boolean of either ${\rm t} = \lambda x. \lambda y. y$ or ${\rm nil} = \lambda x. \lambda y. y$.
  It specifies whether if the following ${\rm src}$ is an immediate value or a register.
- ${\rm src}$ is either an N-bit integer or a register address.
- ${\rm opt}$ is an instruction-specific option with an instruction-specific data structure.

### insttag and opt
${\rm insttag}$ is used to specify the instruction type:

| Instruction | ${\rm insttag}$                                                                             |
|-------------|---------------------------------------------------------------------------------------------|
| `mov`       | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. a$ |
| `load`      | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. b$ |
| `store`     | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. c$ |
| `addsub`    | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. d$ |
| `cmp`       | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. e$ |
| `jmpcmp`    | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. f$ |
| `jmp`       | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. g$ |
| `io`        | $\lambda a. \lambda b. \lambda c. \lambda d. \lambda e. \lambda f. \lambda g. \lambda h. h$ |

| Instruction | ${\rm opt}$                                                            |
|-------------|------------------------------------------------------------------------|
| `mov`       | ${\rm dst}$                                                            |
| `load`      | ${\rm dst}$                                                            |
| `store`     | ${\rm dst}$                                                            |
| `addsub`    | ${\rm cons} ~ {\rm dst} ~ {\rm isadd}$                                 |
| `cmp`       | ${\rm cons} ~ {\rm enumcmp} ~ {\rm dst}$                               |
| `jmpcmp`    | ${\rm cons4} ~ {\rm enumcmp} ~ {\rm jmpisimm} ~ {\rm jmp} ~ {\rm dst}$ |
| `jmp`       | (unused)                                                               |
| `io`        | ${\rm enumio}$                                                         |


### Implementation in LambdaCraft
The examples under [./examples](https://github.com/woodrush/lambdavm/tree/main/examples) wrap this instruction structure as Lisp macros
so that they can be written in an assembly-like notation.
Macros are defined in [./src/lambda-asm-header.cl](https://github.com/woodrush/lambdavm/blob/main/src/lambda-asm-header.cl).
For further details on the instruction structure, please see this source.



## Adaptation to Different Languages
As mentioned earlier, various lambda calculus languages use a slightly different I/O encoding, such as using [Church numerals](https://en.wikipedia.org/wiki/Church_encoding) to express each character.
Such differences can be absorbed by using a [wrapper](https://github.com/woodrush/lambdavm/blob/main/src/blc-clamb-wrapper.cl) that adapts the program to its surrounding environment.

To run a lambda program $F = \lambda s. {\rm code}$ that uses an I/O encoding of method A in a different environment that uses an I/O encoding B,
 you can wrap $F$ by writing a wrapper ${\rm AtoB}$ and ${\rm BtoA}$:

$$
F' = \lambda s. ({\rm AtoB} ~ (F ~ ({\rm BtoA} ~ s)))
$$

When $F'$ is run in the environment B, the input string $s$ is first encoded in the method of B.
However, $F$ expects strings to be encoded in method A.
This difference in the input is first absorbed by the wrapper ${\rm BtoA}$ which translates $s$ to a format recognizable by $F$.

After calculuations, $F$ outputs a string in the method A.
However, this string is not recognizable by the surrounding environment B.
This difference in the output is then absorbed by the wrapper ${\rm AtoB}$ so that the interpreter recognizes the output by $F$.

Using this strategy, any program $F$ compiled in a lambda-like language A can easily be transplied to $F'$ that runs on a different language B.
The same holds for programs written using LambdaVM, making LambdaVM run in various lambda-based languages
such as Binary Lambda Calculus, Universal Lambda, and Lazy K.
The [wrapper](https://github.com/woodrush/lambdavm/blob/main/src/blc-clamb-wrapper.cl) used in LambdaVM is implemented exactly this way.

## Implementation Details
Please see [details.md](details.md).


## Credits
LambdaVM was written by Hikaru Ikuta, inspired by [Kunihiko Sakamoto](https://github.com/irori)'s [UnlambdaVM](https://irori.hatenablog.com/entry/elvm-unlambda-part2) (in Japanese).
The instruction set for LambdaVM is based on and is extended from the [ELVM](https://github.com/shinh/elvm) architecture written by [Shinichiro Hamaji](https://github.com/shinh).
LambdaVM is written using [LambdaCraft](https://github.com/woodrush/lambdacraft) written by Hikaru Ikuta.
