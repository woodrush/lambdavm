(load "./src/lambda-asm-header.cl")


;; Numbers are in big-endian, with t == 0, nil == 1
;; (Raw integer literals `0`, `1`, etc. are bound to different lambda forms in ./lambdacraft.cl)
(def-lazy int-0 (list t t t t t t t t))


;; The assembly is a list of lists of instructions.
;; Each sublist defines a chunk bound with a program counter, starting from PC == 0.
;; When a jump instruction is executed, the instruction pointer transitions to the first instruction of the given program counter.
(def-lazy asm (list
  ;; Initialization (PC == 0)
  (list
    (putc "y")
    (putc "\\n")
    (jmp int-0)
  )
))



;; The number of bits used for I/O.
;; Binary lambda calculus supplies a list of lists of 8-bit-encoded characters.
(def-lazy SYS-IO-BITS 8)
;; The number of supplementary bits to prepend to the I/O bits, to be used for the machine's word size.
;; Machine word size = SYS-IO-BITS + SYS-SUPPLEMENTARY-BITS
(def-lazy SYS-SUPPLEMENTARY-BITS 0)
(def-lazy initial-memory nil)

(def-lazy standalone
  ;; All binary lambda calculus programs are functions that accept a string (stdin) and return a string.
  (lambda (stdin)
    (lambdaVM SYS-IO-BITS SYS-SUPPLEMENTARY-BITS initial-memory asm stdin)))

(format t (compile-to-blc-lazy standalone))
