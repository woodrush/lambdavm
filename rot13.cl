(load "./lambda-asm-header.cl")


;; Define your own register addresses
(def-lazy reg-A (list t t))
(def-lazy reg-B (list t nil))
(def-lazy reg-C (list nil t))
(def-lazy reg-D (list nil nil))
;; Used for the macros in ./lambda-asm-header.cl, to determine if a value is an immediate or a register
(defparameter regnames '(reg-A reg-B reg-C reg-D))

;; Numbers are in big-endian, with t == 0, nil == 1
;; (Raw integer literals `0`, `1`, etc. are bound to different lambda forms in ./lambdacraft.cl)
(def-lazy int-0 (list t t t t t t t t))
(def-lazy int-1 (list t t t t t t t nil))
(def-lazy int-2 (list t t t t t t nil t))
(def-lazy int-3 (list t t t t t t nil nil))
(def-lazy int-4 (list t t t t t nil t t))
(def-lazy int-5 (list t t t t t nil t nil))

;; Define constants
(def-lazy EOF int-0)

;; Define tags for jump instructions
(def-lazy tag-main    int-1)
(def-lazy tag-print   int-2)
(def-lazy tag-plus13  int-3)
(def-lazy tag-minus13 int-4)
(def-lazy tag-exit    int-5)



;; The assembly is a list of lists of instructions.
;; Each sublist defines a chunk bound with a program counter, starting from PC == 0.
;; When a jump instruction is executed, the instruction pointer transitions to the first instruction of the given program counter.
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

    ;; "n" <= reg-A <= "z" : sub 13
    (mov reg-C reg-A)
    (cmp reg-C >= "n")
    (mov reg-D reg-A)
    (cmp reg-D <= "z")
    (add reg-C reg-D)
    (jmpcmp reg-C == int-2 -> tag-minus13)

    ;; "A" <= reg-A < "N" : add 13
    (mov reg-C reg-A)
    (cmp reg-C >= "A")
    (mov reg-D reg-A)
    (cmp reg-D < "N")
    (add reg-C reg-D)
    (jmpcmp reg-C == int-2 -> tag-plus13)

    ;; "N" <= reg-A <= "Z" : sub 13
    (mov reg-C reg-A)
    (cmp reg-C >= "N")
    (mov reg-D reg-A)
    (cmp reg-D <= "Z")
    (add reg-C reg-D)
    (jmpcmp reg-C == int-2 -> tag-minus13)

    ;; If the character is not an alphabet, fall through to tag-print and print it as is
  )
  ;; tag-print
  (list
    (putc reg-A)
    (jmp tag-main)
  )
  ;; tag-plus13
  (list
    (add reg-A reg-B)
    (jmp tag-print)
  )
  ;; tag-minus13
  (list
    (sub reg-A reg-B)
    (jmp tag-print)
  )
  ;; tag-exit
  (list
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
