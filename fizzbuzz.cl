(load "./lambda-asm-header.cl")


;; Define your own register addresses
(def-lazy reg-A (list t t))
(def-lazy reg-B (list t nil))
(def-lazy reg-C (list nil t))
(def-lazy reg-D (list nil nil))
;; Used for the macros in ./lambda-asm-header.cl, to determine if a value is an immediate or a register
(defparameter regnames '(reg-A reg-B reg-C reg-D))

;; Numbers are in big-endian, with t == 0, nil == 1
;; (Raw integer literals `0`, `1`, etc. are bound to different lambda forms in ./lazy.cl)
(def-lazy int-0 (list t t t t t t t t))
(def-lazy int-1 (list t t t t t t t nil))
(def-lazy int-2 (list t t t t t t nil t))
(def-lazy int-3 (list t t t t t t nil nil))
(def-lazy int-4 (list t t t t t nil t t))
(def-lazy int-5 (list t t t t t nil t nil))
(def-lazy int-6 (list t t t t t nil nil t))
(def-lazy int-7 (list t t t t t nil nil nil))
(def-lazy int-8 (list t t t t nil t t t))
(def-lazy int-11 (list t t t t nil t nil nil))
(def-lazy int-12 (list t t t t nil nil t t))
(def-lazy int-13 (list t t t t nil nil t nil))
(def-lazy int-14 (list t t t t nil nil nil t))
(def-lazy int-15 (list t t t t nil nil nil nil))
(def-lazy int-16 (list t t t nil t t t t))

;; The memory is allocated contiguously, starting from address 0 (int-0).
(def-lazy initial-memory (list "F" "i" "z" "z" int-0 "B" "u" "z" "z" int-0))

;; Define constants
(def-lazy *Fizz int-0)
(def-lazy *Buzz int-5)
(def-lazy n int-11)
(def-lazy nmod3 int-12)
(def-lazy nmod5 int-13)
(def-lazy i int-14)
(def-lazy j int-15)
(def-lazy return-pc int-16)

;; Define tags for jump instructions
(def-lazy tag-main              int-1)
(def-lazy tag-print-n-times     int-2)
(def-lazy tag-print-space       int-3)
(def-lazy tag-print-fizz        int-4)
(def-lazy tag-print-buzz        int-5)
(def-lazy tag-print-fizzbuzz    int-6)
(def-lazy tag-print-string      int-7)
(def-lazy tag-prepare-next-iter int-8)



;; The assembly is a list of lists of instructions.
;; Each sublist defines a chunk bound with a program counter, starting from PC == 0.
;; When a jump instruction is executed, the instruction pointer transitions to the first instruction of the given program counter.
(def-lazy asm (list
  ;; Initialization (PC == 0)
  (list
    (mov reg-A int-1)
    (store n reg-A) ;; Set n = 1
    (store nmod3 reg-A) ;; Set nmod3 = 1
    (store nmod5 reg-A) ;; Set nmod5 = 1
    (store i reg-A) ;; Set i = 1
  )
  ;; tag-main (PC == 1)
  (list
    ;; Check nmod3 and nmod5
    (load reg-A nmod3)
    (cmp reg-A == int-0)
    (load reg-B nmod5)
    (cmp reg-B == int-0)
    (mov reg-C reg-A)
    (add reg-C reg-B)
    
    ;; Print FizzBuzz if n === 0 mod 3 && n === 0 mod 5
    (jmpcmp reg-C == int-2 -> tag-print-fizzbuzz)

    ;; Print Fizz if n === 0 mod 3
    (jmpcmp reg-A == int-1 -> tag-print-fizz)

    ;; Print Buzz if n === 0 mod 5
    (jmpcmp reg-B == int-1 -> tag-print-buzz)

    ;; If none of the above, fall through to print-n-times
  )
  ;; tag-print-n-times
  (list
    (load reg-A i)
    (jmpcmp reg-A == int-0 -> tag-prepare-next-iter)
    (putc "*")
    (sub reg-A int-1)
    (store i reg-A)
    (load reg-A j)
    (add reg-A int-1)
    (store j reg-A) ;; Increment j
    (jmpcmp reg-A == int-5 -> tag-print-space)
    (jmp tag-print-n-times)
  )
  ;; tag-print-space
  (list
    (putc " ")
    (mov reg-A int-0)
    (store j reg-A)
    (jmp tag-print-n-times)
  )
  ;; tag-print-fizz
  (list
    (mov reg-A *Fizz)
    (store i reg-A)
    (mov reg-A tag-prepare-next-iter)
    (store return-pc reg-A)
    (jmp tag-print-string)
  )
  ;; tag-print-buzz
  (list
    (mov reg-A *Buzz)
    (store i reg-A)
    (mov reg-A tag-prepare-next-iter)
    (store return-pc reg-A)
    (jmp tag-print-string)
  )
  ;; tag-print-fizzbuzz
  (list
    (mov reg-A *Fizz)
    (store i reg-A)
    (mov reg-A tag-print-buzz)
    (store return-pc reg-A)
    (jmp tag-print-string)
  )
  ;; tag-print-string
  (list
    (load reg-A i)
    (load reg-B reg-A) ;; Load the current character at &i
    (load reg-C return-pc)
    (jmpcmp reg-B == int-0 -> reg-C) ;; If the current character is null, jump to return-pc
    (putc reg-B) ;; Otherwise, print the current character
    (add reg-A int-1)
    (store i reg-A) ;; Increment i
    (jmp tag-print-string)
  )
  ;; tag-prepare-next-iter
  (list
    (putc "\\n") ;; Print newline

    ;; Update n and i
    (load reg-A n)
    (add reg-A int-1)
    (store n reg-A)
    (store i reg-A) ;; i = ++n

    ;; Update nmod3
    (load reg-A nmod3)
    (add reg-A int-1)
    (mov reg-B reg-A)
    (cmp reg-B == int-3) ;; Evaluates to 1 if true
    (mov reg-C reg-B)
    (add reg-C reg-B)
    (add reg-C reg-B)
    (sub reg-A reg-C) ;; Subtract 3 from reg-A if nmod3 == 3
    (store nmod3 reg-A)

    ;; Update nmod5
    (load reg-A nmod5)
    (add reg-A int-1)
    (mov reg-B reg-A)
    (cmp reg-B == int-5) ;; Evaluates to 1 if true
    (mov reg-C reg-B)
    (add reg-C reg-B)
    (add reg-C reg-C)
    (add reg-C reg-B)
    (sub reg-A reg-C) ;; Subtract 5 from reg-A if nmod5 == 5
    (store nmod5 reg-A)

    ;; Update j
    (mov reg-A int-0)
    (store j reg-A)

    (jmp tag-main)
  )
))



;; The number of bits used for I/O.
;; Binary lambda calculus supplies a list of lists of 8-bit-encoded characters.
(def-lazy SYS-IO-BITS 8)
;; The number of supplementary bits to prepend to the I/O bits, to be used for the machine's word size.
;; Machine word size = SYS-IO-BITS + SYS-SUPPLEMENTARY-BITS
(def-lazy SYS-SUPPLEMENTARY-BITS 0)

(def-lazy standalone
  ;; All binary lambda calculus programs are functions that accept a string (stdin) and return a string.
  (lambda (stdin)
    (lambdaVM SYS-IO-BITS SYS-SUPPLEMENTARY-BITS initial-memory asm stdin)))

(format t (compile-to-blc-lazy standalone))
