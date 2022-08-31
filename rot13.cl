(load "./lambda-asm-header.cl")

;; Define the register addresses (they can be any)
(def-lazy reg-A (list t t))
(def-lazy reg-B (list t nil))
(def-lazy reg-C (list nil t))
(def-lazy reg-D (list nil nil))

;; Numbers are in big-endian, with t == 0, nil == 1
(def-lazy int-0 (list t t t t t t t t))
(def-lazy int-1 (list t t t t t t t nil))
(def-lazy int-2 (list t t t t t t nil t))
(def-lazy int-3 (list t t t t t t nil nil))
(def-lazy int-4 (list t t t t t nil t t))
(def-lazy int-5 (list t t t t t nil t nil))

;; Define tags for jump instructions
(def-lazy tag-main int-1)
(def-lazy tag-print-plus13 int-2)
(def-lazy tag-print-minus13 int-3)
(def-lazy tag-exit int-4)


(def-lazy asm (list
  ;; Initialization (PC = 0)
  (list
    ;; Store 26/2 = 13 at reg-B
    (mov reg-B t "N")
    (sub reg-B t "A")
  )
  ;; tag-main (PC = 1)
  (list
    (getc reg-A)

    ;; Exit at EOF
    (jmpcmp reg-A cmp-eq t int-0 -> t tag-exit)

    ;; "a" <= reg-A < "n" : add 13
    (mov reg-C nil reg-A)
    (cmp reg-C cmp-ge t "a")
    (mov reg-D nil reg-A)
    (cmp reg-D cmp-lt t "n")
    (add reg-C nil reg-D)
    (jmpcmp reg-C cmp-eq t int-2 -> t tag-print-plus13)

    ;; "n" <= reg-A <= "z" : sub 13
    (mov reg-C nil reg-A)
    (cmp reg-C cmp-ge t "n")
    (mov reg-D nil reg-A)
    (cmp reg-D cmp-le t "z")
    (add reg-C nil reg-D)
    (jmpcmp reg-C cmp-eq t int-2 -> t tag-print-minus13)

    ;; "A" <= reg-A < "N" : add 13
    (mov reg-C nil reg-A)
    (cmp reg-C cmp-ge t "A")
    (mov reg-D nil reg-A)
    (cmp reg-D cmp-lt t "N")
    (add reg-C nil reg-D)
    (jmpcmp reg-C cmp-eq t int-2 -> t tag-print-plus13)

    ;; "N" <= reg-A <= "Z" : sub 13
    (mov reg-C nil reg-A)
    (cmp reg-C cmp-ge t "N")
    (mov reg-D nil reg-A)
    (cmp reg-D cmp-le t "Z")
    (add reg-C nil reg-D)
    (jmpcmp reg-C cmp-eq t int-2 -> t tag-print-minus13)

    ;; If the character is not an alphabet, just print it
    (putc nil reg-A)
    (jmp t tag-main)
  )
  ;; tag-print-plus13
  (list
    (add reg-A nil reg-B)
    (putc nil reg-A)
    (jmp t tag-main)  
  )
  ;; tag-print-minus13
  (list
    (sub reg-A nil reg-B)
    (putc nil reg-A)
    (jmp t tag-main)  
  )
  ;; tag-exit
  (list
  )
))

(def-lazy standalone
  ;; Remember to make the standalone a function that accepts the stdin, (lambda (stdin) ...)
  ;; Otherwise, the program will not be executed by the interpreter.
  (lambda (stdin)
    (main 8 nil nil asm stdin)))

(format t (compile-to-blc-lazy standalone))
