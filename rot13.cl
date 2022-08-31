(load "./lambda-asm-header.cl")

;; Define the register addresses (they can be any)
(def-lazy reg-A (list t t))
(def-lazy reg-B (list t nil))
(def-lazy reg-C (list nil t))
(def-lazy reg-D (list nil nil))
;; Used for the macros in ./lambda-asm-header.cl, to determine if a value is an immediate or a register
(defparameter regnames '(reg-A reg-B reg-C reg-D))

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
    (mov reg-B "N")
    (sub reg-B "A")
  )
  ;; tag-main (PC = 1)
  (list
    (getc reg-A)

    ;; Exit at EOF
    (jmpcmp reg-A cmp-eq int-0 -> tag-exit)

    ;; "a" <= reg-A < "n" : add 13
    (mov reg-C reg-A)
    (cmp reg-C cmp-ge "a")
    (mov reg-D reg-A)
    (cmp reg-D cmp-lt "n")
    (add reg-C reg-D)
    (jmpcmp reg-C cmp-eq int-2 -> tag-print-plus13)

    ;; "n" <= reg-A <= "z" : sub 13
    (mov reg-C reg-A)
    (cmp reg-C cmp-ge "n")
    (mov reg-D reg-A)
    (cmp reg-D cmp-le "z")
    (add reg-C reg-D)
    (jmpcmp reg-C cmp-eq int-2 -> tag-print-minus13)

    ;; "A" <= reg-A < "N" : add 13
    (mov reg-C reg-A)
    (cmp reg-C cmp-ge "A")
    (mov reg-D reg-A)
    (cmp reg-D cmp-lt "N")
    (add reg-C reg-D)
    (jmpcmp reg-C cmp-eq int-2 -> tag-print-plus13)

    ;; "N" <= reg-A <= "Z" : sub 13
    (mov reg-C reg-A)
    (cmp reg-C cmp-ge "N")
    (mov reg-D reg-A)
    (cmp reg-D cmp-le "Z")
    (add reg-C reg-D)
    (jmpcmp reg-C cmp-eq int-2 -> tag-print-minus13)

    ;; If the character is not an alphabet, print it as is
    (putc reg-A)
    (jmp tag-main)
  )
  ;; tag-print-plus13
  (list
    (add reg-A reg-B)
    (putc reg-A)
    (jmp tag-main)  
  )
  ;; tag-print-minus13
  (list
    (sub reg-A reg-B)
    (putc reg-A)
    (jmp tag-main)  
  )
  ;; tag-exit
  (list
  )
))

(def-lazy SYS-IO-BITS 8)
(def-lazy SYS-SUPPLEMENTARY-BITS 0)
(def-lazy initial-memory nil)

(def-lazy standalone
  ;; `main` is a function that accepts a string (stdin) and returns a string.
  (lambda (stdin)
    (main SYS-IO-BITS SYS-SUPPLEMENTARY-BITS initial-memory asm stdin)))

(format t (compile-to-blc-lazy standalone))
