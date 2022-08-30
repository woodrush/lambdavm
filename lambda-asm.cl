(load "./lambdavm.cl")

(defmacro-lazy putc (is-imm x)
  `(cons4 inst-io ,is-imm ,x io-putc))

(defmacro-lazy exit ()
  `(cons4 inst-io nil nil io-exit))

;; (def-lazy A (list t t t))

(def-lazy asm (list
  (list
    ;; (putc t (io-bitlength-to-wordsize "H"))
    ;; (putc t (io-bitlength-to-wordsize "i"))
    (putc t (io-bitlength-to-wordsize "."))
    (cons4 inst-io nil nil io-exit)
  )
  ;; (list
  ;;   (putc t (io-bitlength-to-wordsize "H"))
  ;;   (putc t (io-bitlength-to-wordsize "A"))
  ;;   (exit)
  ;; )
))
(def-lazy binary
  ;; (cons (list t t nil nil t nil t nil) nil)
  ;; (lambda (stdin) (list (list t t nil nil t nil t nil)))
  (lambda (stdin)
    (let ((supp-bitlength 16))
      (cons "B" (main 8 16 nil asm stdin))
      )
      )
  )

(format t (compile-to-blc-lazy binary))
