(load "./lambdavm.cl")

(defmacro-lazy mov (dst is-imm src)
  `(cons4 inst-mov ,is-imm ,src ,dst))

(defmacro-lazy add (dst is-imm src)
  `(cons4 inst-addsub ,is-imm ,src (cons ,dst t)))

(defmacro-lazy sub (dst is-imm src)
  `(cons4 inst-addsub ,is-imm ,src (cons ,dst nil)))

(defmacro-lazy load (dst is-imm src)
  `(cons4 inst-load ,is-imm ,src ,dst))

(defmacro-lazy store (is-imm dst src)
  `(cons4 inst-store ,is-imm ,dst ,src))

(defmacro-lazy jmp (is-imm jmp)
  `(cons4 inst-jmp ,is-imm ,jmp nil))

(defmacro-lazy jmpcmp (dst enum-cmp src-is-imm src jmp-is-imm jmp)
  `(cons4 inst-jmpcmp ,src-is-imm ,src (cons4 ,enum-cmp ,jmp-is-imm ,jmp ,dst)))

  ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
(defmacro-lazy comp (dst enum-cmp src-is-imm src)
  `(cons4 inst-cmp ,src-is-imm ,src (cons ,enum-cmp ,dst)))


(defmacro-lazy getc (reg)
  `(cons4 inst-io nil ,reg io-getc))

(defmacro-lazy putc (is-imm x)
  `(cons4 inst-io ,is-imm ,x io-putc))

(defmacro-lazy exit ()
  `(cons4 inst-io nil nil io-exit))

(def-lazy asm (list
  (list
    (getc reg-A)
    (store t (io-bitlength-to-wordsize "A") reg-A)
    (load reg-B t (io-bitlength-to-wordsize "A"))
    (mov reg-C nil reg-B)
    (comp reg-C cmp-ne t (io-bitlength-to-wordsize "A"))
    (add reg-B nil reg-C)
    ;; (mov reg-B t (io-bitlength-to-wordsize "B"))
    ;; (sub reg-B t (io-bitlength-to-wordsize "A"))
    ;; (add reg-A nil reg-B)
    (putc nil reg-B)
    ;; (jmpcmp reg-A cmp-ne t (io-bitlength-to-wordsize "A") t ((+ 16 8) (cons* t) nil))
    (jmp t ((+ 16 8) (cons* t) nil))
  )
  (list
    (putc t (io-bitlength-to-wordsize "H"))  
  )
  (list
    (putc t (io-bitlength-to-wordsize "I"))
  )
  ;; (list
  ;;   (putc t (io-bitlength-to-wordsize "H"))
  ;;   (putc t (io-bitlength-to-wordsize "A"))
  ;;   (getc reg-A)
  ;;   (getc reg-B)
  ;;   (putc nil reg-A)
  ;;   (putc t (io-bitlength-to-wordsize "A"))
  ;;   (exit)
  ;; )
))

(def-lazy standalone
  ;; Remember to make the standalone binary a function that accepts the stdin
  (lambda (stdin)
    (let ((supp-bitlength 16))
      (main 8 supp-bitlength nil asm stdin))))

(format t (compile-to-blc-lazy standalone))