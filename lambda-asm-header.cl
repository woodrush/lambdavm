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

(defmacro-lazy jmpcmp (dst enum-cmp src-is-imm src -> jmp-is-imm jmp)
  `(cons4 inst-jmpcmp ,src-is-imm ,src (cons4 ,enum-cmp ,jmp-is-imm ,jmp ,dst)))

(defmacro-lazy cmp (dst enum-cmp src-is-imm src)
  `(cons4 inst-cmp ,src-is-imm ,src (cons ,enum-cmp ,dst)))

(defmacro-lazy getc (reg)
  `(cons4 inst-io nil ,reg io-getc))

(defmacro-lazy putc (is-imm x)
  `(cons4 inst-io ,is-imm ,x io-putc))

(defmacro-lazy exit ()
  `(cons4 inst-io nil nil io-exit))
