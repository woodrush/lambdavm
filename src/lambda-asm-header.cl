(load "./src/lambdavm.cl")


;; Used to determine if a value is an immediate or a register
(defparameter regnames ())

(defmacro-lazy mov (dst src)
  (let ((is-imm (if (position src regnames) nil t)))
    `(cons4 inst-mov ,is-imm ,src ,dst)))

(defmacro-lazy add (dst src)
  (let ((is-imm (if (position src regnames) nil t)))
    `(cons4 inst-addsub ,is-imm ,src (cons ,dst t))))

(defmacro-lazy sub (dst src)
  (let ((is-imm (if (position src regnames) nil t)))
    `(cons4 inst-addsub ,is-imm ,src (cons ,dst nil))))

(defmacro-lazy load (dst src)
  (let ((is-imm (if (position src regnames) nil t)))
    `(cons4 inst-load ,is-imm ,src ,dst)))

(defmacro-lazy store (dst src)
  (let ((is-imm (if (position dst regnames) nil t)))
    `(cons4 inst-store ,is-imm ,dst ,src)))

(defmacro-lazy jmp (jmp)
  (let ((is-imm (if (position jmp regnames) nil t)))
    `(cons4 inst-jmp ,is-imm ,jmp nil)))

(defmacro-lazy jmpcmp (dst enum-cmp src -> jmp)
  (let ((src-is-imm (if (position src regnames) nil t))
        (jmp-is-imm (if (position jmp regnames) nil t))
        (enum-cmp (nth (position enum-cmp '(== <= >= != < >)) '(cmp-eq cmp-le cmp-ge cmp-ne cmp-lt cmp-gt))))
  `(cons4 inst-jmpcmp ,src-is-imm ,src (cons4 ,enum-cmp ,jmp-is-imm ,jmp ,dst))))

(defmacro-lazy cmp (dst enum-cmp src)
  (let ((is-imm (if (position src regnames) nil t))
        (enum-cmp (nth (position enum-cmp '(== <= >= != < >)) '(cmp-eq cmp-le cmp-ge cmp-ne cmp-lt cmp-gt))))
    `(cons4 inst-cmp ,is-imm ,src (cons ,enum-cmp ,dst))))

(defmacro-lazy getc (reg)
  `(cons4 inst-io nil ,reg io-getc))

(defmacro-lazy putc (src)
  (let ((is-imm (if (position src regnames) nil t)))
    `(cons4 inst-io ,is-imm ,src io-putc)))

(defmacro-lazy exit ()
  `(cons4 inst-io nil nil io-exit))
