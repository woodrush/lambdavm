(load "./src/lambdacraft.cl")
(load "./src/blc-numbers.cl")
(load "./src/blc-clamb-wrapper.cl")



;;================================================================
;; Memory and program
;;================================================================
(defrec-lazy lookup-tree** (memory address cont)
  (typematch-nil-cons memory (car-memory cdr-memory)
    ;; nil case
    (cont int-zero)
    ;; cons case
    (typematch-nil-cons address (car-address cdr-address)
      ;; nil case
      (cont memory)
      ;; cons case
      ((if car-address
        (lookup-tree** car-memory)
        (lookup-tree** cdr-memory))
       cdr-address
       cont))))

(defrec-lazy lookup-memory** (memory address cont)
  ;; (typematch-nil-cons memory (car-memory cdr-memory)
  ;;   ;; nil case
  ;;   ;; (cont int-zero)
  ;;   (cont (lambda (f) (f t t t t t t t t t t t t t t t t t t t t t t t t)))
  ;;   ;; cons case
  ;;   )
  (typematch-nil-cons address (car-address cdr-address)
      ;; nil case
      (do
        (<- (ret _) (memory))
        (cont ret))
      ;; cons case
      (do
        (<- (car-memory cdr-memory) (memory))
        ((if car-address
        (lookup-memory** car-memory)
        (lookup-memory** cdr-memory))
       cdr-address
       cont)))
    )

(defun-lazy lookup-tree* (memory address cont)
  (do
    ;; (<- (ret) (lookup-memory** memory address))
    ;; (tuple2list ret cont)
    ;; (cont (list t nil t nil t nil t nil t     nil t nil t nil t nil t     nil t nil t nil t nil))
    ;; (<- (addr-tuple) (list2tuple address))
    (cont (address memory))
    )
    )

(defrec-lazy memory-write** (memory address value cont)
  (typematch-nil-cons address (car-address cdr-address)
    ;; nil case
    (cont value)
    ;; cons case
    (do
      (<- (memory-rewritten memory-orig)
        (do
          (<- (memory-target)
            ((lambda (cont)
              (typematch-nil-cons memory (car-memory cdr-memory)
                ;; nil case
                (cont nil nil)
                ;; cons case
                (cond
                  (car-address
                    (memory cont))
                  (t
                    (cont cdr-memory car-memory) ;; Implicit parameter passing: memory-orig ?
                    ))))))
          (memory-write** memory-target cdr-address value)))
      (if car-address
        (cont (cons memory-rewritten memory-orig))
        (cont (cons memory-orig memory-rewritten))))))

(defun-lazy memory-write* (memory address value cont)
  (do
    ;; (<- (value) (list2tuple value))
    (<- (address) (tuple2list address))
    (memory-write** memory address value cont)))

(defun-lazy memory-write-reg (reg address value cont)
  (do
    ;; (<- (value) (list2tuple value))
    (<- (address) (tuple2list-reg address))
    (memory-write** reg address value cont)))

(defmacro-lazy eval-bool (expr)
  `(lambda (cont)
    (if ,expr
      (cont t)
      (cont nil))))

(defrec-lazy add** (initcarry is-add n m cont)
  (typematch-nil-cons n (car-n cdr-n)
    ;; nil case
    (cont initcarry n)
    ;; cons case
    (do
      (<- (car-m cdr-m) (m))
      (<- (carry curlist) (add** initcarry is-add cdr-n cdr-m))
      (let* not-carry (not carry))
      (let* car-m (if is-add car-m (not car-m)))
      (let* f (lambda (a b)
        (if car-n
          (if car-m a b)
          (if car-m b a))))
      (<- (curbit nextcarry)
        ((lambda (cont)
          (do
            ((eval-bool (f car-m carry)))
            (if (f carry not-carry)
              (cont t)
              (cont nil))))))
      (cont nextcarry (cons curbit curlist)))))

(defun-lazy add* (initcarry is-add n m cont)
  (do
    (<- (n) (tuple2list n))
    (<- (m) (tuple2list m))
    (<- (carry sum) (add** initcarry is-add n m))
    (<- (sum) (list2tuple sum))
    (cont carry sum)))

(defun-lazy tuple2list-reg (n cont)
  (do
    (<- (b1 b2 b3) (n))
    (cont (list b1 b2 b3))))

(defun-lazy tuple2list (n cont)
  (do
    (<- (b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24) (n))
    (cont (list b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24))))

(defun-lazy list2tuple (l cont)
  (do
    (<- (b1 _) (l))
    (<- (b2 _) (_))
    (<- (b3 _) (_))
    (<- (b4 _) (_))
    (<- (b5 _) (_))
    (<- (b6 _) (_))
    (<- (b7 _) (_))
    (<- (b8 _) (_))
    (<- (b9 _) (_))
    (<- (b10 _) (_))
    (<- (b11 _) (_))
    (<- (b12 _) (_))
    (<- (b13 _) (_))
    (<- (b14 _) (_))
    (<- (b15 _) (_))
    (<- (b16 _) (_))
    (<- (b17 _) (_))
    (<- (b18 _) (_))
    (<- (b19 _) (_))
    (<- (b20 _) (_))
    (<- (b21 _) (_))
    (<- (b22 _) (_))
    (<- (b23 _) (_))
    (<- (b24 _) (_))
    (cont (lambda (f) (f b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24)))))


;;================================================================
;; Arithmetic
;;================================================================
(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp** (n m)
  (typematch-nil-cons n (car-n cdr-n)
    ;; nil case
    cmpret-eq
    ;; cons case
    (do
      (<- (car-m cdr-m) (m))
      (let* next (cmp** cdr-n cdr-m))
      (if car-n
        (if car-m
          next
          cmpret-lt)
        (if car-m
          cmpret-gt
          next)))))

(defun-lazy cmp* (n m)
  (do
    (<- (n) (tuple2list n))
    (<- (m) (tuple2list m))
    (cmp** n m)))

(defun-lazy cmp-gt (f) (f nil nil t))
(defun-lazy cmp-lt (f) (f nil t   nil))
(defun-lazy cmp-eq (f) (f t   nil nil))
(defun-lazy cmp-le (f) (f t   t   nil))
(defun-lazy cmp-ge (f) (f t   nil t))
(defun-lazy cmp-ne (f) (f nil t   t))

(defmacro-lazy compare (n m enum-cmp)
  `(,enum-cmp (cmp* ,n ,m)))


;;================================================================
;; I/O
;;================================================================
(defmacro-lazy io-bitlength-to-wordsize (n)
  `(do
    (<- (b1 _) (,n))
    (<- (b2 _) (_))
    (<- (b3 _) (_))
    (<- (b4 _) (_))
    (<- (b5 _) (_))
    (<- (b6 _) (_))
    (<- (b7 _) (_))
    (<- (b8 _) (_))
    (lambda (x) (x t t t t   t t t t   t t t t   t t t t   b1 b2 b3 b4 b5 b6 b7 b8)))
  ;; `(do
  ;;   (<- (n) (list2tuple ,n))
  ;;   (lambda (x) (n (x t t t t   t t t t   t t t t   t t t t)))
  ;;   ;; (supp-bitlength (lambda (x f) (f t x)) n)
  ;;   )
    )

(defmacro-lazy wordsize-to-io-bitlength (n)
  `(do
    (<- (n) (tuple2list ,n))
    (supp-bitlength cdr* n)))


;;================================================================
;; Evaluation
;;================================================================
(defun-lazy lookup-src-if-imm* (src-is-imm *src cont)
  (if src-is-imm
    (cont *src)
    (regread *src cont))) ;; regread is defined in eval

;; Checks if curblock is { t, nil } (returns t) or a cons cell (returns nil).
(defmacro-lazy is-t-or-nil (expr)
  `(,expr (lambda (a b) t) (lambda (a) a) (lambda (a) t) nil))


(defrec-lazy eval (memory stdin curblock curproglist reg)
  (do
    (let* jumpto
      (lambda (jmp)
        (do
          ;; (<- (proglist) (lookup-tree** progtree jmp))
          (((jmp progtree) (eval memory stdin)) reg))))
    (let* regwrite (memory-write-reg reg))
    (let* regread (lambda (addr cont) (cont (addr reg))))
    (cond
      ((is-t-or-nil curblock)
        (typematch-nil-cons curproglist (car-curproglist cdr-curproglist)
          ;; nil case
          curproglist
          ;; cons case
          ((eval memory stdin car-curproglist cdr-curproglist) reg)))
      (t
        (do
          (<- (curinst nextblock) (curblock))
          (let* eval-reg (eval memory stdin nextblock curproglist))
          (<- (inst-type src-is-imm *src) (curinst)) ;; Delayed destruction: *dst
          (<- (src) (lookup-src-if-imm* src-is-imm *src))
          (lambda (*dst)
            **instruction-typematch**))))))


;;================================================================
;; Instructions
;;================================================================
(defun-lazy inst-io      (i1 i2 i3 i4 i5 i6 i7 i8) i1)
(defun-lazy inst-jmpcmp  (i1 i2 i3 i4 i5 i6 i7 i8) i2)
(defun-lazy inst-cmp     (i1 i2 i3 i4 i5 i6 i7 i8) i3)
(defun-lazy inst-jmp     (i1 i2 i3 i4 i5 i6 i7 i8) i4)
(defun-lazy inst-load    (i1 i2 i3 i4 i5 i6 i7 i8) i5)
(defun-lazy inst-store   (i1 i2 i3 i4 i5 i6 i7 i8) i6)
(defun-lazy inst-addsub  (i1 i2 i3 i4 i5 i6 i7 i8) i7)
(defun-lazy inst-mov     (i1 i2 i3 i4 i5 i6 i7 i8) i8)

(def-lazy **instruction-typematch**
  (inst-type
    io-case
    (cons (lambda (cont) (cont jmpcmp-case)) nil)
    (cons (lambda (cont) (cont cmp-case)) nil)
    (cons (lambda (cont) (cont jmp-case)) nil)
    (cons (lambda (cont) (cont load-case)) nil)
    (cons (lambda (cont) (cont store-case)) nil)
    (cons (lambda (cont) (cont addsub-case)) nil)
    (cons (lambda (cont) (cont mov-case)) nil)
    ))

(defun-lazy io-getc (x1 x2 x3) x1)
(defun-lazy io-putc (x1 x2 x3) x2)
(defun-lazy io-exit (x1 x2 x3) x3)

(defmacro-lazy cons4 (x1 x2 x3 x4)
  `(lambda (f) (f ,x1 ,x2 ,x3 ,x4)))


(def-lazy addsub-case
  ;; Instruction structure: (cons4 inst-add [src-isimm] [src] (cons [*dst] is-add))
  ((do
    (<- (*dst is-add) (*dst))
    (<- (carry)  ;; Implicit parameter passing: sum
      ((do
        (regread *dst) ;; Implicit parameter passing: dst
        (add* is-add is-add))
       src))                    ;; Applies src to the preceding add*
    (regwrite *dst)) eval-reg))

(def-lazy store-case
  ;; Instruction structure: (cons4 inst-store [dst-isimm] [dst-memory] [source])
  ;; Note that the destination is stored in the variable *src
  (((regread *dst
      (memory-write* memory src))
     eval)
   stdin nextblock curproglist reg))

(def-lazy mov-case
  ;; Instruction structure:: (cons4 inst-mov [src-isimm] [src] [dst])
  (regwrite *dst src eval-reg))

(def-lazy jmp-case
  ;; Instruction structure:: (cons4 inst-jmp [jmp-isimm] [jmp] _)
  (jumpto src))

(def-lazy jmpcmp-case
  ;; Instruction structure: (cons4 inst-jmpcmp [src-isimm] [src] (cons4 [enum-cmp] [jmp-isimm] [jmp] [*dst]))
  (do
    (<- (enum-cmp jmp-is-imm *jmp *cmp-dst) (*dst))
    (lookup-src-if-imm* jmp-is-imm *jmp)  ;; Implicit parameter passing: jmp
    (regread *cmp-dst)               ;; Implicit parameter passing: dst-value
    (lambda (dst-value jmp)
      (if (compare dst-value src enum-cmp)
        (jumpto jmp)
        (eval-reg reg)))))

(def-lazy load-case
  ;; Instruction structure: (cons4 inst-load [src-isimm] [src] [*dst])
  (do
    (((lookup-tree* memory src)
        (regwrite *dst))
     eval-reg)))

(def-lazy cmp-case
  ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
  ((do
    (<- (enum-cmp dst) (*dst))
    (let* int-zero int-zero)  ;; Share references to save space
    (<- (carry) (add* nil (enum-cmp ((regread dst cmp*) src)) int-zero int-zero)) ;; Implicit parameter passing: sum
    (regwrite dst)) eval-reg))

(def-lazy io-case
  ;; Instruction structure:
  ;;   getc: (cons4 inst-io nil         [dst] io-getc)
  ;;   putc: (cons4 inst-io [src-isimm] [src] io-putc)
  ;;   exit: (cons4 inst-io nil         nil   io-exit)
  ;; For `exit`, the control flow depends on the second term, so it must be set to `nil`.
  ;; Typematch over the inst. type
  (*dst
    ;; getc
    (cons
      (lambda (cont)
        (do
          (<- (c stdin)
            ((lambda (return)
              (typematch-nil-cons stdin (car-stdin cdr-stdin)
                ;; nil case
                (return int-tuple-zero stdin)
                ;; cons case
                (return (io-bitlength-to-wordsize car-stdin) cdr-stdin)))))
          (<- (reg) (regwrite *src c))
          (cont (eval memory stdin nextblock curproglist reg))))
      nil)
    ;; putc
    (do
      (cons
        (lambda (cont)
          (do
            (cons (wordsize-to-io-bitlength src))
            (cont (eval-reg reg))))
        nil))
    ;; exit
    src-is-imm)) ;; always evaluates to nil

(defrec-lazy init-tree (address)
  (typematch-nil-cons address (car-address cdr-address)
    ;; nil case
    (lambda (f) (f t t t t   t t t t   t t t t   t t t t   t t t t   t t t t))
    ;; cons case
    (do
      (let* nexttree (init-tree cdr-address))
      (cons nexttree nexttree))))

(defrec-lazy list2tree** (l depth cont)
  (typematch-nil-cons l (_ _)
    ;; nil case
    (typematch-nil-cons depth (_ cdr-depth)
      ;; nil case
      (cont
        ;; (cons
          (lambda (f) (f t t t t   t t t t   t t t t   t t t t   t t t t   t t t t))
          ;; (lambda (f) (f t t t t   t t t t   t t t t   t t t t   t t t t   t t t t)))
        nil)
      ;; cons case
      (do
        (<- (tree l) (list2tree** nil cdr-depth))
        (cont (cons tree tree) l)))
    ;; cons case
    (typematch-nil-cons depth (_ cdr-depth)
      ;; nil case
      (l cont)
      ;; cons case
      (do
        (<- (right-tree l) (list2tree** l cdr-depth))
        (<- (left-tree) (list2tree** l cdr-depth)) ;; Implicit parameter passing: l
        (cont (cons right-tree left-tree))))))

(defrec-lazy cdr-generator (l)
  (typematch-nil-cons l (_ cdr-l)
    ;; nil case
    l
    ;; cons case
    (cons l (cdr-generator cdr-l))))

(def-lazy initreg nil)
(def-lazy int-tuple-zero (lambda (f) (f t t t t   t t t t   t t t t   t t t t   t t t t   t t t t)))

(defrec-lazy trampoline (expr)
  (typematch-nil-cons expr (car-expr cdr-expr)
    ;; nil case
    expr
    ;; cons case
    (do
      (<- (expr) (car-expr))
      (trampoline expr))))

(defun-lazy lambdaVM (
  io-bitlength supp-bitlength
  memlist proglist stdin)
  (do
    ;; Share references to functions to prevent them from being inlined multiple times
    (let* int-zero
      (let ((cons-t (lambda (x f) (f t x))))
        (supp-bitlength cons-t (io-bitlength cons-t nil))))
    (let* Y-comb Y-comb)
    (let* cmp* cmp*)
    (let* add* add*)
    (let* memory-write* memory-write*)
    (let* lookup-tree* lookup-tree*)

    ;; Implicit parameter passing of memtree and progtree:
    ;; ((proglist (eval memtree progtree stdin)) initreg)
    (trampoline
      ((proglist
        (((do
            (let* list2tree*
              (lambda (l cont)
                (do
                  (<- (tree _) (list2tree** l int-zero))
                  (cont tree))))
            (<- (progtree) (list2tree* (cdr-generator proglist)))
            (list2tree* memlist) ;; Implicit argument passing: memtree
            (eval)))
        stdin))
      ;; initreg
    (let ((zero (lambda (f) (f t t t t   t t t t   t t t t   t t t t   t t t t   t t t t))))
      (cons
        (cons
          (cons zero zero)
          (cons zero zero))
        (cons
          (cons zero zero)
          (cons zero zero))))
      ))))
