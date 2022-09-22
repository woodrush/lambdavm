(load "./lambdacraft.cl")
(load "./blc-numbers.cl")
(load "./blc-clamb-wrapper.cl")


(defmacro-lazy typematch-nil-cons (expr cons-args nil-case cons-case)
  `(,expr
     (lambda ,cons-args
       (lambda (_) ,cons-case))
     ,nil-case))



(def-lazy reg-A  (list t))
(def-lazy reg-B  (list nil t t))
(def-lazy reg-SP (list nil t nil))
(def-lazy reg-D  (list nil nil t))
(def-lazy reg-BP (list nil nil nil t))
(def-lazy reg-C  (list nil nil nil nil))


;;================================================================
;; Memory and program
;;================================================================
(defrec-lazy lookup-tree* (memory address cont)
  (typematch-nil-cons address (car-address cdr-address)
    ;; nil case
    (cont memory)
    ;; cons case
    (typematch-nil-cons memory (car-memory cdr-memory)
      ;; nil case
      (cont int-zero)
      ;; cons case
      ((if car-address
        (lookup-tree* car-memory)
        (lookup-tree* cdr-memory))
       cdr-address
       cont))))

(defrec-lazy memory-write* (memory address value cont)
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
          (memory-write* memory-target cdr-address value)))
      (if car-address
        (cont (cons memory-rewritten memory-orig))
        (cont (cons memory-orig memory-rewritten))))))

(defmacro-lazy eval-bool (expr)
  `(lambda (cont)
    (if ,expr
      (cont t)
      (cont nil))))

(defrec-lazy add* (initcarry is-add n m cont)
  (typematch-nil-cons n (car-n cdr-n)
    ;; nil case
    (cont initcarry n)
    ;; cons case
    (do
      (<- (car-m cdr-m) (m))
      (<- (carry curlist) (add* initcarry is-add cdr-n cdr-m))
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



;;================================================================
;; Arithmetic
;;================================================================
(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (typematch-nil-cons n (car-n cdr-n)
    ;; nil case
    cmpret-eq
    ;; cons case
    (do
      (<- (car-m cdr-m) (m))
      (let* next (cmp* cdr-n cdr-m))
      (if car-n
        (if car-m
          next
          cmpret-lt)
        (if car-m
          cmpret-gt
          next)))))

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
  `(supp-bitlength (lambda (x f) (f t x)) ,n))

(defmacro-lazy wordsize-to-io-bitlength (n)
  `(supp-bitlength cdr* ,n))


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

(defrec-lazy eval (memory progtree stdin curblock curproglist reg)
  (do
    (let* regread (lookup-tree* reg))
    (let* regwrite (memory-write* reg))
    (let* jumpto
      (lambda (jmp)
        (do
          (<- (proglist) (lookup-tree* progtree jmp))
          ((proglist (eval memory progtree stdin)) reg))))
    (cond
      ((is-t-or-nil curblock)
        (typematch-nil-cons curproglist (_ _)
          ;; nil case
          SYS-STRING-TERM
          ;; cons case
          ((curproglist (eval memory progtree stdin)) reg)))
      (t
        (do
          (<- (curinst nextblock) (curblock))
          (let* eval-reg (eval memory progtree stdin nextblock curproglist))
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
    jmpcmp-case
    cmp-case
    jmp-case
    load-case
    store-case
    addsub-case
    mov-case
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
   progtree stdin nextblock curproglist reg))

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
  ;; Typematch over the inst. type
  (*dst
    ;; getc
    (do
      (<- (c stdin)
        ((lambda (return)
          (typematch-nil-cons stdin (car-stdin cdr-stdin)
            ;; nil case
            (return int-zero stdin)
            ;; cons case
            (return (io-bitlength-to-wordsize car-stdin) cdr-stdin)))))
      (regwrite *src c)               ;; Implicit parameter passing: reg
      (eval memory progtree stdin nextblock curproglist))
    ;; putc
    (do
      (cons (wordsize-to-io-bitlength src) (eval-reg reg)))
    ;; exit
    SYS-STRING-TERM))

(defrec-lazy list2tree** (l depth cont)
  (typematch-nil-cons l (_ _)
    ;; nil case
    (cont l l)
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
    ;; (let* int-zero (list t t t t t t t t t t t t t t t t t t t t t t t t ))
    (let* memory-write* memory-write*)
    (let* lookup-tree* lookup-tree*)

    ;; Implicit parameter passing of memtree and progtree:
    ;; ((proglist (eval memtree progtree stdin)) initreg)
    ((proglist
      (((do
          (let* list2tree*
            (lambda (l cont)
              (do
                (<- (tree _) (list2tree** l int-zero))
                (cont tree))))
          (list2tree* (cdr-generator proglist));; Implicit argument passing: progtree)
          (list2tree* memlist) ;; Implicit argument passing: memtree
          (eval)))
      stdin))
     initreg)))

(def-lazy SYS-STRING-TERM nil)



;; (def-lazy SYS-N-BITS (+ 16 8))
;; (def-lazy int-zero (take SYS-N-BITS (inflist nil)))


(defun-lazy main (memlist proglist stdin)
  (blcstr-to-lazykstr (lambdaVM
  8 16
  memlist proglist (lazykstr-to-blcstr stdin))))

;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))

;; (format t (compile-to-ski-lazy lambdaVM))

;; (format t (compile-to-blc-lazy lambdaVM))
;; (format t (compile-to-plaintext-lambda-lazy lambdaVM))
;; (format t (compile-to-lisp-pretty-lazy lambdaVM))

;; ;; (setq *print-pretty* 'nil)
;; (format t (compile-to-simple-lambda-lazy lambdaVM))
;; (format t (compile-to-js-lazy lambdaVM))
;; (format t (compile-to-js-arrow-lazy lambdaVM))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy lambdaVM))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy lambdaVM)))))
