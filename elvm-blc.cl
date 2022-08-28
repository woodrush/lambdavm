(load "./lazy.cl")
(load "./blc-numbers.cl")


;;================================================================
;; Memory and program
;;================================================================
(defrec-lazy lookup-tree* (memory address cont)
  (cond
    ((isnil memory)
      (cont int-zero))
    ((isnil address)
      (cont memory))
    (t
      (do
        (<- (car-address cdr-address) (address))
        (<- (next-memory)
          ((lambda (cont)
            (do
              (<- (car-memory cdr-memory) (memory))
              (if car-address
                (cont car-memory)
                (cont cdr-memory))))))
        (lookup-tree* next-memory cdr-address cont)))))

(defrec-lazy memory-write* (memory address value cont)
  (cond
    ((isnil address)
      (cont value))
    (t
      (do
        (<- (car-address cdr-address) (address))
        (<- (memory-rewritten memory-orig)
          (do
            (<- (memory-target)
              ((lambda (cont)
                (cond
                  ((isnil memory)
                    (cont nil nil))
                  (car-address
                    (memory cont))
                  (t
                    (do
                      (<- (car-memory cdr-memory) (memory))
                      (cont cdr-memory car-memory)))))))
            (memory-write* memory-target cdr-address value)))
        (if car-address
          (cont (cons memory-rewritten memory-orig))
          (cont (cons memory-orig memory-rewritten)))))))

(defmacro-lazy eval-bool (expr)
  `(lambda (cont)
    (if ,expr
      (cont t)
      (cont nil))))

(defrec-lazy add* (initcarry is-add n m cont)
  (cond
    ((isnil n)
      (cont nil initcarry))
    (t
      (do
        (<- (car-n cdr-n) (n))
        (<- (car-m cdr-m) (m))
        (<- (curlist carry) (add* initcarry is-add cdr-n cdr-m))
        (let* not-carry (not carry))
        (let* car-m (if is-add car-m (not car-m)))
        (<- (curbit)
          ((eval-bool
            (if car-n
              (if car-m
                carry
                not-carry)
              (if car-m
                not-carry
                carry)))))
        (<- (nextcarry)
          ((eval-bool
            (if car-n
              (if car-m
                t
                carry)
              (if car-m
                carry
                nil)))))
        (cont (cons curbit curlist) nextcarry)))))



;;================================================================
;; Registers
;;================================================================
(def-lazy reg-PC (cons nil (cons nil nil)))
(def-lazy reg-A  (cons nil (cons t nil)))
(def-lazy reg-D  (cons t (cons nil (cons nil nil))))
(def-lazy reg-SP (cons t (cons nil (cons t nil))))
(def-lazy reg-B  (cons t (cons t (cons nil nil))))
(def-lazy reg-BP (4 (lambda (x f) (f t x)) nil))
(def-lazy reg-C  (cons t (cons t (cons t (cons nil nil)))))



;;================================================================
;; Arithmetic
;;================================================================
(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (do
    (if-then-return (isnil n)
      cmpret-eq)
    (<- (car-n cdr-n) (n))
    (<- (car-m cdr-m) (m))
    (let* next (cmp* cdr-n cdr-m))
    (if car-n
      (if car-m
        next
        cmpret-lt)
      (if car-m
        cmpret-gt
        next))))

(defun-lazy cmp-gt (f) (f nil nil t))
(defun-lazy cmp-lt (f) (f nil t   nil))
(defun-lazy cmp-eq (f) (f t   nil nil))
(defun-lazy cmp-le (f) (f t   t   nil))
(defun-lazy cmp-ge (f) (f t   nil t))
(defun-lazy cmp-ne (f) (f nil t   t))

(defmacro-lazy cmp (n m enum-cmp)
  `(,enum-cmp (cmp* ,n ,m)))


;;================================================================
;; I/O
;;================================================================
(defmacro-lazy 8-to-24-bit* (n)
  `(16 (lambda (x f) (f t x)) ,n))

(defmacro-lazy 24-to-8-bit* (n)
  `(16 cdr* ,n))


;;================================================================
;; Evaluation
;;================================================================
(defun-lazy lookup-src-if-imm* (reg src-is-imm *src cont)
  (if src-is-imm
    (cont *src)
    (lookup-tree* reg *src cont)))


(defrec-lazy eval (memory progtree stdin curblock reg)
  (do
    (let* jumpto
      (lambda (jmp)
        (do
          (memory-write* reg reg-PC jmp)
          (lookup-tree* progtree jmp)
          (eval memory progtree stdin))))
    (cond
      ((isnil curblock)
        (do
          (<- (sum carry) (add* nil t int-zero (cdr (cdr reg))))
          (jumpto sum)))
      ;; Checks if (car curblock)  == t == (lambda (x y) x).
      ;; `jumpto` is a placeholder and can be any term.
      ;; It is used since it is the first visible variable and encodes to `10`, which is short.
      (((car curblock) (lambda (a b c d) t) (lambda (b c d) nil) jumpto jumpto jumpto jumpto)
        SYS-STRING-TERM)
      (t
        (do
          (<- (curinst nextblock) (curblock))
          (let* eval-reg (eval memory progtree stdin nextblock))
          ;; (<- (inst-type opt) (curinst))
          ;; (<- (src-is-imm *src) (opt)) ;; Delayed destruction: *dst
          (<- (inst-type src-is-imm *src) (curinst)) ;; Delayed destruction: *dst
          (<- (src *dst) (lookup-src-if-imm* reg src-is-imm *src))
          **instruction-typematch**)))))


;;================================================================
;; Instructions
;;================================================================
(def-lazy   inst-exit    nil)
(defun-lazy inst-io      (i1 i2 i3 i4 i5 i6 i7 i8) i1)
(defun-lazy inst-jumpcmp (i1 i2 i3 i4 i5 i6 i7 i8) i2)
(defun-lazy inst-cmp     (i1 i2 i3 i4 i5 i6 i7 i8) i3)
(defun-lazy inst-jmp     (i1 i2 i3 i4 i5 i6 i7 i8) i4)
(defun-lazy inst-load    (i1 i2 i3 i4 i5 i6 i7 i8) i5)
(defun-lazy inst-store   (i1 i2 i3 i4 i5 i6 i7 i8) i6)
(defun-lazy inst-addsub  (i1 i2 i3 i4 i5 i6 i7 i8) i7)
(defun-lazy inst-mov     (i1 i2 i3 i4 i5 i6 i7 i8) i8)

(def-lazy **instruction-typematch**
  (inst-type
    io-case
    jumpcmp-case
    cmp-case
    jmp-case
    load-case
    store-case
    addsub-case
    mov-case
    ))

(defun-lazy io-putc (x1 x2) x2)
(defun-lazy io-getc (x1 x2) x1)

(defmacro-lazy cons4 (x1 x2 x3 x4)
  `(lambda (f) (f ,x1 ,x2 ,x3 ,x4)))


(def-lazy addsub-case
  ;; Instruction structure: (cons4 inst-store [src-isimm] [src] (cons [*dst] is-sub))
  (do
    (<- (*dst is-add) (*dst))
    (<- (sum carry)
      ((do
        (lookup-tree* reg *dst)
        (add* is-add is-add))
       src))
    (memory-write* reg *dst sum eval-reg)))

(def-lazy store-case
  ;; Instruction structure: (cons4 inst-store [dst-isimm] [dst-memory] [source])
  ;; Note that the destination is stored in the variable *src
  (do
    (<- (memory) ((lookup-tree* reg *dst (memory-write* memory src))))
    (eval memory progtree stdin nextblock reg)))

(def-lazy mov-case
  ;; Instruction structure:: (cons4 inst-mov [src-isimm] [src] [dst])
  (memory-write* reg *dst src eval-reg))

(def-lazy jmp-case
  ;; Instruction structure:: (cons4 inst-jmp [jmp-isimm] [jmp] _)
  (jumpto src))

(def-lazy jumpcmp-case
  ;; Instruction structure: (cons4 inst-jumpcmp [src-isimm] [src] (cons4 [enum-cmp] [*dst] [jmp-isimm] [jmp]))
  (do
    (<- (enum-cmp jmp-is-imm *jmp *cmp-dst) (*dst))
    (lookup-src-if-imm* reg jmp-is-imm *jmp)
    (lookup-tree* reg *cmp-dst)
    (lambda (dst-value jmp)
      (if (cmp dst-value src enum-cmp)
        (jumpto jmp)
        (eval-reg reg)))))

(def-lazy load-case
  ;; Instruction structure: (cons4 inst-load [src-isimm] [src] [*dst])
  (do
    (<- (value) (lookup-tree* memory src))
    (memory-write* reg *dst value eval-reg)))

(def-lazy cmp-case
  ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
  (do
    (<- (enum-cmp dst) (*dst))
    (<- (dst-value) (lookup-tree* reg dst))
    (<- (sum carry) (add* nil (cmp dst-value src enum-cmp) int-zero int-zero))
    (memory-write* reg dst sum eval-reg)))

(def-lazy io-case
  ;; Instruction structure:
  ;;   getc: (cons4 inst-io nil         [dst] io-getc)
  ;;   putc: (cons4 inst-io [src-isimm] [src] io-putc)
  ;; Typematch over the inst. type
  (*dst
    ;; getc
    (do
      (<- (c stdin)
        ((lambda (return)
          (do
            (if-then-return (isnil stdin)
              (return int-zero stdin))
            (<- (car-stdin cdr-stdin) (stdin))
            (return (8-to-24-bit* car-stdin) cdr-stdin)))))
      (memory-write* reg *src c)
      (eval memory progtree stdin nextblock))
    ;; putc
    (do
      (cons (24-to-8-bit* src) (eval-reg reg)))))


(defun-lazy main (memtree progtree stdin)
  (do
    ;; Share references to functions to prevent them from being inlined multiple times
    (let* Y-comb Y-comb)
    (let* cmp* cmp*)
    (let* add* add*)
    (let* 16 16)
    (<- (int-zero) 
      ((lambda (return)
        (let ((cons-t (lambda (x f) (f t x))))
          (return (16 cons-t (8 cons-t nil)))))))
    (let* memory-write* memory-write*)
    (let* lookup-tree* lookup-tree*)
    (eval
      memtree
      progtree
      stdin
      (lookup-tree* progtree int-zero (lambda (x) x))
      nil)))

(def-lazy SYS-STRING-TERM nil)


;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))
;; (format t (compile-to-ski-lazy main))
(format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
