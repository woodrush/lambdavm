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
        (<- (next-memory) ((lambda (cont)
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
                  (t
                    (if car-address
                      (memory cont)
                      (do
                        (<- (car-memory cdr-memory) (memory))
                        (cont cdr-memory car-memory))))))))
            (memory-write* memory-target cdr-address value)))
        (if car-address
          (cont (cons memory-rewritten memory-orig))
          (cont (cons memory-orig memory-rewritten)))))))

(defun-lazy reverse* (l cont)
  ((letrec-lazy reverse** (g curgen)
    (if (isnil g)
      (cont curgen)
      (do
        (<- (car-g cdr-g) (g))
        (reverse** cdr-g (cons car-g curgen)))))
   l nil))

(defun-lazy eval-bool (expr cont)
  (if expr
    (cont t)
    (cont nil)))

(defrec-lazy add-reverse* (initcarry is-add n m cont)
  (cond
    ((isnil n)
      (cont nil initcarry))
    (t
      (do
        (<- (car-n cdr-n) (n))
        (<- (car-m cdr-m) (m))
        (<- (curlist carry) (add-reverse* initcarry is-add cdr-n cdr-m))
        (let* not-carry (not carry))
        (let* car-m (if is-add car-m (not car-m)))
        (<- (curbit)
          (eval-bool
            (if car-n
              (if car-m
                carry
                not-carry)
              (if car-m
                not-carry
                carry))))
        (<- (nextcarry)
          (eval-bool
            (if car-n
              (if car-m
                t
                carry)
              (if car-m
                carry
                nil))))
        (cont (cons curbit curlist) nextcarry)))))



;;================================================================
;; Registers
;;================================================================
(def-lazy reg-A  (lambda (x) (x nil nil nil)))
(def-lazy reg-B  (lambda (x) (x t   nil nil)))
(def-lazy reg-D  (lambda (x) (x nil t   nil)))
(def-lazy reg-SP (lambda (x) (x nil nil t  )))
(def-lazy reg-PC (lambda (x) (x nil t   t  )))
(def-lazy reg-BP (lambda (x) (x t   nil t  )))
(def-lazy reg-C  (lambda (x) (x t   t   nil)))

;; (def-lazy reg-SP (let ((nil nil))(lambda (x) (x nil nil nil))))
;; (def-lazy reg-C  (let ((t t)) (lambda (x) (x t t t))))
;; (def-lazy reg-A  (lambda (x) (x t   nil nil)))
;; (def-lazy reg-D  (lambda (x) (x nil t   nil)))
;; (def-lazy reg-B (lambda (x) (x nil nil t  )))
;; (def-lazy reg-BP (lambda (x) (x t   nil t  )))
;; (def-lazy reg-PC (lambda (x) (x nil t   t  )))

;; (def-lazy reg-A  (lambda (x) (x t t)))
;; (def-lazy reg-D  (lambda (x) (x t nil)))
;; (def-lazy reg-SP (lambda (x) (x nil t t)))
;; (def-lazy reg-B  (lambda (x) (x nil t nil)))
;; (def-lazy reg-BP (lambda (x) (x nil nil t)))
;; (def-lazy reg-C  (lambda (x) (x nil nil nil t)))
;; (def-lazy reg-PC (lambda (x) (x nil nil nil nil)))

;; ;; REG-A: 000101100000110000101100000110000010
;; ;; REG-A: 0101000001110011101000000101100000110110000010
;; ;; (def-lazy reg-A  (2 (lambda (x f) (f t x)) nil))
(def-lazy reg-A  (cons t (cons t nil)))
(def-lazy reg-D  (cons t (cons nil  nil)))
(def-lazy reg-SP (cons nil (cons t (cons t nil))))
(def-lazy reg-B  (cons nil (cons t (cons nil nil))))
(def-lazy reg-BP (cons nil (cons nil (cons t nil))))
(def-lazy reg-C  (cons nil (cons nil (cons nil (cons t nil)))))
(def-lazy reg-PC 
(cons nil (cons nil (cons nil (cons nil nil))))
)
;; ;; REG-PC: 00010110000010000101100000100001011000001000010110000010000010
;; ;; REG-PC: 010000010110110000101101110000101101111000010110111110111110000010
;; ;; REG-PC: 010101000110100000011100111010010000000101101110110000010000010
;; ;; REG-PC: 0101010001101000000111001110100000010111000001010000010


;; (defun-lazy regcode-to-regptr (regcode)
;;   (regcode (lambda (x y z) (cons x (cons y (cons z nil))))))
(defun-lazy regcode-to-regptr (regcode)
  regcode)


(defun-lazy reg-read* (reg regptr cont)
  (lookup-tree* reg (regcode-to-regptr regptr) cont))

(defun-lazy reg-write** (reg regptr cont value)
  (memory-write* reg (regcode-to-regptr regptr) value cont))

(defmacro-lazy reg-write* (reg value regptr cont)
  `(reg-write** ,reg ,regptr ,cont ,value))



;;================================================================
;; Arithmetic
;;================================================================
(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (cond
    ((isnil n)
      cmpret-eq)
    (t
      (do
        (<- (car-n cdr-n) (n))
        (<- (car-m cdr-m) (m))
        (let* next (cmp* cdr-n cdr-m))
        (if car-n
          (if car-m
            next
            cmpret-lt)
          (if car-m
            cmpret-gt
            next))))))

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
    (reg-read* reg *src cont)))

(defmacro-lazy isnil-4 (item) `(,item (lambda (a b c d x) nil) t))
(defrec-lazy eval (memory progtree stdin curblock reg)
  (do
    (let* jumpto
      (lambda (jmp)
        (do
          (reg-write* reg jmp reg-PC)
          (lookup-tree* progtree jmp)
          (eval memory progtree stdin))))
    (cond
        ((isnil curblock)
          (do
            (<- (sum carry)
              ((reg-read* reg reg-PC)
               (add-reverse* nil t int-zero)))
            (jumpto sum)))
        ((isnil-4 (car curblock))
          SYS-STRING-TERM)
        (t
          (do
            (<- (curinst nextblock) (curblock))
            (let* eval-reg (eval memory progtree stdin nextblock))
            (<- (inst-type src-is-imm *src *dst) (curinst))
            (<- (src) (lookup-src-if-imm* reg src-is-imm *src))
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
        ((lambda (cont) (cont src))) ; src
        (reg-read* reg *dst) ; dst
        (add-reverse* is-add is-add))))
    (reg-write** reg *dst eval-reg sum)))

(def-lazy store-case
  ;; Instruction structure: (cons4 inst-store [dst-isimm] [dst-memory] [source])
  ;; Note that the destination is stored in the variable *src
  (do
    (<- (memory) (reg-read* reg *dst (memory-write* memory src)))
    (eval memory progtree stdin nextblock reg)))

(def-lazy mov-case
  ;; Instruction structure:: (cons4 inst-mov [src-isimm] [src] [dst])
  (reg-write* reg src *dst eval-reg))

(def-lazy jmp-case
  ;; Instruction structure:: (cons4 inst-jmp [jmp-isimm] [jmp] _)
  (jumpto src))

(def-lazy jumpcmp-case
  ;; Instruction structure: (cons4 inst-jumpcmp [src-isimm] [src] (cons4 [enum-cmp] [*dst] [jmp-isimm] [jmp]))
  (do
    (<- (enum-cmp jmp-is-imm *jmp *cmp-dst) (*dst))
    (lookup-src-if-imm* reg jmp-is-imm *jmp)
    (reg-read* reg *cmp-dst)
    (lambda (dst-value jmp)
      (if (cmp dst-value src enum-cmp)
        (jumpto jmp)
        (eval memory progtree stdin nextblock reg)))))

(def-lazy load-case
  ;; Instruction structure:: (cons4 inst-load [src-isimm] [src] [*dst])
  (lookup-tree* memory src
    (reg-write** reg *dst eval-reg)))

(def-lazy cmp-case
  ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
  (do
    (<- (enum-cmp dst) (*dst))
    (<- (src x)
      ((reg-read* reg dst)
       (lambda (dst-value cont)
         (if (cmp dst-value src enum-cmp)
           ;; (reverse* (cons nil (cdr int-zero)) cont)
           (add-reverse* nil t int-zero int-zero cont)
           (cont int-zero (lambda (x) x))))))
     (reg-write** reg dst eval-reg src)))

(def-lazy io-case
  ;; Instruction structure:
  ;;   getc: (cons4 inst-io nil         [dst] io-getc)
  ;;   putc: (cons4 inst-io [src-isimm] [src] io-putc)
  ;; Typematch over the inst. type
  (*dst
    ;; getc
    (do
      (<- (c stdin)
        ((lambda (cont)
          (do
            (if-then-return (isnil stdin)
              (cont int-zero stdin))
            (<- (car-stdin cdr-stdin) (stdin))
            (cont (8-to-24-bit* car-stdin) cdr-stdin)))))
      ((reg-write* reg c *src (eval memory progtree stdin nextblock))))
    ;; putc
    (do
      (cons (24-to-8-bit* src) (eval-reg reg)))))


(defun-lazy main (memtree progtree stdin)
  (do
    ;; Share references to functions to prevent them from being inlined multiple times
    (let* Y-comb Y-comb)
    (let* isnil isnil)
    (let* cmp* cmp*)
    (let* add-reverse* add-reverse*)
    (let* 16 16)
    (let* memory-write* memory-write*)
    (<- (int-zero) 
      ((lambda (cont)
        (let ((cons-t (lambda (x f) (f t x))))
          (cont (16 cons-t (8 cons-t nil)))))))
    (let* lookup-tree* lookup-tree*)
    ;; (let* reverse* reverse*)
    (<- (reg-read* reg-write**)
      ((lambda (cont)
        (let ((regcode-to-regptr regcode-to-regptr))
          (cont reg-read* reg-write**)))))
    (eval
      memtree
      progtree
      stdin
      (list (lambda (f) (f inst-jmp t int-zero f)))
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
