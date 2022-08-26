(load "./lazy.cl")
(load "./blc-numbers.cl")


(defmacro-lazy cons-cdr-only (x)
  `(lambda (f) (f f ,x)))

(defmacro-lazy new-bintree-node (a b)
  `(cons-cdr-only
    (lambda (x cont)
      (if x
        (cont ,a)
        (cont ,b)))))


(def-lazy int-zero
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t 
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t nil)))))))))))))))))))))))))

(def-lazy int-one
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t 
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node t   
  (new-bintree-node t (new-bintree-node t (new-bintree-node t (new-bintree-node nil nil)))))))))))))))))))))))))


;;================================================================
;; Memory and program
;;================================================================
(defrec-lazy lookup-memory* (memory address cont)
  (cond
    ((isnil memory)
      (cont int-zero))
    ((isnil address)
      (cont memory))
    (t
      (do
        (<- (car-address) ((cdr address) t))
        (<- (cdr-address) ((cdr address) nil))
        (<- (next-memory) ((cdr memory) car-address))
        (lookup-memory* next-memory cdr-address cont)))))

(defrec-lazy lookup-progtree (progtree* address cont)
  (cond
    ((isnil progtree*)
      (cont nil))
    ((isnil address)
      (cont progtree*))
    (t
      (do
        (<- (car-address) ((cdr address) t))
        (<- (cdr-address) ((cdr address) nil))
        (<- (progtree-next*) ((cdr progtree*) car-address))
        (lookup-progtree progtree-next* cdr-address cont)))))

(defrec-lazy memory-write* (memory address value cont)
  (cond
    ((isnil address)
      (cont value))
    ((isnil memory)
      (do
        (<- (car-address) ((cdr address) t))
        (<- (cdr-address) ((cdr address) nil))
        (<- (memory-rewritten) (memory-write* nil cdr-address value))
        (if car-address
          (cont (new-bintree-node memory-rewritten nil))
          (cont (new-bintree-node nil memory-rewritten)))))
    (t
      (do
        (<- (car-address) ((cdr address) t))
        (<- (cdr-address) ((cdr address) nil))
        (<- (memory-target) ((cdr memory) car-address))
        (<- (memory-rewritten) (memory-write* memory-target cdr-address value))
        (<- (memory-orig) ((cdr memory) (not car-address)))
        (if car-address
          (cont (new-bintree-node memory-rewritten memory-orig))
          (cont (new-bintree-node memory-orig memory-rewritten)))))))

(defrec-lazy reverse-generator* (g curgen cont)
  (if (isnil g)
    (cont curgen)
    (do
      (<- (car-g) ((cdr g) t))
      (<- (cdr-g) ((cdr g) nil))
      (if car-g
        (reverse-generator* cdr-g (new-bintree-node t curgen) cont)
        (reverse-generator* cdr-g (new-bintree-node nil curgen) cont)))))

(defun-lazy reverse* (l cont)
  (reverse-generator* l nil cont))


(defrec-lazy increment-pc-reverse (pc curlist carry cont)
  (cond
    ((isnil pc)
      (cont curlist))
    (t
      (do
        (<- (car-pc) ((cdr pc) t))
        (<- (cdr-pc) ((cdr pc) nil))
        (if (not (xor car-pc carry))
          (do
            ;; (let* curbit t)
            (if (or car-pc carry)
              (do
                ;; (let* nextcarry t)
                (increment-pc-reverse cdr-pc (new-bintree-node t curlist) t cont))
              (do
                ;; (let* nextcarry nil)
                (increment-pc-reverse cdr-pc (new-bintree-node t curlist) nil cont))))
          (do
            ;; (let* curbit nil)
            (if (or car-pc carry)
              (do
                ;; (let* nextcarry t)
                (increment-pc-reverse cdr-pc (new-bintree-node nil curlist) t cont))
              (do
                ;; (let* nextcarry nil)
                (increment-pc-reverse cdr-pc (new-bintree-node nil curlist) nil cont)))))))))

(defun-lazy increment-pc* (pc cont)
  (do
    (<- (pc) (reverse* pc))
    (<- (pc-rev) (increment-pc-reverse pc nil nil))
    (cont pc-rev)))

(defrec-lazy add-reverse* (n m curlist carry cont)
  (cond
    ((isnil n)
      (cont curlist))
    (t
      (do
        (<- (car-n) ((cdr n) t))
        (<- (cdr-n) ((cdr n) nil))
        (<- (car-m) ((cdr m) t))
        (<- (cdr-m) ((cdr m) nil))
        (if (xor (not car-n) (xor (not car-m) (not carry)))
          (do
            ;; (let* curbit nil)
            (if (or
                  (and car-n carry)
                  (and car-m carry)
                  (and car-n car-m))
              ;; nextcarry
              (add-reverse* cdr-n cdr-m (new-bintree-node nil curlist) t cont)
              (add-reverse* cdr-n cdr-m (new-bintree-node nil curlist) nil cont)))
          (do
            ;; (let* curbit t)
            (if (or
                  (and car-n carry)
                  (and car-m carry)
                  (and car-n car-m))
              ;; nextcarry
              (add-reverse* cdr-n cdr-m (new-bintree-node t curlist) t cont)
              (add-reverse* cdr-n cdr-m (new-bintree-node t curlist) nil cont))))))))



;;================================================================
;; Registers
;;================================================================
(def-lazy reg-A  (new-bintree-node nil (new-bintree-node nil (new-bintree-node nil nil))))
(def-lazy reg-B  (new-bintree-node t   (new-bintree-node nil (new-bintree-node nil nil))))
(def-lazy reg-C  (new-bintree-node nil (new-bintree-node t   (new-bintree-node nil nil))))
(def-lazy reg-D  (new-bintree-node t   (new-bintree-node t   (new-bintree-node nil nil))))
(def-lazy reg-SP (new-bintree-node nil (new-bintree-node nil (new-bintree-node t   nil))))
(def-lazy reg-BP (new-bintree-node t   (new-bintree-node nil (new-bintree-node t   nil))))
(def-lazy reg-PC (new-bintree-node nil (new-bintree-node t   (new-bintree-node t   nil))))

(defun-lazy reg-read* (reg regptr cont)
  (do
    (<- (value) (lookup-memory* reg regptr))
    (cont value)))

(defun-lazy reg-write* (reg value regptr cont)
  (do
    (<- (reg) (memory-write* reg regptr value))
    (cont reg)))



;;================================================================
;; Instructions
;;================================================================
(defun-lazy inst-add     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i9)
(defun-lazy inst-store   (i1 i2 i3 i4 i5 i6 i7 i8 i9) i8)
(defun-lazy inst-mov     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i7)
(defun-lazy inst-jmp     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i6)
(defun-lazy inst-jumpcmp (i1 i2 i3 i4 i5 i6 i7 i8 i9) i5)
(defun-lazy inst-load    (i1 i2 i3 i4 i5 i6 i7 i8 i9) i4)
(defun-lazy inst-cmp     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i3)
(defun-lazy inst-sub     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i2)
(defun-lazy inst-io-int  (i1 i2 i3 i4 i5 i6 i7 i8 i9) i1)

(defun-lazy io-int-putc (x1 x2 x3) x3)
(defun-lazy io-int-getc (x1 x2 x3) x2)
(defun-lazy io-int-exit (x1 x2 x3) x1)

(defmacro-lazy car4-1 (f) `(,f (lambda (x1 x2 x3 x4) x1)))
(defmacro-lazy car4-2 (f) `(,f (lambda (x1 x2 x3 x4) x2)))
(defmacro-lazy car4-3 (f) `(,f (lambda (x1 x2 x3 x4) x3)))
(defmacro-lazy car4-4 (f) `(,f (lambda (x1 x2 x3 x4) x4)))
(defmacro-lazy cons4 (x1 x2 x3 x4)
  `(lambda (f) (f ,x1 ,x2 ,x3 ,x4)))


;;================================================================
;; Arithmetic
;;================================================================
(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (cond ((isnil n)
          cmpret-eq)
        (t
          (do
            (<- (car-n) ((cdr n) t))
            (<- (cdr-n) ((cdr n) nil))
            (<- (car-m) ((cdr m) t))
            (<- (cdr-m) ((cdr m) nil))
            (cond ((and (not car-n) car-m)
                    cmpret-gt)
                  ((and car-n (not car-m))
                    cmpret-lt)
                  (t
                    (cmp* cdr-n cdr-m)))))))

(defun-lazy cmp-gt (x1 x2 x3 x4 x5 x6) x6)
(defun-lazy cmp-lt (x1 x2 x3 x4 x5 x6) x5)
(defun-lazy cmp-eq (x1 x2 x3 x4 x5 x6) x4)
(defun-lazy cmp-le (x1 x2 x3 x4 x5 x6) x3)
(defun-lazy cmp-ge (x1 x2 x3 x4 x5 x6) x2)
(defun-lazy cmp-ne (x1 x2 x3 x4 x5 x6) x1)

(defun-lazy cmp (n m enum-cmp)
  ((cmp* n m)
    (enum-cmp nil t   t   t   nil nil)
    (enum-cmp t   nil t   nil t   nil)
    (enum-cmp t   t   nil nil nil t  )))


;;================================================================
;; I/O
;;================================================================
(defrec-lazy invert-bits-rev* (n curlist cont)
  (cond
    ((isnil n)
      (cont curlist))
    (t
      (do
        (<- (car-n) ((cdr n) t))
        (<- (cdr-n) ((cdr n) nil))
        (if (not car-n)
          (invert-bits-rev* cdr-n (new-bintree-node t curlist) cont)
          (invert-bits-rev* cdr-n (new-bintree-node nil curlist) cont))))))

(defrec-lazy listint-to-gen-rev* (l curgen cont)
  (cond
    ((isnil l)
      (cont curgen))
    (t
      (if (car l)
        (listint-to-gen-rev* (cdr l) (new-bintree-node t curgen) cont)
        (listint-to-gen-rev* (cdr l) (new-bintree-node nil curgen) cont)))))

(defun-lazy 8-to-24-bit* (n cont)
  (do
    (<- (rev-gen) (listint-to-gen-rev* (16 (cons* t) n) nil))
    (<- (gen) (reverse* rev-gen))
    (cont gen)))

(defrec-lazy gen2list (gen)
  (if (isnil gen)
    nil
    (do
      (<- (car-gen) ((cdr gen) t))
      (<- (cdr-gen) ((cdr gen) nil))
      (cons car-gen (gen2list cdr-gen)))))

(defun-lazy 24-to-8-bit* (n cont)
  (cont (16 cdr* (gen2list n))))


;;================================================================
;; Evaluation
;;================================================================
(defun-lazy lookup-src-if-imm (reg src-is-imm *src cont)
  (if src-is-imm
    (cont *src)
    (do
      (<- (src) (reg-read* reg *src))
      (cont src))))

(defrec-lazy eval (reg memory progtree stdin curblock)
      (cond ((isnil curblock)
          (do
            (<- (pc) (reg-read* reg reg-PC))
            (<- (nextpc) (increment-pc* pc))
            (<- (nextblock) (lookup-progtree progtree nextpc))
            (if-then-return (isnil nextblock)
              SYS-STRING-TERM)
            (<- (reg) (reg-write* reg nextpc reg-PC))
            (eval reg memory progtree stdin nextblock)))
        (t
          (do
            (<- (curinst) ((cdr curblock) t))
            (let* *src (car4-3 curinst))
            (<- (src) (lookup-src-if-imm reg (car4-2 curinst) *src))
            (let* *dst (car4-4 curinst))
            (<- (nextblock) ((cdr curblock) nil))
            ;; Typematch on the current instruction's tag
            ((car4-1 curinst)
              ;; ==== inst-io-int ====
              ;; Instruction structure:
              ;;   exit: (cons4 inst-io-int nil         nil   io-int-exit)
              ;;   getc: (cons4 inst-io-int nil         [dst] io-int-getc)
              ;;   putc: (cons4 inst-io-int [src-isimm] [src] io-int-putc)
              ;; Typematch over the inst. type
              (*dst
                ;; exit
                SYS-STRING-TERM
                ;; getc
                (do
                  (<- (c stdin) ((lambda (cont)
                    (if (isnil stdin)
                      (cont int-zero stdin)
                      (do
                        (<- (c) (8-to-24-bit* (car stdin)))
                        (cont c (cdr stdin)))))))
                  (<- (reg) (reg-write* reg c *src))
                  (eval reg memory progtree stdin nextblock))
                ;; putc
                (do
                  (<- (c) (24-to-8-bit* src))
                  (cons c (eval reg memory progtree stdin nextblock))))

              ;; ==== inst-sub ====
              ;; Instruction structure: (cons4 inst-store [src-isimm] [src] [*dst])
              (do
                (<- (v-dst) (reg-read* reg *dst))
                (<- (v-dst-rev) (reverse* v-dst))
                (<- (v-src-rev) (invert-bits-rev* src nil))
                (<- (x) (add-reverse* v-src-rev v-dst-rev nil nil))
                (<- (reg) (reg-write* reg x *dst))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-cmp ====
              ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
              (do
                (let* *dst-cmp (cdr *dst))
                (<- (dst-value) (reg-read* reg *dst-cmp))
                (<- (ret) ((lambda (cont)
                  (if (cmp dst-value src (car *dst))
                    (cont int-one)
                    (cont int-zero)))))
                (<- (reg) (reg-write* reg ret *dst-cmp))
                (eval reg memory progtree stdin nextblock))
 
              ;; ==== inst-load ====
              ;; Instruction structure:: (cons4 inst-load [src-isimm] [src] [*dst])
              (do
                (<- (value) (lookup-memory* memory src))
                (<- (reg) (reg-write* reg value *dst))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-jumpcmp ====
              ;; Instruction structure: (cons4 inst-jumpcmp [src-isimm] [src] (cons4 [enum-cmp] [*dst] [jmp-isimm] [jmp]))
              (do
                (let* *jmp (car4-4 *dst))
                (<- (jmp) (lookup-src-if-imm reg (car4-3 *dst) *jmp))
                (<- (dst-value) (reg-read* reg (car4-2 *dst)))
                (<- (reg nextblock) ((lambda (cont)
                  (if (cmp dst-value src (car4-1 *dst))
                    (do
                      (<- (reg) (reg-write* reg jmp reg-PC))
                      (<- (nextblock) (lookup-progtree progtree jmp))
                      (cont reg nextblock))
                    (cont reg nextblock)))))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-jmp ====
              ;; Instruction structure:: (cons4 inst-jmp [jmp-isimm] [jmp] _)
              (do
                (<- (reg) (reg-write* reg src reg-PC))
                (<- (nextblock) (lookup-progtree progtree src))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-mov ====
              ;; Instruction structure:: (cons4 inst-mov [src-isimm] [src] [dst])
              (do
                (<- (reg) (reg-write* reg src *dst))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-store ====
              ;; Instruction structure: (cons4 inst-store [dst-isimm] [dst-memory] [source])
              ;; Note that the destination is stored in the variable *src
              (do
                (<- (value) (reg-read* reg *dst))
                (<- (memory) (memory-write* memory src value))
                (eval reg memory progtree stdin nextblock))

              ;; ==== inst-add ====
              ;; Instruction structure: (cons4 inst-store [src-isimm] [src] [*dst])
              (do
                (<- (v-dst) (reg-read* reg *dst))
                (<- (v-dst-rev) (reverse* v-dst))
                (<- (v-src-rev) (reverse* src))
                (<- (x) (add-reverse* v-src-rev v-dst-rev nil t))
                (<- (reg) (reg-write* reg x *dst))
                (eval reg memory progtree stdin nextblock)))))))


(defun-lazy main (memtree progtree-cont stdin)
  (do
    ;; Share references to functions to prevent them from being inlined multiple times
    (let* int-zero int-zero)
    (let* int-one int-one)
    (let* lookup-memory* lookup-memory*)
    (let* lookup-progtree lookup-progtree)
    (let* memory-write* memory-write*)
    (let* reverse* reverse*)
    (let* add-reverse* add-reverse*)
    (let* reg-read* reg-read*)
    (let* reg-write* reg-write*)
    (let* cmp cmp)
    (eval
      nil
      memtree
      progtree-cont
      stdin
      (new-bintree-node (cons4 inst-jmp t int-zero nil) nil))))

(defun-lazy debug (stdin)
  (do
    (main nil nil stdin)))

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
