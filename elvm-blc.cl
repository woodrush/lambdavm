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


(def-lazy SYS-N-BITS (+ 16 8))
;; (def-lazy int-zero (take SYS-N-BITS (inflist nil)))
;; (def-lazy int-zero
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t nil)))))))))))))))))))))))))

;; (def-lazy int-one
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons nil nil)))))))))))))))))))))))))

;; (def-lazy int-one (8-to-24-bit* (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t)))))))) (lambda (x) x)))

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

;; (def-lazy int-two
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons nil (cons t nil)))))))))))))))))))))))))

;; (def-lazy address-one
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
;;   (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons nil nil)))))))))))))))))))))))))

;;================================================================
;; Memory and program
;;================================================================
(defrec-lazy lookup-memory* (progtree address cont)
  (cond
    ((isnil progtree)
      (cont int-zero))
    ((isnil address)
      (cont progtree))
    (t
      (do
        (<- (car-address) ((cdr address) t))
        (<- (cdr-address) ((cdr address) nil))
        (<- (next-memory) ((cdr progtree) car-address))
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

;; (defrec-lazy reverse** (l curlist cont)
;;   (if (isnil l)
;;     (cont curlist)
;;     (reverse** (cdr l) (cons (car l) curlist) cont)))

;; (defun-lazy reverse* (l cont)
;;   (reverse** l nil cont))

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
;; (def-lazy reg-A  (list nil nil nil))
;; (def-lazy reg-B  (list t nil nil))
;; (def-lazy reg-C  (list nil t nil))
;; (def-lazy reg-D  (list t t nil))
;; (def-lazy reg-SP (list nil nil t))
;; (def-lazy reg-BP (list t nil t))
;; (def-lazy reg-PC (list nil t t))

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
    (<- (rev-gen) (listint-to-gen-rev*
      (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t
      (cons t (cons t (cons t (cons t (cons t (cons t (cons t (cons t n))))))))))))))))
      nil))
    (<- (gen) (reverse* rev-gen))
    (cont gen)))

(defrec-lazy gen2list (gen)
  (if (isnil gen)
    nil
    (do
      (<- (car-gen) ((cdr gen) t))
      (<- (cdr-gen) ((cdr gen) nil))
      (if car-gen
        (cons t (gen2list cdr-gen))
        (cons nil (gen2list cdr-gen))))))

(defun-lazy 24-to-8-bit* (n cont)
  (do
    ;; (<- (n-rev) (invert-bits-rev* n nil))
    (let* ret (gen2list n))
    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))

    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))

    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))

    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))
    (let* ret (cdr ret))

    (cont ret)))


;;================================================================
;; Evaluation
;;================================================================
(defrec-lazy eval (reg memory progtree stdin curblock)
      (cond ((isnil curblock)
          (do
            (<- (pc) (reg-read* reg reg-PC))
            (<- (nextpc) (increment-pc* pc))
            (<- (nextblock) (lookup-progtree progtree nextpc))
            (cond
              ((isnil nextblock)
                SYS-STRING-TERM)
              (t
                (do
                  (<- (reg) (reg-write* reg nextpc reg-PC))
                  (eval reg memory progtree stdin nextblock))))))
        (t
          ;; Prevent frequently used functions from being inlined every time

          (do   (let* cmp cmp)
                (<- (curinst) ((cdr curblock) t))
                (let* *src (car4-3 curinst))
                (let* src-is-imm (car4-2 curinst))
                (let* src
                  (do
                    (if-then-return (car4-2 curinst)
                      *src)
                    (<- (src) (reg-read* reg *src))
                    src))
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
                (cond ((isnil stdin)
                        (do
                          (<- (reg) (reg-write* reg int-zero *src))
                          (eval reg memory progtree stdin nextblock)))
                      (t
                        (do
                          (<- (c) (8-to-24-bit* (car stdin)))
                          (<- (reg) (reg-write* reg c *src))
                          (eval reg memory progtree (cdr stdin) nextblock))))
                ;; putc
                (cond
                  (src-is-imm
                    (do
                      (let* src *src)
                      (<- (c) (24-to-8-bit* src))
                      (cons c (eval reg memory progtree stdin nextblock))))
                  (t
                    (do
                      (<- (src) (reg-read* reg *src))
                      (<- (c) (24-to-8-bit* src))
                      (cons c (eval reg memory progtree stdin nextblock))))))

              ;; ==== inst-sub ====
              ;; Instruction structure: (cons4 inst-store [src-isimm] [src] [*dst])
              (do
                (<- (v-dst) (reg-read* reg *dst))
                (<- (v-dst-rev) (reverse* v-dst))
                (cond
                  (src-is-imm
                    (do
                      (let* src *src)
                      (<- (v-src-rev) (invert-bits-rev* src nil))
                      (<- (x) (add-reverse* v-src-rev v-dst-rev nil nil))
                      (<- (reg) (reg-write* reg x *dst))
                      (eval reg memory progtree stdin nextblock)))
                  (t
                    (do
                      (<- (src) (reg-read* reg *src))
                      (<- (v-src-rev) (invert-bits-rev* src nil))
                      (<- (x) (add-reverse* v-src-rev v-dst-rev nil nil))
                      (<- (reg) (reg-write* reg x *dst))
                      (eval reg memory progtree stdin nextblock)))))

              ;; ==== inst-cmp ====
              ;; Instruction structure: (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (do
                      (let* *dst-cmp (cdr *dst))
                      (<- (dst-value) (reg-read* reg *dst-cmp))
                      (if (cmp dst-value src (car *dst))
                        (do
                          (<- (reg) (reg-write* reg int-one *dst-cmp))
                          (eval reg memory progtree stdin nextblock))
                        (do
                          (<- (reg) (reg-write* reg int-zero *dst-cmp))
                          (eval reg memory progtree stdin nextblock))))))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (do
                      (let* *dst-cmp (cdr *dst))
                      (<- (dst-value) (reg-read* reg *dst-cmp))
                      (if (cmp dst-value src (car *dst))
                        (do
                          (<- (reg) (reg-write* reg int-one *dst-cmp))
                          (eval reg memory progtree stdin nextblock))
                        (do
                          (<- (reg) (reg-write* reg int-zero *dst-cmp))
                          (eval reg memory progtree stdin nextblock)))))))


              ;; ==== inst-load ====
              ;; Instruction structure:: (cons4 inst-load [src-isimm] [src] [*dst])
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (<- (value) (lookup-memory* memory src))
                    (<- (reg) (reg-write* reg value *dst))
                    (eval reg memory progtree stdin nextblock)))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (<- (value) (lookup-memory* memory src))
                    (<- (reg) (reg-write* reg value *dst))
                    (eval reg memory progtree stdin nextblock))))

              ;; ==== inst-jumpcmp ====
              ;; Instruction structure: (cons4 inst-jumpcmp [src-isimm] [src] (cons4 [enum-cmp] [*dst] [jmp-isimm] [jmp]))
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (do
                      (let* *jmp (car4-4 *dst))
                      (<- (dst-value) (reg-read* reg (car4-2 *dst)))
                      (if (car4-3 *dst)
                        (do
                          (let* jmp *jmp)
                          (if (cmp dst-value src (car4-1 *dst))
                            (do
                              (<- (reg) (reg-write* reg jmp reg-PC))
                              (<- (nextblock) (lookup-progtree progtree jmp))
                              (eval reg memory progtree stdin nextblock))
                            (do
                              (eval reg memory progtree stdin nextblock))))
                        (do
                          (<- (jmp) (reg-read* reg *jmp))
                          (if (cmp dst-value src (car4-1 *dst))
                            (do
                              (<- (reg) (reg-write* reg jmp reg-PC))
                              (<- (nextblock) (lookup-progtree progtree jmp))
                              (eval reg memory progtree stdin nextblock))
                            (do
                              (eval reg memory progtree stdin nextblock))))))))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (do
                      (let* *jmp (car4-4 *dst))
                      (<- (dst-value) (reg-read* reg (car4-2 *dst)))
                      (if (car4-3 *dst)
                        (do
                          (let* jmp *jmp)
                          (if (cmp dst-value src (car4-1 *dst))
                            (do
                              (<- (reg) (reg-write* reg jmp reg-PC))
                              (<- (nextblock) (lookup-progtree progtree jmp))
                              (eval reg memory progtree stdin nextblock))
                            (do
                              (eval reg memory progtree stdin nextblock))))
                        (do
                          (<- (jmp) (reg-read* reg *jmp))
                          (if (cmp dst-value src (car4-1 *dst))
                            (do
                              (<- (reg) (reg-write* reg jmp reg-PC))
                              (<- (nextblock) (lookup-progtree progtree jmp))
                              (eval reg memory progtree stdin nextblock))
                            (do
                              (eval reg memory progtree stdin nextblock)))))))))


              ;; ==== inst-jmp ====
              ;; Instruction structure:: (cons4 inst-jmp [jmp-isimm] [jmp] _)
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (<- (reg) (reg-write* reg src reg-PC))
                    (<- (nextblock) (lookup-progtree progtree src))
                    (eval reg memory progtree stdin nextblock)))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (<- (reg) (reg-write* reg src reg-PC))
                    (<- (nextblock) (lookup-progtree progtree src))
                    (eval reg memory progtree stdin nextblock))))

              ;; ==== inst-mov ====
              ;; Instruction structure:: (cons4 inst-mov [src-isimm] [src] [dst])
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (<- (reg) (reg-write* reg src *dst))
                    (eval reg memory progtree stdin nextblock)))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (<- (reg) (reg-write* reg src *dst))
                    (eval reg memory progtree stdin nextblock))))
              ;; (do
              ;;   (<- (reg) (reg-write* reg src *dst))
              ;;   (eval reg memory progtree stdin nextblock))

              ;; ==== inst-store ====
              ;; Instruction structure: (cons4 inst-store [dst-isimm] [dst-memory] [source])
              ;; Note that the destination is stored in the variable *src
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (<- (value) (reg-read* reg *dst))
                    (<- (memory) (memory-write* memory src value))
                    (eval reg memory progtree stdin nextblock)))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (<- (value) (reg-read* reg *dst))
                    (<- (memory) (memory-write* memory src value))
                    (eval reg memory progtree stdin nextblock))))
              ;; (do
              ;;   (<- (value) (reg-read* reg *dst))
              ;;   (<- (memory) (memory-write* memory src value))
              ;;   (eval reg memory progtree stdin nextblock))

              ;; ==== inst-add ====
              ;; Instruction structure: (cons4 inst-store [src-isimm] [src] [*dst])
              (cond
                (src-is-imm
                  (do
                    (let* src *src)
                    (<- (v-dst) (reg-read* reg *dst))
                    (<- (v-dst-rev) (reverse* v-dst))
                    (<- (v-src-rev) (reverse* src))
                    (<- (x) (add-reverse* v-src-rev v-dst-rev nil t))
                    (<- (reg) (reg-write* reg x *dst))
                    (eval reg memory progtree stdin nextblock)))
                (t
                  (do
                    (<- (src) (reg-read* reg *src))
                    (<- (v-dst) (reg-read* reg *dst))
                    (<- (v-dst-rev) (reverse* v-dst))
                    (<- (v-src-rev) (reverse* src))
                    (<- (x) (add-reverse* v-src-rev v-dst-rev nil t))
                    (<- (reg) (reg-write* reg x *dst))
                    (eval reg memory progtree stdin nextblock))))


              ;; (do
              ;;   (<- (v-dst) (reg-read* reg *dst))
              ;;   (<- (v-dst-rev) (reverse* v-dst))
              ;;   (<- (v-src-rev) (reverse* src))
              ;;   (<- (x) (add-reverse* v-src-rev v-dst-rev nil t))
              ;;   (<- (reg) (reg-write* reg x *dst))
              ;;   (eval reg memory progtree stdin nextblock))

                )))))


(defun-lazy main (memtree progtree-cont stdin)
  (do
    (let* take take)
    (let* int-zero int-zero)
    ;; (let* progtree
    ;;   (cons (cons (cons (cons (cons (cons (cons (cons
    ;;   (cons (cons (cons (cons (cons (cons (cons (cons
    ;;   (cons (cons (cons (cons (cons (cons (cons (cons
    ;;     (list
    ;;       ;; (cons4 inst-io-int t S-24bit io-int-putc)
    ;;       ;; (cons4 inst-mov t A-24bit reg-A)
    ;;       ;; (cons4 inst-add t int-two reg-A)
    ;;       ;; (cons4 inst-io-int nil reg-A io-int-putc)
    ;;       ;; (cons4 inst-add t int-two reg-A)
    ;;       ;; (cons4 inst-io-int nil reg-A io-int-putc)
    ;;       ;; (cons4 inst-add t int-two reg-A)
    ;;       ;; (cons4 inst-io-int nil reg-A io-int-putc)
    ;;       ;; (cons4 inst-add t int-two reg-A)
    ;;       ;; (cons4 inst-io-int nil reg-A io-int-putc)
    ;;       (cons4 inst-io-int nil reg-A io-int-getc)
    ;;       (cons4 inst-mov nil reg-A reg-C)
    ;;       (cons4 inst-store t int-zero reg-C)
    ;;       )
    ;;     (list
    ;;       (cons4 inst-load t int-zero reg-B)
    ;;       (cons4 inst-io-int nil reg-B io-int-putc)
    ;;       (cons4 inst-sub t int-one reg-B)
    ;;       (cons4 inst-store t int-zero reg-B)

    ;;       (cons4 inst-mov nil reg-B reg-D)
    ;;       (cons4 inst-cmp t int-one (cons cmp-eq reg-D))
    ;;       (cons4 inst-jumpcmp nil reg-D (cons4 cmp-eq reg-D t int-one))

    ;;       ;; (cons4 inst-jumpcmp t int-one (cons4 cmp-lt reg-B t int-one))
    ;;       )


    ;;   )
    ;;   nil) nil) nil) nil) nil) nil) nil)
    ;;   nil) nil) nil) nil) nil) nil) nil) nil)
    ;;   nil) nil) nil) nil) nil) nil) nil) nil))

    ;; (<- (S-24bit) (8-to-24-bit* "S"))
    ;; (<- (A-24bit) (8-to-24-bit* "A"))
    (eval
      nil
      memtree
      progtree-cont

      ;; (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil
      ;; (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil
      ;; (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons
      ;;     ))))))))))))))))))))))))
      ;; nil
      stdin
      (new-bintree-node
        ;; (cons4 inst-io-int t A-24bit io-int-putc)
        ;; (cons4 inst-io-int t A-24bit io-int-putc)
        ;; (cons4 inst-mov t (8-to-24-bit "J") reg-A)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-B io-int-getc)
        ;; (cons4 inst-io-int nil reg-B io-int-putc)
        ;; (cons4 inst-io-int t (8-to-24-bit "I") io-int-putc)
        ;; (cons4 inst-io-int t (8-to-24-bit "B") io-int-putc)
        (cons4 inst-jmp t int-zero nil)
        nil))
    )
  )

(defun-lazy debug (stdin)
  (do
    (main nil nil stdin)))

(def-lazy SYS-STRING-TERM nil)

;; (def-lazy "*" (cons t (cons t (cons nil (cons t (cons nil (cons t (cons nil (cons t nil)))))))))



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
