(load "./lazy.cl")


(def-lazy SYS-N-BITS (+ 16 8))
(def-lazy int-zero (take SYS-N-BITS (inflist nil)))
(def-lazy int-one (cons t (take (pred SYS-N-BITS) (inflist nil))))
(def-lazy int-minusone (take SYS-N-BITS (inflist t)))

(defmacro-lazy do-continuation* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do-continuation*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        ((eq 'let* (car (car proc)))
          (let* ((topproc (car proc))
                 (varname (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do-continuation*
                ,(append `(let ((,varname ,body))) `(,top))
                ,@(cdr proc))))
        (t
          `(do-continuation*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do-continuation (&rest proc)
  `(do-continuation* ,@(reverse proc)))


;;================================================================
;; Memory and program
;;================================================================
(def-lazy init-memory nil)

(defrec-lazy memory-read (memory address)
  (cond
    ((isnil memory)
      int-zero)
    ((isnil address)
      memory)
    (t
      (memory-read (memory (car address)) (cdr address)))))

(defrec-lazy lookup-progtree (progtree address)
  (cond
    ((isnil progtree)
      progtree)
    ((isnil address)
      progtree)
    (t
      (lookup-progtree (progtree (car address)) (cdr address)))))

(defrec-lazy memory-write (memory address value)
  (cond
    ((isnil address)
      value)
    ((isnil memory)
      ((car address)
        (cons (memory-write nil (cdr address) value) nil)
        (cons nil (memory-write nil (cdr address) value))))
    (t
      ((car address)
        (cons (memory-write (car memory) (cdr address) value) (cdr memory))
        (cons (car memory) (memory-write (cdr memory) (cdr address) value))))))


(defrec-lazy list2checkpoint-tree (proglist depth)
  (cond
    ((isnil proglist)
      (cons nil nil))
    ((isnil depth)
      (cons proglist (cdr proglist)))
    (t
      (let ((rightstate (list2checkpoint-tree proglist (cdr depth)))
            (righttree (car rightstate))
            (right-restproglist (cdr rightstate))
            (leftstate (list2checkpoint-tree right-restproglist (cdr depth)))
            (lefttree (car leftstate))
            (left-restproglist (cdr leftstate)))
        (cons (cons lefttree righttree) left-restproglist)))))

(defrec-lazy list2tree (memlist depth)
  (cond
    ((isnil memlist)
      (cons nil nil))
    ((isnil depth)
      (cons (car memlist) (cdr memlist)))
    (t
      (let ((rightstate (list2tree memlist (cdr depth)))
            (righttree (car rightstate))
            (right-restmemlist (cdr rightstate))
            (leftstate (list2tree right-restmemlist (cdr depth)))
            (lefttree (car leftstate))
            (left-restmemlist (cdr leftstate)))
        (cons (cons lefttree righttree) left-restmemlist)))))



;;================================================================
;; Registers
;;================================================================
(defun-lazy reg-A  (r1 r2 r3 r4 r5 r6) r1)
(defun-lazy reg-B  (r1 r2 r3 r4 r5 r6) r2)
(defun-lazy reg-C  (r1 r2 r3 r4 r5 r6) r3)
(defun-lazy reg-D  (r1 r2 r3 r4 r5 r6) r4)
(defun-lazy reg-SP (r1 r2 r3 r4 r5 r6) r5)
(defun-lazy reg-BP (r1 r2 r3 r4 r5 r6) r6)
(defmacro-lazy cons6 (r1 r2 r3 r4 r5 r6)
  `(lambda (f) (f ,r1 ,r2 ,r3 ,r4 ,r5 ,r6)))

(def-lazy init-reg
  (cons6 int-zero int-zero int-zero int-zero int-zero int-zero))

(defmacro-lazy reg-read (reg regptr)
  `(,reg ,regptr))

(defmacro-lazy rr (reg regptr)
  `(reg-read ,reg ,regptr))

(defun-lazy reg-write (reg value regptr)
  (regptr
    (cons6  value         (rr reg reg-B) (rr reg reg-C) (rr reg reg-D) (rr reg reg-SP) (rr reg reg-BP))
    (cons6 (rr reg reg-A)  value         (rr reg reg-C) (rr reg reg-D) (rr reg reg-SP) (rr reg reg-BP))
    (cons6 (rr reg reg-A) (rr reg reg-B)  value         (rr reg reg-D) (rr reg reg-SP) (rr reg reg-BP))
    (cons6 (rr reg reg-A) (rr reg reg-B) (rr reg reg-C)  value         (rr reg reg-SP) (rr reg reg-BP))
    (cons6 (rr reg reg-A) (rr reg reg-B) (rr reg reg-C) (rr reg reg-D)  value          (rr reg reg-BP))
    (cons6 (rr reg reg-A) (rr reg reg-B) (rr reg reg-C) (rr reg reg-D) (rr reg reg-SP)  value         )))


;;================================================================
;; Arithmetic
;;================================================================
(defrec-lazy invert (n)
  (if (isnil n)
    nil
    (cons (not (car n)) (invert (cdr n)))))

(defrec-lazy add-carry (n m carry)
  (cond ((isnil n)
          nil)
        (t
          (if (car n)
            (if (car m)
              (cons carry       (add-carry (cdr n) (cdr m) t))
              (cons (not carry) (add-carry (cdr n) (cdr m) carry)))
            (if (car m)
              (cons (not carry) (add-carry (cdr n) (cdr m) carry))
              (cons carry       (add-carry (cdr n) (cdr m) nil)))))))

(defmacro-lazy add (n m)
  `(add-carry ,n ,m nil))

(defmacro-lazy sub (n m)
  `(add-carry ,n (invert ,m) t))

(defrec-lazy iszero-bit (n)
  (cond ((isnil n)
          t)
        ((car n)
          nil)
        (t
          (iszero-bit (cdr n)))))

(defun-lazy cmpret-eq (r1 r2 r3) r1)
(defun-lazy cmpret-lt (r1 r2 r3) r2)
(defun-lazy cmpret-gt (r1 r2 r3) r3)

(defrec-lazy cmp* (n m)
  (cond ((isnil n)
          cmpret-eq)
        (t
          (let ((ncar (car n))
                (mcar (car m)))
            (cond ((and (not ncar) mcar)
                    cmpret-lt)
                  ((and ncar (not mcar))
                    cmpret-gt)
                  (t
                    (cmp* (cdr n) (cdr m))))))))

;; (defmacro-lazy cmp (n m)
;;   `(cmp* (reverse ,n) (reverse ,m)))

;; (defun-lazy cmp (n m enum-cmp)
;;   (let ((cmp-result (cmp* (reverse n) (reverse m))))
;;     ;; Type match over the cmp type
;;     ;; cmp-result: eq, lt, gt
;;     (enum-cmp
;;       ;; eq
;;       (cmp-result t   nil nil)
;;       ;; ne
;;       (cmp-result nil t   t)
;;       ;; lt
;;       (cmp-result nil t   nil)
;;       ;; gt
;;       (cmp-result nil nil t)
;;       ;; le
;;       (cmp-result t   t   nil)
;;       ;; ge
;;       (cmp-result t   nil t))))

(defun-lazy cmp (n m enum-cmp)
  ((cmp* (reverse n) (reverse m))
    (enum-cmp t nil nil nil t t)
    (enum-cmp nil t t nil t nil)
    (enum-cmp nil t nil t nil t)))

;;================================================================
;; I/O
;;================================================================
(def-lazy powerlist
  ((letrec-lazy pows (n exp)
    (if (iszero exp)
      nil
      (cons n (pows (+ n n) (pred exp)))))
   1 SYS-N-BITS)
  ;; (cons 1 (cons 2 (cons 4 (cons 8 (cons 16 (cons 32 (cons 64 (cons 128 nil))))))))
  )

(def-lazy powerlist-256
  ;; ((letrec-lazy pows (n exp)
  ;;   (if (iszero exp)
  ;;     nil
  ;;     (cons n (pows (+ n n) (pred exp)))))
  ;;  1 SYS-N-BITS)
  (cons 1 (cons 2 (cons 4 (cons 8 (cons 16 (cons 32 (cons 64 (cons 128 nil))))))))
  )

(def-lazy revpowerlist
  (reverse powerlist)
  ;; (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil))))))))
  )
(def-lazy revpowerlist-256
  ;; (reverse powerlist-256)
  (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil))))))))
  )

(defrec-lazy bit2int* (n powerlist)
  (cond ((isnil powerlist)
          0)
        (t
          (if (car n)
            (+ (car powerlist) (bit2int* (cdr n) (cdr powerlist)))
            (bit2int* (cdr n) (cdr powerlist))))))

(defmacro-lazy bit2int-256 (n)
  `(bit2int* ,n powerlist-256))

(defrec-lazy int2bit* (n revpowerlist)
  (cond ((isnil revpowerlist)
          nil)
        ((iszero n)
          (cons nil (int2bit* n (cdr revpowerlist))))
        (t
          (if (<= (car revpowerlist) n)
            (cons t   (int2bit* (- n (car revpowerlist)) (cdr revpowerlist)))
            (cons nil (int2bit* n (cdr revpowerlist)))))))

(defmacro-lazy int2bit (n)
  `(reverse (int2bit* ,n revpowerlist)))

(defmacro-lazy int2bit-512 (n)
  `(if (<= 256 ,n)
    (int2bit 256)
    (append-list (reverse (int2bit* ,n revpowerlist-256)) (take 16 (inflist nil)))))

(defrec-lazy append-list (l item)
  (if (isnil l) item (cons (car l) (append-list (cdr l) item))))

;; (defmacro-lazy mod256 (n)
;;   `(append-list (take 8 ,n) (take 16 (inflist nil))))


;;================================================================
;; Instructions
;;================================================================
(defun-lazy inst-io-int  (i1 i2 i3 i4 i5 i6 i7 i8 i9) i1)
(defun-lazy inst-sub     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i2)
(defun-lazy inst-cmp     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i3)
(defun-lazy inst-load    (i1 i2 i3 i4 i5 i6 i7 i8 i9) i4)
(defun-lazy inst-jumpcmp (i1 i2 i3 i4 i5 i6 i7 i8 i9) i5)
(defun-lazy inst-jmp     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i6)
(defun-lazy inst-mov     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i7)
(defun-lazy inst-store   (i1 i2 i3 i4 i5 i6 i7 i8 i9) i8)
(defun-lazy inst-add     (i1 i2 i3 i4 i5 i6 i7 i8 i9) i9)

(defun-lazy cmp-eq (x1 x2 x3 x4 x5 x6) x1)
(defun-lazy cmp-ne (x1 x2 x3 x4 x5 x6) x2)
(defun-lazy cmp-lt (x1 x2 x3 x4 x5 x6) x3)
(defun-lazy cmp-gt (x1 x2 x3 x4 x5 x6) x4)
(defun-lazy cmp-le (x1 x2 x3 x4 x5 x6) x5)
(defun-lazy cmp-ge (x1 x2 x3 x4 x5 x6) x6)

(defun-lazy io-int-exit (x1 x2 x3) x1)
(defun-lazy io-int-getc (x1 x2 x3) x2)
(defun-lazy io-int-putc (x1 x2 x3) x3)


(defun-lazy car3-1 (f) (f (lambda (x1 x2 x3) x1)))
(defun-lazy car3-2 (f) (f (lambda (x1 x2 x3) x2)))
(defun-lazy car3-3 (f) (f (lambda (x1 x2 x3) x3)))
(defmacro-lazy cons3 (x1 x2 x3)
  `(lambda (f) (f ,x1 ,x2 ,x3)))

(defmacro-lazy car4-1 (f) `(,f (lambda (x1 x2 x3 x4) x1)))
(defmacro-lazy car4-2 (f) `(,f (lambda (x1 x2 x3 x4) x2)))
(defmacro-lazy car4-3 (f) `(,f (lambda (x1 x2 x3 x4) x3)))
(defmacro-lazy car4-4 (f) `(,f (lambda (x1 x2 x3 x4) x4)))
(defmacro-lazy cons4 (x1 x2 x3 x4)
  `(lambda (f) (f ,x1 ,x2 ,x3 ,x4)))


;;================================================================
;; Evaluation
;;================================================================
(defmacro-lazy await (stdin-top body)
  ;; The key ingredient to managing the I/O control flow.
  ;; By inspecting the value of the top character of the standard input and branching depending on its value,
  ;; `await` is able to halt the further execution of `body` until the input is actually provided.
  ;; Since elements of `stdin` are always a number, this form is guaranteed to evaluate to `body`.
  ;; However, since most interpreters do not use that fact during beta reduction
  ;; and expect `stdin` to be an arbitrary lambda form,
  ;; such interpreters cannot deduce that this form always reduces to `body`,
  ;; effectively making this form a method for halting evaluation until the standard input is provided.
  `(if (<= 0 ,stdin-top)
    ,body
    nil))

(defrec-lazy flatten (curlist listlist)
  (cond ((isnil curlist)
          (if (isnil listlist)
            nil
            (flatten (car listlist) (cdr listlist))))
        (t
          (cons (car curlist) (flatten (cdr curlist) listlist)))))

(defrec-lazy eval (reg memory progtree stdin curblock)
  ;; Prevent frequently used functions from being inlined every time
  (let ((reverse-helper reverse-helper)
        ;; (add-carry add-carry)
        ;; (flatten flatten)
        ;; (cmp* cmp*)
        (reg-write reg-write))
    (cond ((isnil curblock)
            SYS-STRING-TERM)
          (t
            (let ((curinst (car curblock))
                  (*src (car4-3 curinst))
                  (src (if (car4-2 curinst) *src (reg-read reg *src)))
                  (*dst (car4-4 curinst))
                  (nextblock (cdr curblock))
                  (eval-reg (lambda (reg) (eval reg memory progtree stdin nextblock))))
              ;; Typematch on the current instruction's tag
              ((car4-1 curinst)
                ;; ==== inst-io-int ====
                ;; Structure:
                ;;   exit: (cons4 inst-io-int nil         nil   io-int-exit)
                ;;   getc: (cons4 inst-io-int nil         [dst] io-int-getc)
                ;;   putc: (cons4 inst-io-int [src-isimm] [src] io-int-putc)
                ;; Typematch over the inst. type
                (*dst
                  ;; exit
                  SYS-STRING-TERM
                  ;; getc
                  (let ((c (car stdin))
                        (c-bit (int2bit-512 c)))
                    ;; Await for the user input
                    (await c
                      (cond
                        ((cmp c-bit (int2bit 256) cmp-eq)
                          (eval (reg-write reg int-zero *src) memory progtree (cdr stdin) nextblock))
                        (t
                          (eval (reg-write reg c-bit *src) memory progtree (cdr stdin) nextblock)))))
                  ;; putc
                  (cons (bit2int-256 src) (eval-reg reg))
                  ;; (if (cmp src (int2bit 256) cmp-lt)
                  ;;   (cons (bit2int-256 src) (eval-reg reg))
                  ;;   (cons int-zero (eval-reg reg)))
                    )

                ;; ==== inst-sub ====
                ;; Structure:
                ;;   (cons4 inst-store [src-isimm] [src] [*dst])
                (eval-reg (reg-write reg
                        (sub (reg-read reg *dst) src)
                        *dst))

                ;; ==== inst-cmp ====
                ;; Structure:
                ;;  (cons4 inst-cmp [src-isimm] [src] (cons [emum-cmp] [dst]))
                (let ((*dst-cmp (cdr *dst))
                      (cmp-result (cmp (reg-read reg *dst-cmp) src (car *dst))))
                  (eval-reg
                    (reg-write
                      reg
                      (if cmp-result int-one int-zero)
                      *dst-cmp)))

                ;; ==== inst-load ====
                ;; Structure:
                ;;   (cons4 inst-load [src-isimm] [src] [*dst])
                (eval-reg (reg-write reg (memory-read memory (reverse-helper src nil)) *dst))

                ;; ==== inst-jumpcmp ====
                ;; Structure:
                ;;   (cons4 inst-jumpcmp [src-isimm] [src] (cons4 [enum-cmp] [*dst] [jmp-isimm] [jmp]))
                (let ((*jmp (car4-4 *dst))
                      (jmp (if (car4-3 *dst) *jmp (reg-read reg *jmp)))
                      ;; (cmp-result (cmp (reg-read reg (car4-2 *dst)) src (car4-1 *dst)))
                      )
                  (cond ((cmp (reg-read reg (car4-2 *dst)) src (car4-1 *dst))
                          (eval reg memory progtree stdin (flatten nil (lookup-progtree progtree (reverse-helper jmp nil)))))
                        (t
                          (eval-reg reg))))

                ;; ==== inst-jmp ====
                ;; Structure:
                ;;   (cons4 inst-jmp [jmp-isimm] [jmp] _)
                (eval reg memory progtree stdin (flatten nil (lookup-progtree progtree (reverse-helper src nil))))

                ;; ==== inst-mov ====
                ;; Structure:
                ;;   (cons4 inst-mov [src-isimm] [src] [*dst])
                (eval-reg (reg-write reg src *dst))

                ;; ==== inst-store ====
                ;; Structure:
                ;;   (cons4 inst-store [src-isimm] [src] [*dst])
                ;; src:  The destination address
                ;; *dst: The source register
                (eval reg (memory-write memory (reverse-helper src nil) (reg-read reg *dst)) progtree stdin nextblock)

                ;; ==== inst-add ====
                ;; Structure:
                ;;   (cons4 inst-store [src-isimm] [src] [*dst])
                (eval-reg (reg-write reg
                        (add src (reg-read reg *dst))
                        *dst)
                      )
                      ))))))



(defun-lazy main (stdin)
  (do-continuation
    (let* memory init-memory)
    (let* reg init-reg)
    (let* progtree nil)
    (let* pc int-zero)

    (let* address (list t t nil))

    (let* n (reg-read reg reg-B))

    (let* m (int2bit 4))
    (let* a (int2bit 2))
    (let* zx (add (add n m) a))
    (let* zy (add (add (add n m) m) a))
    (let* zz (sub (add (add n m) m) a))

    ;; (let* progtree (memory-write progtree (int2bit 0) p1))
    ;; (let* progtree (memory-write progtree (int2bit 1) p2))

    (let* curblock
      (list
        ;; (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-B io-int-getc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-A io-int-putc)
        ;; (cons4 inst-io-int nil reg-B io-int-putc)
        ;; (cons4 inst-add t (int2bit 2) reg-A)
        ;; ;; (cons4 inst-add t (int2bit 2) reg-A)
        ;; ;; (cons4 inst-sub t (int2bit 2) reg-A)
        ;; (cons4 inst-store t (int2bit (+ 32 4)) reg-A)
        ;; (cons4 inst-load t (int2bit (+ 32 4)) reg-B)
        ;; (cons4 inst-io-int nil reg-B io-int-putc)

        ;; (cons4 inst-add t (int2bit 2) reg-A)
        ;; (cons4 inst-store t (int2bit (+ 2 (+ 32 4))) reg-A)
        ;; (cons4 inst-load t (int2bit (+ 2 (+ 32 4))) reg-B)
        ;; (cons4 inst-io-int nil reg-B io-int-putc)

        ;; (cons4 inst-io-int nil reg-B io-int-putc)
        ;; (cons4 inst-mov t (int2bit 2) reg-C)
        ;; (cons4 inst-sub nil reg-B reg-C)
        ;; (cons4 inst-io-int nil reg-C io-int-putc)


        (cons4 inst-mov t (int2bit (+ 32 8)) reg-D)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-eq reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-lt reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-gt reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-le reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-ge reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)

        (cons4 inst-mov t (int2bit (+ 32 16)) reg-C)
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-mov nil reg-D reg-B)
        (cons4 inst-cmp nil reg-A (cons cmp-ne reg-B))
        (cons4 inst-add nil reg-B reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)


        ;; (cons4 inst-mov t (int2bit (+ 2 (+ 32 4))) reg-B)
        )
        ;; nil
        )
    (let* p0
      (list
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-B)))
    (let* p1
      (list
        (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)))
    (let* p2
      (list
        (cons4 inst-mov t (int2bit (+ 32 8)) reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)))
    (let* p3
      (list
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-sub t int-one reg-B)
        (cons4 inst-mov t (int2bit (+ 32 16)) reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-jumpcmp t int-one (cons4 cmp-le reg-B t (int2bit 8)))
        (cons4 inst-jmp t (add int-one int-one) nil)))
    (let* proglist (list p0 p1 p2 p3))
    (let* progtree (car (list2checkpoint-tree proglist int-zero)))
    ;; (let* progtree nil)
    ;; (let* progtree (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons nil (cons (list p1 p2 p2 p2) (list p1 p1 p2))))))))))
    (let* initinst (list (cons4 inst-jmp t int-zero nil)))

    (cons "A")
    (eval reg memory progtree stdin initinst)
    ))

(defun-lazy main* (memlist proglist stdin)
  (eval
    init-reg
    (car (list2tree memlist int-zero))
    (car (list2checkpoint-tree proglist int-zero))
    stdin
    ;; (list
    ;;     (cons4 inst-mov t (int2bit (+ 32 4)) reg-A)
    ;;     (cons4 inst-io-int nil reg-A io-int-putc)
    ;;     (cons4 inst-jmp t int-zero nil))
    
    (list
     (cons4 inst-jmp t int-zero nil))
        ))

(defun-lazy main*** (stdin)
  (main**
    (list
      (int2bit (+ 32 4))
      (int2bit (+ 32 8))
      )
    (list
      
      (list
        (cons4 inst-load t int-zero reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-load t int-one reg-B)
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-jmp t int-zero nil)
        (cons4 inst-mov t (int2bit (+ 32 8)) reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-load t int-zero reg-C)
        (cons4 inst-io-int nil reg-C io-int-putc)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-mov t int-minusone reg-A)
        (cons4 inst-add nil reg-A reg-A)
        (cons4 inst-jmp t int-one nil)
        )
      (list
        (cons4 inst-io-int nil reg-B io-int-getc)
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-io-int nil reg-B io-int-getc)
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-io-int nil reg-B io-int-getc)
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-io-int nil reg-B io-int-getc)
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-mov t (int2bit (+ 32 8)) reg-A)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-io-int nil reg-A io-int-putc)
        (cons4 inst-load t int-zero reg-B)
                ;;   (cons4 inst-load [src-isimm] [src] [*dst])
        (cons4 inst-io-int nil reg-B io-int-putc)
        (cons4 inst-io-int nil reg-A io-int-putc)
        )
        )
      stdin
        ))