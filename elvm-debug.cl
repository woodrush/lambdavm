(load "./elvm.cl")

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


(defun-lazy main*** (stdin)
  (main
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

;; In Lazy K, strings are terminated by an infinite list of `256`s
(def-lazy SYS-STRING-TERM (inflist 256))

;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))
(format t (compile-to-ski-lazy main***))
;; (format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
