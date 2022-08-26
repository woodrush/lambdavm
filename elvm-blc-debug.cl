(load "./elvm-blc.cl")


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
