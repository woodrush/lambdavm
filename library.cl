(load "./elvm-blc.cl")

(defmacro-lazy cons (x y) `(lambda (f) (f ,x ,y)))

(defun-lazy return-tree (x cont)
  (if x
    (cont a)
    (cont b)))

(defparameter formlist `(
    (cons x y)
    (cons4 x1 x2 x3 x4)
    t
    nil

    (cons (lambda (x) x) return-tree)
    (lambda (f) (f f return-tree))
    (new-bintree-node a b)

    reg-A
    reg-B
    reg-C
    reg-D
    reg-SP
    reg-BP

    (list nil nil nil)
    (list t nil nil)
    (list nil t nil)
    (list t t nil)
    (list nil nil t)
    (list t nil t)

    inst-exit
    inst-io
    inst-jumpcmp
    inst-cmp
    inst-jmp
    inst-load
    inst-store
    inst-addsub
    inst-mov

    cmp-gt
    cmp-lt
    cmp-eq
    cmp-le
    cmp-ge
    cmp-ne

    io-int-putc
    io-int-getc
    io-int-exit

    string-term
))


;; (defun print-expression (expr)
;;   (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-ski-lazy ,expr))))
;;   (terpri))

(defun print-expression (expr)
  (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-blc-lazy ,expr))))
  (terpri))

(mapcar #'print-expression formlist)
