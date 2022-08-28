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

    inst-exit
    inst-io
    inst-jumpcmp
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

    io-putc
    io-getc

    SYS-STRING-TERM

    reg-A
    reg-B
    reg-C
    reg-D
    reg-SP
    reg-BP
    reg-PC
    (cons t (cons t (cons t (cons t nil))))

    (let ((cons-t   (lambda (x f) (f t x)))
          (cons-nil (lambda (x f) (f nil x))))
      a)
    (f3 (f2 (f1 nil)))

))


;; (defun print-expression (expr)
;;   (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-ski-lazy ,expr))))
;;   (terpri))

(defun print-expression (expr)
  (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-blc-lazy ,expr))))
  (terpri))

(mapcar #'print-expression formlist)
