(load "./elvm.cl")

(defmacro-lazy cons (x y) `(lambda (f) (f ,x ,y)))

(defparameter formlist `(
    (cons x y)
    (cons4 x1 x2 x3 x4)
    (lambda (stdin) (cons 64 (cdr (cdr stdin))))
    t
    nil

    reg-A 
    reg-B 
    reg-C 
    reg-D 
    reg-SP
    reg-BP

    inst-io-int   
    inst-sub    
    inst-cmp    
    inst-load   
    inst-jumpcmp
    inst-jmp    
    inst-mov    
    inst-store  
    inst-add      

    cmp-eq
    cmp-ne
    cmp-lt
    cmp-gt
    cmp-le
    cmp-ge

    io-int-exit
    io-int-getc
    io-int-putc

    string-term
))


;; (defun print-expression (expr)
;;   (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-ski-lazy ,expr))))
;;   (terpri))

(defun print-expression (expr)
  (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-blc-lazy ,expr))))
  (terpri))

(mapcar #'print-expression formlist)
