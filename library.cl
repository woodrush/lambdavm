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

    ;; (cons (lambda (x) x) return-tree)
    ;; (lambda (f) (f f return-tree))
    ;; (new-bintree-node a b)

    reg-A
    reg-B
    reg-C
    reg-D
    reg-SP
    reg-BP

    ;; (list nil nil nil)
    ;; (list t nil nil)
    ;; (list nil t nil)
    ;; (list t t nil)
    ;; (list nil nil t)
    ;; (list t nil t)

    inst-exit
    inst-io
    inst-jmpcmp
    inst-cmp
    inst-jmp
    inst-load
    inst-store
    inst-addsub
    inst-mov
    ;; inst-nand
    ;; inst-rshift

    cmp-gt
    cmp-lt
    cmp-eq
    cmp-le
    cmp-ge
    cmp-ne

    io-getc
    io-putc
    io-exit

    ((lambda (cons-t cons-nil) A) (lambda (x f) (f t x)) (lambda (x f) (f nil x)))
    ;; (lambda (x) (x (lambda (a b c d) t) (lambda (b c d) nil) x x x x))
    ;; (lambda (x) (x (lambda (a b) t) (lambda (a) a) x nil))

    16
    8
    ;; (lambda (f x) (f (f (f (f (f (f (f (f x)))))))))
    ;; (+ 4 4)
    ;; (lambda (f x) ((lambda (f x) (f (f x))) (lambda (f x) (f (f (f x)))) f x))

    ;; (lambda (x) (x (lambda (a b) t) (lambda (b) nil) x x))

    ;; (lambda (x) (x (lambda (a b) t) i x nil))
    ;; (x (lambda (a b) t) i z nil)
    ;; SYS-STRING-TERM

    ((letrec-lazy loop (x) (loop nil)) nil)
))



;; 8                                               : 000001010000011100111010010100011010000001110011101011010
;; (LAMBDA (F X) (F (F (F (F (F (F (F (F X))))))))): 0000011100111001110011100111001110011100111010
;; (+ 4 4)                                         : 000001010100011010000001110011101011001010100011010000001110011101011010
                                                    ;;  0000010101000001110011101000000111001110011101011010


;; (LAMBDA (X) (X (LAMBDA (A B C D) T) (LAMBDA (B C D) NIL) X X X X)): 000101010101011000000000000011000000000001010101010
;; (LAMBDA (X) (X (LAMBDA (A B) T) (LAMBDA (A) A) X NIL))            : 00010101011000000000110001010000010

;; (LAMBDA (X) (X (LAMBDA (A B C D) T) (LAMBDA (B C D) NIL) X X X X)): 000101010101011000000000000011000000000001010101010
;; (LAMBDA (X) (X (LAMBDA (A B) T) (LAMBDA (B) NIL) X X))            : 00010101011000000000110000000101010
;; (LAMBDA (X) (X (LAMBDA (A B) T) I X NIL))                         : 00010101011000000000110001010000010
;; (X (LAMBDA (A B) T) I Z NIL)                                      : 01010101[X]000000001100010[Z]000010


;; item (lambda (a b) z) e a b

;; z e a b = nil
;; z = t

;; t e a b = nil

;; e b = nil

;; e = i, b = nil

;; (lambda (x) (x (lambda (a b) t) i x nil))



;; (defun print-expression (expr)
;;   (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-ski-lazy ,expr))))
;;   (terpri))

(defun print-expression (expr)
  (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-blc-lazy ,expr))))
  (terpri))

(mapcar #'print-expression formlist)
