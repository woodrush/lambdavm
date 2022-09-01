(load "./lazy.cl")

(defparameter s (mapcar (lambda (x) (if (equal x #\0) 0 1)) (coerce (read-line) 'list)))

(defun lex-var (s)
  (let ((c (car s)))
    (cond
      ((not s)
        (error "Lexing error: Unexpected EOF"))
      ((= c 0)
        (cons 0 (cdr s)))
      (t
        (let ((ret (lex-var (cdr s))))
          (cons (+ 1 (car ret)) (cdr ret)))))))

(defun lex-blc (s)
  (let ((c (car s)) (ret nil))
    (cond
      ((not s)
        nil)
      ((= c 0)
        (let ((c2 (car (cdr s))))
          (cond
            ((= c2 0)
              (setq ret 'ABS))
            (t
              (setq ret 'APP))))
        (cons ret (lex-blc (cdr (cdr s)))))
      (t
        (let ((ret (lex-var s)))
          (cons (car ret) (lex-blc (cdr ret))))))))


(defun parse-de-bruijn (l)
  (let ((stack nil)
        (envdepth -1)
        (curexpr nil))
    (loop :for token :in l :do
      ;; (format t "token: ~a~%" token)
      ;; (format t "stack: ~a~%" stack)
      (cond
        ((equal 'APP token)
          (setq stack (cons 'APP stack)))
        ((equal 'ABS token)
          (setq stack (cons 'ABS stack))
          (setq envdepth (+ 1 envdepth)))
        (t
          (setq curexpr (- envdepth token -1))
          (loop
            ;; (format t "L curexpr: ~a~%" curexpr)
            ;; (format t "L stack: ~a~%" stack)
            (if (not stack)
              (return))
            (let ((stacktop (car stack)))
              (cond
                ((equal 'APP stacktop)
                  (setq stack (cons curexpr stack))
                  (return))
                ((equal 'ABS stacktop)
                  (setq curexpr `(lambda (,envdepth) ,curexpr))
                  (setq envdepth (- envdepth 1))
                  (setq stack (cdr stack)))
                ((equal 'APP (car (cdr stack)))
                  (setq curexpr `(,stacktop ,curexpr))
                  (setq stack (cdr (cdr stack)))))))
          ;; (setq stack (cons curexpr stack))
          (if (not stack)
            (return curexpr)))))))

;; (print (lex-blc s))
;; (print (parse-de-bruijn (lex-blc s)))

(format t (compile-to-blc (parse-de-bruijn (lex-blc s))))
