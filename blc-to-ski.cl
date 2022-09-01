(load "./lazy.cl")


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
      (cond
        ((equal 'APP token)
          (setq stack (cons 'APP stack)))
        ((equal 'ABS token)
          (setq stack (cons 'ABS stack))
          (setq envdepth (+ 1 envdepth)))
        (t
          (setq curexpr (- envdepth token -1))
          (loop
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
          (if (not stack)
            (return curexpr)))))))


(defun parse-argv ()
  (cond
     ;; SBCL
    ((boundp '*posix-argv*)
      (cdr (eval '*posix-argv*)))
    ;; CLISP
    ((boundp '*args*)
      (eval '*args*))
    (t
      nil)))

(defun show-help ()
  (format *error-output* "Converts binary lambda calculus terms (0010, etc.) to a target format.

Usage:
  sbcl --script blc-to-ski.cl [option]
  clisp blc-to-ski.cl [option]

Options:
  -h, -help       : Show this help message
  -oski           : blc to ski
  -ojs            : blc to js
  -ojs-arrow      : blc to js arrow style
  -olambda        : blc to simple lambda
  -olambda-unl    : blc to simple lambda (Unlambda style)
  Defualt         : blc to ski

Notes:
  - Runs on SBCL or CLISP (where command line arguments are bound to either *posix-argv* or *args*)
  - Running on CLISP prints a trailing newline. Remove it if it affects utilities such as asc2bin:
    clisp blc-to-ski.cl -oski | tr -d '\n'
"))

(let ((argv (parse-argv)))
  (cond
    ((or (not argv)
         (equal (car argv) "-h")
         (equal (car argv) "-help"))
      (show-help))
    (t
      (let* ((s (read-line))
             (s (coerce s 'list))
             (s (mapcar (lambda (x) (if (equal x #\0) 0 1)) s)))
        (cond
          ((equal (car argv) "-oski")
            (format t (compile-to-ski (parse-de-bruijn (lex-blc s)))))
          (t
            (show-help))))))
  ;; (format t (compile-to-simple-lambda (parse-de-bruijn (lex-blc s))))
  ;; (format t (compile-to-simple-lambda-unl (parse-de-bruijn (lex-blc s))))
  ;; (format t (compile-to-de-bruijn-unl (parse-de-bruijn (lex-blc s))))
  ;; (format t (compile-to-js-arrow (parse-de-bruijn (lex-blc s))))
  ;; (format t (compile-to-js (parse-de-bruijn (lex-blc s))))

  )

;; (print *posix-argv*)
;; (print *args*)
