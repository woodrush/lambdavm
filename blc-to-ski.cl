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
  (let ((curexpr nil))
    (loop
      (let ((ret nil))
        (cond
          ((not s)
            (return (reverse curexpr)))
          ((= (car s) 0)
            (let ((c2 (car (cdr s))))
              (cond
                ((= c2 0)
                  (setq ret 'ABS))
                (t
                  (setq ret 'APP))))
            (setq curexpr (cons ret curexpr))
            (setq s (cdr (cdr s))))
          (t
            (let ((ret (lex-var s)))
              (setq curexpr (cons (car ret) curexpr))
              (setq s (cdr ret)))))))))

(defun decorate-var (i)
  (intern (format nil "X~a" i)))

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
          (setq curexpr (decorate-var (- envdepth token -1)))
          (loop
            (if (not stack)
              (return))
            (let ((stacktop (car stack)))
              (cond
                ((equal 'APP stacktop)
                  (setq stack (cons curexpr stack))
                  (return))
                ((equal 'ABS stacktop)
                  (setq curexpr `(lambda (,(decorate-var envdepth)) ,curexpr))
                  (setq envdepth (- envdepth 1))
                  (setq stack (cdr stack)))
                ((equal 'APP (car (cdr stack)))
                  (setq curexpr `(,stacktop ,curexpr))
                  (setq stack (cdr (cdr stack)))))))
          (if (not stack)
            (return curexpr)))))))

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
  -odb-unl        : blc to De Bruijn index notation (Unlambda style)
  -olisp          : blc to Lisp S-expression (no pretty printing)
  -olisp-pp       : blc to Lisp S-expression (with pretty printing)
  Defualt         : blc to ski

Notes:
  - Runs on SBCL or CLISP (where command line arguments are bound to either *posix-argv* or *args*)
  - Running on CLISP prints a trailing newline. Remove it if it affects utilities such as asc2bin:
    clisp blc-to-ski.cl -oski | tr -d '\n'
"))

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

(defun parse-input (argv)
  (let* ((s (read-line))
         (s (coerce s 'list))
         (s (mapcar (lambda (x) (if (equal x #\0) 0 1)) s)))
   (cond
     ((equal (car argv) "-oski")
       (format t (compile-to-ski  (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-ojs")
       (format t (compile-to-js (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-ojs-arrow")
       (format t (compile-to-js-arrow (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-olambda")
       (format t (compile-to-simple-lambda (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-olambda-unl")
       (format t (compile-to-simple-lambda-unl (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-db-unl")
       (format t (compile-to-de-bruijn-unl (parse-de-bruijn (lex-blc s)))))
     ((equal (car argv) "-olisp")
       (setq *print-pretty* 'nil)
       (format t "~a" (parse-de-bruijn (lex-blc s))))
     ((equal (car argv) "-olisp-pp")
       (setf *print-right-margin* 800)
       (format t "~a" (parse-de-bruijn (lex-blc s))))
     (t
       (show-help)))))

(defun main ()
  (let ((argv (parse-argv)))
    (cond
      ((or (not argv)
          (equal (car argv) "-h")
          (equal (car argv) "-help"))
        (show-help))
      (t
        (parse-input argv)))))

(main)
