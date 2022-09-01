(load "./lazy.cl")


(defun show-help ()
  (format *error-output* "Converts binary lambda calculus terms (0010, etc.) to a target format.

Usage:
  sbcl --script blc-to-ski.cl [input option] [output option]
  clisp blc-to-ski.cl [input option] [output option]

Input options:
  -iblc           : From binary lambda calculus notation
  -iski           : From SKI combinator calculus notation (Unlambda style)

Output options:
  -oblc           : To binary lambda calculus notation
  -oski           : To ski (Unlambda style)
  -ojs            : To js
  -ojs-arrow      : To js arrow style
  -opython        : To Python lambda
  -olambda        : To simple lambda
  -olambda-unl    : To simple lambda (Unlambda style)
  -odb-unl        : To De Bruijn index notation (Unlambda style)
  -olisp          : To Lisp S-expression (no pretty printing)
  -olisp-pp       : To Lisp S-expression (with pretty printing)

Other options:
  -h, -help       : Show this help message

Notes:
  - Runs on SBCL or CLISP (where command line arguments are bound to either *posix-argv* or *args*)
  - Running on CLISP prints a trailing newline. Remove it if it affects utilities such as asc2bin:
    clisp blc-to-ski.cl -oski | tr -d '\n'
"))

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
  (let ((curexpr nil) (ret nil))
    (loop
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
          (let ((ret (lex-var (cdr s))))
            (setq curexpr (cons (car ret) curexpr))
            (setq s (cdr ret))))))))

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
          (setq curexpr (decorate-var (- envdepth token)))
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

(defun ski-to-blc (s)
  (setq s (coerce s 'list))
  (setq s (subst "01" (car (coerce "`" 'list)) s))
  (setq s (subst "00000001011110100111010" #\s s))
  (setq s (subst "0000110" #\k s))
  (setq s (subst "0010" #\i s))
  (setq s (apply #'concatenate (cons 'string s)))
  s)

(defun parse-input (argv)
  (cond
    ((equal (car argv) "-iblc")
      (let* ((s (read-line))
             (s (coerce s 'list))
             (s (mapcar (lambda (x) (if (equal x #\0) 0 1)) s))
             (parsed (parse-de-bruijn (lex-blc s))))
        parsed))
    ((equal (car argv) "-iski")
      (let* ((s (read-line))
             (s (ski-to-blc s))
             (s (coerce s 'list))
             (s (mapcar (lambda (x) (if (equal x #\0) 0 1)) s))
             (parsed (parse-de-bruijn (lex-blc s))))
        parsed))
    (t
      (show-help))))

(defun compile-parsed (argv parsed)
  (cond
    ((equal (car argv) "-oblc")
      (format t (compile-to-blc parsed)))
    ((equal (car argv) "-oski")
      (format t (compile-to-ski parsed)))
    ((equal (car argv) "-ojs")
      (format t (compile-to-js parsed)))
    ((equal (car argv) "-ojs-arrow")
      (format t (compile-to-js-arrow parsed)))
    ((equal (car argv) "-opython")
      (format t (compile-to-python parsed)))
    ((equal (car argv) "-olambda")
      (format t (compile-to-simple-lambda parsed)))
    ((equal (car argv) "-olambda-unl")
      (format t (compile-to-simple-lambda-unl parsed)))
    ((equal (car argv) "-db-unl")
      (format t (compile-to-de-bruijn-unl parsed)))
    ((equal (car argv) "-olisp")
      (setq *print-pretty* 'nil)
      (format t "~a" parsed))
    ((equal (car argv) "-olisp-pp")
      (setf *print-right-margin* 800)
      (format t "~a" parsed))
    (t
      (show-help))))

(defun main ()
  (let ((argv (parse-argv)))
    (cond
      ((or (not argv)
           (not (= 2 (length argv)))
           (equal (car argv) "-h")
           (equal (car argv) "-help"))
        (show-help))
      (t
        (compile-parsed (cdr argv) (parse-input argv))))))

(main)
