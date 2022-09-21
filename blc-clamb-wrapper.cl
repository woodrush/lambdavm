;; (load "./elvm.cl")

(def-lazy powerlist
  (cons 128 (cons 64 (cons 32 (cons 16 (cons 8 (cons 4 (cons 2 (cons 1 nil)))))))))

(defrec-lazy int2bitlist (n powerlist cont)
  (if (isnil powerlist)
    (cont nil)
    (do
      (<- (car-pow cdr-pow) (powerlist))
      (if-then-return (<= car-pow n)
        (do
          (<- (nextlist) (int2bitlist (- n car-pow) cdr-pow))
          (cont (cons nil nextlist))))
      (<- (nextlist) (int2bitlist n cdr-pow))
      (cont (cons t nextlist)))))

(defrec-lazy clambstr-to-blcstr (s)
  (cond
    ((isnil s)
      nil)
    (t
      (do
        (<- (c-clamb s-cdr) (s))
        (<- (c-blc) (int2bitlist c-clamb powerlist))
        (cons c-blc (clambstr-to-blcstr s-cdr))))))

(defrec-lazy bitlist2int (n powerlist cont)
  (if (isnil powerlist)
    (cont 0)
    (do
      (<- (car-pow cdr-pow) (powerlist))
      (<- (car-n cdr-n) (n))
      (<- (n-ret) (bitlist2int cdr-n cdr-pow))
      (if car-n
        (cont n-ret)
        (cont (+ car-pow n-ret))))))

(defun-lazy blcchar-to-clambchar (c cont)
  (cond
    ((isnil c)
      (cont nil))
    (t
      (bitlist2int c powerlist cont))))

(defrec-lazy blcstr-to-clambstr (s)
  (cond
    ((isnil s)
      nil)
    (t
      (do
        (<- (c-blc s-cdr) (s))
        (<- (c-clamb) (blcchar-to-clambchar c-blc))
        (cons c-clamb (blcstr-to-clambstr s-cdr))))))


;; Lazy K
(defrec-lazy blcstr-to-lazykstr (s)
  (cond
    ((isnil s)
      (inflist 256))
    (t
      (do
        (<- (c-blc s-cdr) (s))
        (<- (c-clamb) (blcchar-to-clambchar c-blc))
        (cons c-clamb (blcstr-to-lazykstr s-cdr))))))

(defrec-lazy lazykstr-to-blcstr (s)
  (do
    (<- (c-clamb s-cdr) (s))
    (if-then-return (= 256 c-clamb)
      nil)
    (<- (c-blc) (int2bitlist c-clamb powerlist))
    (cons c-blc (lazykstr-to-blcstr s-cdr))))


(defun-lazy clamb-to-blc-wrapper (program io-bitlength supp-bitlength memlist proglist stdin)
  (blcstr-to-clambstr (program io-bitlength supp-bitlength memlist proglist (clambstr-to-blcstr stdin))))

(defun-lazy lazyk-to-blc-wrapper (program io-bitlength supp-bitlength memlist proglist stdin)
  (blcstr-to-lazykstr (program io-bitlength supp-bitlength memlist proglist (lazykstr-to-blcstr stdin))))


;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))
;; (format t (compile-to-ski-lazy main-clamb))

;; (format t (compile-to-ski-lazy lazyk-to-blc-wrapper))
;; (format t (compile-to-blc-lazy clamb-to-blc-wrapper))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
