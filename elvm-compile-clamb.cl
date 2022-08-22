(load "./elvm.cl")


;; In Universal Lambda, strings are terminated by `nil`
(def-lazy SYS-STRING-TERM nil)

(defun-lazy main-clamb (memlist proglist stdin)
  (let ((int-zero int-zero)
        (list2tree list2tree))
    (eval
      init-reg
      (car (list2tree memlist int-zero car*))
      (car (list2tree proglist int-zero (lambda (x) x)))
      stdin
      (list
      (cons4 inst-jmp t int-zero nil)))))

;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))
;; (format t (compile-to-ski-lazy main-clamb))
(format t (compile-to-blc-lazy main-clamb))

;; ;; Print lambda term
;; (setf *print-right-margin* 500)
;; (format t (write-to-string (curry (macroexpand-lazy main-clamb))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
