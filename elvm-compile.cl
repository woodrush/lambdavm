(load "./elvm.cl")


;; In Lazy K, strings are terminated by an infinite list of `256`s
(def-lazy SYS-STRING-TERM (inflist 256))

;;================================================================
;; Code output
;;================================================================
;; (format t (compile-to-ski-lazy main))
(format t (compile-to-ski-lazy main*))
;; (format t (compile-to-blc-lazy main))

;; ;; Print lambda term
;; (setf *print-right-margin* 800)
;; (format t (write-to-string (curry (macroexpand-lazy main))))

;; ;; Print in curried De Bruijn notation
;; (format t (write-to-string (to-de-bruijn (curry (macroexpand-lazy main)))))
