(load "./src/lambdavm.cl")


(defun-lazy main (a b memlist proglist stdin)
  (blcstr-to-ulambstr (lambdaVM
  a b
  memlist proglist (ulambstr-to-blcstr stdin))))

(format t (compile-to-blc-lazy main))
