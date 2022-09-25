(load "./src/lambdavm.cl")


(defun-lazy main (a b memlist proglist stdin)
  (blcstr-to-lazykstr (lambdaVM
  a b
  memlist proglist (lazykstr-to-blcstr stdin))))

(format t (compile-to-ski-lazy main))
