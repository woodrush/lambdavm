(defparameter compile-ski t)

(load "./src/lambdavm.cl")
(load "./src/blc-clamb-wrapper.cl")


(defun-lazy main (io-bitlength supp-bitlength memlist proglist stdin)
  (blcstr-to-lazykstr (lambdaVM io-bitlength supp-bitlength memlist proglist (lazykstr-to-blcstr stdin))))

(format t (compile-to-lam-lazy main))
