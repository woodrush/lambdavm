(load "./src/lambdavm.cl")


(format t (compile-to-lam-lazy lambdaVM))
