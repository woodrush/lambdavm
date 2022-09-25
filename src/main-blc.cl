(load "./src/lambdavm.cl")


(format t (compile-to-blc-lazy lambdaVM))
