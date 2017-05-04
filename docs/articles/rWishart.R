## ---- include = FALSE----------------------------------------------------
library(rWishart)

## ----rWish---------------------------------------------------------------
S <- rWishart(n = 1, df = 2, Sigma = diag(1, 10), 
              covariance = TRUE, simplify = FALSE)[[1]]

## ----degfree-------------------------------------------------------------
degreesfreedom <- attributes(S)$df

## ----evaldf--------------------------------------------------------------
lazyeval::f_eval(degreesfreedom)

