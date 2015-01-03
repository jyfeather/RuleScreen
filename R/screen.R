rm(list=ls())
load("./depression.RData")

alpha <- 1
beta <- 1 # if beta = 0, same as formulation in the paper.

################################################################
#       Find a feasible integral solution to the primal
################################################################
M <- alpha / t * matrix(1,t,n) + beta * B
# generate w according to comparizon of # of 1 in A.Z and A.P
w <- rep(0, n)
diff <- sort(colSums(A.P) - colSums(A.Z), decreasing = TRUE)
cut <- diff[200]
w[which(colSums(A.P) - colSums(A.Z) >= cut)] <- 1
# compute err
err.P <- pmax(rep(0, nrow(A.P)), rep(1, nrow(A.P)) - A.P %*% w)
err.Z <- A.Z %*% w
# compute primal objective value
pri.sol <- c(w, err.P, err.Z)
pri.val <- sum(err.P) + sum(err.Z) + alpha * sum(w) + beta * sum(B %*% w)


################################################################
#                   optimal solution to primal
################################################################
pri.coef <- c(colSums(M) + colSums(A.Z), rep(1, nrow(A.P)))
pri.con <- cbind(A.P, diag(nrow(A.P)))
pri.dir <- rep(">=", nrow(A.P))
pri.rhs <- rep(1, nrow(A.P))
pri <- lp(direction = "min", pri.coef, pri.con, pri.dir, pri.rhs)

################################################################
#       Find a feasible solution to the dual (using optimal solution insteadly)
################################################################
library(lpSolve)
dual.coef <- rep(1,nrow(A.P))
dual.con <- rbind(t(A.P), diag(nrow(A.P)))
dual.dir <- rep("<=",n+nrow(A.P))
dual.rhs <- c(colSums(M)+colSums(A.Z), rep(1,nrow(A.P)))
dual <- lp(direction = "max", dual.coef, dual.con, dual.dir, dual.rhs)
dual.sol <- dual$solution
#dual.sol[which(dual.sol != 0)] = 1

################################################################
#       Screen over all rules
################################################################
w.chosen <- rep(0, n)
for (i in 1:n) {
  tmp.dual <- sum(M[,i]) + sum(A.Z[,i]) + sum(dual.sol[A.P[,i] == 0])
  if (tmp.dual <= pri.val) w.chosen[i] = 1
}
rm(tmp.dual)

table(w.chosen)
table(B %*% w.chosen == 0)
