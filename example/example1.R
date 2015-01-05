################################################
#         Repeat results in the paper
# Screening for learning classification rules via -
# boolean compressed sensing
#
# Dataset: ionosphere, m = 351, t = 33
################################################
rm(list=ls())

################################################
#               Input
################################################
dat <- read.csv(file = "./data/ionosphere.data", header = FALSE)
dat <- dat[,-c(1,2)]
dat.x <- dat[,-ncol(dat)]
dat.y <- dat[,ncol(dat)]
t <- ncol(dat.x)
m <- nrow(dat.x)
rm(dat)

################################################
#           A.P, A.Z, B 
################################################
source("./R/RuleGenerator.R")
ruleset <- RuleGenThr(dat.x, num.thr = 10)
n <- length(ruleset)
A <- ABuilder(ruleset, m, n, X = dat.x)
A.P <- A[which(dat.y == "b"),]
A.Z <- A[which(dat.y != "b"),]
B <- BBuilder(ruleset, t, n)

##############################################
# Find a feasible integral solution to the primal
################################################
alpha <- 10
beta <- 10 
# init w = 0 and compute err appropriately
M <- alpha / t * matrix(1,t,n) + beta * B
w <- rep(0, n)
err.P <- pmax(rep(0, nrow(A.P)), rep(1, nrow(A.P)) - A.P %*% w)
err.Z <- A.Z %*% w

pri.sol <- c(w, err.P, err.Z)
pri.val <- sum(err.P) + sum(err.Z) + alpha * sum(w) + beta * sum(B %*% w)

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
dual.sol[which(dual.sol != 0)] = 1

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
