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
# init w = 0 and compute err appropriately
