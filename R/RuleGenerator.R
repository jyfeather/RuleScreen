################################
# rule generation via threshold
# Input:
#   X:            complete observation-feature matrix
#   num.thr:      # of thresholding values   
# Output:         Vector of rules
################################
RuleGenThr <- function(X, num.thr = 10) {
  # rules generated through quantile based threshold
  t <- ncol(X)
  ruleset <- c()
  for (i in 1:t) {
    quans <- quantile(X[,i], probs = seq(0, 1, length.out = num.thr))
    for (j in 1:(num.thr-1)) {
      # both directions of boolean condition
      ruleset <- c(ruleset, paste("X[,",i,"]>=", quans[j], sep = ""),
                   paste("X[,",i,"]<=", quans[j], sep = ""))
    }
  }
  return(ruleset)
}

################################
# rule generation via RuleFit
# Input:
#   X:            complete observation-feature matrix
# Output:         Vector of rules
################################
RuleGenRulefit <- function(X) {
  # not finished due to rulefit closed
  stop("Function Not Finished")
  
  platform = "windows"
  rfhome = "C:/Users/jyfea_000/Dropbox/Research/RuleBased/code/RuleFit"
  source("C:/Users/jyfea_000/Dropbox/Research/RuleBased/code/RuleFit/rulefit.r")
  library(akima, lib.loc = rfhome)
  rfmod <- rulefit(dat.x, as.vector(as.matrix(dat.y)), rfmode = "class")  
}

################################
# rule generation via randomForest
# Input:
#   X:            complete observation-feature matrix
#   Y:            observed response vector  
# Output:         Vector of rules
################################
RuleGenRandomforest <- function(X, Y) {
  require(randomForest)  
  require(inTrees)
  rf <- randomForest(X, Y, ntree = 300)
  treeList <- RF2List(rf)
  exec <- extractRules(treeList, X, maxdepth = 3, ntree = treeList$ntree)  
  #ruleMetric <- getRuleMetric(exec, X, Y)
  return(exec)
}

################################
# Matrix A Builder
# Input:
#   rules:        vector of rules
#   m    :        # of observations
#   n    :        # of rules
#   X:            complete observation-feature matrix
# Output:         matrix A
################################
ABuilder <- function(rules, m, n, X) {
  A <- matrix(0, m, n)
  for (i in 1:n) {
    rule.eval <- eval(parse(text = rules[i]))
    A[rule.eval, i] = 1  
  }
  return(A)
}

################################
# Matrix B Builder
# Input:
#   rules:        vector of rules
#   t    :        # of features
#   n    :        # of rules
# Output:         matrix B
################################
BBuilder <- function(rules, t, n) {
  B <- matrix(0, t, n)
  pattern1 <- "\\[,\\d*\\]"
  rules1 <- regmatches(ruleset, gregexpr(pattern1, ruleset))
  for (i in 1:length(rules1)) {
    pattern2 <- "\\d+"
    rules2 <- as.integer(unlist(regmatches(rules1[[i]], gregexpr(pattern2, rules1[[i]]))))
    B[rules2,i] <- 1
  }
  return(B)
}