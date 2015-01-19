rm(list=ls())
source("RuleScreen.Rproj")

# input
dat.x <- read.csv("C:/Users/jyfea_000/Dropbox/Research/RuleBased/data/Depression/x.csv", header = FALSE)
dat.y <- read.csv("C:/Users/jyfea_000/Dropbox/Research/RuleBased/data/Depression/y.csv", header = FALSE)

no.del <- c()
for (i in 1:ncol(dat.x)) {
  tmp.var <- dat.x[,i]
  tmp.na <- as.data.frame(table(is.na(tmp.var)))$Freq
  if (length(tmp.na) == 1) next;
  if (50 < tmp.na[2]) {no.del <- c(no.del, i); next;}
}

dat.x <- dat.x[,-no.del]
dat.y <- dat.y[complete.cases(dat.x),]
dat.x <- dat.x[complete.cases(dat.x),]
dat.y <- as.factor(dat.y)
rm(tmp.na, tmp.var)

# rule generation via RuleFit 
if (FALSE) {
  platform = "windows"
  rfhome = "C:/Users/jyfea_000/Dropbox/Research/RuleBased/code/RuleFit"
  source("C:/Users/jyfea_000/Dropbox/Research/RuleBased/code/RuleFit/rulefit.r")
  library(akima, lib.loc = rfhome)
  rfmod <- rulefit(dat.x, as.vector(as.matrix(dat.y)), rfmode = "class")  
}

# rule generation via randomForest
if (TRUE) {
  library(randomForest)
  library(inTrees)
  rf <- randomForest(dat.x, dat.y)  
  # rf <- rfImpute(dat.x, dat.y, nodesize = 5) # deal with missing data
}

# matrix B generation via inTrees
# matrix A generation
treeList <- RF2List(rf)
exec <- extractRules(treeList, dat.x, maxdepth = 5, ntree = treeList$ntree)  
ruleMetric <- getRuleMetric(exec, dat.x, dat.y)
t <- ncol(dat.x)
m <- nrow(dat.x)
n <- nrow(ruleMetric) 
B <- Bbuild(exec)
A <- Abuild(exec)
A.P <- A[which(dat.y==1),]
A.Z <- A[which(dat.y==-1),]

# save image
unlist("./depression.RData")
save(A.P, A.Z, B, t, m, n, file = "depression.RData")
write.table(A.P, "./matlab/AP.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(A.Z, "./matlab/AZ.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(B, "./matlab/B.csv", sep = ",", row.names = FALSE, col.names = FALSE)