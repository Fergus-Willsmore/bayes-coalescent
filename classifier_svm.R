library(e1071)
library(rpart)
library(pROC)
data(Glass, package="mlbench")
setwd('~/Desktop/DNA/fsc_simulations')

#### Data ####
source("SourceData.R")
ss.train <- source.data(100)
ss.test <- source.data(30)

svm.model <- svm(class ~ ., data = ss.train, cost = 1, gamma = 3)
svm.pred <- predict(svm.model, subset(ss.test,select=-class),decision.values = TRUE)
svm.prob <- attr(svm.pred,"decision.values")

cnf <- table(pred = svm.pred, true = ss.test$class)
cnf <- cbind(cnf,total=rowSums(cnf))
cnf

