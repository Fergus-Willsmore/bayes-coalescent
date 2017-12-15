library(tidyverse)
library(MASS)
setwd('~/Desktop/DNA/fsc_simulations')

#### Train Data ####
source("SourceData.R")
ss.train <- source.data(100)

pairs(subset(ss.train,select = -class))
 
#### LDA ####

fit.lda <- lda(class ~ ., ss.train, prior = rep(1/3,3))

#### Test Data ####

ss.test <- source.data(30)

pred <- predict(fit.lda, subset(ss.test,select = -class))$class
source("ConfusionMtx.R")
cnf <- confusion.matrix(ss.test$class,pred)

print(cnf)
