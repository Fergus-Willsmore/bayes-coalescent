library(tidyverse)
library(fBasics)
library(VGAM)
setwd('~/Desktop/DNA/fsc_simulations')

#### Train Data ####
source("SourceData.R")
ss.train <- source.data(100)

#### Pairwise visualisation ####

pairs(subset(ss.train,select = -class))
# possibly correlated statistics
# gr_H_1 and gr_Hsd_1
# gr_tot_h and tot_h
# gr_S_1 and gr_Pi_1

#### Model Selection ####

library(MASS)
fit.MLR <- vglm( class~., family=multinomial, ss.train)
# backward selection
p <- 1
tol <- 0.05
while(p>tol){
  val <- coef(summary(fit.MLR))[,4] # extracted p values
  p <- max(val)                                # max p value
  ind <- which.max(val) # row index
  var <- rownames(coef(summary(fit.MLR)))[ind] # variable name
  fit.MLR <- update(fit.MLR,.~.-var) # update fit 
}
summary(fit.MLR)

ss.train <- subset(ss.train, select=-gr_Hsd_1)
fit.MLR <- vglm( class~., family=multinomial, subset(ss.train,select = -class))


#### Test data ####

ss.test <- source.data(30)
ss.test <- subset(ss.test,select=colnames(ss.train))

#### predict ####

probabilities.MLR <- predict(fit.MLR, subset(ss.test,select=-class), type="response")
pred <- apply(probabilities.MLR, 1, which.max)
source("ConfusionMtx.R")
cnf <- confusion.matrix(ss.test$class, pred)






#### PCA visualisation ####

library(ggfortify)
ss.pca<-prcomp(ss.fit)
x<-ss.pca$x
newclass<-c(rep("const",nrow(ss.fit)/3),rep("exp",nrow(ss.fit)/3),rep("btl",nrow(ss.fit)/3))
pc <- data.frame(x,newclass)
head(pc)
ggplot(pc,aes(x=PC1,y=PC2,color=factor(newclass)))+geom_point()

#### Perform test on PC's ####

test <- vglm(newclass ~ PC1+PC2+PC3, family=multinomial, data = pc)
summary(test)
project<-scale(nd[,1:3], ss.pca$center, ss.pca$scale) %*% ss.pca$rotation 
prob.mlr<-predict(test, as.data.frame(project[,1:3]), type="response")
pred <- apply(probabilities.MLR, 1, which.max)
nd$pred <- pred
nd$class <- factor(nd$class)
cnf <- confusion.matrix(nd$class, pred)
