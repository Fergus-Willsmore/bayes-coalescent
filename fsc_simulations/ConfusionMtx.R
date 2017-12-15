# Confusion Matrix
# Computes the confusion matrix between known class and predicted class from a classifier.

confusion.matrix<-function(class,pred){
  lvls <- length(levels(class))
  
  for(i in 1:lvls){
  pred[which(pred==paste(i))] <- levels(class)[i]
  }
  
  confusion <- table(class, pred) 
  confusion <- rbind(confusion,total=colSums(confusion))
  return(confusion)
}