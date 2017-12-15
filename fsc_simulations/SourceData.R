# Source Data from fsc into Data Frame
# Performs n simulations of fsc on constant, exponential and bottleneck models.
# Imports data to dataframe and simplifies to eliminate zero and duplicate columns

source.data <- function(n){
  # set directory
  setwd('~/Desktop/DNA/fsc_simulations')
  # run simulations
  system(paste("./sim_script_est const",n),wait = TRUE)
  system(paste("./sim_script_est exp",n),wait = TRUE)
  system(paste("./sim_script_est btl",n),wait = TRUE)
  # import to dataframe
  const <- read.table("constSS.txt",header = TRUE)
  exp <- read.table("expSS.txt",header = TRUE)
  btl <- read.table("btlSS.txt",header = TRUE)
  class <- c(rep("const",nrow(const)),rep("exp",nrow(exp)),rep("btl",nrow(btl)))
  ss<-rbind(const,exp,btl)
  ss$class<-factor(class)
  # simplify dataframe
  ss <- ss[, colSums(ss != 0) > 0] # Remove zero columns
  library(digest)
  ss <- ss[!duplicated(lapply(ss, digest))] # remove duplicate columns
  ss <- Filter(function(x)(length(unique(x))>1), ss)
  # return dataframe
  return(ss)
}
