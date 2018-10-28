#################################################################
##  BOOT ####
###(##############################################################
library(abind)
n_iter <- 2 # number of simulations
Mp <- as.data.frame(matrix(runif(25),5,5))
Mp$id <- 1:5
colnames(Mp) <-c("A","B","C","D","E","id")
M <- Mp
for (i in 1:n_iter) {
  x<-length(Mp$id)
  bootclasses <- lapply(1:n_iter, function(i)
    Mp[sample(x, x, replace=T), ])
  bootd <- as.data.frame(bootclasses[i])
  M<- abind(M,bootd,along=3)}


print(Mp)
print(M) 
#################################################################