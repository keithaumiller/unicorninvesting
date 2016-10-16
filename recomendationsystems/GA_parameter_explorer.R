#GA Implementation to expore the network parameter space
library(GA)
fitnesfunction<-function(){
  return(3)
}

GA<-ga(type = "real-valued", fitness = fitnesfunction, min = -20, max = 20)



