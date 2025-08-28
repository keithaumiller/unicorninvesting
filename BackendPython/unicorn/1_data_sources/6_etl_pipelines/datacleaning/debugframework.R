#debugframework
#PITA doing BS print lines everywhere
debugswitch <<-0
mydebug <- function(x){
  if (debugswitch == 1)
  {
  print(paste('DEBUG:', x,sep = ' '))
  }
}