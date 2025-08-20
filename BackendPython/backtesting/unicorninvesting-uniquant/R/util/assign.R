assign.if.na <- function (a, b) {
  if ( is.na(a) ) { b <- a }

  return(a)
}
