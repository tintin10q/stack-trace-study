dangerous <- function(v1, v2){ if(v2==0L) stop("divide by zero"); v1 %/% v2 }
foo <- function(array, counter){
  if(counter==0L) return(dangerous(array[[1L]], counter))
  foo(array, counter-1L)
}
array <- as.integer(rep(0L,1000))
cat("The result is", foo(array, 6L), "\n")

