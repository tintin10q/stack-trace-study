dangerous <- function(array, index) array[index + 3]  # R is 1-based
foo  <- function(array, index) dangerous(array, index)
foo1 <- function(array, index) foo(array, index * 3)
foo2 <- function(array, index) foo1(array, index + 137)
foo3 <- function(array, index) foo2(array, index - 1)
foo4 <- function(array, index) foo3(array, index * 137)
foo5 <- function(array, index) foo4(array, index + 20)
foo6 <- function(array, index) foo5(array, as.integer(index / 3))
array <- rep(0L, 1000)
cat(foo6(array, 50L), "\n")

