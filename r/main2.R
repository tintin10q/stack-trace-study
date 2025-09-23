dangerous <- function(array, index) {
  array[[index]]  # 'subscript out of bounds'
}

foo <- function(array, counter) {
  if (counter == 0) {
    return(dangerous(array, counter + 9137))
  } else {
    return(foo(array, counter - 1))
  }
}

arr <- as.list(integer(1000))
res <- foo(arr, 6)
cat("The result is", res, "\n")
