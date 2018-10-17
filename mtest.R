library(microbenchmark)

nrow <- 1000L
ncol <- 1000L

mat = matrix(sample(c(1,0),nrow*ncol,replace = TRUE), nrow = nrow, ncol = ncol)
vec = colSums(mat)

print(microbenchmark(
  m = matrix(vec, nrow, ncol),
  times = 300
))

m = matrix(vec, nrow, ncol)

is.vector(m)

get_arm_context <- function(X, arm) {
  if(is.vector(X)) return(X) else return(X[, arm])
}
