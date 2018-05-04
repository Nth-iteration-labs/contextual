#' @export
"inc<-" = function(x, value) {
  x + value
}
#' @export
"dec<-" = function(x, value) {
  x - value
}
#' @export
max_in = function(x, equal_is_random = TRUE) {
  x <- unlist(x)
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L)  {
    if (equal_is_random) {
      return(sample(y, 1L, replace = TRUE))
    } else {
      return(y[1])
    }
  } else {
    return(y)
  }
}
#' @export
normalize = function(M) {
  M / sqrt(sum(M^2))
}
#' @export
sum_of = function(x) {
  sum(unlist(x))
}
#' @export
inv = function(M) {
  chol2inv(chol(M))
}
