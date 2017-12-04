#' @export
index.of.max = function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) sample(y, 1L) else y
}

#' @export
"inc<-" <- function(x, value) {
  x + value
}

#' @export
"dec<-" <- function(x, value) {
  x - value
}

#' @export
"mult<-" <- function(x, value) {
  x * value
}
