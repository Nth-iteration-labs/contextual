
index_of_maximum <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    return(sample(y, 1L))
  } else {
    return(y)
  }
}
