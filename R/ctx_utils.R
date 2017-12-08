#' @export
ctx_utils <- list(
  index_of_max = function(x)
  {
    y <- seq_along(x)[x == max(x)]
    if (length(y) > 1L) {
      return(sample(y, 1L))
    } else {
      return(y)
    }
  },
  "inc<-" = function(x, value) {
    x + value
  },
  "dec<-" = function(x, value) {
    x - value
  },
  "mult<-" = function(x, value) {
    x * value
  }
)

