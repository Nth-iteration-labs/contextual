library(R.devices)

index.of.max <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) sample(y, 1L) else y
}

is.rstudio = function(){
  .Platform$GUI == "RStudio"
}

"inc<-" <- function(x, value) {
  x + value
}

"dec<-" <- function(x, value) {
  x - value
}

"mult<-" <- function(x, value) {
  x * value
}
