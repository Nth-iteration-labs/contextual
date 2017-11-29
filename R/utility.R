library(R.devices)
#library(dplyr)

# make all dot

index_of_max <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) sample(y, 1L) else y
}

moving_average <- function(arr, n=15){
  res = arr
  for (i in n:length(arr)) {
    res[i] = mean(arr[(i - n):i])
  }
  res
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

"div<-" <- function(x, value) {
  x / value
}

