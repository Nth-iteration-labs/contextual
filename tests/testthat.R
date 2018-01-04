Sys.setenv("R_TESTS" = "")

library(testthat)
library(contextual)

test_check("contextual")
