library(foreach)
library(Rmpi)

# Instructions for installing Rmpi: http://fisher.stats.uwo.ca/faculty/yu/Rmpi/

mpi.spawn.Rslaves()
Sys.sleep(3)

mpi.setup.rngstream(iseed=123)
mpi.parReplicate(80, mean(rnorm(1000000)))

mpi.close.Rslaves()
