# here, we did setweights, can also generate based on d/k, but check if either
setwd("~/GitHub/contextual/demo")
source("dev.R")

library(tcltk)

bandit <- SyntheticBandit$new(
  weight_distribution = "Uniform",
  reward_type         = "Bernoulli",
  seed                = 1,
  precache            = TRUE
)
                            #d1  #d2  #d3
bandit$set_weights(matrix(c(0.9, 0.1, 0.1,  #k1
                            0.1, 0.2, 0.1,  #k2
                            0.2, 0.1, 0.2), #k3
                            nrow = 3L,
                            ncol = 3L))

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

ptm            <- proc.time()

simulation     <- SimulatorParallel$new(
  agents,
  horizon      = 100L,
  simulations  = 100L,
  save_context = FALSE,
  save_theta   = FALSE,
  worker_max   = 7
)

history        <- simulation$run()

print(proc.time() - ptm)

plot <- Plot$new()
plot$grid(history)

simulation$object_size()


server <- function(){
  while(TRUE){
    writeLines("Listening...")
    con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
                            server=TRUE, open="r+")
    data <- readLines(con, 1)
    print(data)
    response <- toupper(data)
    writeLines(response, con)
    close(con)
  }
}
server()

client <- function(){
  while(TRUE){
    con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
                            server=FALSE, open="r+")
    f <- file("stdin")
    open(f)
    print("Enter text to be upper-cased, q to quit")
    sendme <- readLines(f, n=1)
    if(tolower(sendme)=="q"){
      break
    }
    write_resp <- writeLines(sendme, con)
    server_resp <- readLines(con, 1)
    print(paste("Your upper cased text:  ", server_resp))
    close(con)
  }
}
client()

