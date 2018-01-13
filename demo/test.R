grid = FALSE
xlim = NULL
legend = TRUE
regret = FALSE


#if (!is.data.table(history)) history = history$get_data_table()
if (grid == FALSE) dev.hold()
if (regret) {
  cs <- history[, list(mean = mean(reward)), by = list(t, agent)]
} else {
  cs <- history[, list(mean = mean(reward)), by = list(t, agent)]
}
agent_list <- unique(cs$agent)
agents <- length(agent_list)
setorder(cs, agent, t)
agent_list <- unique(cs$agent)
ms <- matrix(unlist(cs[, 3], FALSE, FALSE), ncol = agents)
if (regret) {
  ylab_title = "Average regret"
} else {
  ylab_title = "Average reward"
}


matplot(
  ms,
  type = "l",
  xlim = xlim,
  lwd = 1,
  lty = 1,
  xlab = "Time Step",
  ylab = ylab_title
)
if (legend)
  legend(
    "topleft",
    NULL,
    agent_list,
    col = 1:agents,
    lwd = 1,
    lty = 1,
    bg = "white"
  )
if (grid == FALSE) dev.flush()
