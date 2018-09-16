context("Plot")

#vdiffr::validate_cases()
#skip_on_cran()

bandit             <- ContextualBernoulliBandit$new(weights = c(0.9, 0.1, 0.1))
agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit))

history            <- Simulator$new(agents, horizon = 20, simulations = 20,
                                    do_parallel = FALSE)$run()

a <- plot(history, type = "cumulative", use_colors = FALSE)

b <- plot(history, type = "cumulative", regret = FALSE, legend = FALSE,
     limit_agents = c("UCB1"), traces = TRUE)

c <- plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ci = "sd",
     limit_agents = c("Exp3", "ThompsonSampling"),
     legend_position = "bottomright")

d <- plot(history, type = "cumulative", rate = TRUE, plot_only_ci = TRUE,
     ci = "var", smooth = TRUE, limit_agents = c("UCB1", "GittinsBrezziLai"),
     legend_position = "topright")

e <- plot(history, type = "average", ci = "ci", regret = FALSE, interval = 10,
     smooth = TRUE, legend_position = "bottomright")

f <- plot(history, type = "average", ci = "ci", regret = TRUE, interval = 10,
          smooth = TRUE, legend_position = "bottomright")

g <- plot(history, type = "arms", limit_agents = c("UCB1"),
     interval = 10)

h <- plot(history, type = "cumulative", xlim=c(1,10), ylim(c(0,20)),
          legend_border = FALSE)

i <- plot(history, type = "cumulative", regret = FALSE, legend = FALSE,
          limit_agents = c("UCB1"), traces = TRUE, smooth = TRUE)

vdiffr::expect_doppelganger("Basic cumulative plot", a)
vdiffr::expect_doppelganger("Cumulative traces plot", b)
vdiffr::expect_doppelganger("Cumulative sd plot", c)
vdiffr::expect_doppelganger("Only sd plot", d)
vdiffr::expect_doppelganger("Average reward plot", e)
vdiffr::expect_doppelganger("Average regret plot", f)
vdiffr::expect_doppelganger("Arm plot", g)
vdiffr::expect_doppelganger("limits", h)
vdiffr::expect_doppelganger("Traces plot smooth", i)

expect_warning(plot(history, type = "arms", interval = 10),
               "results of one agent")

par1 <- plot(history, type = "arms", limit_agents = c("UCB1"),
          interval = 10, no_par = TRUE)

par2 <- plot(history, type = "cumulative", xlim=c(1,10), ylim(c(0,20)),
             legend_border = FALSE, no_par = TRUE)

dev.off()

expect_equal(class(par1),"recordedplot")
expect_equal(class(par2),"recordedplot")
