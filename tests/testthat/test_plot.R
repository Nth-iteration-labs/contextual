context("Plot")

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

svg(width=7,height=7)

a <- plot(history, type = "cumulative", use_colors = FALSE, no_par = TRUE)

b <- plot(history, type = "cumulative", regret = FALSE, legend = FALSE,
     limit_agents = c("UCB1"), traces = TRUE, no_par = TRUE)

c <- plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ci = "sd",
     limit_agents = c("Exp3", "ThompsonSampling"),
     legend_position = "bottomright", no_par = TRUE)

d <- plot(history, type = "cumulative", rate = TRUE, plot_only_ci = TRUE,
     ci = "var", smooth = TRUE, limit_agents = c("UCB1", "GittinsBrezziLai"),
     legend_position = "topright", no_par = TRUE)

e <- plot(history, type = "average", ci = "ci", regret = FALSE, interval = 10,
     smooth = TRUE, legend_position = "bottomright", no_par = TRUE)

f <- plot(history, limit_agents = c("ThompsonSampling"), type = "arms",
     interval = 10, no_par = TRUE)

vdiffr::expect_doppelganger("Basic cumulative plot", a)
vdiffr::expect_doppelganger("Cumulative traces plot", b)
vdiffr::expect_doppelganger("Cumulative sd plot", c)
vdiffr::expect_doppelganger("Only sd plot", d)
vdiffr::expect_doppelganger("Average plot", e)
vdiffr::expect_doppelganger("Arm plot", f)

#vdiffr::validate_cases()
