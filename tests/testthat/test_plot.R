context("Plot")

test_that("test plots", {

  # run vdiffr::validate_cases() at commandline to generate test svg

  # debug using: print(gdtools::version_freetype())
  # waiting for new release before test, now:

  # skip("CRAN release versions skip all vdiffr test code.")

  skip_on_cran()
  skip_on_bioc()
  skip_if_not_installed("vdiffr")

  bandit             <-
    ContextualPrecachingBandit$new(weights = c(0.9, 0.1, 0.1))
  agents             <- list(
    Agent$new(RandomPolicy$new(), bandit),
    Agent$new(OraclePolicy$new(), bandit),
    Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
    Agent$new(Exp3Policy$new(0.1), bandit),
    Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
    Agent$new(UCB1Policy$new(), bandit)
  )

  history            <-
    Simulator$new(
      agents,
      horizon = 20,
      simulations = 20,
      do_parallel = FALSE
    )$run()

  expect_error(
    plot(
      history,
      type = "cumulative",
      plot_only_disp = TRUE,
      no_par = TRUE
    ),
    "Need to set disp to"
  )

  vdiffr::expect_doppelganger(
    "Basic cumulative plot",
    plot(
      history,
      type = "cumulative",
      use_colors = FALSE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Cumulative traces plot",
    plot(
      history,
      type = "cumulative",
      regret = FALSE,
      legend = FALSE,
      limit_agents = c("UCB1"),
      traces = TRUE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Cumulative sd plot",
    plot(
      history,
      type = "cumulative",
      regret = FALSE,
      rate = TRUE,
      disp = "sd",
      limit_agents = c("Exp3", "ThompsonSampling"),
      legend_position = "bottomright",
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Only sd plot",
    plot(
      history,
      type = "cumulative",
      rate = TRUE,
      plot_only_disp = TRUE,
      disp = "var",
      smooth = TRUE,
      limit_agents = c("UCB1", "GittinsBrezziLai"),
      legend_position = "topright",
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Plot inc var, no color",
    plot(
      history,
      type = "cumulative",
      rate = TRUE,
      plot_only_disp = FALSE,
      disp = "var",
      smooth = TRUE,
      limit_agents = c("UCB1", "GittinsBrezziLai"),
      use_colors = FALSE,
      legend_position = "topright",
      no_par = TRUE
    )
  )


  vdiffr::expect_doppelganger(
    "Average reward plot",
    plot(
      history,
      type = "average",
      disp = "ci",
      regret = FALSE,
      interval = 10,
      smooth = TRUE,
      legend_position = "bottomright",
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Average regret plot",
    plot(
      history,
      type = "average",
      disp = "ci",
      regret = TRUE,
      interval = 10,
      smooth = TRUE,
      legend_position = "bottomright",
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger("Arm plot",
                              plot(
                                history,
                                type = "arms",
                                limit_agents = c("UCB1"),
                                interval = 10,
                                no_par = TRUE
                              ))

  vdiffr::expect_doppelganger(
    "Limits plot",
    plot(
      history,
      type = "cumulative",
      xlim = c(1, 10),
      ylim(c(0, 20)),
      legend_border = FALSE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Traces plot smooth",
    plot(
      history,
      type = "cumulative",
      regret = FALSE,
      legend = FALSE,
      limit_agents = c("UCB1"),
      traces = TRUE,
      smooth = TRUE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Traces alpha and max plot",
    plot(
      history,
      traces_alpha = 0.2,
      traces_max = 2,
      traces = TRUE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Lwd pot",
    plot(
      history,
      traces_alpha = 0.2,
      traces_max = 2,
      lwd = 1,
      traces = TRUE,
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger("Color and lty stepping",
                              plot(
                                history,
                                color_step  = 2,
                                lty_step = 2,
                                no_par = TRUE
                              ))

  vdiffr::expect_doppelganger(
    "Legend title and labels plot",
    plot(
      history,
      legend_labels = c(1:5),
      legend_title = "Policies",
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger("Ylim plot",
                              plot(history, ylim = c(0.3, 3.5), no_par = TRUE))

  vdiffr::expect_doppelganger(
    "Arms lims",
    plot(
      history,
      type = "arms",
      ylim = c(10, 80),
      xlim = c(2, 9),
      limit_agents = c("UCB1"),
      no_par = TRUE
    )
  )

  vdiffr::expect_doppelganger(
    "Arms color",
    plot(
      history,
      type = "arms",
      use_colors = FALSE,
      limit_agents = c("UCB1"),
      no_par = TRUE
    )
  )

  ############################

  q <-
    plot(
      history,
      ylim = c(0.3, 3.5),
      xlim = c(1, 10),
      no_par = FALSE
    )
  r <- plot(
    history,
    ylim = c(0.3, 3.5),
    xlim = c(1, 10),
    no_par = TRUE
  )

  expect_warning(plot(history, type = "arms", interval = 10),
                 "results of one agent")

  par1 <- plot(
    history,
    type = "arms",
    limit_agents = c("UCB1"),
    interval = 10
  )

  par2 <-
    plot(
      history,
      type = "cumulative",
      xlim = c(1, 10),
      ylim(c(0, 20)),
      legend_border = FALSE
    )

  dev.off()

  expect_equal(class(par1), "recordedplot")
  expect_equal(class(par2), "recordedplot")

})
