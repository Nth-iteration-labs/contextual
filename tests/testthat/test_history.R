context("history")

bandit             <- ContextualPrecachingBandit$new(weights = c(0.9, 0.1, 0.1))

agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit))

history            <- Simulator$new(agents, horizon = 3, simulations = 3,
                                    do_parallel = FALSE, save_context = TRUE)$run()


test_that("History summary and print", {

  expect_equal_to_reference(capture.output(summary(history)), file = "summary_history.rds")
  expect_equal_to_reference(capture.output(print(history)), file = "summary_history.rds")

})

test_that("History save_csv without filename", {

  csv_comparison_file <- read.csv("history_test.ref")
  history$save_csv()
  import_context <- read.csv(dir(pattern='contextual_data_*')[1])
  expect_equal(csv_comparison_file, import_context)

})

test_that("History save_csv without filename", {

  csv_comparison_file <- read.csv("history_test.ref")
  history$save_csv("history_test.csv")
  import_file <- read.csv("history_test.csv")
  expect_equal(csv_comparison_file,  import_file)

})

test_that("History save_csv with context", {

  csv_comparison_file <- read.csv("history_context_test.ref")
  history$save_csv("history_context_test.csv")
  import_file <- read.csv("history_context_test.csv")
  expect_equal(csv_comparison_file,  import_file)

})

test_that("History get and set data.table", {

  df1 <- history$get_data_table()
  history$set_data_table(df1)
  df2 <- history$get_data_table()
  expect_equal(df1, df2)

})

test_that("History get and set data.frame", {

  df1 <- history$get_data_frame()
  history$set_data_frame(df1)
  df2 <- history$get_data_frame()
  expect_equal(df1, df2)

})


test_that("History inc theta", {
  history_theta      <- Simulator$new(agents, horizon = 3, simulations = 3,
                                      do_parallel = FALSE, save_context = TRUE,
                                      save_theta = TRUE)$run()

  expect_equal(history_theta$cumulative$GittinsBrezziLai$cum_regret,
                     history$cumulative$GittinsBrezziLai$cum_regret)
})

test_that("History save_csv inc theta removal without filename", {

  csv_comparison_file <- read.csv("history_theta_test.ref")
  history$save_csv("history_theta_test.csv")
  import_file <- read.csv("history_theta_test.csv")
  expect_equal(csv_comparison_file,  import_file)

})

test_that("History save_csv nc theta removal theta with context", {

  csv_comparison_file <- read.csv("history_context_theta_test.ref")
  history$save_csv("history_context_theta_test.csv")
  import_file <- read.csv("history_context_theta_test.csv")
  expect_equal(csv_comparison_file,  import_file)

})

test_that("Limit agents", {

  expect_equal_to_reference(capture.output(summary(history, limit_agents = c("Exp3","UCB1"))),
                            file = "summary_history_limit.rds")

})


