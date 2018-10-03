#' Plot Method for Contextual History
#'
#' plot.history, a method for the plot generic. It is designed for a quick look at History data.
#'
#' @name plot.history
#'
#' @param x A \code{History} object.
#' @param ... Further plotting parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBernoulliBandit}},
#' \code{\link{OfflinePolicyEvaluatorBandit}}, \code{\link{ContextualLogitBandit}}
#'
#' @export
plot.History <- function(x, ...) {
  args <- eval(substitute(alist(...)))
  if ("type" %in% names(args))
    type <- eval(args$type)
  else
    type <- "cumulative"
  if ("xlim" %in% names(args))
    xlim <- eval(args$xlim)
  else
    xlim <- NULL
  if ("legend" %in% names(args))
    legend <- eval(args$legend)
  else
    legend <- TRUE
  if ("regret" %in% names(args))
    regret <- eval(args$regret)
  else
    regret <- TRUE
  if ("use_colors" %in% names(args))
    use_colors <- eval(args$use_colors)
  else
    use_colors <- TRUE
  if ("plot_only_disp" %in% names(args))
    plot_only_disp <- eval(args$plot_only_disp)
  else
    plot_only_disp <- FALSE
  if ("disp" %in% names(args))
    disp <- eval(args$disp)
  else
    disp <- NULL
  if ("traces" %in% names(args))
    traces <- eval(args$traces)
  else
    traces <- FALSE
  if ("traces_alpha" %in% names(args))
    traces_alpha <- eval(args$traces_alpha)
  else
    traces_alpha <- 0.3
  if ("traces_max" %in% names(args))
    traces_max <- eval(args$traces_max)
  else
    traces_max <- 100
  if ("smooth" %in% names(args))
    smooth <- eval(args$smooth)
  else
    smooth <- FALSE
  if ("interval" %in% names(args))
    interval <- eval(args$interval)
  else
    interval <- 1
  if ("color_step" %in% names(args))
    color_step <- eval(args$color_step)
  else
    color_step <- 1
  if ("lty_step" %in% names(args))
    lty_step <- eval(args$lty_step)
  else
    lty_step <- 1
  if ("lwd" %in% names(args))
    lwd <- eval(args$lwd)
  else
    lwd <- 2
  if ("ylim" %in% names(args))
    ylim <- eval(args$ylim)
  else
    ylim <- NULL
  if ("legend_labels" %in% names(args))
    legend_labels <- eval(args$legend_labels)
  else
    legend_labels <- NULL
  if ("legend_position" %in% names(args))
    legend_position <- args$legend_position
  else
    legend_position <- "topleft"
  if ("limit_agents" %in% names(args))
    limit_agents <- eval(args$limit_agents)
  else
    limit_agents <- NULL
  if ("legend_border" %in% names(args))
    legend_border <- eval(args$legend_border)
  else
    legend_border <- NULL
  if ("legend_title" %in% names(args))
    legend_title <- eval(args$legend_title)
  else
    legend_title <- NULL
  if ("rate" %in% names(args))
    rate <- eval(args$rate)
  else
    rate <- FALSE
  if ("no_par" %in% names(args))
    no_par <- eval(args$no_par)
  else
    no_par <- FALSE
  if (type == "cumulative") {
    Plot$new()$cumulative(
      x,
      xlim = xlim,
      legend = legend,
      regret = regret,
      use_colors = use_colors,
      disp = disp,
      plot_only_disp = plot_only_disp,
      traces = traces,
      traces_max = traces_max,
      traces_alpha = traces_alpha,
      smooth = smooth,
      interval = interval,
      color_step = color_step,
      lty_step = lty_step,
      lwd = lwd,
      rate = rate,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_position = legend_position,
      legend_title = legend_title,
      no_par = no_par,
      limit_agents = limit_agents
    )
  } else if (type == "average") {
    Plot$new()$average(
      x,
      xlim = xlim,
      legend = legend,
      regret = regret,
      use_colors = use_colors,
      disp = disp,
      plot_only_disp = plot_only_disp,
      traces = traces,
      traces_max = traces_max,
      traces_alpha = traces_alpha,
      smooth = smooth,
      interval = interval,
      color_step = color_step,
      lty_step = lty_step,
      lwd = lwd,
      rate = rate,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_position = legend_position,
      legend_title = legend_title,
      no_par = no_par,
      limit_agents = limit_agents
    )
  } else if (type == "arms") {
    Plot$new()$arms(
      x,
      xlim = xlim,
      legend = legend,
      use_colors = use_colors,
      interval = interval,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_position = legend_position,
      legend_title = legend_title,
      no_par = no_par,
      limit_agents = limit_agents
    )
  }
}
#' Print Method for Contextual History
#'
#' print.history, a method for the print generic. It is designed for a quick look at History data.
#'
#' @name print.history
#'
#' @param x A \code{History} object.
#' @param ... Further plotting parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBernoulliBandit}},
#' \code{\link{OfflinePolicyEvaluatorBandit}}, \code{\link{ContextualLogitBandit}}
#'
#' @export
print.History <- function(x, ...) {
  summary.History(x)
}
#' Summary Method for Contextual History
#'
#' summary.history, a method for the summary generic. It is designed for a quick summary of History data.
#'
#' @name summary.history
#'
#' @param object A \code{History} object.
#' @param ... Further summary parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @export
summary.History <- function(object, ...) {

  args <- eval(substitute(alist(...)))
  if ("limit_agents" %in% names(args))
    limit_agents <- eval(args$limit_agents)
  else
    limit_agents <- NULL

  cum <- object$get_cumulative_result(limit_agents=limit_agents, as_list = FALSE)
  cum$sims <- object$get_simulation_count()

  cat("\nAgents:\n\n")
  agents <- object$get_agent_list()
  cat(paste(' ', agents, collapse = ', '))

  cat("\n\nCumulative regret:\n\n")
  print(cum[,c("agent","t", "sims", "cum_regret", "cum_regret_var",
               "cum_regret_sd")], fill = TRUE, row.names = FALSE)

  cat("\n\nCumulative reward:\n\n")
  print(cum[,c("agent","t", "sims", "cum_reward", "cum_reward_var",
               "cum_reward_sd")], fill = TRUE, row.names = FALSE)

  cat("\n\nCumulative reward rate:\n\n")
  crr <- cum[,c("agent","t", "sims", "cum_reward_rate", "cum_reward_rate_var",
               "cum_reward_rate_sd")]
  names(crr) <- c("agent","t", "sims", "cur_reward", "cur_reward_var",
               "cur_reward_sd")
  print(crr, fill = TRUE, row.names = FALSE)


  cat("\n")
}

