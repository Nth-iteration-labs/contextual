#' Plot Method for Contextual History
#'
#' plot.history, a method for the plot generic. It is designed for a quick look at History data.
#'
#' @name plot.history
#' @family contextual
#'
#' @param x A \code{History} object.
#' @param ... Further plotting parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{MabWeightBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{ContextualWeightBandit}}
#'
#' @export
plot.History <- function(x, ...) {
  args <- eval(substitute(alist(...)))
  if ("type" %in% names(args))
    type <- eval(args$type)
  else
    type <- "cumulative"
  if ("args" %in% names(args))
    no_par <- eval(args$no_par)
  else
    no_par <- FALSE
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
  if ("plot_only_ci" %in% names(args))
    plot_only_ci <- eval(args$plot_only_ci)
  else
    plot_only_ci <- FALSE
  if ("ci" %in% names(args))
    ci <- eval(args$ci)
  else
    ci <- NULL
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
    lwd <- 1
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
      ci = ci,
      plot_only_ci = plot_only_ci,
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
      ci = ci,
      plot_only_ci = plot_only_ci,
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
#' @family contextual
#'
#' @param x A \code{History} object.
#' @param ... Further plotting parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{MabWeightBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{ContextualWeightBandit}}
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
#' @family contextual
#'
#' @param object A \code{History} object.
#' @param ... Further summary parameters.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{MabWeightBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{ContextualWeightBandit}}
#'
#' @importFrom data.table data.table setcolorder rbindlist
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

  cat("\n\nCumulative Regret:\n\n")
  print(cum[,c("agent","t", "sims", "cum_regret", "cum_regret_var",
               "cum_regret_sd", "cum_regret_ci")], fill = TRUE, row.names = FALSE)

  cat("\n\nCumulative Reward:\n\n")
  print(cum[,c("agent","t", "sims", "cum_reward", "cum_reward_var",
               "cum_reward_sd", "cum_reward_ci")], fill = TRUE, row.names = FALSE)

  cat("\n\nCumulative Reward Rate:\n\n")
  crr <- cum[,c("agent","t", "sims", "cum_reward_rate", "cum_reward_rate_var",
               "cum_reward_rate_sd", "cum_reward_rate_ci")]
  names(crr) <- c("agent","t", "sims", "crr_reward", "crr_reward_var",
               "crr_reward_sd", "crr_reward_ci")
  print(crr, fill = TRUE, row.names = FALSE)


  cat("\n")
}

