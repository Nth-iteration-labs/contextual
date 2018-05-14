#' @export
plot.History <- function(x, ...) {
  args <- eval(substitute(alist(...)))
  if ("type" %in% names(args))
    type <- eval(args$type)
  else
    type <- "cumulative"
  if ("args" %in% names(args))
    grid <- eval(args$grid)
  else
    grid <- FALSE
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
  if ("ci" %in% names(args))
    ci <- eval(args$ci)
  else
    ci <- FALSE
  if ("step_size" %in% names(args))
    step_size <- eval(args$step_size)
  else
    step_size <- 1
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
  if ("start_step" %in% names(args))
    start_step <- eval(args$start_step)
  else
    start_step <- 1
  if ("ylim" %in% names(args))
    ylim <- eval(args$ylim)
  else
    ylim <- NULL
  if ("legend_labels" %in% names(args))
    legend_labels <- eval(args$legend_labels)
  else
    legend_labels <- NULL
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
  if ("grid" %in% names(args))
    grid <- eval(args$grid)
  else
    grid <- FALSE
  if (type == "grid") {
    Plot$new()$grid(x,
                    xlim = xlim,
                    use_colors = use_colors,
                    ci = ci,
                    step_size = step_size,
                    start_step = start_step,
                    color_step = color_step,
                    lty_step = lty_step,
                    lwd = lwd,
                    rate = rate,
                    ylim = ylim,
                    legend_labels = legend_labels,
                    legend_border = legend_border,
                    legend_title = legend_title)
  } else if (type == "cumulative") {
    Plot$new()$cumulative(
      x,
      xlim = xlim,
      legend = legend,
      regret = regret,
      use_colors = use_colors,
      ci = ci,
      step_size = step_size,
      start_step = start_step,
      color_step = color_step,
      lty_step = lty_step,
      lwd = lwd,
      rate = rate,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_title = legend_title,
      grid = grid
    )
  } else if (type == "average") {
    Plot$new()$average(
      x,
      xlim = xlim,
      legend = legend,
      regret = regret,
      use_colors = use_colors,
      ci = ci,
      step_size = step_size,
      start_step = start_step,
      color_step = color_step,
      lty_step = lty_step,
      lwd = lwd,
      rate = rate,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_title = legend_title,
      grid = grid
    )
  } else if (type == "optimal") {
    Plot$new()$optimal(
      x,
      xlim = xlim,
      legend = legend,
      regret = regret,
      use_colors = use_colors,
      ci = ci,
      step_size = step_size,
      start_step = start_step,
      color_step = color_step,
      lwd = lwd,
      lty_step = lty_step,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_title = legend_title,
      grid = grid
    )
  } else if (type == "arms") {
    Plot$new()$arms(
      x,
      xlim = xlim,
      legend = legend,
      use_colors = use_colors,
      step_size = step_size,
      start_step = start_step,
      ylim = ylim,
      legend_labels = legend_labels,
      legend_border = legend_border,
      legend_title = legend_title,
      grid = grid
    )
  }
}

#' @export
print.History <- function(x, ...) {
  x$print_data()
}

#' @export
summary.History <- function(object, ...) {
  p <- do.call(
    rbind,
    Map(
      data.frame,
      cumulative_regret = object$cumulative(
        final = TRUE,
        rate = FALSE,
        regret = FALSE
      ),
      cumulative_regret_rate = object$cumulative(
        final = TRUE,
        rate = TRUE,
        regret = FALSE
      ),
      cumulative_reward = object$cumulative(
        final = TRUE,
        rate = FALSE,
        regret = TRUE
      ),
      cumulative_reward_rate = object$cumulative(
        final = TRUE,
        rate = TRUE,
        regret = TRUE
      )
    )
  )
  print(p)
}

