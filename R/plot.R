#' @import R.devices
#' @export
Plot <- R6::R6Class(
  "Plot",
  portable = FALSE,
  inherit = Contextual,
  class = FALSE,
  public = list(
    bandit_matrix = NULL,
    initialize = function() {
    },
    grid = function(history,
                    type = "grid",
                    xlim = NULL,
                    legend = TRUE,
                    regret = FALSE,
                    use_colors = TRUE,
                    ci = FALSE) {
      if (!data.table::is.data.table(history)) history = history$get_data_table()
      history <- history[t <= history[ , max(t), by = c("sim")][,min(V1)]]
      old.par <- par(no.readonly = TRUE)
      dev.hold()
      self$bandit_matrix <- layout(matrix(c(1, 3, 2, 4), 2, 2, byrow = TRUE))
      par(mar = c(3, 5, 1, 1))#b,l,t,r
      self$cummulative(history, grid = TRUE, legend = TRUE, regret = TRUE)
      par(mar = c(3, 5, 1, 1))
      self$optimal(history, grid = TRUE, legend = FALSE)
      par(mar = c(3, 5, 1, 2))
      self$cummulative(history, grid = TRUE, legend = FALSE, regret = FALSE)
      par(mar = c(3, 5, 1, 2))
      self$average(history, grid = TRUE, legend = FALSE)
      dev.flush()
      par(old.par)
      invisible(self)
    },
    cummulative = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE,
                       regret = FALSE,
                       use_colors = TRUE,
                       ci = FALSE) {
      if (!data.table::is.data.table(history)) history = history$get_data_table()
      max_sim   = history[, max(sim)]
      history <- history[order(agent,t,sim)]
      if (regret) {
        ylab_title = "Cummulative regret"
        cs <- history[, list(sd = sd(oracle - reward) / sqrt(max_sim), data = mean(oracle - reward)), by = list(t, agent)]
      } else {
        ylab_title = "Cummulative reward"
        cs <- history[, list(sd = sd(reward) / sqrt(max_sim), data = mean(reward)), by = list(t, agent)]
      }
      cs$data = cs[, cumsum(data), by = list(agent)][, 2]
      do_plot(cs, ylab_title, use_colors, ci, legend, grid)
      invisible(self)
    },
    average = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE,
                       regret = FALSE,
                       use_colors = TRUE,
                       ci = FALSE) {

      if (!data.table::is.data.table(history)) history = history$get_data_table()
      max_sim   = history[, max(sim)]
      history <- history[order(agent,t,sim)]
      if (regret) {
        ylab_title = "Average regret"
        cs <- history[, list(sd = sd(oracle - reward) / sqrt(max_sim), data = mean(oracle - reward)), by = list(t, agent)]
      } else {
        ylab_title = "Average reward"
        cs <- history[, list(sd = sd(reward) / sqrt(max_sim), data = mean(reward)), by = list(t, agent)]
      }
      do_plot(cs = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, grid = grid)
      invisible(self)
    },
    optimal = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE,
                       regret = FALSE,
                       use_colors = TRUE,
                       ci = FALSE) {
      if (!data.table::is.data.table(history)) history = history$get_data_table()
      history <- history[order(agent,t,sim)]
      ylab_title = "Optimal arm"
      cs <- history[, list(data = mean(is_optimal) * 100), by = list(t, agent)]
      do_plot(cs = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, grid = grid, ylim = c(0, 100))
      invisible(self)
    },
    ts = function(history,
                  grid = FALSE,
                  xlim = NULL,
                  legend = TRUE) {
      if (!data.table::is.data.table(history)) history = history$get_data_table()
      if (grid == FALSE)
        dev.hold()

      last_t   = history[,max(t)]
      theta_list = history[agent == "TSampling" & t == last_t & sim == 1,theta]
      mu = lname_to_vector(theta_list[[1]], "mu")
      succes_n = lname_to_vector(theta_list[[1]], "succes")
      chosen_n = lname_to_vector(theta_list[[1]], "chosen")

      alpha = 1; beta = 1
      alpha_per_arm = alpha + succes_n
      beta_per_arm = beta + chosen_n - succes_n
      arm_n = length(alpha_per_arm)

      xv = seq(0, 1, by = 0.01); yv_all = c()
      for (i in 1:arm_n) {
        yv_all = as.vector(rbind(yv_all,dbeta(xv,alpha_per_arm[i],beta_per_arm[i])))
      }
      #ymax = max(yv_all)
      curve(dbeta(x,alpha_per_arm[1],
                  beta_per_arm[1]),
            col = 4,
            xlab = "X",
            ylab = "PDF",
            ylim = c(0, (10 + 0.6)))
      #ylim = c(0, (ymax + 0.6)))
      for (i in 2:arm_n) {
        curve(dbeta(x,alpha_per_arm[i],beta_per_arm[i]),add = TRUE,col = (i + 3))
      }
      abline(v = mu, col = c(4:(arm_n + 4)))
      #points(max(mu), 0, type = "p")
      if (legend)
        legend(
          "topleft",
          NULL,
          paste("Arm ", 1:arm_n, sep = ""),
          col = c(4:(arm_n + 4)),
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE)
        dev.flush()
      invisible(self)
    },
    do_plot = function(cs,
                       ylab_title,
                       use_colors = FALSE,
                       ci = FALSE,
                       legend = TRUE,
                       grid = FALSE,
                       ylim = NULL) {


      if (grid == FALSE) dev.hold()

      cs <- cs[order(agent,t)]

      if (ci) {
        # 95% confidence
        ci_range <- cs$data + outer(cs$sd, c(1.96, -1.96))
        cs = cbind(cs, ci_range)
        colnames(cs)[colnames(cs) == 'V2'] <- 'ci_lower'
        colnames(cs)[colnames(cs) == 'V1'] <- 'ci_upper'
      }

      plot.new()

      agent_levels <- levels(as.factor(cs$agent))

      cl <- gg_color_hue(length(agent_levels))

      if (ci) {
        min_ylim = cs[, min(ci_lower)]
        max_ylim = cs[, max(ci_upper)]
      } else {
        min_ylim = cs[, min(data)]
        max_ylim = cs[, max(data)]
      }
      if (!is.null(ylim)) {
        min_ylim <- ylim[1]
        max_ylim <- ylim[2]
      }
      plot.window(xlim = c(0, cs[, max(t)]),
                  ylim = c(min_ylim, max_ylim))

      if (use_colors) {
        cl <- gg_color_hue(length(agent_levels))
        if (ci) {
          color <- 1
          for (agent_name in agent_levels) {
            polygon(
              c(cs[cs$agent == agent_name]$t, rev(cs[cs$agent == agent_name]$t)),
              c(cs[cs$agent == agent_name]$ci_lower, rev(cs[cs$agent == agent_name]$ci_upper)),
              col = adjustcolor(cl[color], alpha.f = 0.3),
              border = NA
            )
            color <- color + 1
          }
        }
        color <- 1
        for (agent_name in agent_levels) {
          lines(
            cs[cs$agent == agent_name]$t,
            cs[cs$agent == agent_name]$data,
            lwd = 1,
            lty = 1,
            col = adjustcolor(cl[color], alpha.f = 0.9),
            type = "l"
          )
          color <- color + 1
        }
      } else {
        for (agent_name in agent_levels) {
          if (ci) {
            polygon(
              c(cs[cs$agent == agent_name]$t, rev(cs[cs$agent == agent_name]$t)),
              c(cs[cs$agent == agent_name]$ci_lower, rev(cs[cs$agent == agent_name]$ci_upper)),
              col = rgb(0.8, 0.8, 0.8, 0.4),
              border = NA
            )
          }
          lines(cs[cs$agent == agent_name]$t,
                cs[cs$agent == agent_name]$data,
                lwd = 1,
                lty = 1,
                col = rgb(0.2, 0.2, 0.2, 0.8),
                type = "l")
        }
      }
      axis(1)
      axis(2)
      title(xlab = "Time Step")
      title(ylab = ylab_title)
      box()
      if (legend)
        legend(
          "topleft",
          NULL,
          agent_levels,
          col = cl,
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE) dev.flush()
    },
    gg_color_hue = function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    },
    is_rstudio = function() {
      .Platform$GUI == "RStudio"
    },
    set_external = function(ext = TRUE,
                            width = 10,
                            height = 6) {
      if (self$is_rstudio()) {
        if (isTRUE(ext)) {
          sysname <- tolower(Sys.info()["sysname"])
          device.name <- "x11"
          switch(sysname,
                 darwin = {
                   device.name = "quartz"
                 },
                 windows = {
                   device.name = "windows"
                 })
          options("device" = device.name)
          R.devices::devOptions(sysname, width = width, height = height)
        } else{
          options("device" = "RStudioGD")
        }
        graphics.off()
      }
      invisible(self)
    }
  )
)

#' @export
plot.History <- function(x,...) {

    # need to extract these values from the ellipsis, as this function overrides
    # plot(x, ...) for the History class, and needs to have the exact same (x,..)
    # signature

    args <- eval(substitute(alist(...)))

    if ("type" %in% names(args)) type = args$type else type = "cummulative"
    if ("args" %in% names(args)) grid = args$grid else grid = FALSE
    if ("xlim" %in% names(args)) xlim = args$xlim else xlim = NULL
    if ("legend" %in% names(args)) legend = args$legend else legend = TRUE
    if ("regret" %in% names(args)) regret = args$regret else regret = TRUE
    if ("use_colors" %in% names(args)) use_colors = args$use_colors else use_colors = TRUE
    if ("ci" %in% names(args)) ci = args$ci else ci = FALSE

    if (type == "grid") {
      Plot$new()$grid(x,
                      xlim = xlim,
                      use_colors = use_colors,
                      ci = ci)
    } else if (type == "cummulative") {
      Plot$new()$cummulative(
        x,
        xlim = xlim,
        legend = legend,
        regret = regret,
        use_colors = use_colors,
        ci = ci
      )
    } else if (type == "average") {
      Plot$new()$average(
        x,
        xlim = xlim,
        legend = legend,
        regret = regret,
        use_colors = use_colors,
        ci = ci
      )
    } else if (type == "optimal") {
      Plot$new()$optimal(
        x,
        xlim = xlim,
        legend = legend,
        regret = regret,
        use_colors = use_colors,
        ci = ci
      )
    }
  }

#' External Plot
#'
#' Plot intro
#'
#' @section Usage:
#' \preformatted{b <- Plot$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Plot} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Plot, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Plot
#' @examples
#'\dontrun{}
#'
NULL
