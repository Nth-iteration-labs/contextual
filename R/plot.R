#' @import R.devices
#' @export
Plot <- R6::R6Class(
  "Plot",
  portable = FALSE,
  class = FALSE,
  public = list(
    max_sim = NULL,
    initialize = function() {
      self$max_sim = 0
    },

    ############################ plot types ############################

    cumulative = function(history, grid = FALSE, xlim = NULL, legend = TRUE, regret = TRUE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, rate = FALSE, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      history <- check_history_data(history)
      if (regret) {
        if (rate) {
          ylab_title = "Cumulative regret - rate"
          history$cumsum = history[, cumsum(opimal - reward)/t, by = list(agent, sim)]$V1
        } else {
          ylab_title = "Cumulative expected regret"
          history$cumsum = history[, cumsum(opimal - reward), by = list(agent, sim)]$V1
        }
        cs <- history[, list(var = var(cumsum) , data = mean(cumsum)), by = list(t, agent)]
      } else {
        if (rate) {
          ylab_title = "Cumulative reward - rate"
          history$cumsum = history[, cumsum(reward)/t, by = list(agent, sim)]$V1
        } else {
          ylab_title = "Cumulative reward"
          history$cumsum = history[, cumsum(reward), by = list(agent, sim)]$V1
        }
        cs <- history[, list(var = var(cumsum), data = mean(cumsum)), by = list(t, agent)]
      }
      do_plot(cs = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, grid = grid, step_size = step_size, start_step = start_step, color_step = color_step, lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels, legend_border = legend_border, legend_title = legend_title)
      invisible(self)
    },
    average = function(history, grid = FALSE, xlim = NULL, legend = TRUE, regret = FALSE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, rate = FALSE, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      history <- check_history_data(history)
      if (regret) {
          ylab_title = "Average expected regret"
          cs <- history[, list(var = var(opimal - reward) , data = mean(opimal - reward)), by = list(t, agent)]
      } else {
          ylab_title = "Average reward"
          cs <-  history[, list(var = var(reward) , data = mean(reward)), by = list(t, agent)]
      }
      do_plot(cs = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, grid = grid, step_size = step_size, start_step = start_step, color_step = color_step, lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels, legend_border = legend_border, legend_title = legend_title)
      invisible(self)
    },
    optimal = function(history, grid = FALSE, xlim = NULL, legend = TRUE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      history <- check_history_data(history)
      ylab_title = "Optimal arm"
      cs <- history[, list(var = var(is_optimal * 100), data = mean(is_optimal) * 100), by = list(t, agent)]
      do_plot( cs = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, grid = grid, step_size = step_size, start_step = start_step, color_step = color_step, lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels, legend_border = legend_border, legend_title = legend_title)#ylim = c(0, 100)
      invisible(self)
    },

    ############################ main plot function  ############################

    do_plot = function(cs, ylab_title, use_colors = FALSE, ci = FALSE, legend = TRUE, grid = FALSE, ylim = NULL, step_size = 1, start_step = 1, color_step = 1, lty_step = 1, lwd = 1, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      if (grid == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }
      cs <- cs[order(agent, t)]
      if (ci) {
        # 95% confidence
        ci_range <- cs$data + outer(sqrt(cs$var)/sqrt(self$max_sim), c(1.64, -1.64))
        cs = cbind(cs, ci_range)
        colnames(cs)[colnames(cs) == 'V2'] <- 'ci_lower'
        colnames(cs)[colnames(cs) == 'V1'] <- 'ci_upper'
      }
      plot.new()
      agent_levels <- levels(as.factor(cs$agent))
      n_agents <- length(agent_levels)
      cl <- gg_color_hue(round(n_agents/color_step))
      cl <- rep(cl, round(color_step))
      lt <- rep(1,n_agents)
      if (lty_step > 1) {
        lt <- rep(1:round(lty_step), each = round(n_agents/lty_step))
      }
      if (ci) {
        min_ylim = cs[t >= start_step][, min(ci_lower)]
        max_ylim = cs[t >= start_step][, max(ci_upper)]
      } else {
        min_ylim = cs[t >= start_step][, min(data)]
        max_ylim = cs[t >= start_step][, max(data)]
      }
      if (!is.null(ylim)) {
        min_ylim <- ylim[1]
        max_ylim <- ylim[2]
      }
      plot.window(xlim = c(start_step, cs[, max(t)]), ylim = c(min_ylim, max_ylim))

      if (use_colors) {
        if (ci) {
          color <- 1
          for (agent_name in agent_levels) {
            step_seq <- seq(start_step, nrow(cs[cs$agent == agent_name]), step_size)
            polygon(
              c(cs[cs$agent == agent_name][step_seq]$t, rev(cs[cs$agent == agent_name][step_seq]$t)),
              c(cs[cs$agent == agent_name][step_seq]$ci_lower, rev(cs[cs$agent == agent_name][step_seq]$ci_upper)),
              col = adjustcolor(cl[color], alpha.f = 0.3),
              border = NA
            )
            color <- color + 1
          }
        }
        line_counter <- 1
        for (agent_name in agent_levels) {
          step_seq <- seq(start_step, nrow(cs[cs$agent == agent_name]), step_size)
          lines(
            cs[cs$agent == agent_name][step_seq]$t,
            cs[cs$agent == agent_name][step_seq]$data,
            lwd = lwd,
            lty = lt[line_counter],
            col = adjustcolor(cl[line_counter], alpha.f = 0.9),
            type = "l"
          )
          line_counter <- line_counter + 1
        }
      } else {
        line_counter <- 1
        for (agent_name in agent_levels) {
          step_seq <- seq(start_step, nrow(cs[cs$agent == agent_name]), step_size)
          if (ci) {
            polygon(
              c(cs[cs$agent == agent_name][step_seq]$t, rev(cs[cs$agent == agent_name][step_seq]$t)),
              c(cs[cs$agent == agent_name][step_seq]$ci_lower, rev(cs[cs$agent == agent_name][step_seq]$ci_upper)),
              col = rgb(0.8, 0.8, 0.8, 0.4),
              border = NA
            )
          }
          lines(
            cs[cs$agent == agent_name][step_seq]$t,
            cs[cs$agent == agent_name][step_seq]$data,
            lwd = lwd,
            lty = lt[line_counter],
            col = rgb(0.2, 0.2, 0.2, 0.8),
            type = "l"
          )
          line_counter <- line_counter + 1
        }
      }
      axis(1)
      axis(2)
      title(xlab = "Time step")
      title(ylab = ylab_title)
      box()


      if (ylab_title == "Cumulative regret - rate") {
        legend_position <- "topright"
      } else {
        legend_position <- "topleft"
      }
      if (legend) {
        if (!is.null(legend_labels)) agent_levels = legend_labels
        if (!is.null(legend_border))  {
          bty = "n"
        } else {
          bty = "o"
        }
        if (use_colors == FALSE) cl = rgb(0.2, 0.2, 0.2, 0.8)
        legend(
          legend_position,
          NULL,
          agent_levels,
          col = cl,
          title = legend_title,
          lwd = lwd,
          lty = lt,
          bty = bty,
          bg = "white"
        )
      }
      if (grid == FALSE) {
        dev.flush()
        par(old.par)
      }
    },

    ############################ arms plot ############################

    arms = function(history, grid = FALSE, xlim = NULL, legend = TRUE, use_colors = TRUE, step_size = 1, start_step = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      if (grid == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }
      history <- check_history_data(history)
      ylab_title = "Arm choice %"
      agent_levels <- levels(as.factor(history$agent))
      if (length(agent_levels) > 1)
        warning("## Arm percentage plot always plots the results of just one agent.", call. = FALSE)
      history <- history[agent == agent_levels[1]]
      cs <- history[, list(arm_count = .(rowCount = .N)), by = list(t,choice)]
      max_sim  = history[, max(sim)]
      max_t    = history[, max(t)]
      arm_levels <- levels(as.factor(cs$choice))
      max_arm = length(arm_levels)
      cs$arm_count <- as.double((unlist(cs$arm_count)/max_sim) * 100L)
      eg = expand.grid(t = seq(1.0, max_t, 1), choice = seq(1.0, max_arm, 1))
      cs <- merge(cs, eg, all = TRUE)
      cs[is.na(cs)] <- 0.0
      cs$csum <- ave(cs$arm_count, cs$t, FUN = cumsum)
      cs$zero <- 0.0
      cs <- cs[order(choice, t)]
      min_ylim <- 0
      max_ylim <- 100
      plot.new()
      cl <- gg_color_hue(length(agent_levels))
      plot.window(xlim = c(start_step, cs[, max(t)]), ylim = c(min_ylim, max_ylim))
      cl <- gg_color_hue(length(arm_levels))
      color <- 1
      polygon(
        c(cs[cs$choice == 1]$t, rev(cs[cs$choice == 1]$t)),
        c(cs[cs$choice == 1]$csum, rev(cs[cs$choice == 1]$zero)),
        col = adjustcolor(cl[color], alpha.f = 0.6),
        border = NA
      )
      color <- 2
      for (arm_nr in c(2:length(arm_levels))) {
        polygon(
          c(cs[cs$choice == arm_nr]$t, rev(cs[cs$choice == arm_nr]$t)),
          c(cs[cs$choice == arm_nr - 1]$csum, rev(cs[cs$choice == arm_nr]$csum)),
          col = adjustcolor(cl[color], alpha.f = 0.6),
          border = NA
        )
        color <- color + 1
      }
      axis(1)
      axis(2)
      title(xlab = "Time Step")
      title(ylab = ylab_title)
      box()
      if (legend) {
        legend(
          "bottomright",
          NULL,
          paste("arm", arm_levels, sep = " "),
          col = cl,
          title = agent_levels[1],
          pch = 15,
          pt.cex = 1.2,
          bg = "white",
          inset = c(0.08, 0.1)
        )
      }
      if (grid == FALSE) {
        dev.flush()
        par(old.par)
      }
      invisible(self)
    },
    gg_color_hue = function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    },
    check_history_data = function(history) {
      if (!data.table::is.data.table(history)) {
        if (is(history,"History")) {
          history = history$get_data_table()
          self$max_sim   = history[, max(sim)]
          history <- history[order(agent, sim, t)]
          return(history)
        } else {
          stop("Plots need History or data.table object",
               call. = FALSE)
        }
      } else {
        self$max_sim   = history[, max(sim)]
        history <- history[order(agent, sim, t)]
        return(history)
      }
    }
  )
)

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

# variance plot optimal arm --> is the arm set in advance? / sqrt(max_sim)?
# REALLY needs refactoring again.

# TODO: use of (...)
