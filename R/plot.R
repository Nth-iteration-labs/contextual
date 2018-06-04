#' @export
Plot <- R6::R6Class(
  "Plot",
  portable = FALSE,
  class = FALSE,
  public = list(
    max_sim = NULL,
    initialize = function() {
      self$max_sim <- 0
    },

    ############################ plot types ############################

    cumulative = function(history, no_par = FALSE, xlim = NULL, legend = TRUE, regret = TRUE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, rate = FALSE, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {

      if (regret) {
        if (rate) {
          ylab_title <- "Cumulative regret - rate"
          line_data_name <- "cum_regret_rate"
          ci_data_name <- "cum_regret_rate_ci"
        } else {
          ylab_title <- "Cumulative expected regret"
          line_data_name <- "cum_regret"
          ci_data_name <- "cum_regret_ci"
        }
      } else {
        if (rate) {
          ylab_title <- "Cumulative reward - rate"
          line_data_name <- "cum_reward_rate"
          ci_data_name <- "cum_reward_rate_ci"
        } else {
          ylab_title <- "Cumulative reward"
          line_data_name <- "cum_reward"
          ci_data_name <- "cum_reward_ci"
        }
      }

      data <- history$get_cumulative_data(limit_cols = c("agent","t",
                                                         line_data_name,ci_data_name))

      do_plot(data = data, line_data_name = line_data_name, ci_data_name = ci_data_name,
              ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend,
              no_par = no_par, step_size = step_size, start_step = start_step, color_step = color_step,
              lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels,
              legend_border = legend_border, legend_title = legend_title)

      invisible(self)
    },
    average = function(history, no_par = FALSE, xlim = NULL, legend = TRUE, regret = TRUE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, rate = FALSE, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {

      if (regret) {
          ylab_title <- "Average regret"
          line_data_name <- "reward"
          ci_data_name <- "regret_ci"
      } else {
          ylab_title <- "Average reward"
          line_data_name <- "regret"
          ci_data_name <- "regret_ci"
      }

      data <- history$get_cumulative_data(limit_cols = c("agent","t",
                                                         line_data_name,ci_data_name))

      do_plot(data = data, line_data_name = line_data_name, ci_data_name = ci_data_name,
              ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend,
              no_par = no_par, step_size = step_size, start_step = start_step, color_step = color_step,
              lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels,
              legend_border = legend_border, legend_title = legend_title)

      invisible(self)
    },


    optimal = function(history, no_par = FALSE, xlim = NULL, legend = TRUE, use_colors = TRUE, ci = FALSE, step_size = 1, start_step = 1, color_step = 1, lty_step = 1, lwd = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      history <- check_history_data(history)
      ylab_title <- "Optimal arm"
      data <- history[, list(var = var(choice_is_optimal * 100), data = mean(choice_is_optimal) * 100), by = list(t, agent)]
      do_plot( data = cs, ylab_title = ylab_title, use_colors = use_colors, ci = ci, legend = legend, no_par = no_par, step_size = step_size, start_step = start_step, color_step = color_step, lty_step = lty_step, lwd = lwd, ylim = ylim, legend_labels = legend_labels, legend_border = legend_border, legend_title = legend_title)#ylim = c(0, 100)
      invisible(self)
    },

    ############################ main plot function  ############################

    # lines = xxxxSD, sd = xxxSD, multi-lines = FALSE, agents = NULL

    do_plot = function(data = data, line_data_name = line_data_name, ci_data_name = ci_data_name,
                       ylab_title, use_colors = FALSE, ci = FALSE, legend = TRUE, no_par = FALSE,
                       ylim = NULL, step_size = 1, start_step = 1, color_step = 1, lty_step = 1,
                       lwd = 1, legend_labels = NULL, legend_border = NULL,
                       legend_title = NULL) {

      if (no_par == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }
      data <- data[order(agent, t)]
      if (ci) {
        ci_range <- data[[line_data_name]] + outer(data[[ci_data_name]], c(1, -1))
        data <- cbind(data, ci_range)
        colnames(data)[colnames(data) == 'V2'] <- 'ci_lower'
        colnames(data)[colnames(data) == 'V1'] <- 'ci_upper'
      }

      plot.new()
      agent_levels <- levels(as.factor(data$agent))
      n_agents <- length(agent_levels)
      cl <- gg_color_hue(round(n_agents/color_step))
      cl <- rep(cl, round(color_step))
      lt <- rep(1,n_agents)
      if (lty_step > 1) {
        lt <- rep(1:round(lty_step), each = round(n_agents/lty_step))
      }
      if (ci) {
        min_ylim <- data[t >= start_step][, min(ci_lower)]
        max_ylim <- data[t >= start_step][, max(ci_upper)]
      } else {
        min_ylim <- data[t >= start_step][, min(data[[line_data_name]])]
        max_ylim <- data[t >= start_step][, max(data[[line_data_name]])]
      }
      if (!is.null(ylim)) {
        min_ylim <- ylim[1]
        max_ylim <- ylim[2]
      }
      plot.window(xlim = c(start_step, data[, max(t)]), ylim = c(min_ylim, max_ylim))

      if (use_colors) {
        if (ci) {
          if (FALSE) {
          ################################# temp ################################### multiline
          #### smooth #####

          history[order(agent, sim, t)]
          for (agent_name in agent_levels) {
            step_seq <- seq(start_step, nrow(data[data$agent == agent_name]), step_size)
            agent_sims <- unique(history[history$agent == agent_name]$sim)
            for (as in agent_sims) {
              lines(
                history[history$agent == agent_name & history$sim == as][step_seq]$t,
                history[history$agent == agent_name & history$sim == as][step_seq][[line_data_name]],
                #supsmu(history[history$agent == agent_name & history$sim == as][step_seq]$t, ####
                #history[history$agent == agent_name & history$sim == as][step_seq]$cumsum),
                lwd = lwd,
                col = rgb(0.8, 0.8, 0.8, 0.2)
              )
            }
          }
          ############################
          }

          color <- 1
          for (agent_name in agent_levels) {
            step_seq <- seq(start_step, nrow(data[data$agent == agent_name]), step_size)
            polygon(
              c(data[data$agent == agent_name][step_seq]$t, rev(data[data$agent == agent_name][step_seq]$t)),
              c(data[data$agent == agent_name][step_seq]$ci_lower, rev(data[data$agent == agent_name][step_seq]$ci_upper)),
              col = adjustcolor(cl[color], alpha.f = 0.3),
              border = NA
            )
            color <- color + 1
          }
        }
        line_counter <- 1
        for (agent_name in agent_levels) {
          step_seq <- seq(start_step, nrow(data[data$agent == agent_name]), step_size)
          lines(
            data[data$agent == agent_name][step_seq]$t,
            data[data$agent == agent_name][step_seq][[line_data_name]],
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
          step_seq <- seq(start_step, nrow(data[data$agent == agent_name]), step_size)
          if (ci) {
            polygon(
              c(data[data$agent == agent_name][step_seq]$t, rev(data[data$agent == agent_name][step_seq]$t)),
              c(data[data$agent == agent_name][step_seq]$ci_lower, rev(data[data$agent == agent_name][step_seq]$ci_upper)),
              col = rgb(0.8, 0.8, 0.8, 0.4),
              border = NA
            )
          }
          lines(
            data[data$agent == agent_name][step_seq]$t,
            data[data$agent == agent_name][step_seq]$data,
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
        if (!is.null(legend_labels)) agent_levels <- legend_labels
        if (!is.null(legend_border))  {
          bty <- "n"
        } else {
          bty <- "o"
        }
        if (use_colors == FALSE) cl <- rgb(0.2, 0.2, 0.2, 0.8)
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
      if (no_par == FALSE) {
        dev.flush()
        par(old.par)
      }
    },

    ############################ arms plot ############################

    arms = function(history, no_par = FALSE, xlim = NULL, legend = TRUE, use_colors = TRUE, step_size = 1, start_step = 1, ylim = NULL, legend_labels = NULL, legend_border = NULL, legend_title = NULL) {
      if (no_par == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }
      history <- history$get_data_table(limit_cols = c("agent","t","choice","sim"))
      ylab_title <- "Arm choice %"
      agent_levels <- levels(as.factor(history$agent))
      if (length(agent_levels) > 1)
        warning("## Arm percentage plot always plots the results of just one agent.", call. = FALSE)
      history <- history[agent == agent_levels[1]]
      data <- history[, list(arm_count = .(rowCount = .N)), by = list(t,choice)]
      max_sim  <- history[, max(sim)]
      max_t    <- history[, max(t)]
      arm_levels <- levels(as.factor(data$choice))
      max_arm <- length(arm_levels)
      data$arm_count <- as.double((unlist(data$arm_count)/max_sim) * 100L)
      eg <- expand.grid(t = seq(1.0, max_t, 1), choice = seq(1.0, max_arm, 1))
      data <- merge(data, eg, all = TRUE)
      data[is.na(data)] <- 0.0
      data$dataum <- ave(data$arm_count, data$t, FUN = cumsum)
      data$zero <- 0.0
      data <- data[order(choice, t)]
      min_ylim <- 0
      max_ylim <- 100
      plot.new()
      cl <- gg_color_hue(length(agent_levels))
      plot.window(xlim = c(start_step, data[, max(t)]), ylim = c(min_ylim, max_ylim))
      cl <- gg_color_hue(length(arm_levels))
      color <- 1
      polygon(
        c(data[data$choice == 1]$t, rev(data[data$choice == 1]$t)),
        c(data[data$choice == 1]$dataum, rev(data[data$choice == 1]$zero)),
        col = adjustcolor(cl[color], alpha.f = 0.6),
        border = NA
      )
      color <- 2
      for (arm_nr in c(2:length(arm_levels))) {
        polygon(
          c(data[data$choice == arm_nr]$t, rev(data[data$choice == arm_nr]$t)),
          c(data[data$choice == arm_nr - 1]$dataum, rev(data[data$choice == arm_nr]$dataum)),
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
      if (no_par == FALSE) {
        dev.flush()
        par(old.par)
      }
      invisible(self)
    },
    gg_color_hue = function(n) {
      hues <- seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
  )
)

#' Plot
#'
#' The R6 class \code{Plot} generates plots from on \code{History} data.
#' It is usually actually invoked by calling the generic \code{plot(h)}, where \code{h}
#' is an \code{History} class instance.
#'
#' @name Plot
#' @aliases average optimal arms do_plot gg_color_hue check_history_data
#'
#' @section Usage:
#' \preformatted{
#' Plot <- Plot$new()
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{cumulative(history,...)}}{
#'      Writes a plot of cumulative regret or reward (depending on parameter regret=TRUE/FALSE)
#'      over time.
#'   }
#'   \item{\code{average(history,...)}}{
#'      Writes a plot of average regret or reward (depending on parameter regret=TRUE/FALSE)
#'      over time.
#'   }
#'   \item{\code{optimal(history,...)}}{
#'      Writes a plot of the percentage the optimal arm was chosen over time.
#'   }
#'   \item{\code{arms(history),...}}{
#'      Writes a plot of the ratio with which each arm of a simulation was chosen over time.
#'      If multiple agents are run, only plots the arm plot of the first agent.
#'   }
#'  }
#'
#' @section Plot method arguments:
#'
#' \describe{
#'   \item{\code{regret}}{
#'      \code{(logical, TRUE)} If regret is TRUE, regret will be plotted on the y-axis.
#'      When regret is set to FALSE, reward will be plotted on the y-axis.
#'   }
#'   \item{\code{rate}}{
#'      (\code{logical, TRUE)} If rate is TRUE, the rate of the regret or reward is plotted.
#'   }
#'   \item{\code{no_par}}{
#'      \code{(logical, FALSE)} If no_par is TRUE, Plot() does not set or adjust plotting parameters.
#'      This makes it possible to set your own plotting parameters, for instance, when position multiple
#'      Plots in a no_par.
#'   }
#'   \item{\code{legend}}{
#'      \code{(logical, TRUE)} Print the legend.
#'   }
#'   \item{\code{legend_title}}{
#'      \code{(character , NULL)} Set a legend title.
#'   }
#'   \item{\code{legend_labels}}{
#'      \code{(list , NULL)} Set legend labels to custom values as specificed in this list.
#'   }
#'   \item{\code{legend_border}}{
#'      \code{(logical , NULL)} Set a legend title.
#'   }
#'   \item{\code{xlim}}{
#'      \code{(c(integer,integer), NULL)} Set x-axis limits.
#'   }
#'   \item{\code{xlim}}{
#'      \code{(c(integer,integer), NULL)} Set y-axis limits.
#'   }
#'   \item{\code{use_colors}}{
#'      \code{(logical, TRUE)} If use_colors is set to FALSE, plots will be in grayscale.
#'      Otherwise, plots will make use of a color palette.
#'   }
#'   \item{\code{ci}}{
#'      \code{(logical, NULL)} If ci is TRUE, Plot() will display 95% confidence intervals.
#'   }
#'   \item{\code{step_size}}{
#'      \code{(integer, NULL)} Plot only for every every t%%step_size==0
#'   }
#'   \item{\code{start_step}}{
#'      \code{(integer, NULL)} Start plotting at t=start_step
#'   }
#'   \item{\code{color_step}}{
#'      \code{(integer, 1)} Linecharts will cycle through agents/color_step colors.
#'   }
#'   \item{\code{color_step}}{
#'      \code{(integer, 1)} Linecharts will cycle through agents/lty_step line types
#'   }
#'   \item{\code{lwd}}{
#'      \code{(integer, 1)} Linecharts will be of lwd width.
#'   }
#'  }
#'
#'
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{ContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
