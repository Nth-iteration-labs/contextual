#' @export
Plot <- R6::R6Class(
  "Plot",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    history = NULL,

    cumulative = function(history,

                          regret             = TRUE,
                          ci                 = NULL,
                          plot_only_ci       = FALSE,
                          rate               = FALSE,
                          interval           = 1,
                          traces             = FALSE,
                          traces_max         = 100,
                          traces_alpha       = 0.3,
                          smooth             = FALSE,
                          no_par             = FALSE,
                          xlim               = NULL,
                          ylim               = NULL,
                          legend             = TRUE,
                          use_colors         = TRUE,
                          color_step         = 1,
                          lty_step           = 1,
                          lwd                = 1,
                          legend_labels      = NULL,
                          legend_border      = NULL,
                          legend_position    = "topleft",
                          legend_title       = NULL,
                          limit_agents       = NULL) {

      self$history       <- history

      if (regret) {
        if (rate) {
          ylab_title     <- "Cumulative regret - rate"
          line_data_name <- "cum_regret_rate"
          ci_data_name   <- "cum_regret_rate_none"
        } else {
          ylab_title     <- "Cumulative expected regret"
          line_data_name <- "cum_regret"
          ci_data_name   <- "cum_regret_none"
        }
      } else {
        if (rate) {
          ylab_title     <- "Cumulative reward - rate"
          line_data_name <- "cum_reward_rate"
          ci_data_name   <- "cum_reward_rate_none"
        } else {
          ylab_title     <- "Cumulative reward"
          line_data_name <- "cum_reward"
          ci_data_name   <- "cum_reward_none"
        }
      }

      private$do_plot(
        line_data_name      = line_data_name,
        ci_data_name        = ci_data_name,
        ylab_title          = ylab_title,
        use_colors          = use_colors,
        legend              = legend,
        ci                  = ci,
        plot_only_ci        = plot_only_ci,
        no_par              = no_par,
        interval            = interval,
        color_step          = color_step,
        lty_step            = lty_step,
        lwd                 = lwd,
        xlim                = xlim,
        ylim                = ylim,
        legend_labels       = legend_labels,
        legend_border       = legend_border,
        legend_position     = legend_position,
        legend_title        = legend_title,
        limit_agents        = limit_agents,
        traces              = traces,
        traces_max          = traces_max,
        traces_alpha        = traces_alpha,
        smooth              = smooth
      )

      invisible(self)
    },

    average = function(history,
                       regret             = TRUE,
                       ci                 = NULL,
                       plot_only_ci       = FALSE,
                       rate               = FALSE,
                       interval           = 1,
                       traces             = FALSE,
                       traces_max         = 100,
                       traces_alpha       = 0.3,
                       smooth             = FALSE,
                       no_par             = FALSE,
                       xlim               = NULL,
                       ylim               = NULL,
                       legend             = TRUE,
                       use_colors         = TRUE,
                       color_step         = 1,
                       lty_step           = 1,
                       lwd                = 1,
                       legend_labels      = NULL,
                       legend_border      = NULL,
                       legend_position    = "topleft",
                       legend_title       = NULL,
                       limit_agents       = NULL) {
      self$history <- history

      if (regret) {
        ylab_title     <- "Average regret"
        line_data_name <- "regret"
        ci_data_name   <- "regret_none"
      } else {
        ylab_title     <- "Average reward"
        line_data_name <- "reward"
        ci_data_name   <- "reward_none"
      }

      private$do_plot(
        line_data_name      = line_data_name,
        ci_data_name        = ci_data_name,
        ylab_title          = ylab_title,
        use_colors          = use_colors,
        legend              = legend,
        ci                  = ci,
        plot_only_ci        = plot_only_ci,
        no_par              = no_par,
        interval            = interval,
        color_step          = color_step,
        lty_step            = lty_step,
        lwd                 = lwd,
        xlim                = xlim,
        ylim                = ylim,
        legend_labels       = legend_labels,
        legend_border       = legend_border,
        legend_position     = legend_position,
        legend_title        = legend_title,
        limit_agents        = limit_agents,
        traces              = traces,
        traces_max          = traces_max,
        traces_alpha        = traces_alpha,
        smooth              = smooth
      )

      invisible(self)
    },

    arms = function(history,

                    no_par             = FALSE,
                    legend             = TRUE,
                    use_colors         = TRUE,
                    interval           = 1,
                    xlim               = NULL,
                    ylim               = NULL,
                    legend_labels      = NULL,
                    legend_border      = NULL,
                    legend_position    = "topleft",
                    legend_title       = NULL,
                    limit_agents       = NULL) {

      self$history <- history

      if (no_par == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }

      dt <- self$history$get_data_table(
        limit_cols   = c("agent", "t", "choice", "sim"),
        limit_agents = limit_agents,
        interval     = interval
      )

      ylab_title        <- "Arm choice %"
      agent_levels      <- levels(as.factor(dt$agent))

      if (length(agent_levels) > 1) {
        warning(strwrap(
          prefix = " ", initial = "",
          "## Arm percentage plot always plots the results of one agent, either at
          index position one, or the agent specified in limit_agents."
        ),
        call. = FALSE
        )
      }

      dt                <- dt[agent == agent_levels[1]]
      data              <- dt[, list(arm_count = .(rowCount = .N)), by = list(t, choice)]
      max_sim           <- dt[, max(sim)]
      max_t             <- dt[, max(t)]
      arm_levels        <- levels(as.factor(data$choice))
      max_arm           <- length(arm_levels)
      data$arm_count    <- as.double((unlist(data$arm_count, FALSE, FALSE) / max_sim) * 100L)
      eg                <- expand.grid(t = dt[sim == 1]$t, choice = seq(1.0, max_arm, 1))
      data              <- merge(data, eg, all = TRUE)
      data[is.na(data)] <- 0.0
      data$dataum       <- ave(data$arm_count, data$t, FUN = cumsum)
      data$zero         <- 0.0
      min_ylim          <- 0
      max_ylim          <- 100

      data.table::setorder(data, choice, t)
      plot.new()

      if (!is.null(xlim)) {
        min_xlim <- xlim[1]
        max_xlim <- xlim[2]
      } else {
        min_xlim <- 1
        max_xlim <- data[, max(t)]
      }
      if (!is.null(ylim)) {
        min_ylim <- ylim[1]
        max_ylim <- ylim[2]
      }
      plot.window(
        xlim = c(min_xlim, max_xlim),
        ylim = c(min_ylim, max_ylim)
      )

      if (isTRUE(use_colors)) {
        cl <- private$gg_color_hue(length(arm_levels))
      } else {
        cl <- gray.colors(length(arm_levels))
      }

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
          col = adjustcolor(cl, alpha.f = 0.6),
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
    }
  ),
  private = list(
    do_plot = function(line_data_name      = line_data_name,
                       ci_data_name        = ci_data_name,
                       ci                  = NULL,
                       plot_only_ci        = FALSE,
                       ylab_title          = NULL,
                       use_colors          = FALSE,
                       legend              = TRUE,
                       no_par              = FALSE,
                       xlim                = NULL,
                       ylim                = NULL,
                       interval            = 1,
                       color_step          = 1,
                       lty_step            = 1,
                       lwd                 = 1,
                       legend_labels       = NULL,
                       legend_border       = NULL,
                       legend_position     = "topleft",
                       legend_title        = NULL,
                       limit_agents        = NULL,
                       traces              = NULL,
                       traces_max          = 100,
                       traces_alpha        = 0.3,
                       smooth              = FALSE) {

      if (!is.null(ci) && ci %in% c("sd", "var", "ci")) {

        ci_data_name <- gsub("none", ci, ci_data_name)
        data <-
          self$history$get_cumulative_data(
            limit_cols   = c("agent", "t", line_data_name, ci_data_name),
            limit_agents = limit_agents,
            interval     = interval
          )

      } else {
        ci <- NULL
        data <-
          self$history$get_cumulative_data(
            limit_cols   = c("agent", "t", line_data_name),
            limit_agents = limit_agents,
            interval     = interval
          )
      }

      if (interval==1 && strtoi(self$history$meta$simulations) > 2000) {
        interval <- ceiling(strtoi(self$history$meta$simulations)/2000)
      }

      data.table::setorder(data, agent, t)

      agent_levels <- levels(as.factor(data$agent))
      n_agents <- length(agent_levels)

      if (isTRUE(smooth)) {
        for (agent_name in agent_levels) {
          data[data$agent == agent_name, c("t", line_data_name) :=
            supsmu(data[data$agent == agent_name]$t, data[data$agent == agent_name][[line_data_name]])]
          if (!is.null(ci)) {
            data[data$agent == agent_name, c("t", ci_data_name) :=
              supsmu(data[data$agent == agent_name]$t, data[data$agent == agent_name][[ci_data_name]])]
          }
        }
      }

      if (no_par == FALSE) {
        dev.hold()
        old.par <- par(no.readonly = TRUE)
        par(mar = c(5, 5, 1, 1))
      }

      if (!is.null(ci) && !isTRUE(plot_only_ci)) {
        ci_range <- data[[line_data_name]] + outer(data[[ci_data_name]], c(1, -1))
        data     <- cbind(data, ci_range)
        colnames(data)[colnames(data) == "V2"] <- "ci_lower"
        colnames(data)[colnames(data) == "V1"] <- "ci_upper"
      }

      if (isTRUE(plot_only_ci)) {
        if(is.null(ci)) stop("Need to set ci to 'var','sd' or 'ci' when plot_only_ci is TRUE", call. = FALSE)
        line_data_name = ci_data_name
      }

      plot.new()
      cl <- private$gg_color_hue(round(n_agents / color_step))
      cl <- rep(cl, round(color_step))
      lt <- rep(1, n_agents)
      if (lty_step > 1) {
        lt <- rep(1:round(lty_step), each = round(n_agents / lty_step))
      }
      if (use_colors == FALSE) {
        lt <- seq(1, n_agents)
      }
      if (!is.null(ci) && !isTRUE(plot_only_ci)) {
        min_ylim <- data[, min(ci_lower)]
        max_ylim <- data[, max(ci_upper)]
      } else {
        min_ylim <- data[, min(data[[line_data_name]])]
        max_ylim <- data[, max(data[[line_data_name]])]
      }
      if (!is.null(xlim)) {
        min_xlim <- xlim[1]
        max_xlim <- xlim[2]
      } else {
        min_xlim <- 1
        max_xlim <- data[, max(t)]
      }
      if (!is.null(ylim)) {
        min_ylim <- ylim[1]
        max_ylim <- ylim[2]
      }
      plot.window(
        xlim = c(min_xlim, max_xlim),
        ylim = c(min_ylim, max_ylim)
      )

      if (isTRUE(traces) && !isTRUE(plot_only_ci)) {
        dt <- self$history$get_data_table(limit_agents = limit_agents, interval = interval)
        data.table::setorder(dt, agent, sim, t)
        for (agent_name in agent_levels) {
          agent_sims <- unique(dt[dt$agent == agent_name]$sim)
          for (as in head(agent_sims, traces_max)) {
            Sys.sleep(0)
            if (isTRUE(smooth)) {
              lines(supsmu(
                dt[dt$agent == agent_name & dt$sim == as]$t,
                dt[dt$agent == agent_name & dt$sim == as][[line_data_name]]
              ),
              lwd = lwd,
              col = rgb(0.8, 0.8, 0.8, traces_alpha)
              )
            } else {
              lines(dt[dt$agent == agent_name & dt$sim == as]$t,
                dt[dt$agent == agent_name &
                  dt$sim == as][[line_data_name]],
                lwd = lwd,
                col = rgb(0.8, 0.8, 0.8, traces_alpha)
              )
            }
          }
        }
      }

      if (isTRUE(use_colors)) {
        if (!is.null(ci) && !isTRUE(plot_only_ci)) {
          color <- 1
          for (agent_name in agent_levels) {
            polygon(
              c(data[data$agent == agent_name]$t, rev(data[data$agent == agent_name]$t)),
              c(data[data$agent == agent_name]$ci_lower, rev(data[data$agent == agent_name]$ci_upper)),
              col = adjustcolor(cl[color], alpha.f = 0.3),
              border = NA
            )
            color <- color + 1
          }
        }
        line_counter <- 1
        for (agent_name in agent_levels) {
          lines(
            data[data$agent == agent_name]$t,
            data[data$agent == agent_name][[line_data_name]],
            lwd  = lwd,
            lty  = lt[line_counter],
            col  = adjustcolor(cl[line_counter], alpha.f = 0.9),
            type = "l"
          )
          line_counter <- line_counter + 1
        }
      } else {
        line_counter <- 1
        for (agent_name in agent_levels) {
          if (!is.null(ci) && !isTRUE(plot_only_ci)) {
            polygon(
              c(data[data$agent == agent_name]$t, rev(data[data$agent == agent_name]$t)),
              c(data[data$agent == agent_name]$ci_lower, rev(data[data$agent == agent_name]$ci_upper)),
              col = rgb(0.8, 0.8, 0.8, 0.4),
              border = NA
            )
          }
          lines(
            data[data$agent == agent_name]$t,
            data[data$agent == agent_name][[line_data_name]],
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
      if(isTRUE(plot_only_ci)) ylab_title <- paste0(ylab_title,": ",ci)
      title(ylab = ylab_title)
      box()
      if (legend) {
        if (!is.null(legend_labels)) {
          agent_levels <- legend_labels
        }
        if (!is.null(legend_border)) {
          bty <- "n"
        } else {
          bty <- "o"
        }
        if (use_colors == FALSE) {
          cl <- rgb(0.2, 0.2, 0.2, 0.8)
        }
        legend(
          legend_position,
          NULL,
          agent_levels,
          col   = cl,
          title = legend_title,
          lwd   = lwd,
          lty   = lt,
          bty   = bty,
          bg    = "white"
        )
      }
      if (no_par == FALSE) {
        dev.flush()
        par(old.par)
      }
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
#'   \item{\code{limit_agents}}{
#'      \code{(list , NULL)} Limit the plotted agents to the agents in list.
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
#'   \item{\code{ylim}}{
#'      \code{(c(integer,integer), NULL)} Set y-axis limits.
#'   }
#'   \item{\code{use_colors}}{
#'      \code{(logical, TRUE)} If use_colors is set to FALSE, plots will be in grayscale.
#'      Otherwise, plots will make use of a color palette.
#'   }
#'   \item{\code{ci}}{
#'      \code{(logical, FALSE)} When ci is TRUE, Plot() will display 95% confidence intervals.
#'   }
#'   \item{\code{traces}}{
#'      \code{(logical , FALSE)} When traces is TRUE, Plot() will plot the traces of each independent simulation.
#'   }
#'   \item{\code{traces_max}}{
#'      \code{(integer , 100)} The maximum number of trace lines.
#'   }
#'   \item{\code{traces_alpha}}{
#'      \code{(numeric , 0.3)} Sets the opacity of all trace lines. By default traces_alpha is set to 0.3,
#'      that is, at an opacity of 30%.
#'   }
#'   \item{\code{smooth}}{
#'      \code{(logical , FALSE)} When smooth is TRUE, Plot() will smooth all plots.
#'   }
#'   \item{\code{interval}}{
#'      \code{(integer, NULL)} Plot only for every every t%%interval==0
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
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{BasicContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
