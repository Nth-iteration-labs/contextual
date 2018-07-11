library(MonetDB.R)
YahooBandit <- R6::R6Class(
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    class_name = "YahooBandit",
    con = NULL,
    arm_choice = NULL,
    click = NULL,
    buffer = NULL,
    cache = NULL,
    arm_lookup = NULL,
    initialize   = function(k, d, arm_lookup, cache = 500 ) {
      self$k <- k
      self$d <- d
      self$cache <- cache
      self$arm_lookup <- arm_lookup
    },
    pre_calculate = function() {
      self$con <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname="yahoo", user="monetdb", password="monetdb")
      self$buffer <-
        as.matrix(DBI::dbGetQuery(
          self$con,
          paste0(
            "SELECT * FROM yahoo WHERE t BETWEEN 1 AND ",
            self$cache - 1,
            " LIMIT ",
            self$cache - 1
          )
        ))
    },
    get_context = function(index) {
      if (index%%(self$cache) == 0) {
        self$buffer <- as.matrix(DBI::dbGetQuery(
          self$con,
          paste0(
            "SELECT * FROM yahoo WHERE t BETWEEN ",
            index,
            " AND ",
            index + self$cache - 1,
            " LIMIT ",
            self$cache
          )
        ))
        print(index)
      }
      row <- self$buffer[as.integer(self$buffer[,185]) == index,]

      self$arm_choice          <- which(self$arm_lookup %in% row[2])
      self$click               <- row[3]

      # get article ids

      a_index                  <- seq(10, 184, by = 7)
      article_ids              <- row[a_index]
      article_ids              <- article_ids[!is.na(article_ids)]
      nr_articles              <- length(article_ids)
      arm_nrs                  <- which(self$arm_lookup %in% article_ids)

      # get article context

      a_values                 <- setdiff(seq(10, 184, by = 1),a_index)
      article_context          <- row[a_values]
      article_context          <- article_context[!is.na(article_context)]

      article_context          <- matrix(article_context,nrow=6)

      user_context             <- matrix(as.numeric(row[4:9]), nrow =6, ncol = nr_articles)

      X                        <- rbind2(article_context,user_context)

      class(X)                 <- "numeric"

      contextlist <- list(
        k = self$k,
        d = self$d,
        arms = arm_nrs,
        X = X
      )

      contextlist
    },
    get_reward = function(index, context, action) {
      if (self$arm_choice == action$choice) {
        list(
          reward = as.integer(self$click)
        )
      } else {
        NULL
      }
    },
    close = function() {
      DBI::dbDisconnect(self$con)
    }
  )
)
