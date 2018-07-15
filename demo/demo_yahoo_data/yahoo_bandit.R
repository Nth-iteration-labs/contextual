YahooBandit <- R6::R6Class(
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    class_name = "YahooBandit",
    con = NULL,
    chosen_arm = NULL,
    click_reward = NULL,
    buffer = NULL,
    cache = NULL,
    arm_lookup = NULL,
    initialize   = function(k, d, arm_lookup, cache = 500) {
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
          paste0("SELECT * FROM yahoo WHERE t BETWEEN 1 AND ",self$cache - 1," LIMIT ",self$cache - 1,";"
          )
        ))
    },
    get_context = function(index) {
      if (index%%(self$cache) == 0) {
        self$buffer <- as.matrix(DBI::dbGetQuery(
          self$con,
          paste0(
            "SELECT * FROM yahoo WHERE t BETWEEN ",index," AND ",index + self$cache - 1," LIMIT ",self$cache,";"
          )
        ))
      }
      row <- self$buffer[as.integer(self$buffer[,185]) == index,]

      self$chosen_arm          <- which(self$arm_lookup == as.integer(row[2]))
      self$click_reward        <- as.integer(row[3])

      # get article ids, translate to lookup

      a_index                  <- seq(10, 184, by = 7)
      article_ids              <- row[a_index]
      article_ids              <- article_ids[!is.na(article_ids)]
      article_ids              <- match(article_ids,self$arm_lookup)
      nr_articles              <- length(article_ids)

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
        arms = article_ids,
        X = X
      )

      contextlist
    },
    get_reward = function(index, context, action) {

      if (self$chosen_arm == action$choice) {
        list(
          reward = as.integer(self$click_reward)
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
