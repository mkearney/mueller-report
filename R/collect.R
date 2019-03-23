library(tfse)
library(rtweet)

search_query <- paste0(c(
  "mueller",
  '"Attorney General"',
  "barr",
  "(trump investigation)"
), collapse = " OR ")


mid <- NULL
sid <- NULL
options(scipen = 10)

secs_to_reset <- function() {
  st <- rate_limit(bearer_token())
  ra <- dplyr::filter(st, grepl("search/tweets", query))$reset_at
  s <- as.numeric(difftime(ra, Sys.time()), "secs")
  if (s < 0) {
    s <- 0
  }
  s
}

for (i in seq_len(100)) {
  ## skip the first
  if (i > 1) {
    ## sleep if necessary
    s <- secs_to_reset()
    if (s > 0) {
      tfse::print_start("Sleeping for " %P% round(s / 60, 0) %P% " mins...")
      Sys.sleep(s)
    }
    ## conduct a bearer-token search
    d <- search_tweets(search_query, n = 45000, token = bearer_token(),
      since_id = sid)

    ## print, update, and save
    tfse::print_complete("Collected " %P% nrow(d) %P% " tweets!")
    sid <- since_id(d)
    tfse::save_RDS(d, here::here("data",
      "mueller-report-" %P% gsub("\\D", "", Sys.time()) %P% ".rds"))
  }

  ## if 40k+ are collected, keep searching for older
  if (nrow(d) >= 40000) {
    while (nrow(d) >= 40000) {
      ## set max ID value
      mid <- max_id(d)

      ## sleep if necessary
      s <- secs_to_reset()
      if (s > 0) {
        tfse::print_start("Sleeping for " %P% round(s / 60, 0) %P% " mins...")
        Sys.sleep(s)
      }
      ## conduct follow up search
      d <- search_tweets(search_query, n = 45000, token = bearer_token(),
        max_id = mid)
      ## save!
      tfse::save_RDS(d, here::here("data",
        "mueller-report-" %P% gsub("\\D", "", Sys.time()) %P% ".rds"))
    }
  }
}
