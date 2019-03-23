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
  if (dplyr::filter(st, grepl("search/tweets", query))$remaining > 400) {
    return(0)
  }
  ra <- dplyr::filter(st, grepl("search/tweets", query))$reset_at
  s <- as.numeric(difftime(ra, Sys.time()), "secs")
  if (s < 0) {
    s <- 0
  }
  s
}

format_timestamp <- function(x) {
  x <- sub("AM", "am", sub("PM", "pm", format(x, "%H:%M%p (%b %d)")))
  gsub("[ ]?0(?=\\d)", " ", x, perl = TRUE)
}
pnum <- function(x) {
  if (getOption("scipen") < 1) {
    options(scipen = 4)
  }
  prettyNum(x, big.mark = ",")
}
n_row <- function(x) {
  pnum(nrow(x))
}
uq_users <- function(x) {
  pnum(tfse::n_uq(x$user_id))
}
print_summary <- function(d) {
  tfse::print_complete("Done collecting tweets!")
  cat("  - Tweets : " %P% n_row(d) %P% "\n")
  cat("  - Users  : " %P% uq_users(d) %P% "\n")
  cat("  - Retweet: " %P% pnum(sum(d$is_retweet)) %P% "\n")
  cat("  - Reply  :  " %P% pnum(sum(!is.na(d$reply_to_status_id))) %P% "\n")
  cat("  - Quote  :    " %P% pnum(sum(d$is_quote)) %P% "\n")
  cat("  - Tw/Usr :      " %P% pnum(round(nrow(d) / tfse::n_uq(d$user_id), 1)) %P% "\n")
  cat("  - Newest : " %P% format_timestamp(max(d$created_at)) %P% "\n")
  cat("  - Oldest : " %P% format_timestamp(min(d$created_at)) %P% "\n")
}

for (i in seq_len(100)) {
  if (i > 1) {
    ## sleep if necessary
    s <- secs_to_reset()
    if (s > 0) {
      tfse::print_start("Sleeping for approx. " %P% ceiling(s / 60) %P% " mins...")
      Sys.sleep(s + 1)
    }
    ## conduct a bearer-token search
    d <- search_tweets(search_query, n = 45000, token = bearer_token(),
      since_id = sid)

    ## print out summary info
    print_summary(d)

    ## update since_id and save
    sid <- since_id(d)
    tfse::save_RDS(d, here::here("data",
      "mueller-report-" %P% gsub("\\D", "", Sys.time()) %P% ".rds"))
  }
  ## if 40k+ are collected, keep searching for older
  if (nrow(d) >= 40000) {
    while (nrow(d) >= 40000) {
      tfse::print_start("Some older tweets still need to be collected!")

      ## set max ID value
      mid <- max_id(d)

      ## sleep if necessary
      s <- secs_to_reset()
      if (s > 0) {
        tfse::print_start("Sleeping for " %P% ceiling(s / 60) %P% " mins...")
        Sys.sleep(s)
      }

      ## conduct follow up search
      d <- search_tweets(search_query, n = 45000, token = bearer_token(),
        max_id = mid, since_id = sid)

      ## print and save
      print_summary(d)
      tfse::save_RDS(d, here::here("data",
        "mueller-report-" %P% gsub("\\D", "", Sys.time()) %P% ".rds"))
    }
  }
}
