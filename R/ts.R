library(tfse)



read_vars <- function(x) {
  d <- readRDS(x)
  tfse::print_complete(x)
  d[, c("status_id", "created_at", "user_id", "screen_name", "is_retweet")]
}

fs::dir_ls("~/R/mueller-report/data") %>%
#  fs::file_info() %>%
#  dplyr::arrange(dplyr::desc(birth_time)) %>%
#  dplyr::slice(1:3) %>%
#  dplyr::pull(path) %>%
  purrr::map(read_vars) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(!duplicated(status_id)) -> d

gc()

random_sample <- function(x, n = 10000) {
  rows <- sort(sample(seq_len(nrow(x)), n))
  x[rows, ]
}
format_datetime <- function(x) sub("^0", "", tolower(format(x, "%I%p")))

d %>%
  dplyr::filter(created_at >= "2019-03-22 06:00:00",
    created_at < "2019-03-23 05:00:00") %>%
  dplyr::mutate(created_at = as.POSIXct(created_at - 60 * 60 * 5,
    tz = Sys.timezone())) %>%
  rtweet::ts_plot("15 mins", size = 1.0, trim = 1, tz = Sys.timezone()) +
  dataviz::theme_mwk(base_size = 18) +
  ggplot2::labs(title = "News of Mueller report hits Twitter",
    subtitle = "Time series of tweets in 15 minute intervals",
    x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) prettyNum(x, big.mark = ",")) +
  ggplot2::scale_x_datetime(labels = format_datetime, date_breaks = "3 hours") +
  ggplot2::ggsave("~/Desktop/mueller-report.png")
