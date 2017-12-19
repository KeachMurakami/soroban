darkperiod <-
  function(.tbl, time_col_name = time, day_init_end, ...){
    time_col <- enquo(time_col_name)

    turning_times <-
      day_init_end %>%
      stringr::str_pad(., width = 2, side = "left", pad = "0") %>%
      paste0("_", ., ":00:00")

    dates_contained <-
      .tbl %>%
      pull(rlang::UQ(time_col)) %>%
      range %>%
      as.Date %>%
      tidyr::full_seq(period = 1)

    turning_points <-
      paste0(
        rep(dates_contained, each = length(turning_times)),
        rep(turning_times, times = length(dates_contained))
      ) %>%
      c(paste0(min(dates_contained) - 1, tail(turning_times, 1)),
        .,
        paste0(max(dates_contained) + 1, head(turning_times, 1))) %>%
      matrix(., ncol = 2, byrow = T)

    turning_points[,1] %>%
      seq_along %>%
      map(., function(i){
        temp <- lubridate::ymd_hms(turning_points[i, ])
        annotate(geom = "rect", xmin = temp[1], xmax = temp[2], ...)
      })
  }

photoperiod <-
  function(.tbl, time_col_name = time, day_init_end, ...){
    time_col <- enquo(time_col_name)

    turning_times <-
      day_init_end %>%
      stringr::str_pad(., width = 2, side = "left", pad = "0") %>%
      paste0("_", ., ":00:00")

    dates_contained <-
      .tbl %>%
      pull(rlang::UQ(time_col)) %>%
      range %>%
      as.Date %>%
      tidyr::full_seq(period = 1)


    turning_points <-
      paste0(
        rep(dates_contained, each = length(turning_times)),
        rep(turning_times, times = length(dates_contained))
      ) %>%
      matrix(., ncol = 2, byrow = T)

    turning_points[,1] %>%
      seq_along %>%
      map(., function(i){
        temp <- lubridate::ymd_hms(turning_points[i, ])
        annotate(geom = "rect", xmin = temp[1], xmax = temp[2], ...)
      })
  }

# test_data <-
#   read_csv("~/Dropbox/2017/20171213/pam/summary.csv")
#
# test_data %>%
#   ggplot(aes(time, y2, col = location_id)) +
#   photoperiod(test_data, time, day_init_end = c(7, 23), ymin = -Inf, ymax = Inf, fill = "white") +
#   geom_line() +
#   geom_errorbar(aes(ymin = y2 - y2_sd, ymax = y2 + y2_sd)) +
#   geom_point()
# test_data %>%
#   ggplot(aes(time, y2, col = location_id)) +
#   darkperiod(test_data, time, day_init_end = c(7, 23), ymin = -Inf, ymax = Inf, fill = "grey") +
#   geom_line() +
#   geom_errorbar(aes(ymin = y2 - y2_sd, ymax = y2 + y2_sd)) +
#   geom_point()
