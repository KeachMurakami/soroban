photoperiod <-
  function(.tbl, time_col_name = time, day_init_end, ...){
    time_col <- enquo(time_col_name)

    turning_times <-
      day_init_end %>%
      stringr::str_pad(., width = 2, side = "left", pad = "0") %>%
      paste0("_", ., ":00:00")

    turning_points <-
      .tbl %>%
      pull(rlang::UQ(time_col)) %>%
      range %>%
      as.Date %>%
      tidyr::full_seq(period = 1) %>%
      rep(., 2) %>%
      paste0(., turning_times) %>%
      matrix(., ncol = 2, byrow = T)

    turning_points[,1] %>%
      seq_along %>%
      map(., function(i){
        temp <- lubridate::ymd_hms(turning_points[i, ])
        annotate(geom = "rect", xmin = temp[1], xmax = temp[2], ...)
      })
  }
