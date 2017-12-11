trace <-
  function(.tbl, column, n, skip = 0, fun = "mean"){
    target <-
      rlang::enquo(column)
    target_init <-
      paste0("init_", quo_name(target))
    target_delta <-
      paste0("delta_", quo_name(target))
    target_ratio <-
      paste0("ratio_", quo_name(target))

    fun <- rlang::sym(fun)

    rows_extracted <-
      tail(1:(n+skip), n)

    values <-
      .tbl %>%
      dplyr::slice(rows_extracted) %>%
      dplyr::pull(!! target)

    .tbl %>%
      mutate(!!target_init := (!!fun)(values),
             !!target_delta := (!!target) - (!!rlang::sym(target_init)),
             !!target_ratio := (!!target) / (!!rlang::sym(target_init)))
  }
