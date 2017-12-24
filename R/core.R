trace <-
  function(.tbl, y_reg, x_reg, n, skip, method = "reg", ...){
    target <-
      rlang::enquo(y_reg)
    regressor <-
      rlang::enquo(x_reg)

    target_init <-
      paste0("init_", quo_name(target))
    target_delta <-
      paste0("delta_", quo_name(target))
    target_ratio <-
      paste0("ratio_", quo_name(target))

    rows_extracted <-
      tail(1:(n+skip), n)

    y_values <-
      .tbl %>%
      dplyr::slice(rows_extracted) %>%
      dplyr::pull(!! target)

    x_values <-
      .tbl %>%
      dplyr::slice(rows_extracted) %>%
      dplyr::pull(!! regressor)


    initial_value <-
      data_frame(
        x = x_values,
        y = y_values) %>%
      lm(data = ., y ~ x, ...) %>%
      tidy %$%
      estimate %>%
      .[1]

    if(method == "mean"){
      initial_value <-
        y_values %>%
        mean
    }

    .tbl %>%
      mutate(!!target_init := initial_value,
             !!target_delta := (!!target) - (!!rlang::sym(target_init)),
             !!target_ratio := (!!target) / (!!rlang::sym(target_init)))
  }


split_group <-
  function(.tbl){
    spliter <-
      .tbl %>%
      group_vars %>%
      paste0("rlang::UQ(", ., ")") %>%
      stringr::str_c(collapse = ", ") %>%
      paste0("paste0(", ., ")")

    .tbl %>%
      dplyr::mutate(`_spliter` = rlang::eval_tidy(rlang::parse_expr(spliter))) %>%
      split(.$`_spliter`) %>%
      purrr::map(~ dplyr::select(., -`_spliter`))
  }


### demo

# ddd <-
#   data_frame(time = 0:999,
#              var1 = 50 + seq(0, 1000, length.out = 1000) + rnorm(1000, 0, 50),
#              var2 = 100 + seq(0, 500, length.out = 1000) + rnorm(1000, 0, 10)) %>%
#   gather(variable, y, -time) %>%
#   mutate(another_group = sample(letters[1:3], 2000, replace = T)) %>%
#   group_by(variable, another_group)
#
# ddd %>%
#   mutate(spliter = paste0(variable, another_group)) %>%
#   split(.$spliter) %>%
#   map_df(~ trace(., y, time, n = 500, skip = 0, method = "reg")) %>%
#   ggplot(aes(time, delta_y, col = variable)) +
#   geom_point(alpha = .25)
#
# ddd %>%
#   mutate(spliter = paste0(variable, another_group)) %>%
#   split(.$spliter) %>%
#   map_df(~ trace(., y, time, n = 500, skip = 0, method = "mean")) %>%
#   ggplot(aes(time, delta_y, col = variable)) +
#   geom_point(alpha = .25)
