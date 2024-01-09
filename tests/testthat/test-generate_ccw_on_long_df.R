test_that("casting clones requires ccw_clones_long class", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    generate_ccw_on_long_df(df)
  )

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  attributes(df_long)$id <- NULL

  expect_error(
    generate_ccw_on_long_df(df_long)
  )

  df_long$t_start <- NULL

  expect_error(
    generate_ccw_on_long_df(df_long)
  )
})

test_that("when predvars columns are missing, an error is thrown", {

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw_on_long_df(df_long, predvars = "hamburger")
  )

  expect_error(
    generate_ccw_on_long_df(df_long, predvars = NULL)
  )

})

test_that("categorical vars are dealt with", {

  toy_df_s <- toy_df
  toy_df_s$sandwich <- rep(c("ham", "turkey", "cheese"), length.out = NROW(toy_df_s))

  df_long <- toy_df_s |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw_on_long_df(df_long, predvars = "sandwich")
  )

  df_long$sandwich_f <- factor(df_long$sandwich)

  expect_error(
    generate_ccw_on_long_df(df_long, predvars = "sandwich_f")
  )

  expect_error(
    generate_ccw_on_long_df(df_long, predvars = NULL)
  )

})

test_that("weights are adequately calculated compared to Maringe", {
  
  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 182.62) |>
    cast_clones_to_long()

  id <- attributes(df)$id
  event <- attributes(df)$event
  exposure <- attributes(df)$exposure
  time_to_event <- attributes(df)$time_to_event
  time_to_exposure <- attributes(df)$time_to_exposure
  ced_window <- attributes(df)$ced_window
  event_times_df <- attributes(df)$event_times_df

  # Create weights
  predvars <- c("age", "sex", "perf", "stage", "deprivation", "charlson", "emergency")

  df_1 <- generate_ccw_calc_weights(df[df$clone == 1L, ], event_times_df, predvars)
  df_1 <- df_1[order(df_1$id, df_1$time_id),]
  row.names(df_1) <- NULL

  df_0 <- generate_ccw_calc_weights(df[df$clone == 0L, ], event_times_df, predvars)
  df_0 <- df_0[order(df_0$id, df_0$time_id),]
  row.names(df_0) <- NULL

  # Compare exposed
  load(system.file("tests/testthat/data/data_long_maringe.RData", package = "survivalCCW"))
  data_long <- data_long[order(data_long$id, data_long$time_id),]
  row.names(data_long) <- NULL

  load(system.file("tests/testthat/data/data_long_2_maringe.RData", package = "survivalCCW"))
  data_long_2 <- data_long_2[order(data_long_2$id, data_long_2$time_id),]
  row.names(data_long_2) <- NULL

  # Compare all columns
  for (col in c("time_id", "lp", "t", "hazard")) {
    row.names(df_1[[col]]) <- NULL
    row.names(data_long[[col]]) <- NULL
    expect_equal(
      df_1[[col]],
      data_long[[col]],
      tolerance = 0.001
    )
  }

  # Compare all columns
  for (col in c("time_id", "lp", "t", "hazard")) {
    row.names(df_0[[col]]) <- NULL
    row.names(data_long_2[[col]]) <- NULL
    expect_equal(
      df_0[[col]],
      data_long_2[[col]],
      tolerance = 0.001
    )
  }
  
})

