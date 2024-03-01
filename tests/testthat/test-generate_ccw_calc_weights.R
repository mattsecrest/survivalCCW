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

  df_1 <- calc_weights(df[df$clone == 1L, ], event_times_df, predvars)
  df_1 <- df_1[order(df_1$id, df_1$time_id),]
  row.names(df_1) <- NULL

  df_0 <- calc_weights(df[df$clone == 0L, ], event_times_df, predvars)
  df_0 <- df_0[order(df_0$id, df_0$time_id),]
  row.names(df_0) <- NULL

  # Compare exposed
  data_long <- data_long[order(data_long$id, data_long$time_id),]
  row.names(data_long) <- NULL

  data_long_2 <- data_long_2[order(data_long_2$id, data_long_2$time_id),]
  row.names(data_long_2) <- NULL

  # Compare all columns
  for (col in c("time_id", "lp", "t", "hazard", "p_uncens", "weight_cox")) {
    row.names(df_1[[col]]) <- NULL
    row.names(data_long[[col]]) <- NULL
    expect_equal(
      df_1[[col]],
      data_long[[col]],
      tolerance = 1e-6
    )
  }

  # Compare all columns
  for (col in c("time_id", "lp", "t", "hazard", "p_uncens", "weight_cox")) {
    row.names(df_0[[col]]) <- NULL
    row.names(data_long_2[[col]]) <- NULL
    expect_equal(
      df_0[[col]],
      data_long_2[[col]],
      tolerance = 1e-6
    )
  }
  
})

test_that("plus signs are caught in column names", {
  
  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c('ham', 'is', 'good', 'food'),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    calc_weights(df, event_times_df, predvars = "hamburger+"),
    "No plus signs allowed in column names -- this impacts the formula creation"
  )

})
