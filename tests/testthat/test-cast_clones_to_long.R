test_that("casting clones requires ccw_clones class", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    cast_clones_to_long(df)
  )

  ccw_df <- create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)

  attributes(ccw_df)$id <- NULL

  expect_error(
    cast_clones_to_long(ccw_df)
  )

  ccw_df$outcome <- NULL

  expect_error(
    cast_clones_to_long(ccw_df)
  )


})
