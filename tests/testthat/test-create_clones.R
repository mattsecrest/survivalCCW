test_that("CED trims time to exposure appropriately", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(200L, 250L, 9900L, 100L),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_message(
    ccw_df <- create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 2.5),
    "Updating 1 patients' exposure and time-to-exposure based on CED window"
  )

  expect_equal(0L, unique(ccw_df[ccw_df$id==4, "exposure"]))

})
