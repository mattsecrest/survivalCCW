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

test_that("Spot check that outcomes are correctly assigned", {

  df <- data.frame(
    id = 1:6,
    event = c(1L, 1L, 1L, 1L, 1L, 1L),
    time_to_event = c(10, 100, 100, 10, 100, 100),
    exposure = c(rep(0L, 3), rep(1L, 3)),
    time_to_exposure = c(rep(NA_real_, 3), 2, 8, 12)
  )

  ccw_df <- create_clones(df,
                          id = "id", 
                          event = "event", 
                          time_to_event = "time_to_event", 
                          exposure = "exposure", 
                          time_to_exposure = "time_to_exposure", 
                          ced_window = 20)

  # Exposed clones
  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 1, "outcome"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 1, "fup_outcome"],
    10
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 1, "outcome"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 1, "fup_outcome"],
    10
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 1, "outcome"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 1, "fup_outcome"],
    20
  )

  # Unexposed clones
  # Exposed clones
  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 0, "outcome"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 0, "fup_outcome"],
    2
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 0, "outcome"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 0, "fup_outcome"],
    10
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 0, "outcome"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 0, "fup_outcome"],
    100
  )

})


test_that("Spot check that censoring statuses are correctly assigned",{

  df <- data.frame(
    id = 1:6,
    event = c(1L, 1L, 1L, 1L, 1L, 1L),
    time_to_event = c(10, 100, 100, 10, 100, 100),
    exposure = c(rep(0L, 3), rep(1L, 3)),
    time_to_exposure = c(rep(NA_real_, 3), 2, 8, 12)
  )

  ccw_df <- create_clones(df,
                          id = "id", 
                          event = "event", 
                          time_to_event = "time_to_event", 
                          exposure = "exposure", 
                          time_to_exposure = "time_to_exposure", 
                          ced_window = 20)

  # Exposed clones
  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 1, "censor"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 1, "fup_censor"],
    2
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 1, "censor"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 1, "fup_censor"],
    10
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 1, "censor"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 1, "fup_censor"],
    20
  )

  # Unexposed clones
  # Exposed clones
  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 0, "censor"],
    1L
  )

  expect_equal(
    ccw_df[ccw_df$id==4 & ccw_df$clone == 0, "fup_censor"],
    2
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 0, "censor"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==1 & ccw_df$clone == 0, "fup_censor"],
    10
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 0, "censor"],
    0L
  )

  expect_equal(
    ccw_df[ccw_df$id==2 & ccw_df$clone == 0, "fup_censor"],
    20
  )

})

test_that("attribute are passed correctly", {

  df <- data.frame(
    id = 1:6,
    event = c(1L, 1L, 1L, 1L, 1L, 1L),
    time_to_event = c(10, 100, 100, 10, 100, 100),
    exposure = c(rep(0L, 3), rep(1L, 3)),
    time_to_exposure = c(rep(NA_real_, 3), 2, 8, 12)
  )

  ccw_df <- create_clones(df,
                          id = "id", 
                          event = "event", 
                          time_to_event = "time_to_event", 
                          exposure = "exposure", 
                          time_to_exposure = "time_to_exposure", 
                          ced_window = 20)

  expect_equal(
    attributes(ccw_df)$id,
    "id"
  )

  expect_equal(
    attributes(ccw_df)$event,
    "event"
  )

  expect_equal(
    attributes(ccw_df)$time_to_event,
    "time_to_event"
  )

  expect_equal(
    attributes(ccw_df)$exposure,
    "exposure"
  )

  expect_equal(
    attributes(ccw_df)$time_to_exposure,
    "time_to_exposure"
  )

  expect_equal(
    attributes(ccw_df)$ced_window,
    20
  )

})