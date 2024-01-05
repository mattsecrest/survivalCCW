test_that("input types correct", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = id, event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = df$event, time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = 2.0, exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event",  exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = "ced_window")
  )

})

test_that("columns are in data", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "hamburger", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "sausage", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "pumpkin", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("no missing data exists in these columns (other than time to exposure)", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, NA_integer_, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("the data are one-row-per-patient", {
  
    df <- data.frame(
      id = c(1, 1, 2, 2),
      event = c(0L, 1L, 0L, 1L),
      time_to_event = c(100, 200, 100, 200),
      exposure = c(0L, 1L, 0L, 1L),
      time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
    )
  
    expect_error(
      create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
    )

    df <- data.frame(
      id = c(1.1123, 1.1124, 1.1123, 4),
      event = c(0L, 1L, 0L, 1L),
      time_to_event = c(100, 200, 100, 200),
      exposure = c(0L, 1L, 0L, 1L),
      time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
    )

    expect_error(
      create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
    )

  
})

test_that("the same column name is not passed >1 time", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("the respective columns have the right classes", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c('ham', 'is', 'good', 'food'),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1.0, 1.0, 0.0, 1.0),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(1.0, 1.0, 0.0, 1.0),
    time_to_exposure = c(2.2, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )


  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c('ham', 'is', 'great', 'food'),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = factor(c('ham', 'is', 'great', 'food'))
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("for all pts with an exposure, time to exposure is complete", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, NA_real_, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, 9.4, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("protected column names are blocked", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    clone = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "clone", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    censor = c(T, T, F, T),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "censor", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("exposure and event are just 0/1 or T/F", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 2L, 3L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(1L, 1L, 0L, 1L),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 0L, 0L, 0L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 2L, 3L),
    time_to_exposure = c(NA_real_, 2.2, 3.3, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)
  )

})

test_that("No outcomes before exposure dates", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(1L, 1L, 0L, 0L),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(1L, 1L, 0L, 1L),
    time_to_exposure = c(1.1, 3.3, NA_real_, 3.3)
  )

  expect_error(
    create_clones_check_inputs(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 1.0)
  )

})