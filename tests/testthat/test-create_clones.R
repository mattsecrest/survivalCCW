test_that("input types correct", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0, 1, 0, 1),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(0, 1, 0, 1),
    time_to_exposure = c(1, 2, 1, 2)
  )
  expect_error(
    create_clones(df, id = id, event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

  expect_error(
    create_clones(df, id = "id", event = df$event, time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

  expect_error(
    create_clones(df, id = "id", event = "event", time_to_event = 2.0, exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

  expect_error(
    create_clones(df, id = "id", event = "event", time_to_event = "time_to_event",  exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = "ced_window")
  )

})

test_that("columns are in data", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0, 1, 0, 1),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(0, 1, 0, 1),
    time_to_exposure = c(1, 2, 1, 2)
  )

  expect_error(
    create_clones(df, id = "hamburger", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

  expect_error(
    create_clones(df, id = "id", event = "sausage", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

  expect_error(
    create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "pumpkin", time_to_exposure = "time_to_exposure")
  )

})

test_that("no missing data exists in these columns", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0, 1, NA_integer_, 1),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(0, 1, 0, 1),
    time_to_exposure = c(1, 2, 1, 2)
  )

  expect_error(
    create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

})

test_that("the data are one-row-per-patient", {
  
    df <- data.frame(
      id = c(1, 1, 2, 2),
      event = c(0, 1, 0, 1),
      time_to_event = c(1, 2, 1, 2),
      exposure = c(0, 1, 0, 1),
      time_to_exposure = c(1, 2, 1, 2)
    )
  
    expect_error(
      create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure")
    )
  
})

test_that("the same column name is not passed >1 time", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0, 1, 0, 1),
    time_to_event = c(1, 2, 1, 2),
    exposure = c(0, 1, 0, 1),
    time_to_exposure = c(1, 2, 1, 2)
  )

  expect_error(
    create_clones(df, id = "id", event = "event", time_to_event = "event", exposure = "exposure", time_to_exposure = "time_to_exposure")
  )

})

