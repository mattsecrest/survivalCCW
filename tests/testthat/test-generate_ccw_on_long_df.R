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

test_that("when predvar columns are missing, an error is thrown", {

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw_on_long_df(df_long, predvar = "hamburger")
  )

  expect_error(
    generate_ccw_on_long_df(df_long, predvar = NULL)
  )

})

test_that("weights are adequately calculated", {
  expect_true(TRUE)
  #@TODO more test cases
})
