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

test_that("weights are adequately calculated", {
  expect_true(TRUE)
  #@TODO more test cases
})
