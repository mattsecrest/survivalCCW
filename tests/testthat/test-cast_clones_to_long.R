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

test_that("long format was created correctly", {

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_true(TRUE)
  #@TODO more test cases
})


test_that("Compare results to Maringe", {

  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  df <- df[order(df$id, df$clone, df$time_id),]
  row.names(df) <- NULL

  load(system.file("tests/testthat/data/data_final_maringe.RData", package = "survivalCCW"))
  data_final <- data_final[order(data_final$id, data_final$clone, data_final$time_id),]
  row.names(data_final) <- NULL
  
  for (col in names(df)) {
    expect_equal(
      df[[col]],
      data_final[[col]],
      tolerance = 0.05
    )
  }

})
