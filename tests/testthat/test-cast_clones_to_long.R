test_that("appropriate class (ccw_clones) is caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_error(
    cast_clones_to_long(df),
    "Must inherit from class 'ccw_clones', but has class 'data.frame'."
  )

  ccw_df <- create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 200)

  attributes(ccw_df)$id <- NULL

  expect_error(
    cast_clones_to_long(ccw_df),
    "The input data.frame is missing at least one attribute"
  )

  ccw_df$outcome <- NULL

  expect_error(
    cast_clones_to_long(ccw_df),
    "The input data.frame is missing at least one of the required columns"
  )

})

test_that("long format was created correctly", {

  # Check statuses for all combinations:
  # Exposed before CED vs unexposed before CED 
  # Censored before CED vs censored after CED
  # Death before CED vs death after CED

  ced <- 100
  exposure_time <- c(80, NA_real_)
  fup_time <- c(85, 125)
  event <- c(0L, 1L)

  df <- expand.grid(
    exposure_time = exposure_time,
    fup_time = fup_time,
    event = event
  )

  df$exposure <- !is.na(df$exposure_time)
  df$id <- 1:NROW(df)

  ccw_df <- create_clones(
    df, 
    id = "id", 
    event = "event", 
    time_to_event = "fup_time", 
    exposure = "exposure", 
    time_to_exposure = "exposure_time", 
    ced_window = ced
  )

  df_long <- cast_clones_to_long(ccw_df)

  ## Truly exposed
  ### Exposed clone
  df_long_ <- df_long[df_long$id %in% df[df$exposure==1, "id"] & df_long$clone == 1, ]
  df_long_$id_time <- paste0(df_long_$id, "_", df_long_$time_id)
  df_max_time_id <- aggregate(time_id ~ id, data = df_long_, max)
  df_long_last <- merge(df_long_, df_max_time_id, by = c("id", "time_id"))
  df_long_other <- df_long_[!df_long_$id_time %in% df_long_last$id_time,]
  
  expect_equal(df_long_last$outcome, df[df$exposure==1, "event"])
  expect_true(all(df_long_other$outcome == 0))
  expect_equal(df_long_last$t_stop, df[df$exposure==1, "fup_time"])
  expect_equal(df_long_last$outcome, df[df$exposure==1, "event"])
  expect_true(all(df_long_$censor ==0))

  ### Unexposed clone
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "outcome"] == 0))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 1L, "exposure_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "censor"] == 1L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_censor"], df[df$exposure == 1L, "exposure_time"])

  ## Truly unexposed
  ### Censor before CED 
  #### Exposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "outcome"], df[df$exposure == 0L & df$fup_time < ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "fup_outcome"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 1L, "fup_censor"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])

  #### Unexposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "outcome"], df[df$exposure == 0L & df$fup_time < ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time < ced, "id"] & ccw_df$clone == 0L, "fup_censor"], df[df$exposure == 0L & df$fup_time < ced, "fup_time"])

  ### Censor after CED
  #### Exposed clone
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "outcome"] == 0))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "fup_outcome"] == ced))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "censor"] == 1L))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 1L, "fup_censor"] == ced))

  #### Unexposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "outcome"], df[df$exposure == 0L & df$fup_time >= ced, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 0L & df$fup_time >= ced, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "censor"] == 0L))
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 0L & df$fup_time >= ced, "id"] & ccw_df$clone == 0L, "fup_censor"] == ced))


  
  
})


test_that("Compare results to Maringe", {

  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 182.62) |>
    cast_clones_to_long()

  df <- df[order(df$id, df$clone, df$time_id),]
  row.names(df) <- NULL

  data_final <- data_final[order(data_final$id, data_final$clone, data_final$time_id),]
  row.names(data_final) <- NULL
  
  for (col in names(df)) {
    expect_equal(
      df[[col]],
      data_final[[col]],
      tolerance = 1e-6
    )
  }

})
