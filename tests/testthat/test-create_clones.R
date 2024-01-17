test_that("CED trims time to exposure appropriately", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(200L, 250L, 9900L, 100L),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3)
  )

  expect_equal(1L, df[df$id==4, "exposure"])

  expect_message(
    ccw_df <- create_clones(df, id = "id", event = "event", time_to_event = "time_to_event", exposure = "exposure", time_to_exposure = "time_to_exposure", ced_window = 2.5),
    "Updating 1 patients' exposure and time-to-exposure based on CED window"
  )

  expect_equal(0L, unique(ccw_df[ccw_df$id==4, "exposure"]))

})

test_that("All clone statuses are correct", {

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

  ## Truly exposed
  ### Exposed clone
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "outcome"], df[df$exposure == 1L, "event"])
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "fup_outcome"], df[df$exposure == 1L, "fup_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "censor"] == 0L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 1L, "fup_censor"], df[df$exposure == 1L, "exposure_time"])
  
  ### Unexposed clone
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "outcome"] == 0))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_outcome"], df[df$exposure == 1L, "exposure_time"])
  expect_true(all(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "censor"] == 1L))
  expect_equal(ccw_df[ccw_df$id %in% df[df$exposure == 1L, "id"] & ccw_df$clone == 0L, "fup_censor"], df[df$exposure == 1L, "exposure_time"])

  
  ### Truly not exposed, follow-up ends before CED --> keep outcomes
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "outcome"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, event]
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "fup_outcome"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, time_to_event]

  ### Truly not exposed, follow-up ends after CED --> censor
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "outcome"] <- 0L
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "fup_outcome"] <- ced_window

  ## UNEXPOSED
  ### Truly unexposed --> keep outcomes
  df_0[df_0[, exposure] == 0L, "outcome"] <- df_0[df_0[, exposure] == 0L, event]
  df_0[df_0[, exposure] == 0L, "fup_outcome"] <- df_0[df_0[, exposure] == 0L, time_to_event]

  ### Truly not exposed --> censor at exposure
  df_0[df_0[, exposure] == 1L, "outcome"] <- 0L
  df_0[df_0[, exposure] == 1L, "fup_outcome"] <- df_0[df_0[, exposure] == 1L, time_to_exposure]

  # CENSORING
  ## EXPOSED
  ### Truly exposed --> Do not censor. Risk of censoring ends at exposure date
  df_1[df_1[, exposure] == 1L, "censor"] <- 0L
  df_1[df_1[, exposure] == 1L, "fup_censor"] <- df_1[df_1[, exposure] == 1L, time_to_exposure]

  ### Truly not exposed, true censorship before/on CED --> Do not censor. Risk of censoring ends at event date
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "censor"] <- 0L
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, "fup_censor"] <- df_1[df_1[, exposure] == 0L & df_1[, time_to_event] <= ced_window, time_to_event]

  ### Truly not exposed, true censorship on/after CED --> Censor at CED.
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "censor"] <- 1L
  df_1[df_1[, exposure] == 0L & df_1[, time_to_event] > ced_window, "fup_censor"] <- ced_window

  ## UNEXPOSED
  ### Truly exposed --> Censored at time of exposure.
  df_0[df_0[, exposure] == 1L, "censor"] <- 1L
  df_0[df_0[, exposure] == 1L, "fup_censor"] <- df_0[df_0[, exposure] == 1L, time_to_exposure]

  ### Truly not exposed, true censorship before/on CED --> Do not censor. Risk of censoring ends at event date
  df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, "censor"] <- 0L
  df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, "fup_censor"] <- df_0[df_0[, exposure] == 0L & df_0[, time_to_event] <= ced_window, time_to_event]

  ### Truly not exposed, true censorship on/after CED --> Do not censor. Risk of censoring ends at CED.
  df_0[df_0[, exposure] == 0L & df_0[, time_to_event] > ced_window, "censor"] <- 0L
  df_0[df_0[, exposure] == 0L & df_0[, time_to_event] > ced_window, "fup_censor"] <- ced_window





})

test_that("Maringe results are replicated", {

  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 182.62)

  df <- df[order(df$id, df$clone),]

  tab <- tab[order(tab$id, tab$clone),]

  # Compare each 
  for (col in c("outcome", "fup_outcome", "censor", "fup_censor")) {
    row.names(df) <- NULL
    row.names(tab) <- NULL
    expect_equal(
      df[[col]],
      tab[[col]],
      tolerance = 1e-6
    )
  }

})

test_that("Study attributes are passed correctly", {

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