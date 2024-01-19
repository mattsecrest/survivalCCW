test_that("incorrect classes are caught", {

  df <- data.frame(
    id = c(1, 2, 3, 4),
    event = c(0L, 1L, 0L, 1L),
    time_to_event = c(100, 200, 100, 200),
    exposure = c(0L, 1L, 0L, 1L),
    time_to_exposure = c(NA_real_, 2.3, NA_real_, 3.3),
    ham = c(0, 1, 0, 1)
  )

  expect_error(
    generate_ccw(df, predvars = c("ham")),
    "Must inherit from class 'ccw_clones_long', but has class 'data.frame'"
  )

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  attributes(df_long)$id <- NULL

  expect_error(
    generate_ccw(df_long, "ham"),
    "The input data.frame is missing at least one attribute: id, event, time_to_event, exposure, time_to_exposure, ced_window. Did you remove these or try to make a custom data.frame?"
  )

  df_long$t_start <- NULL

  expect_error(
    generate_ccw(df_long),
    "The input data.frame is missing at least one of the required columns: outcome, fup_outcome, censor, fup_censor, clone, t_start, t_stop, time_id, t_event. Did you remove this?"
  )
})

test_that("when predvars columns are missing, an error is thrown", {

  df_long <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw(df_long, predvars = "hamburger"),
    "At least one of these predvars columns is not on the data.frame: hamburger"
  )

  expect_error(
    generate_ccw(df_long, predvars = NULL),
    "predvars cannot be NULL. Please specify at least one variable to use for weights"
  )

})

test_that("categorical vars are dealt with", {

  toy_df_s <- toy_df
  toy_df_s$sandwich <- rep(c("ham", "turkey", "cheese"), length.out = NROW(toy_df_s))

  df_long <- toy_df_s |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 365.25/2) |>
    cast_clones_to_long()

  expect_error(
    generate_ccw(df_long, predvars = "sandwich"),
    "At least one of the predvars columns is character/factor. In this early version of `survivalCCW`, only numeric variables are considered. Please make dummy vars on your own!"
  )

  df_long$sandwich_f <- factor(df_long$sandwich)

  expect_error(
    generate_ccw(df_long, predvars = "sandwich_f"),
    "At least one of the predvars columns is character/factor. In this early version of `survivalCCW`, only numeric variables are considered. Please make dummy vars on your own!"
  )

})

test_that("weights are adequately calculated compared to Maringe", {

  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 182.62) |>
    cast_clones_to_long() |>
    generate_ccw(predvars = c("age", "sex", "perf", "stage", "deprivation", "charlson", "emergency"))

  df <- df[order(df$id, df$clone, df$time_id),]
  row.names(df) <- NULL

  # Compare exposed
  data_long_cox <- data_long_cox[order(data_long_cox$id, data_long_cox$clone, data_long_cox$time_id),]
  row.names(data_long_cox) <- NULL

  # Compare all columns
  for (col in colnames(data_long_cox)[colnames(data_long_cox) %in% colnames(df)]) {
    row.names(df[[col]]) <- NULL
    row.names(data_long_cox[[col]]) <- NULL
    expect_equal(
      df[[col]],
      data_long_cox[[col]],
      tolerance = 1e-6
    )
  }

  cox_df <- survival::coxph(survival::Surv(t_start, t_stop, outcome) ~ clone, data = df, weights = weight_cox)
  cox_data_long_cox <- survival::coxph(survival::Surv(t_start, t_stop, outcome) ~ clone, data = data_long_cox, weights = weight_cox)

  expect_equal(cox_df$coefficients, cox_data_long_cox$coefficients, tolerance = 1e-6)

})