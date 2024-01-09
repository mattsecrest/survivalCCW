
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


test_that("class is correct", {

  df <- toy_df |>
    create_clones(id = "id", event = "death", time_to_event = "fup_obs", exposure = "surgery", time_to_exposure = "timetosurgery", ced_window = 182.62) |>
    cast_clones_to_long() |>
    generate_ccw(predvars = c("age", "sex", "perf", "stage", "deprivation", "charlson", "emergency"))

  expect_equal(
    class(df),
    "ccw_clones_long_weights"
  )

})