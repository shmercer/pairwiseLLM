testthat::test_that("artifact writing writes expected files", {
  output_dir <- withr::local_tempdir()

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$output_dir <- output_dir
  state$config$v3 <- adaptive_v3_config(
    state$N,
    list(
      write_outputs = TRUE,
      keep_draws = TRUE,
      thin_draws = 2L,
      output_dir = output_dir
    )
  )

  fit <- make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(
      c(
        0.2, -0.1, -0.1,
        0.1, 0.0, -0.1,
        0.3, -0.2, -0.1,
        0.0, 0.1, -0.1
      ),
      nrow = 4,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )

  metrics <- list(
    theta_sd_median_S = 0.1,
    tau = 0.2,
    theta_sd_pass = TRUE,
    U0 = 0.5,
    U_pass = TRUE,
    diagnostics_pass = TRUE
  )
  stop_out <- list(stop_decision = FALSE, stop_reason = NA_character_)
  state$config$round_log <- build_round_log_row(
    state = state,
    fit = fit,
    metrics = metrics,
    stop_out = stop_out,
    config = state$config$v3
  )

  .adaptive_write_v3_artifacts(state, fit = fit, output_dir = output_dir)

  config_path <- file.path(output_dir, "adaptive_config.rds")
  round_log_path <- file.path(output_dir, "round_log.rds")
  batch_log_path <- file.path(output_dir, "batch_log.rds")
  item_summary_path <- file.path(output_dir, "item_summary.rds")
  draws_path <- file.path(output_dir, "theta_draws.rds")

  for (path in c(config_path, round_log_path, batch_log_path, item_summary_path, draws_path)) {
    testthat::expect_true(file.exists(path))
    testthat::expect_true(file.info(path)$size > 0)
  }

  draws <- readRDS(draws_path)
  testthat::expect_equal(nrow(draws$theta), 2L)
})
