test_that("stop payload validators enforce stable schemas", {
  # Minimal fake fit objects for bt_stop_metrics
  fit1 <- list(
    theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 0, -1)),
    se = tibble::tibble(ID = c("A", "B", "C"), se = c(0.2, 0.2, 0.2)),
    diagnostics = list(
      reliability = 0.9,
      sepG = 3.1,
      item_misfit_prop = 0,
      judge_misfit_prop = 0
    )
  )
  fit2 <- list(
    theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(1.01, 0.01, -1.02)),
    se = tibble::tibble(ID = c("A", "B", "C"), se = c(0.2, 0.2, 0.2)),
    diagnostics = fit1$diagnostics
  )

  m <- bt_stop_metrics(fit2, prev_fit = fit1, stability_topk = 2L)
  expect_silent(.validate_stop_metrics_tbl(m))

  rounds <- tibble::tibble(
    round = 1L,
    n_new_pairs_scored = 0L,
    n_total_results = 0L,
    stop = FALSE,
    stop_reason = NA_character_,
    degree_min = 1,
    largest_component_frac = 1,
    rms_theta_delta = 0.01,
    topk_overlap = 1
  )
  expect_silent(.validate_rounds_schema(rounds))
})
