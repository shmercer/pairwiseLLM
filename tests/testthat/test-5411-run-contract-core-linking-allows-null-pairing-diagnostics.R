test_that("run contract: linking outputs may set pairing_diagnostics = NULL", {
  x <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    estimates = NULL,
    theta = tibble::tibble(ID = c("A", "B"), theta = c(0.1, -0.1)),
    theta_engine = "rank_centrality",
    fit_provenance = list(),
    stop_reason = "pair_budget_exhausted",
    stop_round = 1L,
    pairing_diagnostics = NULL
  )

  expect_silent(pairwiseLLM:::validate_pairwise_run_output(x))
})
