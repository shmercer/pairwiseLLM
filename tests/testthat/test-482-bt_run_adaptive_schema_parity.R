testthat::test_that("bt_run_adaptive default outputs are schema-valid and expose engine provenance", {
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tibble")

  # Use an existing minimal fixture if present; otherwise build a tiny synthetic set.
  if (exists("example_writing_samples", envir = asNamespace("pairwiseLLM"), inherits = FALSE)) {
    samples <- pairwiseLLM::example_writing_samples
  } else if (exists("example_writing_pairs", envir = asNamespace("pairwiseLLM"), inherits = FALSE)) {
    # fallback: derive sample table from pairs IDs
    ids <- unique(c(pairwiseLLM::example_writing_pairs$ID1, pairwiseLLM::example_writing_pairs$ID2))
    samples <- tibble::tibble(ID = as.character(ids), text = paste("Sample", ids))
  } else {
    samples <- tibble::tibble(ID = as.character(1:6), text = paste("Sample", 1:6))
  }

  # Deterministic mock judge: always prefer shorter ID (stable, no network)
  judge_fun <- function(pairs, ...) {
    pairs <- tibble::as_tibble(pairs)
    id1 <- as.character(pairs$ID1)
    id2 <- as.character(pairs$ID2)
    better_id <- ifelse(id1 <= id2, id1, id2)
    tibble::tibble(
      ID1 = id1,
      ID2 = id2,
      better_id = better_id,
      result_type = "succeeded"
    )
  }

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 10,
    init_round_size = 10,
    max_rounds = 1,
    final_refit = TRUE,
    fit_engine_final = "none", # force RC-only final to avoid Suggests variability
    fit_engine_running = "rank_centrality",
    seed = 1
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true(inherits(out$estimates, "data.frame"))
  testthat::expect_true(inherits(out$theta, "data.frame"))

  # Schema columns must exist
  pairwiseLLM:::.validate_estimates_tbl(out$estimates, arg_name = "out$estimates")

  # Provenance is explicit
  testthat::expect_true(is.list(out$fit_provenance))
  testthat::expect_true(!is.null(out$theta_engine))
  testthat::expect_true(out$theta_engine %in% c("rank_centrality", "bt_firth", "bt_mle", "bt_bayes"))
})
