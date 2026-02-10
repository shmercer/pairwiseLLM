resolve_build_results_fn <- function() {
  ns <- asNamespace("pairwiseLLM")
  if (exists("build_btl_results_data", envir = ns, inherits = FALSE)) {
    return(get("build_btl_results_data", envir = ns, inherits = FALSE))
  }

  # Fallback for test harnesses that attach an installed namespace before
  # load_all; source the local definition under test.
  src_env <- new.env(parent = ns)
  sys.source(testthat::test_path("..", "..", "R", "bayes_btl_mcmc.R"), envir = src_env)
  get("build_btl_results_data", envir = src_env, inherits = FALSE)
}

test_that("build_btl_results_data converts legacy pairs into canonical results_tbl", {
  data("example_writing_pairs", package = "pairwiseLLM")
  build_results <- resolve_build_results_fn()

  out <- build_results(example_writing_pairs)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(example_writing_pairs))
  expect_true(all(c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "better_id", "winner_pos",
    "phase", "iter", "received_at", "backend", "model"
  ) %in% names(out)))
  expect_true(all(out$winner_pos %in% c(1L, 2L)))
  expect_no_error(pairwiseLLM:::validate_results_tbl(out))
})

test_that("build_btl_results_data is deterministic for pair_uid/iter/timestamps", {
  build_results <- resolve_build_results_fn()
  df <- tibble::tibble(
    ID1 = c("A", "A", "B"),
    ID2 = c("B", "B", "A"),
    better_id = c("A", "B", "A")
  )

  out <- build_results(
    df,
    iter_start = 5L,
    received_at_start = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )

  expect_equal(out$pair_uid, c("A:B#1", "A:B#2", "A:B#3"))
  expect_equal(out$iter, c(5L, 6L, 7L))
  expect_equal(
    out$received_at,
    as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 00:00:01", "2026-01-01 00:00:02"), tz = "UTC")
  )
})

test_that("build_btl_results_data validates malformed rows", {
  build_results <- resolve_build_results_fn()
  bad_self <- tibble::tibble(ID1 = "A", ID2 = "A", better_id = "A")
  expect_error(build_results(bad_self), "self-pairs")

  bad_winner <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "C")
  expect_error(build_results(bad_winner), "must match `ID1` or `ID2`")
})

test_that("fit_bayes_btl_mcmc accepts converted non-adaptive data", {
  build_results <- resolve_build_results_fn()
  mock_fit <- function(bt_data, config, seed = NULL, model_fn = NULL) {
    theta <- matrix(seq_len(2L * bt_data$N), nrow = 2L)
    colnames(theta) <- bt_data$item_id
    list(
      draws = list(theta = theta, epsilon = c(0.05, 0.06), beta = c(-0.1, 0.1)),
      model_variant = "btl_e_b",
      diagnostics = list(divergences = 0L, max_rhat = 1.01, min_ess_bulk = 200),
      mcmc_config_used = list(
        chains = 1L,
        parallel_chains = 1L,
        core_fraction = 0.8,
        cores_detected_physical = 1L,
        cores_detected_logical = 1L,
        threads_per_chain = 1L,
        cmdstanr_version = "0.1"
      )
    )
  }

  data("example_writing_pairs", package = "pairwiseLLM")
  results_tbl <- build_results(example_writing_pairs)
  ids <- sort(unique(c(results_tbl$A_id, results_tbl$B_id)))

  out <- testthat::with_mocked_bindings(
    .fit_bayes_btl_mcmc_adaptive = mock_fit,
    pairwiseLLM::fit_bayes_btl_mcmc(results_tbl, ids = ids, model_variant = "btl_e_b"),
    .env = asNamespace("pairwiseLLM")
  )

  expect_s3_class(out$round_log, "tbl_df")
  expect_s3_class(out$item_summary, "tbl_df")
})
