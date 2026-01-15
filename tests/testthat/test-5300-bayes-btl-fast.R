make_results_tbl <- function() {
  base <- tibble::tibble(
    A_id = c("A", "A", "B", "D", "C"),
    B_id = c("B", "C", "C", "A", "D"),
    better_id = c("A", "C", "B", "D", "D")
  )

  base <- dplyr::mutate(
    base,
    unordered_key = paste(pmin(.data$A_id, .data$B_id), pmax(.data$A_id, .data$B_id), sep = ":"),
    ordered_key = paste(.data$A_id, .data$B_id, sep = ":"),
    pair_uid = paste0(.data$unordered_key, "#1"),
    winner_pos = ifelse(.data$better_id == .data$A_id, 1L, 2L),
    phase = "phase2",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  base[, c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "better_id", "winner_pos",
    "phase", "iter", "received_at", "backend", "model"
  )]
}

test_that("fit_bayes_btl_fast returns aligned outputs and is deterministic", {
  results <- make_results_tbl()
  ids <- c("A", "B", "C", "D", "E")

  withr::local_seed(123)
  fit1 <- pairwiseLLM:::fit_bayes_btl_fast(results, ids, n_draws = 40, seed = 10)
  withr::local_seed(123)
  fit2 <- pairwiseLLM:::fit_bayes_btl_fast(results, ids, n_draws = 40, seed = 10)

  expect_equal(names(fit1$theta_mean), ids)
  expect_equal(dim(fit1$theta_draws), c(40L, length(ids)))
  expect_equal(colnames(fit1$theta_draws), ids)
  expect_equal(fit1$theta_mean, fit2$theta_mean)
  expect_equal(fit1$theta_draws, fit2$theta_draws)
})

test_that("fit_bayes_btl_fast enforces identifiability and fixed scale", {
  results <- make_results_tbl()
  ids <- c("A", "B", "C", "D", "E")

  withr::local_seed(99)
  fit <- pairwiseLLM:::fit_bayes_btl_fast(results, ids, n_draws = 30, seed = 5)

  expect_equal(sum(fit$theta_mean), 0, tolerance = 1e-8)
  expect_equal(stats::sd(fit$theta_mean), 1, tolerance = 1e-6)

  row_means <- rowMeans(fit$theta_draws)
  row_sd <- apply(fit$theta_draws, 1, stats::sd)
  expect_true(all(abs(row_means) < 1e-6))
  expect_true(all(abs(row_sd - 1) < 1e-6))
})

test_that("fit_bayes_btl_fast includes missing ids with finite draws", {
  results <- make_results_tbl()
  ids <- c("A", "B", "C", "D", "E")

  withr::local_seed(7)
  fit <- pairwiseLLM:::fit_bayes_btl_fast(results, ids, n_draws = 25, seed = 3)

  expect_true(is.finite(fit$theta_mean[["E"]]))
  expect_true(all(is.finite(fit$theta_draws[, "E"])))
})

test_that("fit_bayes_btl_fast rejects invalid winners", {
  results <- make_results_tbl()
  results$better_id[1] <- "Z"

  expect_error(
    pairwiseLLM:::fit_bayes_btl_fast(results, ids = c("A", "B", "C", "D", "E")),
    "better_id"
  )
})

test_that("fit_bayes_btl_fast covers missing ids, n_draws validation, and empty results", {
  results <- make_results_tbl()

  expect_error(
    pairwiseLLM:::fit_bayes_btl_fast(results, ids = c("A", "B")),
    "contained in `ids`"
  )
  expect_error(
    pairwiseLLM:::fit_bayes_btl_fast(results, ids = c("A", "B", "C", "D", "E"), n_draws = 1),
    "n_draws"
  )

  empty_results <- results[0, ]
  fit <- pairwiseLLM:::fit_bayes_btl_fast(empty_results, ids = c("A", "B"), n_draws = 10, seed = 1)
  expect_true(isTRUE(fit$fit_meta$converged))
  expect_match(fit$fit_meta$message, "No comparisons available")
})

test_that("internal BTL helpers guard invalid ids and zero-length wins", {
  expect_error(pairwiseLLM:::.btl_fast_validate_ids(character()), "at least one")
  expect_error(pairwiseLLM:::.btl_fast_validate_ids(c("A", NA)), "non-missing")
  expect_error(pairwiseLLM:::.btl_fast_validate_ids(c("A", "A")), "unique")

  results <- make_results_tbl()
  edges <- pairwiseLLM:::.btl_fast_prepare_edges(results)
  expect_true(all(c("i_id", "j_id", "win_i", "win_j", "n_ij") %in% names(edges)))

  edges_min <- data.frame(i_id = "A", j_id = "B", n_ij = 1L, stringsAsFactors = FALSE)
  fit <- pairwiseLLM:::.btl_fast_fit(edges_min, ids = c("A", "B"))
  expect_true(is.numeric(fit$theta_raw))
  expect_true(is.numeric(fit$se_raw))
})

test_that("refit helpers validate arguments", {
  expect_error(pairwiseLLM:::refit_every_batches(CW = 0, batch_size = 1), "positive integer")
  expect_error(pairwiseLLM:::refit_every_batches(CW = 1, batch_size = 0), "positive integer")
  expect_error(
    pairwiseLLM:::should_refit(-1, last_refit_at = 0, batch_size = 1, CW = 1),
    "non-negative"
  )
  expect_error(
    pairwiseLLM:::should_refit(1, last_refit_at = -1, batch_size = 1, CW = 1),
    "non-negative"
  )
  expect_error(
    pairwiseLLM:::should_refit(1, last_refit_at = 0, batch_size = 0, CW = 1),
    "positive integer"
  )
  expect_error(
    pairwiseLLM:::should_refit(1, last_refit_at = 0, batch_size = 1, CW = 0),
    "positive integer"
  )
})

test_that("refit cadence helpers follow comparison-space policy", {
  expect_equal(pairwiseLLM:::refit_every_batches(CW = 10, batch_size = 4), 3L)
  expect_equal(pairwiseLLM:::refit_every_batches(CW = 1, batch_size = 10), 1L)

  expect_false(pairwiseLLM:::should_refit(19, last_refit_at = 10, batch_size = 5, CW = 10))
  expect_true(pairwiseLLM:::should_refit(20, last_refit_at = 10, batch_size = 5, CW = 10))
})
