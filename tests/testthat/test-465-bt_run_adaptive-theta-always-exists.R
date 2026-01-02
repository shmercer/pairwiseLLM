test_that("bt_run_adaptive returns theta when final_refit = FALSE", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )

  judge_fun <- function(pairs_tbl) {
    # Deterministic: always pick ID1 as better
    tibble::tibble(
      ID1 = as.character(pairs_tbl$ID1),
      ID2 = as.character(pairs_tbl$ID2),
      better_id = as.character(pairs_tbl$ID1)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    # ensure we have some results and at least one fit stored
    init_round_size = 6,
    round_size = 0,
    max_rounds = 1,
    final_refit = FALSE,
    seed_pairs = 123
  )

  expect_true(!is.null(out$theta))
  expect_true(all(c("ID", "theta", "se", "rank") %in% names(out$theta)))
  expect_true(is.character(out$theta_engine))
  expect_length(out$theta_engine, 1)
  expect_false(is.na(out$theta_engine))
})

test_that("bt_run_adaptive falls back to running theta when final refit errors", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )

  judge_fun <- function(pairs_tbl) {
    tibble::tibble(
      ID1 = as.character(pairs_tbl$ID1),
      ID2 = as.character(pairs_tbl$ID2),
      better_id = as.character(pairs_tbl$ID1)
    )
  }

  # Force compute_final_estimates() to error; PR4.1 should still populate out$theta
  testthat::local_mocked_bindings(
    compute_final_estimates = function(...) stop("forced failure"),
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    init_round_size = 6,
    round_size = 0,
    max_rounds = 1,
    final_refit = TRUE,
    seed_pairs = 456
  )

  expect_true(!is.null(out$theta))
  expect_true(all(c("ID", "theta", "se", "rank") %in% names(out$theta)))
  expect_true(is.character(out$theta_engine))
  expect_length(out$theta_engine, 1)
  expect_false(is.na(out$theta_engine))

  expect_true(is.list(out$fit_provenance))
  expect_true(isTRUE(out$fit_provenance$fallback_used))
  expect_true(is.character(out$fit_provenance$fallback_reason))
  expect_length(out$fit_provenance$fallback_reason, 1)
})
