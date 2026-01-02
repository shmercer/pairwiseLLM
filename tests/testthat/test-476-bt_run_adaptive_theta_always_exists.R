testthat::test_that("bt_run_adaptive returns theta when final_refit is FALSE", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    dplyr::mutate(pairs, better_id = .data$ID1) |>
      dplyr::select(.data$ID1, .data$ID2, .data$better_id)
  }

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 3,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 1,
    final_refit = FALSE
  )

  testthat::expect_true(!is.null(out$theta))
  testthat::expect_s3_class(out$theta, "tbl_df")
  testthat::expect_true(all(c("ID", "theta", "se", "rank") %in% names(out$theta)))
  testthat::expect_true(is.character(out$theta_engine) && length(out$theta_engine) == 1L)

  testthat::expect_true(is.list(out$fit_provenance))
  testthat::expect_true(isTRUE(out$fit_provenance$fallback_used))
  testthat::expect_equal(out$fit_provenance$fallback_reason, "final_refit_disabled")
  testthat::expect_equal(out$fit_provenance$fallback_source, "last_running_fit")
})


testthat::test_that("bt_run_adaptive falls back to running theta when final refit errors", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    dplyr::mutate(pairs, better_id = .data$ID1) |>
      dplyr::select(.data$ID1, .data$ID2, .data$better_id)
  }

  testthat::local_mocked_bindings(
    compute_final_estimates = function(...) {
      stop("boom")
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    round_size = 3,
    init_round_size = 3,
    max_rounds = 1,
    seed_pairs = 1
  )

  testthat::expect_true(!is.null(out$theta))
  testthat::expect_s3_class(out$theta, "tbl_df")
  testthat::expect_true(all(c("ID", "theta", "se", "rank") %in% names(out$theta)))
  testthat::expect_true(is.character(out$theta_engine) && length(out$theta_engine) == 1L)

  testthat::expect_true(is.list(out$fit_provenance))
  testthat::expect_true(isTRUE(out$fit_provenance$fallback_used))
  testthat::expect_equal(out$fit_provenance$fallback_reason, "final_refit_failed")
  testthat::expect_equal(out$fit_provenance$fallback_source, "last_running_fit")
})
