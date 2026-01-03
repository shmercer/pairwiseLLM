testthat::test_that("bt_run_adaptive stops cleanly when round_size is zero", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    round_size = 0,
    init_round_size = 0,
    max_rounds = 1,
    judge_fun = function(pairs) dplyr::mutate(pairs, better_id = .data$ID1)
  )

  testthat::expect_equal(out$stop_reason, "round_size_zero")
  testthat::expect_true(is.data.frame(out$pairing_diagnostics))
})


testthat::test_that("bt_run_adaptive errors when no valid comparisons remain after filtering", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  testthat::expect_error(
    pairwiseLLM::bt_run_adaptive(
      samples = samples,
      init_round_size = 1,
      round_size = 1,
      max_rounds = 1,
      judge_fun = function(pairs) dplyr::mutate(pairs, better_id = NA_character_),
      final_refit = FALSE,
      # fit_fun won't be reached, but include to avoid sirt calls in case of changes
      fit_fun = function(bt_data, ...) {
        ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
        list(theta = tibble::tibble(ID = ids, theta = 0, se = NA_real_), reliability = NA_real_)
      }
    ),
    "No valid comparisons found after filtering"
  )
})

testthat::test_that("bt_run_adaptive uses max_rounds when no stop targets are set", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    init_round_size = 1,
    round_size = 1,
    max_rounds = 1,
    rel_se_p90_target = NA_real_,
    reliability_target = NA_real_,
    judge_fun = function(pairs) dplyr::mutate(pairs, better_id = .data$ID1),
    final_refit = FALSE
  )

  testthat::expect_equal(out$stop_reason, "max_rounds")
})

