test_that("bt_run_adaptive_core_linking validates key inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("A", "B")),
      judge_fun = judge_fun,
      linking_min_n = 0L
    ),
    "linking_min_n"
  )
})

test_that("bt_run_adaptive_core_linking calls exhaustion fallback when enabled", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    tibble_theta <- tibble::tibble(ID = sort(unique(c(bt_data$object1, bt_data$object2))), theta = 0)
    list(theta = tibble_theta, se = tibble::tibble(ID = tibble_theta$ID, se = 1))
  }

  # Ensure we actually go through the exhaustion fallback branch without relying on the full proposer.
  testthat::local_mocked_bindings(
    bt_core_link_round = function(samples, fit, core_ids, new_ids, round_size, ...) {
      list(pairs = tibble::tibble(
        custom_id = "p1",
        ID1 = "A",
        text1 = "a",
        ID2 = "B",
        text2 = "b"
      ))
    },
    .bt_apply_exhaustion_fallback = function(pairs, ...) pairs,
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("A", "B")),
    judge_fun = judge_fun,
    round_size = 1L,
    init_round_size = 1L,
    max_rounds_per_batch = 1L,
    min_rounds = 0L,
    exhaustion_fallback = "cross_batch_new_new",
    fit_fun = fit_fun,
    final_refit = FALSE,
    store_running_estimates = FALSE,
    return_diagnostics = TRUE
  )

  expect_true(is.list(out))
  expect_true(is.data.frame(out$theta))
  expect_true(is.null(out$estimates))
  expect_true(isTRUE(out$fit_provenance$fallback_used))
})
