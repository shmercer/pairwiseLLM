add_scheduled_pair <- function(state, A_id, B_id, iter = 1L, phase = "phase1") {
  created_at <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  unordered_key <- pairwiseLLM:::make_unordered_key(A_id, B_id)
  ordered_key <- pairwiseLLM:::make_ordered_key(A_id, B_id)
  pair_uid <- pairwiseLLM:::pair_uid_from_state(state, unordered_key)

  row <- tibble::tibble(
    pair_uid = pair_uid,
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    A_text = state$texts[[A_id]],
    B_text = state$texts[[B_id]],
    phase = phase,
    iter = as.integer(iter),
    created_at = created_at
  )

  state <- pairwiseLLM:::record_presentation(state, A_id, B_id)
  state$history_pairs <- dplyr::bind_rows(state$history_pairs, row)
  state$comparisons_scheduled <- as.integer(state$comparisons_scheduled + 1L)

  list(state = state, pair_row = row)
}

testthat::test_that("scheduled exposure metrics use history_pairs", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("alpha", "bravo", "charlie", "delta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(state$N, list())

  state <- add_scheduled_pair(state, "A", "B")$state
  state <- add_scheduled_pair(state, "A", "C")$state
  state <- add_scheduled_pair(state, "B", "D")$state

  state <- pairwiseLLM:::record_judgment_exposure(state, "A", "B")
  state$comparisons_observed <- 1L

  scheduled <- pairwiseLLM:::.adaptive_compute_scheduled_exposure(state)

  deg_scheduled <- c(A = 2L, B = 2L, C = 1L, D = 1L)
  posA_scheduled <- c(A = 2L, B = 1L, C = 0L, D = 0L)
  pos_balance_scheduled <- posA_scheduled / pmax(deg_scheduled, 1L)
  pos_balance_sd <- stats::sd(pos_balance_scheduled)

  testthat::expect_equal(scheduled$deg_scheduled, deg_scheduled)
  testthat::expect_equal(scheduled$posA_scheduled, posA_scheduled)
  testthat::expect_equal(scheduled$mean_degree_scheduled, mean(as.double(deg_scheduled)))
  testthat::expect_equal(scheduled$min_degree_scheduled, 1L)
  testthat::expect_equal(scheduled$pos_balance_sd_scheduled, pos_balance_sd)

  row <- pairwiseLLM:::build_round_log_row(
    state = state,
    metrics = list(scheduled_pairs = 3L, completed_pairs = 1L),
    config = state$config$v3,
    new_pairs = 1L
  )

  testthat::expect_equal(row$mean_degree_scheduled[[1L]], mean(as.double(deg_scheduled)))
  testthat::expect_equal(row$min_degree_scheduled[[1L]], 1L)
  testthat::expect_equal(row$pos_balance_sd_scheduled[[1L]], pos_balance_sd)
  testthat::expect_true(abs(row$mean_degree_scheduled[[1L]] - row$mean_degree[[1L]]) > 1e-8)
})
