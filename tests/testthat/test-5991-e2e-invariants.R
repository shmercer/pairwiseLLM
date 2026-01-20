testthat::test_that("e2e adaptive run satisfies structural and contract invariants", {
  seed <- 1602
  run <- e2e_run_locked_scenario(seed = seed)
  out <- run$out
  state <- out$state
  v3_config <- state$config$v3
  selection_log <- run$selection_log

  total_pairs <- state$N * (state$N - 1L) / 2
  hard_cap_threshold <- ceiling(0.40 * total_pairs)
  unique_pairs_seen <- sum(state$pair_count >= 1L)

  testthat::expect_true(e2e_is_connected(state$ids, state$history_results$unordered_key))
  testthat::expect_false(any(state$history_pairs$A_id == state$history_pairs$B_id))
  testthat::expect_false(any(state$history_results$A_id == state$history_results$B_id))
  testthat::expect_true(unique_pairs_seen <= hard_cap_threshold)

  pair_counts <- dplyr::count(state$history_pairs, .data$unordered_key, name = "n")
  testthat::expect_true(all(pair_counts$n <= as.integer(v3_config$dup_max_count)))

  dup_selected <- dplyr::filter(selection_log, .data$count_before >= 1L)
  if (nrow(dup_selected) > 0L) {
    testthat::expect_true(all(abs(dup_selected$p_mean - 0.5) <= v3_config$dup_p_margin))
    testthat::expect_true(all(dup_selected$utility >= dup_selected$U_dup_threshold))
  }

  dup_keys <- pair_counts$unordered_key[pair_counts$n > 1L]
  if (length(dup_keys) > 0L) {
    for (key in dup_keys) {
      rows <- dplyr::filter(state$history_pairs, .data$unordered_key == key)
      for (idx in 2L:nrow(rows)) {
        testthat::expect_identical(as.character(rows$A_id[[idx]]), as.character(rows$B_id[[idx - 1L]]))
        testthat::expect_identical(as.character(rows$B_id[[idx]]), as.character(rows$A_id[[idx - 1L]]))
      }
    }
  }

  gini_degree <- pairwiseLLM:::compute_gini_degree(state$deg)
  testthat::expect_true(is.finite(gini_degree))
  testthat::expect_true(gini_degree >= 0)

  pos_prop <- as.double(state$pos_count) / pmax(1, as.double(state$deg))
  pos_balance_max_abs_dev <- max(abs(pos_prop - 0.5), na.rm = TRUE)
  testthat::expect_true(is.finite(pos_balance_max_abs_dev))
  testthat::expect_true(pos_balance_max_abs_dev <= 0.5 + 1e-12)

  deg_from_pairs <- function(pairs_tbl, ids) {
    vapply(ids, function(id) {
      sum(pairs_tbl$A_id == id | pairs_tbl$B_id == id)
    }, integer(1L))
  }

  warm_pairs <- dplyr::filter(state$history_pairs, .data$phase == "phase1")
  warm_deg <- deg_from_pairs(warm_pairs, state$ids)
  warm_range <- max(warm_deg) - min(warm_deg)

  adaptive_pairs <- dplyr::filter(state$history_pairs, .data$phase %in% c("phase2", "phase3"))
  iters <- sort(unique(as.integer(adaptive_pairs$iter)))
  if (length(iters) > 0L) {
    ranges <- vapply(iters, function(it) {
      pairs_now <- dplyr::bind_rows(
        warm_pairs,
        dplyr::filter(adaptive_pairs, .data$iter <= it)
      )
      deg_now <- deg_from_pairs(pairs_now, state$ids)
      as.integer(max(deg_now) - min(deg_now))
    }, integer(1L))
    ranges <- c(warm_range, ranges)
    range_jumps <- diff(ranges)
    batch_sizes <- dplyr::count(selection_log, .data$batch_index, name = "n")
    batch_size <- max(batch_sizes$n, na.rm = TRUE)
    testthat::expect_true(all(range_jumps <= 2L * as.integer(batch_size)))
  }

  round_log <- state$config$round_log
  testthat::expect_true(nrow(round_log) >= 1L)
  testthat::expect_identical(colnames(round_log), colnames(round_log_schema()))
  testthat::expect_true(all(is.na(round_log$pos_balance_sd) | round_log$pos_balance_sd <= 0.5))

  has_exploit_in_batch <- selection_log |>
    dplyr::group_by(.data$batch_index) |>
    dplyr::summarise(
      has_exploit = any(is.finite(.data$utility) & is.finite(.data$p_mean)),
      diagnostics_pass = dplyr::first(.data$diagnostics_pass),
      .groups = "drop"
    )
  if (nrow(has_exploit_in_batch) > 0L) {
    testthat::expect_true(all(has_exploit_in_batch$diagnostics_pass[has_exploit_in_batch$has_exploit]))
  }

  stop_reason <- as.character(state$stop_reason)
  testthat::expect_true(stop_reason %in% c("hard_cap_40pct", "v3_converged", "diagnostics_failed"))
  testthat::expect_identical(as.character(out$next_action$reason), stop_reason)

  mock_submit <- e2e_mock_submit_from_theta(run$theta_true)
  resume_out <- testthat::with_mocked_bindings(
    pairwiseLLM::adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = out$submission_info,
      adaptive = run$adaptive,
      seed = seed
    ),
    submit_llm_pairs = mock_submit
  )
  testthat::expect_identical(resume_out$next_action$action, "done")
  testthat::expect_equal(nrow(resume_out$submission_info$pairs_submitted), 0L)

  item_summary <- pairwiseLLM:::build_item_summary(state, fit = state$fit)
  testthat::expect_equal(nrow(item_summary), state$N)
  testthat::expect_identical(colnames(item_summary), colnames(item_summary_schema()))

  rel <- round_log$reliability_EAP
  rel <- rel[!is.na(rel)]
  if (length(rel) > 0L) {
    testthat::expect_true(all(rel >= 0 & rel <= 1))
  }
})
