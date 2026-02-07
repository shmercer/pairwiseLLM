test_that("round_log includes required stopping and star-cap audit fields", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  round_log <- adaptive_round_log(out)
  required <- c(
    "theta_corr_pass",
    "delta_sd_theta_pass",
    "star_cap_rejects_since_last_refit",
    "star_cap_reject_rate_since_last_refit",
    "recent_deg_median_since_last_refit",
    "recent_deg_max_since_last_refit",
    "ci95_theta_width_mean",
    "cov_trace_theta",
    "top20_boundary_entropy_mean",
    "nn_diff_sd_mean",
    "refit_id",
    "round_id_at_refit",
    "mcmc_chains",
    "mcmc_parallel_chains"
  )
  expect_true(all(required %in% names(round_log)))

  expect_true(is.na(round_log$theta_corr_pass[[1L]]))
  expect_true(is.na(round_log$delta_sd_theta_pass[[1L]]))
  expect_true(is.logical(round_log$theta_corr_pass[[2L]]))
  expect_true(is.logical(round_log$delta_sd_theta_pass[[2L]]))

  expect_true(is.integer(round_log$star_cap_rejects_since_last_refit))
  expect_true(is.double(round_log$star_cap_reject_rate_since_last_refit))
  expect_true(is.double(round_log$recent_deg_median_since_last_refit))
  expect_true(is.integer(round_log$recent_deg_max_since_last_refit))
  expect_true(is.double(round_log$ci95_theta_width_mean))
  expect_true(is.double(round_log$cov_trace_theta))
  expect_true(is.double(round_log$top20_boundary_entropy_mean))
  expect_true(is.double(round_log$nn_diff_sd_mean))
  expect_true(is.integer(round_log$refit_id))
  expect_true(is.integer(round_log$round_id_at_refit))
  expect_true(is.integer(round_log$mcmc_chains))
  expect_true(is.integer(round_log$mcmc_parallel_chains))
  reject_rate <- round_log$star_cap_reject_rate_since_last_refit
  reject_rate <- reject_rate[!is.na(reject_rate)]
  expect_true(all(reject_rate >= 0))
  expect_true(all(reject_rate <= 1))
})

test_that("round_log stop decisions and committed counts are reconstructable from logs", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  withr::local_seed(1)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 4L,
    fit_fn = stub$fit_fn,
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none"
  )

  step_log <- adaptive_step_log(out)
  round_log <- adaptive_round_log(out)
  committed_indicator <- !is.na(step_log$pair_id)
  committed_cumsum <- cumsum(as.integer(committed_indicator))
  step_at_refit <- as.integer(round_log$step_id_at_refit)
  reconstructed_total <- committed_cumsum[step_at_refit]

  expect_equal(as.integer(round_log$total_pairs_done), as.integer(reconstructed_total))
  expect_equal(
    as.integer(round_log$new_pairs_since_last_refit),
    c(round_log$total_pairs_done[[1L]], diff(as.integer(round_log$total_pairs_done)))
  )
  expect_identical(as.logical(round_log$stop_decision), vapply(
    seq_len(nrow(round_log)),
    function(idx) {
      pairwiseLLM:::should_stop(
        as.list(round_log[idx, , drop = FALSE]),
        config = list(
          eap_reliability_min = round_log$eap_reliability_min[[idx]],
          theta_corr_min = round_log$theta_corr_min[[idx]],
          theta_sd_rel_change_max = round_log$theta_sd_rel_change_max[[idx]],
          rank_spearman_min = round_log$rank_spearman_min[[idx]]
        )
      )
    },
    logical(1)
  ))
})

test_that("step_log stage counters and quotas are reconstructable from logs", {
  items <- make_test_items(5)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")

  withr::local_seed(2)
  out <- adaptive_rank_run_live(
    state,
    judge,
    n_steps = 8L,
    progress = "none"
  )

  step_log <- adaptive_step_log(out)
  staged <- step_log[step_log$round_stage %in% c("anchor_link", "long_link", "mid_link", "local_link"), , drop = FALSE]
  if (nrow(staged) == 0L) {
    skip("No staged rows were generated in this short deterministic run.")
  }

  expected_so_far <- integer(nrow(staged))
  seen <- new.env(parent = emptyenv())
  for (idx in seq_len(nrow(staged))) {
    key <- paste(staged$round_id[[idx]], staged$round_stage[[idx]], sep = "::")
    prior <- if (exists(key, envir = seen, inherits = FALSE)) get(key, envir = seen) else 0L
    expected_so_far[[idx]] <- prior
    if (!is.na(staged$pair_id[[idx]])) {
      assign(key, prior + 1L, envir = seen)
    }
  }

  expect_equal(as.integer(staged$stage_committed_so_far), expected_so_far)

  quota_by_group <- split(staged$stage_quota, paste(staged$round_id, staged$round_stage, sep = "::"))
  stable_quota <- vapply(quota_by_group, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))
  expect_true(all(stable_quota))
})
