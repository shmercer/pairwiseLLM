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
    "uncertainty_concentration",
    "top_boundary_uncertainty",
    "adjacent_separation_uncertainty",
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
  expect_true(is.double(round_log$uncertainty_concentration))
  expect_true(is.double(round_log$top_boundary_uncertainty))
  expect_true(is.double(round_log$adjacent_separation_uncertainty))
  expect_true(is.integer(round_log$mcmc_chains))
  expect_true(is.integer(round_log$mcmc_parallel_chains))
  reject_rate <- round_log$star_cap_reject_rate_since_last_refit
  reject_rate <- reject_rate[!is.na(reject_rate)]
  expect_true(all(reject_rate >= 0))
  expect_true(all(reject_rate <= 1))
})
