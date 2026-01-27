test_that("adaptive_v3_defaults includes required fields", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(6)
  required <- c(
    "N", "W", "A_anchors", "C_max",
    "refit_B", "batch_size", "explore_rate",
    "min_degree", "target_mean_degree",
    "dup_p_margin", "dup_max_count", "dup_utility_quantile",
    "hard_cap_frac",
    "eap_reliability_min", "min_refits_for_stability", "stability_lag",
    "theta_corr_min", "theta_sd_rel_change_max", "rank_spearman_min",
    "max_rhat", "min_ess_bulk", "min_ess_bulk_near_stop",
    "require_divergences_zero", "repair_max_cycles",
    "progress", "progress_every_iter", "progress_every_refit", "progress_level",
    "write_outputs", "output_dir", "keep_draws", "thin_draws"
  )

  expect_true(all(required %in% names(cfg)))
})

test_that("adaptive_v3_defaults scales with N", {
  for (N in c(6L, 12L, 30L)) {
    cfg <- pairwiseLLM:::adaptive_v3_defaults(N)
    W <- max(5L, min(60L, as.integer(round(2 * sqrt(N)))))
    batch_size <- max(10L, min(80L, as.integer(round(4 * sqrt(N)))))
    refit_B <- max(100L, min(800L, as.integer(round(10 * sqrt(N)))))
    anchors <- max(25L, min(120L, as.integer(round(2 * sqrt(N)))))
    explore_rate <- max(0.10, min(0.25, 0.20 - 0.02 * log10(N)))

    expect_equal(cfg$W, W)
    expect_equal(cfg$batch_size, batch_size)
    expect_equal(cfg$refit_B, refit_B)
    expect_equal(cfg$A_anchors, anchors)
    expect_equal(cfg$explore_rate, explore_rate)
    expect_equal(cfg$hard_cap_frac, 0.40)
    expect_equal(cfg$eap_reliability_min, 0.95)
    expect_equal(cfg$min_refits_for_stability, 3L)
    expect_equal(cfg$stability_lag, 2L)
    expect_equal(cfg$theta_corr_min, 0.995)
    expect_equal(cfg$theta_sd_rel_change_max, 0.01)
    expect_equal(cfg$rank_spearman_min, 0.995)
  }
})

test_that("adaptive_v3_config merges overrides", {
  cfg <- pairwiseLLM:::adaptive_v3_config(12, batch_size = 55L, explore_rate = 0.15)

  expect_equal(cfg$batch_size, 55L)
  expect_equal(cfg$explore_rate, 0.15)

  cfg2 <- pairwiseLLM:::adaptive_v3_config(12, list(batch_size = 33L))
  expect_equal(cfg2$batch_size, 33L)
})

test_that("adaptive_v3_config handles NULL overrides", {
  cfg <- pairwiseLLM:::adaptive_v3_config(6, NULL)
  expect_equal(cfg$N, 6L)
})

test_that("adaptive_round_log_defaults returns typed NA row", {
  defaults <- pairwiseLLM:::.adaptive_round_log_defaults()
  expect_equal(nrow(defaults), 1L)
  expect_true(is.double(defaults$epsilon_mean))
  expect_true(all(is.na(defaults$epsilon_mean)))
  expect_true(is.logical(defaults$diagnostics_pass))
  expect_true(is.na(defaults$diagnostics_pass[[1L]]))
})
