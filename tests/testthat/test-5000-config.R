test_that("adaptive_v3_defaults includes required fields", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(6)
  required <- c(
    "N", "W", "A_anchors", "C_max",
    "refit_B", "batch_size", "explore_rate",
    "dup_p_margin", "dup_max_count", "dup_utility_quantile",
    "hard_cap_frac",
    "S_subset", "tau_fn", "K_top", "U_abs", "checks_passed_target",
    "max_rhat", "min_ess_bulk", "min_ess_bulk_near_stop",
    "require_divergences_zero", "repair_max_cycles",
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
    S_subset <- max(100L, min(400L, as.integer(round(6 * sqrt(N)))))
    K_top <- max(20L, min(200L, as.integer(round(2 * W))))
    explore_rate <- max(0.10, min(0.25, 0.20 - 0.02 * log10(N)))
    U_abs <- max(0.0015, min(0.006, 0.004 * (30 / N)^0.25))

    expect_equal(cfg$W, W)
    expect_equal(cfg$batch_size, batch_size)
    expect_equal(cfg$refit_B, refit_B)
    expect_equal(cfg$A_anchors, anchors)
    expect_equal(cfg$S_subset, S_subset)
    expect_equal(cfg$K_top, K_top)
    expect_equal(cfg$explore_rate, explore_rate)
    expect_equal(cfg$U_abs, U_abs)
    expect_equal(cfg$hard_cap_frac, 0.40)
  }
})

test_that("adaptive_v3_config merges overrides", {
  cfg <- pairwiseLLM:::adaptive_v3_config(12, batch_size = 55L, explore_rate = 0.15)

  expect_equal(cfg$batch_size, 55L)
  expect_equal(cfg$explore_rate, 0.15)

  cfg2 <- pairwiseLLM:::adaptive_v3_config(12, list(batch_size = 33L))
  expect_equal(cfg2$batch_size, 33L)
})
