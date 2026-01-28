test_that("validate_config rejects invalid fields", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(6)

  expect_error(pairwiseLLM:::validate_config(1), "config")

  bad_W <- cfg
  bad_W$W <- 0L
  expect_error(pairwiseLLM:::validate_config(bad_W), "W")

  bad_explore <- cfg
  bad_explore$explore_rate <- 1.2
  expect_error(pairwiseLLM:::validate_config(bad_explore), "explore_rate")

  bad_min_degree <- cfg
  bad_min_degree$min_degree <- 1L
  expect_error(pairwiseLLM:::validate_config(bad_min_degree), "min_degree")

  bad_target_degree <- cfg
  bad_target_degree$target_mean_degree <- cfg$N + 1
  expect_error(pairwiseLLM:::validate_config(bad_target_degree), "target_mean_degree")

  bad_cap <- cfg
  bad_cap$hard_cap_frac <- 1.2
  expect_error(pairwiseLLM:::validate_config(bad_cap), "hard_cap_frac")

  bad_eap <- cfg
  bad_eap$eap_reliability_min <- 1.5
  expect_error(pairwiseLLM:::validate_config(bad_eap), "eap_reliability_min")

  bad_lag <- cfg
  bad_lag$stability_lag <- 0L
  expect_error(pairwiseLLM:::validate_config(bad_lag), "stability_lag")

  bad_theta_corr <- cfg
  bad_theta_corr$theta_corr_min <- -0.1
  expect_error(pairwiseLLM:::validate_config(bad_theta_corr), "theta_corr_min")

  bad_theta_sd <- cfg
  bad_theta_sd$theta_sd_rel_change_max <- -0.1
  expect_error(pairwiseLLM:::validate_config(bad_theta_sd), "theta_sd_rel_change_max")

  bad_rank_corr <- cfg
  bad_rank_corr$rank_spearman_min <- 1.5
  expect_error(pairwiseLLM:::validate_config(bad_rank_corr), "rank_spearman_min")

  bad_output <- cfg
  bad_output$output_dir <- 123
  expect_error(pairwiseLLM:::validate_config(bad_output), "output_dir")
})

test_that("validate_state rejects inconsistent counters", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  expect_silent(pairwiseLLM:::validate_state(state))

  state_bad <- state
  state_bad$deg[["A"]] <- -1L
  expect_error(pairwiseLLM:::validate_state(state_bad), "pos1")

  state_bad <- state
  state_bad$pair_count <- stats::setNames(1L, "A:A")
  expect_error(pairwiseLLM:::validate_state(state_bad), "self-pairs")

  state_bad <- state
  state_bad$mode <- "unknown"
  expect_error(pairwiseLLM:::validate_state(state_bad), "state\\$mode")

  expect_error(pairwiseLLM:::validate_state(list()), "adaptive_state")
})

test_that("adaptive_v3_config normalizes overrides", {
  cfg <- pairwiseLLM:::adaptive_v3_config(6, NULL)
  expect_true(is.list(cfg))

  expect_error(pairwiseLLM:::adaptive_v3_config(6, 1), "overrides")
})
