test_that("validate_config_v3 rejects invalid fields", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(6)

  expect_error(pairwiseLLM:::validate_config_v3(1), "config")

  bad_W <- cfg
  bad_W$W <- 0L
  expect_error(pairwiseLLM:::validate_config_v3(bad_W), "W")

  bad_explore <- cfg
  bad_explore$explore_rate <- 1.2
  expect_error(pairwiseLLM:::validate_config_v3(bad_explore), "explore_rate")

  bad_min_degree <- cfg
  bad_min_degree$min_degree <- 1L
  expect_error(pairwiseLLM:::validate_config_v3(bad_min_degree), "min_degree")

  bad_target_degree <- cfg
  bad_target_degree$target_mean_degree <- cfg$N + 1
  expect_error(pairwiseLLM:::validate_config_v3(bad_target_degree), "target_mean_degree")

  bad_cap <- cfg
  bad_cap$hard_cap_frac <- 1.2
  expect_error(pairwiseLLM:::validate_config_v3(bad_cap), "hard_cap_frac")

  bad_output <- cfg
  bad_output$output_dir <- 123
  expect_error(pairwiseLLM:::validate_config_v3(bad_output), "output_dir")
})

test_that("validate_state_v3 rejects inconsistent counters", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "beta", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  cfg <- pairwiseLLM:::adaptive_v3_defaults(state$N)

  expect_silent(pairwiseLLM:::validate_state_v3(state, cfg))

  state_bad <- state
  state_bad$deg[["A"]] <- -1L
  expect_error(pairwiseLLM:::validate_state_v3(state_bad, cfg), "non-negative")

  state_bad <- state
  state_bad$pair_count <- stats::setNames(1L, "A:A")
  expect_error(pairwiseLLM:::validate_state_v3(state_bad, cfg), "self-pairs")

  state_bad <- state
  state_bad$mode <- "unknown"
  expect_error(pairwiseLLM:::validate_state_v3(state_bad, cfg), "state\\$mode")

  expect_error(pairwiseLLM:::validate_state_v3(list(), cfg), "adaptive_state")
  expect_error(pairwiseLLM:::validate_state_v3(state, "bad"), "config")
})

test_that("adaptive_v3_config normalizes overrides", {
  cfg <- pairwiseLLM:::adaptive_v3_config(6, NULL)
  expect_true(is.list(cfg))

  expect_error(pairwiseLLM:::adaptive_v3_config(6, 1), "overrides")
})

test_that("adaptive_v3 tau function uses N scaling", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(10)
  tau <- cfg$tau_fn(10)
  expect_true(is.numeric(tau))
  expect_true(tau >= 0.8 && tau <= 3.0)
})
