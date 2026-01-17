test_that("validate_config_v3 rejects invalid fields", {
  cfg <- pairwiseLLM:::adaptive_v3_defaults(6)

  bad_W <- cfg
  bad_W$W <- 0L
  expect_error(pairwiseLLM:::validate_config_v3(bad_W), "W")

  bad_explore <- cfg
  bad_explore$explore_rate <- 1.2
  expect_error(pairwiseLLM:::validate_config_v3(bad_explore), "explore_rate")

  bad_cap <- cfg
  bad_cap$hard_cap_frac <- 1.2
  expect_error(pairwiseLLM:::validate_config_v3(bad_cap), "hard_cap_frac")
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
})
