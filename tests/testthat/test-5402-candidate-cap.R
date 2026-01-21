testthat::test_that("generate_candidates caps deterministically at C_max", {
  withr::local_seed(1)
  samples <- tibble::tibble(
    ID = LETTERS[1:10],
    text = paste("text", LETTERS[1:10])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  config <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(W = 9L, A_anchors = 10L, C_max = 7L)
  )
  state$config$v3 <- config

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = seq(10, 1),
    theta_sd = rep(0.4, state$N)
  )

  out1 <- pairwiseLLM:::generate_candidates(theta_summary, state, config)
  out2 <- pairwiseLLM:::generate_candidates(theta_summary, state, config)

  expect_equal(nrow(out1), config$C_max)
  expect_equal(out1, out2)
})

testthat::test_that("generate_candidates_from_anchors enforces C_max and cap", {
  samples <- tibble::tibble(
    ID = LETTERS[1:6],
    text = paste("text", LETTERS[1:6])
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 3)
  config <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(W = 5L, C_max = 4L)
  )

  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = seq(6, 1),
    theta_sd = rep(0.3, state$N)
  )

  config_bad <- config
  config_bad$C_max <- 0L
  testthat::expect_error(
    pairwiseLLM:::generate_candidates_from_anchors(
      anchors = state$ids,
      theta_summary = theta_summary,
      state = state,
      config = config_bad
    ),
    "C_max"
  )

  out <- pairwiseLLM:::generate_candidates_from_anchors(
    anchors = state$ids,
    theta_summary = theta_summary,
    state = state,
    config = config
  )
  testthat::expect_equal(nrow(out), config$C_max)
  testthat::expect_equal(names(out), c("i", "j"))
})
