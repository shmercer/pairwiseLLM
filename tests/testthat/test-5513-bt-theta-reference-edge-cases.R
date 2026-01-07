test_that("5513 win scoring handles empty data and missing winner/result gracefully", {
  empty_bt <- tibble::tibble(
    object1 = character(),
    object2 = character()
  )

  wins_empty <- pairwiseLLM:::.bt_win_scores(empty_bt)
  expect_true(is.numeric(wins_empty))
  expect_equal(length(wins_empty), 0L)
  expect_true(is.character(names(wins_empty)))

  # Missing winner columns should yield a named numeric(0)
  no_winner_bt <- tibble::tibble(
    object1 = c("A", "A"),
    object2 = c("B", "B")
  )
  wins_nowinner <- pairwiseLLM:::.bt_win_scores(no_winner_bt)
  expect_equal(length(wins_nowinner), 2L)
  expect_equal(unname(wins_nowinner), c(0, 0))
})

test_that("5513 theta orientation flips to align with win scores when correlation is negative", {
  bt_data <- tibble::tibble(
    object1 = c("A", "A"),
    object2 = c("B", "B"),
    result = c(1, 1) # A wins
  )
  theta_tbl <- tibble::tibble(
    ID = c("A", "B"),
    theta = c(-2, 2),
    theta_linked = c(-2, 2)
  )

  orient <- pairwiseLLM:::.bt_orient_theta_by_wins(theta_tbl, bt_data)
  expect_true(tibble::is_tibble(orient$theta))
  expect_equal(orient$theta$theta, c(2, -2))
  expect_equal(orient$theta$theta_linked, c(2, -2))
})

test_that("5513 normalize_theta_tbl uses robust fallbacks when scale is near-zero", {
  theta_flat <- tibble::tibble(ID = c("A", "B", "C"), theta = c(5, 5, 5))

  out_mad <- pairwiseLLM:::.bt_normalize_theta_tbl(
    theta_flat,
    center = TRUE,
    scale = TRUE,
    scale_method = "median_mad",
    max_abs = 6
  )
  expect_true(is.list(out_mad))
  expect_true("theta" %in% names(out_mad))
  expect_true(tibble::is_tibble(out_mad$theta))
  expect_true(is.finite(out_mad$center))
  expect_true(is.finite(out_mad$scale))
  expect_equal(out_mad$scale, 1)

  out_sd <- pairwiseLLM:::.bt_normalize_theta_tbl(
    theta_flat,
    center = TRUE,
    scale = TRUE,
    scale_method = "mean_sd",
    max_abs = 6
  )
  expect_equal(out_sd$scale, 1)
})