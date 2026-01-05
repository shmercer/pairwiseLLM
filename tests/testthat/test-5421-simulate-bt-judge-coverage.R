test_that("simulate_bt_judge validates inputs", {
  pairs <- tibble::tibble(ID1 = c("A", "B"), ID2 = c("B", "A")) |>
    dplyr::mutate(text1 = paste0("t", .data$ID1), text2 = paste0("t", .data$ID2))
  true_theta <- stats::setNames(c(1, 0), c("A", "B"))

  expect_error(
    pairwiseLLM::simulate_bt_judge(tibble::tibble(ID1 = "A"), true_theta),
    "must contain columns"
  )

  expect_error(
    pairwiseLLM::simulate_bt_judge(pairs, c(1, 0)),
    "true_theta must be a named numeric"
  )

  expect_error(
    pairwiseLLM::simulate_bt_judge(pairs, true_theta, judge_col = 1),
    "judge_col must be a single character"
  )

  expect_error(
    pairwiseLLM::simulate_bt_judge(pairs, true_theta, judges = c("", "j2")),
    "judges must be a character vector of non-empty strings"
  )
})


test_that("simulate_bt_judge assigns judge labels deterministically", {
  # Many pairs so random assignment (round_robin = FALSE) hits multiple judges.
  pairs <- tibble::tibble(
    ID1 = rep(c("A", "B", "A", "B"), 10),
    ID2 = rep(c("B", "A", "B", "A"), 10)
  ) |>
    dplyr::mutate(text1 = paste0("t", .data$ID1), text2 = paste0("t", .data$ID2))
  true_theta <- stats::setNames(c(1, 0), c("A", "B"))

  withr::local_seed(123)
  out <- pairwiseLLM::simulate_bt_judge(
    pairs,
    true_theta,
    judges = c("j1", "j2"),
    judge_col = "judge",
    round_robin = FALSE
  )

  expect_true("judge" %in% names(out))
  expect_true(all(out$judge %in% c("j1", "j2")))
  expect_true(length(unique(out$judge)) > 1)
})
