test_that(".bt_round_state returns zeros for empty results and applies prefix", {
  out <- pairwiseLLM:::.bt_round_state(
    results = tibble::tibble(ID1 = character(), ID2 = character()),
    ids = c("A", "B"),
    prefix = "new_"
  )

  expect_true(inherits(out, "tbl_df"))
  expect_equal(nrow(out), 1)

  # Prefixed names exist
  expect_true(all(startsWith(names(out), "new_")))

  # A few key fields are zeros/NA as expected for empty input
  expect_equal(out$new_n_results[[1]], 0L)
  expect_equal(out$new_n_unique_unordered_pairs[[1]], 0L)
  expect_equal(out$new_n_ids[[1]], 2L)
  expect_equal(out$new_n_ids_seen[[1]], 0L)
  expect_true(is.na(out$new_n_unique_unordered_pairs_in_ids[[1]]))
})

test_that(".bt_round_state errors when results lacks ID1/ID2", {
  bad <- tibble::tibble(x = "A", y = "B")

  expect_error(
    pairwiseLLM:::.bt_round_state(results = bad),
    "results.*ID1.*ID2"
  )
})

test_that(".bt_round_state derives ids when ids=NULL and computes appearances/imbalance", {
  res <- tibble::tibble(
    ID1 = c("A", "A", "B", "C"),
    ID2 = c("B", "C", "C", "A"),
    better_id = c("A", "A", "C", "A"),
    judge = c("j1", "j1", "j2", "j2")
  )

  out <- pairwiseLLM:::.bt_round_state(results = res, ids = NULL, judge_col = "judge")

  expect_equal(out$n_results[[1]], 4L)
  expect_equal(out$n_ids[[1]], 3L) # A,B,C universe
  expect_equal(out$n_ids_seen[[1]], 3L)
  expect_true(out$n_unique_unordered_pairs[[1]] >= 1L)
  expect_equal(out$n_judges[[1]], 2L)

  # imbalance should be non-negative integer
  expect_true(is.numeric(out$pos_imbalance_max[[1]]))
  expect_true(out$pos_imbalance_max[[1]] >= 0L)
})

test_that(".bt_round_state safe_q returns 0 when derived ids universe is empty", {
  # ids=NULL, but all IDs are NA/"" so ids_u becomes length 0 after filtering
  res <- tibble::tibble(
    ID1 = c(NA_character_, ""),
    ID2 = c("", NA_character_),
    better_id = c(NA_character_, NA_character_)
  )

  out <- pairwiseLLM:::.bt_round_state(results = res, ids = NULL)

  expect_equal(out$n_results[[1]], 2L)
  expect_equal(out$n_ids[[1]], 0L) # empty derived universe
  expect_equal(out$n_ids_seen[[1]], 0L)

  # appearance quantiles use safe_q => return 0L when x length is 0
  expect_equal(out$p10_appearances[[1]], 0L)
  expect_equal(out$median_appearances[[1]], 0L)
  expect_equal(out$p90_appearances[[1]], 0L)
})
