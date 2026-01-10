# Workstream B: Hybrid Stage 1 should not stop due to BT precision/stability stop rules

test_that("hybrid Stage 1 does not stop early when BT precision would stop", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = c("tA", "tB", "tC", "tD")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = pairs$ID1,
      ID2 = pairs$ID2,
      better_id = pairs$ID1
    )
  }

  fit_fun <- function(bt_data, ...) {
    ids <- sort(unique(c(bt_data$object1, bt_data$object2)))

    theta_tbl <- tibble::tibble(
      ID = ids,
      theta = seq_along(ids),
      se = rep(0.01, length(ids))
    )

    list(
      engine = "mock",
      theta = theta_tbl,
      reliability = 0.99,
      diagnostics = list(sepG = 10)
    )
  }

  # Force BT precision to be "reached" every round.
  testthat::local_mocked_bindings(
    bt_should_stop = function(...) {
      list(
        stop = TRUE,
        details = tibble::tibble(),
        improve = tibble::tibble()
      )
    },
    .package = "pairwiseLLM"
  )

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "hybrid",
    init_round_size = 2,
    round_size = 2,
    min_rounds = 1,
    max_rounds = 2,
    seed_pairs = 1,
    # Ensure hybrid never switches to stage2 in this test.
    stage1_min_degree_median = 999,
    stage1_min_degree_min_lcc = 999
  )

  expect_equal(nrow(out$rounds), 2L)
  expect_equal(out$stop_reason, "max_rounds_reached")

  expect_true(all(out$rounds$pairing_stage == "stage1_rc"))
  expect_true(all(out$rounds$stage == "stage1_rc"))
  expect_true(all(out$rounds$stage == out$rounds$pairing_stage))

  # Even though `bt_should_stop()` reports stop=TRUE, stage1 should not treat that as a stop condition.
  expect_true(all(out$rounds$precision_reached == FALSE))
  expect_false(any(stats::na.omit(out$rounds$stop_reason) == "precision_reached"))
})
