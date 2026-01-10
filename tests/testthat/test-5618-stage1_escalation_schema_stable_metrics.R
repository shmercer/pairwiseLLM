# Workstream E: Stage 1 escalation fields must be schema-stable

test_that("stage1 escalation fields are present and stable in rounds and metrics", {
  withr::local_seed(1)

  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("tA", "tB", "tC")
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
    list(
      engine = "mock",
      theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.1, length(ids))),
      reliability = 0.99,
      diagnostics = list(sepG = 10)
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "bt",
    init_round_size = 1,
    round_size = 1,
    min_rounds = 1,
    max_rounds = 1,
    seed_pairs = 1,
    final_refit = FALSE
  )

  expect_true(all(c("stage1_escalated", "stage1_escalation_round") %in% names(out$rounds)))
  expect_true(is.logical(out$rounds$stage1_escalated))
  expect_true(is.integer(out$rounds$stage1_escalation_round))
  expect_false(any(out$rounds$stage1_escalated))
  expect_true(all(is.na(out$rounds$stage1_escalation_round)))

  expect_true(all(c("stage1_escalated", "stage1_escalation_round") %in% names(out$metrics)))
  expect_true(is.logical(out$metrics$stage1_escalated))
  expect_true(is.integer(out$metrics$stage1_escalation_round))
})
