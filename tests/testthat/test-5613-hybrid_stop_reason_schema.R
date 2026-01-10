# Workstream B: stop_reason + stage bookkeeping schema should be stable

test_that("bt_run_adaptive always includes stop_reason and stage bookkeeping columns", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )

  judge_fun <- function(pairs) {
    tibble::tibble(
      ID1 = as.character(pairs$ID1),
      ID2 = as.character(pairs$ID2),
      better_id = as.character(pairs$ID1)
    )
  }

  fit_fun <- function(bt_data, ...) {
    bt_data <- tibble::as_tibble(bt_data)
    ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))

    theta_tbl <- tibble::tibble(
      ID = ids,
      theta = seq_along(ids),
      se = rep(1, length(ids))
    )

    list(
      engine = "mock",
      theta = theta_tbl,
      reliability = 0.5,
      diagnostics = list()
    )
  }

  out <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    fit_engine_running = "hybrid",
    init_round_size = 0,
    round_size = 0,
    max_rounds = 0
  )

  known <- pairwiseLLM:::known_stop_reasons()

  expect_true(out$stop_reason %in% known)
  expect_true(all(c("stop_reason", "pairing_stage", "stage", "stage1_rounds", "stage2_rounds") %in% names(out$rounds)))
  expect_true("stop_reason" %in% names(out$pairing_diagnostics))

  expect_true(is.character(out$rounds$stop_reason))
  expect_true(is.character(out$rounds$stage))
  expect_true(is.integer(out$rounds$stage1_rounds))
  expect_true(is.integer(out$rounds$stage2_rounds))

  expect_true(all(stats::na.omit(out$rounds$stop_reason) %in% known))
  expect_true(all(stats::na.omit(out$pairing_diagnostics$stop_reason) %in% known))
})
