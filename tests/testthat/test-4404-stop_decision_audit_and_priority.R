
test_that("4404-01 stop audit: precision reached but graph unhealthy does not stop", {
  stop_chk <- pairwiseLLM:::.stop_decision(
    round = 3,
    min_rounds = 2,
    no_new_pairs = FALSE,
    budget_exhausted = FALSE,
    max_rounds_reached = FALSE,
    graph_healthy = FALSE,
    stability_reached = FALSE,
    precision_reached = TRUE
  )

  aud <- pairwiseLLM:::.stop_decision_record(
    round_index = 3,
    stop_chk = stop_chk,
    precision_reached = TRUE,
    stability_reached = FALSE,
    graph_healthy = FALSE,
    min_rounds_satisfied = TRUE,
    mixing_guard_pass = NA,
    no_new_pairs = FALSE,
    max_rounds_hit = FALSE,
    pair_budget_exhausted = FALSE
  )

  expect_s3_class(aud, "tbl_df")
  expect_equal(nrow(aud), 1L)
  expect_false(aud$stop_decision[[1]])
  expect_true(aud$precision_reached[[1]])
  expect_false(aud$graph_healthy[[1]])
})

test_that("4404-02 stop audit: stability reached but min rounds not met does not stop", {
  stop_chk <- pairwiseLLM:::.stop_decision(
    round = 1,
    min_rounds = 2,
    no_new_pairs = FALSE,
    budget_exhausted = FALSE,
    max_rounds_reached = FALSE,
    graph_healthy = TRUE,
    stability_reached = TRUE,
    precision_reached = FALSE
  )

  aud <- pairwiseLLM:::.stop_decision_record(
    round_index = 1,
    stop_chk = stop_chk,
    precision_reached = FALSE,
    stability_reached = TRUE,
    graph_healthy = TRUE,
    min_rounds_satisfied = FALSE,
    mixing_guard_pass = NA,
    no_new_pairs = FALSE,
    max_rounds_hit = FALSE,
    pair_budget_exhausted = FALSE
  )

  expect_false(aud$stop_decision[[1]])
  expect_true(aud$stability_reached[[1]])
  expect_false(aud$min_rounds_satisfied[[1]])
})

test_that("4404-03 hard stop: no_new_pairs overrides precision/stability", {
  stop_chk <- pairwiseLLM:::.stop_decision(
    round = 3,
    min_rounds = 2,
    no_new_pairs = TRUE,
    budget_exhausted = FALSE,
    max_rounds_reached = FALSE,
    graph_healthy = FALSE,
    stability_reached = TRUE,
    precision_reached = TRUE
  )

  aud <- pairwiseLLM:::.stop_decision_record(
    round_index = 3,
    stop_chk = stop_chk,
    precision_reached = TRUE,
    stability_reached = TRUE,
    graph_healthy = FALSE,
    min_rounds_satisfied = TRUE,
    mixing_guard_pass = NA,
    no_new_pairs = TRUE,
    max_rounds_hit = FALSE,
    pair_budget_exhausted = FALSE
  )

  expect_true(aud$stop_decision[[1]])
  expect_equal(aud$stop_reason[[1]], "no_new_pairs")
})

test_that("4404-04 runner outputs include schema-stable stop_audit", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", 1:4)
  )

  judge_fun <- function(pairs) {
    pairs %>%
      dplyr::mutate(
        better_id = dplyr::if_else(.data$ID1 < .data$ID2, .data$ID2, .data$ID1)
      )
  }

  # Mock fit function used by multiple runner tests in the package.
  fit_fun <- function(bt, engine, ...) {
    ids <- sort(unique(c(bt$ID1, bt$ID2)))
    wins <- rep(1, length(ids))
    names(wins) <- ids
    theta <- as.double(seq_along(ids))
    se <- rep(0.1, length(ids))
    list(
      status = "succeeded",
      engine_used = engine,
      reliability = 0.95,
      theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
      diagnostics = list(sepG = 3.5)
    )
  }

  out_adaptive <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 2,
    init_round_size = 2,
    max_rounds = 1,
    min_rounds = 1,
    final_refit = FALSE,
    # Stop-audit schema should be independent of the running engine.
    # Use the BT running engine here to keep the test focused and deterministic.
    fit_engine_running = "bt",
    fit_engine_final = "none",
    forbid_repeats = FALSE
  )

  expect_true("stop_audit" %in% names(out_adaptive))
  expect_s3_class(out_adaptive$stop_audit, "tbl_df")

  expected_cols <- c(
    "round_index",
    "stop_decision",
    "stop_reason",
    "precision_reached",
    "stability_reached",
    "graph_healthy",
    "min_rounds_satisfied",
    "mixing_guard_pass",
    "no_new_pairs",
    "max_rounds_hit",
    "pair_budget_exhausted"
  )
  expect_true(all(expected_cols %in% names(out_adaptive$stop_audit)))

  out_acl <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("A", "B", "C", "D")),
    judge_fun = judge_fun,
    core_ids = c("A", "B", "C"),
    fit_fun = fit_fun,
    engine = "mock",
    round_size = 2,
    init_round_size = 2,
    max_rounds_per_batch = 1,
    min_rounds = 1,
    final_refit = FALSE,
    linking = "never",
    fit_engine_running = "bt",
    forbid_repeats = FALSE
  )

  expect_true("stop_audit" %in% names(out_acl))
  expect_s3_class(out_acl$stop_audit, "tbl_df")
  expect_true(all(expected_cols %in% names(out_acl$stop_audit)))
})
