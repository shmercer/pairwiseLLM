# Tests for uncovered validation branches in stop_validators.R, stop_decision.R,
# and stop_reasons.R.

.make_min_stop_metrics <- function() {
  cols <- pairwiseLLM:::.required_stop_metrics()
  out <- rlang::set_names(vector("list", length(cols)), cols)

  # Fill with sensible defaults by name.
  out$engine <- "rank_centrality"
  out$n_items <- 1L
  out$n_total_items <- 1L
  out$n_matched <- 1L

  num_cols <- setdiff(cols, c("engine"))
  for (nm in num_cols) {
    out[[nm]] <- 0
  }

  tibble::as_tibble(out)
}

# ---- stop_validators.R ----

test_that("5306-01 validate_stop_metrics_tbl rejects non-1-row input", {
  metrics <- tibble::tibble(x = 1:2)
  expect_error(
    pairwiseLLM:::.validate_stop_metrics_tbl(metrics),
    "stop metrics must be a one-row tibble",
    fixed = TRUE
  )
})

test_that("5306-02 validate_stop_metrics_tbl rejects missing required columns", {
  metrics <- tibble::tibble(engine = "rank_centrality")
  expect_error(
    pairwiseLLM:::.validate_stop_metrics_tbl(metrics),
    "stop metrics missing required columns",
    fixed = TRUE
  )
})

test_that("5306-03 validate_stop_decision rejects missing required fields", {
  # Missing `stop`
  expect_error(
    pairwiseLLM:::.validate_stop_decision(list(reason = "precision_reached")),
    "stop decision must be a list with element `stop`.",
    fixed = TRUE
  )

  # Non-character `reason`
  expect_error(
    pairwiseLLM:::.validate_stop_decision(list(stop = TRUE, reason = 1)),
    "stop decision `reason` must be a scalar character",
    fixed = TRUE
  )
})

test_that("5306-04 validate_stop_decision rejects non-scalar stop", {
  decision <- list(stop = c(TRUE, FALSE), stop_reason = "precision_reached")
  expect_error(
    pairwiseLLM:::.validate_stop_decision(decision),
    "stop decision `stop` must be a non-missing scalar logical.",
    fixed = TRUE
  )
})

test_that("5306-05 validate_rounds_schema rejects missing columns", {
  rounds <- tibble::tibble(round = 1L)
  expect_error(
    pairwiseLLM:::.validate_rounds_schema(rounds),
    "rounds schema missing required columns:",
    fixed = TRUE
  )
})

test_that("5306-06 validate_rounds_schema rejects list stop_reason", {
  rounds <- tibble::tibble(
    round = 1L,
    n_new_pairs_scored = 0L,
    n_total_results = 0L,
    stop = FALSE,
    stop_reason = list("oops"),
    stop_blocked_by = NA_character_,
    stop_blocked_candidates = NA_character_,
    degree_min = 0,
    largest_component_frac = 1,
    rms_theta_delta = NA_real_,
    topk_overlap = 1
  )

  expect_error(
    pairwiseLLM:::.validate_rounds_schema(rounds),
    "rounds `stop_reason` must not be a list column",
    fixed = TRUE
  )
})

# ---- stop_decision.R ----

test_that("5306-07 .stop_decision validates stop_reason_priority type", {
  expect_error(
    pairwiseLLM:::.stop_decision(
      round = 10L,
      min_rounds = 1L,
      stop_reason_priority = 1
    ),
    "`stop_reason_priority` must be a character vector",
    fixed = TRUE
  )
})

test_that("5306-08 .stop_decision rejects unknown stop reasons", {
  expect_error(
    pairwiseLLM:::.stop_decision(
      round = 10L,
      min_rounds = 1L,
      stop_reason_priority = c("precision_reached", "made_up_reason")
    ),
    "contains unknown stop reasons",
    fixed = TRUE
  )
})

test_that("5306-09 .stop_decision respects stop_reason_priority ordering", {
  # Two candidates TRUE -> chosen reason should follow priority.
  d_default <- pairwiseLLM:::.stop_decision(
    round = 10L,
    min_rounds = 1L,
    budget_exhausted = TRUE,
    max_rounds_reached = TRUE
  )
  expect_equal(d_default$reason, "pair_budget_exhausted")

  d_custom <- pairwiseLLM:::.stop_decision(
    round = 10L,
    min_rounds = 1L,
    budget_exhausted = TRUE,
    max_rounds_reached = TRUE,
    stop_reason_priority = c("max_rounds_reached", "pair_budget_exhausted")
  )
  expect_equal(d_custom$reason, "max_rounds_reached")
})

# ---- stop_reasons.R ----

test_that("5306-10 .bt_resolve_stop_reason maps legacy stopped to precision", {
  reason <- pairwiseLLM:::.bt_resolve_stop_reason(stopped = TRUE)
  expect_equal(reason, "precision_reached")
})
