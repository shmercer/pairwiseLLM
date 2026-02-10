make_legacy_state_fixture <- function(ids = c("A", "B", "C")) {
  ids <- as.character(ids)
  z <- stats::setNames(integer(length(ids)), ids)
  structure(list(
    N = as.integer(length(ids)),
    ids = ids,
    unordered_count = integer(),
    ordered_seen = logical(),
    pair_ordered_count = integer(),
    pos1 = z,
    pos2 = z,
    pos_count = z,
    deg = z,
    imb = z,
    pair_count = integer(),
    history_results = pairwiseLLM:::.adaptive_empty_results_tbl(),
    comparisons_observed = 0L,
    comparisons_scheduled = 0L,
    new_since_refit = 0L,
    posterior = list(stop_metrics = list())
  ), class = "adaptive_state")
}

make_results_fixture <- function() {
  tibble::tibble(
    pair_uid = c("A:B#1", "A:C#1"),
    unordered_key = c("A:B", "A:C"),
    ordered_key = c("A:B", "A:C"),
    A_id = c("A", "A"),
    B_id = c("B", "C"),
    better_id = c("A", "C"),
    winner_pos = c(1L, 2L),
    phase = c("phase2", "phase2"),
    iter = c(1L, 2L),
    received_at = as.POSIXct(c("2026-01-01 00:00:00", "2026-01-01 00:00:01"), tz = "UTC"),
    backend = c("openai", "openai"),
    model = c("gpt-test", "gpt-test")
  )
}

test_that("core budget helpers validate and clamp as expected", {
  expect_error(pairwiseLLM:::compute_core_budget(core_fraction = -1), "non-negative")
  expect_error(pairwiseLLM:::compute_core_budget(min_cores = 0), "positive")
  expect_error(pairwiseLLM:::compute_core_budget(max_cores = 0), "positive")

  out <- pairwiseLLM:::compute_core_budget(core_fraction = 0.5, max_cores = 3L, min_cores = 2L)
  expect_true(is.integer(out))
  expect_true(out >= 1L)

  expect_error(pairwiseLLM:::compute_batch_sizes(0), "positive")
  sizes <- pairwiseLLM:::compute_batch_sizes(100L, overrides = list(BATCH2 = 999L))
  expect_identical(sizes$BATCH2, 999L)
  expect_true(all(c("BATCH1", "BATCH2", "BATCH3", "CW") %in% names(sizes)))
})

test_that("detect_physical_cores falls back to logical and then 1", {
  logical_only <- testthat::with_mocked_bindings(
    detectCores = function(logical) {
      if (isTRUE(logical)) 6L else NA_integer_
    },
    pairwiseLLM:::detect_physical_cores(),
    .package = "parallel"
  )
  expect_identical(logical_only, 6L)

  fallback_one <- testthat::with_mocked_bindings(
    detectCores = function(logical) stop("boom"),
    pairwiseLLM:::detect_physical_cores(),
    .package = "parallel"
  )
  expect_identical(fallback_one, 1L)
})

test_that("draw sanitization handles invalid types and non-finite replacement", {
  expect_error(pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(1:3), "numeric matrix")
  expect_error(
    pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(matrix(numeric(), nrow = 0, ncol = 0)),
    "at least one row and column"
  )

  draws <- matrix(c(1, 2, Inf, 4, NaN, 6), nrow = 3)
  expect_warning(
    pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws, name = "x"),
    "Non-finite values detected"
  )
  clean <- suppressWarnings(pairwiseLLM:::.pairwiseLLM_sanitize_draws_matrix(draws, name = "x"))
  expect_true(all(is.finite(clean)))
  expect_equal(colnames(clean), c("1", "2"))
})

test_that("model variant helpers validate and map flags", {
  expect_error(pairwiseLLM:::normalize_model_variant(NA_character_), "length-1 character")
  expect_error(pairwiseLLM:::normalize_model_variant("bad"), "must be one of")

  expect_identical(pairwiseLLM:::normalize_model_variant("btl_e_b"), "btl_e_b")
  expect_true(pairwiseLLM:::model_has_e("btl_e"))
  expect_false(pairwiseLLM:::model_has_e("btl_b"))
  expect_true(pairwiseLLM:::model_has_b("btl_b"))
  expect_false(pairwiseLLM:::model_has_b("btl"))
})

test_that("legacy state table/schema helpers behave deterministically", {
  pairs <- pairwiseLLM:::.adaptive_empty_pairs_tbl()
  results <- pairwiseLLM:::.adaptive_empty_results_tbl()
  failed <- pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
  expect_equal(nrow(pairs), 0L)
  expect_equal(nrow(results), 0L)
  expect_equal(nrow(failed), 0L)

  keys <- pairwiseLLM:::.adaptive_unordered_keys(c("A", "B", "C"))
  expect_equal(sort(keys), sort(c("A:B", "A:C", "B:C")))
  expect_identical(pairwiseLLM:::.adaptive_unordered_keys("A"), character())

  expect_true(is.na(pairwiseLLM:::.adaptive_log_default_value(raw())))
  expect_identical(pairwiseLLM:::.adaptive_log_default_value(integer()), NA_integer_)
  expect_identical(pairwiseLLM:::.adaptive_log_default_value(double()), NA_real_)
  expect_identical(pairwiseLLM:::.adaptive_log_default_value(logical()), NA)
  expect_identical(pairwiseLLM:::.adaptive_log_default_value(character()), NA_character_)
  expect_true(inherits(pairwiseLLM:::.adaptive_log_default_value(as.POSIXct(character(), tz = "UTC")), "POSIXct"))

  schema <- tibble::tibble(a = integer(), b = character())
  aligned <- pairwiseLLM:::.adaptive_align_log_schema(tibble::tibble(a = 1L), schema)
  expect_true("b" %in% names(aligned))
  aligned2 <- pairwiseLLM:::.adaptive_align_log_schema(NULL, schema)
  expect_equal(names(aligned2), names(schema))

  state <- make_legacy_state_fixture()
  state$config <- list()
  state$batch_log <- NULL
  state$posterior <- list()
  state$log_counters <- 1L
  state$failed_attempts <- pairwiseLLM:::.adaptive_empty_failed_attempts_tbl()
  state <- pairwiseLLM:::.adaptive_state_init_logs(state)
  expect_true(is.list(state$posterior$stop_metrics))
  expect_true(is.list(state$log_counters))

  expect_error(pairwiseLLM:::btl_mcmc_state_new(tibble::tibble(), list()), "disabled")
  expect_error(pairwiseLLM:::btl_mcmc_state_save(list(), tempfile(fileext = ".rds")), "disabled")
  expect_error(pairwiseLLM:::btl_mcmc_state_load(tempfile(fileext = ".rds")), "disabled")
})

test_that("presentation and rollback constraints handle logical and environment seen stores", {
  state <- make_legacy_state_fixture()

  expect_error(pairwiseLLM:::btl_mcmc_record_presentation(state, "Z", "B"), "must exist")

  state <- pairwiseLLM:::btl_mcmc_record_presentation(state, "A", "B")
  expect_identical(unname(state$unordered_count[["A:B"]]), 1L)
  expect_true(isTRUE(state$ordered_seen[["A:B"]]))
  expect_identical(unname(state$pair_ordered_count[["A:B"]]), 1L)

  state <- pairwiseLLM:::btl_mcmc_rollback_presentation(state, "A", "B")
  expect_identical(unname(state$unordered_count[["A:B"]]), 0L)
  expect_false("A:B" %in% names(state$pair_ordered_count))

  state_env <- make_legacy_state_fixture()
  state_env$ordered_seen <- new.env(parent = emptyenv())
  state_env <- pairwiseLLM:::btl_mcmc_record_presentation(state_env, "A", "B")
  expect_true(exists("A:B", envir = state_env$ordered_seen, inherits = FALSE))
  state_env <- pairwiseLLM:::btl_mcmc_rollback_presentation(state_env, "A", "B")
  expect_false(exists("A:B", envir = state_env$ordered_seen, inherits = FALSE))

  bad <- make_legacy_state_fixture()
  expect_error(pairwiseLLM:::btl_mcmc_rollback_presentation(bad, "A", "B"), "missing the requested unordered pair")

  bad_seen <- make_legacy_state_fixture()
  bad_seen$unordered_count <- stats::setNames(1L, "A:B")
  bad_seen$pair_ordered_count <- stats::setNames(1L, "A:B")
  bad_seen$ordered_seen <- stats::setNames(FALSE, "A:B")
  expect_error(pairwiseLLM:::btl_mcmc_rollback_presentation(bad_seen, "A", "B"), "cannot be rolled back below zero")

  bad_ordered <- make_legacy_state_fixture()
  bad_ordered$unordered_count <- stats::setNames(1L, "A:B")
  expect_error(pairwiseLLM:::btl_mcmc_rollback_presentation(bad_ordered, "A", "B"), "missing the requested ordered pair")

  bad_env <- make_legacy_state_fixture()
  bad_env$ordered_seen <- new.env(parent = emptyenv())
  bad_env$unordered_count <- stats::setNames(1L, "A:B")
  bad_env$pair_ordered_count <- stats::setNames(1L, "A:B")
  expect_error(pairwiseLLM:::btl_mcmc_rollback_presentation(bad_env, "A", "B"), "ordered_seen")

  expect_error(pairwiseLLM:::btl_mcmc_rollback_presentation(make_legacy_state_fixture(), "Z", "B"), "must exist")

  expect_error(pairwiseLLM:::btl_mcmc_record_exposure(make_legacy_state_fixture(), "A", "Z"), "must exist")
})

test_that("judgment exposure counters and record_exposure compose correctly", {
  state <- make_legacy_state_fixture()
  expect_error(pairwiseLLM:::btl_mcmc_record_judgment_exposure(state, "A", "Z"), "must exist")

  state <- pairwiseLLM:::btl_mcmc_record_judgment_exposure(state, "A", "B")
  expect_identical(state$pos1[["A"]], 1L)
  expect_identical(state$pos2[["B"]], 1L)
  expect_identical(state$deg[["A"]], 1L)
  expect_identical(state$pair_count[["A:B"]], 1L)

  state2 <- make_legacy_state_fixture()
  state2 <- pairwiseLLM:::btl_mcmc_record_exposure(state2, "A", "B")
  expect_identical(state2$unordered_count[["A:B"]], 1L)
  expect_identical(state2$pair_count[["A:B"]], 1L)

  state3 <- make_legacy_state_fixture()
  state3$pos_count <- NULL
  state3 <- pairwiseLLM:::btl_mcmc_record_judgment_exposure(state3, "A", "B")
  expect_identical(state3$pos_count[["A"]], 1L)
})

test_that("rollback preserves ordered seen when ordered count remains positive", {
  st <- make_legacy_state_fixture()
  st$unordered_count <- stats::setNames(2L, "A:B")
  st$pair_ordered_count <- stats::setNames(2L, "A:B")
  st$ordered_seen <- stats::setNames(TRUE, "A:B")
  out <- pairwiseLLM:::btl_mcmc_rollback_presentation(st, "A", "B")
  expect_true(isTRUE(out$ordered_seen[["A:B"]]))

  st_env <- make_legacy_state_fixture()
  st_env$unordered_count <- stats::setNames(2L, "A:B")
  st_env$pair_ordered_count <- stats::setNames(2L, "A:B")
  st_env$ordered_seen <- new.env(parent = emptyenv())
  st_env$ordered_seen[["A:B"]] <- TRUE
  out_env <- pairwiseLLM:::btl_mcmc_rollback_presentation(st_env, "A", "B")
  expect_true(isTRUE(out_env$ordered_seen[["A:B"]]))
})

test_that("incremental ingest tracks results_seen and updates exposure metrics", {
  state <- make_legacy_state_fixture()
  expect_identical(pairwiseLLM:::btl_mcmc_results_seen_names(state), character())
  state_named <- state
  state_named$results_seen <- stats::setNames(TRUE, "k")
  expect_identical(pairwiseLLM:::btl_mcmc_results_seen_names(state_named), "k")
  state_named_after <- pairwiseLLM:::btl_mcmc_state_sync_results_seen(state_named)
  expect_identical(pairwiseLLM:::btl_mcmc_results_seen_names(state_named_after), "k")

  synced_empty <- pairwiseLLM:::btl_mcmc_state_sync_results_seen(state)
  expect_true(is.logical(synced_empty$results_seen) || is.environment(synced_empty$results_seen))

  state$history_results <- make_results_fixture()
  synced <- pairwiseLLM:::btl_mcmc_state_sync_results_seen(state)
  expect_true(all(c("A:B#1", "A:C#1") %in% names(synced$results_seen)))

  out0 <- pairwiseLLM:::btl_mcmc_ingest_results_incremental(state, tibble::tibble())
  expect_equal(nrow(out0$new_results), 0L)

  with_missing <- make_results_fixture()
  with_missing$pair_uid[[1L]] <- NA_character_
  expect_warning(
    pairwiseLLM:::btl_mcmc_ingest_results_incremental(make_legacy_state_fixture(), with_missing),
    "Dropping results with missing `pair_uid`"
  )
  out <- suppressWarnings(
    pairwiseLLM:::btl_mcmc_ingest_results_incremental(make_legacy_state_fixture(), with_missing)
  )
  expect_equal(nrow(out$new_results), 1L)
  expect_identical(out$state$comparisons_observed, 1L)
  expect_identical(out$state$new_since_refit, 1L)

  out_repeat <- pairwiseLLM:::btl_mcmc_ingest_results_incremental(out$state, out$new_results)
  expect_equal(nrow(out_repeat$new_results), 0L)

  env_state <- make_legacy_state_fixture()
  env_state$results_seen <- new.env(parent = emptyenv())
  env_state <- pairwiseLLM:::btl_mcmc_results_seen_set(env_state, c("x", ""))
  expect_true(exists("x", envir = env_state$results_seen, inherits = FALSE))

  vec_state <- make_legacy_state_fixture()
  vec_state$results_seen <- NULL
  vec_state <- pairwiseLLM:::btl_mcmc_results_seen_set(vec_state, c("a", "b"))
  expect_true(all(c("a", "b") %in% names(vec_state$results_seen)))
  unchanged <- pairwiseLLM:::btl_mcmc_results_seen_set(vec_state, c("", NA_character_))
  expect_identical(names(unchanged$results_seen), names(vec_state$results_seen))
})

test_that("terminal stop metrics are filled from state counters and hard-cap", {
  state <- make_legacy_state_fixture(ids = c("A", "B", "C", "D"))
  state$comparisons_scheduled <- 10L
  state$comparisons_observed <- 8L
  state$pair_count <- stats::setNames(c(1L, 0L, 2L), c("A:B", "A:C", "B:C"))

  m <- pairwiseLLM:::btl_mcmc_fill_terminal_stop_metrics(
    state,
    config = list(hard_cap_frac = 0.5),
    metrics = list()
  )
  expect_identical(m$scheduled_pairs, 10L)
  expect_identical(m$completed_pairs, 8L)
  expect_identical(m$n_unique_pairs_seen, 2L)
  expect_identical(m$hard_cap_threshold, 3L)
  expect_false(isTRUE(m$hard_cap_reached))

  m2 <- pairwiseLLM:::btl_mcmc_fill_terminal_stop_metrics(
    state,
    config = list(hard_cap_frac = 2),
    metrics = list()
  )
  expect_true(is.na(m2$hard_cap_threshold))

  state$pair_count <- stats::setNames(rep(1L, 3L), c("A:B", "A:C", "B:C"))
  m3 <- pairwiseLLM:::btl_mcmc_fill_terminal_stop_metrics(
    state,
    config = list(hard_cap_frac = 0.5),
    metrics = list()
  )
  expect_true(isTRUE(m3$hard_cap_reached))
})

test_that("btl mcmc contracts helpers cover config validators and summary metrics", {
  cfg <- pairwiseLLM:::btl_mcmc_config(4L, NULL)
  expect_true(is.list(cfg))
  expect_error(pairwiseLLM:::btl_mcmc_config(4L, 1L), "must be a list")

  expect_error(pairwiseLLM:::.btl_mcmc_check_named_int(1, "x"), "must be integer")
  expect_error(
    pairwiseLLM:::.btl_mcmc_check_named_int(stats::setNames(1L, "A"), "x", ids = c("A", "B")),
    "named vector over `ids`"
  )
  expect_no_error(pairwiseLLM:::.btl_mcmc_check_named_int(stats::setNames(c(0L, 1L), c("A", "B")), "x"))

  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_pair_keys("A", c("A", "B"), ordered = FALSE, name = "k"),
    "must be `A:B`"
  )
  expect_error(
    pairwiseLLM:::.btl_mcmc_validate_pair_keys("A:A", c("A", "B"), ordered = FALSE, name = "k"),
    "cannot include self-pairs"
  )
  expect_no_error(
    pairwiseLLM:::.btl_mcmc_validate_pair_keys("A:B", c("A", "B"), ordered = FALSE, name = "k")
  )

  bad_cfg <- pairwiseLLM:::btl_mcmc_defaults(4L)
  bad_cfg$target_mean_degree <- Inf
  expect_error(pairwiseLLM:::validate_btl_mcmc_config(bad_cfg), "finite numeric scalar")
  bad_cfg2 <- pairwiseLLM:::btl_mcmc_defaults(4L)
  bad_cfg2$cmdstan <- "x"
  expect_error(pairwiseLLM:::validate_btl_mcmc_config(bad_cfg2), "must be a list")

  schema <- pairwiseLLM:::item_log_schema()
  expect_true(all(c("ID", "deg", "theta_mean", "rank_mean") %in% names(schema)))

  expect_true(is.na(pairwiseLLM:::compute_reliability_EAP(NULL)))
  draws <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  expect_true(is.finite(pairwiseLLM:::compute_reliability_EAP(draws)))

  expect_true(is.na(pairwiseLLM:::compute_gini_degree(NULL)))
  expect_identical(pairwiseLLM:::compute_gini_degree(c(0, 0, 0)), 0)
  expect_error(pairwiseLLM:::compute_gini_degree(c(1, -1)), "non-negative")

  expect_true(is.na(pairwiseLLM:::compute_gini_posA(NULL)))
  expect_identical(pairwiseLLM:::compute_gini_posA(c(0, 0)), 0)
  expect_error(pairwiseLLM:::compute_gini_posA(c(1, 1), deg = c(1)), "same length")
})

test_that("btl mcmc contracts constructors build defaults and log rows", {
  round_defaults <- pairwiseLLM:::.adaptive_round_log_defaults()
  batch_defaults <- pairwiseLLM:::.adaptive_batch_log_defaults()
  item_defaults <- pairwiseLLM:::.adaptive_item_log_defaults(n_rows = 2L)
  expect_true(nrow(round_defaults) == 1L)
  expect_true(nrow(batch_defaults) == 1L)
  expect_true(nrow(item_defaults) == 2L)
  expect_true(is.character(pairwiseLLM:::.adaptive_mode_non_na(c(NA, "", "x", "x"))))

  batch_row <- pairwiseLLM:::build_batch_log_row(
    iter = 1L,
    phase = "phase2",
    mode = "adaptive",
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    batch_size_target = 10L,
    n_pairs_selected = 8L,
    n_pairs_completed = 7L,
    n_pairs_failed = 1L,
    backlog_unjudged = 2L,
    n_explore_target = 3L,
    n_explore_selected = 2L,
    n_exploit_target = 7L,
    n_exploit_selected = 6L,
    n_candidates_generated = 20L,
    n_candidates_after_filters = 9L,
    candidate_starved = FALSE,
    reason_short_batch = "none",
    W_used = 10L,
    explore_rate_used = 0.2,
    utility_selected_p50 = 0.4,
    utility_selected_p90 = 0.8,
    utility_candidate_p90 = 0.9
  )
  expect_true(nrow(batch_row) == 1L)
  expect_true("iter_exit_path" %in% names(batch_row))

  legacy <- make_legacy_state_fixture(c("A", "B"))
  legacy$deg <- stats::setNames(c(1L, 1L), legacy$ids)
  legacy$pos1 <- stats::setNames(c(1L, 0L), legacy$ids)
  theta <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2)
  colnames(theta) <- legacy$ids
  log_tbl <- pairwiseLLM:::build_item_log(legacy, fit = list(theta_draws = theta))
  expect_true(nrow(log_tbl) == 2L)
  expect_true(all(c("theta_mean", "rank_p50") %in% names(log_tbl)))
})
