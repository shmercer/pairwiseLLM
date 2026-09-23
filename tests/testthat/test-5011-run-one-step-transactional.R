add_link_phase_a_evidence <- function(state) {
  artifact_ids <- names(state$linking$phase_a$artifacts %||% list())
  for (set_id in artifact_ids) {
    state$linking$phase_a$artifacts[[set_id]] <- add_test_phase_a_evidence(
      state$linking$phase_a$artifacts[[set_id]],
      state = state,
      set_id = as.integer(set_id)
    )
  }
  state
}

mark_link_phase_b_ready_with_artifacts <- function(state) {
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  artifacts <- stats::setNames(vector("list", length(set_ids)), as.character(set_ids))
  for (set_id in set_ids) {
    in_set <- as.integer(state$items$set_id) == set_id
    n_set <- sum(in_set)
    artifacts[[as.character(set_id)]] <- list(
      items = tibble::tibble(
        global_item_id = as.character(state$items$global_item_id[in_set]),
        theta_raw_mean = seq(0.2, by = -0.2, length.out = n_set),
        theta_raw_sd = rep(0.1, n_set),
        rank_mu_raw = seq_len(n_set)
      )
    )
  }
  state$warm_start_done <- TRUE
  state$linking$phase_a <- list(
    set_status = tibble::tibble(
      set_id = set_ids,
      source = rep("run", length(set_ids)),
      status = rep("ready", length(set_ids)),
      validation_message = rep("ok", length(set_ids)),
      artifact_path = rep(NA_character_, length(set_ids))
    ),
    artifacts = artifacts,
    ready_for_phase_b = TRUE,
    strict_ready_for_phase_b = TRUE,
    required_sets = set_ids,
    set_stop_pass_by_set = stats::setNames(as.list(rep(TRUE, length(set_ids))), as.character(set_ids)),
    phase = "phase_b",
    ready_spokes = setdiff(set_ids, as.integer(state$controller$hub_id %||% 1L)),
    active_phase_a_set = NA_integer_,
    phase_b_started_at_step = 1L
  )
  add_link_phase_a_evidence(state)
}

test_that("run_one_step commits valid results transactionally", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- function(A, B, state, ...) {
    list(
      is_valid = TRUE,
      Y = 1L,
      backend = "openai",
      model = "gpt-5.1",
      endpoint = "responses",
      status_code = 200L,
      error_message = NA_character_,
      custom_id = "custom-ok",
      prompt_tokens = 11,
      completion_tokens = 7,
      total_tokens = 18,
      raw_response = list(ok = TRUE, better_id = as.character(A$item_id[[1L]]))
    )
  }

  before_mu <- state$trueskill_state$items$mu
  before_sigma <- state$trueskill_state$items$sigma
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "ok")
  expect_equal(out$step_log$utility_mode[[1L]], "pairing_trueskill_u0")
  expect_false(is.na(out$step_log$pair_id[[1L]]))
  expect_equal(out$step_log$Y[[1L]], 1L)
  expect_identical(out$step_log$i_id[[1L]], as.character(state$item_ids[[out$step_log$i[[1L]]]]))
  expect_identical(out$step_log$j_id[[1L]], as.character(state$item_ids[[out$step_log$j[[1L]]]]))
  expect_identical(out$step_log$A_id[[1L]], as.character(state$item_ids[[out$step_log$A[[1L]]]]))
  expect_identical(out$step_log$B_id[[1L]], as.character(state$item_ids[[out$step_log$B[[1L]]]]))
  expect_identical(
    out$step_log$unordered_key[[1L]],
    pairwiseLLM:::make_unordered_key(out$step_log$i_id[[1L]], out$step_log$j_id[[1L]])
  )
  expect_identical(
    out$step_log$ordered_key[[1L]],
    pairwiseLLM:::make_ordered_key(out$step_log$A_id[[1L]], out$step_log$B_id[[1L]])
  )
  expect_identical(out$step_log$judge_backend[[1L]], "openai")
  expect_identical(out$step_log$judge_model[[1L]], "gpt-5.1")
  expect_identical(out$step_log$judge_endpoint[[1L]], "responses")
  expect_true(isTRUE(out$step_log$judge_valid[[1L]]))
  expect_true(is.na(out$step_log$judge_invalid_reason[[1L]]))
  expect_identical(out$step_log$llm_status_code[[1L]], 200L)
  expect_identical(out$step_log$llm_custom_id[[1L]], "custom-ok")
  expect_identical(out$step_log$prompt_tokens[[1L]], 11)
  expect_identical(out$step_log$completion_tokens[[1L]], 7)
  expect_identical(out$step_log$total_tokens[[1L]], 18)
  expect_identical(out$step_log$raw_response_json[[1L]], "{\"ok\":true,\"better_id\":\"1\"}")
  expect_false(isTRUE(all.equal(before_mu, out$trueskill_state$items$mu)))
  expect_false(isTRUE(all.equal(before_sigma, out$trueskill_state$items$sigma)))

  expect_equal(nrow(out$history_pairs), 1L)
  expect_equal(nrow(out$item_step_log), out$n_items)
})

test_that("run_one_step logs invalid results without mutating state", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- function(A, B, state, ...) {
    list(
      is_valid = FALSE,
      invalid_reason = "invalid_fixture",
      backend = "anthropic",
      model = "claude-test",
      status_code = 422L,
      error_message = "bad output",
      custom_id = "custom-invalid",
      prompt_tokens = 9,
      completion_tokens = 0,
      total_tokens = 9,
      raw_response = list(error = "bad output")
    )
  }

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_false(is.na(out$step_log$i_id[[1L]]))
  expect_false(is.na(out$step_log$j_id[[1L]]))
  expect_false(is.na(out$step_log$A_id[[1L]]))
  expect_false(is.na(out$step_log$B_id[[1L]]))
  expect_false(is.na(out$step_log$unordered_key[[1L]]))
  expect_false(is.na(out$step_log$ordered_key[[1L]]))
  expect_identical(out$step_log$judge_backend[[1L]], "anthropic")
  expect_identical(out$step_log$judge_model[[1L]], "claude-test")
  expect_true(is.na(out$step_log$judge_endpoint[[1L]]))
  expect_false(isTRUE(out$step_log$judge_valid[[1L]]))
  expect_identical(out$step_log$judge_invalid_reason[[1L]], "invalid_fixture")
  expect_identical(out$step_log$llm_status_code[[1L]], 422L)
  expect_identical(out$step_log$llm_error_message[[1L]], "bad output")
  expect_identical(out$step_log$llm_custom_id[[1L]], "custom-invalid")
  expect_identical(out$step_log$prompt_tokens[[1L]], 9)
  expect_identical(out$step_log$completion_tokens[[1L]], 0)
  expect_identical(out$step_log$total_tokens[[1L]], 9)
  expect_identical(out$step_log$raw_response_json[[1L]], "{\"error\":\"bad output\"}")
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("held-out probe commits do not mutate the shared history-state cache", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "s21"),
    set_id = c(1L, 1L, 2L),
    global_item_id = c("gh1", "gh2", "gs21")
  )
  state <- adaptive_rank_start(
    items,
    seed = 19L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$history_pairs <- tibble::tibble(A_id = "h1", B_id = "h2")
  state$history_state <- pairwiseLLM:::.adaptive_history_state_rebuild(
    state$history_pairs,
    state$item_ids
  )
  state$linking$probe <- list(
    panels_by_spoke = list(
      `2` = tibble::tibble(
        probe_panel_id = "panel-2",
        link_epoch_id = 1L,
        spoke_id = 2L,
        hub_item_id = "h1",
        spoke_item_id = "s21",
        planned_rank = 1L,
        pair_key = pairwiseLLM:::make_unordered_key("h1", "s21"),
        realized = FALSE,
        realized_step_id = NA_integer_,
        realized_pair_id = NA_integer_,
        realized_run_mode = NA_character_
      )
    ),
    prediction_cache = pairwiseLLM:::.adaptive_link_probe_empty_cache(),
    realized_edges = pairwiseLLM:::.adaptive_link_probe_empty_realized_log(),
    realized_index_by_panel = pairwiseLLM:::.adaptive_link_probe_empty_realized_index(),
    collect_holdout_now_by_spoke = list()
  )

  before_history <- state$history_pairs
  before_cache <- state$history_state
  out <- testthat::with_mocked_bindings(
    .adaptive_link_refit_summary_update_after_commit = function(state_before, state_after, step_row) {
      state_after
    },
    pairwiseLLM:::apply_step_update(
      state,
      list(
        row = list(
          step_id = 2L,
          timestamp = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
          pair_id = 2L,
          status = "ok",
          A = 1L,
          B = 3L,
          Y = 1L,
          set_i = 1L,
          set_j = 2L,
          is_cross_set = TRUE,
          is_probe_step = TRUE,
          run_mode = "link_probe_holdout",
          link_spoke_id = 2L,
          fallback_used = "probe_panel_fixed_refit"
        ),
        is_valid = TRUE,
        A_id = "h1",
        B_id = "s21",
        Y = 1L
      )
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(out$history_pairs, before_history)
  expect_identical(out$history_state, before_cache)
  expect_identical(nrow(out$linking$probe$realized_edges), 1L)
  expect_true(isTRUE(out$refit_meta$committed_results_cache_built))
  expect_identical(nrow(out$refit_meta$committed_results_cache), 1L)
  expect_identical(out$refit_meta$committed_results_cache$A_id[[1L]], "h1")
  expect_true(isTRUE(out$refit_meta$link_cross_edges_cache_built))
  expect_identical(
    out$refit_meta$link_cross_edges_by_spoke[["2"]]$fallback_used[[1L]],
    "probe_panel_fixed_refit"
  )
  expect_history_state_matches_history(out)
})

test_that("apply_step_update updates history-state from the pre-commit cache", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(
    items,
    trueskill_state,
    history = tibble::tibble(A_id = "1", B_id = "2")
  )
  before_history <- state$history_pairs
  before_cache <- state$history_state
  resolve_rows_seen <- NULL
  history_update_orig <- pairwiseLLM:::.adaptive_history_state_update

  out <- testthat::with_mocked_bindings(
    .adaptive_history_state_resolve = function(state, ids = NULL, validate_existing = FALSE, context = "runtime") {
      resolve_rows_seen <<- nrow(state$history_pairs)
      before_cache
    },
    .adaptive_history_state_update = function(cache, A_id, B_id) {
      expect_identical(as.integer(cache$n_pairs), as.integer(nrow(before_history)))
      history_update_orig(cache, A_id, B_id)
    },
    .adaptive_link_refit_summary_update_after_commit = function(state_before, state_after, step_row) {
      state_after
    },
    pairwiseLLM:::apply_step_update(
      state,
      list(
        row = list(
          step_id = 2L,
          timestamp = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
          pair_id = 2L,
          status = "ok",
          A = 1L,
          B = 3L,
          Y = 1L,
          set_i = 1L,
          set_j = 1L,
          is_cross_set = FALSE,
          is_probe_step = FALSE,
          run_mode = "within_set"
        ),
        is_valid = TRUE,
        A_id = "1",
        B_id = "3",
        Y = 1L
      )
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(resolve_rows_seen, nrow(before_history))
  expect_identical(nrow(out$history_pairs), nrow(before_history) + 1L)
  expect_identical(as.integer(out$history_state$n_pairs), as.integer(nrow(before_history) + 1L))
  expect_equal(
    out$linking$phase_a$within_set_evidence_by_set[["1"]],
    tibble::tibble(
      pair_id = 2L,
      step_id = 2L,
      A_item = "1",
      B_item = "3",
      y_A = 1L
    )
  )
  expect_true(isTRUE(out$refit_meta$committed_results_cache_built))
  expect_identical(nrow(out$refit_meta$committed_results_cache), 1L)
  expect_identical(out$refit_meta$committed_results_cache$A_id[[1L]], "1")
  expect_true(isTRUE(out$refit_meta$link_cross_edges_cache_built))
  expect_identical(length(out$refit_meta$link_cross_edges_by_spoke), 0L)
  expect_history_state_matches_history(out)
})

test_that("run_one_step enforces canonical judge contract", {
  items <- make_test_items(3)
  trueskill_state <- make_test_trueskill_state(items)
  state <- make_test_state(items, trueskill_state)
  judge <- function(A, B, state, ...) list(Y = 1L)

  snapshot <- snapshot_state_core(state)
  withr::local_seed(1)
  out <- pairwiseLLM:::run_one_step(state, judge)

  expect_equal(nrow(out$step_log), 1L)
  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_true(is.na(out$step_log$pair_id[[1L]]))
  expect_true(is.na(out$step_log$Y[[1L]]))
  expect_true(is.na(out$step_log$p_ij[[1L]]))
  expect_true(is.na(out$step_log$U0_ij[[1L]]))

  expect_equal(snapshot, snapshot_state_core(out))
})

test_that("run_one_step consumes warm-start pairs only on valid results", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items, seed = 42L)
  judge_ok <- make_deterministic_judge("i_wins")
  judge_bad <- make_deterministic_judge("invalid")

  first_pair <- state$warm_start_pairs[1, , drop = FALSE]
  out_bad <- pairwiseLLM:::run_one_step(state, judge_bad)
  expect_equal(out_bad$warm_start_idx, 1L)
  expect_false(out_bad$warm_start_done)

  out_ok <- pairwiseLLM:::run_one_step(out_bad, judge_ok)
  unordered <- pairwiseLLM:::make_unordered_key(
    out_ok$history_pairs$A_id[[1L]],
    out_ok$history_pairs$B_id[[1L]]
  )
  expected <- pairwiseLLM:::make_unordered_key(first_pair$i_id[[1L]], first_pair$j_id[[1L]])
  expect_equal(unordered, expected)
  expect_equal(out_ok$warm_start_idx, 2L)
})

test_that("invalid linking step does not mutate controller link routing state", {
  items <- tibble::tibble(
    item_id = c("a", "b"),
    set_id = c(1L, 2L),
    global_item_id = c("ga", "gb")
  )
  state <- adaptive_rank_start(
    items,
    seed = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L
    )
  )
  state$controller$current_link_spoke_id <- 99L
  state$controller$link_stage_coverage_bins_used <- list(`99` = 3L)
  state$controller$link_stage_coverage_source <- list(`99` = "seed")
  judge_bad <- make_deterministic_judge("invalid")

  out <- pairwiseLLM:::run_one_step(state, judge_bad)

  expect_equal(out$step_log$status[[1L]], "invalid")
  expect_identical(out$controller$current_link_spoke_id, 99L)
  expect_identical(out$controller$link_stage_coverage_bins_used, list(`99` = 3L))
  expect_identical(out$controller$link_stage_coverage_source, list(`99` = "seed"))
})

test_that("run_one_step handles starved selections with NA linking endpoints", {
  items <- make_test_items(2)
  state <- adaptive_rank_start(items, seed = 2L)
  state$warm_start_done <- TRUE
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  judge_ok <- make_deterministic_judge("i_wins")

  out <- state
  for (idx in seq_len(6L)) {
    out <- pairwiseLLM:::run_one_step(out, judge_ok)
    if (identical(utils::tail(out$step_log$status, 1L), "starved")) {
      break
    }
  }

  row <- out$step_log[nrow(out$step_log), , drop = FALSE]
  expect_equal(row$status[[1L]], "starved")
  expect_true(is.na(row$set_i[[1L]]))
  expect_true(is.na(row$set_j[[1L]]))
  expect_true(is.na(row$is_cross_set[[1L]]))
  expect_true(is.na(row$link_spoke_id[[1L]]))
})
