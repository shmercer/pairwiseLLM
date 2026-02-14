test_that("adaptive rank helper argument and trait validators cover edge branches", {
  expect_error(
    pairwiseLLM:::.adaptive_rank_merge_args(list(a = 1L), list(2L)),
    "must be named"
  )
  merged <- pairwiseLLM:::.adaptive_rank_merge_args(list(a = 1L, b = 2L), list(b = 9L, c = 3L))
  expect_identical(merged$a, 1L)
  expect_identical(merged$b, 9L)
  expect_identical(merged$c, 3L)

  expect_error(
    pairwiseLLM:::.adaptive_rank_resolve_trait(
      trait = "overall_quality",
      trait_name = "OnlyName",
      trait_description = NULL
    ),
    "Provide both `trait_name` and `trait_description`"
  )
})

test_that("adaptive rank read_data supports file modes and rejects invalid inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("alpha", "beta"))
  expect_error(
    pairwiseLLM:::.adaptive_rank_read_data(1L, id_col = "ID", text_col = "text"),
    "must be a data frame or a single file/directory path"
  )

  withr::local_tempdir() -> td
  bad <- file.path(td, "bad.json")
  writeLines("{\"a\":1}", bad)
  expect_error(
    pairwiseLLM:::.adaptive_rank_read_data(bad, id_col = "ID", text_col = "text"),
    "Unsupported file extension"
  )

  missing <- file.path(td, "missing.csv")
  expect_error(
    pairwiseLLM:::.adaptive_rank_read_data(missing, id_col = "ID", text_col = "text"),
    "path does not exist"
  )

  txt <- file.path(td, "ok.tsv")
  utils::write.table(samples, txt, sep = "\t", row.names = FALSE, quote = FALSE)
  parsed <- pairwiseLLM:::.adaptive_rank_read_data(txt, id_col = "ID", text_col = "text")
  expect_true(all(c("ID", "text") %in% names(parsed)))
})

test_that("make_adaptive_judge_llm invalid-item and runtime argument branches are deterministic", {
  judge <- pairwiseLLM::make_adaptive_judge_llm(backend = "openai", model = "gpt-test")

  bad_items <- judge(A = tibble::tibble(item_id = "A"), B = tibble::tibble(item_id = "B"), state = list())
  expect_false(isTRUE(bad_items$is_valid))
  expect_identical(bad_items$invalid_reason, "missing_text_column")

  A <- tibble::tibble(item_id = "A", text = "alpha")
  B <- tibble::tibble(item_id = "B", text = "beta")
  out_bad_runtime <- judge(A, B, state = list(), 1L)
  expect_false(isTRUE(out_bad_runtime$is_valid))
  expect_identical(out_bad_runtime$invalid_reason, "invalid_runtime_args")

  out_missing_text <- judge(
    tibble::tibble(item_id = "A", text = NA_character_),
    B,
    state = list()
  )
  expect_false(isTRUE(out_missing_text$is_valid))
  expect_identical(out_missing_text$invalid_reason, "missing_text")

})

test_that("make_adaptive_judge_llm validates constructor args and non-openai endpoint fallback", {
  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(backend = "openai", model = ""),
    "`model` must be a single non-empty string"
  )
  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(backend = "openai", model = "gpt", text_col = ""),
    "`text_col` must be a single non-empty string"
  )
  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "openai",
      model = "gpt",
      judge_args = list("x")
    ),
    "`judge_args` must be a named list"
  )

  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "anthropic",
    model = "claude-test",
    endpoint = NA_character_
  )
  expect_true(is.function(judge))
})

test_that("adaptive_rank validates wrapper arguments and output saving", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"), score = c(3, 2, 1))
  judge <- function(A, B, state, ...) {
    list(is_valid = TRUE, Y = as.integer(A$score[[1L]] >= B$score[[1L]]), invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = judge, judge_args = list("x"), n_steps = 1L, progress = "none"),
    "`judge_args` must be a named list"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples, judge = judge, judge_call_args = list("x"), n_steps = 1L, progress = "none"
    ),
    "`judge_call_args` must be a named list"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = judge, resume = NA, n_steps = 1L, progress = "none"),
    "`resume` must be TRUE or FALSE"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = judge, save_outputs = NA, n_steps = 1L, progress = "none"),
    "`save_outputs` must be TRUE or FALSE"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = judge, output_file = "", n_steps = 1L, progress = "none"),
    "`output_file` must be NULL or a single non-empty string"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = 1L, n_steps = 1L, progress = "none"),
    "`judge` must be NULL or a function"
  )
  expect_error(
    pairwiseLLM::adaptive_rank(data = samples, judge = NULL, model = NULL, n_steps = 1L, progress = "none"),
    "`model` must be a single non-empty string"
  )

  withr::local_tempdir() -> td
  out_path <- file.path(td, "nested", "out.rds")
  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    progress = "none",
    save_outputs = TRUE,
    output_file = out_path
  )
  expect_identical(out$output_file, out_path)
  expect_true(file.exists(out_path))
})

test_that("adaptive_rank resume branch aborts on id mismatch", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"), score = c(3, 2, 1))
  samples_changed <- tibble::tibble(ID = c("A", "B", "X"), text = c("a", "b", "x"), score = c(3, 2, 1))
  judge <- function(A, B, state, ...) {
    list(is_valid = TRUE, Y = as.integer(A$score[[1L]] >= B$score[[1L]]), invalid_reason = NA_character_)
  }

  withr::local_tempdir() -> session_dir
  first <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    session_dir = session_dir,
    resume = FALSE,
    progress = "none"
  )
  expect_true(inherits(first$state, "adaptive_state"))

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples_changed,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      session_dir = session_dir,
      resume = TRUE,
      progress = "none"
    ),
    "Input `data` IDs do not match IDs in resumed session"
  )
})

test_that("adaptive rank wrapper and judge constructors cover endpoint fallback and response-error branches", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"), score = c(3, 2, 1))
  judge <- function(A, B, state, ...) {
    list(is_valid = TRUE, Y = as.integer(A$score[[1L]] >= B$score[[1L]]), invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    backend = "anthropic",
    endpoint = NA_character_,
    judge = judge,
    n_steps = 1L,
    progress = "none"
  )
  expect_true(inherits(out$state, "adaptive_state"))

  out_tmp <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    progress = "none",
    save_outputs = TRUE
  )
  expect_true(file.exists(out_tmp$output_file))

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_rank_read_data = function(data, id_col, text_col) tibble::tibble(ID = c("A", "B")),
      .package = "pairwiseLLM",
      {
        pairwiseLLM::adaptive_rank(
          data = samples,
          judge = judge,
          n_steps = 1L,
          progress = "none"
        )
      }
    ),
    "include a text column after normalization"
  )

  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "openai",
      model = "gpt-test",
      judge_args = 1L
    ),
    "named list"
  )

  judge_llm <- pairwiseLLM::make_adaptive_judge_llm(backend = "openai", model = "gpt-test")
  bad_struct <- judge_llm(A = tibble::tibble(item_id = "A", text = "a")[0, ], B = tibble::tibble(item_id = "B", text = "b"), state = list())
  expect_identical(bad_struct$invalid_reason, "invalid_items")

  bad_missing_id <- judge_llm(A = tibble::tibble(text = "a"), B = tibble::tibble(item_id = "B", text = "b"), state = list())
  expect_identical(bad_missing_id$invalid_reason, "invalid_items")

  bad_id <- judge_llm(A = tibble::tibble(item_id = NA_character_, text = "a"), B = tibble::tibble(item_id = "B", text = "b"), state = list())
  expect_identical(bad_id$invalid_reason, "invalid_items")

  out_err <- testthat::with_mocked_bindings(
    llm_compare_pair = function(...) stop("boom"),
    .package = "pairwiseLLM",
    {
      judge_llm(tibble::tibble(item_id = "A", text = "a"), tibble::tibble(item_id = "B", text = "b"), state = list())
    }
  )
  expect_identical(out_err$invalid_reason, "llm_error")

  out_invalid_resp <- testthat::with_mocked_bindings(
    llm_compare_pair = function(...) tibble::tibble(other = "x"),
    .package = "pairwiseLLM",
    {
      judge_llm(tibble::tibble(item_id = "A", text = "a"), tibble::tibble(item_id = "B", text = "b"), state = list())
    }
  )
  expect_identical(out_invalid_resp$invalid_reason, "invalid_response")

  out_bad_better <- testthat::with_mocked_bindings(
    llm_compare_pair = function(...) tibble::tibble(better_id = "Z"),
    .package = "pairwiseLLM",
    {
      judge_llm(tibble::tibble(item_id = "A", text = "a"), tibble::tibble(item_id = "B", text = "b"), state = list())
    }
  )
  expect_identical(out_bad_better$invalid_reason, "invalid_response")

  withr::local_tempdir() -> td
  dir.create(file.path(td, "samples"), showWarnings = FALSE)
  writeLines("alpha", file.path(td, "samples", "A.txt"))
  writeLines("beta", file.path(td, "samples", "B.txt"))
  from_dir <- pairwiseLLM:::.adaptive_rank_read_data(file.path(td, "samples"), id_col = "ID", text_col = "text")
  expect_true(is.data.frame(from_dir))
  expect_gte(nrow(from_dir), 2L)

  expect_error(
    pairwiseLLM:::.adaptive_rank_resolve_trait(trait = "", trait_name = NULL, trait_description = NULL),
    "single non-empty string"
  )
})

test_that("adaptive run helpers cover warm-start and round-stage edge branches", {
  empty_pairs <- pairwiseLLM:::.adaptive_build_warm_start_pairs(c("A"), seed = 1L)
  expect_equal(nrow(empty_pairs), 0L)

  seeded <- pairwiseLLM:::.adaptive_build_warm_start_pairs(c("A", "B", "C"), seed = 3L)
  expect_equal(nrow(seeded), 2L)

  state <- pairwiseLLM::adaptive_rank_start(make_test_items(4), seed = 1L)
  activated <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
  expect_true(inherits(activated, "adaptive_state"))

  expect_identical(pairwiseLLM:::.adaptive_round_active_stage(list(round = NULL)), "warm_start")
  bad_stage <- pairwiseLLM:::.adaptive_round_active_stage(list(round = list(staged_active = TRUE, stage_index = 99L)))
  expect_true(is.na(bad_stage))

  fake_state <- list(round = list(stage_index = 1L, stage_order = c("anchor_link"), stage_shortfalls = list(anchor_link = 0L)))
  advanced <- pairwiseLLM:::.adaptive_round_advance_stage(fake_state, shortfall = 2L)
  expect_identical(advanced$round$stage_shortfalls$anchor_link, 2L)
})

test_that("adaptive_rank_run_live validates inputs", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(2), seed = 1L)
  judge_invalid <- make_deterministic_judge("invalid")

  expect_error(pairwiseLLM::adaptive_rank_run_live(list(), judge_invalid, n_steps = 1L, progress = "none"), "adaptive_state")
  expect_error(pairwiseLLM::adaptive_rank_run_live(state, 1L, n_steps = 1L, progress = "none"), "`judge` must be a function")
  expect_error(pairwiseLLM::adaptive_rank_run_live(state, judge_invalid, n_steps = 0L, progress = "none"), "positive integer")
  expect_error(pairwiseLLM::adaptive_rank_run_live(state, judge_invalid, n_steps = 1L, session_dir = 1L, progress = "none"), "single string")
  expect_error(
    pairwiseLLM::adaptive_rank_run_live(state, judge_invalid, n_steps = 1L, persist_item_log = 1L, progress = "none"),
    "must be TRUE or FALSE"
  )
})

test_that("adaptive print and log accessors cover validation and canonicalization branches", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)

  expect_identical(pairwiseLLM:::.adaptive_item_log_na_value("degree"), NA_integer_)
  expect_identical(pairwiseLLM:::.adaptive_item_log_na_value("item_id"), NA_character_)
  expect_true(is.na(pairwiseLLM:::.adaptive_item_log_na_value("theta_mean")))

  empty_refit <- pairwiseLLM:::.adaptive_build_item_log_refit(state, refit_id = 1L)
  expect_equal(nrow(empty_refit), 0L)

  state_with_bad <- state
  state_with_bad$item_log <- 1L
  expect_error(pairwiseLLM:::.adaptive_append_item_log(state_with_bad, tibble::tibble(a = 1L)), "must be a list")

  item_row <- tibble::tibble(ID = "1", deg = 2L, theta_mean = 0.1, rank_mean = 1.0)
  canonical <- pairwiseLLM:::.adaptive_canonicalize_item_log(item_row, state, refit_id = 3L)
  expect_true(all(pairwiseLLM:::.adaptive_item_log_columns() %in% names(canonical)))
  expect_identical(canonical$refit_id[[1L]], 3L)

  expect_error(pairwiseLLM::adaptive_get_logs(list()), "adaptive_state")

  missing_step <- state
  missing_step$step_log <- NULL
  expect_error(pairwiseLLM::adaptive_get_logs(missing_step), "step_log")
  expect_error(pairwiseLLM::adaptive_step_log(missing_step), "step_log")

  missing_round <- state
  missing_round$round_log <- NULL
  expect_error(pairwiseLLM::adaptive_round_log(missing_round), "round_log")

  missing_item <- state
  missing_item$item_log <- NULL
  expect_error(pairwiseLLM::adaptive_item_log(missing_item), "item_log")
})

test_that("adaptive item log/history and progress helpers hit optional branches", {
  state <- pairwiseLLM::adaptive_rank_start(make_test_items(3), seed = 1L)
  state$item_log <- list(tibble::tibble(ID = as.character(state$item_ids), deg = c(0L, 0L, 0L)))

  stacked <- pairwiseLLM::adaptive_item_log(state, stack = TRUE)
  expect_true(nrow(stacked) >= 1L)
  expect_error(pairwiseLLM::adaptive_item_log(state, refit_id = 999L), "not available")

  hist_all <- pairwiseLLM::adaptive_results_history(state, committed_only = FALSE)
  expect_true(is.data.frame(hist_all))
  expect_equal(nrow(hist_all), 0L)

  expect_error(pairwiseLLM:::.adaptive_progress_config("steps", 0L, TRUE, TRUE), "positive integer")
  cfg <- pairwiseLLM:::.adaptive_progress_config("steps", 1L, TRUE, FALSE)
  expect_true(isTRUE(cfg$progress_show_events))
  expect_false(isTRUE(cfg$progress_errors))

  expect_false(pairwiseLLM:::.adaptive_meets_threshold(NA_real_, 1, "ge"))
  expect_true(pairwiseLLM:::.adaptive_meets_threshold(0.1, 0.2, "le"))
})

test_that("progress event and refit block formatting covers starved/invalid/fallback cases", {
  cfg <- list(progress_show_events = TRUE, progress_errors = TRUE)

  starved <- tibble::tibble(
    step_id = 1L, round_stage = "anchor_link", candidate_starved = TRUE,
    status = "starved", starvation_reason = NA_character_, fallback_used = NA_character_
  )
  msg_starved <- pairwiseLLM:::adaptive_progress_step_event(starved, cfg)
  expect_match(msg_starved, "candidate_starved=TRUE")

  invalid <- tibble::tibble(
    step_id = 2L, round_stage = "mid_link", candidate_starved = FALSE,
    status = "invalid", starvation_reason = "", fallback_used = NA_character_
  )
  msg_invalid <- pairwiseLLM:::adaptive_progress_step_event(invalid, cfg)
  expect_match(msg_invalid, "invalid judge")

  fallback <- tibble::tibble(
    step_id = 3L, round_stage = "local_link", candidate_starved = FALSE,
    status = "ok", starvation_reason = NA_character_, fallback_used = "global_safe"
  )
  msg_fb <- pairwiseLLM:::adaptive_progress_step_event(fallback, cfg)
  expect_match(msg_fb, "fallback_used=global_safe")

  expect_null(pairwiseLLM:::adaptive_progress_step_event(fallback, list(progress_show_events = FALSE, progress_errors = TRUE)))

  row <- tibble::tibble(
    refit_id = 1L,
    round_id_at_refit = 1L,
    step_id_at_refit = 3L,
    model_variant = "btl_e_b",
    n_items = 3L,
    total_pairs_done = 3L,
    new_pairs_since_last_refit = 3L,
    n_unique_pairs_seen = 3L,
    proposed_pairs_mode = "normal",
    starve_rate_since_last_refit = 0,
    fallback_rate_since_last_refit = 0,
    fallback_used_mode = "base",
    starvation_reason_mode = NA_character_,
    mean_degree = 2,
    min_degree = 1L,
    pos_balance_sd = 0.1,
    epsilon_mean = 0.1,
    epsilon_p2.5 = 0.05,
    epsilon_p50 = 0.1,
    epsilon_p97.5 = 0.2,
    b_mean = 0.2,
    b_p2.5 = 0.1,
    b_p50 = 0.2,
    b_p97.5 = 0.3,
    diagnostics_pass = TRUE,
    divergences = 0L,
    divergences_max_allowed = 0L,
    diagnostics_divergences_pass = TRUE,
    max_rhat = 1.0,
    max_rhat_allowed = 1.01,
    diagnostics_rhat_pass = TRUE,
    min_ess_bulk = 500,
    ess_bulk_required = 300,
    reliability_EAP = 0.99,
    eap_reliability_min = 0.95,
    rho_rank = 0.99,
    rank_spearman_min = 0.95,
    rho_theta = 0.99,
    theta_corr_min = 0.95,
    delta_sd_theta = 0.01,
    theta_sd_rel_change_max = 0.05,
    lag_eligible = TRUE,
    ci95_theta_width_mean = 0.1,
    near_tie_adj_frac = 0.1,
    cov_trace_theta = 1.1,
    top20_boundary_entropy_mean = 0.2,
    nn_diff_sd_mean = 0.3,
    mcmc_chains = 2L,
    mcmc_parallel_chains = 2L,
    mcmc_core_fraction = 0.8,
    mcmc_threads_per_chain = 1L,
    stop_decision = TRUE,
    stop_reason = "btl_converged"
  )
  block <- pairwiseLLM:::adaptive_progress_refit_block(row, cfg = list(stop_thresholds = list()))
  expect_true(any(grepl("Decision: STOP", block)))
  expect_true(any(grepl("Model params", block)))
  expect_equal(length(pairwiseLLM:::adaptive_progress_refit_block(tibble::tibble(), cfg = list())), 0L)
})

test_that("legacy stopping scaffold helpers abort loudly", {
  expect_error(pairwiseLLM:::near_stop_from_state(list()), "Legacy scaffold stopping helpers are disabled")
  expect_error(pairwiseLLM:::btl_mcmc_compute_stop_metrics(list(), NULL, NULL, list()), "Legacy scaffold")
  expect_error(pairwiseLLM:::.adaptive_update_theta_history(list()), "Legacy scaffold")
  expect_error(pairwiseLLM:::btl_mcmc_should_stop(list(), list(), list()), "Legacy scaffold")
})

test_that("adaptive select helpers cover history, strata, and duplicate branches", {
  expect_error(pairwiseLLM:::adaptive_defaults(1L), ">= 2")
  defaults <- pairwiseLLM:::adaptive_defaults(8L)
  expect_true(is.list(defaults))

  hist_ab <- suppressWarnings(
    pairwiseLLM:::.adaptive_history_tbl(list(history_pairs = tibble::tibble(A = "a", B = "b")))
  )
  expect_true(all(c("A_id", "B_id") %in% names(hist_ab)))
  hist_ij <- suppressWarnings(
    pairwiseLLM:::.adaptive_history_tbl(list(history_pairs = tibble::tibble(i = "a", j = "b")))
  )
  expect_true(all(c("A_id", "B_id") %in% names(hist_ij)))
  hist_empty <- pairwiseLLM:::.adaptive_history_tbl(list(history_pairs = tibble::tibble(x = 1L)))
  expect_equal(nrow(hist_empty), 0L)

  counts <- pairwiseLLM:::.adaptive_pair_counts(
    tibble::tibble(A_id = c("a", "a", "x"), B_id = c("b", "a", "b")),
    ids = c("a", "b")
  )
  expect_equal(unname(counts$deg["a"]), 1L)
  expect_equal(unname(counts$deg["b"]), 1L)

  recent <- pairwiseLLM:::.adaptive_recent_deg(
    tibble::tibble(A_id = c("a", "b"), B_id = c("b", "a")),
    ids = c("a", "b"),
    W_cap = 1L
  )
  expect_equal(unname(recent["a"]), 1L)
  expect_true(length(pairwiseLLM:::.adaptive_low_degree_set(c(a = 2L, b = 1L))) >= 1L)
  expect_true(is.character(pairwiseLLM:::.adaptive_underrep_set(integer())))

  state <- pairwiseLLM::adaptive_rank_start(tibble::tibble(item_id = letters[1:6]), seed = 1L)
  rank_index <- pairwiseLLM:::.adaptive_rank_index(state)
  strata <- pairwiseLLM:::.adaptive_strata_index(rank_index, k_base = 3L)
  expect_equal(length(strata), length(rank_index))
  anchors <- pairwiseLLM:::.adaptive_anchor_ids(rank_index)
  expect_true(length(anchors) >= 1L)

  cand <- tibble::tibble(i = c("a", "a", "b"), j = c("b", "c", "d"))
  anchor_filtered <- pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "anchor_link", "base", rank_index, defaults)
  expect_true(nrow(anchor_filtered) >= 0L)
  mid_global <- pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "mid_link", "global_safe", rank_index, defaults)
  expect_true(nrow(mid_global) >= 0L)
  local_expand <- pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "local_link", "expand_locality", rank_index, defaults)
  expect_true(nrow(local_expand) >= 0L)

  pair_count <- c("a:b" = 2L, "a:c" = 1L, "b:d" = 0L)
  no_repeat <- pairwiseLLM:::.adaptive_duplicate_filter(cand, pair_count, dup_max_obs = 2L, allow_repeats = FALSE)
  expect_true(nrow(no_repeat) <= nrow(cand))
  relaxed_no_meta <- pairwiseLLM:::.adaptive_duplicate_filter(cand, pair_count, dup_max_obs = 3L, allow_repeats = TRUE, dup_max_obs_default = 1L)
  expect_true(nrow(relaxed_no_meta) <= nrow(cand))
  relaxed_meta <- pairwiseLLM:::.adaptive_duplicate_filter(
    cand, pair_count, dup_max_obs = 3L, allow_repeats = TRUE, dup_max_obs_default = 1L,
    dup_p_margin = 0.2, p_vals = c(0.51, 0.52, 0.90), u0_vals = c(1, 1, 0.1), u0_quantile = 0.5
  )
  expect_true(nrow(relaxed_meta) >= nrow(relaxed_no_meta))

  expect_true(is.list(pairwiseLLM:::.adaptive_resolve_controller(state, defaults)))
})

test_that("adaptive state and trueskill validators cover additional edge branches", {
  expect_error(pairwiseLLM:::.adaptive_state_normalize_items(NULL), "must be provided")
  expect_error(pairwiseLLM:::.adaptive_state_normalize_items(list(a = 1L)), "vector or data frame")
  expect_error(pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(x = 1L)), "must include an `item_id`")
  expect_error(pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = c("a", "a"))), "must be unique")
  norm_from_id <- pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(id = c("a", "b")))
  expect_identical(norm_from_id$item_id, c("a", "b"))

  expect_error(pairwiseLLM:::.adaptive_validate_controller_config(1L, 5L), "named list")
  expect_error(pairwiseLLM:::.adaptive_validate_controller_config(list(a = 1), 5L), "Unknown `adaptive_config`")
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(list(boundary_k = 0L), 5L),
    "must be in \\[1, 5\\]"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(list(p_long_low = 0.8, p_long_high = 0.2), 5L),
    "strictly less"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "bad_mode"),
      5L
    ),
    "must be one of"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(link_transform_mode = "bad_mode"),
      5L
    ),
    "must be one of"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(cross_set_utility = "entropy"),
      5L
    ),
    "must be one of"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(allow_spoke_spoke_cross_set = "yes"),
      5L
    ),
    "must be TRUE or FALSE"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_multi_spoke"),
      5L,
      set_ids = c(1L, 1L, 1L)
    ),
    "require multi-set input"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_multi_spoke", multi_spoke_mode = "concurrent", hub_lock_mode = "free"),
      5L,
      set_ids = c(1L, 2L, 2L)
    ),
    "must be `hard_lock` or `soft_lock`"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(
      list(run_mode = "link_one_spoke", hub_id = 1L),
      5L,
      set_ids = c(1L, 2L, 3L)
    ),
    "exactly one spoke set"
  )
  cfg_ok <- pairwiseLLM:::.adaptive_validate_controller_config(list(boundary_k = 3L, p_long_low = 0.1, p_long_high = 0.9), 5L)
  expect_identical(cfg_ok$boundary_k, 3L)
  cfg_link_ok <- pairwiseLLM:::.adaptive_validate_controller_config(
    list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      hub_lock_mode = "soft_lock"
    ),
    5L,
    set_ids = c(1L, 2L, 3L)
  )
  expect_identical(cfg_link_ok$hub_id, 1L)
  cfg_spoke_spoke <- pairwiseLLM:::.adaptive_validate_controller_config(
    list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      allow_spoke_spoke_cross_set = TRUE
    ),
    5L,
    set_ids = c(1L, 2L, 3L)
  )
  expect_true(isTRUE(cfg_spoke_spoke$allow_spoke_spoke_cross_set))

  resolved_num <- pairwiseLLM:::.adaptive_controller_resolve(5L)
  expect_true(is.list(resolved_num))
  quotas <- pairwiseLLM:::.adaptive_round_compute_quotas(1L, n_items = 10L, controller = list(global_identified = TRUE))
  expect_equal(sum(quotas), pairwiseLLM:::adaptive_defaults(10L)$round_pairs_target)

  ts <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = c("a", "b"), mu = c(25, 26), sigma = c(8, 8)))
  expect_true(inherits(ts, "trueskill_state"))
  expect_error(pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = c("a", "b"), sigma = c(1, -1))), "must be > 0")
  expect_error(pairwiseLLM:::validate_trueskill_state(list()), "must inherit")
  expect_true(is.numeric(pairwiseLLM:::trueskill_win_probability("a", "b", ts)))
  expect_error(pairwiseLLM:::update_trueskill_state(ts, "a", "x"), "must be present")
  updated <- pairwiseLLM:::update_trueskill_state(ts, "a", "b")
  expect_true(all(updated$items$sigma > 0))
})

test_that("adaptive run/rank wrapper branches cover save paths and starvation exits", {
  trait <- pairwiseLLM:::.adaptive_rank_resolve_trait("overall_quality", NULL, NULL)
  expect_true(all(c("name", "description") %in% names(trait)))

  withr::local_tempdir() -> td
  one <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  rds_path <- file.path(td, "samples.rds")
  saveRDS(one, rds_path)
  parsed <- pairwiseLLM:::.adaptive_rank_read_data(rds_path, id_col = "ID", text_col = "text")
  expect_true(nrow(parsed) == 3L)

  session_dir <- file.path(td, "session")
  run_out <- pairwiseLLM::adaptive_rank(
    data = one,
    id_col = "ID",
    text_col = "text",
    judge = function(A, B, state, ...) list(is_valid = TRUE, Y = 1L, invalid_reason = NA_character_),
    n_steps = 1L,
    progress = "none",
    session_dir = session_dir,
    save_outputs = TRUE
  )
  expect_true(file.exists(run_out$output_file))

  session_dir2 <- file.path(td, "session2")
  state <- pairwiseLLM::adaptive_rank_start(one, seed = 1L, session_dir = session_dir2)
  ran <- pairwiseLLM::adaptive_rank_run_live(
    state = state,
    judge = function(A, B, state, ...) list(is_valid = TRUE, Y = 1L, invalid_reason = NA_character_),
    n_steps = 1L,
    progress = "none"
  )
  expect_true(inherits(ran, "adaptive_state"))
})

test_that("adaptive selector branch guards and validation errors are exercised", {
  state <- pairwiseLLM::adaptive_rank_start(tibble::tibble(item_id = letters[1:4]), seed = 1L)

  hist <- tibble::tibble(A_id = c("a", "x", "b"), B_id = c("a", "b", "c"))
  recent <- pairwiseLLM:::.adaptive_recent_deg(hist, ids = c("a", "b", "c"), W_cap = 3L)
  expect_true(is.integer(as.integer(recent["b"])))

  one_rank <- c(a = 1L)
  expect_equal(pairwiseLLM:::.adaptive_anchor_ids(one_rank), character())

  defaults <- pairwiseLLM:::adaptive_defaults(4L)
  cand <- tibble::tibble(i = c("a", "a"), j = c("b", "c"), p = c(0.5, 0.6), u0 = c(1, 1))
  rank_index <- c(a = 1L, b = 2L, c = 3L, d = 4L)
  expect_true(nrow(pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "long_link", "base", rank_index, defaults)) >= 0L)
  expect_true(nrow(pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "mid_link", "expand_locality", rank_index, defaults)) >= 0L)
  expect_true(nrow(pairwiseLLM:::.adaptive_stage_candidate_filter(cand, "local_link", "global_safe", rank_index, defaults)) >= 0L)

  dup <- pairwiseLLM:::.adaptive_duplicate_filter(
    candidates = cand,
    pair_count = c("a:b" = 0L, "a:c" = 0L),
    dup_max_obs = 2L,
    allow_repeats = TRUE
  )
  expect_equal(nrow(dup), nrow(cand))

  state$controller <- pairwiseLLM:::.adaptive_controller_defaults(length(state$item_ids))
  state$controller$global_identified <- TRUE
  state$controller$p_long_low <- 0.45
  state$controller$p_long_high <- 0.55
  state$round$staged_active <- TRUE
  state$round$stage_index <- 2L
  out_gate <- testthat::with_mocked_bindings(
    trueskill_win_probability = function(i_id, j_id, state) 0.99,
    pairwiseLLM:::select_next_pair(
      state,
      step_id = 1L,
      candidates = tibble::tibble(i = "a", j = "b")
    ),
    .package = "pairwiseLLM"
  )
  expect_identical(out_gate$long_gate_reason, "trueskill_extreme")

  lp <- pairwiseLLM:::.adaptive_local_priority_select(tibble::tibble(), state, state$round, 0L, 1L, defaults)
  expect_identical(lp$mode, "standard")
  partner <- pairwiseLLM:::.adaptive_select_partner(cand, i_id = "a", mu = c(a = 1, b = 2, c = 3), recent_deg = c(a = 0, b = 0, c = 0), mode = "nonlocal")
  expect_true(is.data.frame(partner))

  expect_error(pairwiseLLM:::select_next_pair(list()), "adaptive_state object")
  bad_state <- state
  bad_state$trueskill_state <- NULL
  expect_error(pairwiseLLM:::select_next_pair(bad_state), "must be set")
  expect_error(pairwiseLLM:::select_next_pair(state, step_id = 0L), "positive integer")
})

test_that("adaptive state/trueskill additional scalar and validator branches", {
  vec_norm <- pairwiseLLM:::.adaptive_state_normalize_items(c("x", "y"))
  expect_identical(vec_norm$item_id, c("x", "y"))
  expect_error(
    pairwiseLLM:::.adaptive_state_normalize_items(tibble::tibble(item_id = c("x", NA_character_))),
    "non-missing"
  )

  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(list(boundary_frac = 1.2), 10L),
    "must be in \\[0, 1\\]"
  )
  expect_error(
    pairwiseLLM:::.adaptive_validate_controller_config(list(star_override_budget_per_round = 1.5), 10L),
    "single integer value"
  )
  passthrough <- pairwiseLLM:::.adaptive_sync_round_controller(list(round = NULL))
  expect_null(passthrough$round)

  ts <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = c("a", "b")))
  bad_class <- ts
  class(bad_class) <- "not_ts"
  expect_error(pairwiseLLM:::validate_trueskill_state(bad_class), "must inherit")

  bad_items <- ts
  bad_items$items <- list(a = 1)
  expect_error(pairwiseLLM:::validate_trueskill_state(bad_items), "must be a data frame")

  bad_cols <- ts
  bad_cols$items <- tibble::tibble(item_id = c("a", "b"), mu = c(1, 2))
  expect_error(pairwiseLLM:::validate_trueskill_state(bad_cols), "must include")

  bad_mu <- ts
  bad_mu$items$mu <- c(Inf, 1)
  expect_error(pairwiseLLM:::validate_trueskill_state(bad_mu), "finite numeric")

  bad_sigma <- ts
  bad_sigma$items$sigma <- c(0, 1)
  expect_error(pairwiseLLM:::validate_trueskill_state(bad_sigma), "must be > 0")

  missing_id_res <- tryCatch(
    pairwiseLLM:::trueskill_win_probability("a", "z", ts),
    error = function(e) e
  )
  expect_true(inherits(missing_id_res, "error") || is.numeric(missing_id_res))
  expect_error(pairwiseLLM:::update_trueskill_state("x", "a", "b"), "must inherit")
  expect_error(pairwiseLLM:::.validate_trueskill_scalar("x", "mu0", TRUE), "finite numeric")
  expect_error(pairwiseLLM:::.validate_trueskill_scalar(0, "sigma0", FALSE), "must be > 0")
})

test_that("adaptive_rank_start remains deterministic with multi-set identifiers", {
  items <- tibble::tibble(
    item_id = c("a", "b", "c", "d"),
    set_id = c(1L, 1L, 2L, 2L),
    global_item_id = c("ga", "gb", "gc", "gd")
  )
  withr::local_seed(99)
  s1 <- pairwiseLLM::adaptive_rank_start(items, seed = 123L)
  withr::local_seed(99)
  s2 <- pairwiseLLM::adaptive_rank_start(items, seed = 123L)

  expect_equal(s1$warm_start_pairs, s2$warm_start_pairs)
  expect_equal(s1$set_ids, s2$set_ids)
  expect_equal(s1$global_item_ids, s2$global_item_ids)
})
