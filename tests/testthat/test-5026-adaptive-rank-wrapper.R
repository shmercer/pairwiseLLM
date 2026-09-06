make_test_samples_df <- function(n = 6L) {
  tibble::tibble(
    ID = sprintf("S%02d", seq_len(n)),
    text = paste("sample", seq_len(n)),
    quality_score = seq_len(n)
  )
}

make_linking_samples_df <- function() {
  hub_ids <- paste0("h", seq_len(10L))
  spoke2_ids <- paste0("s2", seq_len(6L))
  spoke3_ids <- paste0("s3", seq_len(6L))
  tibble::tibble(
    ID = c(hub_ids, spoke2_ids, spoke3_ids),
    text = paste("sample", seq_len(22L)),
    quality_score = c(
      seq(10, 100, by = 10),
      c(9, 19, 29, 39, 49, 59),
      c(8, 18, 28, 38, 48, 58)
    ),
    set_id = c(rep(1L, 10L), rep(2L, 6L), rep(3L, 6L)),
    global_item_id = c(paste0("g", hub_ids), paste0("g", spoke2_ids), paste0("g", spoke3_ids))
  )
}

make_linking_subset_df <- function(set_id) {
  samples <- make_linking_samples_df()
  samples[samples$set_id == as.integer(set_id), , drop = FALSE]
}

make_wrapper_import_artifacts <- function(items) {
  state <- pairwiseLLM::adaptive_rank_start(items = items, seed = 91L)
  ids <- as.character(state$item_ids)
  draws <- matrix(
    seq_along(ids),
    nrow = 4L,
    ncol = length(ids),
    byrow = TRUE
  )
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  artifacts <- lapply(set_ids, function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
    art <- add_test_phase_a_evidence(art, state = state, set_id = set_id)
    art$quality_gate_accepted <- TRUE
    art
  })
  names(artifacts) <- as.character(set_ids)
  artifacts
}

test_that("make_adaptive_judge_llm forwards model options and returns valid contract", {
  calls <- list()

  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1",
    endpoint = "responses",
    include_raw = TRUE,
    judge_args = list(service_tier = "flex")
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      calls <<- append(calls, list(list(...)))
      tibble::tibble(
        better_id = "S02",
        model = "gpt-5.1-2026-01-01",
        status_code = 200L,
        error_message = NA_character_,
        custom_id = "wrapped-custom",
        prompt_tokens = 21,
        completion_tokens = 4,
        total_tokens = 25,
        raw_response = list(list(ok = TRUE, winner = "S02"))
      )
    },
    {
      out <- judge(A, B, state = list(), reasoning = "low")
      expect_true(isTRUE(out$is_valid))
      expect_identical(out$Y, 0L)
      expect_true(is.na(out$invalid_reason))
      expect_identical(out$judge_backend, "openai")
      expect_identical(out$judge_model, "gpt-5.1-2026-01-01")
      expect_identical(out$judge_endpoint, "responses")
      expect_identical(out$llm_status_code, 200L)
      expect_true(is.na(out$llm_error_message))
      expect_identical(out$llm_custom_id, "wrapped-custom")
      expect_identical(out$prompt_tokens, 21)
      expect_identical(out$completion_tokens, 4)
      expect_identical(out$total_tokens, 25)
      expect_identical(out$raw_response_json, "{\"ok\":true,\"winner\":\"S02\"}")
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  call <- calls[[1L]]
  expect_identical(call$model, "gpt-5.1")
  expect_identical(call$service_tier, "flex")
  expect_identical(call$reasoning, "low")
})

test_that("make_adaptive_judge_llm forwards vertex backend options", {
  calls <- list()

  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "vertex",
    model = "gemini-2.5-flash",
    judge_args = list(service_tier = "flex")
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      calls <<- append(calls, list(list(...)))
      tibble::tibble(better_id = "S01")
    },
    {
      out <- judge(A, B, state = list(), thinking_level = "low")
      expect_true(isTRUE(out$is_valid))
      expect_identical(out$Y, 1L)
      expect_true(is.na(out$invalid_reason))
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  call <- calls[[1L]]
  expect_identical(call$backend, "vertex")
  expect_identical(call$model, "gemini-2.5-flash")
  expect_identical(call$service_tier, "flex")
  expect_identical(call$thinking_level, "low")
})

test_that("make_adaptive_judge_llm returns invalid when response cannot be mapped", {
  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1",
    include_raw = TRUE
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      tibble::tibble(
        better_id = NA_character_,
        status_code = 422L,
        error_message = "missing winner",
        custom_id = "bad-custom",
        prompt_tokens = 12,
        completion_tokens = 0,
        total_tokens = 12,
        raw_response = list(list(error = "missing winner"))
      )
    },
    {
      out <- judge(A, B, state = list())
      expect_false(isTRUE(out$is_valid))
      expect_identical(out$invalid_reason, "invalid_response")
      expect_identical(out$judge_backend, "openai")
      expect_identical(out$judge_model, "gpt-5.1")
      expect_identical(out$judge_endpoint, "chat.completions")
      expect_identical(out$llm_status_code, 422L)
      expect_identical(out$llm_error_message, "missing winner")
      expect_identical(out$llm_custom_id, "bad-custom")
      expect_identical(out$prompt_tokens, 12)
      expect_identical(out$completion_tokens, 0)
      expect_identical(out$total_tokens, 12)
      expect_identical(out$raw_response_json, "{\"error\":\"missing winner\"}")
    },
    .env = asNamespace("pairwiseLLM")
  )
})

test_that("make_adaptive_judge_llm allows custom traits without built-in trait key", {
  calls <- list()
  judge <- pairwiseLLM::make_adaptive_judge_llm(
    backend = "openai",
    model = "gpt-5.1",
    trait = "any-custom-key",
    trait_name = "Voice",
    trait_description = "Strength of writing voice."
  )

  A <- tibble::tibble(item_id = "S01", text = "A")
  B <- tibble::tibble(item_id = "S02", text = "B")

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      calls <<- append(calls, list(list(...)))
      tibble::tibble(better_id = "S01")
    },
    {
      out <- judge(A, B, state = list())
      expect_true(isTRUE(out$is_valid))
      expect_identical(out$Y, 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_identical(calls[[1L]]$trait_name, "Voice")
  expect_identical(calls[[1L]]$trait_description, "Strength of writing voice.")
})

test_that("endpoint validation is only enforced for openai backend", {
  expect_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "openai",
      model = "gpt-5.1",
      endpoint = "bad-endpoint"
    )
  )

  expect_no_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "anthropic",
      model = "claude",
      endpoint = "bad-endpoint"
    )
  )

  expect_no_error(
    pairwiseLLM::make_adaptive_judge_llm(
      backend = "vertex",
      model = "gemini-2.5-flash",
      endpoint = "bad-endpoint"
    )
  )
})

test_that("adaptive_rank runs end-to-end with user-supplied judge", {
  samples <- make_test_samples_df(6L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 4L,
    progress = "none",
    seed = 7L
  )

  expect_true(is.list(out))
  expect_true(inherits(out$state, "adaptive_state"))
  expect_equal(nrow(out$state$step_log), 4L)
  expect_s3_class(out$summary, "tbl_df")
  expect_true(all(c("step_log", "round_log", "item_log") %in% names(out$logs)))
})

test_that("adaptive_rank supports file inputs and resumability", {
  samples <- make_test_samples_df(5L)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(samples, csv, row.names = FALSE)
  session_dir <- tempfile("adaptive-session-")

  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  first <- pairwiseLLM::adaptive_rank(
    data = csv,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 2L,
    session_dir = session_dir,
    resume = FALSE,
    progress = "none"
  )

  second <- pairwiseLLM::adaptive_rank(
    data = csv,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    session_dir = session_dir,
    resume = TRUE,
    progress = "none"
  )

  expect_equal(nrow(second$state$step_log), nrow(first$state$step_log) + 1L)
})

test_that("adaptive_rank exposes canonical phase_a outputs for wrapper-driven within-set runs", {
  samples <- make_linking_subset_df(1L)
  fit_override <- make_deterministic_fit_fn(ids = as.character(samples$ID))
  session_dir <- withr::local_tempdir()
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 6L,
    session_dir = session_dir,
    resume = FALSE,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none",
    seed = 101L
  )

  expect_true(all(c(
    "session_dir", "artifact_dir", "artifact_paths", "set_status", "manifest"
  ) %in% names(out$phase_a)))
  expect_identical(out$phase_a$session_dir, session_dir)
  expect_identical(out$phase_a$artifact_dir, file.path(session_dir, "phase_a_artifacts"))
  expect_true(inherits(out$phase_a$manifest, "adaptive_phase_a_manifest"))
  expect_true("1" %in% names(out$phase_a$manifest))
  expect_true(file.exists(out$phase_a$artifact_paths[["1"]]))

  status <- tibble::as_tibble(out$phase_a$set_status)
  expect_equal(status$set_id, 1L)
  expect_identical(status$source, "run")
  expect_true(status$status %in% c("ready", "pending_finalization"))
  expect_identical(as.integer(out$phase_a$manifest[["1"]]$set_id), 1L)
})

test_that("adaptive_rank aborts loudly when saved artifacts cannot be resumed", {
  samples <- make_test_samples_df(4L)
  session_dir <- tempfile("adaptive-bad-session-")
  dir.create(session_dir, recursive = TRUE)
  saveRDS(list(bad = TRUE), file.path(session_dir, "state.rds"))

  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      session_dir = session_dir,
      resume = TRUE,
      progress = "none"
    ),
    "Failed to resume adaptive session from `session_dir`"
  )
})

test_that("adaptive_rank wrapper logs preserve anchor/local/repeat and underrep semantics", {
  samples <- make_test_samples_df(8L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 30L,
    btl_config = list(refit_pairs_target = 1000L),
    progress = "none",
    seed = 77L
  )

  step_log <- out$logs$step_log
  committed <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  staged <- committed[committed$round_stage != "warm_start", , drop = FALSE]
  expect_true(nrow(staged) > 0L)

  anchor_rows <- staged[staged$round_stage == "anchor_link", , drop = FALSE]
  if (nrow(anchor_rows) > 0L) {
    expect_true(all(xor(anchor_rows$is_anchor_i, anchor_rows$is_anchor_j)))
  }

  local_rows <- staged[staged$round_stage == "local_link", , drop = FALSE]
  if (nrow(local_rows) > 0L) {
    used_relaxed_locality <- grepl("expand_locality|global_safe", as.character(local_rows$fallback_path))
    ok_local <- local_rows$dist_stratum <= 1L |
      local_rows$is_anchor_i |
      local_rows$is_anchor_j |
      used_relaxed_locality
    expect_true(all(ok_local, na.rm = TRUE))
  }

  staged_by_round <- split(staged, staged$round_id)
  repeat_budget <- pairwiseLLM:::adaptive_defaults(out$state$n_items)$repeat_in_round_budget
  for (round_rows in staged_by_round) {
    repeat_used <- 0L
    uses <- list()
    for (idx in seq_len(nrow(round_rows))) {
      i_key <- as.character(round_rows$i[[idx]])
      j_key <- as.character(round_rows$j[[idx]])
      i_prev <- if (is.null(uses[[i_key]])) 0L else as.integer(uses[[i_key]])
      j_prev <- if (is.null(uses[[j_key]])) 0L else as.integer(uses[[j_key]])
      repeat_used <- repeat_used + as.integer(i_prev > 0L) + as.integer(j_prev > 0L)
      uses[[i_key]] <- i_prev + 1L
      uses[[j_key]] <- j_prev + 1L
    }
    expect_lte(repeat_used, repeat_budget)
  }

  deg <- stats::setNames(rep.int(0L, out$state$n_items), as.character(seq_len(out$state$n_items)))
  explore_rows_checked <- 0L
  for (idx in seq_len(nrow(step_log))) {
    row <- step_log[idx, , drop = FALSE]
    if (!is.na(row$pair_id[[1L]]) && row$round_stage[[1L]] != "warm_start") {
      if (isTRUE(row$is_explore_step[[1L]])) {
        d_min <- min(deg)
        i_key <- as.character(row$i[[1L]])
        j_key <- as.character(row$j[[1L]])
        is_underrep_endpoint <- deg[[i_key]] <= (d_min + 1L) || deg[[j_key]] <= (d_min + 1L)
        expect_true(is_underrep_endpoint)
        explore_rows_checked <- explore_rows_checked + 1L
      }
      deg[[as.character(row$i[[1L]])]] <- deg[[as.character(row$i[[1L]])]] + 1L
      deg[[as.character(row$j[[1L]])]] <- deg[[as.character(row$j[[1L]])]] + 1L
    }
  }
  expect_true(explore_rows_checked >= 0L)
})

test_that("adaptive_rank wrapper exposes top-band defaults and ceiling top-band size", {
  samples <- make_test_samples_df(15L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    btl_config = list(refit_pairs_target = 1000L),
    progress = "none",
    seed = 9L
  )

  defaults <- pairwiseLLM:::adaptive_defaults(out$state$n_items)
  expect_equal(defaults$top_band_pct, 0.10)
  expect_equal(defaults$top_band_bins, 5L)

  proxy <- pairwiseLLM:::.adaptive_rank_proxy(out$state)
  strata <- pairwiseLLM:::.adaptive_assign_strata(proxy$scores, defaults)
  expect_equal(length(strata$top_band_ids), as.integer(ceiling(0.10 * out$state$n_items)))
})

test_that("adaptive_rank summary uses persisted meta stop state, not stale round-log stop flags", {
  samples <- make_linking_samples_df()
  judge <- function(A, B, state, ...) {
    list(is_valid = TRUE, Y = 1L, invalid_reason = NA_character_)
  }

  out <- testthat::with_mocked_bindings(
    adaptive_rank_run_live = function(state, judge, n_steps, ...) {
      state$meta$stop_decision <- FALSE
      state$meta$stop_reason <- NA_character_
      state$round_log <- tibble::tibble(
        refit_id = 1L,
        phase_scope = "global",
        stop_decision = TRUE,
        stop_reason = "btl_converged"
      )
      state
    },
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      progress = "none",
      seed = 17L,
      adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L)
    ),
    .package = "pairwiseLLM"
  )

  expect_false(isTRUE(out$summary$last_stop_decision[[1L]]))
  expect_true(is.na(out$summary$last_stop_reason[[1L]]))
  expect_true(isTRUE(out$logs$round_log$stop_decision[[1L]]))
  expect_identical(as.character(out$logs$round_log$stop_reason[[1L]]), "btl_converged")
})

test_that("adaptive_rank later linking consumes prior wrapper phase_a surfaces", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  hub_samples <- make_linking_subset_df(1L)
  spoke_samples <- make_linking_subset_df(2L)
  fit_hub <- make_deterministic_fit_fn(ids = as.character(hub_samples$ID))
  fit_spoke <- make_deterministic_fit_fn(ids = as.character(spoke_samples$ID))
  fit_link <- make_deterministic_fit_fn(ids = as.character(two_set$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  hub_run <- pairwiseLLM::adaptive_rank(
    data = hub_samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_hub$fit_fn,
    n_steps = 6L,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none",
    seed = 111L
  )
  spoke_run <- pairwiseLLM::adaptive_rank(
    data = spoke_samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_spoke$fit_fn,
    n_steps = 6L,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none",
    seed = 112L
  )

  link_out <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_link$fit_fn,
    n_steps = 12L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = list(
        `1` = hub_run$phase_a,
        `2` = spoke_run$phase_a$manifest
      )
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 113L
  )

  cross <- link_out$logs$step_log[
    link_out$logs$step_log$is_cross_set %in% TRUE &
      !is.na(link_out$logs$step_log$pair_id),
    ,
    drop = FALSE
  ]
  expect_true(nrow(cross) > 0L)
  expect_true(nrow(link_out$logs$link_stage_log) >= 1L)
  status <- tibble::as_tibble(link_out$phase_a$set_status)
  expect_true(all(status$source == "import"))
  expect_true(all(status$status == "ready"))
})

test_that("adaptive_rank reuses session_dir and artifact_dir phase_a sources after resume", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  hub_samples <- make_linking_subset_df(1L)
  spoke_samples <- make_linking_subset_df(2L)
  hub_session <- file.path(withr::local_tempdir(), "hub")
  spoke_session <- file.path(withr::local_tempdir(), "spoke")
  fit_hub <- make_deterministic_fit_fn(ids = as.character(hub_samples$ID))
  fit_spoke <- make_deterministic_fit_fn(ids = as.character(spoke_samples$ID))
  fit_link <- make_deterministic_fit_fn(ids = as.character(two_set$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  first_hub <- pairwiseLLM::adaptive_rank(
    data = hub_samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_hub$fit_fn,
    n_steps = 6L,
    session_dir = hub_session,
    resume = FALSE,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none",
    seed = 121L
  )
  first_spoke <- pairwiseLLM::adaptive_rank(
    data = spoke_samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_spoke$fit_fn,
    n_steps = 6L,
    session_dir = spoke_session,
    resume = FALSE,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none",
    seed = 122L
  )
  resumed_hub <- pairwiseLLM::adaptive_rank(
    data = hub_samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_hub$fit_fn,
    n_steps = 1L,
    session_dir = hub_session,
    resume = TRUE,
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )

  expect_identical(resumed_hub$phase_a$artifact_dir, first_hub$phase_a$artifact_dir)
  expect_identical(names(resumed_hub$phase_a$manifest), names(first_hub$phase_a$manifest))
  expect_identical(
    names(resumed_hub$phase_a$artifact_paths),
    names(first_hub$phase_a$artifact_paths)
  )

  link_out <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_link$fit_fn,
    n_steps = 12L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = list(
        `1` = hub_session,
        `2` = first_spoke$phase_a$artifact_dir
      )
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 123L
  )

  cross <- link_out$logs$step_log[
    link_out$logs$step_log$is_cross_set %in% TRUE &
      !is.na(link_out$logs$step_log$pair_id),
    ,
    drop = FALSE
  ]
  expect_true(nrow(cross) > 0L)
  expect_true(nrow(link_out$logs$link_stage_log) >= 1L)
  expect_true(file.exists(first_spoke$phase_a$artifact_paths[["2"]]))
})

test_that("adaptive_rank builds internal llm judge and forwards judge_call_args", {
  samples <- make_test_samples_df(4L)[, c("ID", "text")]
  calls <- list()

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      args <- list(...)
      calls <<- append(calls, list(args))
      tibble::tibble(better_id = as.character(args$ID1))
    },
    {
      out <- pairwiseLLM::adaptive_rank(
        data = samples,
        id_col = "ID",
        text_col = "text",
        backend = "openai",
        model = "gpt-5.1",
        endpoint = "responses",
        judge_args = list(service_tier = "flex"),
        judge_call_args = list(reasoning = "low"),
        n_steps = 1L,
        progress = "none"
      )
      expect_true(inherits(out$state, "adaptive_state"))
      expect_equal(nrow(out$state$step_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$service_tier, "flex")
  expect_identical(calls[[1L]]$reasoning, "low")
})

test_that("adaptive_rank builds internal vertex judge and forwards judge args", {
  samples <- make_test_samples_df(4L)[, c("ID", "text")]
  calls <- list()

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) {
      args <- list(...)
      calls <<- append(calls, list(args))
      tibble::tibble(better_id = as.character(args$ID1))
    },
    {
      out <- pairwiseLLM::adaptive_rank(
        data = samples,
        id_col = "ID",
        text_col = "text",
        backend = "vertex",
        model = "gemini-2.5-flash",
        endpoint = "not-used-here",
        judge_args = list(service_tier = "priority"),
        judge_call_args = list(thinking_level = "low"),
        n_steps = 1L,
        progress = "none"
      )
      expect_true(inherits(out$state, "adaptive_state"))
      expect_equal(nrow(out$state$step_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )

  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$backend, "vertex")
  expect_identical(calls[[1L]]$service_tier, "priority")
  expect_identical(calls[[1L]]$thinking_level, "low")
})

test_that("adaptive_rank ignores endpoint for non-openai backends", {
  samples <- make_test_samples_df(4L)[, c("ID", "text")]

  testthat::with_mocked_bindings(
    llm_compare_pair = function(...) tibble::tibble(better_id = list(...)$ID1),
    {
      out <- pairwiseLLM::adaptive_rank(
        data = samples,
        id_col = "ID",
        text_col = "text",
        backend = "anthropic",
        model = "claude-test",
        endpoint = "not-used-here",
        n_steps = 1L,
        progress = "none"
      )
      expect_true(inherits(out$state, "adaptive_state"))
      expect_equal(nrow(out$state$step_log), 1L)
    },
    .env = asNamespace("pairwiseLLM")
  )
})

test_that("adaptive_rank_run_live applies adaptive_config overrides", {
  samples <- make_test_samples_df(5L)
  items <- dplyr::rename(samples, item_id = ID)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  state <- pairwiseLLM::adaptive_rank_start(items = items, seed = 11L)
  out <- pairwiseLLM::adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = 1L,
    adaptive_config = list(
      explore_taper_mult = 0.35,
      boundary_frac = 0.20,
      star_override_budget_per_round = 3L
    ),
    progress = "none"
  )

  expect_equal(out$controller$explore_taper_mult, 0.35)
  expect_equal(out$controller$boundary_frac, 0.20)
  expect_equal(out$controller$star_override_budget_per_round, 3L)
  expect_equal(out$round$star_override_budget_per_round, 3L)
})

test_that("adaptive_rank forwards adaptive_config and rejects unknown keys", {
  samples <- make_test_samples_df(5L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    adaptive_config = list(
      global_identified_reliability_min = 0.85,
      p_long_low = 0.20,
      p_long_high = 0.80
    ),
    progress = "none"
  )

  expect_equal(out$state$controller$global_identified_reliability_min, 0.85)
  expect_equal(out$state$controller$p_long_low, 0.20)
  expect_equal(out$state$controller$p_long_high, 0.80)

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      adaptive_config = list(bad_key = 1),
      progress = "none"
    ),
    "Unknown `adaptive_config` field"
  )
})

test_that("adaptive_rank accepts reviewed public Phase B controls", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  items <- dplyr::rename(two_set, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts[c("1", "2")],
      hub_anchor_required_phase_b = FALSE,
      probe_panel_edges = 12L
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
    progress = "none",
    seed = 27L
  )

  expect_false(isTRUE(out$state$controller$hub_anchor_required_phase_b))
  expect_identical(out$state$controller$probe_panel_edges, 12L)
})

test_that("adaptive_rank rejects removed Phase B public controls", {
  samples <- make_linking_samples_df()
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      progress = "none",
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        probe_edges_count_toward_active_constraints = TRUE
      )
    ),
    "probe_edges_count_toward_active_constraints"
  )

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      progress = "none",
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        allow_spoke_spoke_cross_set = TRUE
      )
    ),
    "allow_spoke_spoke_cross_set"
  )
})

test_that("adaptive_rank resume preserves adaptive controller config", {
  samples <- make_test_samples_df(5L)
  session_dir <- tempfile("adaptive-controller-session-")
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  first <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    adaptive_config = list(
      explore_taper_mult = 0.42,
      star_override_budget_per_round = 2L
    ),
    btl_config = list(refit_pairs_target = 1000L),
    session_dir = session_dir,
    resume = FALSE,
    progress = "none"
  )

  second <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 1L,
    btl_config = list(refit_pairs_target = 1000L),
    session_dir = session_dir,
    resume = TRUE,
    progress = "none"
  )

  expect_equal(second$state$controller$explore_taper_mult, 0.42)
  expect_equal(second$state$controller$star_override_budget_per_round, 2L)
  expect_equal(nrow(second$state$step_log), nrow(first$state$step_log) + 1L)
})

test_that("adaptive_rank logs include documented adaptive step and refit fields", {
  samples <- make_test_samples_df(6L)
  fit_override <- make_deterministic_fit_fn(ids = as.character(samples$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    n_steps = 4L,
    fit_fn = fit_override$fit_fn,
    adaptive_config = list(
      global_identified_reliability_min = 0.10,
      global_identified_rank_corr_min = 0.10
    ),
    btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
    progress = "none",
    seed = 5L
  )

  step_cols <- c(
    "explore_rate_used",
    "local_priority_mode",
    "long_gate_pass",
    "long_gate_reason",
    "star_override_used",
    "star_override_reason"
  )
  round_cols <- c(
    "global_identified",
    "global_identified_reliability_min",
    "global_identified_rank_corr_min",
    "long_quota_raw",
    "long_quota_effective",
    "long_quota_removed",
    "realloc_to_mid",
    "realloc_to_local"
  )

  expect_true(all(step_cols %in% names(out$logs$step_log)))
  expect_true(all(round_cols %in% names(out$logs$round_log)))
})

test_that("adaptive_rank wrapper defaults link_one_spoke import flow to anchored-joint", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  items <- dplyr::rename(samples, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  fit_override <- make_deterministic_fit_fn(ids = as.character(two_set$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 12L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts[c("1", "2")]
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 13L
  )

  cross <- out$logs$step_log[
    out$logs$step_log$is_cross_set %in% TRUE & !is.na(out$logs$step_log$pair_id),
    ,
    drop = FALSE
  ]
  expect_true(nrow(cross) > 0L)
  expect_true(all(cross$link_spoke_id == 2L))
  expect_true(nrow(out$logs$link_stage_log) >= 1L)
  expect_true(all(as.character(out$logs$link_stage_log$link_estimation_mode) == "anchored_joint"))
  expect_true(all(is.na(out$logs$link_stage_log$link_transform_policy)))
  expect_true(all(is.na(out$logs$link_stage_log$link_transform_state)))
  expect_true(all(is.na(out$logs$link_stage_log$link_refit_mode)))
  expect_true(all(as.character(out$logs$link_stage_log$hub_lock_mode) == "hard_lock"))
  expect_true(is.function(out$state$config$btl_config$cmdstan_fit_fn))
  expect_true("rank_link" %in% names(out$items))

  printed <- capture.output(print(out$state))
  expect_true(any(grepl("estimation_mode=anchored_joint", printed, fixed = TRUE)))
  expect_true(any(grepl("mode=anchored_joint", printed, fixed = TRUE)))
  expect_false(any(grepl("transform_policy=", printed, fixed = TRUE)))
})

test_that("adaptive_rank wrapper supports anchored-joint linking activation", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  items <- dplyr::rename(samples, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  fit_override <- make_deterministic_fit_fn(ids = as.character(two_set$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 12L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts[c("1", "2")]
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 23L
  )

  expect_true(nrow(out$logs$link_stage_log) >= 1L)
  expect_true(all(as.character(out$logs$link_stage_log$link_estimation_mode) == "anchored_joint"))
  expect_true(all(is.na(out$logs$link_stage_log$link_transform_policy)))
  expect_true(all(is.na(out$logs$link_stage_log$link_transform_state)))
  expect_true(all(is.na(out$logs$link_stage_log$link_refit_mode)))
  expect_true(all(as.character(out$logs$link_stage_log$hub_lock_mode) == "hard_lock"))
  expect_false(is.null(out$state$linking$anchored_joint$accepted_state_by_spoke[["2"]]))
  expect_true("rank_link" %in% names(out$items))
})

test_that("adaptive_rank wrapper supports link_multi_spoke concurrent flow", {
  samples <- make_linking_samples_df()
  items <- dplyr::rename(samples, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  fit_override <- make_deterministic_fit_fn(ids = as.character(samples$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 24L,
    adaptive_config = list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      probe_panel_edges = 18L,
      probe_pairs_per_refit_per_spoke = 1L,
      probe_edges_min_for_stop = 2L,
      link_refit_pairs_per_spoke_rule = "fixed",
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 17L
  )

  cross <- out$logs$step_log[
    out$logs$step_log$is_cross_set %in% TRUE & !is.na(out$logs$step_log$pair_id),
    ,
    drop = FALSE
  ]
  expect_true(nrow(cross) > 0L)
  expect_true(all(sort(unique(cross$link_spoke_id)) == c(2L, 3L)))
  expect_true(all(xor(cross$set_i == 1L, cross$set_j == 1L)))
  expect_true(nrow(out$logs$link_stage_log) >= 2L)
  expect_true(all(as.character(out$logs$link_stage_log$link_estimation_mode) == "anchored_joint"))
  expect_true(all(c("link_transform_policy", "link_transform_state", "link_epoch_id") %in%
    names(out$logs$link_stage_log)))
  expect_true(is.function(out$state$config$btl_config$cmdstan_fit_fn))
})

test_that("adaptive_rank wrapper supports mixed Phase A and strict linking resume", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  items <- dplyr::rename(two_set, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  fit_override <- make_deterministic_fit_fn(ids = as.character(two_set$ID))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  mixed <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 8L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "mixed",
      phase_a_artifacts = list(`1` = artifacts[["1"]])
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    progress = "none",
    seed = 19L
  )

  mixed_status <- tibble::as_tibble(mixed$phase_a$set_status)
  expect_identical(mixed_status$source[match(1L, mixed_status$set_id)], "import")
  expect_identical(mixed_status$source[match(2L, mixed_status$set_id)], "run")

  session_dir <- withr::local_tempdir()
  link_config <- list(
    run_mode = "link_one_spoke",
    hub_id = 1L,
    phase_a_mode = "import",
    phase_a_artifacts = artifacts
  )
  first <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 6L,
    adaptive_config = link_config,
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    session_dir = session_dir,
    resume = FALSE,
    progress = "none",
    seed = 29L
  )
  resumed <- pairwiseLLM::adaptive_rank(
    data = two_set,
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 2L,
    adaptive_config = link_config,
    btl_config = test_link_btl_config(list(refit_pairs_target = 2L)),
    session_dir = session_dir,
    resume = TRUE,
    progress = "none"
  )

  expect_gte(nrow(resumed$logs$step_log), nrow(first$logs$step_log))
  expect_equal(
    resumed$logs$step_log[seq_len(nrow(first$logs$step_log)), , drop = FALSE],
    first$logs$step_log
  )
  expect_true(any(resumed$logs$step_log$is_cross_set %in% TRUE))
})

test_that("adaptive_rank wrapper falls back to rank_raw when linked ranks are unavailable", {
  samples <- make_linking_samples_df()
  fit_override <- make_deterministic_fit_fn(ids = as.character(samples$ID[samples$set_id %in% c(1L, 2L)]))
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  out <- pairwiseLLM::adaptive_rank(
    data = samples[samples$set_id %in% c(1L, 2L), , drop = FALSE],
    id_col = "ID",
    text_col = "text",
    judge = judge,
    fit_fn = fit_override$fit_fn,
    n_steps = 1L,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    btl_config = test_link_btl_config(list(refit_pairs_target = 1L)),
    progress = "none",
    seed = 31L
  )

  expect_true("rank_raw" %in% names(out$items))
  expect_true("rank_link" %in% names(out$items))
  expect_true(all(is.na(out$items$theta_link_eap)))
  expect_identical(out$items$item_id, out$items$item_id[order(out$items$rank_raw)])
})

test_that("adaptive_rank wrapper emits clear linking preflight errors", {
  samples <- make_test_samples_df(6L)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      adaptive_config = list(run_mode = "link_multi_spoke", hub_id = 1L),
      progress = "none"
    ),
    "Linking run modes require multi-set input"
  )

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = samples,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      adaptive_config = list(run_mode = "within_set", phase_a_mode = "import"),
      progress = "none"
    ),
    "phase_a_mode.*only be import/mixed when linking run_mode is enabled"
  )
})

test_that("adaptive_rank rejects removed within-set maintenance control", {
  samples <- make_linking_samples_df()
  two_set <- samples[samples$set_id %in% c(1L, 2L), , drop = FALSE]
  items <- dplyr::rename(two_set, item_id = ID)
  artifacts <- make_wrapper_import_artifacts(items)
  judge <- function(A, B, state, ...) {
    y <- as.integer(A$quality_score[[1L]] >= B$quality_score[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }

  expect_error(
    pairwiseLLM::adaptive_rank(
      data = two_set,
      id_col = "ID",
      text_col = "text",
      judge = judge,
      n_steps = 1L,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "import",
        phase_a_artifacts = artifacts[c("1", "2")],
        within_phase_b_within_set_steps_allowed = TRUE
      ),
      btl_config = test_link_btl_config(list(refit_pairs_target = 5L)),
      progress = "none",
      seed = 29L
    ),
    "within_phase_b_within_set_steps_allowed"
  )
})
