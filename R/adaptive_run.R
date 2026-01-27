#' @include adaptive_contracts.R
#' @include bayes_btl_mcmc_adaptive.R
NULL

# -------------------------------------------------------------------------
# Adaptive orchestration for start/resume/live runs
# -------------------------------------------------------------------------

.adaptive_default_config <- function() {
  list(
    d1 = 8L,
    bins = 8L,
    mix_struct = 0.70,
    within_adj_split = 0.50,
    exploration_frac = 0.05,
    per_item_cap = NULL,
    batch_overrides = list(),
    max_refill_rounds = 2L,
    max_replacements = NULL,
    max_iterations = 50L,
    budget_max = NULL,
    M1_target = NULL
  )
}

.adaptive_merge_config <- function(adaptive) {
  adaptive <- adaptive %||% list()
  if (!is.list(adaptive)) {
    rlang::abort("`adaptive` must be a list.")
  }
  merged <- utils::modifyList(.adaptive_default_config(), adaptive)
  merged
}

.adaptive_v3_overrides_from_adaptive <- function(N, adaptive) {
  adaptive <- adaptive %||% list()
  defaults <- adaptive_v3_defaults(N)
  overrides <- adaptive$v3 %||% list()
  if (length(overrides) == 0L && is.list(adaptive)) {
    known <- intersect(names(adaptive), names(defaults))
    overrides <- adaptive[known]
  }
  overrides
}

.adaptive_check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    rlang::abort(paste0("`", name, "` must be a non-empty character string."))
  }
  invisible(x)
}

.adaptive_check_backend <- function(backend, mode) {
  .adaptive_check_string(backend, "backend")
  if (mode == "batch" && !backend %in% c("openai", "anthropic", "gemini")) {
    rlang::abort("Batch mode supports only openai, anthropic, or gemini backends.")
  }
  invisible(backend)
}

.adaptive_sanitize_submission_options <- function(submission, reserved = character()) {
  submission <- submission %||% list()
  if (!is.list(submission)) {
    rlang::abort("`submission` must be a list.")
  }
  if (length(reserved) > 0L) {
    submission[reserved] <- NULL
  }
  submission
}

.adaptive_merge_submission_options <- function(state, submission, reserved = character()) {
  stored <- state$config$submission %||% list()
  stored <- .adaptive_sanitize_submission_options(stored, reserved = reserved)
  incoming <- .adaptive_sanitize_submission_options(submission, reserved = reserved)
  state$config$submission <- utils::modifyList(stored, incoming)
  state
}

.adaptive_prepare_paths <- function(paths, submission, mode) {
  paths <- paths %||% list()
  if (!is.list(paths)) {
    rlang::abort("`paths` must be a list.")
  }
  submission <- submission %||% list()
  user_output_dir <- !is.null(paths$output_dir) || !is.null(submission$output_dir)
  output_dir <- paths$output_dir %||% submission$output_dir %||% tempfile("adaptive_rank_")
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    rlang::abort("`paths$output_dir` must be a single character path.")
  }
  if (mode == "batch" || isTRUE(user_output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  state_path <- paths$state_path
  if (is.null(state_path) && mode == "batch") {
    state_path <- file.path(output_dir, "adaptive_state.rds")
  }
  list(state_path = state_path, output_dir = output_dir)
}

.adaptive_write_v3_artifacts <- function(state, fit = NULL, output_dir = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  v3_config <- state$config$v3 %||% list()
  if (!isTRUE(v3_config$write_outputs)) {
    return(list(state = state, item_summary = NULL))
  }

  output_dir <- output_dir %||% v3_config$output_dir %||% state$config$output_dir %||% NULL
  if (is.null(output_dir)) {
    rlang::abort("`output_dir` must be provided when `write_outputs` is TRUE.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    rlang::abort("`output_dir` must be a length-1 character path.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  config_path <- file.path(output_dir, "adaptive_config.rds")
  saveRDS(state$config, config_path)

  round_log <- state$config$round_log %||% round_log_schema()
  round_log <- tibble::as_tibble(round_log)
  round_log_path <- file.path(output_dir, "round_log.rds")
  saveRDS(round_log, round_log_path)

  batch_log <- state$batch_log %||% batch_log_schema()
  batch_log <- tibble::as_tibble(batch_log)
  batch_log_path <- file.path(output_dir, "batch_log.rds")
  saveRDS(batch_log, batch_log_path)

  item_summary <- build_item_summary(state, fit = fit)
  item_summary_path <- file.path(output_dir, "item_summary.rds")
  saveRDS(item_summary, item_summary_path)
  state$config$item_summary <- item_summary

  if (isTRUE(v3_config$keep_draws)) {
    thin_draws <- as.integer(v3_config$thin_draws %||% 1L)
    if (is.na(thin_draws) || thin_draws < 1L) {
      rlang::abort("`thin_draws` must be a positive integer.")
    }

    draws <- NULL
    if (is.list(fit) && !is.null(fit$theta_draws)) {
      draws <- list(
        theta = fit$theta_draws,
        epsilon = fit$epsilon_draws %||% NULL,
        beta = fit$beta_draws %||% NULL
      )
    }

    if (!is.null(draws) && !is.null(draws$theta)) {
      theta_draws <- draws$theta
      if (is.matrix(theta_draws) && thin_draws > 1L) {
        keep_idx <- seq(1L, nrow(theta_draws), by = thin_draws)
        draws$theta <- theta_draws[keep_idx, , drop = FALSE]
        if (!is.null(draws$epsilon)) {
          draws$epsilon <- draws$epsilon[keep_idx]
        }
        if (!is.null(draws$beta)) {
          draws$beta <- draws$beta[keep_idx]
        }
      }
      draws_path <- file.path(output_dir, "theta_draws.rds")
      saveRDS(draws, draws_path)
    }
  }

  list(state = state, item_summary = item_summary)
}

.adaptive_pairs_to_submit_tbl <- function(pairs_tbl) {
  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  required <- c("A_id", "B_id", "A_text", "B_text", "pair_uid", "phase", "iter")
  missing <- setdiff(required, names(pairs_tbl))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "`pairs_tbl` is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }

  tibble::tibble(
    ID1 = as.character(pairs_tbl$A_id),
    text1 = as.character(pairs_tbl$A_text),
    ID2 = as.character(pairs_tbl$B_id),
    text2 = as.character(pairs_tbl$B_text),
    pair_uid = as.character(pairs_tbl$pair_uid),
    phase = as.character(pairs_tbl$phase),
    iter = as.integer(pairs_tbl$iter)
  )
}

.adaptive_results_seen_names <- function(state) {
  if (is.environment(state$results_seen)) {
    return(ls(state$results_seen, all.names = TRUE))
  }
  if (is.null(state$results_seen) || length(state$results_seen) == 0L) {
    return(character())
  }
  names(state$results_seen)
}

.adaptive_results_seen_set <- function(state, keys) {
  keys <- as.character(keys)
  keys <- keys[!is.na(keys) & keys != ""]
  if (length(keys) == 0L) return(state)

  if (is.environment(state$results_seen)) {
    for (key in keys) {
      state$results_seen[[key]] <- TRUE
    }
    return(state)
  }

  seen <- state$results_seen
  if (is.null(seen) || length(seen) == 0L) {
    seen <- logical()
  }
  seen[keys] <- TRUE
  state$results_seen <- seen
  state
}

.adaptive_remove_history_pair <- function(state, pair_uid) {
  pair_uid <- as.character(pair_uid)
  if (length(pair_uid) != 1L || is.na(pair_uid) || !nzchar(pair_uid)) {
    rlang::abort("`pair_uid` must be a non-empty character scalar.")
  }
  pairs <- state$history_pairs
  if (is.null(pairs) || nrow(pairs) == 0L) {
    rlang::abort("`state$history_pairs` is empty; cannot rollback presentation.")
  }
  idx <- which(pairs$pair_uid == pair_uid)
  if (length(idx) != 1L) {
    rlang::abort("`state$history_pairs` must contain exactly one matching `pair_uid`.")
  }
  state$history_pairs <- pairs[-idx, , drop = FALSE]
  state$comparisons_scheduled <- as.integer(state$comparisons_scheduled - 1L)
  if (is.na(state$comparisons_scheduled) || state$comparisons_scheduled < 0L) {
    rlang::abort("`state$comparisons_scheduled` cannot be rolled back below zero.")
  }
  state
}

.adaptive_rollback_presentations <- function(state, pairs_submitted, failed_attempts = NULL) {
  if (is.null(pairs_submitted) || nrow(pairs_submitted) == 0L) {
    return(state)
  }

  pairs_submitted <- tibble::as_tibble(pairs_submitted)
  required <- c("pair_uid", "A_id", "B_id")
  .adaptive_required_cols(pairs_submitted, "pairs_submitted", required)

  observed <- .adaptive_results_seen_names(state)
  missing_mask <- !(pairs_submitted$pair_uid %in% observed)
  missing_tbl <- pairs_submitted[missing_mask, c("pair_uid", "A_id", "B_id"), drop = FALSE]

  failed_attempts <- failed_attempts %||% .adaptive_empty_failed_attempts_tbl()
  failed_tbl <- if (!is.null(failed_attempts) && nrow(failed_attempts) > 0L) {
    failed_attempts <- tibble::as_tibble(failed_attempts)
    .adaptive_required_cols(failed_attempts, "failed_attempts", required)
    failed_attempts[, c("pair_uid", "A_id", "B_id"), drop = FALSE]
  } else {
    tibble::tibble(pair_uid = character(), A_id = character(), B_id = character())
  }

  rollback_tbl <- dplyr::bind_rows(missing_tbl, failed_tbl)
  if (nrow(rollback_tbl) == 0L) {
    return(state)
  }
  rollback_tbl <- rollback_tbl[!(rollback_tbl$pair_uid %in% observed), , drop = FALSE]
  if (nrow(rollback_tbl) == 0L) {
    return(state)
  }
  rollback_tbl <- dplyr::distinct(rollback_tbl, .data$pair_uid, .keep_all = TRUE)

  for (idx in seq_len(nrow(rollback_tbl))) {
    A_id <- as.character(rollback_tbl$A_id[[idx]])
    B_id <- as.character(rollback_tbl$B_id[[idx]])
    state <- rollback_presentation(state, A_id, B_id)
    state <- .adaptive_remove_history_pair(state, rollback_tbl$pair_uid[[idx]])
  }

  state
}

.adaptive_state_sync_results_seen <- function(state) {
  if (!is.null(state$results_seen) && length(.adaptive_results_seen_names(state)) > 0L) {
    return(state)
  }
  if (nrow(state$history_results) == 0L) {
    state$results_seen <- logical()
    return(state)
  }
  keys <- as.character(state$history_results$pair_uid)
  keys <- keys[!is.na(keys) & keys != ""]
  if (length(keys) == 0L) {
    state$results_seen <- logical()
    return(state)
  }
  state$results_seen <- stats::setNames(rep(TRUE, length(keys)), keys)
  state
}

.adaptive_ingest_results_incremental <- function(state, results_tbl) {
  if (is.null(results_tbl) || nrow(results_tbl) == 0L) {
    return(list(state = state, new_results = .adaptive_empty_results_tbl()))
  }

  results_tbl <- tibble::as_tibble(results_tbl)
  validate_results_tbl(results_tbl)
  state <- .adaptive_state_sync_results_seen(state)

  pair_uid <- as.character(results_tbl$pair_uid)
  missing_uid <- is.na(pair_uid) | pair_uid == ""
  if (any(missing_uid)) {
    rlang::warn("Dropping results with missing `pair_uid`.")
  }
  keep_mask <- !missing_uid
  results_tbl <- results_tbl[keep_mask, , drop = FALSE]
  pair_uid <- pair_uid[keep_mask]

  seen <- .adaptive_results_seen_names(state)
  new_mask <- !(pair_uid %in% seen)
  new_results <- results_tbl[new_mask, , drop = FALSE]

  if (nrow(new_results) == 0L) {
    return(list(state = state, new_results = .adaptive_empty_results_tbl()))
  }

  new_results <- new_results[!duplicated(new_results$pair_uid), , drop = FALSE]
  for (idx in seq_len(nrow(new_results))) {
    state <- record_judgment_exposure(
      state,
      as.character(new_results$A_id[[idx]]),
      as.character(new_results$B_id[[idx]])
    )
  }
  state$history_results <- dplyr::bind_rows(state$history_results, new_results)
  state$comparisons_observed <- as.integer(nrow(state$history_results))
  state$new_since_refit <- as.integer((state$new_since_refit %||% 0L) + nrow(new_results))
  state <- .adaptive_results_seen_set(state, new_results$pair_uid)

  list(state = state, new_results = new_results)
}

.adaptive_is_valid_results_tbl <- function(x) {
  ok <- tryCatch(
    {
      validate_results_tbl(tibble::as_tibble(x))
      TRUE
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}

.adaptive_is_valid_failed_attempts_tbl <- function(x) {
  ok <- tryCatch(
    {
      validate_failed_attempts_tbl(tibble::as_tibble(x))
      TRUE
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}

.adaptive_normalize_submission_output <- function(raw, pairs_submitted, backend, model, include_raw = FALSE) {
  empty <- list(
    results = .adaptive_empty_results_tbl(),
    failed_attempts = .adaptive_empty_failed_attempts_tbl()
  )
  if (is.null(raw)) return(empty)

  # Already-normalized tibble (results_tbl).
  if (is.data.frame(raw) && .adaptive_is_valid_results_tbl(raw)) {
    return(list(results = tibble::as_tibble(raw), failed_attempts = .adaptive_empty_failed_attempts_tbl()))
  }

  if (is.list(raw) && !inherits(raw, "data.frame")) {
    raw_results <- raw$results %||% NULL
    raw_failed <- raw$failed_attempts %||% NULL
    if (!is.null(raw_results) && is.data.frame(raw_results) && .adaptive_is_valid_results_tbl(raw_results)) {
      failed_attempts <- if (!is.null(raw_failed) &&
        is.data.frame(raw_failed) &&
        .adaptive_is_valid_failed_attempts_tbl(raw_failed)) {
        tibble::as_tibble(raw_failed)
      } else {
        .adaptive_empty_failed_attempts_tbl()
      }
      return(list(results = tibble::as_tibble(raw_results), failed_attempts = failed_attempts))
    }
  }

  if (is.null(pairs_submitted) || nrow(pairs_submitted) == 0L) {
    rlang::abort("Cannot normalize results without `pairs_submitted`.")
  }

  submit_tbl <- .adaptive_pairs_to_submit_tbl(pairs_submitted)
  normalized <- .normalize_llm_results(
    raw = raw,
    pairs = submit_tbl,
    backend = backend,
    model = model,
    include_raw = include_raw
  )
  list(
    results = tibble::as_tibble(normalized$results),
    failed_attempts = tibble::as_tibble(normalized$failed_attempts)
  )
}

.adaptive_append_failed_attempts <- function(state, failed_attempts, phase = NULL, iter = NULL) {
  if (is.null(failed_attempts) || nrow(failed_attempts) == 0L) {
    return(state)
  }
  failed_attempts <- tibble::as_tibble(failed_attempts)
  if (!"phase" %in% names(failed_attempts)) {
    failed_attempts$phase <- NA_character_
  }
  if (!"iter" %in% names(failed_attempts)) {
    failed_attempts$iter <- NA_integer_
  }
  if (!is.null(phase)) {
    failed_attempts$phase <- ifelse(
      is.na(failed_attempts$phase) | failed_attempts$phase == "",
      as.character(phase),
      failed_attempts$phase
    )
  }
  if (!is.null(iter)) {
    failed_attempts$iter <- ifelse(
      is.na(failed_attempts$iter),
      as.integer(iter),
      as.integer(failed_attempts$iter)
    )
  } else {
    failed_attempts$iter <- as.integer(failed_attempts$iter)
  }
  validate_failed_attempts_tbl(failed_attempts)
  state$failed_attempts <- dplyr::bind_rows(state$failed_attempts, failed_attempts)
  state
}

.adaptive_get_batch_sizes <- function(state, adaptive) {
  overrides <- adaptive$batch_overrides %||% list()
  batch_sizes <- compute_batch_sizes(state$N, overrides = overrides)
  state$config$batch_sizes <- batch_sizes
  state$config$CW <- batch_sizes$CW
  state
}

.adaptive_get_refit_fit <- function(state, adaptive, batch_size, seed, allow_refit = TRUE) {
  refit_B <- state$config$v3$refit_B %||% adaptive$refit_B %||% 1L
  refit_B <- as.integer(refit_B)
  if (is.na(refit_B) || refit_B < 1L) {
    rlang::abort("`refit_B` must be a positive integer.")
  }
  batch_size <- as.integer(batch_size)
  if (!is.na(batch_size) && batch_size < 0L) {
    rlang::abort("`batch_size` must be non-negative.")
  }

  needs_init <- is.null(state$fit)
  do_refit <- isTRUE(allow_refit) && isTRUE(state$new_since_refit >= refit_B)
  refit_performed <- FALSE
  new_pairs <- as.integer(state$new_since_refit %||% 0L)

  if (needs_init || do_refit) {
    bt_data <- .btl_mcmc_v3_prepare_bt_data(state$history_results, state$ids)
    mcmc_config <- state$config$v3 %||% adaptive_v3_config(state$N)
    mcmc_fit <- .fit_bayes_btl_mcmc_adaptive(bt_data, config = mcmc_config, seed = seed)
    state$posterior$mcmc_config_used <- mcmc_fit$mcmc_config_used %||% list()
    fit_contract <- as_v3_fit_contract_from_mcmc(mcmc_fit, ids = state$ids)
    state$fit <- fit_contract
    # Use posterior mean from the fit contract (avoid prior fallback).
    epsilon_mean <- fit_contract$epsilon_mean %||% NULL
    if (is.numeric(epsilon_mean) && length(epsilon_mean) == 1L && is.finite(epsilon_mean)) {
      state$posterior$epsilon_mean <- as.double(epsilon_mean)
    }
    refit_performed <- TRUE
    if (do_refit) {
      state$last_refit_at <- as.integer(state$comparisons_observed)
      state$config$last_refit_at <- state$last_refit_at
      state$new_since_refit <- 0L
    }
  }

  if (is.null(state$fit)) {
    rlang::abort("MCMC inference failed to initialize.")
  }

  list(
    state = state,
    fit = state$fit,
    refit_performed = refit_performed,
    new_pairs = as.integer(new_pairs)
  )
}

.adaptive_dup_threshold_from_utilities <- function(U_all) {
  U_all <- as.double(U_all)
  n_candidates <- length(U_all)
  if (n_candidates == 0L || all(is.na(U_all))) {
    return(NA_real_)
  }
  if (n_candidates >= 50L) {
    return(as.double(stats::quantile(U_all, 0.90, type = 7, na.rm = TRUE)[[1L]]))
  }
  as.double(max(U_all, na.rm = TRUE))
}

.adaptive_update_dup_threshold <- function(state, utilities, refit_performed) {
  if (!isTRUE(refit_performed)) {
    return(state)
  }
  if (!is.data.frame(utilities)) {
    rlang::abort("`utilities` must be a data frame or tibble.")
  }
  U_all <- utilities$utility %||% double()
  state$posterior$U_dup_threshold <- .adaptive_dup_threshold_from_utilities(U_all)
  state
}

.adaptive_run_stopping_checks <- function(state, adaptive, seed = NULL, allow_refit = NULL) {
  validate_state(state)
  state$posterior <- state$posterior %||% list()
  state$posterior$stop_metrics <- .adaptive_stop_metrics_align(state$posterior$stop_metrics %||% NULL)
  if (state$comparisons_observed < 1L) {
    return(list(state = state))
  }
  allow_refit <- allow_refit %||% state$config$allow_refit %||% TRUE

  if (!isTRUE(allow_refit) && is.null(state$fit)) {
    return(list(state = state))
  }

  CW <- state$config$CW %||% floor(state$N / 2)
  CW <- as.integer(CW)
  if (is.na(CW) || CW < 1L) {
    rlang::abort("`CW` must be a positive integer.")
  }
  if (identical(state$mode, "warm_start") && identical(state$phase, "phase1")) {
    return(list(state = state))
  }
  if (identical(state$mode, "adaptive") &&
    identical(state$phase, "phase2") &&
    is.integer(state$iter) &&
    length(state$iter) == 1L &&
    state$iter == 0L) {
    return(list(state = state))
  }

  v3_config <- state$config$v3 %||% adaptive_v3_config(state$N)
  refit_performed <- FALSE
  if (isTRUE(allow_refit)) {
    fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size = 1L, seed = seed)
    state <- fit_out$state
    fit <- fit_out$fit
    refit_performed <- isTRUE(fit_out$refit_performed)
    new_pairs <- as.integer(fit_out$new_pairs %||% 0L)
  } else {
    fit <- state$fit
    new_pairs <- as.integer(state$new_since_refit %||% 0L)
  }

  if (is.null(fit) || is.null(fit$theta_draws)) {
    return(list(state = state))
  }
  if (!is.matrix(fit$theta_draws) || nrow(fit$theta_draws) < 2L) {
    return(list(state = state))
  }

  state$posterior$diagnostics_pass <- diagnostics_gate(
    fit,
    v3_config,
    near_stop = near_stop_from_state(state)
  )

  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  candidates <- generate_candidates(theta_summary, state, v3_config)
  candidates <- .adaptive_filter_candidates_to_draws(candidates, fit$theta_draws)

  if (nrow(candidates) == 0L) {
    utilities <- tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    )
  } else {
    names(candidates)[names(candidates) == "i"] <- "i_id"
    names(candidates)[names(candidates) == "j"] <- "j_id"
    utilities <- compute_pair_utility_dispatch(
      fit = fit,
      candidates = candidates,
      state = state,
      config = v3_config,
      diagnostics_pass = state$posterior$diagnostics_pass
    )
  }
  state <- .adaptive_update_dup_threshold(state, utilities, refit_performed)

  metrics <- compute_stop_metrics(
    state = state,
    fit = fit,
    candidates_with_utility = utilities,
    config = v3_config
  )
  metrics$refit_performed <- refit_performed
  state$posterior$stop_metrics <- metrics

  stop_out <- should_stop(metrics, state, v3_config)
  state <- stop_out$state

  if (isTRUE(refit_performed)) {
    round_row <- build_round_log_row(
      state = state,
      fit = fit,
      metrics = metrics,
      stop_out = stop_out,
      config = v3_config,
      batch_size = v3_config$batch_size,
      window_W = v3_config$W,
      exploration_rate = v3_config$explore_rate,
      new_pairs = new_pairs
    )
    prior_log <- state$config$round_log %||% round_log_schema()
    state$config$round_log <- dplyr::bind_rows(prior_log, round_row)
    .adaptive_progress_emit_refit(state, round_row, v3_config)
  }

  list(state = state)
}

.adaptive_apply_diagnostics_gate <- function(state, fit, config, near_stop, refit_performed = TRUE) {
  diagnostics_pass <- diagnostics_gate(fit, config, near_stop = near_stop)
  if (isTRUE(diagnostics_pass)) {
    state$posterior$diagnostics_pass <- TRUE
    if (!is.null(state$repair_attempts) && state$repair_attempts > 0L) {
      state$repair_attempts <- 0L
    }
    if (identical(state$mode, "repair")) {
      state$mode <- "adaptive"
    }
    return(list(state = state, diagnostics_pass = TRUE))
  }

  state$posterior$diagnostics_pass <- FALSE
  state$mode <- "repair"
  if (isTRUE(refit_performed)) {
    state$repair_attempts <- as.integer((state$repair_attempts %||% 0L) + 1L)
  }
  max_cycles <- as.integer(config$repair_max_cycles)
  if (isTRUE(refit_performed) && state$repair_attempts > max_cycles) {
    state$mode <- "stopped"
    state$stop_reason <- "diagnostics_failed"
    rlang::warn("Diagnostics gate failed; repair limit exceeded. Stopping adaptive run.")
    return(list(state = state, diagnostics_pass = FALSE))
  }

  rlang::warn(paste0(
    "Diagnostics gate failed; entering repair mode (attempt ",
    state$repair_attempts,
    " of ",
    max_cycles,
    ")."
  ))

  list(state = state, diagnostics_pass = FALSE)
}

.adaptive_select_exploration_only <- function(state, candidates_with_utility, config, seed = NULL,
                                              dup_policy = c("default", "relaxed")) {
  dup_policy <- match.arg(dup_policy)
  validate_state(state)
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)
  required <- c("i_id", "j_id", "unordered_key", "utility", "utility_raw", "p_mean")
  .adaptive_required_cols(candidates_with_utility, "candidates_with_utility", required)

  batch_size <- as.integer(config$batch_size)
  if (is.na(batch_size) || batch_size < 0L) {
    rlang::abort("`config$batch_size` must be a non-negative integer.")
  }
  if (batch_size == 0L) {
    return(candidates_with_utility[0, , drop = FALSE])
  }

  explore <- .pairwiseLLM_with_seed(seed, function() {
    sample_exploration_pairs(
      state = state,
      candidates = candidates_with_utility,
      n_explore = batch_size,
      config = config,
      dup_policy = dup_policy
    )
  })

  if (nrow(explore) == 0L) {
    return(explore)
  }
  ordered <- assign_order(explore, state)
  ordered$is_explore <- TRUE
  ordered
}

.adaptive_filter_candidates_to_draws <- function(candidates, theta_draws) {
  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  if (!is.matrix(theta_draws) || is.null(colnames(theta_draws))) {
    rlang::abort("`theta_draws` must be a matrix with column names.")
  }

  candidates <- tibble::as_tibble(candidates)
  if (nrow(candidates) == 0L) {
    return(candidates)
  }

  id_cols <- NULL
  if (all(c("i_id", "j_id") %in% names(candidates))) {
    id_cols <- c("i_id", "j_id")
  } else if (all(c("i", "j") %in% names(candidates))) {
    id_cols <- c("i", "j")
  }
  if (is.null(id_cols)) {
    rlang::abort("`candidates` must include `i_id`/`j_id` or `i`/`j` columns.")
  }

  i_id <- as.character(candidates[[id_cols[[1L]]]])
  j_id <- as.character(candidates[[id_cols[[2L]]]])
  keep <- i_id %in% colnames(theta_draws) & j_id %in% colnames(theta_draws)
  candidates[keep, , drop = FALSE]
}

.adaptive_expand_window <- function(W, N) {
  W <- as.integer(W)
  if (is.na(W) || W < 1L) {
    rlang::abort("`W` must be a positive integer.")
  }
  N <- as.integer(N)
  if (is.na(N) || N < 2L) {
    rlang::abort("`N` must be an integer >= 2.")
  }
  max_W <- max(1L, N - 1L)
  if (W >= max_W) {
    return(as.integer(W))
  }
  as.integer(min(max_W, W * 2L))
}

.adaptive_select_batch_by_ladder <- function(
    state,
    fit,
    theta_summary,
    config,
    candidates_with_utility,
    n_candidates_generated = NULL,
    seed = NULL,
    exploration_only = FALSE,
    safe_no_utility = FALSE
) {
  validate_state(state)
  if (!is.list(fit) || is.null(fit$theta_draws)) {
    rlang::abort("`fit` must include `theta_draws`.")
  }
  if (!is.data.frame(candidates_with_utility)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  candidates_with_utility <- tibble::as_tibble(candidates_with_utility)

  batch_size <- as.integer(config$batch_size)
  if (is.na(batch_size) || batch_size < 0L) {
    rlang::abort("`config$batch_size` must be a non-negative integer.")
  }

  if (is.null(n_candidates_generated)) {
    n_candidates_generated <- nrow(candidates_with_utility)
  }
  n_candidates_generated <- as.integer(n_candidates_generated)
  if (is.na(n_candidates_generated) || n_candidates_generated < 0L) {
    rlang::abort("`n_candidates_generated` must be a non-negative integer.")
  }

  call_with_formals <- function(fn, args) {
    fn_formals <- formals(fn)
    if (is.null(fn_formals)) {
      return(do.call(fn, args))
    }
    fn_names <- names(fn_formals)
    if ("..." %in% fn_names) {
      return(do.call(fn, args))
    }
    args <- args[names(args) %in% fn_names]
    do.call(fn, args)
  }

  select_from_utilities <- function(utilities, dup_policy = "default") {
    if (isTRUE(exploration_only)) {
      return(call_with_formals(.adaptive_select_exploration_only, list(
        state = state,
        candidates_with_utility = utilities,
        config = config,
        seed = seed,
        dup_policy = dup_policy
      )))
    }
    if (isTRUE(safe_no_utility)) {
      return(call_with_formals(.adaptive_select_safe_no_utility, list(
        state = state,
        candidates_with_utility = utilities,
        config = config,
        seed = seed,
        dup_policy = dup_policy
      )))
    }
    call_with_formals(select_batch, list(
      state = state,
      candidates_with_utility = utilities,
      config = config,
      seed = seed,
      exploration_only = FALSE,
      dup_policy = dup_policy
    ))
  }

  build_counts <- function(utilities, n_candidates_generated, n_pairs_selected, dup_policy = "default") {
    list(
      n_pairs_requested = as.integer(batch_size),
      n_pairs_selected = as.integer(n_pairs_selected),
      n_candidates_generated = as.integer(n_candidates_generated),
      n_candidates_after_filters = as.integer(.adaptive_candidate_after_filters(
        utilities,
        state,
        config,
        dup_policy = dup_policy
      ))
    )
  }

  W_base <- as.integer(config$W)
  W_cap <- as.integer(.adaptive_v3_clamp(1L, state$N - 1L, state$N - 1L))
  W2 <- as.integer(min(2L * W_base, W_cap))
  W4 <- as.integer(min(4L * W_base, W_cap))

  empty_utilities <- function() {
    tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    )
  }

  attempt_from_candidates <- function(candidates, stage_name, W_used, anchor_pool,
                                      dup_policy = "default") {
    candidates <- tibble::as_tibble(candidates)
    n_generated <- nrow(candidates)
    candidates <- .adaptive_filter_candidates_to_draws(candidates, fit$theta_draws)
    if (nrow(candidates) == 0L) {
      utilities <- empty_utilities()
    } else {
      names(candidates)[names(candidates) == "i"] <- "i_id"
      names(candidates)[names(candidates) == "j"] <- "j_id"
      utilities <- compute_pair_utility_dispatch(
        fit = fit,
        candidates = candidates,
        state = state,
        config = config,
        diagnostics_pass = state$posterior$diagnostics_pass
      )
    }
    utilities <- .adaptive_filter_duplicate_candidates(
      utilities,
      state,
      config,
      dup_policy = dup_policy
    )

    selection <- select_from_utilities(utilities, dup_policy = dup_policy)
    counts <- build_counts(utilities, n_generated, nrow(selection), dup_policy = dup_policy)
    attempt <- tibble::tibble(
      stage = as.character(stage_name),
      W_used = as.integer(W_used),
      anchor_pool = as.character(anchor_pool),
      n_generated = as.integer(n_generated),
      n_survive = as.integer(counts$n_candidates_after_filters),
      n_selected = as.integer(counts$n_pairs_selected),
      dup_policy = as.character(dup_policy)
    )
    list(selection = selection, candidate_stats = counts, stage_attempt = attempt)
  }

  candidates_with_utility <- .adaptive_filter_duplicate_candidates(
    candidates_with_utility,
    state,
    config,
    dup_policy = "default"
  )
  base_selection <- select_from_utilities(candidates_with_utility, dup_policy = "default")
  base_counts <- build_counts(
    candidates_with_utility,
    n_candidates_generated,
    nrow(base_selection),
    dup_policy = "default"
  )
  stage_attempts <- list(tibble::tibble(
    stage = "base_window",
    W_used = as.integer(W_base),
    anchor_pool = "default",
    n_generated = as.integer(n_candidates_generated),
    n_survive = as.integer(base_counts$n_candidates_after_filters),
    n_selected = as.integer(base_counts$n_pairs_selected),
    dup_policy = "default"
  ))
  fallback_path <- c("base_window")
  best <- list(
    selection = base_selection,
    candidate_stats = base_counts,
    W_used = W_base,
    fallback_stage = "base_window"
  )

  if (batch_size == 0L || base_counts$n_pairs_selected >= base_counts$n_pairs_requested) {
    return(list(
      selection = base_selection,
      candidate_starved = FALSE,
      candidate_stats = base_counts,
      fallback_stage = "base_window",
      W_used = W_base,
      fallback_used = "base_window",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  config_expand <- config
  config_expand$W <- W2
  attempt <- attempt_from_candidates(
    generate_candidates(theta_summary, state, config_expand, allow_repeats = TRUE),
    stage_name = "expand_2x",
    W_used = W2,
    anchor_pool = "default",
    dup_policy = "default"
  )
  stage_attempts <- c(stage_attempts, list(attempt$stage_attempt))
  fallback_path <- c(fallback_path, "expand_2x")
  if (attempt$candidate_stats$n_pairs_selected > best$candidate_stats$n_pairs_selected) {
    best <- list(
      selection = attempt$selection,
      candidate_stats = attempt$candidate_stats,
      W_used = W2,
      fallback_stage = "expand_2x"
    )
  }
  if (attempt$candidate_stats$n_pairs_selected >= batch_size) {
    return(list(
      selection = attempt$selection,
      candidate_starved = FALSE,
      candidate_stats = attempt$candidate_stats,
      fallback_stage = "expand_2x",
      W_used = W2,
      fallback_used = "expand_2x",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  config_expand$W <- W4
  attempt <- attempt_from_candidates(
    generate_candidates(theta_summary, state, config_expand, allow_repeats = TRUE),
    stage_name = "expand_4x",
    W_used = W4,
    anchor_pool = "default",
    dup_policy = "default"
  )
  stage_attempts <- c(stage_attempts, list(attempt$stage_attempt))
  fallback_path <- c(fallback_path, "expand_4x")
  if (attempt$candidate_stats$n_pairs_selected > best$candidate_stats$n_pairs_selected) {
    best <- list(
      selection = attempt$selection,
      candidate_stats = attempt$candidate_stats,
      W_used = W4,
      fallback_stage = "expand_4x"
    )
  }
  if (attempt$candidate_stats$n_pairs_selected >= batch_size) {
    return(list(
      selection = attempt$selection,
      candidate_starved = FALSE,
      candidate_stats = attempt$candidate_stats,
      fallback_stage = "expand_4x",
      W_used = W4,
      fallback_used = "expand_4x",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  config_uncertainty <- config
  config_uncertainty$W <- W_base
  uncertainty_anchors <- select_uncertainty_anchors(state, config, theta_summary = theta_summary)
  attempt <- attempt_from_candidates(
    generate_candidates_from_anchors(
      uncertainty_anchors,
      theta_summary,
      state,
      config_uncertainty,
      allow_repeats = TRUE
    ),
    stage_name = "uncertainty_pool",
    W_used = W_base,
    anchor_pool = "uncertainty",
    dup_policy = "default"
  )
  stage_attempts <- c(stage_attempts, list(attempt$stage_attempt))
  fallback_path <- c(fallback_path, "uncertainty_pool")
  if (attempt$candidate_stats$n_pairs_selected > best$candidate_stats$n_pairs_selected) {
    best <- list(
      selection = attempt$selection,
      candidate_stats = attempt$candidate_stats,
      W_used = W_base,
      fallback_stage = "uncertainty_pool"
    )
  }
  if (attempt$candidate_stats$n_pairs_selected >= batch_size) {
    return(list(
      selection = attempt$selection,
      candidate_starved = FALSE,
      candidate_stats = attempt$candidate_stats,
      fallback_stage = "uncertainty_pool",
      W_used = W_base,
      fallback_used = "uncertainty_pool",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  attempt <- attempt_from_candidates(
    generate_candidates_from_anchors(
      uncertainty_anchors,
      theta_summary,
      state,
      config_uncertainty,
      dup_policy = "relaxed",
      allow_repeats = TRUE
    ),
    stage_name = "dup_relax",
    W_used = W_base,
    anchor_pool = "uncertainty",
    dup_policy = "relaxed"
  )
  stage_attempts <- c(stage_attempts, list(attempt$stage_attempt))
  fallback_path <- c(fallback_path, "dup_relax")
  if (attempt$candidate_stats$n_pairs_selected > best$candidate_stats$n_pairs_selected) {
    best <- list(
      selection = attempt$selection,
      candidate_stats = attempt$candidate_stats,
      W_used = W_base,
      fallback_stage = "dup_relax"
    )
  }
  if (attempt$candidate_stats$n_pairs_selected >= batch_size) {
    return(list(
      selection = attempt$selection,
      candidate_starved = FALSE,
      candidate_stats = attempt$candidate_stats,
      fallback_stage = "dup_relax",
      W_used = W_base,
      fallback_used = "dup_relax",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  config_global <- config
  config_global$W <- W_cap
  all_anchors <- as.character(theta_summary$item_id)
  attempt <- attempt_from_candidates(
    generate_candidates_from_anchors(
      all_anchors,
      theta_summary,
      state,
      config_global,
      allow_repeats = TRUE
    ),
    stage_name = "global_safe",
    W_used = W_cap,
    anchor_pool = "global",
    dup_policy = "default"
  )
  stage_attempts <- c(stage_attempts, list(attempt$stage_attempt))
  fallback_path <- c(fallback_path, "global_safe")
  if (attempt$candidate_stats$n_pairs_selected > best$candidate_stats$n_pairs_selected) {
    best <- list(
      selection = attempt$selection,
      candidate_stats = attempt$candidate_stats,
      W_used = W_cap,
      fallback_stage = "global_safe"
    )
  }
  if (attempt$candidate_stats$n_pairs_selected >= batch_size) {
    return(list(
      selection = attempt$selection,
      candidate_starved = FALSE,
      candidate_stats = attempt$candidate_stats,
      fallback_stage = "global_safe",
      W_used = W_cap,
      fallback_used = "global_safe",
      fallback_exhausted = FALSE,
      fallback_path = fallback_path,
      stage_attempts = stage_attempts
    ))
  }

  candidate_starved <- best$candidate_stats$n_pairs_selected < batch_size
  selection_empty <- best$candidate_stats$n_pairs_selected == 0L
  fallback_stage <- if (selection_empty) "FAILED" else best$fallback_stage
  list(
    selection = best$selection,
    candidate_starved = candidate_starved,
    candidate_stats = best$candidate_stats,
    fallback_stage = fallback_stage,
    W_used = best$W_used,
    fallback_used = fallback_stage,
    fallback_exhausted = TRUE,
    fallback_path = fallback_path,
    stage_attempts = stage_attempts
  )
}

.adaptive_select_batch_with_fallbacks <- function(
    state,
    fit,
    theta_summary,
    config,
    candidates_with_utility,
    n_candidates_generated = NULL,
    seed = NULL,
    exploration_only = FALSE,
    safe_no_utility = FALSE
) {
  .adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config,
    candidates_with_utility = candidates_with_utility,
    n_candidates_generated = n_candidates_generated,
    seed = seed,
    exploration_only = exploration_only,
    safe_no_utility = safe_no_utility
  )
}

.adaptive_quantile <- function(values, prob) {
  values <- as.double(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NA_real_)
  }
  as.double(stats::quantile(values, prob, type = 7, na.rm = TRUE)[[1L]])
}

.adaptive_starvation_reason_from_attempts <- function(stage_attempts, batch_size, config) {
  if (!is.data.frame(stage_attempts) || nrow(stage_attempts) == 0L) {
    return(NA_character_)
  }
  if (!all(c("n_selected", "n_generated") %in% names(stage_attempts))) {
    return(NA_character_)
  }
  batch_size <- as.integer(batch_size)
  if (is.na(batch_size) || batch_size < 0L) {
    return(NA_character_)
  }
  incomplete <- stage_attempts[stage_attempts$n_selected < batch_size, , drop = FALSE]
  if (nrow(incomplete) == 0L) {
    return(NA_character_)
  }
  idx <- which.max(incomplete$n_generated)
  best <- incomplete[idx, , drop = FALSE]
  C_max <- as.integer(config$C_max %||% NA_integer_)
  threshold <- if (!is.na(C_max)) max(10L, floor(0.05 * C_max)) else 10L
  n_generated <- as.integer(best$n_generated[[1L]])
  if (!is.na(n_generated) && n_generated < threshold) {
    return("few_candidates_generated")
  }
  "unknown"
}

.adaptive_stage_attempts_fields <- function(stage_attempts, batch_size, config, candidate_starved) {
  attempts_tbl <- tibble::tibble()
  if (is.list(stage_attempts) && length(stage_attempts) > 0L) {
    attempts_tbl <- dplyr::bind_rows(stage_attempts)
  }
  attempts_tbl <- tibble::as_tibble(attempts_tbl)

  extract <- function(tbl, name, default) {
    if (nrow(tbl) == 0L || !name %in% names(tbl)) {
      return(default)
    }
    value <- tbl[[name]][[1L]]
    if (is.null(value)) {
      return(default)
    }
    value
  }

  get_attempt <- function(idx) {
    if (nrow(attempts_tbl) >= idx) {
      return(attempts_tbl[idx, , drop = FALSE])
    }
    tibble::tibble()
  }

  a1 <- get_attempt(1L)
  a2 <- get_attempt(2L)
  a3 <- get_attempt(3L)

  aN_tried <- max(0L, nrow(attempts_tbl) - 3L)
  aN_best <- tibble::tibble()
  if (aN_tried > 0L) {
    beyond <- attempts_tbl[4:nrow(attempts_tbl), , drop = FALSE]
    if ("n_selected" %in% names(beyond) && any(!is.na(beyond$n_selected))) {
      best_idx <- which.max(replace(beyond$n_selected, is.na(beyond$n_selected), -Inf))
      aN_best <- beyond[best_idx, , drop = FALSE]
    }
  }

  starvation_reason <- NA_character_
  if (isTRUE(candidate_starved)) {
    starvation_reason <- .adaptive_starvation_reason_from_attempts(attempts_tbl, batch_size, config)
    if (is.na(starvation_reason)) {
      starvation_reason <- "unknown"
    }
  }

  list(
    a1_stage = extract(a1, "stage", NA_character_),
    a1_W_used = extract(a1, "W_used", NA_integer_),
    a1_anchor_pool = extract(a1, "anchor_pool", NA_character_),
    a1_n_generated = extract(a1, "n_generated", NA_integer_),
    a1_n_survive = extract(a1, "n_survive", NA_integer_),
    a1_n_selected = extract(a1, "n_selected", NA_integer_),
    a2_stage = extract(a2, "stage", NA_character_),
    a2_W_used = extract(a2, "W_used", NA_integer_),
    a2_anchor_pool = extract(a2, "anchor_pool", NA_character_),
    a2_n_generated = extract(a2, "n_generated", NA_integer_),
    a2_n_survive = extract(a2, "n_survive", NA_integer_),
    a2_n_selected = extract(a2, "n_selected", NA_integer_),
    a3_stage = extract(a3, "stage", NA_character_),
    a3_W_used = extract(a3, "W_used", NA_integer_),
    a3_anchor_pool = extract(a3, "anchor_pool", NA_character_),
    a3_n_generated = extract(a3, "n_generated", NA_integer_),
    a3_n_survive = extract(a3, "n_survive", NA_integer_),
    a3_n_selected = extract(a3, "n_selected", NA_integer_),
    aN_tried = as.integer(aN_tried),
    aN_best_stage = extract(aN_best, "stage", NA_character_),
    aN_best_n_generated = extract(aN_best, "n_generated", NA_integer_),
    aN_best_n_survive = extract(aN_best, "n_survive", NA_integer_),
    aN_best_n_selected = extract(aN_best, "n_selected", NA_integer_),
    starvation_reason = starvation_reason
  )
}

.adaptive_append_batch_log <- function(state,
    iter,
    phase,
    mode,
    created_at,
    batch_size_target,
    selection,
    candidate_stats,
    candidate_starved,
    fallback_exhausted = NULL,
    fallback_stage,
    fallback_used = NULL,
    fallback_path = NULL,
    stage_attempts = NULL,
    W_used,
    config,
    exploration_only,
    safe_no_utility = FALSE,
    utilities,
    iter_exit_path = NULL) {
  state <- .adaptive_state_init_logs(state)

  selection <- tibble::as_tibble(selection %||% tibble::tibble())
  n_pairs_selected <- nrow(selection)

  counters <- state$log_counters %||% list()
  prev_observed <- as.integer(counters$comparisons_observed %||% 0L)
  prev_failed <- as.integer(counters$failed_attempts %||% 0L)
  n_pairs_completed <- as.integer(state$comparisons_observed - prev_observed)
  n_pairs_failed <- as.integer(nrow(state$failed_attempts) - prev_failed)
  n_pairs_completed <- as.integer(max(0L, n_pairs_completed))
  n_pairs_failed <- as.integer(max(0L, n_pairs_failed))

  backlog_unjudged <- as.integer(state$comparisons_scheduled - state$comparisons_observed)

  explore_rate <- as.double(config$explore_rate %||% NA_real_)
  if (isTRUE(exploration_only)) {
    n_explore_target <- as.integer(batch_size_target)
    n_exploit_target <- 0L
    explore_rate_used <- 1
  } else {
    n_explore_target <- as.integer(round(explore_rate * batch_size_target))
    n_exploit_target <- as.integer(batch_size_target - n_explore_target)
    explore_rate_used <- explore_rate
  }

  if ("is_explore" %in% names(selection)) {
    n_explore_selected <- as.integer(sum(selection$is_explore %in% TRUE))
    n_exploit_selected <- as.integer(n_pairs_selected - n_explore_selected)
  } else if (isTRUE(exploration_only)) {
    n_explore_selected <- as.integer(n_pairs_selected)
    n_exploit_selected <- 0L
  } else {
    n_explore_selected <- NA_integer_
    n_exploit_selected <- NA_integer_
  }

  utilities_tbl <- tibble::as_tibble(utilities %||% tibble::tibble())
  utility_candidate_p90 <- NA_real_
  if ("utility" %in% names(utilities_tbl)) {
    utility_candidate_p90 <- .adaptive_quantile(utilities_tbl$utility, 0.90)
  }

  exploit_utilities <- numeric()
  if (n_pairs_selected > 0L && "utility" %in% names(selection)) {
    if ("is_explore" %in% names(selection)) {
      exploit_utilities <- selection$utility[!selection$is_explore %in% TRUE]
    } else if (!isTRUE(exploration_only)) {
      exploit_utilities <- selection$utility
    }
  }
  utility_selected_p50 <- .adaptive_quantile(exploit_utilities, 0.50)
  utility_selected_p90 <- .adaptive_quantile(exploit_utilities, 0.90)

  reason_short_batch <- NA_character_
  if (is.finite(batch_size_target) && n_pairs_selected < batch_size_target) {
    reason_short_batch <- as.character(fallback_stage %||% "short_batch")
  }

  metrics <- .adaptive_stop_metrics_align(state$posterior$stop_metrics %||% NULL)
  metrics$scheduled_pairs <- as.integer(state$comparisons_scheduled)
  metrics$completed_pairs <- as.integer(state$comparisons_observed)
  if (is.data.frame(utilities_tbl)) {
    metrics$proposed_pairs <- as.integer(nrow(utilities_tbl))
  }
  metrics$candidate_starved <- as.logical(candidate_starved %||% NA)
  metrics$reason_short_batch <- as.character(reason_short_batch)
  state$posterior$stop_metrics <- metrics

  stop_reason <- state$stop_reason %||% NA_character_
    if (isTRUE(candidate_starved) &&
      n_pairs_selected == 0L &&
      (!is.character(stop_reason) ||
        length(stop_reason) != 1L ||
        is.na(stop_reason) ||
        !nzchar(stop_reason))) {
    mode <- "stopped"
    state$mode <- "stopped"
      state$stop_reason <- "candidate_starvation"
    }

    fallback_used <- fallback_used %||% NA_character_
    fallback_path <- fallback_path %||% NA_character_
    if (length(fallback_path) > 1L) {
      fallback_path <- paste(as.character(fallback_path), collapse = ">")
    } else if (length(fallback_path) == 1L && !is.na(fallback_path)) {
      fallback_path <- as.character(fallback_path)
    } else {
      fallback_path <- NA_character_
    }

    stage_fields <- .adaptive_stage_attempts_fields(
      stage_attempts,
      batch_size = batch_size_target,
      config = config,
      candidate_starved = candidate_starved
    )

  row <- build_batch_log_row(
      iter = iter,
      phase = phase,
      mode = mode,
      created_at = created_at,
    batch_size_target = batch_size_target,
    n_pairs_selected = n_pairs_selected,
    n_pairs_completed = n_pairs_completed,
    n_pairs_failed = n_pairs_failed,
    backlog_unjudged = backlog_unjudged,
    n_explore_target = n_explore_target,
    n_explore_selected = n_explore_selected,
    n_exploit_target = n_exploit_target,
    n_exploit_selected = n_exploit_selected,
      safe_no_utility = safe_no_utility,
      n_candidates_generated = candidate_stats$n_candidates_generated %||% NA_integer_,
      n_candidates_after_filters = candidate_stats$n_candidates_after_filters %||% NA_integer_,
      candidate_starved = candidate_starved %||% NA,
      fallback_exhausted = fallback_exhausted %||% NA,
      fallback_used = fallback_used,
      fallback_path = fallback_path,
      a1_stage = stage_fields$a1_stage,
      a1_W_used = stage_fields$a1_W_used,
      a1_anchor_pool = stage_fields$a1_anchor_pool,
      a1_n_generated = stage_fields$a1_n_generated,
      a1_n_survive = stage_fields$a1_n_survive,
      a1_n_selected = stage_fields$a1_n_selected,
      a2_stage = stage_fields$a2_stage,
      a2_W_used = stage_fields$a2_W_used,
      a2_anchor_pool = stage_fields$a2_anchor_pool,
      a2_n_generated = stage_fields$a2_n_generated,
      a2_n_survive = stage_fields$a2_n_survive,
      a2_n_selected = stage_fields$a2_n_selected,
      a3_stage = stage_fields$a3_stage,
      a3_W_used = stage_fields$a3_W_used,
      a3_anchor_pool = stage_fields$a3_anchor_pool,
      a3_n_generated = stage_fields$a3_n_generated,
      a3_n_survive = stage_fields$a3_n_survive,
      a3_n_selected = stage_fields$a3_n_selected,
      aN_tried = stage_fields$aN_tried,
      aN_best_stage = stage_fields$aN_best_stage,
      aN_best_n_generated = stage_fields$aN_best_n_generated,
      aN_best_n_survive = stage_fields$aN_best_n_survive,
      aN_best_n_selected = stage_fields$aN_best_n_selected,
      starvation_reason = stage_fields$starvation_reason,
      reason_short_batch = reason_short_batch,
      W_used = W_used %||% config$W %||% NA_integer_,
      explore_rate_used = explore_rate_used,
    utility_selected_p50 = utility_selected_p50,
    utility_selected_p90 = utility_selected_p90,
    utility_candidate_p90 = utility_candidate_p90,
    iter_exit_path = iter_exit_path
  )

  state$batch_log <- dplyr::bind_rows(state$batch_log, row)
  state$log_counters$comparisons_observed <- as.integer(state$comparisons_observed)
  state$log_counters$failed_attempts <- as.integer(nrow(state$failed_attempts))

  .adaptive_progress_emit_iter(state)

  state
}

.adaptive_schedule_repair_pairs <- function(state, target_pairs, adaptive, seed) {
  target_pairs <- as.integer(target_pairs)
  if (is.na(target_pairs) || target_pairs < 0L) {
    rlang::abort("`target_pairs` must be a non-negative integer.")
  }
  if (target_pairs == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  phase <- state$phase
  iter <- as.integer(state$iter + 1L)
  iter_start <- Sys.time()

  fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size = target_pairs, seed = seed)
  state <- fit_out$state
  fit <- fit_out$fit
  new_pairs <- as.integer(fit_out$new_pairs %||% 0L)

  v3_config <- state$config$v3 %||% adaptive_v3_config(state$N)
  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  candidates <- generate_candidates(theta_summary, state, v3_config)

  candidates <- .adaptive_filter_candidates_to_draws(candidates, fit$theta_draws)
  n_candidates_generated <- as.integer(nrow(candidates))

  if (nrow(candidates) == 0L) {
    utilities <- tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    )
  } else {
    names(candidates)[names(candidates) == "i"] <- "i_id"
    names(candidates)[names(candidates) == "j"] <- "j_id"
    utilities <- compute_pair_utility_dispatch(
      fit = fit,
      candidates = candidates,
      state = state,
      config = v3_config,
      diagnostics_pass = state$posterior$diagnostics_pass
    )
  }
  state <- .adaptive_update_dup_threshold(state, utilities, fit_out$refit_performed)

  config_select <- v3_config
  config_select$batch_size <- target_pairs
  config_select$dup_max_count <- 0L
  selection <- .adaptive_select_exploration_only(
    state = state,
    candidates_with_utility = utilities,
    config = config_select,
    seed = seed
  )
  candidate_stats <- list(
    n_pairs_requested = as.integer(target_pairs),
    n_pairs_selected = as.integer(nrow(selection)),
    n_candidates_generated = n_candidates_generated,
    n_candidates_after_filters = as.integer(.adaptive_candidate_after_filters(utilities, state, config_select))
  )

  if (nrow(selection) == 0L) {
    state$iter <- iter
    state$mode <- "repair"
    state <- .adaptive_append_batch_log(
      state = state,
      iter = iter,
      phase = phase,
      mode = state$mode,
      created_at = iter_start,
      batch_size_target = target_pairs,
      selection = selection,
      candidate_stats = candidate_stats,
      candidate_starved = TRUE,
      fallback_stage = "repair",
      W_used = v3_config$W,
      config = config_select,
      exploration_only = TRUE,
      utilities = utilities,
      iter_exit_path = "repair_no_pairs"
    )
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  created_at <- iter_start
  rows <- vector("list", nrow(selection))
  state_local <- state
  for (idx in seq_len(nrow(selection))) {
    row <- selection[idx, , drop = FALSE]
    A_id <- as.character(row$A_id)
    B_id <- as.character(row$B_id)
    unordered_key <- as.character(row$unordered_key)
    ordered_key <- make_ordered_key(A_id, B_id)
    pair_uid <- pair_uid_from_state(state_local, unordered_key)

    pair_row <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = state_local$texts[[A_id]],
      B_text = state_local$texts[[B_id]],
      phase = phase,
      iter = iter,
      created_at = created_at,
      utility_raw = as.double(row$utility_raw),
      utility = as.double(row$utility),
      deg_A = as.integer(state_local$deg[[A_id]]),
      deg_B = as.integer(state_local$deg[[B_id]]),
      imb_A = as.integer(state_local$imb[[A_id]]),
      imb_B = as.integer(state_local$imb[[B_id]])
    )

    state_local <- record_presentation(state_local, A_id, B_id)
    state_local$history_pairs <- dplyr::bind_rows(state_local$history_pairs, pair_row)
    state_local$comparisons_scheduled <- as.integer(state_local$comparisons_scheduled + 1L)
    rows[[idx]] <- pair_row
  }

  pairs_tbl <- dplyr::bind_rows(rows)
  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  validate_pairs_tbl(pairs_tbl)
  required_cols <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs_tbl <- pairs_tbl[, c(required_cols, setdiff(names(pairs_tbl), required_cols)), drop = FALSE]
  state_local$iter <- iter
  state_local$mode <- "repair"
  state_local <- .adaptive_append_batch_log(
    state = state_local,
    iter = iter,
    phase = phase,
    mode = state_local$mode,
    created_at = created_at,
    batch_size_target = target_pairs,
    selection = selection,
    candidate_stats = candidate_stats,
    candidate_starved = nrow(selection) < target_pairs,
    fallback_stage = "repair",
    W_used = v3_config$W,
    config = config_select,
    exploration_only = TRUE,
    utilities = utilities
  )

  list(state = state_local, pairs = pairs_tbl)
}

.adaptive_warm_start_order <- function(state, i_id, j_id, pair_index) {
  imb_i <- state$imb[[i_id]]
  imb_j <- state$imb[[j_id]]
  if (imb_i > imb_j) {
    return(list(A_id = j_id, B_id = i_id))
  }
  if (imb_j > imb_i) {
    return(list(A_id = i_id, B_id = j_id))
  }
  if ((pair_index %% 2L) == 0L) {
    return(list(A_id = j_id, B_id = i_id))
  }
  list(A_id = i_id, B_id = j_id)
}

.adaptive_schedule_warm_start <- function(state, config, seed = NULL) {
  seed_use <- seed %||% state$seed
  warm_pairs <- warm_start(state$ids, config, seed = seed_use)
  warm_pairs <- tibble::as_tibble(warm_pairs)
  if (nrow(warm_pairs) == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  budget_remaining <- as.integer(state$budget_max - state$comparisons_scheduled)
  if (nrow(warm_pairs) > budget_remaining) {
    rlang::abort("`budget_max` is too small for the warm-start schedule.")
  }

  created_at <- Sys.time()
  rows <- vector("list", nrow(warm_pairs))

  for (idx in seq_len(nrow(warm_pairs))) {
    i_id <- as.character(warm_pairs$i[[idx]])
    j_id <- as.character(warm_pairs$j[[idx]])
    order <- .adaptive_warm_start_order(state, i_id, j_id, idx)
    unordered_key <- make_unordered_key(order$A_id, order$B_id)
    ordered_key <- make_ordered_key(order$A_id, order$B_id)
    pair_uid <- pair_uid_from_state(state, unordered_key)

    row <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = order$A_id,
      B_id = order$B_id,
      A_text = state$texts[[order$A_id]],
      B_text = state$texts[[order$B_id]],
      phase = "phase1",
      iter = 0L,
      created_at = created_at
    )

    state <- record_presentation(state, order$A_id, order$B_id)
    state$history_pairs <- dplyr::bind_rows(state$history_pairs, row)
    state$comparisons_scheduled <- as.integer(state$comparisons_scheduled + 1L)
    rows[[idx]] <- row
  }

  pairs_tbl <- dplyr::bind_rows(rows)
  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  validate_pairs_tbl(pairs_tbl)
  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs_tbl <- pairs_tbl[, c(required, setdiff(names(pairs_tbl), required)), drop = FALSE]

  state$phase <- "phase2"
  state$mode <- "adaptive"

  list(state = state, pairs = pairs_tbl)
}

.adaptive_schedule_next_pairs <- function(state, target_pairs, adaptive, seed, near_stop = FALSE) {
  validate_state(state)
  target_pairs <- as.integer(target_pairs)
  if (is.na(target_pairs) || target_pairs < 0L) {
    rlang::abort("`target_pairs` must be a non-negative integer.")
  }
  if (target_pairs == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  if (identical(state$mode, "stopped")) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  budget_remaining <- as.integer(state$budget_max - state$comparisons_scheduled)
  if (budget_remaining <= 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  target_pairs <- min(target_pairs, budget_remaining)

  total_pairs <- state$N * (state$N - 1L) / 2
  hard_cap_threshold <- ceiling(0.40 * total_pairs)
  n_unique_pairs_seen <- sum(state$pair_count >= 1L)
  if (n_unique_pairs_seen >= hard_cap_threshold) {
    state$mode <- "stopped"
    state$stop_reason <- "hard_cap_40pct"
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  remaining_unique <- hard_cap_threshold - n_unique_pairs_seen
  target_pairs <- min(target_pairs, as.integer(remaining_unique))

  if (identical(state$mode, "warm_start") && identical(state$phase, "phase1")) {
    return(.adaptive_schedule_warm_start(state, state$config$v3, seed = seed))
  }

  near_stop <- isTRUE(near_stop) || near_stop_from_state(state)
  if (near_stop && state$phase == "phase2") {
    state$phase <- "phase3"
  }
  phase <- state$phase
  batch_size <- target_pairs
  iter <- as.integer(state$iter + 1L)
  iter_start <- Sys.time()

  fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size, seed)
  state <- fit_out$state
  fit <- fit_out$fit
  new_pairs <- as.integer(fit_out$new_pairs %||% 0L)

  gate_out <- .adaptive_apply_diagnostics_gate(
    state,
    fit,
    state$config$v3,
    near_stop = near_stop,
    refit_performed = fit_out$refit_performed
  )
  state <- gate_out$state
  if (identical(state$mode, "stopped")) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  v3_config <- state$config$v3 %||% rlang::abort("`state$config$v3` must be set.")
  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  candidates <- generate_candidates(theta_summary, state, v3_config, allow_repeats = TRUE)

  candidates <- .adaptive_filter_candidates_to_draws(candidates, fit$theta_draws)

  if (nrow(candidates) == 0L) {
    utilities <- tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    )
  } else {
    names(candidates)[names(candidates) == "i"] <- "i_id"
    names(candidates)[names(candidates) == "j"] <- "j_id"
    utilities <- compute_pair_utility_dispatch(
      fit = fit,
      candidates = candidates,
      state = state,
      config = v3_config,
      diagnostics_pass = state$posterior$diagnostics_pass
    )
  }
  state <- .adaptive_update_dup_threshold(state, utilities, fit_out$refit_performed)

  config_select <- v3_config
  config_select$batch_size <- batch_size
  diagnostics_pass <- isTRUE(state$posterior$diagnostics_pass)
  exploration_only <- identical(state$mode, "repair")
  safe_no_utility <- !diagnostics_pass && !identical(state$mode, "repair")
  if (isTRUE(exploration_only)) {
    config_select$dup_max_count <- 0L
  }

  check_stop <- !isTRUE(state$config$skip_stop_checks)
  if (isTRUE(check_stop)) {
    stop_metrics <- compute_stop_metrics(
      state = state,
      fit = fit,
      candidates_with_utility = utilities,
      config = v3_config
    )
    stop_metrics$refit_performed <- isTRUE(fit_out$refit_performed)
    state$posterior$stop_metrics <- stop_metrics

    stop_out <- should_stop(stop_metrics, state, v3_config)
    state <- stop_out$state
    if (isTRUE(fit_out$refit_performed)) {
      history <- state$posterior$theta_mean_history %||% list()
      if (!is.list(history)) {
        rlang::abort("`state$posterior$theta_mean_history` must be a list when set.")
      }
      min_refits_for_stability <- as.integer(v3_config$min_refits_for_stability)
      if (is.na(min_refits_for_stability) || min_refits_for_stability < 1L) {
        rlang::abort("`config$min_refits_for_stability` must be a positive integer.")
      }
      current_refit <- length(history) + 1L
      state$stop_candidate <- isTRUE(current_refit >= min_refits_for_stability &&
        isTRUE(stop_metrics$diagnostics_pass))

      round_row <- build_round_log_row(
        state = state,
        fit = fit,
        metrics = stop_metrics,
        stop_out = stop_out,
        config = v3_config,
        batch_size = batch_size,
        window_W = config_select$W,
        exploration_rate = config_select$explore_rate,
        new_pairs = new_pairs
      )
      prior_log <- state$config$round_log %||% round_log_schema()
      state$config$round_log <- dplyr::bind_rows(prior_log, round_row)
      .adaptive_progress_emit_refit(state, round_row, v3_config)
    }
    if (isTRUE(stop_out$stop_decision) || identical(state$mode, "stopped")) {
      return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
    }
  } else {
    stop_metrics <- compute_stop_metrics(
      state = state,
      fit = fit,
      candidates_with_utility = utilities,
      config = v3_config
    )
    stop_metrics$refit_performed <- isTRUE(fit_out$refit_performed)
    state$posterior$stop_metrics <- stop_metrics

    if (isTRUE(fit_out$refit_performed)) {
      round_row <- build_round_log_row(
        state = state,
        fit = fit,
        metrics = stop_metrics,
        stop_out = list(stop_decision = NA, stop_reason = state$stop_reason %||% NA_character_),
        config = v3_config,
        batch_size = batch_size,
        window_W = config_select$W,
        exploration_rate = config_select$explore_rate,
        new_pairs = new_pairs
      )
      prior_log <- state$config$round_log %||% round_log_schema()
      state$config$round_log <- dplyr::bind_rows(prior_log, round_row)
      .adaptive_progress_emit_refit(state, round_row, v3_config)
      state <- .adaptive_update_theta_history(state, fit)
    }
  }

  selection_out <- .adaptive_select_batch_by_ladder(
    state = state,
    fit = fit,
    theta_summary = theta_summary,
    config = config_select,
    candidates_with_utility = utilities,
    n_candidates_generated = nrow(candidates),
    seed = seed,
    exploration_only = exploration_only,
    safe_no_utility = safe_no_utility
  )
  selection <- selection_out$selection
  state$posterior$candidate_starved <- selection_out$candidate_starved
  state$posterior$candidate_stats <- selection_out$candidate_stats
  state$posterior$candidate_fallback_stage <- selection_out$fallback_stage

  if (nrow(selection) == 0L) {
    state$iter <- iter
      state <- .adaptive_append_batch_log(
        state = state,
        iter = iter,
        phase = phase,
        mode = state$mode,
        created_at = iter_start,
        batch_size_target = batch_size,
        selection = selection,
        candidate_stats = selection_out$candidate_stats,
        candidate_starved = selection_out$candidate_starved,
        fallback_exhausted = selection_out$fallback_exhausted,
        fallback_stage = selection_out$fallback_stage,
        fallback_used = selection_out$fallback_used,
        fallback_path = selection_out$fallback_path,
        stage_attempts = selection_out$stage_attempts,
        W_used = selection_out$W_used,
        config = config_select,
        exploration_only = exploration_only,
        safe_no_utility = safe_no_utility,
        utilities = utilities,
        iter_exit_path = "no_pairs_selected"
      )
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  created_at <- iter_start
  rows <- vector("list", nrow(selection))
  state_local <- state
  for (idx in seq_len(nrow(selection))) {
    row <- selection[idx, , drop = FALSE]
    A_id <- as.character(row$A_id)
    B_id <- as.character(row$B_id)
    unordered_key <- as.character(row$unordered_key)
    ordered_key <- make_ordered_key(A_id, B_id)
    pair_uid <- pair_uid_from_state(state_local, unordered_key)

    pair_row <- tibble::tibble(
      pair_uid = pair_uid,
      unordered_key = unordered_key,
      ordered_key = ordered_key,
      A_id = A_id,
      B_id = B_id,
      A_text = state_local$texts[[A_id]],
      B_text = state_local$texts[[B_id]],
      phase = phase,
      iter = iter,
      created_at = created_at,
      utility_raw = as.double(row$utility_raw),
      utility = as.double(row$utility),
      deg_A = as.integer(state_local$deg[[A_id]]),
      deg_B = as.integer(state_local$deg[[B_id]]),
      imb_A = as.integer(state_local$imb[[A_id]]),
      imb_B = as.integer(state_local$imb[[B_id]])
    )

    state_local <- record_presentation(state_local, A_id, B_id)
    state_local$history_pairs <- dplyr::bind_rows(state_local$history_pairs, pair_row)
    state_local$comparisons_scheduled <- as.integer(state_local$comparisons_scheduled + 1L)
    rows[[idx]] <- pair_row
  }

  pairs_tbl <- dplyr::bind_rows(rows)
  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  validate_pairs_tbl(pairs_tbl)
  required_cols <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  pairs_tbl <- pairs_tbl[, c(required_cols, setdiff(names(pairs_tbl), required_cols)), drop = FALSE]
  state_local$iter <- iter
    state_local <- .adaptive_append_batch_log(
      state = state_local,
      iter = iter,
      phase = phase,
      mode = state_local$mode,
      created_at = created_at,
      batch_size_target = batch_size,
      selection = selection,
      candidate_stats = selection_out$candidate_stats,
      candidate_starved = selection_out$candidate_starved,
      fallback_exhausted = selection_out$fallback_exhausted,
      fallback_stage = selection_out$fallback_stage,
      fallback_used = selection_out$fallback_used,
      fallback_path = selection_out$fallback_path,
      stage_attempts = selection_out$stage_attempts,
      W_used = selection_out$W_used,
      config = config_select,
      exploration_only = exploration_only,
      safe_no_utility = safe_no_utility,
      utilities = utilities
    )
  list(state = state_local, pairs = pairs_tbl)
}

.adaptive_submit_live <- function(pairs, model, trait_name, trait_description,
                                  prompt_template, backend, submission) {
  reserved <- c(
    "pairs", "model", "trait_name", "trait_description",
    "prompt_template", "backend"
  )
  submission <- .adaptive_sanitize_submission_options(submission, reserved = reserved)
  args <- c(list(
    pairs = pairs,
    model = model,
    trait_name = trait_name,
    trait_description = trait_description,
    prompt_template = prompt_template,
    backend = backend
  ), submission)
  do.call(submit_llm_pairs, args)
}

.adaptive_submit_batch <- function(pairs, model, trait_name, trait_description,
                                   prompt_template, backend, submission, output_dir) {
  reserved <- c(
    "pairs", "model", "trait_name", "trait_description",
    "prompt_template", "backend", "output_dir"
  )
  submission <- .adaptive_sanitize_submission_options(submission, reserved = reserved)
  args <- c(list(
    pairs = pairs,
    model = model,
    trait_name = trait_name,
    trait_description = trait_description,
    prompt_template = prompt_template,
    backend = backend,
    output_dir = output_dir
  ), submission)
  do.call(llm_submit_pairs_multi_batch, args)
}

.adaptive_next_action <- function(state, scheduled_pairs) {
  stop_reason <- state$stop_reason %||% NA_character_
  if (identical(state$mode, "stopped") &&
    is.character(stop_reason) &&
    length(stop_reason) == 1L &&
    !is.na(stop_reason) &&
    nzchar(stop_reason)) {
    return(list(action = "done", reason = stop_reason))
  }
  if (state$comparisons_scheduled >= state$budget_max) {
    return(list(action = "done", reason = "budget_exhausted"))
  }
  if (scheduled_pairs == 0L) {
    return(list(action = "done", reason = "no_feasible_pairs"))
  }
  list(action = "resume", reason = "more_pairs_available")
}

.adaptive_schedule_target <- function(state, adaptive) {
  batch_sizes <- state$config$batch_sizes
  if (is.null(batch_sizes)) {
    state <- .adaptive_get_batch_sizes(state, adaptive)
    batch_sizes <- state$config$batch_sizes
  }
  if (state$phase != "phase1" && near_stop_from_state(state)) {
    state$phase <- "phase3"
  }
  if (state$phase == "phase3") {
    return(list(state = state, target = batch_sizes$BATCH3))
  }
  if (state$phase == "phase2") {
    return(list(state = state, target = batch_sizes$BATCH2))
  }
  list(state = state, target = batch_sizes$BATCH1)
}

.adaptive_replacement_target <- function(missing, adaptive, batch_size) {
  missing <- as.integer(missing)
  if (missing <= 0L) return(0L)
  max_replacements <- adaptive$max_replacements %||% batch_size
  max_replacements <- as.integer(max_replacements)
  if (is.na(max_replacements) || max_replacements < 1L) {
    max_replacements <- batch_size
  }
  min(missing, max_replacements)
}

.adaptive_phase_scalar_from_pairs <- function(pairs_tbl) {
  if (is.null(pairs_tbl) || nrow(pairs_tbl) == 0L) return(NULL)
  pairs_tbl <- tibble::as_tibble(pairs_tbl)
  if (!"phase" %in% names(pairs_tbl)) return(NULL)
  phases <- unique(as.character(pairs_tbl$phase))
  phases <- phases[!is.na(phases) & phases != ""]
  if (length(phases) == 0L) return(NULL)
  if (length(phases) > 1L) {
    rlang::abort("`pairs_submitted` must contain a single phase value.")
  }
  phases[[1L]]
}

.adaptive_schedule_replacement_pairs <- function(state, target_pairs, adaptive, seed, replacement_phase) {
  validate_state(state)
  target_pairs <- as.integer(target_pairs)
  if (is.na(target_pairs) || target_pairs < 0L) {
    rlang::abort("`target_pairs` must be a non-negative integer.")
  }
  if (target_pairs == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  budget_remaining <- as.integer(state$budget_max - state$comparisons_scheduled)
  if (budget_remaining <= 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  target_pairs <- min(target_pairs, budget_remaining)

  replacement_phase <- as.character(replacement_phase)
  if (length(replacement_phase) != 1L || is.na(replacement_phase) || !nzchar(replacement_phase)) {
    rlang::abort("`replacement_phase` must be a non-empty character scalar.")
  }
  state$config$skip_stop_checks <- TRUE
  out <- .adaptive_schedule_next_pairs(state, target_pairs, adaptive, seed = seed)
  out$state$config$skip_stop_checks <- NULL
  out
}

.adaptive_run_replacements_live <- function(state, model, trait_name, trait_description,
                                            prompt_template, backend, adaptive, submission,
                                            missing, seed, replacement_phase,
                                            base_batch_size) {
  if (missing <= 0L) {
    return(list(state = state, submissions = list()))
  }

  submissions <- list()
  base_batch_size <- as.integer(base_batch_size)
  if (is.na(base_batch_size) || base_batch_size < 1L) {
    base_batch_size <- as.integer(missing)
  }
  refill_rounds <- as.integer(adaptive$max_refill_rounds)
  if (is.na(refill_rounds) || refill_rounds < 1L) {
    refill_rounds <- 1L
  }
  replacement_phase <- replacement_phase %||% "phase1"

  for (round in seq_len(refill_rounds)) {
    target <- .adaptive_replacement_target(missing, adaptive, base_batch_size)
    if (target <= 0L) break

    scheduled <- .adaptive_schedule_replacement_pairs(
      state,
      target,
      adaptive,
      seed = seed,
      replacement_phase = replacement_phase
    )
    state <- scheduled$state
    pairs <- scheduled$pairs
    if (nrow(pairs) == 0L) break

    submit_tbl <- .adaptive_pairs_to_submit_tbl(pairs)
    res <- .adaptive_submit_live(
      pairs = submit_tbl,
      model = model,
      trait_name = trait_name,
      trait_description = trait_description,
      prompt_template = prompt_template,
      backend = backend,
      submission = submission
    )
    normalized <- .adaptive_normalize_submission_output(
      raw = res,
      pairs_submitted = pairs,
      backend = backend,
      model = model,
      include_raw = isTRUE(submission$include_raw)
    )
    ingest <- .adaptive_ingest_results_incremental(state, normalized$results)
    state <- ingest$state
    state <- .adaptive_append_failed_attempts(
      state,
      normalized$failed_attempts,
      phase = unique(pairs$phase),
      iter = unique(pairs$iter)
    )
    state <- .adaptive_rollback_presentations(
      state,
      pairs_submitted = pairs,
      failed_attempts = normalized$failed_attempts
    )
    submissions[[length(submissions) + 1L]] <- list(pairs = pairs, results = normalized$results)

    observed_now <- sum(pairs$pair_uid %in% .adaptive_results_seen_names(state))
    missing <- missing - observed_now
    if (missing <= 0L) break
  }

  list(state = state, submissions = submissions)
}

#' Adaptive pairwise ranking with warm start and Bayesian refinement
#'
#' Initialize an adaptive ranking run, schedule Phase 1 warm-start pairs, and
#' submit them in live or batch mode. Live mode submits immediately and ingests
#' observed outcomes. Batch mode submits jobs, saves state, and returns resume
#' metadata for later polling. A single call may not complete the full run;
#' use \code{adaptive_rank_resume()} to continue.
#'
#' @details
#' Adaptive ranking proceeds in three phases: Phase 1 (warm start), Phase 2
#' (adaptive refinement), and Phase 3 (near-stop polish). \code{adaptive_rank_start()}
#' creates a fresh \code{adaptive_state}, schedules Phase 1 pairs up to the
#' Phase 1 target, and submits those comparisons. \code{adaptive_rank_resume()}
#' ingests newly observed results and schedules subsequent adaptive batches.
#'
#' Exposure and observation are distinct. Scheduled comparisons update exposure
#' counters immediately (pairs, degrees, position counts), while observed
#' outcomes are ingested only after a backend returns results. Failed attempts
#' are logged separately and never treated as observations. This separation
#' prevents retries or missing results from contaminating the ranking signal.
#'
#' Adaptive refits are triggered when at least \code{refit_B} new observations
#' arrive since the last refit (or when the first refit is needed). Each refit
#' updates the posterior used for scheduling and stopping checks. When stopping
#' criteria are met, the state moves to \code{mode = "stopped"} and no new
#' batches are scheduled.
#'
#' All LLM outputs are normalized into a single canonical results schema,
#' independent of backend, and all downstream logic operates exclusively on
#' this canonical form.
#'
#' @section Model variants:
#' Adaptive v3 tracks model variants in the fit contract and logs. Supported
#' labels include \code{"btl"} (plain Bradley--Terry), \code{"btl_e"} (lapse
#' epsilon), \code{"btl_b"} (position bias beta), and \code{"btl_e_b"} (both;
#' default). Variants that omit parameters still carry the full output schema:
#' percentile columns such as \code{epsilon_p2.5} or \code{beta_p50} remain
#' present and are \code{NA} when the parameter is not estimated.
#'
#' @section Stopping logic:
#' Stopping is a staged gate evaluated only after a refit. First, the run must
#' meet the minimum observed comparisons (\code{M1_target}). Next, diagnostics
#' must pass: zero divergences, \code{max_rhat} below \code{max_rhat}, and
#' \code{min_ess_bulk} above a threshold (using \code{min_ess_bulk_near_stop}
#' once near-stop checks begin). If diagnostics pass, reliability must exceed
#' \code{eap_reliability_min}. Stability is evaluated against a lagged refit:
#' theta means must have correlation at least \code{theta_corr_min} and a
#' relative SD change no larger than \code{theta_sd_rel_change_max}, and ranks
#' must have Spearman correlation at least \code{rank_spearman_min}. A stop is
#' declared only after \code{stability_consecutive} consecutive refits pass all
#' checks. Refits are eligible for stability checks only after
#' \code{min_refits_for_stability} and once a lag of \code{stability_lag} refits
#' is available.
#'
#' @section Adaptive configuration:
#' The \code{adaptive} list controls run-scale scheduling:
#' \describe{
#'   \item{\code{d1}}{Warm-start degree target used to compute the default
#'   \code{M1_target} (\code{floor(N * d1 / 2)}).}
#'   \item{\code{bins}}{Reserved for legacy binning logic; currently unused.}
#'   \item{\code{mix_struct}}{Reserved for legacy mixing logic; currently unused.}
#'   \item{\code{within_adj_split}}{Reserved for legacy adjacency splits;
#'   currently unused.}
#'   \item{\code{exploration_frac}}{Reserved for legacy exploration logic;
#'   currently unused in v3 scheduling.}
#'   \item{\code{per_item_cap}}{Reserved per-item cap; currently unused.}
#'   \item{\code{batch_overrides}}{Named list overriding batch sizes computed by
#'   \code{compute_batch_sizes()} (e.g., \code{BATCH1}, \code{BATCH2},
#'   \code{BATCH3}, \code{CW}).}
#'   \item{\code{max_refill_rounds}}{Maximum number of live replacement rounds
#'   attempted when submissions are missing.}
#'   \item{\code{max_replacements}}{Maximum number of replacement pairs per
#'   iteration; defaults to the batch size.}
#'   \item{\code{max_iterations}}{Maximum number of start/resume iterations in
#'   wrappers such as \code{adaptive_rank_run_live()}.}
#'   \item{\code{budget_max}}{Hard cap on scheduled comparisons; defaults to
#'   \code{floor(0.40 * choose(N, 2))}.}
#'   \item{\code{M1_target}}{Minimum observed comparisons before stop checks
#'   are eligible; defaults to \code{floor(N * d1 / 2)}.}
#' }
#' The \code{adaptive$v3} list overrides v3 configuration fields:
#' \describe{
#'   \item{\code{W}}{Window size used in candidate generation.}
#'   \item{\code{A_anchors}}{Anchor count used in candidate selection.}
#'   \item{\code{C_max}}{Maximum candidate pairs considered per stage.}
#'   \item{\code{refit_B}}{Refit cadence in newly observed pairs.}
#'   \item{\code{batch_size}}{Target pairs scheduled per iteration.}
#'   \item{\code{explore_rate}}{Fraction of exploration pairs within a batch.}
#'   \item{\code{min_degree}}{Warm-start minimum degree per item.}
#'   \item{\code{target_mean_degree}}{Optional warm-start mean degree target;
#'   expands the initial schedule when set.}
#'   \item{\code{dup_p_margin}}{Repeat allowance margin for ambiguous pairs.}
#'   \item{\code{dup_max_count}}{Maximum repeated presentations per unordered
#'   pair.}
#'   \item{\code{dup_utility_quantile}}{Utility quantile for repeat allowance.}
#'   \item{\code{hard_cap_frac}}{Fraction of all unordered pairs that triggers
#'   the hard stop (\code{0.40} by default).}
#'   \item{\code{eap_reliability_min}}{Minimum \code{reliability_EAP} for stop
#'   eligibility.}
#'   \item{\code{min_refits_for_stability}}{Minimum refits before stability
#'   checks are evaluated.}
#'   \item{\code{stability_lag}}{Lag (in refits) used to compare theta and rank
#'   stability.}
#'   \item{\code{theta_corr_min}}{Minimum correlation between current and lagged
#'   theta means.}
#'   \item{\code{theta_sd_rel_change_max}}{Maximum relative change in theta SD
#'   versus the lagged refit.}
#'   \item{\code{rank_spearman_min}}{Minimum Spearman correlation between
#'   current and lagged ranks.}
#'   \item{\code{stability_consecutive}}{Consecutive refits required to stop.}
#'   \item{\code{max_rhat}}{Maximum allowed R-hat for diagnostics pass.}
#'   \item{\code{min_ess_bulk}}{Minimum bulk ESS for diagnostics pass.}
#'   \item{\code{min_ess_bulk_near_stop}}{Minimum bulk ESS once near-stop checks
#'   have begun.}
#'   \item{\code{require_divergences_zero}}{Reserved; diagnostics currently
#'   require zero divergences regardless of this value.}
#'   \item{\code{repair_max_cycles}}{Maximum repair cycles before giving up.}
#'   \item{\code{progress}}{Logical toggle for progress reporting.}
#'   \item{\code{progress_every_iter}}{Iteration cadence for progress prints.}
#'   \item{\code{progress_every_refit}}{Refit cadence for progress prints.}
#'   \item{\code{progress_level}}{Verbosity level: \code{"basic"},
#'   \code{"refit"}, or \code{"full"}.}
#'   \item{\code{write_outputs}}{Whether to write \code{batch_log},
#'   \code{round_log}, and \code{item_summary} to disk.}
#'   \item{\code{output_dir}}{Directory used for adaptive artifacts when
#'   \code{write_outputs = TRUE}.}
#'   \item{\code{keep_draws}}{Whether to write thinned posterior draws to disk.}
#'   \item{\code{thin_draws}}{Thinning interval for saved draws.}
#'   \item{\code{cmdstan}}{CmdStan configuration list (chains, parallelism,
#'   core fraction, output directory).}
#' }
#'
#' @section Canonical outputs:
#' Canonical adaptive outputs are \code{batch_log}, \code{round_log}, and
#' \code{item_summary}. They are stored on the state as \code{state$batch_log},
#' \code{state$config$round_log}, and \code{state$config$item_summary}. When
#' \code{adaptive$v3$write_outputs = TRUE}, they are written to
#' \code{batch_log.rds}, \code{round_log.rds}, and \code{item_summary.rds} in
#' \code{output_dir}. Summary helpers such as \code{summarize_iterations()},
#' \code{summarize_refits()}, and \code{summarize_items()} are pure views of
#' these tables.
#'
#' Batch log columns (one row per iteration):
#' \describe{
#'   \item{\code{iter}}{Iteration index (starting at 1).}
#'   \item{\code{phase}}{Phase label: \code{"phase1"}, \code{"phase2"}, or
#'   \code{"phase3"}.}
#'   \item{\code{mode}}{Run mode: \code{"warm_start"}, \code{"adaptive"}, or
#'   \code{"stopped"}.}
#'   \item{\code{created_at}}{Timestamp when the iteration log row was created.}
#'   \item{\code{batch_size_target}}{Target number of pairs for the iteration.}
#'   \item{\code{n_pairs_selected}}{Pairs scheduled in this iteration.}
#'   \item{\code{n_pairs_completed}}{New results observed since the previous
#'   iteration log.}
#'   \item{\code{n_pairs_failed}}{New failed attempts since the previous log.}
#'   \item{\code{backlog_unjudged}}{Scheduled minus completed pairs after
#'   scheduling.}
#'   \item{\code{n_explore_target}}{Target exploration pairs in the batch.}
#'   \item{\code{n_explore_selected}}{Exploration pairs scheduled.}
#'   \item{\code{n_exploit_target}}{Target exploitation pairs in the batch.}
#'   \item{\code{n_exploit_selected}}{Exploitation pairs scheduled.}
#'   \item{\code{n_candidates_generated}}{Candidate pairs generated before
#'   filtering.}
#'   \item{\code{n_candidates_after_filters}}{Candidates remaining after
#'   filtering (duplicates, degree, constraints).}
#'   \item{\code{candidate_starved}}{TRUE when fewer than target pairs were
#'   scheduled due to candidate scarcity.}
#'   \item{\code{fallback_exhausted}}{TRUE when the fallback ladder was fully
#'   exhausted without reaching the target batch size.}
#'   \item{\code{fallback_used}}{Fallback mode used when starved (from
#'   \code{.adaptive_fallback_used_levels()}).}
#'   \item{\code{fallback_path}}{Fallback path taken for the iteration.}
#'   \item{\code{a1_stage}}{Stage label for the first fallback attempt.}
#'   \item{\code{a1_W_used}}{Window size used in stage 1.}
#'   \item{\code{a1_anchor_pool}}{Anchor pool used in stage 1.}
#'   \item{\code{a1_n_generated}}{Candidates generated in stage 1.}
#'   \item{\code{a1_n_survive}}{Candidates surviving filters in stage 1.}
#'   \item{\code{a1_n_selected}}{Pairs selected in stage 1.}
#'   \item{\code{a2_stage}}{Stage label for the second fallback attempt.}
#'   \item{\code{a2_W_used}}{Window size used in stage 2.}
#'   \item{\code{a2_anchor_pool}}{Anchor pool used in stage 2.}
#'   \item{\code{a2_n_generated}}{Candidates generated in stage 2.}
#'   \item{\code{a2_n_survive}}{Candidates surviving filters in stage 2.}
#'   \item{\code{a2_n_selected}}{Pairs selected in stage 2.}
#'   \item{\code{a3_stage}}{Stage label for the third fallback attempt.}
#'   \item{\code{a3_W_used}}{Window size used in stage 3.}
#'   \item{\code{a3_anchor_pool}}{Anchor pool used in stage 3.}
#'   \item{\code{a3_n_generated}}{Candidates generated in stage 3.}
#'   \item{\code{a3_n_survive}}{Candidates surviving filters in stage 3.}
#'   \item{\code{a3_n_selected}}{Pairs selected in stage 3.}
#'   \item{\code{aN_tried}}{Number of fallback stages attempted.}
#'   \item{\code{aN_best_stage}}{Best fallback stage selected.}
#'   \item{\code{aN_best_n_generated}}{Candidates generated in best stage.}
#'   \item{\code{aN_best_n_survive}}{Candidates surviving in best stage.}
#'   \item{\code{aN_best_n_selected}}{Pairs selected in best stage.}
#'   \item{\code{starvation_reason}}{Primary starvation reason label (from
#'   \code{.adaptive_starvation_reason_levels()}).}
#'   \item{\code{reason_short_batch}}{Human-readable reason for short batch.}
#'   \item{\code{W_used}}{Window size actually used for selection.}
#'   \item{\code{explore_rate_used}}{Exploration rate actually used.}
#'   \item{\code{utility_selected_p50}}{Median utility of selected pairs.}
#'   \item{\code{utility_selected_p90}}{90th percentile utility of selected
#'   pairs.}
#'   \item{\code{utility_candidate_p90}}{90th percentile utility of candidate
#'   pool.}
#'   \item{\code{iter_exit_path}}{Exit path label for the iteration.}
#' }
#'
#' Round log columns (one row per refit, canonical stop audit trail):
#' \describe{
#'   \item{\code{round_id}}{Refit index (starting at 1).}
#'   \item{\code{iter_at_refit}}{Iteration index at refit time.}
#'   \item{\code{mode}}{Run mode at refit.}
#'   \item{\code{model_variant}}{Model variant label (see Model variants).}
#'   \item{\code{n_items}}{Number of items in the run.}
#'   \item{\code{total_pairs}}{Total unordered pairs, \eqn{N(N-1)/2}.}
#'   \item{\code{hard_cap_threshold}}{Hard cap count based on
#'   \code{hard_cap_frac}.}
#'   \item{\code{n_unique_pairs_seen}}{Unique unordered pairs observed at least
#'   once.}
#'   \item{\code{scheduled_pairs}}{Total scheduled pairs.}
#'   \item{\code{completed_pairs}}{Total observed results.}
#'   \item{\code{backlog_unjudged}}{Scheduled minus completed at refit time.}
#'   \item{\code{new_pairs}}{Pairs observed since the previous refit.}
#'   \item{\code{proposed_pairs}}{Candidate pairs evaluated at refit time.}
#'   \item{\code{batch_size}}{Batch size target in effect at refit.}
#'   \item{\code{window_W}}{Window size used for candidate generation.}
#'   \item{\code{exploration_rate}}{Exploration rate used for candidate mixes.}
#'   \item{\code{mean_degree}}{Mean item degree at refit.}
#'   \item{\code{min_degree}}{Minimum item degree at refit.}
#'   \item{\code{pos_balance_mean}}{Mean position balance across items.}
#'   \item{\code{pos_balance_sd}}{SD of position balance across items.}
#'   \item{\code{epsilon_mean}}{Posterior mean of epsilon (NA if absent).}
#'   \item{\code{epsilon_p2.5}}{2.5th percentile of epsilon (NA if absent).}
#'   \item{\code{epsilon_p5}}{5th percentile of epsilon (NA if absent).}
#'   \item{\code{epsilon_p50}}{Median epsilon (NA if absent).}
#'   \item{\code{epsilon_p95}}{95th percentile of epsilon (NA if absent).}
#'   \item{\code{epsilon_p97.5}}{97.5th percentile of epsilon (NA if absent).}
#'   \item{\code{beta_mean}}{Posterior mean of beta (NA if absent).}
#'   \item{\code{beta_p2.5}}{2.5th percentile of beta (NA if absent).}
#'   \item{\code{beta_p5}}{5th percentile of beta (NA if absent).}
#'   \item{\code{beta_p50}}{Median beta (NA if absent).}
#'   \item{\code{beta_p95}}{95th percentile of beta (NA if absent).}
#'   \item{\code{beta_p97.5}}{97.5th percentile of beta (NA if absent).}
#'   \item{\code{divergences}}{Total divergences reported by MCMC.}
#'   \item{\code{max_rhat}}{Maximum R-hat across parameters.}
#'   \item{\code{min_ess_bulk}}{Minimum bulk ESS across parameters.}
#'   \item{\code{diagnostics_pass}}{Diagnostics gate status.}
#'   \item{\code{reliability_EAP}}{EAP reliability statistic.}
#'   \item{\code{theta_sd_eap}}{SD of theta means at refit.}
#'   \item{\code{rho_theta_lag}}{Correlation between current and lagged theta
#'   means.}
#'   \item{\code{delta_sd_theta_lag}}{Relative change in theta SD vs lagged
#'   refit.}
#'   \item{\code{rho_rank_lag}}{Spearman correlation between current and lagged
#'   ranks.}
#'   \item{\code{rank_stability_pass}}{Rank stability gate status.}
#'   \item{\code{stop_passes}}{Consecutive refits meeting all stop checks.}
#'   \item{\code{stop_eligible}}{TRUE when refit count meets stability minimum.}
#'   \item{\code{stop_decision}}{TRUE when stop criteria are met.}
#'   \item{\code{stop_reason}}{Stop reason label when stopped.}
#'   \item{\code{starve_rate_since_last_refit}}{Fraction of iterations since
#'   last refit that were candidate-starved.}
#'   \item{\code{fallback_rate_since_last_refit}}{Fraction of iterations since
#'   last refit that used fallback selection.}
#'   \item{\code{fallback_used_mode}}{Most frequent fallback mode since last
#'   refit.}
#'   \item{\code{starvation_reason_mode}}{Most frequent starvation reason since
#'   last refit.}
#'   \item{\code{mcmc_chains}}{Number of MCMC chains used.}
#'   \item{\code{mcmc_parallel_chains}}{Number of chains run in parallel.}
#'   \item{\code{mcmc_core_fraction}}{Core fraction used to size parallel
#'   chains.}
#'   \item{\code{mcmc_cores_detected_physical}}{Detected physical cores.}
#'   \item{\code{mcmc_cores_detected_logical}}{Detected logical cores.}
#'   \item{\code{mcmc_threads_per_chain}}{Threads per chain (if set).}
#'   \item{\code{mcmc_cmdstanr_version}}{CmdStanR version used for the refit.}
#' }
#'
#' Item summary columns (one row per item):
#' \describe{
#'   \item{\code{ID}}{Item identifier.}
#'   \item{\code{deg}}{Observed degree (total comparisons involving the item).}
#'   \item{\code{posA_prop}}{Share of comparisons where the item appeared in
#'   position A.}
#'   \item{\code{theta_mean}}{Posterior mean of item strength.}
#'   \item{\code{theta_p2.5}}{2.5th percentile of theta.}
#'   \item{\code{theta_p5}}{5th percentile of theta.}
#'   \item{\code{theta_p50}}{Median theta.}
#'   \item{\code{theta_p95}}{95th percentile of theta.}
#'   \item{\code{theta_p97.5}}{97.5th percentile of theta.}
#'   \item{\code{theta_sd}}{Posterior SD of theta.}
#'   \item{\code{rank_mean}}{Mean induced rank (lower is better).}
#'   \item{\code{rank_p2.5}}{2.5th percentile of rank.}
#'   \item{\code{rank_p5}}{5th percentile of rank.}
#'   \item{\code{rank_p50}}{Median rank.}
#'   \item{\code{rank_p95}}{95th percentile of rank.}
#'   \item{\code{rank_p97.5}}{97.5th percentile of rank.}
#'   \item{\code{rank_sd}}{Posterior SD of rank.}
#' }
#'
#' @param samples A data frame or tibble with columns \code{ID} and \code{text}.
#' @param model Model identifier for the selected backend.
#' @param trait_name Short label for the trait.
#' @param trait_description Full-text trait description.
#' @param prompt_template Optional prompt template string. Defaults to
#'   \code{set_prompt_template()}.
#' @param backend Backend name. For live mode: one of \code{"openai"},
#'   \code{"anthropic"}, \code{"gemini"}, \code{"together"}, or \code{"ollama"}.
#'   For batch mode: one of \code{"openai"}, \code{"anthropic"}, or
#'   \code{"gemini"}.
#' @param mode Submission mode. Either \code{"live"} or \code{"batch"}.
#' @param submission A list of arguments passed through to
#'   \code{submit_llm_pairs()} (live) or \code{llm_submit_pairs_multi_batch()}
#'   (batch). Common options include \code{endpoint}, \code{include_raw},
#'   \code{batch_size}, and \code{n_segments}. The list is extensible in future
#'   versions.
#' @param adaptive A list of adaptive configuration overrides. See the
#'   "Adaptive configuration" and "Canonical outputs" sections for v3-specific
#'   controls, stopping thresholds, and logging behavior. Progress printing is
#'   enabled by default; set \code{adaptive$v3$progress = FALSE} for silent
#'   runs or adjust \code{progress_level} for more detail.
#' @param paths A list with optional \code{state_path} and \code{output_dir}.
#'   For batch mode, \code{state_path} defaults to
#'   \code{file.path(output_dir, "adaptive_state.rds")}.
#' @param seed Optional integer seed for deterministic scheduling.
#'
#' @return A list with:
#' \describe{
#'   \item{state}{The updated \code{adaptive_state}.}
#'   \item{state_path}{Path where the state was saved (batch mode only).}
#'   \item{submission_info}{Metadata needed for resume, including pairs submitted.}
#'   \item{next_action}{List with \code{action} and \code{reason}.}
#'   \item{final_summary}{Cached summary payload, if produced.}
#' }
#'
#' @examples
#' # Minimal synthetic setup (no submission).
#' samples <- tibble::tibble(
#'   ID = c("S1", "S2", "S3", "S4"),
#'   text = c("alpha", "bravo", "charlie", "delta")
#' )
#' td <- trait_description("overall_quality")
#' adaptive_cfg <- list(d1 = 8, M1_target = 40)
#'
#' \dontrun{
#' # Live start (submits immediately and ingests observed results)
#' start_out <- adaptive_rank_start(
#'   samples = samples,
#'   model = "gpt-4.1",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   backend = "openai",
#'   mode = "live",
#'   adaptive = adaptive_cfg,
#'   seed = 123
#' )
#'
#' # Live resume (continues scheduling; may require multiple calls)
#' resume_out <- adaptive_rank_resume(
#'   state = start_out$state,
#'   mode = "live",
#'   submission_info = start_out$submission_info,
#'   adaptive = adaptive_cfg,
#'   seed = 123
#' )
#'
#' # Batch start (submits jobs and returns resume info)
#' batch_out <- adaptive_rank_start(
#'   samples = samples,
#'   model = "gpt-4.1",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   backend = "openai",
#'   mode = "batch",
#'   submission = list(batch_size = 1000, write_registry = TRUE),
#'   paths = list(output_dir = "adaptive_runs"),
#'   adaptive = adaptive_cfg,
#'   seed = 123
#' )
#'
#' # Batch resume loop (poll until done)
#' next_action <- batch_out$next_action
#' state <- batch_out$state
#' submission_info <- batch_out$submission_info
#' while (identical(next_action$action, "resume")) {
#'   res <- adaptive_rank_resume(
#'     state = state,
#'     mode = "batch",
#'     submission_info = submission_info,
#'     adaptive = adaptive_cfg,
#'     seed = 123
#'   )
#'   state <- res$state
#'   submission_info <- res$submission_info
#'   next_action <- res$next_action
#' }
#' }
#'
#' @export
adaptive_rank_start <- function(
    samples,
    model,
    trait_name,
    trait_description,
    prompt_template = NULL,
    backend = NULL,
    mode = c("live", "batch"),
    submission = list(),
    adaptive = list(),
    paths = list(state_path = NULL, output_dir = NULL),
    seed = NULL
) {
  mode <- match.arg(mode)
  .adaptive_check_backend(backend, mode)
  .adaptive_check_string(model, "model")
  .adaptive_check_string(trait_name, "trait_name")
  .adaptive_check_string(trait_description, "trait_description")

  prompt_template <- prompt_template %||% set_prompt_template()
  adaptive <- .adaptive_merge_config(adaptive)
  n_items <- nrow(tibble::as_tibble(samples))
  v3_overrides <- .adaptive_v3_overrides_from_adaptive(n_items, adaptive)
  v3_config <- adaptive_v3_config(n_items, v3_overrides)
  path_info <- .adaptive_prepare_paths(paths, submission, mode)

  config <- list(
    d1 = adaptive$d1,
    budget_max = adaptive$budget_max,
    M1_target = adaptive$M1_target
  )
  state <- adaptive_state_new(samples, config = config, seed = seed)
  state$config$adaptive <- adaptive
  state$config$v3 <- v3_config
  state$config$backend <- backend
  state$config$model <- model
  state$config$trait_name <- trait_name
  state$config$trait_description <- trait_description
  state$config$prompt_template <- prompt_template
  state$config$output_dir <- path_info$output_dir
  state$config$state_path <- path_info$state_path
  state <- .adaptive_merge_submission_options(state, submission)
  state <- .adaptive_get_batch_sizes(state, adaptive)
  state <- .adaptive_state_sync_results_seen(state)
  validate_state(state)

  target_info <- .adaptive_schedule_target(state, adaptive)
  state <- target_info$state
  target <- target_info$target

  scheduled <- .adaptive_schedule_next_pairs(state, target, adaptive, seed = seed)
  state <- scheduled$state
  pairs <- scheduled$pairs

  submission_info <- list(
    mode = mode,
    backend = backend,
    model = model,
    trait_name = trait_name,
    trait_description = trait_description,
    prompt_template = prompt_template,
    pairs_submitted = pairs
  )

  if (nrow(pairs) > 0L) {
    submit_tbl <- .adaptive_pairs_to_submit_tbl(pairs)
    if (mode == "live") {
      res <- .adaptive_submit_live(
        pairs = submit_tbl,
        model = model,
        trait_name = trait_name,
        trait_description = trait_description,
        prompt_template = prompt_template,
        backend = backend,
        submission = state$config$submission
      )
      normalized <- .adaptive_normalize_submission_output(
        raw = res,
        pairs_submitted = pairs,
        backend = backend,
        model = model,
        include_raw = isTRUE(state$config$submission$include_raw)
      )
      ingest <- .adaptive_ingest_results_incremental(state, normalized$results)
      state <- ingest$state
      state <- .adaptive_append_failed_attempts(
        state,
        normalized$failed_attempts,
        phase = unique(pairs$phase),
        iter = unique(pairs$iter)
      )
      state <- .adaptive_rollback_presentations(
        state,
        pairs_submitted = pairs,
        failed_attempts = normalized$failed_attempts
      )

      observed_now <- sum(pairs$pair_uid %in% .adaptive_results_seen_names(state))
      missing <- nrow(pairs) - observed_now
      refill <- .adaptive_run_replacements_live(
        state = state,
        model = model,
        trait_name = trait_name,
        trait_description = trait_description,
        prompt_template = prompt_template,
        backend = backend,
        adaptive = adaptive,
        submission = state$config$submission,
        missing = missing,
        seed = seed,
        replacement_phase = .adaptive_phase_scalar_from_pairs(pairs),
        base_batch_size = nrow(pairs)
      )
      state <- refill$state
      submission_info$live_submissions <- c(
        list(list(pairs = pairs, results = normalized$results)),
        refill$submissions
      )
    } else {
      batch_out <- .adaptive_submit_batch(
        pairs = submit_tbl,
        model = model,
        trait_name = trait_name,
        trait_description = trait_description,
        prompt_template = prompt_template,
        backend = backend,
        submission = state$config$submission,
        output_dir = path_info$output_dir
      )
      submission_info$jobs <- batch_out$jobs
      submission_info$registry <- batch_out$registry
      submission_info$output_dir <- path_info$output_dir

      if (!is.null(path_info$state_path)) {
        adaptive_state_save(state, path_info$state_path)
      }
    }
  }

  state$config$allow_refit <- TRUE
  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  artifacts <- .adaptive_write_v3_artifacts(state, fit = state$fit, output_dir = path_info$output_dir)
  state <- artifacts$state

  if (!is.null(path_info$state_path)) {
    dir.create(dirname(path_info$state_path), recursive = TRUE, showWarnings = FALSE)
    adaptive_state_save(state, path_info$state_path)
  }

  list(
    state = state,
    state_path = path_info$state_path,
    submission_info = submission_info,
    next_action = .adaptive_next_action(state, nrow(pairs)),
    final_summary = state$config$final_summary %||% NULL
  )
}

#' Resume an adaptive ranking run
#'
#' Resume from a saved or in-memory \code{adaptive_state}, ingesting only newly
#' observed outcomes (using \code{pair_uid} to deduplicate) and scheduling the
#' next batch of pairs. This function supports both live and batch modes and
#' may need to be called multiple times until the run completes.
#'
#' @details
#' Incremental ingestion is idempotent: results are filtered to previously
#' unseen \code{pair_uid}s before updating \code{history_results}. This ensures
#' that cumulative backend returns can be resumed safely without double-counting.
#' Resume may schedule new comparisons if the budget and constraints allow.
#'
#' Model variants, stopping gates, and canonical outputs follow the same rules
#' as \code{adaptive_rank_start()}. Refits occur when at least \code{refit_B}
#' new results are available (or when the first refit is needed).
#'
#' @param state An \code{adaptive_state} object. If \code{NULL},
#'   \code{state_path} must be provided.
#' @param state_path Optional path to a saved \code{adaptive_state} RDS file.
#' @param mode Submission mode. Either \code{"live"} or \code{"batch"}.
#' @param submission_info Metadata returned by \code{adaptive_rank_start()} or
#'   prior calls to \code{adaptive_rank_resume()}.
#' @param submission A list of arguments passed through to
#'   \code{submit_llm_pairs()} (live) or \code{llm_resume_multi_batches()} /
#'   \code{llm_submit_pairs_multi_batch()} (batch).
#' @param adaptive A list of adaptive configuration overrides. See
#'   \code{adaptive_rank_start()} for supported keys.
#' @param seed Optional integer seed for deterministic scheduling.
#'
#' @return A list with:
#' \describe{
#'   \item{state}{The updated \code{adaptive_state}.}
#'   \item{state_path}{Path where the state was saved (batch mode only).}
#'   \item{submission_info}{Metadata for subsequent resumes.}
#'   \item{next_action}{List with \code{action} and \code{reason}.}
#'   \item{new_results}{The newly ingested results, if any.}
#'   \item{final_summary}{Cached summary payload, if produced.}
#' }
#'
#' @examples
#' # Minimal synthetic setup (no submission).
#' samples <- tibble::tibble(
#'   ID = c("S1", "S2", "S3"),
#'   text = c("alpha", "bravo", "charlie")
#' )
#' state <- pairwiseLLM:::adaptive_state_new(
#'   samples = samples,
#'   config = list(d1 = 2L, M1_target = 2L, budget_max = 4L)
#' )
#'
#' \dontrun{
#' # Batch resume (state loaded from disk)
#' resume_out <- adaptive_rank_resume(
#'   state_path = "adaptive_runs/adaptive_state.rds",
#'   mode = "batch",
#'   submission_info = batch_out$submission_info,
#'   adaptive = list(per_item_cap = 3),
#'   seed = 123
#' )
#'
#' # Live resume (state kept in memory)
#' resume_out <- adaptive_rank_resume(
#'   state = start_out$state,
#'   mode = "live",
#'   submission_info = start_out$submission_info,
#'   adaptive = list(per_item_cap = 3),
#'   seed = 123
#' )
#' }
#'
#' @export
adaptive_rank_resume <- function(
    state = NULL,
    state_path = NULL,
    mode = c("live", "batch"),
    submission_info = NULL,
    submission = list(),
    adaptive = list(),
    seed = NULL
) {
  mode <- match.arg(mode)
  if (is.null(state)) {
    if (is.null(state_path)) {
      rlang::abort("Provide `state` or `state_path`.")
    }
    state <- adaptive_state_load(state_path)
  }
  state <- .adaptive_state_init_logs(state)
  validate_state(state)
  if (is.null(state$repair_attempts)) {
    state$repair_attempts <- 0L
  }
  if (is.null(state$stop_reason)) {
    state$stop_reason <- NA_character_
  }

  adaptive <- .adaptive_merge_config(adaptive)
  v3_overrides <- .adaptive_v3_overrides_from_adaptive(state$N, adaptive)
  v3_config <- adaptive_v3_config(state$N, v3_overrides)
  state$config$v3 <- v3_config
  validate_state(state)
  state$config$adaptive <- utils::modifyList(state$config$adaptive %||% list(), adaptive)
  state <- .adaptive_merge_submission_options(state, submission)
  state <- .adaptive_get_batch_sizes(state, adaptive)
  state <- .adaptive_state_sync_results_seen(state)

  backend <- submission_info$backend %||% state$config$backend %||% NULL
  model <- submission_info$model %||% state$config$model %||% NULL
  if (is.null(backend) || is.null(model)) {
    rlang::abort("`submission_info` must include `backend` and `model`.")
  }

  submission_info <- submission_info %||% list()
  submission_info$trait_name <- submission_info$trait_name %||% state$config$trait_name
  submission_info$trait_description <- submission_info$trait_description %||% state$config$trait_description
  submission_info$prompt_template <- submission_info$prompt_template %||% state$config$prompt_template
  if (is.null(submission_info$trait_name) || is.null(submission_info$trait_description)) {
    rlang::abort("`submission_info` must include `trait_name` and `trait_description`.")
  }
  pairs_submitted <- submission_info$pairs_submitted %||% NULL
  if (mode == "live" &&
    is.null(submission_info$results) &&
    is.null(submission_info$failed_attempts)) {
    pairs_submitted <- .adaptive_empty_pairs_tbl()
  }
  new_results <- .adaptive_empty_results_tbl()
  failed_attempts_current <- .adaptive_empty_failed_attempts_tbl()

  if (mode == "batch") {
    .adaptive_check_backend(backend, mode)
    output_dir <- submission_info$output_dir %||% state$config$output_dir %||% NULL
    if (is.null(output_dir)) {
      output_dir <- tempfile("adaptive_rank_")
    }

    resume_args <- state$config$submission
    resume_args$jobs <- submission_info$jobs %||% NULL
    resume_args$output_dir <- output_dir
    resume_formals <- names(formals(llm_resume_multi_batches))
    resume_args <- resume_args[names(resume_args) %in% resume_formals]
    res <- do.call(llm_resume_multi_batches, resume_args)

    if (!is.null(res$combined) && nrow(res$combined) > 0L) {
      ingest <- .adaptive_ingest_results_incremental(state, res$combined)
      state <- ingest$state
      new_results <- ingest$new_results
    }
    state <- .adaptive_append_failed_attempts(
      state,
      res$failed_attempts %||% .adaptive_empty_failed_attempts_tbl()
    )
    failed_attempts_current <- res$failed_attempts %||% .adaptive_empty_failed_attempts_tbl()
  } else {
    if (!is.null(submission_info$results)) {
      normalized <- .adaptive_normalize_submission_output(
        raw = submission_info$results,
        pairs_submitted = pairs_submitted,
        backend = backend,
        model = model,
        include_raw = isTRUE(state$config$submission$include_raw)
      )
      ingest <- .adaptive_ingest_results_incremental(state, normalized$results)
      state <- ingest$state
      new_results <- ingest$new_results
      state <- .adaptive_append_failed_attempts(state, normalized$failed_attempts)
      failed_attempts_current <- normalized$failed_attempts
    }
    if (!is.null(submission_info$failed_attempts)) {
      state <- .adaptive_append_failed_attempts(state, submission_info$failed_attempts)
      failed_attempts_current <- dplyr::bind_rows(
        failed_attempts_current,
        submission_info$failed_attempts
      )
    }
  }

  state <- .adaptive_rollback_presentations(
    state,
    pairs_submitted = pairs_submitted,
    failed_attempts = failed_attempts_current
  )

  missing <- 0L
  if (!is.null(pairs_submitted) && nrow(pairs_submitted) > 0L) {
    observed_now <- sum(pairs_submitted$pair_uid %in% .adaptive_results_seen_names(state))
    missing <- nrow(pairs_submitted) - observed_now
  }
  state$config$allow_refit <- missing == 0L
  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  if (identical(state$mode, "stopped")) {
    scheduled <- list(state = state, pairs = .adaptive_empty_pairs_tbl())
  } else if (missing > 0L) {
    replacement_phase <- .adaptive_phase_scalar_from_pairs(pairs_submitted) %||% state$phase
    target <- .adaptive_replacement_target(missing, adaptive, nrow(pairs_submitted))
    scheduled <- .adaptive_schedule_replacement_pairs(
      state,
      target,
      adaptive,
      seed = seed,
      replacement_phase = replacement_phase
    )
  } else {
    target_info <- .adaptive_schedule_target(state, adaptive)
    state <- target_info$state
    target <- target_info$target
    state$config$skip_stop_checks <- TRUE
    scheduled <- .adaptive_schedule_next_pairs(state, target, adaptive, seed = seed)
  }
  state <- scheduled$state
  state$config$skip_stop_checks <- NULL
  scheduled$state <- state
  pairs <- scheduled$pairs

  submission_out <- list(
    mode = mode,
    backend = backend,
    model = model,
    trait_name = submission_info$trait_name,
    trait_description = submission_info$trait_description,
    prompt_template = submission_info$prompt_template,
    pairs_submitted = pairs
  )

    if (nrow(pairs) > 0L) {
      submit_tbl <- .adaptive_pairs_to_submit_tbl(pairs)
    if (mode == "live") {
      res <- .adaptive_submit_live(
        pairs = submit_tbl,
        model = model,
        trait_name = submission_info$trait_name,
        trait_description = submission_info$trait_description,
        prompt_template = submission_info$prompt_template %||% set_prompt_template(),
        backend = backend,
        submission = state$config$submission
      )
      normalized <- .adaptive_normalize_submission_output(
        raw = res,
        pairs_submitted = pairs,
        backend = backend,
        model = model,
        include_raw = isTRUE(state$config$submission$include_raw)
      )
      ingest <- .adaptive_ingest_results_incremental(state, normalized$results)
      state <- ingest$state
      state <- .adaptive_append_failed_attempts(
        state,
        normalized$failed_attempts,
        phase = unique(pairs$phase),
        iter = unique(pairs$iter)
      )
      state <- .adaptive_rollback_presentations(
        state,
        pairs_submitted = pairs,
        failed_attempts = normalized$failed_attempts
      )
      submission_out$live_submissions <- list(list(pairs = pairs, results = normalized$results))

      observed_now <- sum(pairs$pair_uid %in% .adaptive_results_seen_names(state))
      missing <- nrow(pairs) - observed_now
      refill <- .adaptive_run_replacements_live(
        state = state,
        model = model,
        trait_name = submission_info$trait_name,
        trait_description = submission_info$trait_description,
        prompt_template = submission_out$prompt_template %||% set_prompt_template(),
        backend = backend,
        adaptive = adaptive,
        submission = state$config$submission,
        missing = missing,
        seed = seed,
        replacement_phase = .adaptive_phase_scalar_from_pairs(pairs),
        base_batch_size = nrow(pairs)
      )
      state <- refill$state
      submission_out$live_submissions <- c(
        submission_out$live_submissions,
        refill$submissions
      )
    } else {
      output_dir <- submission_info$output_dir %||% state$config$output_dir %||% tempfile("adaptive_rank_")
      batch_out <- .adaptive_submit_batch(
        pairs = submit_tbl,
          model = model,
          trait_name = submission_info$trait_name,
          trait_description = submission_info$trait_description,
          prompt_template = submission_info$prompt_template %||% set_prompt_template(),
          backend = backend,
          submission = state$config$submission,
          output_dir = output_dir
        )
      submission_out$jobs <- batch_out$jobs
      submission_out$registry <- batch_out$registry
      submission_out$output_dir <- output_dir
    }
  }

  state$config$allow_refit <- TRUE
  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  output_dir_write <- submission_info$output_dir %||% state$config$output_dir %||% NULL
  artifacts <- .adaptive_write_v3_artifacts(state, fit = state$fit, output_dir = output_dir_write)
  state <- artifacts$state

  save_path <- state_path %||% state$config$state_path %||% NULL
  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    adaptive_state_save(state, save_path)
  }

  list(
    state = state,
    state_path = save_path,
    submission_info = submission_out,
    next_action = .adaptive_next_action(state, nrow(pairs)),
    new_results = new_results,
    final_summary = state$config$final_summary %||% NULL
  )
}

#' Run adaptive ranking in a live loop
#'
#' Convenience wrapper that repeatedly calls
#' \code{adaptive_rank_start()} and \code{adaptive_rank_resume()} in live mode
#' until the budget is exhausted, no feasible pairs remain, or
#' \code{max_iterations} is reached.
#'
#' Running this function will submit LLM comparisons and will incur API usage
#' costs for hosted backends. For larger runs, prefer batch mode so you can
#' checkpoint and control polling.
#'
#' Model variants, stopping gates, and canonical outputs match
#' \code{adaptive_rank_start()}. Each refit occurs after \code{refit_B}
#' new results, and stop checks apply the diagnostics, reliability, and
#' stability gates described there.
#'
#' @param samples A data frame or tibble with columns \code{ID} and \code{text}.
#' @param model Model identifier for the selected backend.
#' @param trait_name Short label for the trait.
#' @param trait_description Full-text trait description.
#' @param prompt_template Optional prompt template string. Defaults to
#'   \code{set_prompt_template()}.
#' @param backend Backend name (live-only): one of \code{"openai"},
#'   \code{"anthropic"}, \code{"gemini"}, \code{"together"}, or \code{"ollama"}.
#' @param submission A list of arguments passed through to
#'   \code{submit_llm_pairs()} on each live submission.
#' @param adaptive A list of adaptive configuration overrides. See
#'   \code{adaptive_rank_start()} for supported keys.
#' @param paths A list with optional \code{state_path} and \code{output_dir}.
#' @param seed Optional integer seed for deterministic scheduling.
#' @param max_iterations Optional integer override for the live loop cap.
#'
#' @return A list with:
#' \describe{
#'   \item{state}{The final \code{adaptive_state}.}
#'   \item{submission_info}{Metadata for the last submission.}
#'   \item{next_action}{List with \code{action} and \code{reason}.}
#'   \item{iterations}{Number of iterations executed.}
#' }
#'
#' @examples
#' \dontrun{
#' # Full live loop with safety cap
#' out <- adaptive_rank_run_live(
#'   samples = samples,
#'   model = "gpt-4.1",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   backend = "openai",
#'   adaptive = list(d1 = 8, M1_target = 40),
#'   seed = 123,
#'   max_iterations = 10
#' )
#' }
#'
#' @export
adaptive_rank_run_live <- function(
    samples,
    model,
    trait_name,
    trait_description,
    prompt_template = NULL,
    backend = NULL,
    submission = list(),
    adaptive = list(),
    paths = list(state_path = NULL, output_dir = NULL),
    seed = NULL,
    max_iterations = NULL
) {
  adaptive <- .adaptive_merge_config(adaptive)
  if (!is.null(max_iterations)) {
    adaptive$max_iterations <- max_iterations
  }
  n_items <- nrow(tibble::as_tibble(samples))
  v3_overrides <- .adaptive_v3_overrides_from_adaptive(n_items, adaptive)
  adaptive_v3_config(n_items, v3_overrides)
  max_iterations <- as.integer(adaptive$max_iterations)
  if (is.na(max_iterations) || max_iterations < 1L) {
    rlang::abort("`max_iterations` must be a positive integer.")
  }

  start_out <- adaptive_rank_start(
    samples = samples,
    model = model,
    trait_name = trait_name,
    trait_description = trait_description,
    prompt_template = prompt_template,
    backend = backend,
    mode = "live",
    submission = submission,
    adaptive = adaptive,
    paths = paths,
    seed = seed
  )

  state <- start_out$state
  submission_info <- start_out$submission_info
  next_action <- start_out$next_action

  iter <- 1L
  while (next_action$action == "resume" && iter < max_iterations) {
    resume_out <- adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = submission_info,
      submission = submission,
      adaptive = adaptive,
      seed = seed
    )
    state <- resume_out$state
    submission_info <- resume_out$submission_info
    next_action <- resume_out$next_action
    iter <- iter + 1L
  }

  list(
    state = state,
    submission_info = submission_info,
    next_action = next_action,
    iterations = iter
  )
}

#' Run adaptive ranking in a batch loop
#'
#' Convenience wrapper that repeatedly calls
#' \code{adaptive_rank_start()} and \code{adaptive_rank_resume()} in batch mode
#' until the budget is exhausted, no feasible pairs remain, or
#' \code{max_iterations} is reached.
#'
#' This wrapper is intended for hosted backends that support batch submission
#' (e.g., Gemini). It forces \code{n_segments = 1} so each adaptive iteration
#' submits a single batch segment (rather than splitting into multiple segments).
#'
#' Running this function will submit LLM comparisons and will incur API usage
#' costs for hosted backends. Batch mode is recommended for longer runs because
#' it supports checkpointing and controlled polling behavior.
#'
#' Some backends and helper functions accept different arguments for batch
#' submission vs polling/resume. This wrapper uses \code{formals()}-based
#' filtering to pass only arguments that are accepted by the polling function
#' (currently \code{llm_resume_multi_batches()}) during the resume loop. This
#' prevents unused-argument errors when the same \code{submission} list contains
#' submit-only keys (e.g., \code{n_segments}, \code{progress}).
#'
#' @param samples A data frame or tibble with columns \code{ID} and \code{text}.
#' @param model Model identifier for the selected backend.
#' @param trait_name Short label for the trait.
#' @param trait_description Full-text trait description.
#' @param prompt_template Optional prompt template string. Defaults to
#'   \code{set_prompt_template()}.
#' @param backend Backend name (batch-capable): one of \code{"openai"},
#'   \code{"anthropic"}, \code{"gemini"}, \code{"together"}, or \code{"ollama"}.
#'   (Note: actual batch support depends on the backend implementation.)
#' @param submission A list of arguments passed through to
#'   \code{llm_submit_pairs_multi_batch()} (and the corresponding resume/polling
#'   functions) on each batch submission. This wrapper will force
#'   \code{submission$n_segments <- 1}. During polling, the wrapper will filter
#'   the list to the formals of \code{llm_resume_multi_batches()} before calling
#'   \code{adaptive_rank_resume()}.
#' @param adaptive A list of adaptive configuration overrides. See
#'   \code{adaptive_rank_start()} for supported keys.
#' @param paths A list with optional \code{state_path} and \code{output_dir}.
#'   Batch mode strongly benefits from setting these so the run can be resumed.
#' @param seed Optional integer seed for deterministic scheduling.
#' @param max_iterations Optional integer override for the batch loop cap.
#'
#' @return A list with:
#' \describe{
#'   \item{state}{The final \code{adaptive_state}.}
#'   \item{submission_info}{Metadata for the last submission.}
#'   \item{next_action}{List with \code{action} and \code{reason}.}
#'   \item{iterations}{Number of iterations executed.}
#' }
#'
#' @examples
#' \dontrun{
#' # Gemini batch loop, single segment per iteration
#' out <- adaptive_rank_run_batch(
#'   samples = samples,
#'   model = "gemini-3-flash-preview",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   backend = "gemini",
#'   submission = list(
#'     # submit-time and poll-time controls may be mixed here;
#'     # submit-only keys will be filtered out automatically during polling
#'     interval_seconds = 60,
#'     per_job_delay = 2,
#'     progress = TRUE,
#'     verbose = TRUE
#'   ),
#'   paths = list(
#'     state_path = "./dev-output/adaptive_gemini/state.rds",
#'     output_dir = "./dev-output/adaptive_gemini"
#'   ),
#'   seed = 123,
#'   max_iterations = 10
#' )
#' }
#'
#' @export
adaptive_rank_run_batch <- function(
    samples,
    model,
    trait_name,
    trait_description,
    prompt_template = NULL,
    backend = NULL,
    submission = list(),
    adaptive = list(),
    paths = list(state_path = NULL, output_dir = NULL),
    seed = NULL,
    max_iterations = NULL
) {
  adaptive <- .adaptive_merge_config(adaptive)
  if (!is.null(max_iterations)) {
    adaptive$max_iterations <- max_iterations
  }

  n_items <- nrow(tibble::as_tibble(samples))
  v3_overrides <- .adaptive_v3_overrides_from_adaptive(n_items, adaptive)
  adaptive_v3_config(n_items, v3_overrides)

  max_iterations <- as.integer(adaptive$max_iterations)
  if (is.na(max_iterations) || max_iterations < 1L) {
    rlang::abort("`max_iterations` must be a positive integer.")
  }

  if (is.null(submission) || !is.list(submission)) submission <- list()

  # Submit args: force single-segment batching
  submission_submit <- submission
  submission_submit$n_segments <- 1L

  # Poll/resume args: filter to what llm_resume_multi_batches() actually accepts.
  # Note: adaptive_rank_resume() ultimately calls llm_resume_multi_batches() via do.call().
  poll_formals <- names(formals(llm_resume_multi_batches))
  submission_poll <- submission[names(submission) %in% poll_formals]

  start_out <- adaptive_rank_start(
    samples = samples,
    model = model,
    trait_name = trait_name,
    trait_description = trait_description,
    prompt_template = prompt_template,
    backend = backend,
    mode = "batch",
    submission = submission_submit,
    adaptive = adaptive,
    paths = paths,
    seed = seed
  )

  state <- start_out$state
  submission_info <- start_out$submission_info
  next_action <- start_out$next_action

  iter <- 1L
  while (next_action$action == "resume" && iter < max_iterations) {
    resume_out <- adaptive_rank_resume(
      state = state,
      mode = "batch",
      submission_info = submission_info,
      submission = submission_poll,
      adaptive = adaptive,
      seed = seed
    )
    state <- resume_out$state
    submission_info <- resume_out$submission_info
    next_action <- resume_out$next_action
    iter <- iter + 1L
  }

  list(
    state = state,
    submission_info = submission_info,
    next_action = next_action,
    iterations = iter
  )
}
