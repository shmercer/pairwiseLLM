#' @include adaptive_v3_contracts.R
#' @include bayes_btl_mcmc_v3.R
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
    n_draws_fast = 400L,
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
  output_dir <- paths$output_dir %||% submission$output_dir %||% tempfile("adaptive_rank_")
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    rlang::abort("`paths$output_dir` must be a single character path.")
  }
  if (mode == "batch") {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  state_path <- paths$state_path
  if (is.null(state_path) && mode == "batch") {
    state_path <- file.path(output_dir, "adaptive_state.rds")
  }
  list(state_path = state_path, output_dir = output_dir)
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

.adaptive_get_refit_fit <- function(state, adaptive, batch_size, seed) {
  last_refit_at <- state$config$last_refit_at %||% 0L
  CW <- state$config$CW %||% floor(state$N / 2)
  do_refit <- is.null(state$fast_fit) ||
    should_refit(
      comparisons_observed = state$comparisons_observed,
      last_refit_at = last_refit_at,
      batch_size = batch_size,
      CW = CW
    )

  if (do_refit) {
    fit <- fit_bayes_btl_fast(
      results = state$history_results,
      ids = state$ids,
      n_draws = adaptive$n_draws_fast,
      seed = seed
    )
    state$fast_fit <- fit
    state$config$last_refit_at <- as.integer(state$comparisons_observed)
    state$last_refit_at <- as.integer(state$comparisons_observed)
    state$new_since_refit <- 0L
  }

  if (is.null(state$fast_fit)) {
    rlang::abort("Fast inference failed to initialize.")
  }

  list(state = state, fit = state$fast_fit)
}

.adaptive_run_stopping_checks <- function(state, adaptive, seed) {
  validate_state(state)
  if (state$comparisons_observed < 1L) {
    return(list(state = state, stop_confirmed = isTRUE(state$config$stop_confirmed)))
  }

  CW <- state$config$CW %||% floor(state$N / 2)
  CW <- as.integer(CW)
  if (is.na(CW) || CW < 1L) {
    rlang::abort("`CW` must be a positive integer.")
  }
  if ((state$comparisons_observed - state$last_check_at) < CW) {
    return(list(state = state, stop_confirmed = isTRUE(state$config$stop_confirmed)))
  }

  fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size = 1L, seed = seed)
  state <- fit_out$state
  fit <- fit_out$fit

  ranking <- compute_ranking_from_theta_mean(fit$theta_mean, state)
  near_stop <- near_stop_from_state(state)
  phase_for_window <- if (state$phase %in% c("phase2", "phase3")) state$phase else "phase2"
  W <- select_window_size(state$N, phase = phase_for_window, near_stop = near_stop)
  candidates <- build_candidate_pairs(
    ranking_ids = ranking,
    W = W,
    state = state,
    exploration_frac = adaptive$exploration_frac,
    seed = seed
  )
  if (nrow(candidates) == 0L) {
    return(list(state = state, stop_confirmed = isTRUE(state$config$stop_confirmed)))
  }

  utilities <- compute_pair_utility(fit$theta_draws, candidates)
  utilities <- apply_degree_penalty(utilities, state)
  if (!is.finite(state$U0)) {
    state$U0 <- as.double(compute_Umax(utilities))
  }

  stop_out <- stopping_check(
    state = state,
    fast_fit = fit,
    ranking_ids = ranking,
    candidates = candidates,
    utilities_tbl = utilities
  )
  state <- stop_out$state
  if (isTRUE(stop_out$condition_A) && isTRUE(stop_out$condition_B)) {
    if (is.null(state$config$stop_candidate_at)) {
      state$config$stop_candidate_at <- as.integer(state$last_check_at)
      state$checks_passed_in_row <- as.integer(max(state$checks_passed_in_row, 1L))
    } else if (state$last_check_at > state$config$stop_candidate_at) {
      state$checks_passed_in_row <- as.integer(max(state$checks_passed_in_row, 2L))
    }
  } else {
    state$config$stop_candidate_at <- NULL
  }

  two_checks_passed <- isTRUE(state$checks_passed_in_row >= 2L)
  if (!two_checks_passed && !is.null(state$config$stop_candidate_at)) {
    two_checks_passed <- isTRUE(state$last_check_at > state$config$stop_candidate_at)
  }

  if (!isTRUE(state$config$stop_confirmed) && two_checks_passed) {
    last_attempt_at <- state$config$mcmc_attempted_at %||% -1L
    if (state$last_check_at > last_attempt_at) {
      bt_data <- .btl_mcmc_v3_prepare_bt_data(state$history_results, state$ids)
      mcmc_fit <- tryCatch(
        fit_bayes_btl_mcmc_v3(bt_data = bt_data, config = state$config$v3, seed = seed),
        error = function(e) e
      )
      state$config$mcmc_attempted_at <- as.integer(state$last_check_at)

      if (inherits(mcmc_fit, "error")) {
        state$config$mcmc_error <- conditionMessage(mcmc_fit)
        rlang::warn(paste0("MCMC confirmation failed: ", conditionMessage(mcmc_fit)))
      } else {
        theta_draws <- .btl_mcmc_v3_theta_draws(mcmc_fit$draws, item_id = state$ids)
        ranking_ids <- names(sort(colMeans(theta_draws), decreasing = TRUE))
        q_vals <- compute_adjacent_certainty(theta_draws, ranking_ids)
        q_median <- stats::median(q_vals)
        q_p10 <- as.double(stats::quantile(q_vals, probs = 0.1, names = FALSE))

        if ((q_median > 0.95) && (q_p10 > 0.80)) {
          state$config$stop_confirmed <- TRUE
          state$config$final_summary <- finalize_adaptive_ranking(state, mcmc_fit)
          state$config$mcmc_error <- NULL
        }
      }
    }
  }

  if (isTRUE(state$config$stop_confirmed)) {
    state$mode <- "stopped"
  }

  list(state = state, stop_confirmed = isTRUE(state$config$stop_confirmed))
}

.adaptive_apply_diagnostics_gate <- function(state, fit, config, near_stop) {
  diagnostics <- fit$diagnostics %||% NULL
  if (is.null(diagnostics)) {
    return(list(state = state, diagnostics_pass = TRUE))
  }

  diagnostics_pass <- diagnostics_gate_v3(fit, config, near_stop = near_stop)
  if (isTRUE(diagnostics_pass)) {
    if (!is.null(state$repair_attempts) && state$repair_attempts > 0L) {
      state$repair_attempts <- 0L
    }
    if (identical(state$mode, "repair")) {
      state$mode <- "adaptive"
    }
    return(list(state = state, diagnostics_pass = TRUE))
  }

  state$mode <- "repair"
  state$repair_attempts <- as.integer((state$repair_attempts %||% 0L) + 1L)
  max_cycles <- as.integer(config$repair_max_cycles)
  if (state$repair_attempts > max_cycles) {
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
  seen_before <- state$unordered_count

  out <- phase1_generate_pairs(
    state = state,
    n_pairs = target_pairs,
    mix_struct = adaptive$mix_struct,
    within_adj_split = adaptive$within_adj_split,
    bins = adaptive$bins,
    seed = seed
  )

  state <- out$state
  pairs <- out$pairs
  if (nrow(pairs) > 0L && !is.null(seen_before) && length(seen_before) > 0L) {
    dup_counts <- seen_before[pairs$unordered_key]
    dup_mask <- !is.na(dup_counts) & dup_counts > 0L
    if (any(dup_mask)) {
      rlang::warn("Repair batch generated duplicate comparisons due to constraints.")
    }
  }
  if (nrow(pairs) > 0L) {
    pairs$phase <- phase
    pairs$iter <- iter
    idx_start <- nrow(state$history_pairs) - nrow(pairs) + 1L
    idx <- seq.int(idx_start, nrow(state$history_pairs))
    state$history_pairs$phase[idx] <- phase
    state$history_pairs$iter[idx] <- iter
    state$iter <- iter
  }
  state$mode <- "repair"

  list(state = state, pairs = pairs)
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

.adaptive_schedule_warm_start <- function(state, config) {
  warm_pairs <- warm_start_v3(state$ids, config)
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

    state <- record_exposure(state, order$A_id, order$B_id)
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
  if (isTRUE(state$config$stop_confirmed)) {
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

  if (identical(state$mode, "warm_start") && identical(state$phase, "phase1")) {
    return(.adaptive_schedule_warm_start(state, state$config$v3))
  }

  near_stop <- isTRUE(near_stop) || near_stop_from_state(state)
  if (near_stop && state$phase == "phase2") {
    state$phase <- "phase3"
  }
  phase <- state$phase
  batch_size <- target_pairs
  iter <- as.integer(state$iter + 1L)

  fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size, seed)
  state <- fit_out$state
  fit <- fit_out$fit

  gate_out <- .adaptive_apply_diagnostics_gate(state, fit, state$config$v3, near_stop = near_stop)
  state <- gate_out$state
  if (identical(state$mode, "stopped")) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  if (!isTRUE(gate_out$diagnostics_pass)) {
    return(.adaptive_schedule_repair_pairs(state, target_pairs, adaptive, seed = seed))
  }

  ranking <- compute_ranking_from_theta_mean(fit$theta_mean, state)
  W <- select_window_size(state$N, phase = phase, near_stop = near_stop)
  candidates <- build_candidate_pairs(
    ranking_ids = ranking,
    W = W,
    state = state,
    exploration_frac = adaptive$exploration_frac,
    seed = seed
  )

  if (nrow(candidates) == 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }

  utilities <- compute_pair_utility(fit$theta_draws, candidates)
  utilities <- apply_degree_penalty(utilities, state)
  if (!is.finite(state$U0)) {
    state$U0 <- as.double(compute_Umax(utilities))
  }
  out <- select_pairs_from_candidates(
    state = state,
    utilities_tbl = utilities,
    batch_size = batch_size,
    per_item_cap = adaptive$per_item_cap,
    phase = phase,
    iter = iter,
    seed = seed
  )
  out$state$iter <- iter
  out
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
  if (isTRUE(state$config$stop_confirmed)) {
    return(list(action = "done", reason = "stop_confirmed"))
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
  .adaptive_schedule_next_pairs(state, target_pairs, adaptive, seed = seed)
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
#' The adaptive engine uses a confirmation window (CW) based on observed
#' comparisons, along with a two-check confirmation rule, to decide when to
#' stop refinement. Fast inference provides selection-grade posterior draws for
#' adaptive selection and stopping checks. Final uncertainty summaries are
#' produced only after the full MCMC audit.
#'
#' All LLM outputs are normalized into a single canonical results schema,
#' independent of backend, and all downstream logic operates exclusively on
#' this canonical form.
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
#' @param adaptive A list of adaptive configuration overrides. Supported keys
#'   include: \code{d1} (default 8), \code{bins} (8), \code{mix_struct} (0.70),
#'   \code{within_adj_split} (0.50), \code{exploration_frac} (0.05),
#'   \code{per_item_cap} (NULL), \code{n_draws_fast} (400),
#'   \code{batch_overrides} (list), \code{max_refill_rounds} (2),
#'   \code{max_replacements} (NULL), \code{max_iterations} (50),
#'   \code{budget_max} (NULL; defaults to 0.40 * choose(N,2)), and
#'   \code{M1_target} (NULL; defaults to floor(N * d1 / 2)). The list is
#'   extensible in future versions.
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
  config_v3 <- adaptive_v3_config(n_items, v3_overrides)
  path_info <- .adaptive_prepare_paths(paths, submission, mode)

  config <- list(
    d1 = adaptive$d1,
    budget_max = adaptive$budget_max,
    M1_target = adaptive$M1_target
  )
  state <- adaptive_state_new(samples, config = config, seed = seed)
  state$config$adaptive <- adaptive
  state$config$v3 <- config_v3
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
  validate_state_v3(state, config_v3)

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

  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  if (mode == "batch" && !is.null(path_info$state_path)) {
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
  validate_state(state)
  if (is.null(state$repair_attempts)) {
    state$repair_attempts <- 0L
  }
  if (is.null(state$stop_reason)) {
    state$stop_reason <- NA_character_
  }

  adaptive <- .adaptive_merge_config(adaptive)
  v3_overrides <- .adaptive_v3_overrides_from_adaptive(state$N, adaptive)
  config_v3 <- adaptive_v3_config(state$N, v3_overrides)
  state$config$v3 <- config_v3
  validate_state_v3(state, config_v3)
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
  new_results <- .adaptive_empty_results_tbl()

  if (mode == "batch") {
    .adaptive_check_backend(backend, mode)
    output_dir <- submission_info$output_dir %||% state$config$output_dir %||% NULL
    if (is.null(output_dir)) {
      output_dir <- tempfile("adaptive_rank_")
    }

    resume_args <- state$config$submission
    resume_args$jobs <- submission_info$jobs %||% NULL
    resume_args$output_dir <- output_dir
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
    }
    if (!is.null(submission_info$failed_attempts)) {
      state <- .adaptive_append_failed_attempts(state, submission_info$failed_attempts)
    }
  }

  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  missing <- 0L
  if (!is.null(pairs_submitted) && nrow(pairs_submitted) > 0L) {
    observed_now <- sum(pairs_submitted$pair_uid %in% .adaptive_results_seen_names(state))
    missing <- nrow(pairs_submitted) - observed_now
  }

  if (isTRUE(state$config$stop_confirmed)) {
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
    scheduled <- .adaptive_schedule_next_pairs(state, target, adaptive, seed = seed)
  }
  state <- scheduled$state
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

  stop_out <- .adaptive_run_stopping_checks(state, adaptive, seed)
  state <- stop_out$state

  if (!is.null(state_path) && mode == "batch") {
    adaptive_state_save(state, state_path)
  }

  list(
    state = state,
    state_path = state_path,
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
