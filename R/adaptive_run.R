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
    failed_attempts$phase <- if (is.null(phase)) NA_character_ else as.character(phase)
  }
  if (!"iter" %in% names(failed_attempts)) {
    failed_attempts$iter <- if (is.null(iter)) NA_integer_ else as.integer(iter)
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
  }

  if (is.null(state$fast_fit)) {
    rlang::abort("Fast inference failed to initialize.")
  }

  list(state = state, fit = state$fast_fit)
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

  budget_remaining <- as.integer(state$budget_max - state$comparisons_scheduled)
  if (budget_remaining <= 0L) {
    return(list(state = state, pairs = .adaptive_empty_pairs_tbl()))
  }
  target_pairs <- min(target_pairs, budget_remaining)

  if (state$phase == "phase1" && state$comparisons_scheduled < state$M1_target) {
    remaining <- as.integer(state$M1_target - state$comparisons_scheduled)
    n_pairs <- min(target_pairs, remaining)
    return(
      phase1_generate_pairs(
        state = state,
        n_pairs = n_pairs,
        mix_struct = adaptive$mix_struct,
        within_adj_split = adaptive$within_adj_split,
        bins = adaptive$bins,
        seed = seed
      )
    )
  }

  if (state$phase == "phase1") {
    state$phase <- "phase2"
  }

  phase <- state$phase
  batch_size <- target_pairs
  iter <- as.integer(state$iter + 1L)

  fit_out <- .adaptive_get_refit_fit(state, adaptive, batch_size, seed)
  state <- fit_out$state
  fit <- fit_out$fit

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
  if (replacement_phase == "phase1") {
    out <- phase1_generate_pairs(
      state = state,
      n_pairs = target_pairs,
      mix_struct = adaptive$mix_struct,
      within_adj_split = adaptive$within_adj_split,
      bins = adaptive$bins,
      seed = seed
    )
    return(out)
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

#' Start an adaptive ranking run
#'
#' Initialize adaptive ranking, schedule Phase 1 pairs, and submit them in live
#' or batch mode. Live mode submits immediately and ingests observed outcomes.
#' Batch mode submits jobs, saves state, and returns resume metadata.
#'
#' This is the entry point for adaptive ranking. It creates an
#' \code{adaptive_state} using the canonical schemas and schedules pairs using
#' Phase 1 logic until \code{M1_target} is met, then switches to adaptive
#' batches (Phase 2/3) on subsequent resumes. All results are normalized via
#' the shared normalization helper to ensure schema consistency.
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
#'   \code{batch_size}, and \code{n_segments}.
#' @param adaptive A list of adaptive configuration overrides. Supported keys
#'   include: \code{d1}, \code{bins}, \code{mix_struct},
#'   \code{within_adj_split}, \code{exploration_frac}, \code{per_item_cap},
#'   \code{n_draws_fast}, \code{batch_overrides}, \code{max_refill_rounds},
#'   \code{max_replacements}, \code{max_iterations}, \code{budget_max}, and
#'   \code{M1_target}.
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
#' \dontrun{
#' samples <- tibble::tibble(
#'   ID = c("S1", "S2", "S3", "S4"),
#'   text = c("alpha", "bravo", "charlie", "delta")
#' )
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' # Live start (submits immediately and ingests observed results)
#' start_out <- adaptive_rank_start(
#'   samples = samples,
#'   model = "gpt-4.1",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   backend = "openai",
#'   mode = "live",
#'   adaptive = list(d1 = 8, M1_target = 40),
#'   seed = 123
#' )
#'
#' # Batch start (submits jobs and returns resume info)
#' batch_out <- adaptive_rank_start(
#'   samples = samples,
#'   model = "gpt-4.1",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   backend = "openai",
#'   mode = "batch",
#'   submission = list(batch_size = 1000, write_registry = TRUE),
#'   paths = list(output_dir = "adaptive_runs"),
#'   adaptive = list(d1 = 8, M1_target = 40),
#'   seed = 123
#' )
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
  path_info <- .adaptive_prepare_paths(paths, submission, mode)

  config <- list(
    d1 = adaptive$d1,
    budget_max = adaptive$budget_max,
    M1_target = adaptive$M1_target
  )
  state <- adaptive_state_new(samples, config = config, seed = seed)
  state$config$adaptive <- adaptive
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

  if (mode == "batch" && !is.null(path_info$state_path)) {
    adaptive_state_save(state, path_info$state_path)
  }

  list(
    state = state,
    state_path = path_info$state_path,
    submission_info = submission_info,
    next_action = .adaptive_next_action(state, nrow(pairs))
  )
}

#' Resume an adaptive ranking run
#'
#' Resume from a saved or in-memory \code{adaptive_state}, ingesting only newly
#' observed outcomes (using \code{pair_uid} to deduplicate) and scheduling the
#' next batch of pairs. This function supports both live and batch modes.
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
#' \dontrun{
#' resume_out <- adaptive_rank_resume(
#'   state_path = "adaptive_runs/adaptive_state.rds",
#'   mode = "batch",
#'   submission_info = batch_out$submission_info,
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

  adaptive <- .adaptive_merge_config(adaptive)
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

  missing <- 0L
  if (!is.null(pairs_submitted) && nrow(pairs_submitted) > 0L) {
    observed_now <- sum(pairs_submitted$pair_uid %in% .adaptive_results_seen_names(state))
    missing <- nrow(pairs_submitted) - observed_now
  }

  if (missing > 0L) {
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

  if (!is.null(state_path) && mode == "batch") {
    adaptive_state_save(state, state_path)
  }

  list(
    state = state,
    state_path = state_path,
    submission_info = submission_out,
    next_action = .adaptive_next_action(state, nrow(pairs)),
    new_results = new_results
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
