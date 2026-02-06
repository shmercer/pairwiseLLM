# -------------------------------------------------------------------------
# Adaptive entrypoints.
# -------------------------------------------------------------------------

.adaptive_build_warm_start_pairs <- function(item_ids, seed) {
  item_ids <- as.character(item_ids)
  if (length(item_ids) < 2L) {
    return(tibble::tibble(i_id = character(), j_id = character()))
  }
  seed <- .adaptive_validate_seed(seed)
  shuffled <- withr::with_seed(seed, sample(item_ids, size = length(item_ids)))
  i_ids <- shuffled[-length(shuffled)]
  j_ids <- shuffled[-1L]
  tibble::tibble(i_id = i_ids, j_id = j_ids)
}

#' Adaptive ranking
#'
#' @description
#' Initialize an adaptive ranking session and canonical state object.
#'
#' @details
#' This function creates the stepwise controller state and seeds all canonical
#' logs used in the adaptive pairing workflow. Warm start pair construction
#' follows the shuffled chain design, which guarantees a connected comparison
#' graph after \eqn{N - 1} committed comparisons.
#'
#' Pair selection in this framework is TrueSkill-driven and uses base utility
#' \deqn{U_0 = p_{ij}(1 - p_{ij})} where \eqn{p_{ij}} is the current TrueSkill
#' win probability for pair \eqn{\{i, j\}}. Bayesian
#' BTL posterior draws are not used for pair selection; they are used for
#' posterior inference, diagnostics, and stopping at refit rounds.
#'
#' The returned state contains canonical logs:
#' \itemize{
#'   \item \code{step_log}: one row per attempted step,
#'   \item \code{round_log}: one row per posterior refit,
#'   \item \code{item_log}: per-item posterior summaries by refit.
#' }
#' If \code{session_dir} is supplied, the initialized state is persisted
#' immediately using [save_adaptive_session()].
#'
#' @param items A vector or data frame of items. Data frames must include an
#'   `item_id` column (or `id`/`ID`). Item IDs may be character; internal logs
#'   use integer indices derived from these IDs.
#' @param seed Integer seed used for deterministic warm-start shuffling and
#'   selection randomness.
#' @param session_dir Optional directory for saving session artifacts.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#' @param ... Internal/testing only. Supply `now_fn` to override the clock used
#'   for timestamps.
#'
#' @return An adaptive state object containing `step_log`, `round_log`, and
#'   `item_log`. The object includes class \code{"adaptive_state"}, item ID
#'   mappings, TrueSkill state, warm-start queue, refit metadata, and runtime
#'   configuration.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 11)
#' summarize_adaptive(state)
#'
#' @seealso [adaptive_rank_run_live()], [adaptive_rank_resume()],
#'   [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_start <- function(items,
                                seed = 1L,
                                session_dir = NULL,
                                persist_item_log = FALSE,
                                ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    if (is.null(dot_names) || any(dot_names == "")) {
      rlang::abort("Only named `now_fn` is supported in `...` for now.")
    }
    bad <- setdiff(dot_names, "now_fn")
    if (length(bad) > 0L) {
      rlang::abort("Only `now_fn` is supported in `...` for now.")
    }
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.logical(persist_item_log) ||
    length(persist_item_log) != 1L ||
    is.na(persist_item_log)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }
  seed <- .adaptive_validate_seed(seed)
  now_fn <- dots$now_fn %||% function() Sys.time()
  state <- new_adaptive_state(items, now_fn = now_fn)
  state$meta$seed <- seed
  state$warm_start_pairs <- .adaptive_build_warm_start_pairs(state$item_ids, seed)
  state$warm_start_idx <- 1L
  state$warm_start_done <- nrow(state$warm_start_pairs) == 0L
  state$config$session_dir <- session_dir %||% NULL
  state$config$persist_item_log <- isTRUE(persist_item_log)
  if (!is.null(session_dir)) {
    save_adaptive_session(state, session_dir = session_dir, overwrite = FALSE)
  }
  state
}

#' Adaptive ranking live runner
#'
#' @description
#' Execute stepwise adaptive ranking with a user-supplied judge.
#'
#' @details
#' Each iteration attempts at most one pair evaluation ("one-pair step"), then
#' applies transactional updates if and only if the judge response is valid.
#' Invalid responses produce a logged step with
#' \code{pair_id = NA} and must not update committed-comparison state.
#'
#' Pair selection is TrueSkill-based and does not use BTL posterior draws.
#' Utility is based on
#' \deqn{U_0 = p_{ij}(1 - p_{ij})} with exploration/exploitation routing and
#' fallback handling recorded in \code{step_log}.
#'
#' Bayesian BTL refits are triggered on step-based cadence and evaluated with
#' diagnostics gates (including ESS thresholds), reliability, and lagged
#' stability criteria. Refit-level outcomes are
#' appended to \code{round_log}; per-item posterior summaries are appended to
#' \code{item_log}.
#'
#' @param state An adaptive state object created by [adaptive_rank_start()].
#' @param judge A function called as `judge(A, B, state, ...)` that returns a
#'   list with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
#'   `invalid_reason`.
#' @param n_steps Number of steps to execute.
#' @param fit_fn Optional BTL fit function for deterministic testing; defaults
#'   to `default_btl_fit_fn()` when a refit is due.
#' @param btl_config Optional list overriding BTL refit/stop defaults.
#' @param session_dir Optional directory for saving session artifacts.
#' @param persist_item_log Logical; when TRUE, write per-refit item logs to disk.
#' @param progress Progress output: "all", "refits", "steps", or "none".
#' @param progress_redraw_every Redraw progress bar every N steps.
#' @param progress_show_events Logical; when TRUE, print notable step events.
#' @param progress_errors Logical; when TRUE, include invalid-step events.
#' @param ... Additional arguments passed through to `judge()`.
#'
#' @return An updated \code{adaptive_state}. The returned state includes
#'   appended \code{step_log} rows for attempted steps and, when refits occur,
#'   appended \code{round_log} and \code{item_log} entries.
#'
#' @examples
#' # ------------------------------------------------------------------
#' # Offline end-to-end workflow (fast, deterministic, CRAN-safe)
#' # ------------------------------------------------------------------
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' items <- dplyr::rename(
#'   example_writing_samples[1:8, c("ID", "text", "quality_score")],
#'   item_id = ID
#' )
#'
#' # Use the package defaults for trait and prompt template.
#' trait <- trait_description("overall_quality")
#' prompt_template <- set_prompt_template()
#'
#' # Deterministic local judge based on fixture quality scores.
#' sim_judge <- function(A, B, state, ...) {
#'   y <- as.integer(A$quality_score[[1]] >= B$quality_score[[1]])
#'   list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
#' }
#'
#' session_dir <- tempfile("pwllm-adaptive-session-")
#'
#' state <- adaptive_rank_start(
#'   items = items,
#'   seed = 42,
#'   session_dir = session_dir,
#'   persist_item_log = TRUE
#' )
#'
#' state <- adaptive_rank_run_live(
#'   state = state,
#'   judge = sim_judge,
#'   n_steps = 6,
#'   btl_config = list(
#'     # Keep examples lightweight while showing custom stop config inputs.
#'     refit_pairs_target = 50L,
#'     ess_bulk_min = 400,
#'     eap_reliability_min = 0.90
#'   ),
#'   progress = "steps",
#'   progress_redraw_every = 1L,
#'   progress_show_events = TRUE,
#'   progress_errors = TRUE
#' )
#'
#' # Print and inspect run outputs.
#' print(state)
#' run_summary <- summarize_adaptive(state)
#' step_view <- adaptive_step_log(state)
#' logs <- adaptive_get_logs(state)
#'
#' run_summary
#' head(step_view)
#' names(logs)
#'
#' # Resume from disk and continue.
#' resumed <- adaptive_rank_resume(session_dir)
#' resumed <- adaptive_rank_run_live(
#'   state = resumed,
#'   judge = sim_judge,
#'   n_steps = 4,
#'   progress = "none"
#' )
#' summarize_adaptive(resumed)
#'
#' # ------------------------------------------------------------------
#' # Live OpenAI workflow via backend-agnostic llm_compare_pair()
#' # ------------------------------------------------------------------
#' \dontrun{
#' # Requires network + OPENAI_API_KEY. This incurs API cost.
#' # check_llm_api_keys() is a quick preflight.
#' check_llm_api_keys()
#'
#' data("example_writing_samples", package = "pairwiseLLM")
#' live_items <- dplyr::rename(
#'   example_writing_samples[1:12, c("ID", "text")],
#'   item_id = ID
#' )
#'
#' # Default trait/template setup used by the backend-agnostic runner.
#' trait <- trait_description("overall_quality")
#' prompt_template <- set_prompt_template()
#'
#' live_session_dir <- file.path(tempdir(), "pwllm-adaptive-openai")
#'
#' judge_openai <- function(A, B, state, ...) {
#'   res <- llm_compare_pair(
#'     ID1 = A$item_id[[1]],
#'     text1 = A$text[[1]],
#'     ID2 = B$item_id[[1]],
#'     text2 = B$text[[1]],
#'     model = "gpt-5.1",
#'     trait_name = trait$name,
#'     trait_description = trait$description,
#'     prompt_template = prompt_template,
#'     backend = "openai",
#'     endpoint = "responses",
#'     reasoning = "low",
#'     service_tier = "flex",
#'     include_thoughts = FALSE,
#'     temperature = NULL,
#'     top_p = NULL,
#'     logprobs = NULL
#'   )
#'
#'   better_id <- res$better_id[[1]]
#'   ok_ids <- c(A$item_id[[1]], B$item_id[[1]])
#'   if (is.na(better_id) || !(better_id %in% ok_ids)) {
#'     return(list(
#'       is_valid = FALSE,
#'       Y = NA_integer_,
#'       invalid_reason = "model_response_invalid"
#'     ))
#'   }
#'
#'   list(
#'     is_valid = TRUE,
#'     Y = as.integer(identical(better_id, A$item_id[[1]])),
#'     invalid_reason = NA_character_
#'   )
#' }
#'
#' state_live <- adaptive_rank_start(
#'   items = live_items,
#'   seed = 2026,
#'   session_dir = live_session_dir,
#'   persist_item_log = TRUE
#' )
#'
#' state_live <- adaptive_rank_run_live(
#'   state = state_live,
#'   judge = judge_openai,
#'   n_steps = 120L,
#'   btl_config = list(
#'     refit_pairs_target = 20L,
#'     ess_bulk_min = 500,
#'     ess_bulk_min_near_stop = 1200,
#'     max_rhat = 1.01,
#'     divergences_max = 0L,
#'     eap_reliability_min = 0.92,
#'     stability_lag = 2L,
#'     theta_corr_min = 0.97,
#'     theta_sd_rel_change_max = 0.08,
#'     rank_spearman_min = 0.97
#'   ),
#'   progress = "all",
#'   progress_redraw_every = 1L,
#'   progress_show_events = TRUE,
#'   progress_errors = TRUE
#' )
#'
#' # Reporting outputs for end users.
#' print(state_live)
#' run_summary <- summarize_adaptive(state_live)
#' refit_summary <- summarize_refits(state_live)
#' item_summary <- summarize_items(state_live)
#' logs <- adaptive_get_logs(state_live)
#'
#' # Store outputs for audit/reproducibility.
#' saveRDS(
#'   list(
#'     run_summary = run_summary,
#'     refit_summary = refit_summary,
#'     item_summary = item_summary,
#'     logs = logs
#'   ),
#'   file.path(live_session_dir, "adaptive_outputs.rds")
#' )
#'
#' # Resume from stored state and continue sampling.
#' state_live <- adaptive_rank_resume(live_session_dir)
#' state_live <- adaptive_rank_run_live(
#'   state = state_live,
#'   judge = judge_openai,
#'   n_steps = 40L,
#'   progress = "refits"
#' )
#' print(summarize_adaptive(state_live))
#' }
#'
#' @seealso [adaptive_rank_start()], [adaptive_rank_resume()],
#'   [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_run_live <- function(state,
                                   judge,
                                   n_steps = 1L,
                                   fit_fn = NULL,
                                   btl_config = NULL,
                                   session_dir = NULL,
                                   persist_item_log = NULL,
                                   progress = c("all", "refits", "steps", "none"),
                                   progress_redraw_every = 10L,
                                   progress_show_events = TRUE,
                                   progress_errors = TRUE,
                                   ...) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (!is.function(judge)) {
    rlang::abort("`judge` must be a function.")
  }
  n_steps <- as.integer(n_steps)
  if (length(n_steps) != 1L || is.na(n_steps) || n_steps < 1L) {
    rlang::abort("`n_steps` must be a positive integer.")
  }
  if (!is.null(session_dir) &&
    (!is.character(session_dir) || length(session_dir) != 1L)) {
    rlang::abort("`session_dir` must be a single string.")
  }
  if (!is.null(persist_item_log) &&
    (!is.logical(persist_item_log) || length(persist_item_log) != 1L)) {
    rlang::abort("`persist_item_log` must be TRUE or FALSE.")
  }

  if (!is.null(session_dir)) {
    state$config$session_dir <- session_dir
  }
  if (!is.null(persist_item_log)) {
    state$config$persist_item_log <- isTRUE(persist_item_log)
  }

  cfg <- .adaptive_progress_config(
    progress = progress,
    progress_redraw_every = progress_redraw_every,
    progress_show_events = progress_show_events,
    progress_errors = progress_errors
  )
  btl_cfg <- .adaptive_btl_resolve_config(state, btl_config)
  btl_cfg$refit_pairs_target <- .adaptive_refit_pairs_target(state, btl_cfg)
  cfg$refit_pairs_target <- btl_cfg$refit_pairs_target
  cfg$stop_thresholds <- btl_cfg

  progress_handle <- adaptive_progress_init(state, cfg)
  on.exit(adaptive_progress_finish(progress_handle), add = TRUE)

  remaining <- n_steps
  while (remaining > 0L) {
    state <- run_one_step(state, judge, ...)
    step_row <- tibble::as_tibble(state$step_log)[nrow(state$step_log), , drop = FALSE]
    event <- adaptive_progress_step_event(step_row, cfg)
    if (!is.null(event)) {
      cli::cli_inform(event)
    }
    if (isTRUE(step_row$candidate_starved[[1L]])) {
      state$meta$stop_decision <- TRUE
      state$meta$stop_reason <- "candidate_starvation"
      if (!is.null(state$config$session_dir)) {
        save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
      }
      return(state)
    }

    refit_out <- maybe_refit_btl(state, config = btl_cfg, fit_fn = fit_fn)
    state <- refit_out$state
    if (isTRUE(refit_out$refit_performed)) {
      cfg$stop_thresholds <- refit_out$config
      metrics <- compute_stop_metrics(state, config = refit_out$config)
      state$stop_metrics <- metrics
      state <- .adaptive_maybe_enter_phase3(state, metrics, refit_out$config)
      stop_decision <- should_stop(metrics, config = refit_out$config)
      stop_reason <- if (isTRUE(stop_decision)) "btl_converged" else NA_character_

      round_row <- .adaptive_round_log_row(
        state = state,
        metrics = metrics,
        stop_decision = stop_decision,
        stop_reason = stop_reason,
        refit_context = refit_out$refit_context,
        config = refit_out$config
      )
      state$round_log <- append_round_log(state$round_log, round_row)
      item_log_tbl <- .adaptive_build_item_log_refit(
        state,
        refit_id = round_row$round_id
      )
      state <- .adaptive_append_item_log(state, item_log_tbl)
      if (!is.null(state$config$session_dir) &&
        isTRUE(state$config$persist_item_log)) {
        paths <- .adaptive_session_paths(state$config$session_dir)
        .adaptive_write_item_log_files(state$item_log, paths$item_log_dir)
      }
      if (cfg$progress %in% c("all", "refits")) {
        block <- adaptive_progress_refit_block(
          tibble::as_tibble(round_row),
          cfg
        )
        if (length(block) > 0L) {
          cat(paste(block, collapse = "\n"), "\n")
        }
      }
      if (isTRUE(stop_decision)) {
        state$meta$stop_decision <- TRUE
        state$meta$stop_reason <- stop_reason
        if (!is.null(state$config$session_dir)) {
          save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
        }
        return(state)
      }
    }
    if (!is.null(state$config$session_dir)) {
      save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
    }
    progress_handle <- adaptive_progress_update(progress_handle, state, cfg)
    remaining <- remaining - 1L
  }

  state
}

#' Adaptive ranking resume
#'
#' @description
#' Resume a previously persisted adaptive pairing session.
#'
#' @details
#' This is a thin wrapper around [load_adaptive_session()] and performs schema
#' and log-shape checks during load. Returned state preserves canonical
#' \code{step_log}, \code{round_log}, and \code{item_log} contents used for
#' adaptive auditability.
#'
#' @param session_dir Directory containing session artifacts.
#' @param ... Reserved for future extensions; currently unused.
#'
#' @return An \code{adaptive_state} object restored from disk.
#'
#' @examples
#' dir <- tempfile("pwllm-session-")
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 3)
#' save_adaptive_session(state, dir, overwrite = TRUE)
#' restored <- adaptive_rank_resume(dir)
#' summarize_adaptive(restored)
#'
#' @seealso [adaptive_rank_start()], [adaptive_rank_run_live()],
#'   [save_adaptive_session()], [load_adaptive_session()]
#'
#' @family adaptive ranking
#' @export
adaptive_rank_resume <- function(session_dir, ...) {
  if (missing(session_dir) || is.null(session_dir)) {
    rlang::abort("`session_dir` must be provided.")
  }
  load_adaptive_session(session_dir)
}
