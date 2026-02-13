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

#' @keywords internal
#' @noRd
.adaptive_round_activate_if_ready <- function(state) {
  out <- state
  out$controller <- .adaptive_controller_resolve(out)
  if (is.null(out$round) || !is.list(out$round)) {
    out$round <- .adaptive_new_round_state(
      out$item_ids,
      round_id = 1L,
      staged_active = FALSE,
      controller = out$controller
    )
  }
  if (isTRUE(out$warm_start_done)) {
    out$round$staged_active <- TRUE
    if ((out$round$round_committed %||% 0L) >= (out$round$round_pairs_target %||% 0L)) {
      out <- .adaptive_round_start_next(out)
    }
  }
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_active_stage <- function(state) {
  round <- state$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return("warm_start")
  }
  idx <- as.integer(round$stage_index %||% 1L)
  order <- as.character(round$stage_order %||% .adaptive_stage_order())
  if (idx < 1L || idx > length(order)) {
    return(NA_character_)
  }
  order[[idx]]
}

#' @keywords internal
#' @noRd
.adaptive_round_advance_stage <- function(state, shortfall = 0L) {
  out <- state
  round <- out$round
  idx <- as.integer(round$stage_index %||% 1L)
  order <- as.character(round$stage_order %||% .adaptive_stage_order())
  if (idx < 1L || idx > length(order)) {
    return(out)
  }
  stage <- order[[idx]]
  round$stage_shortfalls[[stage]] <- as.integer(
    (round$stage_shortfalls[[stage]] %||% 0L) + as.integer(shortfall %||% 0L)
  )
  round$stage_index <- as.integer(idx + 1L)
  out$round <- round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_start_next <- function(state) {
  out <- state
  out$controller <- .adaptive_controller_resolve(out)
  prior <- out$round %||% list(round_id = 0L, committed_total = 0L)
  out$refit_meta$last_completed_round_summary <- list(
    round_id = as.integer(prior$round_id %||% NA_integer_),
    global_identified = as.logical(prior$global_identified %||% NA),
    long_quota_raw = as.integer(prior$long_quota_raw %||% NA_integer_),
    long_quota_effective = as.integer(prior$long_quota_effective %||% NA_integer_),
    long_quota_removed = as.integer(prior$long_quota_removed %||% NA_integer_),
    realloc_to_mid = as.integer(prior$realloc_to_mid %||% NA_integer_),
    realloc_to_local = as.integer(prior$realloc_to_local %||% NA_integer_)
  )
  next_id <- as.integer((prior$round_id %||% 0L) + 1L)
  next_round <- .adaptive_new_round_state(
    item_ids = out$item_ids,
    round_id = next_id,
    staged_active = TRUE,
    controller = out$controller
  )
  next_round$committed_total <- as.integer(prior$committed_total %||% 0L)
  out$round <- next_round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_commit <- function(state, step_row) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return(out)
  }

  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  if (is.na(stage) || !stage %in% round$stage_order) {
    return(out)
  }
  round$committed_total <- as.integer((round$committed_total %||% 0L) + 1L)
  round$round_committed <- as.integer((round$round_committed %||% 0L) + 1L)
  round$stage_committed[[stage]] <- as.integer((round$stage_committed[[stage]] %||% 0L) + 1L)

  A <- as.integer(step_row$A[[1L]] %||% NA_integer_)
  B <- as.integer(step_row$B[[1L]] %||% NA_integer_)
  ids <- out$item_ids
  if (!is.na(A) && A >= 1L && A <= length(ids)) {
    a_id <- as.character(ids[[A]])
    a_prev <- as.integer(round$per_round_item_uses[[a_id]] %||% 0L)
    round$per_round_item_uses[[a_id]] <- as.integer((round$per_round_item_uses[[a_id]] %||% 0L) + 1L)
  } else {
    a_prev <- 0L
  }
  if (!is.na(B) && B >= 1L && B <= length(ids)) {
    b_id <- as.character(ids[[B]])
    b_prev <- as.integer(round$per_round_item_uses[[b_id]] %||% 0L)
    round$per_round_item_uses[[b_id]] <- as.integer((round$per_round_item_uses[[b_id]] %||% 0L) + 1L)
  } else {
    b_prev <- 0L
  }

  repeat_item_uses <- as.integer((a_prev > 0L) + (b_prev > 0L))
  if (repeat_item_uses > 0L) {
    round$repeat_in_round_used <- as.integer((round$repeat_in_round_used %||% 0L) + repeat_item_uses)
  }
  star_override_used <- FALSE
  if ("star_override_used" %in% names(step_row)) {
    star_override_used <- isTRUE(step_row$star_override_used[[1L]] %||% FALSE)
  }
  if (isTRUE(star_override_used)) {
    round$star_override_used <- as.integer((round$star_override_used %||% 0L) + 1L)
  }

  quota <- as.integer(round$stage_quotas[[stage]] %||% 0L)
  done <- as.integer(round$stage_committed[[stage]] %||% 0L)
  if (done >= quota) {
    out$round <- round
    out <- .adaptive_round_advance_stage(out, shortfall = 0L)
    round <- out$round
  } else {
    out$round <- round
  }

  stage_count <- length(round$stage_order %||% .adaptive_stage_order())
  if ((round$stage_index %||% 1L) > stage_count ||
    (round$round_committed %||% 0L) >= (round$round_pairs_target %||% 0L)) {
    out <- .adaptive_round_start_next(out)
  }

  out
}

#' @keywords internal
#' @noRd
.adaptive_round_commit_warm_start <- function(state) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round)) {
    return(out)
  }
  round$committed_total <- as.integer((round$committed_total %||% 0L) + 1L)
  round$round_committed <- as.integer((round$round_committed %||% 0L) + 1L)
  out$round <- round
  out
}

#' @keywords internal
#' @noRd
.adaptive_round_starvation <- function(state, step_row) {
  out <- state
  round <- out$round %||% NULL
  if (is.null(round) || !isTRUE(round$staged_active)) {
    return(list(state = out, exhausted = TRUE))
  }
  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  if (is.na(stage) || !stage %in% round$stage_order) {
    return(list(state = out, exhausted = TRUE))
  }
  shortfall <- max(0L, as.integer(round$stage_quotas[[stage]] %||% 0L) -
    as.integer(round$stage_committed[[stage]] %||% 0L))
  out <- .adaptive_round_advance_stage(out, shortfall = shortfall)
  stage_count <- length(out$round$stage_order %||% .adaptive_stage_order())
  exhausted <- (out$round$stage_index %||% 1L) > stage_count
  list(state = out, exhausted = exhausted)
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
#' @param adaptive_config Optional named list overriding adaptive controller
#'   behavior. Supported fields:
#'   \describe{
#'   \item{`global_identified_reliability_min`, `global_identified_rank_corr_min`}{Thresholds
#'   used to mark global identifiability after each refit.}
#'   \item{`p_long_low`, `p_long_high`}{Posterior probability gate used for
#'   long-link eligibility once globally identified.}
#'   \item{`long_taper_mult`, `long_frac_floor`, `mid_bonus_frac`}{Late-stage
#'   long-link taper and quota reallocation controls.}
#'   \item{`explore_taper_mult`}{Late-stage exploration taper multiplier.}
#'   \item{`boundary_k`, `boundary_window`, `boundary_frac`}{Local-stage
#'   boundary-priority controls after global identifiability.}
#'   \item{`p_star_override_margin`, `star_override_budget_per_round`}{Near-tie
#'   star-cap override controls.}
#'   \item{`run_mode`, `hub_id`, `link_transform_mode`, `link_refit_mode`,
#'   `shift_only_theta_treatment`, `judge_param_mode`, `hub_lock_mode`,
#'   `hub_lock_kappa`}{Linking mode
#'   scaffolding controls. Linking modes require multi-set inputs and valid hub
#'   assignment.}
#'   \item{`link_identified_reliability_min`, `link_stop_reliability_min`,
#'   `link_rank_corr_min`, `delta_sd_max`, `delta_change_max`,
#'   `log_alpha_sd_max`, `log_alpha_change_max`, `cross_set_ppc_mae_max`,
#'   `link_transform_escalation_refits_required`,
#'   `link_transform_escalation_is_one_way`,
#'   `spoke_quantile_coverage_bins`,
#'   `spoke_quantile_coverage_min_per_bin_per_refit`, `multi_spoke_mode`,
#'   `min_cross_set_pairs_per_spoke_per_refit`, `cross_set_utility`,
#'   `phase_a_mode`, `phase_a_import_failure_policy`,
#'   `phase_a_required_reliability_min`, `phase_a_compatible_model_ids`,
#'   `phase_a_compatible_config_hashes`, `phase_a_artifacts`,
#'   `phase_a_set_source`}{Linking
#'   threshold and control placeholders validated for future linking workflows.}
#'   }
#'   Unknown fields and invalid values abort with an actionable error.
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
                                ...,
                                adaptive_config = NULL) {
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
  state <- .adaptive_apply_controller_config(state, adaptive_config = adaptive_config)
  state$controller <- .adaptive_controller_resolve(state)
  state <- .adaptive_phase_a_prepare(state)
  state$round <- .adaptive_new_round_state(
    item_ids = state$item_ids,
    round_id = 1L,
    staged_active = isTRUE(state$warm_start_done),
    controller = state$controller
  )
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
#' Round scheduling uses stage-specific admissibility:
#' \itemize{
#'   \item rolling-anchor links compare one anchor and one non-anchor endpoint;
#'   \item long/mid links exclude anchor endpoints and enforce stratum-distance
#'   bounds;
#'   \item local-link routing admits same-stratum pairs and anchor-involving
#'   pairs within local stage bounds.
#' }
#'
#' Exposure and repeat handling are soft, stage-local constraints:
#' under-represented exploration uses degree set `deg <= D_min + 1`, while
#' repeat-pressure gating uses bottom-quantile `recent_deg` (default quantile
#' `0.25`) and per-endpoint repeat-slot accounting against
#' `repeat_in_round_budget`.
#'
#' Top-band defaults for stratum construction are
#' `top_band_pct = 0.10` and `top_band_bins = 5`, with top-band size
#' `ceiling(top_band_pct * N)`.
#'
#' Bayesian BTL refits are triggered on step-based cadence and evaluated with
#' diagnostics gates (including ESS thresholds), reliability, and lagged
#' stability criteria. Refit-level outcomes are
#' appended to \code{round_log}; per-item posterior summaries are appended to
#' \code{item_log}. Controller behavior can change after refits via
#' identifiability-gated settings in \code{adaptive_config}; those controls
#' affect pair routing and quotas, while BTL remains inference-only.
#'
#' @param state An adaptive state object created by [adaptive_rank_start()].
#' @param judge A function called as `judge(A, B, state, ...)` that returns a
#'   list with `is_valid = TRUE` and `Y` in `0/1`, or `is_valid = FALSE` with
#'   `invalid_reason`.
#' @param n_steps Maximum number of attempted adaptive steps to execute in this
#'   call. The run may terminate earlier if candidate starvation is encountered
#'   or if BTL stopping criteria are met at a refit. Each attempted step counts
#'   toward this budget, including invalid judge responses.
#' @param fit_fn Optional BTL fit function for deterministic testing; defaults
#'   to `default_btl_fit_fn()` when a refit is due.
#' @param adaptive_config Optional named list overriding adaptive controller
#'   behavior. Supported fields:
#'   `global_identified_reliability_min`, `global_identified_rank_corr_min`,
#'   `p_long_low`, `p_long_high`, `long_taper_mult`, `long_frac_floor`,
#'   `mid_bonus_frac`, `explore_taper_mult`, `boundary_k`, `boundary_window`,
#'   `boundary_frac`, `p_star_override_margin`,
#'   `star_override_budget_per_round`, and linking scaffolding fields:
#'   `run_mode`, `hub_id`, `link_transform_mode`, `link_refit_mode`,
#'   `shift_only_theta_treatment`, `judge_param_mode`, `hub_lock_mode`,
#'   `hub_lock_kappa`,
#'   `link_identified_reliability_min`, `link_stop_reliability_min`,
#'   `link_rank_corr_min`, `delta_sd_max`, `delta_change_max`,
#'   `log_alpha_sd_max`, `log_alpha_change_max`, `cross_set_ppc_mae_max`,
#'   `link_transform_escalation_refits_required`,
#'   `link_transform_escalation_is_one_way`,
#'   `spoke_quantile_coverage_bins`,
#'   `spoke_quantile_coverage_min_per_bin_per_refit`, `multi_spoke_mode`,
#'   `min_cross_set_pairs_per_spoke_per_refit`, `cross_set_utility`,
#'   `phase_a_mode`, `phase_a_import_failure_policy`,
#'   `phase_a_required_reliability_min`, `phase_a_compatible_model_ids`,
#'   `phase_a_compatible_config_hashes`, `phase_a_artifacts`, and
#'   `phase_a_set_source`.
#'   Unknown fields and invalid values abort with an actionable error.
#' @param btl_config Optional named list overriding BTL refit cadence, stopping
#'   thresholds, and selected round-log diagnostics. Supported fields:
#'   \describe{
#'   \item{`refit_pairs_target`}{Minimum new committed comparisons required
#'   before the next BTL refit.}
#'   \item{`model_variant`}{BTL MCMC variant: `"btl"`, `"btl_e"`, `"btl_b"`,
#'   or `"btl_e_b"`.}
#'   \item{`ess_bulk_min`}{Minimum bulk ESS required for diagnostics to pass.}
#'   \item{`ess_bulk_min_near_stop`}{Stricter ESS requirement when a run is
#'   close to stopping.}
#'   \item{`max_rhat`}{Maximum allowed split-\eqn{\hat{R}} diagnostic value.}
#'   \item{`divergences_max`}{Maximum allowed divergent transitions.}
#'   \item{`eap_reliability_min`}{Minimum EAP reliability to allow stopping.}
#'   \item{`stability_lag`}{Lag (in refits) used for stability checks.}
#'   \item{`theta_corr_min`}{Minimum lagged correlation of posterior means.}
#'   \item{`theta_sd_rel_change_max`}{Maximum relative change in posterior SD
#'   allowed by stability checks.}
#'   \item{`rank_spearman_min`}{Minimum lagged Spearman rank correlation.}
#'   \item{`near_tie_p_low`, `near_tie_p_high`}{Probability band used only for
#'   near-tie diagnostics in round logging (not used for stopping decisions).}
#'   }
#'   Defaults are resolved from the current item count, then merged with user
#'   overrides.
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
#'   adaptive_config = list(
#'     global_identified_reliability_min = 0.85,
#'     star_override_budget_per_round = 2L
#'   ),
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
#'   adaptive_config = list(
#'     explore_taper_mult = 0.40,
#'     boundary_frac = 0.20
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
                                   adaptive_config = NULL,
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
  state <- .adaptive_apply_controller_config(state, adaptive_config = adaptive_config)
  state <- .adaptive_phase_a_prepare(state)
  .adaptive_phase_a_gate_or_abort(state)

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
    state <- .adaptive_round_activate_if_ready(state)
    state <- run_one_step(state, judge, ...)
    step_row <- tibble::as_tibble(state$step_log)[nrow(state$step_log), , drop = FALSE]
    event <- adaptive_progress_step_event(step_row, cfg)
    if (!is.null(event)) {
      cli::cli_inform(event)
    }
    if (isTRUE(step_row$status[[1L]] == "ok")) {
      if (identical(step_row$round_stage[[1L]], "warm_start")) {
        state <- .adaptive_round_commit_warm_start(state)
      } else {
        state <- .adaptive_round_commit(state, step_row)
      }
    } else if (isTRUE(step_row$candidate_starved[[1L]]) &&
      !identical(step_row$round_stage[[1L]], "warm_start")) {
      starve <- .adaptive_round_starvation(state, step_row)
      state <- starve$state
      if (isTRUE(starve$exhausted)) {
        state$meta$stop_decision <- TRUE
        state$meta$stop_reason <- "candidate_starvation"
        if (!is.null(state$config$session_dir)) {
          save_adaptive_session(state, session_dir = state$config$session_dir, overwrite = TRUE)
        }
        return(state)
      }
    } else if (isTRUE(step_row$candidate_starved[[1L]])) {
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
      state <- .adaptive_linking_refit_update_state(
        state = state,
        refit_context = refit_out$refit_context
      )
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
      link_rows <- .adaptive_link_stage_refit_rows(
        state = state,
        refit_id = as.integer(round_row$refit_id),
        refit_context = refit_out$refit_context
      )
      if (nrow(link_rows) > 0L) {
        state$link_stage_log <- append_link_stage_log(
          state$link_stage_log %||% new_link_stage_log(),
          link_rows
        )
      }
      item_log_tbl <- .adaptive_build_item_log_refit(
        state,
        refit_id = round_row$refit_id
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
