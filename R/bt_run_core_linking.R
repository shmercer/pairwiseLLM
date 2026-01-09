#' Run a core-linking batch workflow end-to-end (round-based)
#'
#' This runner orchestrates a multi-wave (batch) workflow using a stable core
#' linking set. For each batch of new items, it runs a round-based loop:
#' propose pairs (core↔new + new↔new + optional core↔core audit), score the pairs
#' via \code{judge_fun}, append results, fit a BT model, compute stop metrics on
#' the batch's new IDs (optionally including core drift), then stop or continue.
#'
#' Stopping is typically driven by precision on the batch's new items (e.g.,
#' \code{rel_se_p90}) and can be gated by core drift guardrails
#' (via \code{core_*_target} thresholds).
#'
#' The runner also records a compact per-round \emph{state snapshot} that summarizes
#' the accumulated judged results after each round. These summaries are returned in
#' \code{$state} and include both overall counts (all IDs) and \code{new_}-prefixed
#' counts restricted to the current batch's new IDs.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}. \code{ID}
#'   must be unique and non-missing.
#' @param batches A non-empty list where each element is a character vector of IDs to
#'   be added in that batch. IDs must be present in \code{samples$ID}.
#' @param core_ids Optional character vector of core IDs. If \code{NULL}, core IDs are
#'   selected using \code{\link{select_core_set}}.
#' @param core_method Core selection method used when \code{core_ids} is \code{NULL}.
#'   Passed to \code{\link{select_core_set}}.
#' @param core_size Core size used when \code{core_ids} is \code{NULL}.
#' @param embeddings Optional embedding matrix for embeddings-based core selection
#'   (\code{core_method} in \code{c("auto","pam","clara","embeddings")}).
#'
#' @param linking Whether to apply anchoring/linking so theta estimates are reported
#'   on a stable scale defined by the baseline core fit. One of
#'   \code{"auto"}, \code{"always"}, or \code{"never"}. In \code{"auto"},
#'   linking is applied only when core drift exceeds the thresholds below.
#' @param linking_method Linking method passed to [bt_link_thetas()].
#'   Robust methods (\code{"median_iqr"}, \code{"median_mad"}) are recommended when
#'   rounds are close to deterministic (separation).
#' @param linking_cor_target In \code{linking = "auto"}, apply linking when the core
#'   Pearson correlation between baseline and current raw thetas is below this value.
#' @param linking_p90_abs_shift_target In \code{linking = "auto"}, apply linking when the
#'   90th percentile of the absolute core-theta shift (baseline vs current raw) exceeds
#'   this value.
#' @param linking_max_abs_shift_target In \code{linking = "auto"}, apply linking when the
#'   maximum absolute core-theta shift (baseline vs current raw) exceeds this value.
#' @param linking_min_n Minimum number of core IDs required to estimate the linking
#'   transform. If fewer are available, linking is skipped.
#' @param reference_scale_method Method used to stabilize the *reference* (baseline)
#'   theta scale before it is used for linking decisions. Defaults to a robust
#'   median/IQR-based scale. This reduces pathological behavior when the early core
#'   fit is close to deterministic (separation).
#' @param reference_max_abs Maximum absolute value allowed for reference thetas after
#'   stabilization (clamping). This is applied only to the reference fit used for
#'   linking/drift diagnostics.
#' @param judge_fun Function that accepts a tibble of pairs with columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2} and returns a tibble with columns
#'   \code{ID1}, \code{ID2}, \code{better_id}. If \code{judge} is provided, the output
#'   must also include that column.
#' @param initial_results Optional tibble of previously-judged results (same schema as
#'   output of \code{judge_fun}). Used as a warm start.
#' @param judge Optional string naming the judge column to pass through to modeling.
#'
#' @param fit_fun Function that fits a BT model from BT data (default \code{\link{fit_bt_model}}).
#' @param build_bt_fun Function to build BT data from results (default \code{\link{build_bt_data}}).
#'
#' @param engine Passed to \code{fit_fun} when \code{fit_fun = fit_bt_model}.
#' @param fit_verbose Passed to \code{fit_fun} when \code{fit_fun = fit_bt_model}.
#' @param return_diagnostics Passed to \code{fit_fun} when \code{fit_fun = fit_bt_model}.
#' @param include_residuals Passed to \code{fit_fun} when \code{fit_fun = fit_bt_model}.
#'
#' @param fit_engine_running Running fitting engine used to propose the *next* round
#'   of pairs. One of \code{"bt"} (default; uses the linked BT thetas when available)
#'   or \code{"rank_centrality"} (Rank Centrality via \code{\link{fit_rank_centrality}}).
#' @param store_running_estimates Logical; if TRUE (default) store the per-round
#'   \emph{running} fits in \code{$fits}. When FALSE, store the raw BT fits.
#' @param rc_smoothing,rc_damping Passed to \code{\link{fit_rank_centrality}} when
#'   \code{fit_engine_running = "rank_centrality"}.
#' @param final_refit Logical; if TRUE (default), compute a final combined estimates
#'   table via \code{\link{compute_final_estimates}} on all accumulated results.
#' @param final_bt_bias_reduction Logical; if TRUE (default), attempt bias-reduced
#'   Bradley--Terry (Firth) at the final refit when \code{final_refit = TRUE}.
#'
#' @param round_size Target number of pairs proposed per round (per batch).
#' @param max_rounds_per_batch Maximum rounds to run for each batch.
#' @param within_batch_frac Fraction of each round allocated to new<->new comparisons.
#' @param core_audit_frac Fraction of each round allocated to core<->core audit comparisons.
#' @param allocation Allocation preset controlling how within-batch and auditing
#'   fractions may be adjusted between rounds. One of `"fixed"`, `"precision_ramp"`,
#'   or `"audit_on_drift"`. If `allocation_fun` is supplied, it takes precedence
#'   and `allocation` is ignored.
#' @param allocation_fun Optional function to update `within_batch_frac` and/or `core_audit_frac`
#'   between rounds. It is called after metrics are computed each round with a state list containing
#'   (at minimum) `batch_index`, `round_index`, `within_batch_frac`, `core_audit_frac`, `metrics`,
#'   and `prev_metrics`. It should return NULL (no change) or a list with elements
#'   `within_batch_frac` and/or `core_audit_frac`.
#' @param k_neighbors Passed to \code{\link{select_core_link_pairs}}.
#' @param min_judgments Passed to \code{\link{select_core_link_pairs}}.
#' @param forbid_repeats Forbid repeat unordered pairs across the entire run.
#' @param balance_positions Balance positions (ID1 vs ID2) when proposing pairs.
#' @param min_rounds Integer. Minimum number of adaptive rounds to run before allowing
#'   stability- or precision-based stopping. Hard stops (no new pairs, budget exhausted,
#'   max rounds) can still terminate earlier. Default \code{2}.
#' @param stop_stability_rms Numeric. Threshold on RMS change in \code{theta} between consecutive
#'   fits; lower values indicate greater stability. Default \code{0.01}.
#' @param stop_stability_consecutive Integer. Number of consecutive rounds the stability criteria
#'   must hold before stopping. Default \code{2}.
#' @param stop_topk Integer. Size \code{k} for the top-\code{k} overlap stability check. Default \code{50}.
#' @param stop_topk_overlap Numeric in \code{[0, 1]}. Minimum overlap fraction between consecutive top-\code{k}
#'   sets required to consider rankings stable. Default \code{0.95}.
#' @param stop_topk_ties Character. How to handle ties at the \code{k}-th boundary for the top-\code{k}
#'   overlap check. One of \code{"id"} (deterministic) or \code{"random"}. Default \code{"id"}.
#' @param stop_min_largest_component_frac Numeric in \code{[0, 1]}. Minimum fraction of nodes that must lie in the
#'   largest connected component for the comparison graph to be considered healthy. Default \code{0.9}.
#' @param stop_min_degree Integer. Minimum node degree required for the comparison graph to be considered
#'   healthy. Default \code{1}.
#' @param stop_reason_priority Optional character vector specifying a priority order for stop reasons when
#'   multiple stopping criteria are met on the same round. If \code{NULL}, a default priority is used.
#'
#'
#' @param se_probs Passed to \code{\link{bt_stop_metrics}}.
#' @param fit_bounds Passed to \code{\link{bt_stop_metrics}} when diagnostics are available.
#' @param stopping_tier Preset stopping thresholds to use (good/strong/very_strong).
#'
#' @param reliability_target Passed to \code{\link{bt_should_stop}}.
#' @param sepG_target Passed to \code{\link{bt_should_stop}}.
#' @param rel_se_p90_target Passed to \code{\link{bt_should_stop}}.
#' @param rel_se_p90_min_improve Passed to \code{\link{bt_should_stop}}.
#' @param max_item_misfit_prop Passed to \code{\link{bt_should_stop}}.
#' @param max_judge_misfit_prop Passed to \code{\link{bt_should_stop}}.
#'
#' @param exhaustion_fallback Fallback strategy to use when the within-batch pair
#'   generator becomes exhausted and fewer than
#'   \code{ceiling(round_size * exhaustion_min_pairs_frac)} admissible pairs are
#'   available. One of \code{"none"}, \code{"cross_batch_new_new"},
#'   \code{"targeted_repeats"}, or \code{"both"}.
#' @param exhaustion_min_pairs_frac Minimum fraction of \code{round_size} that must
#'   be available before triggering the fallback. Must be in \code{(0, 1]}.
#' @param exhaustion_spectral_gap_threshold Optional spectral-gap threshold that
#'   can be used to gate exhaustion handling (reserved for future use).
#'
#' @param core_theta_cor_target Optional drift guardrail for Pearson correlation
#'   (default \code{NA} = disabled).
#' @param core_theta_spearman_target Optional drift guardrail for Spearman correlation
#'   (default \code{NA} = disabled).
#' @param core_max_abs_shift_target Optional drift guardrail for maximum abs shift
#'   (default \code{NA} = disabled).
#' @param core_p90_abs_shift_target Optional drift guardrail for p90 abs shift
#'   (default \code{NA} = disabled).
#'
#' @param drift_reference Drift reference for computing core drift metrics:
#'   \code{"previous_round"} compares to the prior round's fit; \code{"baseline"} compares
#'   to a fixed baseline fit.
#' @param seed Optional integer seed used to make pair proposal reproducible across runs.
#' @param verbose Logical; print minimal progress per batch/round.
#'
#' @param checkpoint_dir Optional directory path for writing checkpoint files during
#'   the run. If provided, the runner writes \code{run_state.rds} (and optionally
#'   per-round snapshot files) after completed rounds and/or batch boundaries. Use
#'   this to resume long jobs after interruption or errors.
#'
#' @param resume_from Optional directory path containing a prior checkpoint file
#'   \code{run_state.rds} created by \code{bt_run_core_linking()}. When provided, the
#'   run resumes from the saved state, including accumulated results and batch/round
#'   indices. The \code{samples}, \code{batches}, and \code{core_ids} must be
#'   compatible with the checkpoint.
#'
#' @param checkpoint_every Integer controlling how frequently per-round snapshot
#'   files are written. A value of \code{1} writes a snapshot after every completed
#'   round; a value of \code{2} writes snapshots every other round, etc. The main
#'   file \code{run_state.rds} is still updated at safe points even when
#'   \code{checkpoint_every > 1}.
#'
#' @param checkpoint_store_fits Logical indicating whether to store fitted model
#'   objects (BT fits and diagnostics) inside checkpoint files. Set to \code{FALSE}
#'   to reduce checkpoint size; fits may be recomputed after resuming.
#'
#' @param checkpoint_overwrite Logical indicating whether to overwrite an existing
#'   \code{run_state.rds} file in \code{checkpoint_dir}. If \code{FALSE} and a
#'   checkpoint already exists, the function should error rather than overwrite.
#'
#' @param ... Additional arguments forwarded to \code{fit_fun}.
#'
#' @details
#' \strong{Checkpointing and resuming:} If \code{checkpoint_dir} is provided, this
#' function writes a checkpoint representing the last completed safe point (after a
#' round completes, and at batch boundaries). If the run is interrupted, resume by
#' calling again with \code{resume_from = checkpoint_dir}.
#'
#' @return A list with:
#' \describe{
#'   \item{core_ids}{Core linking IDs used.}
#'   \item{batches}{Normalized batches list.}
#'   \item{results}{All judged results (canonicalized \code{better_id}).}
#'   \item{fits}{List of per-round fits (including bootstrap/warm start).}
#'   \item{final_fits}{Named list of final fit per batch (plus \code{"bootstrap"}).}
#'   \item{metrics}{Tibble of stop metrics per round (computed on batch new IDs).}
#'   \item{state}{A tibble with one row per scoring round (including bootstrap/warm start)
#'   containing bookkeeping summaries of the accumulated results (overall and for the
#'   current batch's new IDs). The overall fields include
#'   \code{n_unique_unordered_pairs}, appearance quantiles, \code{pos_imbalance_max},
#'   \code{n_self_pairs}, \code{n_missing_better_id}, and \code{n_judges} (if applicable).
#'   New-ID-restricted fields are prefixed with \code{new_}. Rows also include
#'   \code{batch_index}, \code{round_index}, \code{stage}, and (when applicable)
#'   \code{stop_reason}.}
#'   \item{batch_summary}{One row per batch: rounds used, stop reason, counts.}
#' }
#'
#' @examples
#' # CRAN-safe example (no APIs, no sirt): deterministic simulated judging + mock fit.
#' samples <- tibble::tibble(
#'   ID = LETTERS[1:6],
#'   text = paste("text", LETTERS[1:6])
#' )
#' batches <- list(batch1 = c("D", "E"), batch2 = c("F"))
#' core_ids <- c("A", "B", "C")
#'
#' # Deterministic simulated judge (always picks the higher true theta)
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1, E = -2, F = -3)
#' judge_fun <- function(pairs) simulate_bt_judge(pairs, true_theta, deterministic = TRUE)
#'
#' # Tiny mock fit: returns required structure (ID/theta/se)
#' round <- 0
#' mock_fit <- function(bt_data, ...) {
#'   round <<- round + 1
#'   ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
#'   se <- rep(max(0.60 - 0.15 * round, 0.05), length(ids))
#'   list(
#'     engine = "mock",
#'     reliability = NA_real_,
#'     theta = tibble::tibble(ID = ids, theta = seq_along(ids), se = se),
#'     diagnostics = list(sepG = NA_real_)
#'   )
#' }
#'
#' out <- bt_run_core_linking(
#'   samples = samples,
#'   batches = batches,
#'   core_ids = core_ids,
#'   judge_fun = judge_fun,
#'   fit_fun = mock_fit,
#'   engine = "mock",
#'   round_size = 8,
#'   max_rounds_per_batch = 3,
#'   # disable thresholds requiring sirt diagnostics for this example
#'   reliability_target = NA_real_,
#'   sepG_target = NA_real_,
#'   max_item_misfit_prop = NA_real_,
#'   max_judge_misfit_prop = NA_real_,
#'   rel_se_p90_target = 0.80,
#'   verbose = FALSE
#' )
#' out$batch_summary
#'
#' @export
bt_run_core_linking <- function(samples,
                                batches,
                                core_ids = NULL,
                                core_method = c("auto", "pam", "clara", "embeddings", "token_stratified", "random"),
                                core_size = 30,
                                embeddings = NULL,
                                linking = c("auto", "always", "never"),
                                linking_method = c("median_iqr", "median_mad", "mean_sd"),
                                linking_cor_target = 0.98,
                                linking_p90_abs_shift_target = 0.15,
                                linking_max_abs_shift_target = 0.30,
                                linking_min_n = 3L,
                                reference_scale_method = c("median_iqr", "median_mad", "mean_sd"),
                                reference_max_abs = 6,
                                judge_fun,
                                initial_results = NULL,
                                judge = NULL,
                                fit_fun = fit_bt_model,
                                build_bt_fun = build_bt_data,
                                engine = "sirt",
                                fit_verbose = FALSE,
                                return_diagnostics = TRUE,
                                include_residuals = FALSE,
                                fit_engine_running = c("bt", "rank_centrality"),
                                store_running_estimates = TRUE,
                                rc_smoothing = 0.5,
                                rc_damping = 0,
                                final_refit = TRUE,
                                final_bt_bias_reduction = TRUE,
                                round_size = 50,
                                max_rounds_per_batch = 50,
                                # PR7: stopping controls
                                min_rounds = 2L,
                                stop_stability_rms = 0.01,
                                stop_topk = 50L,
                                stop_topk_overlap = 0.95,
                                stop_min_largest_component_frac = 0.9,
                                stop_min_degree = 1L,
                                stop_reason_priority = NULL,
                                stop_stability_consecutive = 2L,
                                stop_topk_ties = c("id", "random"),
                                within_batch_frac = 0.25,
                                core_audit_frac = 0.10,
                                allocation = c("fixed", "precision_ramp", "audit_on_drift"),
                                allocation_fun = NULL,
                                k_neighbors = 10,
                                min_judgments = 12,
                                forbid_repeats = TRUE,
                                balance_positions = TRUE,
                                se_probs = c(0.5, 0.9, 0.95),
                                fit_bounds = c(0.7, 1.3),
                                stopping_tier = c("strong", "good", "very_strong"),
                                reliability_target = 0.90,
                                sepG_target = 3.0,
                                rel_se_p90_target = 0.30,
                                rel_se_p90_min_improve = 0.01,
                                max_item_misfit_prop = 0.05,
                                max_judge_misfit_prop = 0.05,
                                exhaustion_fallback = c("none", "cross_batch_new_new", "targeted_repeats", "both"),
                                exhaustion_min_pairs_frac = 0.5,
                                exhaustion_spectral_gap_threshold = 0,
                                core_theta_cor_target = NA_real_,
                                core_theta_spearman_target = NA_real_,
                                core_max_abs_shift_target = NA_real_,
                                core_p90_abs_shift_target = NA_real_,
                                drift_reference = c("previous_round", "baseline"),
                                seed = NULL,
                                verbose = TRUE,
                                checkpoint_dir = NULL,
                                resume_from = NULL,
                                checkpoint_every = 1L,
                                checkpoint_store_fits = TRUE,
                                checkpoint_overwrite = TRUE,
                                ...) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must have columns 'ID' and 'text'.", call. = FALSE)
  }
  ids_all <- as.character(samples$ID)
  if (anyNA(ids_all) || any(ids_all == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids_all))) stop("`samples$ID` must be unique.", call. = FALSE)

  if (!is.function(judge_fun)) stop("`judge_fun` must be a function.", call. = FALSE)
  if (!is.function(fit_fun)) stop("`fit_fun` must be a function.", call. = FALSE)
  if (!is.function(build_bt_fun)) stop("`build_bt_fun` must be a function.", call. = FALSE)

  if (!is.null(allocation_fun) && !is.function(allocation_fun)) {
    stop("`allocation_fun` must be a function or NULL.", call. = FALSE)
  }

  drift_reference <- match.arg(drift_reference)

  # `core_method` is only relevant when we need to select a core set.
  # When `core_ids` is supplied, we skip strict validation and treat it as "fixed".
  if (is.null(core_ids)) {
    core_method <- match.arg(core_method)
  } else {
    if (missing(core_method) || length(core_method) != 1L) {
      core_method <- "fixed"
    } else {
      core_method <- as.character(core_method)[1]
    }
  }

  allocation <- match.arg(allocation)
  if (is.null(allocation_fun) && allocation != "fixed") {
    allocation_fun <- switch(allocation,
      precision_ramp = allocation_precision_ramp(),
      audit_on_drift = allocation_audit_on_drift(),
      NULL
    )
  }


  linking <- match.arg(linking)
  linking_method <- match.arg(linking_method)
  reference_scale_method <- match.arg(reference_scale_method)
  if (!is.numeric(reference_max_abs) || length(reference_max_abs) != 1L || is.na(reference_max_abs) || !is.finite(reference_max_abs) || reference_max_abs <= 0) {
    stop("`reference_max_abs` must be a single finite number > 0.", call. = FALSE)
  }
  if (!is.numeric(linking_cor_target) || length(linking_cor_target) != 1L) {
    stop("`linking_cor_target` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(linking_p90_abs_shift_target) || length(linking_p90_abs_shift_target) != 1L) {
    stop("`linking_p90_abs_shift_target` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(linking_max_abs_shift_target) || length(linking_max_abs_shift_target) != 1L) {
    stop("`linking_max_abs_shift_target` must be a single numeric value.", call. = FALSE)
  }
  linking_min_n <- as.integer(linking_min_n)
  if (length(linking_min_n) != 1L || is.na(linking_min_n) || linking_min_n < 2L) {
    stop("`linking_min_n` must be a single integer >= 2.", call. = FALSE)
  }

  stopping_tier <- match.arg(stopping_tier)
  stop_topk_ties <- match.arg(stop_topk_ties)
  stop_params <- bt_stop_tiers()[[stopping_tier]]

  # Capture and sanitize `...` forwarded to fit_fun. This prevents collisions like
  # `verbose` (often intended for runner logging) being passed twice to fit_fun.
  .fit_dots <- list(...)
  if (!is.null(.fit_dots$verbose) && missing(fit_verbose)) {
    fit_verbose <- isTRUE(.fit_dots$verbose)
  }
  .fit_dots <- .clean_fit_dots(.fit_dots)

  fit_engine_running <- match.arg(fit_engine_running)
  store_running_estimates <- isTRUE(store_running_estimates)
  final_refit <- isTRUE(final_refit)
  final_bt_bias_reduction <- isTRUE(final_bt_bias_reduction)

  exhaustion_fallback <- match.arg(exhaustion_fallback)
  if (!is.numeric(exhaustion_min_pairs_frac) || length(exhaustion_min_pairs_frac) != 1L ||
    is.na(exhaustion_min_pairs_frac) || exhaustion_min_pairs_frac < 0 || exhaustion_min_pairs_frac > 1) {
    stop("`exhaustion_min_pairs_frac` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(exhaustion_spectral_gap_threshold) || length(exhaustion_spectral_gap_threshold) != 1L ||
    is.na(exhaustion_spectral_gap_threshold) || exhaustion_spectral_gap_threshold < 0) {
    stop("`exhaustion_spectral_gap_threshold` must be a single numeric value >= 0.", call. = FALSE)
  }

  if (!is.numeric(rc_smoothing) || length(rc_smoothing) != 1L || is.na(rc_smoothing) || rc_smoothing < 0) {
    stop("`rc_smoothing` must be a single numeric value >= 0.", call. = FALSE)
  }
  if (!is.numeric(rc_damping) || length(rc_damping) != 1L || is.na(rc_damping) || rc_damping < 0 || rc_damping > 1) {
    stop("`rc_damping` must be a single numeric value in [0, 1].", call. = FALSE)
  }


  # Override tier defaults only when the caller explicitly supplies thresholds.
  if (!missing(reliability_target)) stop_params$reliability_target <- reliability_target
  if (!missing(sepG_target)) stop_params$sepG_target <- sepG_target
  if (!missing(rel_se_p90_target)) stop_params$rel_se_p90_target <- rel_se_p90_target
  if (!missing(rel_se_p90_min_improve)) stop_params$rel_se_p90_min_improve <- rel_se_p90_min_improve
  if (!missing(max_item_misfit_prop)) stop_params$max_item_misfit_prop <- max_item_misfit_prop
  if (!missing(max_judge_misfit_prop)) stop_params$max_judge_misfit_prop <- max_judge_misfit_prop
  if (!missing(core_theta_cor_target)) stop_params$core_theta_cor_target <- core_theta_cor_target
  if (!missing(core_theta_spearman_target)) stop_params$core_theta_spearman_target <- core_theta_spearman_target
  if (!missing(core_max_abs_shift_target)) stop_params$core_max_abs_shift_target <- core_max_abs_shift_target
  if (!missing(core_p90_abs_shift_target)) stop_params$core_p90_abs_shift_target <- core_p90_abs_shift_target

  round_size <- as.integer(round_size)
  if (length(round_size) != 1L || is.na(round_size) || round_size < 0L) {
    stop("`round_size` must be a single non-negative integer.", call. = FALSE)
  }
  max_rounds_per_batch <- as.integer(max_rounds_per_batch)
  if (length(max_rounds_per_batch) != 1L || is.na(max_rounds_per_batch) || max_rounds_per_batch < 0L) {
    stop("`max_rounds_per_batch` must be a single non-negative integer.", call. = FALSE)
  }

  checkpoint_every <- as.integer(checkpoint_every)
  if (is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be an integer >= 1.", call. = FALSE)
  }
  if (!is.null(resume_from) && (is.null(checkpoint_dir) || !nzchar(checkpoint_dir))) {
    checkpoint_dir <- resume_from
  }


  batches <- .normalize_batches_list(batches, ids_all)

  if (is.null(core_ids)) {
    core_out <- select_core_set(
      samples = samples,
      core_size = core_size,
      method = core_method,
      embeddings = embeddings,
      seed = seed
    )

    if (is.data.frame(core_out) && "ID" %in% names(core_out)) {
      core_ids <- as.character(core_out$ID)
    } else {
      core_ids <- as.character(core_out)
    }

    if (anyNA(core_ids) || any(core_ids == "")) {
      stop("`core_ids` must be non-missing and non-empty.", call. = FALSE)
    }
    if (any(duplicated(core_ids))) {
      stop("`core_ids` must be unique.", call. = FALSE)
    }
    if (!all(core_ids %in% ids_all)) {
      stop("All `core_ids` must be present in `samples$ID`.", call. = FALSE)
    }
  } else {
    core_ids_in <- as.character(core_ids)

    if (anyNA(core_ids_in) || any(core_ids_in == "")) {
      stop("`core_ids` must be non-missing and non-empty.", call. = FALSE)
    }
    if (any(duplicated(core_ids_in))) {
      stop("`core_ids` must be unique.", call. = FALSE)
    }
    if (!all(core_ids_in %in% ids_all)) {
      stop("All `core_ids` must be present in `samples$ID`.", call. = FALSE)
    }

    core_ids <- unique(core_ids_in)
  }

  if (length(core_ids) < 2L) stop("`core_ids` must include at least 2 IDs.", call. = FALSE)

  results <- tibble::tibble()
  if (!is.null(initial_results)) {
    results <- .validate_judge_results(initial_results, ids = ids_all, judge_col = judge)
  }

  # If the caller explicitly requests no new sampling and provides no initial
  # results, stop immediately with a consistent stop reason (rather than
  # failing the initial fit with 0 results).
  if (is.null(resume_from) && nrow(results) == 0L && round_size == 0L) {
    out <- list(
      core_ids = core_ids,
      batches = batches,
      results = results,
      estimates = NULL,
      theta = NULL,
      theta_engine = NA_character_,
      fit_provenance = list(),
      pairing_diagnostics = NULL,
      fits = list(),
      final_fits = list(),
      metrics = .bt_align_metrics(tibble::tibble(), se_probs = se_probs),
      state = .bt_align_state(tibble::tibble()),
      batch_summary = tibble::tibble(),
      stop_reason = .bt_resolve_stop_reason(round_size_zero = TRUE),
      stop_round = 0L
    )

    if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
      payload <- list(
        run_type = "core_linking",
        ids = ids_all,
        core_ids = core_ids,
        batches = batches,
        timestamp = Sys.time(),
        completed = TRUE,
        out = out
      )
      .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    }

    validate_pairwise_run_output(out)
    return(.as_pairwise_run(out, run_type = "core_linking"))
  }

  # ---- resume / checkpoint state ----
  checkpoint_payload_last <- NULL
  batch_start <- 1L
  round_start <- 1L
  resume_in_batch <- FALSE
  resume_new_ids <- NULL
  resume_prev_metrics <- NULL

  if (!is.null(resume_from)) {
    chk <- .bt_read_checkpoint(resume_from)
    .bt_validate_checkpoint(chk, run_type = "core_linking", ids = ids_all)

    if (!is.null(chk$core_ids)) {
      core_ids_chk <- as.character(chk$core_ids)
      if (length(core_ids_chk) != length(core_ids) || any(sort(core_ids_chk) != sort(core_ids))) {
        .abort_checkpoint_mismatch(
          field = "core_ids",
          expected = core_ids,
          actual = core_ids_chk,
          hint = "If you changed `core_ids` between runs, restart without `resume_from`."
        )
      }
    }

    if (!is.null(chk$fit_engine_running)) {
      eng_chk <- as.character(chk$fit_engine_running)[1]
      if (!identical(eng_chk, fit_engine_running)) {
        .abort_checkpoint_mismatch(
          field = "fit_engine_running",
          expected = fit_engine_running,
          actual = eng_chk,
          hint = "If you changed the running engine between runs, restart without `resume_from`."
        )
      }
    }

    if (!is.null(chk$random_seed)) {
      try(assign(".Random.seed", chk$random_seed, envir = .GlobalEnv), silent = TRUE)
    }

    # restore persisted state
    results <- tibble::as_tibble(chk$results)
    fits <- chk$fits %||% list()
    final_fits <- chk$final_fits %||% list()
    metrics_hist <- tibble::as_tibble(chk$metrics %||% tibble::tibble())
    state_hist <- tibble::as_tibble(chk$state %||% tibble::tibble())
    batch_summary <- tibble::as_tibble(chk$batch_summary %||% tibble::tibble())

    current_fit <- chk$current_fit %||% NULL
    current_fit_running <- chk$current_fit_running %||% NULL
    baseline_fit <- chk$baseline_fit %||% NULL
    baseline_results_n <- as.integer(chk$baseline_results_n %||% NA_integer_)
    seen_ids <- unique(as.character(chk$seen_ids %||% core_ids))

    within_batch_frac <- chk$within_batch_frac %||% within_batch_frac
    core_audit_frac <- chk$core_audit_frac %||% core_audit_frac

    batch_start <- as.integer(chk$next_batch_index %||% 1L)
    round_start <- as.integer(chk$next_round_index %||% 1L)
    if (is.na(batch_start) || batch_start < 1L) batch_start <- 1L
    if (is.na(round_start) || round_start < 1L) round_start <- 1L

    resume_in_batch <- isTRUE(chk$in_batch)
    resume_new_ids <- chk$new_ids_current %||% NULL
    resume_prev_metrics <- chk$prev_metrics_current %||% NULL

    if (isTRUE(chk$completed)) {
      # Completed checkpoints from older versions (or tests) may store a minimal `out`.
      # Upgrade it to the PR8 run-output contract by filling missing required fields.
      out_skel <- list(
        core_ids = core_ids,
        batches = batches,
        results = results,
        estimates = NULL,
        theta = NULL,
        theta_engine = NA_character_,
        fit_provenance = list(),
        stop_reason = chk$stop_reason %||% NA_character_,
        stop_round = chk$stop_round %||% NA_integer_,
        pairing_diagnostics = NULL,
        bt_data = chk$bt_data %||% NULL,
        fits = fits,
        final_fits = final_fits,
        final_models = chk$final_models %||% list(),
        metrics = metrics_hist,
        state = state_hist,
        batch_summary = batch_summary
      )

      out <- out_skel
      if (is.list(chk$out)) {
        out <- utils::modifyList(out_skel, chk$out, keep.null = TRUE)
      }

      # Defensive: ensure required fields exist even if a legacy checkpoint stored a minimal list.
      if (is.null(out$fit_provenance)) out$fit_provenance <- list()
      if (is.null(out$theta_engine)) out$theta_engine <- NA_character_
      if (!("estimates" %in% names(out))) out["estimates"] <- list(NULL)
      if (!("theta" %in% names(out))) out["theta"] <- list(NULL)
      if (!("pairing_diagnostics" %in% names(out))) out["pairing_diagnostics"] <- list(NULL)
      if (is.null(out$stop_round)) out$stop_round <- chk$stop_round %||% NA_integer_
      if (is.null(out$stop_reason)) out$stop_reason <- chk$stop_reason %||% NA_character_

      validate_pairwise_run_output(out)
      return(.as_pairwise_run(out, run_type = "core_linking"))
    }
  }

  compute_fit <- function(res_tbl) {
    bt_data <- if (is.null(judge)) build_bt_fun(res_tbl) else build_bt_fun(res_tbl, judge = judge)
    out <- do.call(
      fit_fun,
      c(
        list(
          bt_data,
          engine = engine,
          verbose = fit_verbose,
          return_diagnostics = return_diagnostics,
          include_residuals = include_residuals
        ),
        .fit_dots
      )
    )

    # ---- normalize theta table schema ----
    # Downstream code (stopping + linking + running-fit bookkeeping) assumes
    # `ID`, `theta`, and `se` exist, even on edge paths where a fit returns an
    # empty theta table (e.g., no admissible pairs for a round).
    #
    # We accept missing `se` and fill it with NA, but `ID` and `theta` must be
    # present (or inferable) for non-empty tables.
    out$theta <- .as_theta_tibble(out$theta, arg_name = "fit_fun()$theta")
    if (!("se" %in% names(out$theta))) {
      out$theta$se <- NA_real_
    }

    # Defensive validation: downstream linking/metrics assume `$theta` exists.
    if (!is.list(out) || !"theta" %in% names(out) || is.null(out$theta)) {
      stop(
        "`fit_fun()` must return a fit list that contains a `$theta` tibble with columns `ID` and `theta`.",
        call. = FALSE
      )
    }

    # Attach bt_data for downstream diagnostics/debugging (and for reference
    # stabilization in core-linking workflows).
    out$bt_data <- bt_data

    # Orient the scale deterministically using observed win scores.
    orient <- .bt_orient_theta_by_wins(out$theta, bt_data)
    out$theta <- orient$theta
    out$orientation <- list(by = "wins", wins_cor = orient$cor, flipped = isTRUE(orient$flipped))

    out
  }

  apply_linking <- function(fit, reference_fit) {
    # Record drift *before* linking (used for diagnostics + auto-link decision).
    # Note: .bt_should_apply_linking expects drift columns with the `core_` prefix.
    drift_core <- bt_drift_metrics(fit, reference_fit, ids = core_ids, prefix = "core_")
    drift_linking <- drift_core
    names(drift_linking) <- sub("^core_", "linking_", names(drift_linking))

    fit$linking <- list(
      mode = linking,
      reference = "baseline",
      method = linking_method,
      applied = FALSE,
      reason = NA_character_,
      a = NA_real_,
      b = NA_real_,
      n_core = length(core_ids),
      min_n = linking_min_n,
      threshold_r = linking_cor_target,
      threshold_p90 = linking_p90_abs_shift_target,
      drift_pre = drift_linking,
      drift_post = NULL
    )

    if (isTRUE(linking == "never")) {
      fit$linking$reason <- "never"
      return(fit)
    }

    # Overlap on requested core IDs (informational only). Linking can still
    # proceed (and will fall back to an identity transform when overlap is
    # small).
    n_overlap <- drift_core$core_n
    fit$linking$n_overlap <- n_overlap

    if (isTRUE(linking == "auto")) {
      apply <- .bt_should_apply_linking(
        drift_tbl = drift_core,
        trigger_cor = linking_cor_target,
        trigger_p90_abs_shift = linking_p90_abs_shift_target,
        trigger_max_abs_shift = linking_max_abs_shift_target
      )
      fit$linking$reason <- if (isTRUE(apply)) "auto_trigger" else "auto_no_trigger"
      if (!isTRUE(apply)) {
        return(fit)
      }
    } else if (isTRUE(linking == "always")) {
      fit$linking$reason <- "always"
    } else {
      stop("Invalid `linking`. Expected one of: 'never', 'auto', 'always'.", call. = FALSE)
    }

    # Apply linking: attach theta_linked/se_linked to preserve the original
    # scale and provide linked values on the reference scale.
    lk <- bt_link_thetas(
      fit,
      reference_fit,
      ids = core_ids,
      method = linking_method,
      min_n = linking_min_n
    )

    fit$theta <- dplyr::left_join(
      fit$theta,
      dplyr::select(lk$theta, dplyr::all_of(c("ID", "theta_linked", "se_linked"))),
      by = "ID"
    )
    # Keep the original `theta`/`se` columns and add `theta_linked`/`se_linked`.
    # Downstream code can choose which scale to use.

    fit$linking$applied <- TRUE
    fit$linking$a <- lk$a
    fit$linking$b <- lk$b
    fit$linking$n_core <- lk$n_core

    # Drift after linking (diagnostic only)
    drift_post <- bt_drift_metrics(fit, reference_fit, ids = core_ids, prefix = "linking_post_")
    fit$linking$drift_post <- drift_post

    fit
  }

  make_running_fit <- function(bt_data, fit_bt, fit_engine_running, rc_smoothing, rc_damping, linking_mode) {
    # Silence R CMD check notes for NSE dplyr column references.
    ID <- NULL
    theta <- NULL
    se <- NULL
    # Normalize BT theta table (may be empty in no-pairs/no-results paths).
    theta_bt_src <- fit_bt$theta
    if (is.null(theta_bt_src)) {
      theta_bt_src <- tibble::tibble(ID = character(), theta = numeric(), se = numeric())
    }
    if (!("ID" %in% names(theta_bt_src))) {
      if (nrow(theta_bt_src) == 0L) {
        theta_bt_src <- tibble::tibble(ID = character(), theta = numeric(), se = numeric())
      } else {
        bt_stop_metrics("`fit_bt$theta` must contain column `ID`.")
      }
    }
    if (!("theta" %in% names(theta_bt_src))) {
      if (nrow(theta_bt_src) == 0L) {
        theta_bt_src$theta <- numeric()
      } else {
        bt_stop_metrics("`fit_bt$theta` must contain column `theta`.")
      }
    }
    if (!("se" %in% names(theta_bt_src))) theta_bt_src$se <- NA_real_

    theta_bt_linked <- NULL
    if ("theta_linked" %in% names(theta_bt_src)) {
      # Preserve ID even when empty.
      if (!("se_linked" %in% names(theta_bt_src))) theta_bt_src$se_linked <- NA_real_
      theta_bt_linked <- dplyr::transmute(
        theta_bt_src,
        ID = .data$ID,
        theta = .data$theta_linked,
        se = .data$se_linked
      )
    }

    theta_bt <- dplyr::transmute(
      theta_bt_src,
      ID = .data$ID,
      theta_bt = .data$theta,
      se_bt = .data$se
    )

    # Running theta table is either Rank Centrality or (linked) BT.
    engine <- match.arg(fit_engine_running, c("bt", "rank_centrality"))

    # Compute Rank Centrality only when requested (it may fail on degenerate/no-data BT tables).
    rc_fit <- NULL
    if (engine == "rank_centrality") {
      rc_fit <- tryCatch(
        fit_rank_centrality(bt_data, smoothing = rc_smoothing, damping = rc_damping),
        error = function(e) NULL
      )
    }

    theta_run <- if (engine == "rank_centrality") {
      tr <- rc_fit$theta %||% tibble::tibble(ID = character(), theta = numeric())
      tr <- tibble::as_tibble(tr)
      if (!("ID" %in% names(tr))) {
        if (nrow(tr) == 0L) tr$ID <- character() else stop("`fit_rank_centrality()` must return a theta table with column `ID`.", call. = FALSE)
      }
      if (!("theta" %in% names(tr))) {
        if (nrow(tr) == 0L) tr$theta <- numeric() else stop("`fit_rank_centrality()` must return a theta table with column `theta`.", call. = FALSE)
      }
      dplyr::transmute(tr, ID = as.character(.data$ID), theta = as.numeric(.data$theta), se = NA_real_)
    } else {
      src <- if (!is.null(theta_bt_linked)) theta_bt_linked else theta_bt_src
      src <- tibble::as_tibble(src)
      if (!("ID" %in% names(src))) {
        if (nrow(src) == 0L) src$ID <- character() else stop("`fit_bt$theta` must contain column `ID`.", call. = FALSE)
      }
      if (!("theta" %in% names(src))) {
        if (nrow(src) == 0L) src$theta <- numeric() else stop("`fit_bt$theta` must contain column `theta`.", call. = FALSE)
      }
      if (!("se" %in% names(src))) src$se <- NA_real_
      dplyr::transmute(src, ID = as.character(.data$ID), theta = as.numeric(.data$theta), se = as.numeric(.data$se))
    }

    theta_run <- tibble::as_tibble(theta_run)
    if (!("ID" %in% names(theta_run))) theta_run$ID <- character()

    theta_run <- dplyr::left_join(theta_run, theta_bt, by = "ID")
    if (!is.null(theta_bt_linked)) {
      theta_run <- dplyr::left_join(
        theta_run,
        dplyr::rename(theta_bt_linked, theta_bt_linked = theta, se_bt_linked = se),
        by = "ID"
      )
    }

    if (!is.null(rc_fit) && !is.null(rc_fit$pi) && length(rc_fit$pi) > 0L) {
      theta_run$pi_rc <- as.numeric(rc_fit$pi[match(theta_run$ID, names(rc_fit$pi))])
    } else {
      theta_run$pi_rc <- NA_real_
    }

    if (!is.null(rc_fit) && !is.null(rc_fit$theta) && is.data.frame(rc_fit$theta) && ("theta" %in% names(rc_fit$theta))) {
      theta_run$theta_rc <- as.numeric(rc_fit$theta$theta[match(theta_run$ID, rc_fit$theta$ID)])
    } else {
      theta_run$theta_rc <- NA_real_
    }

    theta_run$rank_running <- .rank_desc_numeric(theta_run$theta)
    theta_run$rank_bt <- if ("theta_bt" %in% names(theta_run)) .rank_desc_numeric(theta_run$theta_bt) else NA_integer_
    theta_run$rank_rc <- if ("theta_rc" %in% names(theta_run)) .rank_desc_numeric(theta_run$theta_rc) else NA_integer_

    list(
      engine_running = fit_engine_running,
      engine_bt = fit_bt$engine %||% NA_character_,
      engine_rc = "rank_centrality",
      reliability = fit_bt$reliability %||% NA_real_,
      theta = theta_run,
      diagnostics = fit_bt$diagnostics %||% list(),
      linking = fit_bt$linking %||% list(mode = linking_mode, reason = if (identical(linking_mode, "never")) "never" else NA_character_),
      bt_fit = fit_bt,
      rc_fit = rc_fit
    )
  }

  tag_fit <- function(fit, batch_index, round_index, stage, n_results, n_pairs_this_round, new_ids) {
    attr(fit, "bt_run_core_linking") <- list(
      batch_index = batch_index,
      round_index = round_index,
      stage = stage,
      n_results = n_results,
      n_pairs_this_round = n_pairs_this_round,
      n_new_ids = length(new_ids),
      new_ids = new_ids
    )
    fit
  }

  round_seed <- function(batch_i, round_i) {
    if (is.null(seed)) {
      return(NULL)
    }
    as.integer(seed) + as.integer(batch_i) * 10000L + as.integer(round_i)
  }


  # If resuming and checkpoint_store_fits was FALSE, recompute fits needed for drift/linking.
  # We intentionally keep checkpoints small by allowing fits to be recomputed from saved results.
  if (!is.null(resume_from) && !isTRUE(chk$completed)) {
    # Restore baseline_results_n if missing (best-effort fallback)
    if (is.na(baseline_results_n) || baseline_results_n < 0L) {
      baseline_results_n <- NA_integer_
    }
    if (is.null(baseline_fit) && nrow(results) > 0L) {
      n_base <- baseline_results_n
      if (is.na(n_base) || n_base <= 0L) n_base <- nrow(results)
      n_base <- min(n_base, nrow(results))
      if (n_base > 0L) {
        baseline_fit <- compute_fit(results[seq_len(n_base), , drop = FALSE])
        baseline_fit <- .bt_prepare_reference_fit(
          fit = baseline_fit,
          bt_data = baseline_fit$bt_data,
          core_ids = core_ids,
          scale_method = reference_scale_method,
          max_abs = reference_max_abs
        )
      }
    }
    if (is.null(current_fit) && nrow(results) > 0L) {
      current_fit <- compute_fit(results)
      if (!is.null(baseline_fit)) {
        current_fit <- apply_linking(current_fit, baseline_fit)
      }
      # tag for debugging; not stored unless checkpoint_store_fits=TRUE
      current_fit <- tag_fit(
        current_fit, batch_start - 1L, round_start - 1L, "resume_recompute",
        nrow(results), 0L, character(0)
      )

      if (is.null(current_fit_running) && !is.null(current_fit$bt_data)) {
        current_fit_running <- make_running_fit(
          bt_data = current_fit$bt_data,
          fit_bt = current_fit,
          fit_engine_running = fit_engine_running,
          rc_smoothing = rc_smoothing,
          rc_damping = rc_damping,
          linking_mode = linking
        )
        current_fit_running <- tag_fit(
          current_fit_running, batch_start - 1L, round_start - 1L, "resume_recompute",
          nrow(results), 0L, character(0)
        )
      }
    }
  }

  if (is.null(resume_from)) {
    fits <- list()
    final_fits <- list()
    metrics_hist <- tibble::tibble()
    state_hist <- tibble::tibble()
    batch_summary <- tibble::tibble()

    current_fit <- NULL
    current_fit_running <- NULL
    baseline_fit <- NULL
    baseline_results_n <- NA_integer_

    if (nrow(results) > 0L) {
      current_fit <- compute_fit(results)
      baseline_fit <- .bt_prepare_reference_fit(
        fit = current_fit,
        bt_data = current_fit$bt_data,
        core_ids = core_ids,
        scale_method = reference_scale_method,
        max_abs = reference_max_abs
      )
      baseline_results_n <- nrow(results)
      current_fit <- apply_linking(current_fit, baseline_fit)
      current_fit <- tag_fit(current_fit, 0L, 0L, "warm_start", nrow(results), 0L, character(0))

      current_fit_running <- make_running_fit(
        bt_data = current_fit$bt_data,
        fit_bt = current_fit,
        fit_engine_running = fit_engine_running,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping,
        linking_mode = linking
      )
      current_fit_running <- tag_fit(current_fit_running, 0L, 0L, "warm_start", nrow(results), 0L, character(0))

      fits[[length(fits) + 1L]] <- if (store_running_estimates) current_fit_running else current_fit
      final_fits[["bootstrap"]] <- current_fit

      st0 <- .bt_round_state(results, ids = ids_all, judge_col = judge)
      st0 <- dplyr::mutate(st0, batch_index = 0L, round_index = 0L, stage = "warm_start", stop_reason = NA_character_)
      state_hist <- dplyr::bind_rows(state_hist, st0)
    } else {
      fake_theta <- tibble::tibble(ID = ids_all, theta = rep(0, length(ids_all)), se = rep(1, length(ids_all)))
      boot <- bt_core_link_round(
        samples = samples,
        fit = list(theta = fake_theta),
        core_ids = core_ids,
        include_text = TRUE,
        new_ids = character(0),
        round_size = min(round_size, max(1L, length(core_ids))),
        within_batch_frac = 0,
        core_audit_frac = 1,
        k_neighbors = k_neighbors,
        min_judgments = min_judgments,
        existing_pairs = if (nrow(results) > 0L) results else NULL,
        forbid_repeats = forbid_repeats,
        balance_positions = balance_positions,
        seed = round_seed(0L, 1L)
      )

      if (nrow(boot$pairs) == 0L) {
        stop("Core bootstrap produced 0 pairs. Reduce constraints or increase core size.", call. = FALSE)
      }

      boot_res <- .coerce_judge_output(judge_fun(boot$pairs))
      boot_res <- .validate_judge_results(boot_res, ids = ids_all, judge_col = judge)

      boot_meta <- dplyr::distinct(
        dplyr::select(boot$pairs, dplyr::all_of(c("ID1", "ID2", "pair_type"))),
        dplyr::across(dplyr::all_of(c("ID1", "ID2"))),
        .keep_all = TRUE
      )
      boot_res <- dplyr::left_join(boot_res, boot_meta, by = c("ID1", "ID2"))
      boot_res <- dplyr::mutate(boot_res, batch_index = 0L, round_index = 1L)

      results <- dplyr::bind_rows(results, boot_res)
      current_fit <- compute_fit(results)
      baseline_fit <- .bt_prepare_reference_fit(
        fit = current_fit,
        bt_data = current_fit$bt_data,
        core_ids = core_ids,
        scale_method = reference_scale_method,
        max_abs = reference_max_abs
      )
      baseline_results_n <- nrow(results)
      current_fit <- apply_linking(current_fit, baseline_fit)
      current_fit <- tag_fit(current_fit, 0L, 1L, "bootstrap", nrow(results), nrow(boot$pairs), character(0))

      current_fit_running <- make_running_fit(
        bt_data = current_fit$bt_data,
        fit_bt = current_fit,
        fit_engine_running = fit_engine_running,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping,
        linking_mode = linking
      )
      current_fit_running <- tag_fit(current_fit_running, 0L, 1L, "bootstrap", nrow(results), nrow(boot$pairs), character(0))

      fits[[length(fits) + 1L]] <- if (store_running_estimates) current_fit_running else current_fit
      final_fits[["bootstrap"]] <- current_fit

      st0 <- .bt_round_state(results, ids = ids_all, judge_col = judge)
      st0 <- dplyr::mutate(st0, batch_index = 0L, round_index = 1L, stage = "bootstrap", stop_reason = NA_character_)
      state_hist <- dplyr::bind_rows(state_hist, st0)
    }

    seen_ids <- unique(core_ids)
  }

  .make_checkpoint_payload <- function(next_batch_index,
                                       next_round_index,
                                       in_batch,
                                       new_ids_current = NULL,
                                       prev_metrics_current = NULL,
                                       completed = FALSE,
                                       out = NULL) {
    seed_now <- NULL
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      seed_now <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    list(
      run_type = "core_linking",
      ids = ids_all,
      core_ids = core_ids,
      batches = batches,
      results = results,
      fit_engine_running = fit_engine_running,
      store_running_estimates = store_running_estimates,
      rc_smoothing = rc_smoothing,
      rc_damping = rc_damping,
      final_refit = final_refit,
      final_bt_bias_reduction = final_bt_bias_reduction,
      fits = if (isTRUE(checkpoint_store_fits)) fits else NULL,
      final_fits = if (isTRUE(checkpoint_store_fits)) final_fits else NULL,
      metrics = metrics_hist,
      state = state_hist,
      batch_summary = batch_summary,
      current_fit = if (isTRUE(checkpoint_store_fits)) current_fit else NULL,
      current_fit_running = if (isTRUE(checkpoint_store_fits)) current_fit_running else NULL,
      baseline_fit = if (isTRUE(checkpoint_store_fits)) baseline_fit else NULL,
      baseline_results_n = as.integer(baseline_results_n %||% NA_integer_),
      seen_ids = seen_ids,
      within_batch_frac = within_batch_frac,
      core_audit_frac = core_audit_frac,
      next_batch_index = as.integer(next_batch_index),
      next_round_index = as.integer(next_round_index),
      in_batch = isTRUE(in_batch),
      new_ids_current = new_ids_current,
      prev_metrics_current = prev_metrics_current,
      random_seed = seed_now,
      completed = isTRUE(completed),
      out = out
    )
  }

  .write_checkpoint_now <- function(payload, batch_i = NULL, round_i = NULL) {
    if (is.null(checkpoint_dir) || !nzchar(checkpoint_dir)) {
      return(invisible(NULL))
    }
    .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    if (!is.null(batch_i) && !is.null(round_i)) {
      # encode batch/round in the per-round snapshot index for easier browsing
      snap_round <- as.integer(batch_i) * 1000L + as.integer(round_i)
      .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", round = snap_round, overwrite = checkpoint_overwrite)
    }
    invisible(NULL)
  }

  if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
    on.exit(
      {
        if (!is.null(checkpoint_payload_last)) {
          .bt_write_checkpoint(checkpoint_dir, checkpoint_payload_last, basename = "run_state", overwrite = checkpoint_overwrite)
        }
      },
      add = TRUE
    )
  }

  batch_seq <- if (batch_start <= length(batches)) seq.int(from = batch_start, to = length(batches)) else integer(0)
  for (batch_i in batch_seq) {
    batch_ids <- unique(as.character(batches[[batch_i]]))
    new_ids <- setdiff(batch_ids, seen_ids)

    if (isTRUE(verbose)) {
      message("Batch ", batch_i, ": ", length(new_ids), " new IDs (", length(batch_ids), " requested).")
    }

    stop_reason <- NA_character_
    rounds_used <- 0L
    prev_metrics <- NULL
    prev_fit_for_stability <- NULL
    stability_streak <- 0L
    prev_fit_for_stability <- NULL
    stability_streak <- 0L

    resuming_this_batch <- isTRUE(resume_in_batch) && batch_i == batch_start
    if (resuming_this_batch) {
      if (!is.null(resume_new_ids)) {
        new_ids <- unique(as.character(resume_new_ids))
      }
      prev_metrics <- resume_prev_metrics
      rounds_used <- max(0L, as.integer(round_start) - 1L)
    }

    if (length(new_ids) == 0L) {
      stop_reason <- .bt_resolve_stop_reason(no_new_ids = TRUE)
      batch_summary <- dplyr::bind_rows(
        batch_summary,
        tibble::tibble(
          batch_index = batch_i,
          n_requested = length(batch_ids),
          n_new = 0L,
          rounds_used = 0L,
          stop_reason = stop_reason,
          linking_mode = if (!is.null(current_fit$linking)) current_fit$linking$mode else NA_character_,
          linking_method = if (!is.null(current_fit$linking)) current_fit$linking$method else NA_character_,
          linking_applied = if (!is.null(current_fit$linking)) isTRUE(current_fit$linking$applied) else NA,
          linking_reason = if (!is.null(current_fit$linking)) current_fit$linking$reason else NA_character_,
          linking_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift_pre)) {
            d <- current_fit$linking$drift_pre
            if (is.data.frame(d) && nrow(d) > 0L && "linking_p90_abs_shift" %in% names(d)) as.numeric(d$linking_p90_abs_shift[[1]]) else NA_real_
          } else {
            NA_real_
          },
          linking_post_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift_post)) {
            d <- current_fit$linking$drift_post
            if (is.data.frame(d) && nrow(d) > 0L && "linking_post_p90_abs_shift" %in% names(d)) as.numeric(d$linking_post_p90_abs_shift[[1]]) else NA_real_
          } else {
            NA_real_
          }
        )
      )
      final_fits[[paste0("batch_", batch_i)]] <- current_fit
      next
    }

    round_i_from <- if (resuming_this_batch) as.integer(round_start) else 1L
    if (is.na(round_i_from) || round_i_from < 1L) round_i_from <- 1L
    round_seq <- if (round_i_from <= max_rounds_per_batch) seq.int(from = round_i_from, to = max_rounds_per_batch) else integer(0)
    for (round_i in round_seq) {
      rounds_used <- round_i

      theta_for_pairs <- current_fit_running$theta %||% current_fit$theta
      if (!is.null(theta_for_pairs)) {
        # bt_core_link_round expects ID/theta/se (it ignores extra columns).
        theta_for_pairs <- dplyr::select(theta_for_pairs, dplyr::all_of(intersect(c("ID", "theta", "se"), names(theta_for_pairs))))
      }
      if (is.null(theta_for_pairs) || !all(c("ID", "theta", "se") %in% names(theta_for_pairs))) {
        theta_for_pairs <- tibble::tibble(ID = ids_all, theta = rep(0, length(ids_all)), se = rep(1, length(ids_all)))
      }

      within_batch_frac_this <- within_batch_frac
      core_audit_frac_this <- core_audit_frac

      round_out <- bt_core_link_round(
        samples = samples,
        fit = list(theta = theta_for_pairs),
        core_ids = core_ids,
        include_text = TRUE,
        new_ids = new_ids,
        round_size = round_size,
        within_batch_frac = within_batch_frac_this,
        core_audit_frac = core_audit_frac_this,
        k_neighbors = k_neighbors,
        min_judgments = min_judgments,
        existing_pairs = if (nrow(results) > 0L) results else NULL,
        forbid_repeats = forbid_repeats,
        balance_positions = balance_positions,
        seed = round_seed(batch_i, round_i)
      )

      pairs <- round_out$pairs

      # If we are running out of admissible within-batch pairs, optionally
      # expand the candidate pool to keep the batch moving.
      if (exhaustion_fallback != "none") {
        pairs <- .bt_apply_exhaustion_fallback(
          pairs = pairs,
          samples = samples,
          core_ids = core_ids,
          new_ids = new_ids,
          seen_ids = seen_ids,
          round_size = round_size,
          exhaustion_fallback = exhaustion_fallback,
          exhaustion_min_pairs_frac = exhaustion_min_pairs_frac,
          exhaustion_spectral_gap_threshold = exhaustion_spectral_gap_threshold,
          within_batch_frac = within_batch_frac_this,
          core_audit_frac = core_audit_frac_this,
          k_neighbors = k_neighbors,
          min_judgments = min_judgments,
          include_text = TRUE,
          forbid_repeats = forbid_repeats,
          balance_positions = balance_positions,
          seed = round_seed(batch_i, round_i),
          verbose = verbose
        )
      }

      if (nrow(pairs) == 0L) {
        stop_reason <- .bt_resolve_stop_reason(no_pairs = TRUE)
        break
      }

      res_round <- .coerce_judge_output(judge_fun(pairs))
      res_round <- .validate_judge_results(res_round, ids = ids_all, judge_col = judge)

      pair_meta <- dplyr::distinct(
        dplyr::select(pairs, dplyr::all_of(c("ID1", "ID2", "pair_type"))),
        dplyr::across(dplyr::all_of(c("ID1", "ID2"))),
        .keep_all = TRUE
      )

      res_round <- dplyr::left_join(res_round, pair_meta, by = c("ID1", "ID2"))
      res_round <- dplyr::mutate(
        res_round,
        # Back-compat: older tests expect `batch_i`/`round_i`.
        batch_i = batch_i,
        round_i = round_i,
        batch_index = batch_i,
        round_index = round_i
      )
      results <- dplyr::bind_rows(results, res_round)

      prev_fit_for_drift <- NULL
      if (drift_reference == "previous_round") {
        prev_fit_for_drift <- current_fit
      } else if (drift_reference == "baseline") {
        prev_fit_for_drift <- baseline_fit
      }

      current_fit <- compute_fit(results)
      # Link to the baseline core scale (optionally, based on drift).
      current_fit <- apply_linking(current_fit, baseline_fit)
      # Backwards-compatible stage label used throughout the package/tests.
      current_fit <- tag_fit(current_fit, as.integer(batch_i), as.integer(round_i), "round", nrow(results), nrow(pairs), new_ids)

      current_fit_running <- make_running_fit(
        bt_data = current_fit$bt_data,
        fit_bt = current_fit,
        fit_engine_running = fit_engine_running,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping,
        linking_mode = linking
      )
      current_fit_running <- tag_fit(current_fit_running, as.integer(batch_i), as.integer(round_i), "round", nrow(results), nrow(pairs), new_ids)

      fits[[length(fits) + 1L]] <- if (store_running_estimates) current_fit_running else current_fit

      st_all <- .bt_round_state(results, ids = ids_all, judge_col = judge)
      st_new <- .bt_round_state(results, ids = new_ids, judge_col = judge, prefix = "new_")
      st <- dplyr::bind_cols(st_all, st_new)
      st <- dplyr::mutate(st, batch_index = as.integer(batch_i), round_index = as.integer(round_i), stage = "round", stop = FALSE, stop_reason = NA_character_, n_new_ids = as.integer(length(new_ids)))
      state_hist <- dplyr::bind_rows(state_hist, st)

      ids_for_metrics <- intersect(new_ids, current_fit$theta$ID)
      if (length(ids_for_metrics) == 0L) {
        ids_for_metrics <- NULL
      }
      metrics <- bt_stop_metrics(
        fit = current_fit,
        ids = ids_for_metrics,
        prev_fit = prev_fit_for_drift,
        core_ids = core_ids,
        se_probs = se_probs,
        fit_bounds = fit_bounds,
        stability_topk = stop_topk,
        stability_topk_ties = stop_topk_ties,
        stability_seed = if (is.null(seed)) NULL else (as.integer(seed) + as.integer(batch_i) * 1000L + as.integer(round_i))
      )

      # Counts used by adaptive runner (kept here so metrics schemas match across runners).
      metrics$n_pairs_total <- st$n_unique_unordered_pairs_in_ids
      metrics$n_pairs_new <- st$new_n_unique_unordered_pairs_in_ids
      metrics$n_missing_better_id <- st$n_missing_better_id

      # Add bookkeeping / allocation columns (do NOT pass these into bt_stop_metrics())
      metrics <- dplyr::mutate(
        metrics,
        batch_index = batch_i,
        round_index = round_i,
        stage = "round",
        within_batch_frac = within_batch_frac_this,
        core_audit_frac = core_audit_frac_this,
        n_pairs_proposed = nrow(pairs),
        n_core_new = sum(pairs$pair_type == "core_new"),
        n_new_new = sum(pairs$pair_type == "new_new"),
        n_core_core = sum(pairs$pair_type == "core_core"),
        core_n = as.integer(length(core_ids))
      )


      # Add linking diagnostics (drift-to-baseline pre/post linking + parameters).
      if (!is.null(current_fit$linking) && is.list(current_fit$linking)) {
        lk <- current_fit$linking
        lk_chr <- function(x) if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1]])
        lk_num <- function(x) if (is.null(x) || length(x) == 0L) NA_real_ else as.numeric(x[[1]])
        lk_int <- function(x) if (is.null(x) || length(x) == 0L) NA_integer_ else as.integer(x[[1]])

        metrics <- dplyr::mutate(
          metrics,
          linking_mode = lk_chr(lk$mode),
          linking_reference = lk_chr(lk$reference),
          linking_method = lk_chr(lk$method),
          linking_applied = isTRUE(lk$applied),
          linking_reason = lk_chr(lk$reason),
          linking_a = lk_num(lk$a),
          linking_b = lk_num(lk$b),
          linking_n_core = lk_int(lk$n_core),
          linking_min_n = lk_int(lk$min_n),
          linking_threshold_r = lk_num(lk$threshold_r),
          linking_threshold_p90 = lk_num(lk$threshold_p90)
        )

        if (!is.null(lk$drift_pre) && is.data.frame(lk$drift_pre) && nrow(lk$drift_pre) > 0L) {
          dr <- as.data.frame(lk$drift_pre)[1, , drop = FALSE]
          dr$ids <- NULL
          for (nm in names(dr)) metrics[[nm]] <- dr[[nm]][1]
        }
        if (!is.null(lk$drift_post) && is.data.frame(lk$drift_post) && nrow(lk$drift_post) > 0L) {
          drp <- as.data.frame(lk$drift_post)[1, , drop = FALSE]
          drp$ids <- NULL
          for (nm in names(drp)) metrics[[nm]] <- drp[[nm]][1]
        }
      }

      # A3: ensure both naming schemes exist (core_* and linking_*)
      # (Requires .bt_add_drift_aliases() to exist; see helper below.)
      metrics <- .bt_add_drift_aliases(metrics)
      prev_metrics_for_state <- prev_metrics


      # ---- PR7/PR8.0: graph health + stability (stopping is gated by graph health) ----
      # Use active IDs only so that unintroduced/future sample IDs do not create
      # isolated nodes that block soft stopping.
      ids_graph <- .active_ids_for_graph(
        core_ids = core_ids,
        batches = batches,
        batch_i = batch_i,
        results_so_far = results
      )
      gs <- .graph_state_from_pairs(results, ids = ids_graph)
      gm <- gs$metrics
      degree_min <- as.double(gm$degree_min)
      largest_component_frac <- as.double(gm$largest_component_frac)

      graph_healthy <- isTRUE(degree_min >= as.double(stop_min_degree)) &&
        isTRUE(largest_component_frac >= as.double(stop_min_largest_component_frac))

      stability_pass <- FALSE
      if (!is.null(prev_fit_for_stability)) {
        stability_pass <- is.finite(metrics$rms_theta_delta) &&
          metrics$rms_theta_delta <= as.double(stop_stability_rms) &&
          is.finite(metrics$topk_overlap) &&
          metrics$topk_overlap >= as.double(stop_topk_overlap)
      }

      if (isTRUE(stability_pass)) stability_streak <- as.integer(stability_streak) + 1L else stability_streak <- 0L
      stability_reached <- isTRUE(stability_streak >= as.integer(stop_stability_consecutive))

      metrics$degree_min <- as.double(degree_min)
      metrics$largest_component_frac <- as.double(largest_component_frac)
      metrics$stability_streak <- as.integer(stability_streak)
      metrics$graph_healthy <- isTRUE(graph_healthy)
      metrics$stability_pass <- isTRUE(stability_pass)
      metrics <- .bt_order_metrics(metrics)

      # Ensure a consistent superset schema before stopping logic.
      metrics <- .bt_align_metrics(metrics, se_probs = se_probs)

      metrics_hist <- dplyr::bind_rows(metrics_hist, metrics)


      stop_dec <- do.call(
        bt_should_stop,
        c(list(metrics = metrics, prev_metrics = prev_metrics), stop_params)
      )

      # ---- PR8.2.3: drift guardrails must block stability soft-stops ----
      # bt_should_stop() already gates its own precision/stability soft stop
      # decisions on drift, but core-linking also has an independent stability
      # detector (rms/topk) used for batched stopping. When drift guardrails are
      # active and failing, we must not stop on that stability signal.
      drift_blocking <- FALSE
      if (!is.null(stop_dec$details) && is.data.frame(stop_dec$details)) {
        det <- tibble::as_tibble(stop_dec$details)
        if (all(c("criterion", "threshold", "pass") %in% names(det))) {
          drift_criteria <- c(
            "core_theta_cor",
            "core_theta_spearman",
            "core_max_abs_shift",
            "core_p90_abs_shift"
          )
          det_drift <- det[det$criterion %in% drift_criteria & !is.na(det$threshold), , drop = FALSE]
          if (nrow(det_drift) > 0L) {
            pass_vec <- as.logical(det_drift$pass)
            pass_vec[is.na(pass_vec)] <- FALSE
            drift_blocking <- any(!pass_vec)
          }
        }
      }

      prev_metrics <- metrics
      prev_fit_for_stability <- current_fit

      if (!isTRUE(stop_dec$stop) && !is.null(allocation_fun)) {
        alloc_state <- list(
          stage = "round",
          batch_index = as.integer(batch_i),
          round_index = as.integer(round_i),
          within_batch_frac = within_batch_frac_this,
          core_audit_frac = core_audit_frac_this,
          metrics = metrics,
          prev_metrics = prev_metrics_for_state,
          stop = stop_dec,
          n_results_total = nrow(results),
          n_pairs_proposed = nrow(pairs),
          n_new_ids = length(new_ids),
          new_ids = new_ids,
          core_ids = core_ids,
          plan = round_out$plan
        )
        alloc <- .bt_apply_allocation_fun(
          allocation_fun = allocation_fun,
          state = alloc_state,
          within_batch_frac = within_batch_frac,
          core_audit_frac = core_audit_frac
        )
        within_batch_frac <- alloc$within_batch_frac
        core_audit_frac <- alloc$core_audit_frac
      }

      if (isTRUE(verbose)) {
        msg <- paste0(
          "  Round ", round_i,
          ": n_pairs=", nrow(pairs),
          ", rel_se_p90=", if ("rel_se_p90" %in% names(metrics)) round(metrics$rel_se_p90, 3) else NA
        )
        message(msg)
      }

      # checkpoint after completing this round (safe point)
      checkpoint_payload_last <- .make_checkpoint_payload(
        next_batch_index = as.integer(batch_i),
        next_round_index = as.integer(round_i + 1L),
        in_batch = TRUE,
        new_ids_current = new_ids,
        prev_metrics_current = prev_metrics,
        completed = FALSE
      )
      if ((as.integer(round_i) %% checkpoint_every) == 0L) {
        .write_checkpoint_now(checkpoint_payload_last, batch_i = batch_i, round_i = round_i)
      }
      # ---- PR7: deterministic stop decision + explicit reasons ----
      stability_reached_gated <- isTRUE(stability_reached) && !isTRUE(drift_blocking)
      precision_reached_gated <- isTRUE(stop_dec$stop) && !isTRUE(drift_blocking)
      stop_chk <- .stop_decision(
        round = round_i,
        min_rounds = min_rounds,
        no_new_pairs = (nrow(pairs) == 0L),
        budget_exhausted = (as.integer(round_size) == 0L),
        max_rounds_reached = (as.integer(round_i) > as.integer(max_rounds_per_batch)), # PR8.2.2: max-round is a post-loop fallback label

        graph_healthy = graph_healthy,
        stability_reached = stability_reached_gated,
        precision_reached = precision_reached_gated,
        stop_reason_priority = stop_reason_priority
      )
      .validate_stop_decision(stop_chk)

      # Optional explainability: record when drift guardrails block soft stops.
      # These fields are NOT stop reasons; they are diagnostics only.
      blocked_by <- character(0)
      blocked_candidates <- character(0)

      # Graph gating can already mark stop_blocked_* in stop_chk$details.
      if (!is.null(stop_chk$details) && is.list(stop_chk$details)) {
        if (!is.null(stop_chk$details$stop_blocked_by) && !is.na(stop_chk$details$stop_blocked_by)) {
          blocked_by <- c(blocked_by, as.character(stop_chk$details$stop_blocked_by))
        }
        if (!is.null(stop_chk$details$stop_blocked_candidates) && !is.na(stop_chk$details$stop_blocked_candidates)) {
          bc <- strsplit(as.character(stop_chk$details$stop_blocked_candidates), "\\|", fixed = FALSE)[[1]]
          bc <- bc[nzchar(bc)]
          blocked_candidates <- c(blocked_candidates, bc)
        }
      }

      if (isTRUE(drift_blocking) && as.integer(round_i) >= as.integer(min_rounds)) {
        drift_candidates <- character(0)

        # Drift blocking always blocks the runner-level stability signal.
        if (isTRUE(stability_reached) && isTRUE(graph_healthy)) {
          drift_candidates <- c(drift_candidates, "stability_reached")
        }

        # If bt_should_stop() would have stopped except for drift, mark precision.
        # We approximate this by requiring all non-drift active criteria to pass,
        # and at least one of the precision/stability SE criteria (if active) to pass.
        would_stop_except_drift <- FALSE
        if (!is.null(stop_dec$details) && is.data.frame(stop_dec$details)) {
          det <- tibble::as_tibble(stop_dec$details)
          if (all(c("criterion", "threshold", "pass") %in% names(det))) {
            drift_criteria <- c(
              "core_theta_cor",
              "core_theta_spearman",
              "core_max_abs_shift",
              "core_p90_abs_shift"
            )
            prec_crit <- c("rel_se_p90_precision", "rel_se_p90_stability")
            det_non_drift <- det[!(det$criterion %in% c(drift_criteria, prec_crit)), , drop = FALSE]
            if (nrow(det_non_drift) == 0L) {
              non_drift_active_fail <- FALSE
            } else {
              pass_vec <- as.logical(det_non_drift$pass)
              pass_vec[is.na(pass_vec)] <- FALSE
              non_drift_active_fail <- any(!is.na(det_non_drift$threshold) & !pass_vec)
            }

            det_prec <- det[det$criterion == "rel_se_p90_precision", , drop = FALSE]
            det_stab <- det[det$criterion == "rel_se_p90_stability", , drop = FALSE]

            precision_active <- nrow(det_prec) > 0L && !is.na(det_prec$threshold[[1]])
            stability_active <- nrow(det_stab) > 0L && !is.na(det_stab$threshold[[1]])
            precision_pass <- nrow(det_prec) > 0L && isTRUE(as.logical(det_prec$pass[[1]]))
            stability_pass_stop <- nrow(det_stab) > 0L && isTRUE(as.logical(det_stab$pass[[1]]))

            pass_precision_or_stability <-
              (!precision_active && !stability_active) ||
                (precision_active && precision_pass) ||
                (stability_active && stability_pass_stop)

            would_stop_except_drift <- isTRUE(pass_precision_or_stability) && !isTRUE(non_drift_active_fail)
          }
        }
        if (isTRUE(would_stop_except_drift) && isTRUE(graph_healthy)) {
          drift_candidates <- c(drift_candidates, "precision_reached")
        }

        if (length(drift_candidates) > 0L) {
          blocked_by <- c(blocked_by, "drift_guardrails")
          blocked_candidates <- c(blocked_candidates, drift_candidates)
        }
      }

      if (!("stop_blocked_by" %in% names(state_hist))) {
        state_hist$stop_blocked_by <- NA_character_
      }
      if (!("stop_blocked_candidates" %in% names(state_hist))) {
        state_hist$stop_blocked_candidates <- NA_character_
      }

      if (length(blocked_by) > 0L) {
        blocked_by <- blocked_by[!is.na(blocked_by) & nzchar(blocked_by)]
        # preserve order while de-duplicating
        blocked_by <- blocked_by[!duplicated(blocked_by)]
        state_hist[nrow(state_hist), "stop_blocked_by"] <- paste(blocked_by, collapse = "|")
      }
      if (length(blocked_candidates) > 0L) {
        blocked_candidates <- blocked_candidates[!is.na(blocked_candidates) & nzchar(blocked_candidates)]
        blocked_candidates <- blocked_candidates[!duplicated(blocked_candidates)]
        state_hist[nrow(state_hist), "stop_blocked_candidates"] <- paste(blocked_candidates, collapse = "|")
      }

      if (isTRUE(stop_chk$stop)) {
        stop_reason <- stop_chk$reason
        state_hist[nrow(state_hist), "stop_reason"] <- stop_reason
        break
      }
    }

    # PR8.2.2: Treat max-round as a post-loop fallback label so stability/precision
    # can win on the final allowed round.
    if (is.na(stop_reason)) {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      if (nrow(state_hist) > 0L) {
        idx <- which(state_hist$batch_index == batch_i)
        if (length(idx) > 0L) state_hist[idx[length(idx)], "stop_reason"] <- stop_reason
      }
    }

    seen_ids <- unique(c(seen_ids, new_ids))

    batch_summary <- dplyr::bind_rows(
      batch_summary,
      tibble::tibble(
        batch_index = batch_i,
        n_requested = length(batch_ids),
        n_new = length(new_ids),
        rounds_used = rounds_used,
        stop_reason = stop_reason,
        linking_mode = if (!is.null(current_fit$linking)) current_fit$linking$mode else NA_character_,
        linking_method = if (!is.null(current_fit$linking)) current_fit$linking$method else NA_character_,
        linking_applied = if (!is.null(current_fit$linking)) isTRUE(current_fit$linking$applied) else NA,
        linking_reason = if (!is.null(current_fit$linking)) current_fit$linking$reason else NA_character_,
        linking_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift_pre)) {
          d <- current_fit$linking$drift_pre
          if (is.data.frame(d) && nrow(d) > 0L && "linking_p90_abs_shift" %in% names(d)) as.numeric(d$linking_p90_abs_shift[[1]]) else NA_real_
        } else {
          NA_real_
        },
        linking_post_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift_post)) {
          d <- current_fit$linking$drift_post
          if (is.data.frame(d) && nrow(d) > 0L && "linking_post_p90_abs_shift" %in% names(d)) as.numeric(d$linking_post_p90_abs_shift[[1]]) else NA_real_
        } else {
          NA_real_
        }
      )
    )

    final_fits[[paste0("batch_", batch_i)]] <- current_fit

    # checkpoint at batch boundary (next batch starts at round 1)
    checkpoint_payload_last <- .make_checkpoint_payload(
      next_batch_index = as.integer(batch_i + 1L),
      next_round_index = 1L,
      in_batch = FALSE,
      new_ids_current = NULL,
      prev_metrics_current = NULL,
      completed = FALSE
    )
    .write_checkpoint_now(checkpoint_payload_last)

    # once we successfully progress past a resumed batch, treat future batches as fresh
    resume_in_batch <- FALSE
    round_start <- 1L
  }


  # Derive appearance quantile aliases used by adaptive runner (if absent)
  if (nrow(state_hist) > 0L) {
    if (!("appear_p50" %in% names(state_hist)) && ("median_appearances" %in% names(state_hist))) {
      state_hist$appear_p50 <- state_hist$median_appearances
    } else if (("appear_p50" %in% names(state_hist)) && ("median_appearances" %in% names(state_hist))) {
      state_hist$appear_p50 <- dplyr::coalesce(state_hist$appear_p50, state_hist$median_appearances)
    }

    if (!("appear_p90" %in% names(state_hist)) && ("p90_appearances" %in% names(state_hist))) {
      state_hist$appear_p90 <- state_hist$p90_appearances
    } else if (("appear_p90" %in% names(state_hist)) && ("p90_appearances" %in% names(state_hist))) {
      state_hist$appear_p90 <- dplyr::coalesce(state_hist$appear_p90, state_hist$p90_appearances)
    }

    if (!("new_appear_p50" %in% names(state_hist)) && ("new_median_appearances" %in% names(state_hist))) {
      state_hist$new_appear_p50 <- state_hist$new_median_appearances
    } else if (("new_appear_p50" %in% names(state_hist)) && ("new_median_appearances" %in% names(state_hist))) {
      state_hist$new_appear_p50 <- dplyr::coalesce(state_hist$new_appear_p50, state_hist$new_median_appearances)
    }

    if (!("new_appear_p90" %in% names(state_hist)) && ("new_p90_appearances" %in% names(state_hist))) {
      state_hist$new_appear_p90 <- state_hist$new_p90_appearances
    } else if (("new_appear_p90" %in% names(state_hist)) && ("new_p90_appearances" %in% names(state_hist))) {
      state_hist$new_appear_p90 <- dplyr::coalesce(state_hist$new_appear_p90, state_hist$new_p90_appearances)
    }

    # Mark stop rows where a stop_reason has been recorded
    if (!("stop" %in% names(state_hist))) state_hist$stop <- FALSE
    state_hist$stop <- ifelse(is.na(state_hist$stop_reason), FALSE, TRUE)
  }

  # Ensure standardized metrics + state schema (superset across runners)
  metrics_hist <- .bt_align_metrics(metrics_hist, se_probs = se_probs)
  state_hist <- .bt_align_state(state_hist)

  bt_data <- NULL
  estimates <- NULL
  final_models <- NULL

  # PR8 contract fields: ensure these always exist
  theta <- NULL
  theta_engine <- NA_character_
  fit_provenance <- list()

  if (nrow(results) > 0L) {
    bt_data <- build_bt_fun(results, judge)
  }
  if (isTRUE(final_refit) && nrow(results) > 0L) {
    final_out <- compute_final_estimates(
      results = results,
      ids = ids_all,
      bt_bias_reduction = final_bt_bias_reduction,
      rc_smoothing = rc_smoothing,
      rc_damping = rc_damping
    )
    estimates <- final_out$estimates
    final_models <- list(
      bt = final_out$bt_fit,
      rc = final_out$rc_fit,
      diagnostics = final_out$diagnostics
    )
    theta <- NULL
    theta_engine <- NA_character_
    fit_provenance <- list()

    if (!is.null(final_out$estimates) && nrow(final_out$estimates) > 0L) {
      bt_ok <- identical(final_out$diagnostics$bt_status, "succeeded")

      theta_col <- if (bt_ok) "theta_bt_firth" else "theta_rc"
      se_col <- if (bt_ok) "se_bt_firth" else NA_character_

      theta <- final_out$estimates %>%
        dplyr::transmute(
          ID = as.character(.data$ID),
          theta = as.double(.data[[theta_col]]),
          se = if (is.na(se_col)) as.double(NA_real_) else as.double(.data[[se_col]]),
          rank = .rank_desc(.data[[theta_col]])
        )

      theta_engine <- if (bt_ok) as.character(final_out$diagnostics$bt_engine_used) else "rank_centrality"

      fit_provenance <- final_out$provenance
      if (is.null(fit_provenance)) fit_provenance <- list()
    }
  }

  # PR8.2.4: If no final estimates were produced, expose the last running fit as
  # `theta` so that meaningful-fit ⇒ theta exists (even when final_refit = FALSE).
  if (is.null(theta) && length(final_fits) > 0L) {
    last_fit <- final_fits[[length(final_fits)]]
    fb <- .theta_from_last_running_fit(last_fit, id_vec = ids_all)
    if (!is.null(fb$theta)) {
      theta <- fb$theta
      theta_engine <- fb$engine

      if (is.null(fit_provenance)) fit_provenance <- list()
      fit_provenance$fallback_used <- TRUE
      fit_provenance$fallback_source <- "last_running_fit"
      fit_provenance$fallback_reason <- if (isTRUE(final_refit)) "final_refit_unavailable" else "final_refit_disabled"
    }
  }

  # PR8 contract: fit_provenance must always be a list (possibly empty).
  if (is.null(fit_provenance)) fit_provenance <- list()

  out <- list(
    core_ids = core_ids,
    batches = batches,
    results = results,
    bt_data = bt_data,
    fits = fits,
    final_fits = final_fits,
    estimates = estimates,
    theta = theta,
    theta_engine = theta_engine,
    fit_provenance = fit_provenance,
    pairing_diagnostics = NULL,
    final_models = final_models,
    metrics = metrics_hist,
    state = state_hist,
    batch_summary = batch_summary,
    # Top-level stopping summary (batch-level stop reasons are in `batch_summary`).
    # For batched runners, we surface the *final batch's* stop_reason as the overall stop_reason.
    stop_reason = if (nrow(batch_summary) == 0L) NA_character_ else as.character(batch_summary$stop_reason[[nrow(batch_summary)]]),
    stop_round = if (nrow(batch_summary) == 0L) NA_integer_ else as.integer(sum(batch_summary$rounds_used, na.rm = TRUE))
  )

  # final checkpoint with full return object
  if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
    checkpoint_payload_last <- .make_checkpoint_payload(
      next_batch_index = as.integer(length(batches) + 1L),
      next_round_index = 1L,
      in_batch = FALSE,
      new_ids_current = NULL,
      prev_metrics_current = NULL,
      completed = TRUE,
      out = out
    )
    .write_checkpoint_now(checkpoint_payload_last)
  }

  validate_pairwise_run_output(out)
  .as_pairwise_run(out, run_type = "core_linking")
}

# ---- internal ----

.normalize_batches_list <- function(batches, ids_all) {
  if (!is.list(batches) || length(batches) < 1L) {
    stop("`batches` must be a non-empty list of character vectors.", call. = FALSE)
  }

  out <- vector("list", length(batches))
  for (i in seq_along(batches)) {
    b <- unique(as.character(batches[[i]]))
    if (length(b) < 1L) stop("Each batch must contain at least one ID.", call. = FALSE)
    if (anyNA(b) || any(b == "")) stop("Batch IDs must be non-missing and non-empty.", call. = FALSE)
    if (!all(b %in% ids_all)) stop("All batch IDs must be present in `samples$ID`.", call. = FALSE)
    out[[i]] <- b
  }
  out
}
