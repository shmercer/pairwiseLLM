#' Run an adaptive core-linking BT workflow end-to-end
#'
#' This exported runner implements the "hybrid" workflow that combines:
#' \itemize{
#'   \item a stable core linking bank (anchors),
#'   \item adding new samples in batches, and
#'   \item round-based adaptive pair selection within each batch.
#' }
#'
#' The loop for each batch is:
#' \enumerate{
#' \item Propose a round of pairs using \code{\link{bt_core_link_round}} (core\eqn{\leftrightarrow}new linking,
#'   optional new\eqn{\leftrightarrow}new within-batch comparisons, and optional core\eqn{\leftrightarrow}core audit pairs).
#' \item Score those pairs via \code{judge_fun} and append results.
#' \item Fit a BT model (default: \code{\link{fit_bt_model}}).
#' \item Compute stopping metrics on the batch's new IDs via \code{\link{bt_stop_metrics}}
#'   and decide whether to stop via \code{\link{bt_should_stop}}.
#' \item Repeat until stopping criteria are met, no new pairs can be proposed, or
#'   \code{max_rounds_per_batch} is reached.
#' }
#'
#' If \code{core_ids} is \code{NULL}, the core set can be selected from \code{samples}
#' using \code{\link{select_core_set}}.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}. \code{ID}
#' must be unique and non-missing.
#'
#' @param batches A list defining the batches of new IDs to add. Each element should
#' be a character vector of IDs. A single character vector is treated as one batch.
#' IDs must be present in \code{samples$ID}.
#'
#' @param judge_fun A function that accepts a tibble of pairs with columns
#' \code{ID1}, \code{text1}, \code{ID2}, \code{text2} and returns a tibble with
#' columns \code{ID1}, \code{ID2}, and \code{better_id}. If \code{judge} is provided,
#' the returned tibble must also include that judge column.
#'
#' @param core_ids Optional character vector of core IDs. If \code{NULL}, core IDs are
#' selected using \code{core_method} and sizing arguments.
#'
#' @param core_method Method passed to \code{\link{select_core_set}} when \code{core_ids}
#' is \code{NULL}. One of \code{"auto"}, \code{"pam"}, \code{"clara"},
#' \code{"embeddings"} (alias for \code{"auto"}), \code{"token_stratified"},
#' or \code{"random"}.
#' @param core_size Optional integer core size passed to \code{\link{select_core_set}}.
#' @param core_pct Optional numeric core proportion passed to \code{\link{select_core_set}}.
#' @param embeddings Optional embedding matrix passed to \code{\link{select_core_set}} when
#' using embeddings-based core selection (\code{core_method} in
#' \code{c("auto","pam","clara","embeddings")}).
#' @param embeddings_metric Distance metric for embeddings selection. Passed to
#' \code{\link{select_core_set}}.
#' @param linking Whether to apply anchoring/linking so theta estimates are reported
#'   on a stable scale defined by the baseline core fit. One of
#'   \code{"auto"}, \code{"always"}, or \code{"never"}. In \code{"auto"},
#'   linking is applied only when core drift exceeds the thresholds below.
#' @param linking_method Linking method passed to [bt_link_thetas()].
#'   \itemize{
#'     \item \code{"mean_sd"}: match mean and SD on the core set.
#'     \item \code{"median_iqr"}: match median and a robust SD estimate based on IQR.
#'     \item \code{"median_mad"}: match median and a robust SD estimate based on MAD.
#'   }
#'   Robust methods are strongly recommended for real-world adaptive runs because
#'   early rounds can be close to deterministic (separation).
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
#'   fit is close to deterministic (separation), which can otherwise cause linking
#'   scale factors to explode.
#' @param reference_max_abs Maximum absolute value allowed for reference thetas after
#'   stabilization (clamping). This is applied only to the reference fit used for
#'   linking/drift diagnostics.
#' @param seed_core Optional integer seed for reproducible core selection.
#'
#' @param initial_results Optional tibble/data.frame of already-scored pairs with columns
#' \code{ID1}, \code{ID2}, \code{better_id} (and optional judge column). These results are
#' used as the starting state.
#'
#' @param judge Optional character scalar giving the name of the column in results that
#' identifies the judge/backend/model.
#'
#' @param engine Character scalar passed to \code{fit_fun} as its \code{engine} argument.
#' Default \code{"sirt"}.
#' @param fit_verbose Logical; passed to \code{fit_fun} as \code{verbose}. Default \code{FALSE}.
#' @param return_diagnostics Logical; passed to \code{fit_fun}. Default \code{TRUE}.
#' @param include_residuals Logical; passed to \code{fit_fun}. Default \code{FALSE}.
#'
#' @param fit_engine_running Running fitting engine used to propose the *next* round
#'   of pairs. One of \code{"bt"} (use BT thetas; default) or \code{"rank_centrality"}
#'   (use Rank Centrality scores as the running theta while retaining BT standard
#'   errors for uncertainty-driven sampling).
#' @param store_running_estimates Logical; if TRUE, store the running-fit object
#'   in \code{out$fits}. If FALSE, store the BT fit.
#' @param rc_smoothing Numeric smoothing parameter forwarded to
#'   \code{\link{fit_rank_centrality}} when \code{fit_engine_running="rank_centrality"}
#'   or when final refit computes Rank Centrality. Default \code{0.5}.
#' @param rc_damping Numeric damping/teleport parameter in \code{[0,1)} forwarded to
#'   \code{\link{fit_rank_centrality}}. Use \code{>0} for unique stationary
#'   distributions on disconnected graphs. Default \code{0}.
#' @param final_refit Logical; if TRUE (default), compute final BT and Rank Centrality
#'   estimates on the full result set via \code{\link{compute_final_estimates}} and
#'   return them as \code{out$estimates}.
#' @param final_bt_bias_reduction Logical; if TRUE (default), attempt bias-reduced
#'   BT fitting (Firth / br=TRUE) in the final refit (falls back to MLE if unavailable).
#'
#'
#' @param round_size Integer. Number of new pairs to propose and score per round within each
#' batch.
#' @param init_round_size Integer. Number of bootstrap pairs to score on the core set before
#' processing batches, when no \code{initial_results} are supplied. Default \code{round_size}.
#' @param max_rounds_per_batch Integer. Maximum number of rounds per batch. Default \code{50}.
#'
#' @param within_batch_frac Numeric in \code{[0,1]}. Fraction of non-audit pairs allocated to new\eqn{\leftrightarrow}new
#' within-batch comparisons (passed to \code{\link{select_core_link_pairs}}).
#' @param core_audit_frac Numeric in \code{[0,1]}. Fraction of pairs allocated to core\eqn{\leftrightarrow}core audits
#'   (passed to \code{\link{select_core_link_pairs}}).
#' @param allocation Allocation preset controlling how within-batch and auditing
#'   fractions may be adjusted between rounds. One of `"fixed"`, `"precision_ramp"`,
#'   or `"audit_on_drift"`. If `allocation_fun` is supplied, it takes precedence
#'   and `allocation` is ignored.
#' @param allocation_fun Optional function to update `within_batch_frac` and/or `core_audit_frac`
#'   between rounds. It is called after metrics are computed each round with a state list and
#'   should return NULL (no change) or a list with elements `within_batch_frac` and/or
#'   `core_audit_frac`.
#'
#' @param k_neighbors Integer. Passed to \code{\link{select_core_link_pairs}}.
#' @param min_judgments Integer. Passed to \code{\link{select_core_link_pairs}}.
#' @param forbid_repeats Logical. Passed to \code{\link{select_core_link_pairs}}.
#' @param balance_positions Logical. Passed to \code{\link{select_core_link_pairs}}.
#'
#' @param seed Optional integer alias for `seed_pairs` (backwards compatibility).
#'   Prefer `seed_pairs` going forward.
#' @param seed_pairs Optional integer seed used for bootstrap pair sampling and for each round
#' as \code{seed_pairs + batch_index*1000 + round_index}. RNG state is restored afterward.
#'
#' @param se_probs Numeric vector of probabilities in (0,1) for SE quantiles (passed to
#' \code{\link{bt_stop_metrics}}).
#' @param fit_bounds Numeric length-2 vector giving acceptable infit/outfit bounds
#'   (infit/outfit) passed to \code{\link{bt_stop_metrics}}.
#' @param stopping_tier Preset stopping thresholds to use (\code{"good"},
#'   \code{"strong"}, \code{"very_strong"}). Passed to
#'   \code{\link{bt_stop_metrics}}.
#'
#' @param reliability_target,sepG_target,rel_se_p90_target,rel_se_p90_min_improve,max_item_misfit_prop,max_judge_misfit_prop
#' Stopping thresholds passed to \code{\link{bt_should_stop}}.
#'
#' @param exhaustion_fallback Fallback strategy to use when the within-batch pair
#'   generator becomes exhausted and fewer than \code{round_size * exhaustion_min_pairs_frac}
#'   pairs are available. One of \code{"none"}, \code{"cross_batch_new_new"},
#'   \code{"targeted_repeats"}, or \code{"both"}.
#' @param exhaustion_min_pairs_frac Numeric in (0,1]. Minimum fraction of \code{round_size}
#'   below which the fallback may trigger.
#' @param exhaustion_spectral_gap_threshold Numeric >= 0. Optional diagnostic threshold for
#'   triggering the fallback when spectral gap information is available.
#'
#' @param core_theta_cor_target,core_theta_spearman_target,core_max_abs_shift_target,core_p90_abs_shift_target
#' Optional drift guardrails passed to \code{\link{bt_should_stop}}. If any of these are not
#' \code{NA}, the runner computes core drift metrics per round by comparing the current fit to
#' the prior batch's final fit (or the bootstrap fit for the first batch).
#'
#' @param fit_fun Function used to fit the BT model. Default \code{\link{fit_bt_model}}.
#' Primarily intended as a test hook.
#' @param build_bt_fun Function used to convert results into BT data. Default
#' \code{\link{build_bt_data}}. Primarily intended as a test hook.
#'
#' @param checkpoint_dir Optional directory path for writing checkpoint files during
#'   the run. If provided, the runner writes \code{run_state.rds} (and optionally
#'   per-round snapshot files) after completed rounds and/or batch boundaries. Use
#'   this to resume long jobs after interruption or errors.
#'
#' @param resume_from Optional directory path containing a prior checkpoint file
#'   \code{run_state.rds} created by \code{bt_run_adaptive_core_linking()}. When
#'   provided, the run resumes from the saved state, including accumulated results,
#'   batch/round indices, and stopping/metrics history. The \code{samples}, batch
#'   definitions, and \code{core_ids} must be compatible with the checkpoint.
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
#' @param ... Additional arguments passed through to \code{fit_fun}.
#'
#' @details
#' \strong{Checkpointing and resuming:} If \code{checkpoint_dir} is provided, this
#' function writes a checkpoint at the last completed safe point (typically after a
#' round completes within a batch, and at batch boundaries). If an error occurs
#' mid-round, the checkpoint reflects the most recently completed round. Resume by
#' calling again with \code{resume_from = checkpoint_dir}.
#'
#' @return A list with:
#' \describe{
#'   \item{core_ids}{Core linking IDs used.}
#'   \item{batches}{Normalized batches list.}
#'   \item{results}{All judged results (canonicalized \code{better_id}).}
#'   \item{bt_data}{BT data built from \code{results}.}
#'   \item{fits}{List of per-round fits (including the initial bootstrap fit, if any). Each
#'     fit is tagged with per-round metadata in the
#'     \code{attr(fit, "bt_run_adaptive_core_linking")} attribute.}
#'   \item{final_fits}{Named list of final fit per batch (plus \code{"bootstrap"} when
#'     applicable).}
#'   \item{metrics}{Tibble of stop metrics per round (computed on each batch's new IDs).}
#'   \item{batch_summary}{One row per batch: rounds used, stop reason, counts.}
#'   \item{state}{A tibble with one row per scoring round containing bookkeeping summaries of
#'     accumulated results (overall and for the current batch's new IDs). New-ID fields are
#'     prefixed with \code{new_}. Rows include \code{batch_index}, \code{round_index}, and
#'     \code{stage}.}
#' }
#'
#' @examples
#' # Simple simulated judge: higher true theta wins
#' samples <- tibble::tibble(
#'   ID = LETTERS[1:8],
#'   text = paste0("t", LETTERS[1:8])
#' )
#' true_theta <- stats::setNames(seq(2, -1.5, length.out = 8), samples$ID)
#'
#' judge_fun <- function(pairs) {
#'   b <- ifelse(true_theta[pairs$ID1] >= true_theta[pairs$ID2], pairs$ID1, pairs$ID2)
#'   tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = b)
#' }
#'
#' # Mock BT fitter: theta = centered win counts, se = 1/sqrt(judgments)
#' fit_fun <- function(bt_data, ...) {
#'   bt_data <- tibble::as_tibble(bt_data)
#'   ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
#'   wins <- stats::setNames(rep(0L, length(ids)), ids)
#'   n_j <- stats::setNames(rep(0L, length(ids)), ids)
#'   for (i in seq_len(nrow(bt_data))) {
#'     a <- bt_data$object1[[i]]
#'     b <- bt_data$object2[[i]]
#'     r <- bt_data$result[[i]]
#'     if (isTRUE(is.finite(r))) {
#'       if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
#'       n_j[a] <- n_j[a] + 1L
#'       n_j[b] <- n_j[b] + 1L
#'     }
#'   }
#'   theta <- as.numeric(wins - stats::median(wins))
#'   se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
#'   list(
#'     engine = "mock",
#'     reliability = 0.95,
#'     theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
#'     diagnostics = list(sepG = 3.5)
#'   )
#' }
#'
#' out <- bt_run_adaptive_core_linking(
#'   samples = samples,
#'   batches = list(c("G", "H")),
#'   judge_fun = judge_fun,
#'   core_ids = c("A", "B", "C"),
#'   fit_fun = fit_fun,
#'   engine = "mock",
#'   round_size = 6,
#'   init_round_size = 6,
#'   max_rounds_per_batch = 2,
#'   rel_se_p90_target = 0.7,
#'   reliability_target = NA_real_,
#'   sepG_target = NA_real_,
#'   rel_se_p90_min_improve = NA_real_,
#'   max_item_misfit_prop = NA_real_,
#'   max_judge_misfit_prop = NA_real_
#' )
#' out$batch_summary
#'
#' @export
bt_run_adaptive_core_linking <- function(samples,
                                         batches,
                                         judge_fun,
                                         core_ids = NULL,
                                         core_method = c("auto", "pam", "clara", "embeddings", "token_stratified", "random"),
                                         core_size = NULL,
                                         core_pct = NULL,
                                         embeddings = NULL,
                                         embeddings_metric = c("cosine", "euclidean"),
                                         linking = c("auto", "always", "never"),
                                         linking_method = c("median_iqr", "median_mad", "mean_sd"),
                                         linking_cor_target = 0.98,
                                         linking_p90_abs_shift_target = 0.15,
                                         linking_max_abs_shift_target = 0.30,
                                         linking_min_n = 3L,
                                         reference_scale_method = c("median_iqr", "median_mad", "mean_sd"),
                                         reference_max_abs = 6,
                                         seed_core = NULL,
                                         initial_results = NULL,
                                         judge = NULL,
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
                                         init_round_size = round_size,
                                         max_rounds_per_batch = 50,
                                         within_batch_frac = 0.25,
                                         core_audit_frac = 0.05,
                                         allocation = c("fixed", "precision_ramp", "audit_on_drift"),
                                         allocation_fun = NULL,
                                         k_neighbors = 10,
                                         min_judgments = 12,
                                         forbid_repeats = TRUE,
                                         balance_positions = TRUE,
                                         seed = NULL,
                                         seed_pairs = NULL,
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
                                         checkpoint_dir = NULL,
                                         resume_from = NULL,
                                         checkpoint_every = 1L,
                                         checkpoint_store_fits = TRUE,
                                         checkpoint_overwrite = TRUE,
                                         fit_fun = fit_bt_model,
                                         build_bt_fun = function(results, judge = NULL, ...) {
                                           build_bt_data(results, ...)
                                         },
                                         ...) {
  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }
  ids_all <- as.character(samples$ID)
  if (length(ids_all) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids_all) || any(ids_all == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids_all))) stop("`samples$ID` must be unique.", call. = FALSE)

  if (!is.function(judge_fun)) {
    stop("`judge_fun` must be a function.", call. = FALSE)
  }

  if (!is.null(allocation_fun) && !is.function(allocation_fun)) {
    stop("`allocation_fun` must be a function or NULL.", call. = FALSE)
  }

  stopping_tier <- match.arg(stopping_tier)
  stop_params <- bt_stop_tiers()[[stopping_tier]]

  fit_engine_running <- match.arg(fit_engine_running)
  store_running_estimates <- isTRUE(store_running_estimates)
  final_refit <- isTRUE(final_refit)
  final_bt_bias_reduction <- isTRUE(final_bt_bias_reduction)

  exhaustion_fallback <- match.arg(exhaustion_fallback)
  if (!is.numeric(exhaustion_min_pairs_frac) || length(exhaustion_min_pairs_frac) != 1L ||
    is.na(exhaustion_min_pairs_frac) || exhaustion_min_pairs_frac < 0 || exhaustion_min_pairs_frac > 1) {
    stop("`exhaustion_min_pairs_frac` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(exhaustion_spectral_gap_threshold) || length(exhaustion_spectral_gap_threshold) != 1L ||
    is.na(exhaustion_spectral_gap_threshold) || exhaustion_spectral_gap_threshold < 0) {
    stop("`exhaustion_spectral_gap_threshold` must be a single non-negative number.", call. = FALSE)
  }

  # Capture and sanitize `...` forwarded to fit_fun. This prevents collisions like
  # `verbose` (often intended for runner logging) being passed twice to fit_fun.
  .fit_dots <- list(...)
  if (!is.null(.fit_dots$verbose) && missing(fit_verbose)) {
    fit_verbose <- isTRUE(.fit_dots$verbose)
  }
  .fit_dots <- .clean_fit_dots(.fit_dots)

  # Back-compat: accept `seed=` as an alias for `seed_pairs=`.
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be a single integer (or NULL).", call. = FALSE)
    }
    if (is.null(seed_pairs)) {
      seed_pairs <- seed
    } else {
      seed_pairs_i <- as.integer(seed_pairs)
      if (length(seed_pairs_i) != 1L || is.na(seed_pairs_i)) {
        stop("`seed_pairs` must be a single integer (or NULL).", call. = FALSE)
      }
      if (!identical(seed_pairs_i, seed)) {
        stop("Do not supply both `seed` and `seed_pairs` (they must be identical).", call. = FALSE)
      }
    }
  }

  round_size <- as.integer(round_size)
  if (length(round_size) != 1L || is.na(round_size) || round_size < 0L) {
    stop("`round_size` must be a non-negative integer.", call. = FALSE)
  }
  init_round_size <- as.integer(init_round_size)
  if (length(init_round_size) != 1L || is.na(init_round_size) || init_round_size < 0L) {
    stop("`init_round_size` must be a non-negative integer.", call. = FALSE)
  }
  max_rounds_per_batch <- as.integer(max_rounds_per_batch)
  if (length(max_rounds_per_batch) != 1L || is.na(max_rounds_per_batch) || max_rounds_per_batch < 0L) {
    stop("`max_rounds_per_batch` must be a non-negative integer.", call. = FALSE)
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

  checkpoint_every <- as.integer(checkpoint_every)
  if (is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(resume_from)) {
    resume_from <- as.character(resume_from)
    if (length(resume_from) != 1L || !nzchar(resume_from)) stop("`resume_from` must be a non-empty string.", call. = FALSE)
    if (is.null(checkpoint_dir)) checkpoint_dir <- resume_from
  }
  if (!is.null(checkpoint_dir)) {
    checkpoint_dir <- as.character(checkpoint_dir)
    if (length(checkpoint_dir) != 1L || !nzchar(checkpoint_dir)) stop("`checkpoint_dir` must be a non-empty string.", call. = FALSE)
  }

  # ---- Internal helper: build a "running fit" object that includes BOTH BT + RC columns ----
  # NOTE: NSE-free (base + tibble only) to avoid R CMD check NOTES.
  make_running_fit <- function(bt_data, fit_bt, fit_engine_running, rc_smoothing, rc_damping) {
    if (is.null(fit_bt) || is.null(fit_bt$theta)) {
      stop("make_running_fit(): fit_bt is missing $theta.", call. = FALSE)
    }

    bt_theta <- tibble::as_tibble(fit_bt$theta)
    if (!("ID" %in% names(bt_theta))) stop("make_running_fit(): fit_bt$theta missing ID.", call. = FALSE)
    if (!("theta" %in% names(bt_theta))) stop("make_running_fit(): fit_bt$theta missing theta.", call. = FALSE)
    if (!("se" %in% names(bt_theta))) bt_theta$se <- NA_real_

    ids <- as.character(bt_theta$ID)
    theta_bt <- as.numeric(bt_theta$theta)
    se_bt <- as.numeric(bt_theta$se)

    theta_bt_linked <- theta_bt
    if ("theta_linked" %in% names(bt_theta)) {
      theta_bt_linked <- as.numeric(bt_theta$theta_linked)
    }

    rc_fit <- fit_rank_centrality(
      bt_data = bt_data,
      ids = ids,
      smoothing = rc_smoothing,
      damping = rc_damping,
      verbose = FALSE
    )
    rc_theta <- tibble::as_tibble(rc_fit$theta)

    m <- match(ids, rc_theta$ID)
    theta_rc <- as.numeric(rc_theta$theta[m])
    pi_rc <- as.numeric(rc_theta$pi[m])

    if (identical(fit_engine_running, "rank_centrality")) {
      theta_running <- theta_rc
      engine_running <- "rank_centrality"
    } else {
      theta_running <- theta_bt_linked
      engine_running <- "bt"
    }

    theta_out <- tibble::tibble(
      ID = ids,
      theta = theta_running,
      se = se_bt,
      theta_bt = theta_bt,
      theta_bt_linked = theta_bt_linked,
      se_bt = se_bt,
      theta_rc = theta_rc,
      pi_rc = pi_rc
    )

    fit_running <- list(
      engine = fit_bt$engine %||% "bt",
      engine_running = engine_running,
      reliability = fit_bt$reliability %||% NA_real_,
      theta = theta_out,
      diagnostics = fit_bt$diagnostics %||% list(),
      bt_fit = fit_bt,
      rc_fit = rc_fit
    )
    fit_running
  }

  # Normalize batches
  if (is.character(batches)) {
    batches <- list(batches)
  }
  if (!is.list(batches) || length(batches) < 1L) {
    stop("`batches` must be a non-empty list (or a character vector).", call. = FALSE)
  }
  batches <- lapply(batches, function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & x != ""]
    unique(x)
  })
  if (any(vapply(batches, length, integer(1)) == 0L)) {
    stop("Each batch in `batches` must contain at least 1 ID.", call. = FALSE)
  }
  all_batch_ids <- unique(unlist(batches, use.names = FALSE))
  if (!all(all_batch_ids %in% ids_all)) {
    stop("All batch IDs must be present in `samples$ID`.", call. = FALSE)
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

  # Core selection
  if (is.null(core_ids)) {
    core_method <- match.arg(core_method)

    allocation <- match.arg(allocation)
    if (is.null(allocation_fun) && allocation != "fixed") {
      allocation_fun <- switch(allocation,
        precision_ramp = allocation_precision_ramp(),
        audit_on_drift = allocation_audit_on_drift(),
        NULL
      )
    }

    embeddings_metric <- match.arg(embeddings_metric)

    core_args <- list(
      samples = samples,
      method = core_method,
      embeddings = embeddings,
      distance = embeddings_metric,
      seed = seed_core
    )
    if (!is.null(core_size)) core_args$core_size <- core_size
    if (!is.null(core_pct)) core_args$core_pct <- core_pct

    core_tbl <- do.call(select_core_set, core_args)
    core_ids <- as.character(core_tbl$ID)
  } else {
    core_ids <- as.character(core_ids)
  }

  if (length(core_ids) < 2L) stop("`core_ids` must include at least 2 IDs.", call. = FALSE)
  if (anyNA(core_ids) || any(core_ids == "")) stop("`core_ids` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(core_ids))) stop("`core_ids` must be unique.", call. = FALSE)
  if (!all(core_ids %in% ids_all)) stop("All `core_ids` must be present in `samples$ID`.", call. = FALSE)

  # Results accumulator
  results <- if (is.null(initial_results)) {
    tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
  } else {
    tibble::as_tibble(initial_results)
  }

  if (nrow(results) > 0L) {
    results <- .validate_judge_results(results, ids = ids_all, judge_col = judge)
  }

  # Early exit if no sampling and no initial results
  if (is.null(resume_from) && nrow(results) == 0L && init_round_size == 0L && round_size == 0L) {
    bt_data <- build_bt_fun(results, judge = judge)
    out <- list(
      core_ids = core_ids,
      batches = batches,
      results = results,
      bt_data = bt_data,
      fits = list(),
      final_fits = list(),
      metrics = .bt_align_metrics(tibble::tibble(), se_probs = se_probs),
      batch_summary = tibble::tibble(),
      state = .bt_align_state(tibble::tibble()),
      stop_reason = .bt_resolve_stop_reason(round_size_zero = TRUE),
      stop_round = 0L,
      stop_batch = 0L,
      engine = engine
    )

    if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
      payload <- list(
        run_type = "adaptive_core_linking",
        ids = ids_all,
        core_ids = core_ids,
        batches = batches,
        timestamp = Sys.time(),
        completed = TRUE,
        out = out
      )
      .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    }

    return(.as_pairwise_run(out, run_type = "adaptive_core_linking"))
  }

  # Internal helper: fit on current results
  fit_from_results <- function(res) {
    bt_data <- build_bt_fun(res, judge = judge)
    if (nrow(bt_data) == 0L) {
      return(list(bt_data = bt_data, fit = NULL))
    }
    fit <- do.call(
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

    if (is.null(fit) || !is.list(fit)) {
      return(list(bt_data = bt_data, fit = NULL))
    }

    if (is.null(fit$theta)) {
      bt_ids <- sort(unique(c(as.character(bt_data$object1), as.character(bt_data$object2))))
      fit$theta <- tibble::tibble(
        ID = bt_ids,
        theta = rep(NA_real_, length(bt_ids)),
        se = rep(NA_real_, length(bt_ids))
      )
    } else {
      if (!"se" %in% names(fit$theta)) {
        fit$theta$se <- NA_real_
      }
    }

    orient <- .bt_orient_theta_by_wins(fit$theta, bt_data)
    fit$theta <- orient$theta
    fit$orientation <- list(by = "wins", wins_cor = orient$cor, flipped = isTRUE(orient$flipped))

    list(bt_data = bt_data, fit = fit)
  }

  apply_linking <- function(fit, reference_fit) {
    if (is.null(fit) || is.null(reference_fit)) {
      return(fit)
    }

    if (is.null(fit$theta) || is.null(reference_fit$theta) ||
      !is.data.frame(fit$theta) || !("theta" %in% names(fit$theta)) ||
      all(is.na(fit$theta$theta))) {
      fit$linking <- list(
        mode = linking,
        reference = "baseline",
        method = linking_method,
        applied = FALSE,
        reason = "missing_theta"
      )
      return(fit)
    }

    drift_core <- bt_drift_metrics(fit, reference_fit, ids = core_ids, prefix = "core_")
    drift_linking <- dplyr::rename_with(drift_core, ~ sub("^core_", "linking_", .x))

    fit$linking <- list(
      mode = linking,
      reference = "baseline",
      method = linking_method,
      applied = FALSE,
      reason = "not_applied",
      a = NA_real_,
      b = NA_real_,
      n_core = length(core_ids),
      min_n = linking_min_n,
      threshold_r = linking_cor_target,
      threshold_p90 = linking_p90_abs_shift_target,
      drift = drift_linking,
      drift_core = drift_core,
      drift_post = NULL
    )

    if (isTRUE(linking == "never")) {
      fit$linking$reason <- "never"
      return(fit)
    }

    n_overlap <- drift_core$core_n[[1]]
    fit$linking$n_overlap <- n_overlap

    do_apply <- FALSE
    if (isTRUE(linking == "auto")) {
      apply <- .bt_should_apply_linking(
        drift_tbl = drift_core,
        trigger_cor = linking_cor_target,
        trigger_p90_abs_shift = linking_p90_abs_shift_target,
        trigger_max_abs_shift = linking_max_abs_shift_target
      )
      do_apply <- isTRUE(apply)
      fit$linking$reason <- if (isTRUE(do_apply)) "auto_trigger" else "auto_no_trigger"
    } else if (isTRUE(linking == "always")) {
      do_apply <- TRUE
      fit$linking$reason <- "always"
    } else {
      stop("Invalid `linking`. Expected one of: 'never', 'auto', 'always'.", call. = FALSE)
    }

    if (!isTRUE(do_apply)) {
      return(fit)
    }

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

    fit$linking$applied <- TRUE
    fit$linking$a <- lk$a
    fit$linking$b <- lk$b
    fit$linking$n_core <- lk$n_core

    fit_post <- fit
    if (all(c("theta_linked", "se_linked") %in% names(fit_post$theta))) {
      fit_post$theta$theta <- fit_post$theta$theta_linked
      fit_post$theta$se <- fit_post$theta$se_linked
    }
    drift_post <- bt_drift_metrics(fit_post, reference_fit, ids = core_ids, prefix = "linking_post_")
    fit$linking$drift_post <- drift_post

    fit
  }

  # Internal helper: bookkeeping state
  state_row <- function(res, new_ids, batch_index, round_index, stage, stop, stop_reason) {
    res <- tibble::as_tibble(res)
    if (nrow(res) == 0L) {
      out <- tibble::tibble(
        batch_index = batch_index,
        round_index = round_index,
        stage = stage,
        stop = stop,
        stop_reason = stop_reason,
        n_new_ids = as.integer(length(new_ids)),
        n_results = 0L,
        n_unique_unordered_pairs = 0L,
        appear_p50 = NA_real_,
        appear_p90 = NA_real_,
        appear_p95 = NA_real_,
        pos_imbalance_max = 0L,
        n_self_pairs = 0L,
        n_missing_better_id = 0L,
        n_judges = if (is.null(judge)) NA_integer_ else 0L,
        new_n_unique_unordered_pairs = 0L,
        new_appear_p50 = NA_real_,
        new_appear_p90 = NA_real_,
        new_appear_p95 = NA_real_,
        new_pos_imbalance_max = 0L,
        new_n_missing_better_id = 0L
      )
      return(out)
    }

    key_all <- .unordered_pair_key(res$ID1, res$ID2)
    n_unique <- length(unique(key_all))
    app <- as.integer(table(factor(c(res$ID1, res$ID2), levels = ids_all)))
    q <- function(x, p) {
      if (length(x) == 0L || all(is.na(x))) {
        return(NA_real_)
      }
      as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7))
    }
    pos1 <- as.integer(table(factor(res$ID1, levels = ids_all)))
    pos2 <- as.integer(table(factor(res$ID2, levels = ids_all)))
    imb <- pos1 - pos2

    n_judges <- if (is.null(judge)) NA_integer_ else length(unique(as.character(res[[judge]])))

    idx_new_row <- (res$ID1 %in% new_ids) | (res$ID2 %in% new_ids)
    res_new <- res[idx_new_row, , drop = FALSE]
    key_new <- if (nrow(res_new) == 0L) character(0) else .unordered_pair_key(res_new$ID1, res_new$ID2)
    n_unique_new <- length(unique(key_new))

    app_new <- if (length(new_ids) == 0L) {
      integer(0)
    } else {
      as.integer(table(factor(c(res_new$ID1, res_new$ID2), levels = new_ids)))
    }
    pos1_new <- if (length(new_ids) == 0L) integer(0) else as.integer(table(factor(res_new$ID1, levels = new_ids)))
    pos2_new <- if (length(new_ids) == 0L) integer(0) else as.integer(table(factor(res_new$ID2, levels = new_ids)))
    imb_new <- pos1_new - pos2_new

    tibble::tibble(
      batch_index = batch_index,
      round_index = round_index,
      stage = stage,
      stop = stop,
      stop_reason = stop_reason,
      n_results = nrow(res),
      n_unique_unordered_pairs = n_unique,
      appear_p50 = q(app, 0.5),
      appear_p90 = q(app, 0.9),
      appear_p95 = q(app, 0.95),
      pos_imbalance_max = max(abs(imb), na.rm = TRUE),
      n_self_pairs = sum(res$ID1 == res$ID2),
      n_missing_better_id = sum(is.na(res$better_id) | res$better_id == ""),
      n_judges = n_judges,
      new_n_unique_unordered_pairs = n_unique_new,
      new_appear_p50 = q(app_new, 0.5),
      new_appear_p90 = q(app_new, 0.9),
      new_appear_p95 = q(app_new, 0.95),
      new_pos_imbalance_max = if (length(imb_new) == 0L) 0L else max(abs(imb_new), na.rm = TRUE),
      new_n_missing_better_id = sum(is.na(res_new$better_id) | res_new$better_id == "")
    )
  }

  drift_active <- any(!is.na(c(
    core_theta_cor_target,
    core_theta_spearman_target,
    core_max_abs_shift_target,
    core_p90_abs_shift_target
  )))

  # ---- resume / checkpoint state ----
  checkpoint_payload_last <- NULL
  batch_start <- 1L
  round_start <- 1L
  resume_in_batch <- FALSE
  resume_new_ids <- NULL
  resume_prev_metrics <- NULL

  # Stateful objects that must exist in all control-flow paths
  fits <- list()
  final_fits <- list()
  metrics_rows <- list()
  state_rows <- list()
  batch_summary <- tibble::tibble()
  current_fit <- NULL
  current_fit_running <- NULL
  bootstrap_fit <- NULL
  baseline_fit <- NULL
  baseline_results_n <- NA_integer_
  seen_ids <- core_ids

  if (!is.null(resume_from)) {
    chk <- .bt_read_checkpoint(resume_from)
    .bt_validate_checkpoint(chk, run_type = "adaptive_core_linking", ids = ids_all)

    if (!is.null(chk$core_ids)) {
      if (!setequal(as.character(chk$core_ids), core_ids)) {
        .abort_checkpoint_mismatch(
          field = "core_ids",
          expected = core_ids,
          actual = chk$core_ids,
          hint = "If you changed `core_ids` between runs, restart without `resume_from`."
        )
      }
    }
    if (!is.null(chk$batches)) {
      if (!setequal(unique(unlist(chk$batches, use.names = FALSE)), all_batch_ids)) {
        .abort_checkpoint_mismatch(
          field = "batches",
          expected = batches,
          actual = chk$batches,
          hint = "If you changed `batches` between runs, restart without `resume_from`."
        )
      }
    }

    if (!is.null(chk$random_seed)) {
      try(assign(".Random.seed", chk$random_seed, envir = .GlobalEnv), silent = TRUE)
    }

    results <- tibble::as_tibble(chk$results)
    fits <- chk$fits %||% list()
    final_fits <- chk$final_fits %||% list()
    metrics_rows <- chk$metrics_rows %||% list()
    state_rows <- chk$state_rows %||% list()
    batch_summary <- chk$batch_summary %||% tibble::tibble()
    current_fit <- chk$current_fit %||% NULL
    bootstrap_fit <- chk$bootstrap_fit %||% NULL
    baseline_fit <- chk$baseline_fit %||% NULL
    baseline_results_n <- as.integer(chk$baseline_results_n %||% NA_integer_)
    seen_ids <- chk$seen_ids %||% core_ids
    within_batch_frac <- chk$within_batch_frac %||% within_batch_frac
    core_audit_frac <- chk$core_audit_frac %||% core_audit_frac
    batch_start <- as.integer(chk$next_batch_index %||% 1L)
    round_start <- as.integer(chk$next_round_index %||% 1L)
    resume_in_batch <- isTRUE(chk$in_batch)
    resume_new_ids <- chk$new_ids_current %||% NULL
    resume_prev_metrics <- chk$prev_metrics_current %||% NULL

    if (isTRUE(chk$completed) && !is.null(chk$out)) {
      return(.as_pairwise_run(chk$out, run_type = "adaptive_core_linking"))
    }
  }

  .make_checkpoint_payload <- function(next_batch_index,
                                       next_round_index,
                                       in_batch,
                                       new_ids_current = NULL,
                                       prev_metrics_current = NULL,
                                       completed = FALSE,
                                       out = NULL) {
    list(
      run_type = "adaptive_core_linking",
      ids = ids_all,
      core_ids = core_ids,
      batches = batches,
      timestamp = Sys.time(),
      completed = completed,
      next_batch_index = as.integer(next_batch_index),
      next_round_index = as.integer(next_round_index),
      in_batch = isTRUE(in_batch),
      new_ids_current = new_ids_current,
      prev_metrics_current = prev_metrics_current,
      within_batch_frac = within_batch_frac,
      core_audit_frac = core_audit_frac,
      results = results,
      fits = if (isTRUE(checkpoint_store_fits)) fits else NULL,
      final_fits = if (isTRUE(checkpoint_store_fits)) final_fits else NULL,
      metrics_rows = metrics_rows,
      state_rows = state_rows,
      batch_summary = batch_summary,
      current_fit = current_fit,
      bootstrap_fit = bootstrap_fit,
      baseline_fit = baseline_fit,
      seen_ids = seen_ids,
      random_seed = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL,
      out = out
    )
  }

  .write_checkpoint_now <- function(payload, batch_i = NULL, round_i = NULL) {
    if (is.null(checkpoint_dir) || !nzchar(checkpoint_dir)) {
      return(invisible(NULL))
    }
    .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    if (!is.null(batch_i) && !is.null(round_i)) {
      tag <- sprintf("batch%03d_round%03d", as.integer(batch_i), as.integer(round_i))
      .bt_write_checkpoint(checkpoint_dir, payload, basename = paste0("run_state_", tag), overwrite = checkpoint_overwrite)
    }
    invisible(NULL)
  }

  on.exit(
    {
      if (!is.null(checkpoint_payload_last) && !is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
        .write_checkpoint_now(checkpoint_payload_last)
      }
    },
    add = TRUE
  )

  # If resuming and checkpoint_store_fits was FALSE, recompute fits needed for drift/linking.
  if (!is.null(resume_from) && !isTRUE(chk$completed)) {
    if (is.na(baseline_results_n) || baseline_results_n < 0L) baseline_results_n <- NA_integer_
    if (is.null(baseline_fit) && nrow(results) > 0L) {
      n_base <- baseline_results_n
      if (is.na(n_base) || n_base <= 0L) n_base <- nrow(results)
      n_base <- min(n_base, nrow(results))
      if (n_base > 0L) {
        fr_base <- fit_from_results(results[seq_len(n_base), , drop = FALSE])
        baseline_fit <- .bt_prepare_reference_fit(
          fit = fr_base$fit,
          bt_data = fr_base$bt_data,
          core_ids = core_ids,
          scale_method = reference_scale_method,
          max_abs = reference_max_abs
        )
        bootstrap_fit <- baseline_fit
      }
    }

    if (is.null(current_fit) && nrow(results) > 0L) {
      fr_all <- fit_from_results(results)
      current_fit <- fr_all$fit

      if (!is.null(current_fit) && !is.null(baseline_fit)) {
        current_fit <- apply_linking(current_fit, baseline_fit)
      }

      if (!is.null(current_fit)) {
        current_fit_running <- make_running_fit(
          bt_data = fr_all$bt_data,
          fit_bt = current_fit,
          fit_engine_running = fit_engine_running,
          rc_smoothing = rc_smoothing,
          rc_damping = rc_damping
        )
        attr(current_fit_running, "bt_run_adaptive_core_linking") <- list(stage = "resume_recompute")
      }
    }
  }

  # Fresh run initialization (no resume)
  if (is.null(resume_from)) {
    fits <- list()
    final_fits <- list()
    metrics_rows <- list()
    state_rows <- list()
    batch_summary <- tibble::tibble()
    current_fit <- NULL
    current_fit_running <- NULL
    bootstrap_fit <- NULL
    baseline_fit <- NULL
    baseline_results_n <- NA_integer_
    seen_ids <- core_ids

    # Bootstrap fit if needed
    if (nrow(results) == 0L) {
      init_round_size <- as.integer(init_round_size)
      if (is.na(init_round_size) || init_round_size < 0L) {
        stop("`init_round_size` must be a non-negative integer.", call. = FALSE)
      }
      if (init_round_size > 0L) {
        core_samples <- dplyr::filter(samples, .data$ID %in% core_ids)
        core_pairs <- make_pairs(core_samples)
        core_pairs <- sample_pairs(core_pairs, n_pairs = min(init_round_size, nrow(core_pairs)), seed = seed_pairs)

        judged0 <- .coerce_judge_output(judge_fun(core_pairs))
        judged0 <- .validate_judge_results(judged0, ids = ids_all, judge_col = judge)
        judged0$stage <- "bootstrap"
        judged0$batch_index <- 0L
        judged0$round_index <- 0L
        results <- dplyr::bind_rows(results, judged0)
      }

      fr <- fit_from_results(results)
      if (!is.null(fr$fit)) {
        current_fit <- fr$fit

        baseline_fit <- .bt_prepare_reference_fit(
          fit = fr$fit,
          bt_data = fr$bt_data,
          core_ids = core_ids,
          scale_method = reference_scale_method,
          max_abs = reference_max_abs
        )
        bootstrap_fit <- baseline_fit
        baseline_results_n <- nrow(results)

        current_fit <- apply_linking(current_fit, baseline_fit)

        current_fit_running <- make_running_fit(
          bt_data = fr$bt_data,
          fit_bt = current_fit,
          fit_engine_running = fit_engine_running,
          rc_smoothing = rc_smoothing,
          rc_damping = rc_damping
        )

        attr(current_fit, "bt_run_adaptive_core_linking") <- list(stage = "bootstrap", batch_index = 0L, round_index = 0L)
        attr(current_fit_running, "bt_run_adaptive_core_linking") <- list(stage = "bootstrap", batch_index = 0L, round_index = 0L)

        fits[[length(fits) + 1L]] <- if (store_running_estimates) current_fit_running else current_fit
        final_fits[["bootstrap"]] <- current_fit
      }

      state_rows[[length(state_rows) + 1L]] <- state_row(
        results,
        new_ids = character(0),
        batch_index = 0L,
        round_index = 0L,
        stage = "bootstrap",
        stop = FALSE,
        stop_reason = NA_character_
      )
    } else {
      fr <- fit_from_results(results)
      current_fit <- fr$fit

      # baseline reference should be BT scale (stabilized), not RC
      baseline_fit <- .bt_prepare_reference_fit(
        fit = current_fit,
        bt_data = fr$bt_data,
        core_ids = core_ids,
        scale_method = reference_scale_method,
        max_abs = reference_max_abs
      )
      bootstrap_fit <- baseline_fit
      baseline_results_n <- nrow(results)

      current_fit <- apply_linking(current_fit, baseline_fit)

      if (!is.null(current_fit)) {
        current_fit_running <- make_running_fit(
          bt_data = fr$bt_data,
          fit_bt = current_fit,
          fit_engine_running = fit_engine_running,
          rc_smoothing = rc_smoothing,
          rc_damping = rc_damping
        )
      }
    }

    if (is.null(current_fit) || is.null(current_fit$theta)) {
      .abort_no_fit(
        stage = "initial_fit",
        results = results,
        ids = ids_all,
        judge_col = judge,
        hint = "Ensure `initial_results` contains at least one non-missing `better_id` and valid IDs."
      )
    }
  }

  if (is.null(current_fit) || is.null(current_fit$theta)) {
    .abort_no_fit(
      stage = "start_resume",
      results = results,
      ids = ids_all,
      judge_col = judge,
      hint = "No usable fit could be created from the current results. Check missing `better_id` and resume inputs."
    )
  }
  if (is.null(current_fit_running)) {
    # Ensure always non-NULL after resume if current_fit exists
    fr_tmp <- fit_from_results(results)
    current_fit_running <- make_running_fit(
      bt_data = fr_tmp$bt_data,
      fit_bt = current_fit,
      fit_engine_running = fit_engine_running,
      rc_smoothing = rc_smoothing,
      rc_damping = rc_damping
    )
  }

  # Do we need drift gating?
  drift_active <- any(!is.na(c(
    core_theta_cor_target,
    core_theta_spearman_target,
    core_max_abs_shift_target,
    core_p90_abs_shift_target
  )))

  # Process each batch
  batch_seq <- if (batch_start <= length(batches)) seq.int(from = batch_start, to = length(batches)) else integer(0)
  for (b in batch_seq) {
    batch_ids <- batches[[b]]
    new_ids <- setdiff(batch_ids, seen_ids)
    seen_ids <- unique(c(seen_ids, batch_ids))

    if (length(new_ids) == 0L) {
      final_fits[[paste0("batch", b)]] <- current_fit
      batch_summary <- dplyr::bind_rows(batch_summary, tibble::tibble(
        batch_index = as.integer(b),
        n_new_ids = 0L,
        rounds_used = 0L,
        stop_reason = .bt_resolve_stop_reason(no_new_ids = TRUE),
        n_results_total = nrow(results)
      ))
      next
    }

    prev_fit_for_drift <- if (drift_active) {
      if (b == 1L) {
        if (!is.null(bootstrap_fit)) bootstrap_fit else current_fit
      } else {
        final_fits[[paste0("batch", b - 1L)]]
      }
    } else {
      NULL
    }

    prev_metrics <- NULL
    stop_reason <- NA_character_
    rounds_used <- 0L

    resuming_this_batch <- isTRUE(resume_in_batch) && b == batch_start
    if (resuming_this_batch) {
      if (!is.null(resume_new_ids)) {
        new_ids <- unique(as.character(resume_new_ids))
      }
      prev_metrics <- resume_prev_metrics
      rounds_used <- max(0L, as.integer(round_start) - 1L)
    }

    r_from <- if (resuming_this_batch) as.integer(round_start) else 1L
    if (is.na(r_from) || r_from < 1L) r_from <- 1L
    round_seq <- if (r_from <= max_rounds_per_batch) seq.int(from = r_from, to = max_rounds_per_batch) else integer(0)

    for (r in round_seq) {
      rounds_used <- r

      seed_this <- if (is.null(seed_pairs)) NULL else as.integer(seed_pairs + b * 1000L + r)

      within_batch_frac_this <- within_batch_frac
      core_audit_frac_this <- core_audit_frac

      # Propose pairs: USE running engine thetas when requested.
      prop <- bt_core_link_round(
        samples = samples,
        fit = current_fit_running %||% current_fit,
        core_ids = core_ids,
        new_ids = new_ids,
        round_size = round_size,
        within_batch_frac = within_batch_frac_this,
        core_audit_frac = core_audit_frac_this,
        k_neighbors = k_neighbors,
        min_judgments = min_judgments,
        existing_pairs = results,
        forbid_repeats = forbid_repeats,
        balance_positions = balance_positions,
        seed = seed_this,
        include_text = TRUE
      )

      pairs_next <- prop$pairs

      if (exhaustion_fallback != "none") {
        pairs_next <- .bt_apply_exhaustion_fallback(
          pairs = pairs_next,
          samples = samples,
          core_ids = core_ids,
          new_ids = new_ids,
          seen_ids = seen_ids,
          round_size = round_size,
          forbidden_keys = .bt_round_state(results)$forbidden_keys,
          exhaustion_fallback = exhaustion_fallback,
          exhaustion_min_pairs_frac = exhaustion_min_pairs_frac,
          exhaustion_spectral_gap_threshold = exhaustion_spectral_gap_threshold,
          within_batch_frac = within_batch_frac_this,
          core_audit_frac = core_audit_frac_this,
          k_neighbors = k_neighbors,
          min_judgments = min_judgments,
          existing_pairs = results,
          forbid_repeats = forbid_repeats,
          balance_positions = balance_positions,
          seed = seed_this,
          include_text = TRUE
        )
      }

      if (nrow(pairs_next) == 0L) {
        stop_reason <- .bt_resolve_stop_reason(no_pairs = TRUE)
        state_rows[[length(state_rows) + 1L]] <- state_row(
          results,
          new_ids = new_ids,
          batch_index = b,
          round_index = r,
          stage = "round",
          stop = TRUE,
          stop_reason = stop_reason
        )
        break
      }

      judged <- .coerce_judge_output(judge_fun(pairs_next))
      judged <- .validate_judge_results(judged, ids = ids_all, judge_col = judge)

      judged <- dplyr::left_join(
        judged,
        dplyr::select(pairs_next, dplyr::all_of(c("ID1", "ID2", "pair_type"))),
        by = c("ID1", "ID2"),
        relationship = "many-to-many"
      )

      judged$stage <- "round"
      judged$batch_index <- as.integer(b)
      judged$round_index <- as.integer(r)

      results <- dplyr::bind_rows(results, judged)

      fr <- fit_from_results(results)
      current_fit <- fr$fit
      if (is.null(current_fit) || is.null(current_fit$theta)) {
        stop_reason <- .bt_resolve_stop_reason(no_results = TRUE)
        state_rows[[length(state_rows) + 1L]] <- state_row(
          results,
          new_ids = new_ids,
          batch_index = b,
          round_index = r,
          stage = "round",
          stop = TRUE,
          stop_reason = stop_reason
        )
        break
      }

      current_fit <- apply_linking(current_fit, baseline_fit)

      # Recompute running fit AFTER linking so theta_bt_linked is available.
      current_fit_running <- make_running_fit(
        bt_data = fr$bt_data,
        fit_bt = current_fit,
        fit_engine_running = fit_engine_running,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping
      )

      attr(current_fit, "bt_run_adaptive_core_linking") <- list(stage = "round", batch_index = b, round_index = r, new_ids = new_ids)
      attr(current_fit_running, "bt_run_adaptive_core_linking") <- list(stage = "round", batch_index = b, round_index = r, new_ids = new_ids)

      fits[[length(fits) + 1L]] <- if (store_running_estimates) current_fit_running else current_fit

      m <- bt_stop_metrics(
        current_fit,
        ids = new_ids,
        prev_fit = prev_fit_for_drift,
        core_ids = if (drift_active) core_ids else NULL,
        se_probs = se_probs,
        fit_bounds = fit_bounds
      )

      if (!is.null(current_fit$linking) && is.list(current_fit$linking)) {
        lk <- current_fit$linking
        lk_chr <- function(x) if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1]])
        lk_num <- function(x) if (is.null(x) || length(x) == 0L) NA_real_ else as.numeric(x[[1]])
        lk_int <- function(x) if (is.null(x) || length(x) == 0L) NA_integer_ else as.integer(x[[1]])

        m$linking_mode <- lk_chr(lk$mode)
        m$linking_reference <- lk_chr(lk$reference)
        m$linking_method <- lk_chr(lk$method)
        m$linking_applied <- isTRUE(lk$applied)
        m$linking_reason <- lk_chr(lk$reason)
        m$linking_a <- lk_num(lk$a)
        m$linking_b <- lk_num(lk$b)
        m$linking_n_core <- lk_int(lk$n_core)
        m$linking_min_n <- lk_int(lk$min_n)
        m$linking_threshold_r <- lk_num(lk$threshold_r)
        m$linking_threshold_p90 <- lk_num(lk$threshold_p90)

        if (!is.null(lk$drift) && is.data.frame(lk$drift) && nrow(lk$drift) > 0L) {
          dr <- as.data.frame(lk$drift)[1, , drop = FALSE]
          dr$ids <- NULL
          for (nm in names(dr)) if (!(nm %in% names(m))) m[[nm]] <- dr[[nm]][1]
        }
        if (!is.null(lk$drift_post) && is.data.frame(lk$drift_post) && nrow(lk$drift_post) > 0L) {
          drp <- as.data.frame(lk$drift_post)[1, , drop = FALSE]
          drp$ids <- NULL
          for (nm in names(drp)) if (!(nm %in% names(m))) m[[nm]] <- drp[[nm]][1]
        }
      }

      m$core_n <- as.integer(length(core_ids))
      m <- .bt_add_drift_aliases(m)

      prev_metrics_for_state <- prev_metrics

      decision <- do.call(
        bt_should_stop,
        c(list(metrics = m, prev_metrics = prev_metrics), stop_params)
      )

      stop_now <- isTRUE(decision$stop)
      stop_reason <- .bt_resolve_stop_reason(stopped = stop_now)

      m$batch_index <- as.integer(b)
      m$round_index <- as.integer(r)
      m$within_batch_frac <- within_batch_frac_this
      m$core_audit_frac <- core_audit_frac_this
      m$stage <- "round"
      m$stop <- stop_now
      m$stop_reason <- stop_reason
      m$n_pairs_proposed <- nrow(pairs_next)
      m$n_results_total <- nrow(results)
      m$n_pairs_total <- nrow(results)
      m$n_pairs_new <- nrow(judged)
      m$n_missing_better_id <- sum(is.na(results$better_id))
      m$n_core_new <- sum(pairs_next$pair_type == "core_new")
      m$n_new_new <- sum(pairs_next$pair_type == "new_new")
      m$n_core_core <- sum(pairs_next$pair_type == "core_core")

      m <- .bt_order_metrics(m)

      metrics_rows[[length(metrics_rows) + 1L]] <- m

      state_rows[[length(state_rows) + 1L]] <- state_row(
        results,
        new_ids = new_ids,
        batch_index = b,
        round_index = r,
        stage = "round",
        stop = stop_now,
        stop_reason = stop_reason
      )

      if (!is.null(allocation_fun)) {
        alloc_state <- list(
          stage = "round",
          batch_index = as.integer(b),
          round_index = as.integer(r),
          within_batch_frac = within_batch_frac_this,
          core_audit_frac = core_audit_frac_this,
          metrics = m,
          prev_metrics = prev_metrics_for_state,
          stop = decision,
          n_results_total = nrow(results),
          n_pairs_proposed = nrow(pairs_next),
          n_results_total = nrow(results),
          n_missing_better_id = sum(is.na(results$better_id)),
          n_new_ids = length(new_ids),
          new_ids = new_ids,
          core_ids = core_ids,
          plan = prop$plan
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

      checkpoint_payload_last <- .make_checkpoint_payload(
        next_batch_index = as.integer(b),
        next_round_index = as.integer(r + 1L),
        in_batch = TRUE,
        new_ids_current = new_ids,
        prev_metrics_current = prev_metrics,
        completed = FALSE,
        out = NULL
      )
      if ((r %% checkpoint_every) == 0L) {
        .write_checkpoint_now(checkpoint_payload_last, batch_i = b, round_i = r)
      }

      if (stop_now) {
        break
      }

      prev_metrics <- m
      if (r == max_rounds_per_batch) {
        stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      }
    }

    if (is.na(stop_reason)) {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
    }

    final_name <- paste0("batch", b)
    final_fits[[final_name]] <- current_fit

    batch_summary_row <- tibble::tibble(
      batch_index = as.integer(b),
      n_new_ids = length(new_ids),
      rounds_used = as.integer(rounds_used),
      stop_reason = stop_reason,
      linking_mode = if (!is.null(current_fit$linking)) current_fit$linking$mode else NA_character_,
      linking_method = if (!is.null(current_fit$linking)) current_fit$linking$method else NA_character_,
      linking_applied = if (!is.null(current_fit$linking)) isTRUE(current_fit$linking$applied) else NA,
      linking_reason = if (!is.null(current_fit$linking)) current_fit$linking$reason else NA_character_,
      linking_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift)) {
        d <- current_fit$linking$drift
        if (is.data.frame(d) && nrow(d) > 0L && "linking_p90_abs_shift" %in% names(d)) as.numeric(d$linking_p90_abs_shift[[1]]) else NA_real_
      } else {
        NA_real_
      },
      linking_post_p90_abs_shift = if (!is.null(current_fit$linking) && !is.null(current_fit$linking$drift_post)) {
        d <- current_fit$linking$drift_post
        if (is.data.frame(d) && nrow(d) > 0L && "linking_post_p90_abs_shift" %in% names(d)) as.numeric(d$linking_post_p90_abs_shift[[1]]) else NA_real_
      } else {
        NA_real_
      },
      n_results_total = nrow(results)
    )
    batch_summary <- dplyr::bind_rows(batch_summary, batch_summary_row)

    checkpoint_payload_last <- .make_checkpoint_payload(
      next_batch_index = as.integer(b + 1L),
      next_round_index = 1L,
      in_batch = FALSE,
      new_ids_current = NULL,
      prev_metrics_current = NULL,
      completed = FALSE,
      out = NULL
    )
    .write_checkpoint_now(checkpoint_payload_last, batch_i = b, round_i = rounds_used)
    resume_in_batch <- FALSE
  }

  bt_data <- build_bt_fun(results, judge = judge)

  metrics <- if (length(metrics_rows) == 0L) {
    tibble::tibble()
  } else {
    dplyr::bind_rows(metrics_rows)
  }

  state <- dplyr::bind_rows(state_rows)

  if (nrow(state) > 0L) {
    if (!("median_appearances" %in% names(state)) && ("appear_p50" %in% names(state))) {
      state$median_appearances <- state$appear_p50
    }
    if (!("p90_appearances" %in% names(state)) && ("appear_p90" %in% names(state))) {
      state$p90_appearances <- state$appear_p90
    }
    if (!("new_median_appearances" %in% names(state)) && ("new_appear_p50" %in% names(state))) {
      state$new_median_appearances <- state$new_appear_p50
    }
    if (!("new_p90_appearances" %in% names(state)) && ("new_appear_p90" %in% names(state))) {
      state$new_p90_appearances <- state$new_appear_p90
    }
  }

  metrics <- .bt_align_metrics(metrics, se_probs = se_probs)
  state <- .bt_align_state(state)

  final_est <- NULL
  if (isTRUE(final_refit) && nrow(results) > 0L) {
    final_est <- compute_final_estimates(
      results = results,
      ids = ids_all,
      bt_bias_reduction = final_bt_bias_reduction,
      rc_smoothing = rc_smoothing,
      rc_damping = rc_damping
    )
  }

  out <- list(
    core_ids = core_ids,
    batches = batches,
    results = results,
    bt_data = bt_data,
    fits = fits,
    final_fits = final_fits,
    estimates = if (is.null(final_est)) tibble::tibble() else final_est$estimates,
    final_models = if (is.null(final_est)) list() else list(bt = final_est$bt_fit, rc = final_est$rc_fit, diagnostics = final_est$diagnostics),
    metrics = metrics,
    batch_summary = batch_summary,
    state = state,
    stop_reason = if (nrow(batch_summary) == 0L) NA_character_ else as.character(batch_summary$stop_reason[[nrow(batch_summary)]]),
    stop_round = if (nrow(batch_summary) == 0L) NA_integer_ else as.integer(sum(batch_summary$rounds_used, na.rm = TRUE))
  )

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
    .write_checkpoint_now(checkpoint_payload_last, batch_i = length(batches), round_i = NA_integer_)
  }

  .as_pairwise_run(out, run_type = "adaptive_core_linking")
}
