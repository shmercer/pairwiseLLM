#' Run a round-based adaptive BT workflow end-to-end
#'
#' This exported runner implements the core adaptive loop:
#' \enumerate{
#' \item propose pairs (initial random bootstrap, then adaptive),
#' \item score pairs using a provided judge function (LLM/human/simulator),
#' \item append results,
#' \item fit a BT model,
#' \item compute stopping metrics and decide whether to stop,
#' \item repeat until stopping criteria are met or \code{max_rounds} is reached.
#' }
#'
#' In addition to per-round stop metrics, \code{bt_run_adaptive()} records a compact
#' per-round \emph{state snapshot} (counts of unique unordered pairs judged, ID appearance
#' distribution, position imbalance, and basic missingness checks). This is intended for
#' debugging and monitoring convergence/coverage over rounds.
#'
#' \strong{No reverse-order checks during adaptive sampling.}
#' Optionally, after stopping (or hitting \code{max_rounds}), you can run a
#' post-hoc reverse-order audit on a random subset of already-judged pairs and
#' compute forward-vs-reverse consistency via \code{\link{compute_reverse_consistency}}.
#'
#' The default modeling function is \code{\link{fit_bt_model}} and the default
#' stopping+pairing helper is \code{\link{bt_adaptive_round}}.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' \code{ID} must be unique and non-missing; \code{text} is the content shown to the judge.
#'
#' @param judge_fun A function that accepts a tibble of pairs with columns
#' \code{ID1}, \code{text1}, \code{ID2}, \code{text2} and returns a tibble with
#' columns \code{ID1}, \code{ID2}, and \code{better_id}.
#' If \code{judge} is provided, the returned tibble must also include that judge column.
#' \code{better_id} may be returned as the literal winning ID (\code{ID1} or \code{ID2})
#' or as common positional labels (e.g., \code{SAMPLE_1}/\code{SAMPLE_2},
#' \code{ID1}/\code{ID2}, \code{1}/\code{2}, \code{0}/\code{1}, \code{A}/\code{B},
#' \code{LEFT}/\code{RIGHT}); these are normalized to the corresponding IDs.
#' Blank/NA-like winners are treated as missing (\code{NA}) and are ignored in scoring.
#'
#' @param initial_results Optional tibble/data.frame of already-scored pairs with columns
#' \code{ID1}, \code{ID2}, \code{better_id} (and optional judge column). When provided,
#' these results are used as the starting state (and no bootstrap is performed unless
#' \code{initial_results} is empty).
#'
#' @param judge Optional character scalar giving the name of the column in results that
#' identifies the judge/backend/model (e.g., \code{"gpt4o"} vs \code{"claude"}).
#' When provided, the column must be present in outputs from \code{judge_fun} and is
#' passed to \code{\link{build_bt_data}} so engines that support judge effects can use it.
#'
#' @param engine Character scalar passed to \code{fit_fun} as its \code{engine} argument.
#' Default \code{"sirt"}.
#' @param fit_verbose Logical; passed to \code{fit_fun} as \code{verbose}. Default \code{FALSE}.
#' @param return_diagnostics Logical; passed to \code{fit_fun}. If \code{TRUE}, attempt to
#' return engine-specific diagnostics (e.g., item fit, separation/reliability). Default \code{TRUE}.
#' @param include_residuals Logical; passed to \code{fit_fun}. If \code{TRUE}, request
#' residual/probability outputs when supported (may increase compute/memory). Default \code{FALSE}.
#'
#' @param fit_engine_running Character. Estimator used for the \emph{running} ability
#'   vector that drives adaptive pair selection. \code{"bt"} uses the BT fit returned
#'   by \code{fit_fun}. \code{"rank_centrality"} computes Rank Centrality scores from
#'   the current comparison graph (often more stable early on sparse graphs) while
#'   still using BT standard errors as an uncertainty heuristic when available.
#'   Default \code{"bt"}.
#' @param rc_smoothing,rc_damping Numeric. Parameters forwarded to
#'   \code{\link{fit_rank_centrality}} when \code{fit_engine_running = "rank_centrality"} (default)
#'   (and also stored in per-round fits). See \code{\link{fit_rank_centrality}} for
#'   details.
#' @param final_refit Logical. If \code{TRUE} (default), compute a final combined
#'   estimates table (Rank Centrality plus optional BT variants) at the end of the
#'   run via \code{\link{compute_final_estimates}}. Suggested dependencies are
#'   used when available; if unavailable or if a BT fit fails, the function falls
#'   back to Rank Centrality and records the reason in \code{out$fit_provenance}.
#' @param fit_engine_final Preferred final BT engine used when \code{final_refit = TRUE}.
#'   Options are \code{"bt_firth"}, \code{"bt_mle"}, \code{"bt_bayes"}, and \code{"none"}.
#'   See \code{\link{compute_final_estimates}} for details. Default \code{"bt_firth"}.
#' @param final_bt_bias_reduction Logical. Passed to \code{\link{compute_final_estimates}}
#'   as \code{bt_bias_reduction}. Default \code{TRUE}. Typically leave \code{TRUE}
#'   when \code{fit_engine_final = "bt_firth"}.
#'
#' @param round_size Integer. Number of new pairs to propose and score in each adaptive round.
#' If \code{0}, the runner will fit once (if possible) and then stop without proposing new pairs.
#' @param init_round_size Integer. Number of bootstrap (random) pairs to score before the first
#' model fit when \code{initial_results} is \code{NULL} or empty. Default: \code{round_size}.
#' @param max_rounds Integer. Maximum number of adaptive rounds to run (excluding the bootstrap
#' scoring step). Default \code{50}.
#'
#' @param se_probs Numeric vector of probabilities in (0, 1) used when summarizing the
#' distribution of standard errors for stopping diagnostics (e.g., median, 90th percentile).
#' Passed to \code{\link{bt_adaptive_round}}.
#' @param fit_bounds Numeric length-2 vector giving acceptable infit/outfit (or analogous) bounds
#' @param stopping_tier Preset stopping thresholds to use (good/strong/very_strong).
#' when available. Passed to \code{\link{bt_adaptive_round}}.
#'
#' @param reliability_target Numeric. Target reliability/separation-based criterion used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param sepG_target Numeric. Target separation index (or analogous) used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param rel_se_p90_target Numeric. Target value for the 90th percentile of item SE (or a
#' comparable uncertainty summary) used for stopping. Passed to \code{\link{bt_adaptive_round}}.
#' @param rel_se_p90_min_improve Numeric. Minimum required improvement in the uncertainty summary
#' relative to the previous round; if improvement falls below this, stopping may be allowed
#' (depending on other criteria). Passed to \code{\link{bt_adaptive_round}}.
#' @param max_item_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of item misfit flags
#' (based on \code{fit_bounds}/diagnostics) before stopping is disallowed. Passed to
#' \code{\link{bt_adaptive_round}}.
#' @param max_judge_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of judge misfit flags before stopping is disallowed
#' (when judge diagnostics are available). Passed to \code{\link{bt_adaptive_round}}.
#'
#' @param k_neighbors Integer. When ability estimates are available, restrict candidate pair
#' selection to approximately local neighborhoods in \code{theta} (e.g., near neighbors) to focus
#' comparisons where they are most informative. If \code{theta} is not available (early rounds),
#' selection falls back to non-theta heuristics. Passed to \code{\link{bt_adaptive_round}} /
#' \code{\link{select_adaptive_pairs}}.
#' @param min_judgments Integer. Minimum number of total judgments per item to prioritize before
#' focusing on adaptive informativeness/uncertainty. Passed to \code{\link{bt_adaptive_round}}.
#' @param forbid_repeats Logical. If \code{TRUE}, unordered pairs (A,B) are not repeated across
#' rounds. Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#' @param balance_positions Logical. If \code{TRUE}, attempt to balance how often each item appears
#' in the first vs second position (\code{ID1} vs \code{ID2}) to mitigate positional bias.
#' Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#' @param embeddings Optional numeric embeddings matrix with one row per item.
#' If provided, adaptive candidate generation is augmented with embedding-based
#' neighbor candidates (in addition to theta-neighbors). Use
#' \code{\link{validate_embeddings}} to align/reorder rows.
#' @param embed_k Integer. Number of nearest neighbors (by cosine similarity) to
#' precompute per item from \code{embeddings}. Only used when \code{embeddings}
#' is provided.
#' @param embed_far_k Integer. Number of additional "far" candidates to sample
#' per item (uniformly at random) to preserve global mixing. Only used when
#' \code{embeddings} is provided.
#' @param embed_quota_frac Numeric in \code{[0, 1]}. Minimum fraction of selected pairs
#'   that should come from embedding-neighbor candidates when embeddings are used.
#' @param candidate_pool_cap Non-negative integer or Inf. Global cap on the size
#'   of the candidate pool considered during pair selection.
#' @param per_anchor_cap Non-negative integer or Inf. Per-item cap on the number
#'   of candidates retained (after scoring) for each anchor item.
#' @param w_embed Non-negative numeric. Optional weight on an embedding-source bonus
#'   term used in candidate scoring.
#' @param embed_score_mode Character. How to compute the embedding-source bonus.
#'
#' @param seed_pairs Optional integer seed used for bootstrap pair generation as
#' \code{seed_pairs}, and for adaptive rounds as \code{seed_pairs + round}.
#' The RNG state is restored to its prior value (or returned to "uninitialized" if it was missing).
#' Note: this controls pair selection reproducibility; it does not control randomness inside
#' \code{judge_fun} unless your \code{judge_fun} uses it explicitly.
#'
#' @param reverse_audit Logical. If \code{TRUE}, run a post-stop reverse-order audit by selecting
#' a subset of forward-scored pairs, reversing their order, and re-scoring them. This does not
#' affect adaptive sampling decisions (it is post-hoc).
#' @param reverse_pct Numeric between 0 and 1 (inclusive). Proportion of eligible unique forward pairs to reverse
#' for the audit. Eligible pairs are unique unordered forward pairs with non-missing
#' \code{better_id}. Ignored if \code{n_reverse} is provided.
#' @param n_reverse Optional integer. Number of eligible unique forward pairs to reverse for the
#' audit. If provided, overrides \code{reverse_pct}.
#' @param reverse_seed Optional integer seed used only for selecting which pairs to reverse
#' (RNG state is restored afterward).
#'
#' @param fit_fun Function used to fit the BT model. Default \code{\link{fit_bt_model}}.
#' Primarily intended as a test hook; most users should keep the default.
#' @param build_bt_fun Function used to convert results into BT data. Default
#' \code{\link{build_bt_data}}. Primarily intended as a test hook.
#'
#' @param checkpoint_dir Optional directory path for writing checkpoint files during
#'   a run. If provided, the runner writes a checkpoint file named
#'   \code{run_state.rds} (and optionally per-round snapshot files) after completed
#'   rounds. Use this to resume long jobs after interruption or errors.
#'
#' @param resume_from Optional directory path containing a prior checkpoint file
#'   \code{run_state.rds} created by \code{bt_run_adaptive()}. When provided, the
#'   run resumes from the saved state (including accumulated results and round
#'   counters) rather than starting from scratch. The \code{samples} argument must
#'   match the checkpoint's sample IDs (and typically the same runner configuration
#'   should be used).
#'
#' @param checkpoint_every Integer controlling how frequently per-round snapshot
#'   checkpoint files are written. A value of \code{1} writes a snapshot after every
#'   completed round; a value of \code{2} writes snapshots every other round, etc.
#'   The main file \code{run_state.rds} is still updated at safe points even when
#'   \code{checkpoint_every > 1}.
#'
#' @param checkpoint_store_fits Logical indicating whether to store fitted model
#'   objects (e.g., BT model fits and diagnostics) inside checkpoint files. Set to
#'   \code{FALSE} to reduce checkpoint size and speed up checkpoint writes; in that
#'   case, model fits will be recomputed as needed after resuming.
#'
#' @param checkpoint_overwrite Logical indicating whether to overwrite an existing
#'   \code{run_state.rds} file in \code{checkpoint_dir}. If \code{FALSE} and a
#'   checkpoint file already exists, the function should error rather than overwrite.
#'
#' @param ... Additional arguments passed through to \code{fit_fun}.
#'
#' @details
#' \strong{Checkpointing and resuming:} If \code{checkpoint_dir} is provided, this
#' function writes a checkpoint representing the \emph{last completed safe point}
#' of the adaptive loop (i.e., after a round has fully completed). If the run is
#' interrupted or errors occur mid-round, the checkpoint corresponds to the most
#' recently completed round. Resume by calling the function again with
#' \code{resume_from = checkpoint_dir} (and typically the same key settings such as
#' \code{round_size}, repeat controls, and selection strategy).
#'
#' @return A list with elements:
#' \describe{
#' \item{results}{All accumulated forward-direction results (ID1, ID2, better_id, ...).}
#' \item{bt_data}{BT data built from \code{results}.}
#' \item{fits}{List of per-round fit objects (one per adaptive round). Each fit is
#' tagged with per-round metadata in the \code{attr(fit, "bt_run_adaptive")} attribute.}
#' \item{final_fit}{The final fit object (the last element of \code{fits}), or \code{NULL}
#' if no fit was run.}
#' \item{theta}{A compact tibble with columns \code{ID}, \code{theta}, \code{se}, and
#' \code{rank}, representing the final ability scale used for most downstream workflows.
#' When a BT final fit succeeds, this corresponds to the final BT scale; otherwise it
#' falls back to Rank Centrality.}
#' \item{theta_engine}{A single string describing which engine produced \code{theta}
#' (e.g., \code{"bt_firth"}, \code{"bt_mle"}, \code{"bt_bayes"}, \code{"rank_centrality"}).}
#' \item{estimates}{NULL unless \code{final_refit=TRUE}; then a final combined estimates
#' tibble produced by \code{\link{compute_final_estimates}} (BT + Rank Centrality).}
#' \item{final_models}{NULL unless \code{final_refit=TRUE}; then a list containing
#' the fitted model objects (\code{bt_fit}, \code{rc_fit}) and a summary \code{diagnostics} list.}
#' \item{fit_provenance}{NULL unless \code{final_refit=TRUE}; then a list describing which
#' final engines were requested and used, including any fallback reason.}
#' \item{stop_reason}{A single string describing why the loop ended (e.g., \code{"stopped"},
#' \code{"max_rounds"}, \code{"no_pairs"}, \code{"round_size_zero"}, \code{"no_new_results"},
#' or \code{"no_results"}).}
#' \item{stop_round}{Integer round index at which the loop ended (\code{NA} if no rounds were run).}
#' \item{rounds}{A tibble summarizing each adaptive round (metrics + stop flag + stop_reason).}
#' \item{state}{A tibble with one row per adaptive round containing bookkeeping summaries
#'   of the accumulated results at that round (e.g., \code{n_unique_unordered_pairs},
#'   appearance quantiles, \code{pos_imbalance_max}, \code{n_self_pairs},
#'   \code{n_missing_better_id}), plus \code{round}, \code{stop}, and \code{stop_reason}.}
#' \item{pairs_bootstrap}{Pairs used in the bootstrap scoring step (may be empty).}
#' \item{reverse_audit}{NULL unless \code{reverse_audit=TRUE}; then contains audit
#' pairs, reverse results, and consistency outputs (post-hoc; does not affect stopping).}
#' }
#'
#' @examples
#' # Minimal self-contained example that does not require sirt:
#' samples <- tibble::tibble(
#'   ID = c("A", "B", "C", "D"),
#'   text = paste("text", c("A", "B", "C", "D"))
#' )
#'
#' # A tiny "judge" simulator (deterministic by latent ability):
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' judge_fun <- function(pairs) {
#'   simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
#' }
#'
#' # A tiny fit function (test-style), so the example runs without external engines:
#' fit_fun <- function(bt_data, ...) {
#'   bt_data <- as.data.frame(bt_data)
#'   ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
#'   wins <- stats::setNames(rep(0L, length(ids)), ids)
#'   n_j <- stats::setNames(rep(0L, length(ids)), ids)
#'   for (i in seq_len(nrow(bt_data))) {
#'     a <- as.character(bt_data[[1]][i])
#'     b <- as.character(bt_data[[2]][i])
#'     r <- as.numeric(bt_data[[3]][i])
#'     if (is.finite(r)) {
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
#' out <- bt_run_adaptive(
#'   samples = samples,
#'   judge_fun = judge_fun,
#'   fit_fun = fit_fun,
#'   engine = "mock",
#'   round_size = 2,
#'   init_round_size = 2,
#'   max_rounds = 2,
#'   rel_se_p90_target = NA_real_,
#'   rel_se_p90_min_improve = NA_real_
#' )
#' out$rounds
#'
#' @import tibble
#' @import dplyr
#' @export
bt_run_adaptive <- function(samples,
                            judge_fun,
                            initial_results = NULL,
                            judge = NULL,
                            engine = "sirt",
                            fit_verbose = FALSE,
                            return_diagnostics = TRUE,
                            include_residuals = FALSE,
                            fit_engine_running = c("rank_centrality", "bt"),
                            rc_smoothing = 0.5,
                            rc_damping = 0.0,
                            final_refit = TRUE,
                            fit_engine_final = c("bt_firth", "bt_mle", "bt_bayes", "none"),
                            final_bt_bias_reduction = TRUE,
                            round_size = 50,
                            init_round_size = round_size,
                            max_rounds = 50,
                            se_probs = c(0.5, 0.9, 0.95),
                            fit_bounds = c(0.7, 1.3),
                            stopping_tier = c("strong", "good", "very_strong"),
                            reliability_target = 0.90,
                            sepG_target = 3.0,
                            rel_se_p90_target = 0.30,
                            rel_se_p90_min_improve = 0.01,
                            max_item_misfit_prop = 0.05,
                            max_judge_misfit_prop = 0.05,
                            k_neighbors = 10,
                            min_judgments = 12,
                            repeat_policy = "reverse_only",
                            repeat_cap = 1L,
                            repeat_frac = 0.05,
                            repeat_n = NULL,
                            repeat_guard_min_degree = 1L,
                            repeat_guard_largest_component_frac = 0.90,
                            forbid_repeats = NULL,
                            balance_positions = TRUE,
                            embeddings = NULL,
                            embed_k = 30,
                            embed_far_k = 0,
                            seed_pairs = NULL,
                            reverse_audit = FALSE,
                            reverse_pct = 0.10,
                            n_reverse = NULL,
                            reverse_seed = NULL,
                            fit_fun = fit_bt_model,
                            build_bt_fun = build_bt_data,
                            checkpoint_dir = NULL,
                            resume_from = NULL,
                            checkpoint_every = 1L,
                            checkpoint_store_fits = TRUE,
                            checkpoint_overwrite = TRUE,
                            embed_quota_frac = 0.25,
                            candidate_pool_cap = Inf,
                            per_anchor_cap = Inf,
                            w_embed = 1,
                            embed_score_mode = "rank_decay",
                            ...) {
  tag_fit <- function(fit,
                      round_index,
                      stage,
                      n_results,
                      n_pairs_this_round,
                      stop_reason) {
    meta <- list(
      round_index = as.integer(round_index),
      stage = as.character(stage),
      n_results = as.integer(n_results),
      n_pairs_this_round = as.integer(n_pairs_this_round),
      stop_reason = as.character(stop_reason)
    )
    attr(fit, "bt_run_adaptive") <- meta
    fit
  }

  # ---- PR4.1 helper: extract theta from last available running fit ----
  .theta_from_last_running_fit <- function(last_fit, ids) {
    if (is.null(last_fit) || is.null(last_fit$theta)) {
      return(list(theta = NULL, engine = NA_character_))
    }

    th <- tibble::as_tibble(last_fit$theta)
    if (!all(c("ID", "theta") %in% names(th))) {
      return(list(theta = NULL, engine = NA_character_))
    }

    th$ID <- as.character(th$ID)

    # Ensure consistent row coverage & ordering on ids
    th <- dplyr::right_join(th, tibble::tibble(ID = as.character(ids)), by = "ID")

    # Determine SE to expose:
    # - If running engine is RC, SE is generally not meaningful -> NA
    # - If running engine is BT, use available se column (if present)
    engine_running <- NA_character_
    if (!is.null(last_fit$engine_running) && is.character(last_fit$engine_running) && length(last_fit$engine_running) == 1L) {
      engine_running <- as.character(last_fit$engine_running)
    }

    se_out <- rep(NA_real_, nrow(th))
    if (!is.na(engine_running) && identical(engine_running, "bt")) {
      if ("se" %in% names(th)) {
        se_out <- as.double(th$se)
      } else if ("se_bt" %in% names(th)) {
        se_out <- as.double(th$se_bt)
      }
    } else {
      # RC running scale: keep SE as NA
      se_out <- rep(NA_real_, nrow(th))
    }

    # Rank: higher theta -> smaller rank
    theta_num <- suppressWarnings(as.double(th$theta))
    rank_out <- rep(NA_integer_, length(theta_num))
    ok <- !is.na(theta_num)
    if (any(ok)) {
      rank_out[ok] <- as.integer(rank(-theta_num[ok], ties.method = "min"))
    }

    out_theta <- tibble::tibble(
      ID = as.character(th$ID),
      theta = theta_num,
      se = se_out,
      rank = rank_out
    )

    list(theta = out_theta, engine = if (!is.na(engine_running)) engine_running else "rank_centrality")
  }

  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }

  # Capture and sanitize `...` forwarded to fit_fun. This prevents collisions like
  # `verbose` (often intended for runner logging) being passed twice to fit_fun.
  .fit_dots <- list(...)
  if (!is.null(.fit_dots$verbose) && missing(fit_verbose)) {
    fit_verbose <- isTRUE(.fit_dots$verbose)
  }
  .fit_dots <- .clean_fit_dots(.fit_dots)


  ids <- as.character(samples$ID)
  if (length(ids) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

  # Optional: embedding-augmented candidate generation.
  embedding_neighbors <- NULL
  if (!is.null(embeddings)) {
    if (!is.numeric(embed_k) || length(embed_k) != 1L || is.na(embed_k) || embed_k < 0) {
      stop("`embed_k` must be a single non-negative integer.", call. = FALSE)
    }
    embed_k <- as.integer(embed_k)
    if (!is.numeric(embed_far_k) || length(embed_far_k) != 1L || is.na(embed_far_k) || embed_far_k < 0) {
      stop("`embed_far_k` must be a single non-negative integer.", call. = FALSE)
    }
    embed_far_k <- as.integer(embed_far_k)
    embedding_neighbors <- .compute_embedding_neighbors(embeddings, ids = ids, k = embed_k)
  }

  if (!is.function(judge_fun)) stop("`judge_fun` must be a function.", call. = FALSE)

  if (!is.null(judge)) {
    if (!is.character(judge) || length(judge) != 1L || is.na(judge) || nchar(judge) == 0L) {
      stop("`judge` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  round_size <- as.integer(round_size)
  if (is.na(round_size) || round_size < 0L) stop("`round_size` must be a non-negative integer.", call. = FALSE)

  init_round_size <- as.integer(init_round_size)
  if (is.na(init_round_size) || init_round_size < 0L) stop("`init_round_size` must be a non-negative integer.", call. = FALSE)

  max_rounds <- as.integer(max_rounds)
  if (is.na(max_rounds) || max_rounds < 0L) stop("`max_rounds` must be a non-negative integer.", call. = FALSE)

  checkpoint_every <- as.integer(checkpoint_every)
  if (is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be an integer >= 1.", call. = FALSE)
  }

  if (!is.null(resume_from) && (is.null(checkpoint_dir) || !nzchar(checkpoint_dir))) {
    checkpoint_dir <- resume_from
  }

  fit_engine_running <- match.arg(fit_engine_running)
  fit_engine_final <- match.arg(fit_engine_final)

  # --- repeat policy (PR6) ---
  if (!is.null(forbid_repeats)) {
    warning("`forbid_repeats` is deprecated; use `repeat_policy`.", call. = FALSE)
    if (isTRUE(forbid_repeats)) {
      repeat_policy <- "none"
    } else {
      repeat_policy <- "reverse_only"
    }
  }
  repeat_policy <- match.arg(repeat_policy, c("none", "reverse_only", "forbid_unordered"))
  if (!is.numeric(rc_smoothing) || length(rc_smoothing) != 1L || is.na(rc_smoothing) || rc_smoothing < 0) {
    stop("`rc_smoothing` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(rc_damping) || length(rc_damping) != 1L || is.na(rc_damping) || rc_damping < 0 || rc_damping >= 1) {
    stop("`rc_damping` must be a single number in [0, 1).", call. = FALSE)
  }
  final_refit <- isTRUE(final_refit)
  final_bt_bias_reduction <- isTRUE(final_bt_bias_reduction)

  make_running_fit <- function(bt_data, fit_bt) {
    bt_tbl <- .as_theta_tibble(fit_bt$theta, arg_name = "fit_fun()$theta")
    bt_tbl <- tibble::as_tibble(bt_tbl)
    bt_tbl$ID <- as.character(bt_tbl$ID)

    # Ensure every ID in `samples` is present (for consistent downstream joins)
    bt_tbl <- dplyr::right_join(bt_tbl, tibble::tibble(ID = as.character(ids)), by = "ID")

    theta_bt <- bt_tbl$theta
    se_bt <- bt_tbl$se

    theta_rc <- rep(NA_real_, length(ids))
    pi_rc <- rep(NA_real_, length(ids))
    rc_fit <- NULL

    if (fit_engine_running == "rank_centrality") {
      rc_fit <- fit_rank_centrality(
        bt_data,
        ids = ids,
        smoothing = rc_smoothing,
        damping = rc_damping
      )
      rc_tbl <- tibble::as_tibble(rc_fit$theta)
      rc_tbl$ID <- as.character(rc_tbl$ID)
      rc_tbl <- dplyr::right_join(rc_tbl, tibble::tibble(ID = as.character(ids)), by = "ID")
      theta_rc <- rc_tbl$theta
      pi_rc <- rc_tbl$pi
    }

    theta_running <- if (fit_engine_running == "rank_centrality") theta_rc else theta_bt

    theta_out <- tibble::tibble(
      ID = as.character(ids),
      theta = theta_running,
      se = se_bt,
      theta_bt = theta_bt,
      se_bt = se_bt,
      theta_rc = theta_rc,
      pi_rc = pi_rc
    )

    list(
      engine = fit_bt$engine,
      engine_running = fit_engine_running,
      reliability = fit_bt$reliability,
      theta = theta_out,
      diagnostics = fit_bt$diagnostics,
      bt_fit = fit_bt,
      rc_fit = rc_fit
    )
  }

  .add_pair_key_direction <- function(df) {
    df <- tibble::as_tibble(df)
    if (!all(c("ID1", "ID2") %in% names(df))) {
      return(df)
    }
    if (!"pair_key" %in% names(df)) {
      df <- dplyr::mutate(df, pair_key = .unordered_pair_key(.data$ID1, .data$ID2))
    }
    if (!"direction" %in% names(df)) {
      df <- dplyr::mutate(df, direction = .pair_direction(.data$ID1, .data$ID2))
    }
    df
  }

  # helper: random bootstrap pairs (unique unordered, optional position balancing)
  .bootstrap_pairs <- function(n_pairs, existing_pairs, seed = NULL) {
    n_pairs <- as.integer(n_pairs)
    if (is.na(n_pairs) || n_pairs < 0L) {
      stop("`n_pairs` must be a non-negative integer.", call. = FALSE)
    }

    if (n_pairs == 0L) {
      return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
    }

    existing_pairs <- tibble::as_tibble(existing_pairs)

    # Position counts from existing results
    pos1_counts <- integer(length(ids))
    pos2_counts <- integer(length(ids))

    if (nrow(existing_pairs) > 0L) {
      tab1 <- table(factor(existing_pairs$ID1, levels = ids))
      tab2 <- table(factor(existing_pairs$ID2, levels = ids))
      pos1_counts <- as.integer(tab1)
      pos2_counts <- as.integer(tab2)
    }

    imbalance <- pos1_counts - pos2_counts

    key_fun <- function(a, b) {
      .unordered_pair_key(a, b)
    }

    existing_key <- character()
    if (nrow(existing_pairs) > 0L) {
      existing_key <- unique(key_fun(existing_pairs$ID1, existing_pairs$ID2))
    }

    pairs_id <- .with_seed_restore(
      seed,
      f = function() {
        out_ID1 <- character()
        out_ID2 <- character()
        out_key <- character()

        max_tries <- max(200L, n_pairs * 50L)
        tries <- 0L

        while (length(out_ID1) < n_pairs && tries < max_tries) {
          tries <- tries + 1L
          pair <- sample(ids, size = 2L, replace = FALSE)
          a <- pair[[1]]
          b <- pair[[2]]
          k <- key_fun(a, b)

          if (isTRUE(forbid_repeats) && (k %in% c(existing_key, out_key))) {
            next
          }

          if (isTRUE(balance_positions)) {
            ia <- match(a, ids)
            ib <- match(b, ids)

            if (imbalance[ia] > imbalance[ib]) {
              id1 <- b
              id2 <- a
            } else if (imbalance[ib] > imbalance[ia]) {
              id1 <- a
              id2 <- b
            } else {
              if (stats::runif(1) < 0.5) {
                id1 <- a
                id2 <- b
              } else {
                id1 <- b
                id2 <- a
              }
            }

            imbalance[match(id1, ids)] <- imbalance[match(id1, ids)] + 1L
            imbalance[match(id2, ids)] <- imbalance[match(id2, ids)] - 1L
          } else {
            id1 <- a
            id2 <- b
          }

          out_ID1 <- c(out_ID1, id1)
          out_ID2 <- c(out_ID2, id2)
          out_key <- c(out_key, k)
        }

        tibble::tibble(ID1 = out_ID1, ID2 = out_ID2)
      },
      arg_name = "seed_pairs"
    )

    .add_pair_texts(pairs_id, samples = samples)
  }

  # ---- resume / checkpoint state ----
  checkpoint_payload_last <- NULL
  rounds_tbl_prev <- tibble::tibble()
  state_tbl_prev <- tibble::tibble()
  start_round <- 1L

  if (!is.null(resume_from)) {
    chk <- .bt_read_checkpoint(resume_from)
    .bt_validate_checkpoint(chk, run_type = "adaptive", ids = ids)

    if (!is.null(chk$random_seed)) {
      # best-effort restore RNG state for deterministic continuation
      try(assign(".Random.seed", chk$random_seed, envir = .GlobalEnv), silent = TRUE)
    }

    results <- tibble::as_tibble(chk$results)
    pairs_bootstrap <- tibble::as_tibble(chk$pairs_bootstrap %||% tibble::tibble(
      ID1 = character(), text1 = character(), ID2 = character(), text2 = character()
    ))

    fits <- chk$fits %||% list()
    final_fit <- chk$final_fit %||% NULL
    prev_metrics <- chk$prev_metrics %||% NULL

    stop_reason <- chk$stop_reason %||% NA_character_
    stop_round <- chk$stop_round %||% NA_integer_

    rounds_tbl_prev <- tibble::as_tibble(chk$rounds %||% tibble::tibble())
    state_tbl_prev <- tibble::as_tibble(chk$state %||% tibble::tibble())

    start_round <- as.integer(chk$next_round %||% (nrow(rounds_tbl_prev) + 1L))
    if (is.na(start_round) || start_round < 1L) start_round <- nrow(rounds_tbl_prev) + 1L

    # if the checkpoint indicates completion, return it as-is
    if (isTRUE(chk$completed)) {
      out <- chk$out %||% list(
        results = results,
        bt_data = if (nrow(results) == 0L) tibble::tibble(object1 = character(), object2 = character(), result = numeric()) else build_bt_fun(results, judge = judge),
        fits = fits,
        final_fit = final_fit,
        estimates = NULL,
        final_models = NULL,
        stop_reason = stop_reason,
        stop_round = stop_round,
        rounds = rounds_tbl_prev,
        state = state_tbl_prev,
        pairs_bootstrap = pairs_bootstrap,
        reverse_audit = NULL
      )
      return(.as_pairwise_run(out, run_type = "adaptive"))
    }
  }

  # normalize initial results (NULL or 0-row => empty start)
  if (is.null(resume_from)) {
    if (is.null(initial_results) || (is.data.frame(initial_results) && nrow(initial_results) == 0L)) {
      results <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
      if (!is.null(judge)) results[[judge]] <- character()
    } else {
      results <- .validate_judge_results(initial_results, ids = ids, judge_col = judge)
      results <- tibble::as_tibble(results)
      results <- .add_pair_key_direction(results)
    }
  }

  # bootstrap scoring step (if no initial results)
  if (is.null(resume_from)) {
    pairs_bootstrap <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
  }

  if (is.null(resume_from) && nrow(results) == 0L && init_round_size > 0L) {
    pairs_bootstrap <- .bootstrap_pairs(
      n_pairs = init_round_size,
      existing_pairs = results,
      seed = seed_pairs
    )

    if (nrow(pairs_bootstrap) > 0L) {
      res0 <- .coerce_judge_output(judge_fun(pairs_bootstrap))
      res0 <- .validate_judge_results(res0, ids = ids, judge_col = judge)
      res0 <- .add_pair_key_direction(res0)
      results <- dplyr::bind_rows(results, res0)
      results <- .add_pair_key_direction(results)
    }
  }

  if (is.null(resume_from)) {
    fits <- list()
    final_fit <- NULL
    prev_metrics <- NULL
    stop_reason <- NA_character_
    stop_round <- NA_integer_
  }

  rounds_list <- list()
  state_list <- list()
  pairing_diag_list <- list()

  .make_checkpoint_payload <- function(next_round, completed = FALSE, out = NULL) {
    rounds_now <- dplyr::bind_rows(rounds_tbl_prev, if (length(rounds_list) == 0L) tibble::tibble() else dplyr::bind_rows(rounds_list))
    state_now <- dplyr::bind_rows(state_tbl_prev, if (length(state_list) == 0L) tibble::tibble() else dplyr::bind_rows(state_list))
    seed_now <- NULL
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      seed_now <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    list(
      run_type = "adaptive",
      ids = ids,
      created_at = as.character(Sys.time()),
      results = results,
      pairs_bootstrap = pairs_bootstrap,
      fits = if (isTRUE(checkpoint_store_fits)) fits else NULL,
      final_fit = if (isTRUE(checkpoint_store_fits)) final_fit else NULL,
      prev_metrics = prev_metrics,
      stop_reason = stop_reason,
      stop_round = stop_round,
      rounds = rounds_now,
      state = state_now,
      next_round = as.integer(next_round),
      random_seed = seed_now,
      completed = isTRUE(completed),
      out = out
    )
  }

  .write_checkpoint_now <- function(payload, round_index = NULL) {
    if (is.null(checkpoint_dir) || !nzchar(checkpoint_dir)) {
      return(invisible(NULL))
    }
    .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    if (!is.null(round_index)) {
      .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", round = round_index, overwrite = checkpoint_overwrite)
    }
    invisible(NULL)
  }

  # on exit (including errors/interrupts), write the last completed checkpoint
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

  round_seq <- if (start_round <= max_rounds) seq.int(from = start_round, to = max_rounds) else integer(0)
  for (r in round_seq) {
    if (nrow(results) == 0L) break

    bt_data <- if (is.null(judge)) {
      build_bt_fun(results, judge = NULL)
    } else {
      build_bt_fun(results, judge = judge)
    }

    fit_bt <- do.call(
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

    fit <- make_running_fit(bt_data, fit_bt)
    fit <- tag_fit(
      fit,
      round_index = r,
      stage = "round_fit",
      n_results = nrow(results),
      n_pairs_this_round = NA_integer_,
      stop_reason = NA_character_
    )
    fits[[length(fits) + 1L]] <- fit
    final_fit <- fit

    round_out <- bt_adaptive_round(
      samples = samples,
      fit = fit,
      existing_pairs = results,
      prev_metrics = prev_metrics,
      round_size = round_size,
      se_probs = se_probs,
      fit_bounds = fit_bounds,
      stopping_tier = stopping_tier,
      reliability_target = reliability_target,
      sepG_target = sepG_target,
      rel_se_p90_target = rel_se_p90_target,
      rel_se_p90_min_improve = rel_se_p90_min_improve,
      max_item_misfit_prop = max_item_misfit_prop,
      max_judge_misfit_prop = max_judge_misfit_prop,
      k_neighbors = k_neighbors,
      min_judgments = min_judgments,
      repeat_policy = repeat_policy,
      repeat_cap = repeat_cap,
      repeat_frac = repeat_frac,
      repeat_n = repeat_n,
      repeat_guard_min_degree = repeat_guard_min_degree,
      repeat_guard_largest_component_frac = repeat_guard_largest_component_frac,
      forbid_repeats = forbid_repeats,
      balance_positions = balance_positions,
      embedding_neighbors = embedding_neighbors,
      embed_far_k = embed_far_k,
      embed_quota_frac = embed_quota_frac,
      candidate_pool_cap = candidate_pool_cap,
      per_anchor_cap = per_anchor_cap,
      w_embed = w_embed,
      embed_score_mode = embed_score_mode,
      seed = if (is.null(seed_pairs)) NULL else (as.integer(seed_pairs) + as.integer(r))
    )

    metrics <- round_out$metrics
    decision <- round_out$decision
    pairs_next <- round_out$pairs_next

    diag_pairs_round <- NULL
    planned_repeat_pairs <- attr(pairs_next, "planned_repeat_pairs")
    diag_pairs <- attr(pairs_next, "pairing_diagnostics")
    if (!is.null(diag_pairs)) {
      diag_pairs_round <- dplyr::mutate(diag_pairs, round = as.integer(r))
    }

    this_reason <- .bt_resolve_stop_reason(
      stopped = isTRUE(decision$stop),
      reached_max_rounds = FALSE,
      max_rounds_is_zero = FALSE,
      round_size_zero = (round_size == 0L),
      no_pairs = (nrow(pairs_next) == 0L)
    )
    # If we are stopping before scoring new pairs, state reflects current results
    st_now <- .bt_round_state(results, ids = ids, judge_col = judge)
    st_now <- dplyr::mutate(
      st_now,
      round = as.integer(r),
      stop = isTRUE(decision$stop),
      stop_reason = this_reason
    )
    state_list[[length(state_list) + 1L]] <- st_now

    metrics_round <- dplyr::select(metrics, -dplyr::any_of(c("stop", "stop_reason")))

    rounds_list[[length(rounds_list) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(
        round = as.integer(r),
        n_new_pairs_scored = 0L,
        n_total_results = as.integer(nrow(results)),
        stop = isTRUE(decision$stop),
        stop_reason = this_reason
      ),
      metrics_round,
      .name_repair = "check_unique"
    )

    prev_metrics <- metrics_round

    if (!is.na(this_reason)) {
      stop_reason <- this_reason
      stop_round <- as.integer(r)

      fits[[length(fits)]] <- tag_fit(
        fits[[length(fits)]],
        round_index = r,
        stage = "round_fit",
        n_results = nrow(results),
        n_pairs_this_round = NA_integer_,
        stop_reason = this_reason
      )
      final_fit <- fits[[length(fits)]]

      checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = TRUE)
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
      break
    }

    res_next <- .coerce_judge_output(judge_fun(pairs_next))
    res_next <- .validate_judge_results(res_next, ids = ids, judge_col = judge)
    res_next <- .add_pair_key_direction(res_next)

    n_added <- nrow(res_next)
    if (n_added > 0L) {
      results <- dplyr::bind_rows(results, res_next)
      results <- .add_pair_key_direction(results)
      rounds_list[[length(rounds_list)]][["n_new_pairs_scored"]] <- as.integer(n_added)
      rounds_list[[length(rounds_list)]][["n_total_results"]] <- as.integer(nrow(results))

      # Finalize pairing diagnostics (repeat completion + consistency).
      if (!is.null(diag_pairs_round)) {
        planned_keys <- character()
        if (!is.null(planned_repeat_pairs) && is.data.frame(planned_repeat_pairs) && nrow(planned_repeat_pairs) > 0L) {
          planned_repeat_pairs <- tibble::as_tibble(planned_repeat_pairs)
          if ("pair_key" %in% names(planned_repeat_pairs)) {
            planned_keys <- as.character(planned_repeat_pairs$pair_key)
          } else if (all(c("ID1", "ID2") %in% names(planned_repeat_pairs))) {
            planned_keys <- .unordered_pair_key(planned_repeat_pairs$ID1, planned_repeat_pairs$ID2)
          }
          planned_keys <- unique(planned_keys)
        }

        n_repeat_completed <- 0L
        # Deterministic: use 0 when no repeats were completed in the round.
        repeat_consistency_round <- 0
        if (length(planned_keys) > 0L) {
          res_next_pk <- dplyr::mutate(res_next, pair_key = .unordered_pair_key(.data$ID1, .data$ID2))
          res_next_pk <- dplyr::filter(res_next_pk, .data$pair_key %in% planned_keys)
          res_next_pk <- dplyr::filter(res_next_pk, !is.na(.data$better_id) & nzchar(.data$better_id))

          n_repeat_completed <- nrow(res_next_pk)

          prior <- dplyr::filter(results, .data$pair_key %in% planned_keys)
          prior <- dplyr::filter(prior, !is.na(.data$better_id) & nzchar(.data$better_id))
          prior <- dplyr::group_by(prior, .data$pair_key)
          prior <- dplyr::slice_head(prior, n = 1L)
          prior <- dplyr::ungroup(prior)
          prior <- dplyr::select(prior, "pair_key", prior_better_id = "better_id")

          joined <- dplyr::inner_join(
            prior,
            dplyr::select(res_next_pk, "pair_key", new_better_id = "better_id"),
            by = "pair_key"
          )
          if (nrow(joined) > 0L) {
            repeat_consistency_round <- mean(joined$prior_better_id == joined$new_better_id)
          }
        }

        # Cumulative consistency among unordered pairs with exactly two completed judgments.
        # Deterministic: use 0 when no pairs have two completed judgments.
        repeat_consistency_cum <- 0
        by_key <- dplyr::filter(results, !is.na(.data$better_id) & nzchar(.data$better_id))
        by_key <- dplyr::group_by(by_key, .data$pair_key)
        by_key <- dplyr::summarise(by_key,
          n_done = dplyr::n(),
          n_unique_winner = dplyr::n_distinct(.data$better_id),
          .groups = "drop"
        )
        two_done <- dplyr::filter(by_key, .data$n_done == 2L)
        if (nrow(two_done) > 0L) {
          repeat_consistency_cum <- mean(two_done$n_unique_winner == 1L)
        }

        diag_pairs_round <- dplyr::mutate(
          diag_pairs_round,
          n_repeat_completed = as.integer(n_repeat_completed),
          repeat_consistency_rate_round = repeat_consistency_round,
          repeat_consistency_rate_cumulative = repeat_consistency_cum
        )
        pairing_diag_list[[length(pairing_diag_list) + 1L]] <- diag_pairs_round
      }

      # update the most recent state row to reflect post-append counts
      state_list[[length(state_list)]] <- dplyr::mutate(
        .bt_round_state(results, ids = ids, judge_col = judge),
        round = as.integer(r),
        stop = FALSE,
        stop_reason = NA_character_
      )

      # checkpoint bookkeeping updated at end-of-round (below)
    } else {
      this_reason <- .bt_resolve_stop_reason(no_new_results = TRUE)
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- this_reason
      stop_reason <- this_reason
      stop_round <- as.integer(r)

      fits[[length(fits)]] <- tag_fit(
        fits[[length(fits)]],
        round_index = r,
        stage = "round_fit",
        n_results = nrow(results),
        n_pairs_this_round = NA_integer_,
        stop_reason = "no_new_results"
      )
      final_fit <- fits[[length(fits)]]

      # mark state with terminal reason
      state_list[[length(state_list)]] <- dplyr::mutate(
        state_list[[length(state_list)]],
        stop = TRUE,
        stop_reason = "no_new_results"
      )

      checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = TRUE)
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
      break
    }

    # checkpoint after successfully finishing this round
    checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = FALSE)
    if ((r %% checkpoint_every) == 0L) {
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
    }
  }

  if (is.na(stop_reason)) {
    if (max_rounds == 0L) {
      stop_reason <- .bt_resolve_stop_reason(max_rounds_is_zero = TRUE)
      stop_round <- 0L
    } else if (start_round > max_rounds) {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      stop_round <- as.integer(max_rounds)
    } else if (round_size == 0L && init_round_size == 0L && (nrow(rounds_tbl_prev) + length(rounds_list) == 0L)) {
      stop_reason <- .bt_resolve_stop_reason(round_size_zero = TRUE)
      stop_round <- 0L
    } else if (nrow(rounds_tbl_prev) + length(rounds_list) == 0L) {
      stop_reason <- .bt_resolve_stop_reason(no_results = TRUE)
    } else {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      stop_round <- as.integer(max_rounds)
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- stop_reason
      if (length(fits) > 0L) {
        fits[[length(fits)]] <- tag_fit(
          fits[[length(fits)]],
          round_index = max_rounds,
          stage = "round_fit",
          n_results = nrow(results),
          n_pairs_this_round = NA_integer_,
          stop_reason = "max_rounds"
        )
        final_fit <- fits[[length(fits)]]
      }
      if (length(state_list) > 0L) {
        state_list[[length(state_list)]] <- dplyr::mutate(
          state_list[[length(state_list)]],
          stop = TRUE,
          stop_reason = "max_rounds"
        )
      }
    }
  }

  rounds_tbl <- dplyr::bind_rows(
    rounds_tbl_prev,
    if (length(rounds_list) == 0L) tibble::tibble() else dplyr::bind_rows(rounds_list)
  )
  state_tbl <- dplyr::bind_rows(
    state_tbl_prev,
    if (length(state_list) == 0L) tibble::tibble() else dplyr::bind_rows(state_list)
  )

  # ---- Normalize stopping columns (some early-exit paths can yield list-cols) ----
  if (!"stop_reason" %in% names(rounds_tbl)) {
    rounds_tbl$stop_reason <- NA_character_
  }
  if (is.list(rounds_tbl$stop_reason)) {
    rounds_tbl$stop_reason <- vapply(
      rounds_tbl$stop_reason,
      function(x) {
        if (is.null(x)) {
          return(NA_character_)
        }
        as.character(x)[1]
      },
      character(1)
    )
  } else {
    rounds_tbl$stop_reason <- as.character(rounds_tbl$stop_reason)
  }

  if (!"stop" %in% names(rounds_tbl)) {
    rounds_tbl$stop <- rep(FALSE, nrow(rounds_tbl))
  }
  if (is.list(rounds_tbl$stop)) {
    rounds_tbl$stop <- vapply(
      rounds_tbl$stop,
      function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        isTRUE(x)
      },
      logical(1)
    )
  } else {
    rounds_tbl$stop <- as.logical(rounds_tbl$stop)
  }
  rounds_tbl$stop[is.na(rounds_tbl$stop)] <- FALSE
  # If a stop_reason is recorded, the run stopped for that round.
  rounds_tbl$stop <- rounds_tbl$stop | !is.na(rounds_tbl$stop_reason)

  bt_data_final <- if (nrow(results) == 0L) {
    tibble::tibble(object1 = character(), object2 = character(), result = numeric())
  } else if (is.null(judge)) {
    build_bt_fun(results, judge = NULL)
  } else {
    build_bt_fun(results, judge = judge)
  }

  # Optional final refit / combined estimates table.
  estimates <- NULL
  final_models <- NULL
  theta <- NULL
  theta_engine <- NA_character_
  fit_provenance <- NULL

  final_refit_attempted <- FALSE
  final_refit_failed <- FALSE
  final_refit_disabled <- FALSE

  if (!isTRUE(final_refit) && nrow(results) > 0L) {
    final_refit_disabled <- TRUE
  }
  if (isTRUE(final_refit) && nrow(results) > 0L) {
    final_refit_attempted <- TRUE
    tmp <- tryCatch(
      compute_final_estimates(
        results = results,
        ids = ids,
        fit_engine_final = fit_engine_final,
        bt_bias_reduction = final_bt_bias_reduction,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping
      ),
      error = function(e) e
    )
    if (inherits(tmp, "error")) {
      final_refit_failed <- TRUE
      warning(
        "`final_refit = TRUE` requested but final estimates could not be computed: ",
        conditionMessage(tmp),
        call. = FALSE
      )
    } else {
      estimates <- tmp$estimates
      final_models <- list(
        bt_fit = tmp$bt_fit,
        rc_fit = tmp$rc_fit,
        diagnostics = tmp$diagnostics
      )

      fit_provenance <- tmp$provenance

      # Guarantee a simple top-level theta object for downstream convenience.
      # If a BT variant succeeded, use that; otherwise fall back to Rank Centrality.
      if (is.data.frame(estimates) && nrow(estimates) > 0L && identical(unique(estimates$bt_status), "succeeded")) {
        theta <- tibble::tibble(
          ID = as.character(estimates$ID),
          theta = as.double(estimates$theta_bt_firth),
          se = as.double(estimates$se_bt_firth),
          rank = as.integer(estimates$rank_bt_firth)
        )
        theta_engine <- as.character(unique(estimates$bt_engine_used))[1]
      } else if (is.data.frame(estimates) && nrow(estimates) > 0L) {
        theta <- tibble::tibble(
          ID = as.character(estimates$ID),
          theta = as.double(estimates$theta_rc),
          se = NA_real_,
          rank = as.integer(estimates$rank_rc)
        )
        theta_engine <- "rank_centrality"
      }
    }
  }

  # ---- PR4.1 fallback: ensure theta exists whenever we have a running fit ----
  if (is.null(theta) && length(fits) > 0L) {
    last_fit <- fits[[length(fits)]]
    fb <- .theta_from_last_running_fit(last_fit, ids = ids)

    if (!is.null(fb$theta)) {
      theta <- fb$theta
      theta_engine <- fb$engine

      if (is.null(fit_provenance)) fit_provenance <- list()
      fit_provenance$fallback_used <- TRUE
      fit_provenance$fallback_source <- "last_running_fit"

      fit_provenance$fallback_reason <- dplyr::case_when(
        final_refit_disabled ~ "final_refit_disabled",
        final_refit_failed ~ "final_refit_failed",
        final_refit_attempted ~ "final_refit_unavailable",
        TRUE ~ "final_refit_unavailable"
      )
    }
  }

  # optional post-stop reverse audit (unchanged)
  reverse_out <- NULL
  if (isTRUE(reverse_audit) && nrow(results) > 0L) {
    uniq_pairs <- results |>
      dplyr::filter(!is.na(.data$better_id)) |>
      dplyr::transmute(ID1 = as.character(.data$ID1), ID2 = as.character(.data$ID2))

    uniq_pairs <- .distinct_unordered_pairs(uniq_pairs, id1_col = "ID1", id2_col = "ID2")
    uniq_pairs_txt <- .add_pair_texts(uniq_pairs, samples = samples)

    n_all <- nrow(uniq_pairs_txt)
    k <- NULL
    if (!is.null(n_reverse)) {
      k <- as.integer(n_reverse)
      if (is.na(k) || k < 0L) stop("`n_reverse` must be a non-negative integer.", call. = FALSE)
      k <- min(k, n_all)
    } else {
      if (!is.numeric(reverse_pct) || length(reverse_pct) != 1L || is.na(reverse_pct)) {
        stop("`reverse_pct` must be a single numeric value when `n_reverse` is NULL.", call. = FALSE)
      }
      if (reverse_pct < 0 || reverse_pct > 1) {
        stop("`reverse_pct` must be between 0 and 1 (inclusive) when `n_reverse` is NULL.", call. = FALSE)
      }
      if (reverse_pct <= 0) k <- 0L else if (reverse_pct >= 1) k <- n_all else k <- as.integer(round(n_all * reverse_pct))
      k <- min(max(k, 0L), n_all)
    }

    rev_pairs <- uniq_pairs_txt[0, , drop = FALSE]
    if (k > 0L && n_all > 0L) {
      idx <- .with_seed_restore(reverse_seed, function() sample.int(n_all, size = k, replace = FALSE), arg_name = "reverse_seed")
      sel <- uniq_pairs_txt[idx, , drop = FALSE]
      rev_pairs <- tibble::tibble(
        ID1 = sel$ID2,
        text1 = sel$text2,
        ID2 = sel$ID1,
        text2 = sel$text1
      )
    }

    rev_results <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    if (!is.null(judge)) rev_results[[judge]] <- character()
    consistency <- NULL

    if (nrow(rev_pairs) > 0L) {
      rev_results <- .coerce_judge_output(judge_fun(rev_pairs))
      rev_results <- .validate_judge_results(rev_results, ids = ids, judge_col = judge)
      rev_results <- .add_pair_key_direction(rev_results)

      add_key <- function(df) {
        df |>
          dplyr::mutate(key = .unordered_pair_key(.data$ID1, .data$ID2))
      }

      main_audit <- add_key(results) |>
        dplyr::filter(!is.na(.data$better_id))

      rev_audit <- add_key(rev_results)

      main_audit <- dplyr::semi_join(
        main_audit,
        dplyr::select(rev_audit, "key"),
        by = "key"
      ) |>
        dplyr::select(-"key")

      rev_audit <- dplyr::select(rev_audit, -"key")

      consistency <- compute_reverse_consistency(main_audit, rev_audit)
    }

    reverse_out <- list(
      pairs_reversed = rev_pairs,
      reverse_results = rev_results,
      consistency = consistency
    )
  }

  pairing_diagnostics <- if (length(pairing_diag_list) == 0L) {
    tibble::tibble(
      round = integer(),
      n_candidates_raw = integer(),
      n_candidates_scored = integer(),
      n_candidates_after_constraints = integer(),
      n_candidates_after_caps = integer(),
      n_selected = integer(),
      n_repeat_eligible = integer(),
      n_repeat_planned = integer(),
      n_repeat_completed = integer(),
      repeat_consistency_rate_round = double(),
      repeat_consistency_rate_cumulative = double(),
      repeat_policy = character(),
      repeat_cap = integer(),
      repeat_frac = double(),
      repeat_n = integer(),
      repeat_quota_n = integer(),
      repeat_guard_passed = logical(),
      repeat_guard_min_degree = integer(),
      repeat_guard_largest_component_frac = double(),
      graph_degree_min = double(),
      graph_largest_component_frac = double(),
      n_selected_theta = integer(),
      n_selected_embed = integer(),
      n_selected_far = integer(),
      embed_neighbor_hit_rate = double(),
      embed_quota_frac = double(),
      candidate_pool_cap = integer(),
      per_anchor_cap = integer(),
      w_embed = double(),
      embed_score_mode = character()
    )
  } else {
    dplyr::bind_rows(pairing_diag_list)
  }

  out <- list(
    results = results,
    bt_data = bt_data_final,
    fits = fits,
    final_fit = final_fit,
    theta = theta,
    theta_engine = theta_engine,
    estimates = estimates,
    final_models = final_models,
    fit_provenance = fit_provenance,
    stop_reason = stop_reason,
    stop_round = stop_round,
    rounds = rounds_tbl,
    state = state_tbl,
    pairing_diagnostics = pairing_diagnostics,
    pairs_bootstrap = pairs_bootstrap,
    reverse_audit = reverse_out
  )

  # write a final checkpoint including the returned object
  if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
    next_round_final <- if (is.na(stop_round)) as.integer(nrow(rounds_tbl) + 1L) else as.integer(stop_round + 1L)
    checkpoint_payload_last <- .make_checkpoint_payload(next_round = next_round_final, completed = TRUE, out = out)
    .write_checkpoint_now(checkpoint_payload_last, round_index = stop_round)
  }

  .as_pairwise_run(out, run_type = "adaptive")
}

#' Simulate a judge for BT pairwise comparisons
#'
#' This helper produces synthetic pairwise comparison results for testing,
#' vignettes, and benchmarking. It takes a table of pairs (with texts) and returns
#' \code{better_id} outcomes based on a latent "true ability" vector.
#'
#' The simulator can be deterministic (always pick higher true ability) or
#' stochastic using a Bradley–Terry / logistic probability model.
#'
#' @param pairs A tibble/data.frame with columns \code{ID1}, \code{text1}, \code{ID2}, \code{text2}.
#' @param true_theta Named numeric vector of latent abilities. Names must include
#' the IDs used in \code{pairs}. Missing IDs are treated as 0.
#' @param judges Optional character vector of judge identifiers. If length > 1,
#' each row is assigned a judge (round-robin by default).
#' @param judge_col Optional character scalar column name to use for judge labels.
#' If \code{NULL} (default), no judge column is returned.
#' @param deterministic Logical; if \code{TRUE}, always choose the higher \code{true_theta}.
#' If equal, break ties at random (seed-controlled). Default \code{FALSE}.
#' @param seed Optional integer seed; RNG state is restored afterwards.
#' @param round_robin Logical; if \code{TRUE} (default) and \code{length(judges) > 1},
#' assign judges in repeating order. If \code{FALSE}, assign judges uniformly at random.
#'
#' @return A tibble with columns \code{ID1}, \code{ID2}, \code{better_id}, and optionally
#' a judge column.
#'
#' @examples
#' pairs <- tibble::tibble(
#'   ID1 = c("A", "B"),
#'   text1 = c("a", "b"),
#'   ID2 = c("C", "D"),
#'   text2 = c("c", "d")
#' )
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)
#'
#' @export
simulate_bt_judge <- function(pairs,
                              true_theta,
                              judges = "judge_1",
                              judge_col = NULL,
                              deterministic = FALSE,
                              seed = NULL,
                              round_robin = TRUE) {
  pairs <- tibble::as_tibble(pairs)
  req <- c("ID1", "text1", "ID2", "text2")
  miss <- setdiff(req, names(pairs))
  if (length(miss) > 0L) stop("`pairs` must contain columns: ", paste(req, collapse = ", "), call. = FALSE)

  if (!is.numeric(true_theta) || is.null(names(true_theta))) {
    stop("`true_theta` must be a named numeric vector.", call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L || is.na(judge_col) || nchar(judge_col) == 0L) {
      stop("`judge_col` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  judges <- as.character(judges)
  if (length(judges) < 1L || anyNA(judges) || any(judges == "")) {
    stop("`judges` must contain at least one non-missing, non-empty label.", call. = FALSE)
  }

  n <- nrow(pairs)
  if (n == 0L) {
    out0 <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    if (!is.null(judge_col)) out0[[judge_col]] <- character()
    return(out0)
  }

  id1 <- trimws(as.character(pairs$ID1))
  id2 <- trimws(as.character(pairs$ID2))

  th1 <- true_theta[id1]
  th2 <- true_theta[id2]
  th1[is.na(th1)] <- 0
  th2[is.na(th2)] <- 0

  out <- .with_seed_restore(
    seed,
    f = function() {
      better <- character(n)

      if (isTRUE(deterministic)) {
        for (i in seq_len(n)) {
          if (th1[i] > th2[i]) {
            better[i] <- id1[i]
          } else if (th2[i] > th1[i]) {
            better[i] <- id2[i]
          } else {
            better[i] <- if (stats::runif(1) < 0.5) id1[i] else id2[i]
          }
        }
      } else {
        for (i in seq_len(n)) {
          d <- th1[i] - th2[i]
          p <- 1 / (1 + exp(-d))
          better[i] <- if (stats::runif(1) < p) id1[i] else id2[i]
        }
      }

      out <- tibble::tibble(
        ID1 = id1,
        ID2 = id2,
        better_id = better
      )

      if (!is.null(judge_col)) {
        if (length(judges) == 1L) {
          out[[judge_col]] <- rep(judges[[1]], n)
        } else if (isTRUE(round_robin)) {
          out[[judge_col]] <- judges[((seq_len(n) - 1L) %% length(judges)) + 1L]
        } else {
          out[[judge_col]] <- judges[sample.int(length(judges), size = n, replace = TRUE)]
        }
      }

      out
    },
    arg_name = "seed"
  )

  out
}

#' Extract theta from the last available running fit
#'
#' Internal helper used by \code{bt_run_adaptive()} to guarantee that
#' \code{out$theta} exists even when final refitting is disabled or fails.
#'
#' This function attempts to extract a compact theta table
#' (\code{ID, theta, se, rank}) from the last running fit.
#'
#' @param final_fit A fit object produced during the adaptive loop
#'   (typically \code{fits[[length(fits)]]}), or \code{NULL}.
#' @param id_vec Character vector of item IDs in the model.
#'
#' @return A list with elements:
#' \describe{
#'   \item{theta}{A tibble with columns \code{ID, theta, se, rank}, or \code{NULL}.}
#'   \item{engine}{Character string naming the engine used (e.g., \code{"rank_centrality"}).}
#' }
#'
#' @keywords internal
.theta_from_last_running_fit <- function(final_fit, id_vec) {
  if (is.null(final_fit)) {
    return(list(theta = NULL, engine = NA_character_))
  }

  # Preferred: explicit theta table
  if (!is.null(final_fit$theta)) {
    theta_tbl <- final_fit$theta

    if (all(c("ID", "theta", "se", "rank") %in% names(theta_tbl))) {
      engine <- final_fit$engine_used %||%
        final_fit$engine_requested %||%
        "rank_centrality"

      return(list(theta = theta_tbl, engine = engine))
    }
  }

  # No usable theta found
  list(theta = NULL, engine = NA_character_)
}
