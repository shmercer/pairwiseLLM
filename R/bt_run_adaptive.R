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
#' \strong{Checkpointing \& resuming:} If \code{checkpoint_dir} is provided, this
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
                            forbid_repeats = TRUE,
                            balance_positions = TRUE,
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

  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }

  ids <- as.character(samples$ID)
  if (length(ids) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

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
      res0 <- judge_fun(pairs_bootstrap)
      res0 <- .validate_judge_results(res0, ids = ids, judge_col = judge)
      results <- dplyr::bind_rows(results, res0)
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

    fit <- fit_fun(
      bt_data,
      engine = engine,
      verbose = fit_verbose,
      return_diagnostics = return_diagnostics,
      include_residuals = include_residuals,
      ...
    )

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
      forbid_repeats = forbid_repeats,
      balance_positions = balance_positions,
      seed = if (is.null(seed_pairs)) NULL else (as.integer(seed_pairs) + as.integer(r))
    )

    metrics <- round_out$metrics
    decision <- round_out$decision
    pairs_next <- round_out$pairs_next

    this_reason <- NA_character_
    if (isTRUE(decision$stop)) {
      this_reason <- "stopped"
    } else if (round_size == 0L) {
      this_reason <- "round_size_zero"
    } else if (nrow(pairs_next) == 0L) {
      this_reason <- "no_pairs"
    }

    # If we are stopping before scoring new pairs, state reflects current results
    st_now <- .bt_round_state(results, ids = ids, judge_col = judge)
    st_now <- dplyr::mutate(st_now, round = as.integer(r), stop = isTRUE(decision$stop), stop_reason = this_reason)
    state_list[[length(state_list) + 1L]] <- st_now

    rounds_list[[length(rounds_list) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(
        round = as.integer(r),
        n_new_pairs_scored = 0L,
        n_total_results = as.integer(nrow(results)),
        stop = isTRUE(decision$stop),
        stop_reason = this_reason
      ),
      metrics
    )

    prev_metrics <- metrics

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

    res_next <- judge_fun(pairs_next)
    res_next <- .validate_judge_results(res_next, ids = ids, judge_col = judge)

    n_added <- nrow(res_next)
    if (n_added > 0L) {
      results <- dplyr::bind_rows(results, res_next)
      rounds_list[[length(rounds_list)]][["n_new_pairs_scored"]] <- as.integer(n_added)
      rounds_list[[length(rounds_list)]][["n_total_results"]] <- as.integer(nrow(results))

      # update the most recent state row to reflect post-append counts
      state_list[[length(state_list)]] <- dplyr::mutate(
        .bt_round_state(results, ids = ids, judge_col = judge),
        round = as.integer(r),
        stop = FALSE,
        stop_reason = NA_character_
      )

      # checkpoint bookkeeping updated at end-of-round (below)
    } else {
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- "no_new_results"
      stop_reason <- "no_new_results"
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
    if (start_round > max_rounds) {
      stop_reason <- "max_rounds"
      stop_round <- as.integer(max_rounds)
    } else if (max_rounds == 0L) {
      stop_reason <- "max_rounds"
      stop_round <- 0L
    } else if (nrow(rounds_tbl_prev) + length(rounds_list) == 0L) {
      stop_reason <- "no_results"
    } else {
      stop_reason <- "max_rounds"
      stop_round <- as.integer(max_rounds)
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- "max_rounds"

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

  bt_data_final <- if (nrow(results) == 0L) {
    tibble::tibble(object1 = character(), object2 = character(), result = numeric())
  } else if (is.null(judge)) {
    build_bt_fun(results, judge = NULL)
  } else {
    build_bt_fun(results, judge = judge)
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
      rev_results <- judge_fun(rev_pairs)
      rev_results <- .validate_judge_results(rev_results, ids = ids, judge_col = judge)

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

  out <- list(
    results = results,
    bt_data = bt_data_final,
    fits = fits,
    final_fit = final_fit,
    stop_reason = stop_reason,
    stop_round = stop_round,
    rounds = rounds_tbl,
    state = state_tbl,
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
