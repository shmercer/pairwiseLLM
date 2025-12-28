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
#' @param linking_method Linking method. Currently only \code{"mean_sd"} is supported,
#'   which applies an affine transform to match the mean and standard deviation of
#'   core thetas to the baseline core fit.
#' @param linking_cor_target In \code{linking = "auto"}, apply linking when the core
#'   Pearson correlation between baseline and current raw thetas is below this value.
#' @param linking_p90_abs_shift_target In \code{linking = "auto"}, apply linking when the
#'   90th percentile of the absolute core-theta shift (baseline vs current raw) exceeds
#'   this value.
#' @param linking_max_abs_shift_target In \code{linking = "auto"}, apply linking when the
#'   maximum absolute core-theta shift (baseline vs current raw) exceeds this value.
#' @param linking_min_n Minimum number of core IDs required to estimate the linking
#'   transform. If fewer are available, linking is skipped.
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
#' @param round_size Target number of pairs proposed per round (per batch).
#' @param max_rounds_per_batch Maximum rounds to run for each batch.
#' @param within_batch_frac Fraction of each round allocated to new<->new comparisons.
#' @param core_audit_frac Fraction of each round allocated to core<->core audit comparisons.
#' @param allocation_fun Optional function to update `within_batch_frac` and/or `core_audit_frac`
#'   between rounds. It is called after metrics are computed each round with a state list containing
#'   (at minimum) `batch_index`, `round_index`, `within_batch_frac`, `core_audit_frac`, `metrics`,
#'   and `prev_metrics`. It should return NULL (no change) or a list with elements
#'   `within_batch_frac` and/or `core_audit_frac`.
#' @param k_neighbors Passed to \code{\link{select_core_link_pairs}}.
#' @param min_judgments Passed to \code{\link{select_core_link_pairs}}.
#' @param forbid_repeats Forbid repeat unordered pairs across the entire run.
#' @param balance_positions Balance positions (ID1 vs ID2) when proposing pairs.
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
#' @param ... Additional arguments forwarded to \code{fit_fun}.
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
                                linking_method = c("mean_sd"),
                                linking_cor_target = 0.98,
                                linking_p90_abs_shift_target = 0.15,
                                linking_max_abs_shift_target = 0.30,
                                linking_min_n = 3L,
                                judge_fun,
                                initial_results = NULL,
                                judge = NULL,
                                fit_fun = fit_bt_model,
                                build_bt_fun = build_bt_data,
                                engine = "sirt",
                                fit_verbose = FALSE,
                                return_diagnostics = TRUE,
                                include_residuals = FALSE,
                                round_size = 50,
                                max_rounds_per_batch = 50,
                                within_batch_frac = 0.25,
                                core_audit_frac = 0.10,
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
                                core_theta_cor_target = NA_real_,
                                core_theta_spearman_target = NA_real_,
                                core_max_abs_shift_target = NA_real_,
                                core_p90_abs_shift_target = NA_real_,
                                drift_reference = c("previous_round", "baseline"),
                                seed = NULL,
                                verbose = TRUE,
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
  core_method <- match.arg(core_method)

  linking <- match.arg(linking)
  linking_method <- match.arg(linking_method)
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
  stop_params <- bt_stop_tiers()[[stopping_tier]]

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

  compute_fit <- function(res_tbl) {
    bt_data <- if (is.null(judge)) build_bt_fun(res_tbl) else build_bt_fun(res_tbl, judge = judge)
    fit_fun(
      bt_data,
      engine = engine,
      verbose = fit_verbose,
      return_diagnostics = return_diagnostics,
      include_residuals = include_residuals,
      ...
    )
  }

  apply_linking <- function(fit, reference_fit) {
    if (is.null(fit) || is.null(fit$theta) || is.null(reference_fit) || is.null(reference_fit$theta)) {
      return(fit)
    }

    drift_tbl <- bt_drift_metrics(
      current = fit,
      previous = reference_fit,
      ids = core_ids,
      prefix = "core_"
    )

    do_apply <- FALSE
    reason <- NA_character_
    if (linking == "never") {
      do_apply <- FALSE
      reason <- "never"
    } else if (linking == "always") {
      do_apply <- TRUE
      reason <- "always"
    } else if (linking == "auto") {
      do_apply <- .bt_should_apply_linking(
        drift_tbl,
        cor_target = linking_cor_target,
        p90_abs_shift_target = linking_p90_abs_shift_target,
        max_abs_shift_target = linking_max_abs_shift_target
      )
      reason <- if (isTRUE(do_apply)) "auto_trigger" else "auto_no_trigger"
    }

    fit$linking <- list(
      mode = linking,
      method = linking_method,
      applied = isTRUE(do_apply),
      reason = reason,
      reference = "baseline",
      trigger_thresholds = list(
        cor_target = linking_cor_target,
        p90_abs_shift_target = linking_p90_abs_shift_target,
        max_abs_shift_target = linking_max_abs_shift_target
      ),
      drift = drift_tbl
    )

    if (!isTRUE(do_apply)) {
      return(fit)
    }

    lk <- bt_link_thetas(
      current = fit,
      reference = reference_fit,
      ids = core_ids,
      method = linking_method,
      min_n = linking_min_n
    )

    fit$linking$a <- lk$a
    fit$linking$b <- lk$b
    fit$linking$n_core <- lk$n_core

    fit$theta <- dplyr::left_join(
      tibble::as_tibble(fit$theta),
      dplyr::select(lk$theta, dplyr::all_of(c("ID", "theta_linked", "se_linked"))),
      by = "ID"
    )

    fit
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

  fits <- list()
  final_fits <- list()
  metrics_hist <- tibble::tibble()
  state_hist <- tibble::tibble()
  batch_summary <- tibble::tibble()

  current_fit <- NULL
  baseline_fit <- NULL

  if (nrow(results) > 0L) {
    current_fit <- compute_fit(results)
    baseline_fit <- current_fit
    current_fit <- apply_linking(current_fit, baseline_fit)
    current_fit <- tag_fit(current_fit, 0L, 0L, "warm_start", nrow(results), 0L, character(0))
    fits[[length(fits) + 1L]] <- current_fit
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

    boot_res <- judge_fun(boot$pairs)
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
    baseline_fit <- current_fit
    current_fit <- apply_linking(current_fit, baseline_fit)
    current_fit <- tag_fit(current_fit, 0L, 1L, "bootstrap", nrow(results), nrow(boot$pairs), character(0))
    fits[[length(fits) + 1L]] <- current_fit
    final_fits[["bootstrap"]] <- current_fit

    st0 <- .bt_round_state(results, ids = ids_all, judge_col = judge)
    st0 <- dplyr::mutate(st0, batch_index = 0L, round_index = 1L, stage = "bootstrap", stop_reason = NA_character_)
    state_hist <- dplyr::bind_rows(state_hist, st0)
  }

  seen_ids <- unique(core_ids)

  for (batch_i in seq_along(batches)) {
    batch_ids <- unique(as.character(batches[[batch_i]]))
    new_ids <- setdiff(batch_ids, seen_ids)

    if (isTRUE(verbose)) {
      message("Batch ", batch_i, ": ", length(new_ids), " new IDs (", length(batch_ids), " requested).")
    }

    stop_reason <- NA_character_
    rounds_used <- 0L
    prev_metrics <- NULL

    if (length(new_ids) == 0L) {
      stop_reason <- "no_new_ids"
      batch_summary <- dplyr::bind_rows(
        batch_summary,
        tibble::tibble(
          batch_index = batch_i,
          n_requested = length(batch_ids),
          n_new = 0L,
          rounds_used = 0L,
          stop_reason = stop_reason
        )
      )
      final_fits[[paste0("batch_", batch_i)]] <- current_fit
      next
    }

    for (round_i in seq_len(max_rounds_per_batch)) {
      rounds_used <- round_i

      theta_for_pairs <- current_fit$theta
      if (is.null(theta_for_pairs)) {
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
      if (nrow(pairs) == 0L) {
        stop_reason <- "no_pairs"
        break
      }

      res_round <- judge_fun(pairs)
      res_round <- .validate_judge_results(res_round, ids = ids_all, judge_col = judge)

      pair_meta <- dplyr::distinct(
        dplyr::select(pairs, dplyr::all_of(c("ID1", "ID2", "pair_type"))),
        dplyr::across(dplyr::all_of(c("ID1", "ID2"))),
        .keep_all = TRUE
      )

      res_round <- dplyr::left_join(res_round, pair_meta, by = c("ID1", "ID2"))
      res_round <- dplyr::mutate(res_round, batch_index = batch_i, round_index = round_i)
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
      current_fit <- tag_fit(current_fit, as.integer(batch_i), as.integer(round_i), "batch_round", nrow(results), nrow(pairs), new_ids)
      fits[[length(fits) + 1L]] <- current_fit

      st_all <- .bt_round_state(results, ids = ids_all, judge_col = judge)
      st_new <- .bt_round_state(results, ids = new_ids, judge_col = judge, prefix = "new_")
      st <- dplyr::bind_cols(st_all, st_new)
      st <- dplyr::mutate(st, batch_index = as.integer(batch_i), round_index = as.integer(round_i), stage = "batch_round")
      state_hist <- dplyr::bind_rows(state_hist, st)

      metrics <- bt_stop_metrics(
        fit = current_fit,
        ids = new_ids,
        prev_fit = prev_fit_for_drift,
        core_ids = core_ids,
        se_probs = se_probs,
        fit_bounds = fit_bounds
      )

      metrics <- dplyr::mutate(metrics,
        batch_index = batch_i,
        round_index = round_i,
        within_batch_frac = within_batch_frac_this,
        core_audit_frac = core_audit_frac_this,
        n_pairs_proposed = nrow(pairs),
        n_core_new = sum(pairs$pair_type == "core_new"),
        n_new_new = sum(pairs$pair_type == "new_new"),
        n_core_core = sum(pairs$pair_type == "core_core")
      )
      metrics_hist <- dplyr::bind_rows(metrics_hist, metrics)

      prev_metrics_for_state <- prev_metrics

      stop_dec <- do.call(
        bt_should_stop,
        c(list(metrics = metrics, prev_metrics = prev_metrics), stop_params)
      )

      prev_metrics <- metrics

      if (!isTRUE(stop_dec$stop) && !is.null(allocation_fun)) {
        alloc_state <- list(
          stage = "batch_round",
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

      if (isTRUE(stop_dec$stop)) {
        stop_reason <- "stopped"
        state_hist[nrow(state_hist), "stop_reason"] <- "stopped"
        break
      }
    }

    if (is.na(stop_reason)) stop_reason <- "max_rounds"
    if (stop_reason == "max_rounds" && nrow(state_hist) > 0L) {
      # mark last state row of this batch as max_rounds
      idx <- which(state_hist$batch_index == batch_i)
      if (length(idx) > 0L) state_hist[idx[length(idx)], "stop_reason"] <- "max_rounds"
    }

    seen_ids <- unique(c(seen_ids, new_ids))

    batch_summary <- dplyr::bind_rows(
      batch_summary,
      tibble::tibble(
        batch_index = batch_i,
        n_requested = length(batch_ids),
        n_new = length(new_ids),
        rounds_used = rounds_used,
        stop_reason = stop_reason
      )
    )

    final_fits[[paste0("batch_", batch_i)]] <- current_fit
  }

  list(
    core_ids = core_ids,
    batches = batches,
    results = results,
    fits = fits,
    final_fits = final_fits,
    metrics = metrics_hist,
    state = state_hist,
    batch_summary = batch_summary
  )
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
