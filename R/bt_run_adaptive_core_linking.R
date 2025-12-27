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
#' is \code{NULL}. One of \code{"random"}, \code{"token_stratified"}, or
#' \code{"embeddings"}.
#' @param core_size Optional integer core size passed to \code{\link{select_core_set}}.
#' @param core_pct Optional numeric core proportion passed to \code{\link{select_core_set}}.
#' @param embeddings Optional embedding matrix passed to \code{\link{select_core_set}} when
#' \code{core_method="embeddings"}.
#' @param embeddings_metric Distance metric for embeddings selection. Passed to
#' \code{\link{select_core_set}}.
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
#' @param round_size Integer. Number of new pairs to propose and score per round within each
#' batch.
#' @param init_round_size Integer. Number of bootstrap pairs to score on the core set before
#' processing batches, when no \code{initial_results} are supplied. Default \code{round_size}.
#' @param max_rounds_per_batch Integer. Maximum number of rounds per batch. Default \code{50}.
#'
#' @param within_batch_frac Numeric in \code{[0,1]}. Fraction of non-audit pairs allocated to new\eqn{\leftrightarrow}new
#' within-batch comparisons (passed to \code{\link{select_core_link_pairs}}).
#' @param core_audit_frac Numeric in \code{[0,1]}. Fraction of pairs allocated to core\eqn{\leftrightarrow}core audits
#' (passed to \code{\link{select_core_link_pairs}}).
#'
#' @param k_neighbors Integer. Passed to \code{\link{select_core_link_pairs}}.
#' @param min_judgments Integer. Passed to \code{\link{select_core_link_pairs}}.
#' @param forbid_repeats Logical. Passed to \code{\link{select_core_link_pairs}}.
#' @param balance_positions Logical. Passed to \code{\link{select_core_link_pairs}}.
#'
#' @param seed_pairs Optional integer seed used for bootstrap pair sampling and for each round
#' as \code{seed_pairs + batch_index*1000 + round_index}. RNG state is restored afterward.
#'
#' @param se_probs Numeric vector of probabilities in (0,1) for SE quantiles (passed to
#' \code{\link{bt_stop_metrics}}).
#' @param fit_bounds Numeric length-2 vector giving acceptable infit/outfit bounds (passed to
#' @param stopping_tier Preset stopping thresholds to use (good/strong/very_strong).
#' \code{\link{bt_stop_metrics}}).
#'
#' @param reliability_target,sepG_target,rel_se_p90_target,rel_se_p90_min_improve,max_item_misfit_prop,max_judge_misfit_prop
#' Stopping thresholds passed to \code{\link{bt_should_stop}}.
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
#' @param ... Additional arguments passed through to \code{fit_fun}.
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
                                         core_method = c("random", "token_stratified", "embeddings"),
                                         core_size = NULL,
                                         core_pct = NULL,
                                         embeddings = NULL,
                                         embeddings_metric = c("euclidean", "cosine"),
                                         seed_core = NULL,
                                         initial_results = NULL,
                                         judge = NULL,
                                         engine = "sirt",
                                         fit_verbose = FALSE,
                                         return_diagnostics = TRUE,
                                         include_residuals = FALSE,
                                         round_size = 50,
                                         init_round_size = round_size,
                                         max_rounds_per_batch = 50,
                                         within_batch_frac = 0.25,
                                         core_audit_frac = 0.05,
                                         k_neighbors = 10,
                                         min_judgments = 12,
                                         forbid_repeats = TRUE,
                                         balance_positions = TRUE,
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
                                         core_theta_cor_target = NA_real_,
                                         core_theta_spearman_target = NA_real_,
                                         core_max_abs_shift_target = NA_real_,
                                         core_p90_abs_shift_target = NA_real_,
                                         fit_fun = fit_bt_model,
                                         build_bt_fun = build_bt_data,
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

  # Core selection
  if (is.null(core_ids)) {
    core_method <- match.arg(core_method)
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

  # Internal helper: fit on current results
  fit_from_results <- function(res) {
    bt_data <- build_bt_fun(res, judge = judge)
    if (nrow(bt_data) == 0L) {
      return(list(bt_data = bt_data, fit = NULL))
    }
    fit <- fit_fun(
      bt_data,
      engine = engine,
      verbose = fit_verbose,
      return_diagnostics = return_diagnostics,
      include_residuals = include_residuals,
      ...
    )
    list(bt_data = bt_data, fit = fit)
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

    # new-id restricted summaries
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

  # Do we need drift gating?
  drift_active <- any(!is.na(c(
    core_theta_cor_target,
    core_theta_spearman_target,
    core_max_abs_shift_target,
    core_p90_abs_shift_target
  )))

  fits <- list()
  final_fits <- list()
  metrics_rows <- list()
  state_rows <- list()

  # Bootstrap fit if needed
  current_fit <- NULL
  bootstrap_fit <- NULL
  batch_summary <- tibble::tibble()

  if (nrow(results) == 0L) {
    init_round_size <- as.integer(init_round_size)
    if (is.na(init_round_size) || init_round_size < 0L) {
      stop("`init_round_size` must be a non-negative integer.", call. = FALSE)
    }
    if (init_round_size > 0L) {
      core_samples <- dplyr::filter(samples, .data$ID %in% core_ids)
      core_pairs <- make_pairs(core_samples)
      core_pairs <- sample_pairs(core_pairs, n_pairs = min(init_round_size, nrow(core_pairs)), seed = seed_pairs)

      judged0 <- judge_fun(core_pairs)
      judged0 <- .validate_judge_results(judged0, ids = ids_all, judge_col = judge)
      judged0$stage <- "bootstrap"
      judged0$batch_index <- 0L
      judged0$round_index <- 0L
      results <- dplyr::bind_rows(results, judged0)
    }

    fr <- fit_from_results(results)
    if (!is.null(fr$fit)) {
      current_fit <- fr$fit
      bootstrap_fit <- fr$fit
      attr(current_fit, "bt_run_adaptive_core_linking") <- list(stage = "bootstrap", batch_index = 0L, round_index = 0L)
      fits[[length(fits) + 1L]] <- current_fit
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
  }

  if (is.null(current_fit)) {
    stop("No BT fit could be computed (no non-missing results).", call. = FALSE)
  }

  # Process each batch
  seen_ids <- core_ids
  for (b in seq_along(batches)) {
    batch_ids <- batches[[b]]
    new_ids <- setdiff(batch_ids, seen_ids)
    seen_ids <- unique(c(seen_ids, batch_ids))

    # If this batch adds no new IDs, record and continue.
    if (length(new_ids) == 0L) {
      final_fits[[paste0("batch", b)]] <- current_fit
      batch_summary <- dplyr::bind_rows(batch_summary, tibble::tibble(
        batch_index = as.integer(b),
        n_new_ids = 0L,
        rounds_used = 0L,
        stop_reason = "no_new_ids",
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

    for (r in seq_len(max_rounds_per_batch)) {
      rounds_used <- r

      # Propose pairs
      seed_this <- if (is.null(seed_pairs)) NULL else as.integer(seed_pairs + b * 1000L + r)

      prop <- bt_core_link_round(
        samples = samples,
        fit = current_fit,
        core_ids = core_ids,
        new_ids = new_ids,
        round_size = round_size,
        within_batch_frac = within_batch_frac,
        core_audit_frac = core_audit_frac,
        k_neighbors = k_neighbors,
        min_judgments = min_judgments,
        existing_pairs = results,
        forbid_repeats = forbid_repeats,
        balance_positions = balance_positions,
        seed = seed_this,
        include_text = TRUE
      )

      pairs_next <- prop$pairs

      if (nrow(pairs_next) == 0L) {
        stop_reason <- "no_pairs"
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

      judged <- judge_fun(pairs_next)
      judged <- .validate_judge_results(judged, ids = ids_all, judge_col = judge)

      # Attach pair_type + metadata
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
      if (is.null(current_fit)) {
        stop_reason <- "no_results"
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

      attr(current_fit, "bt_run_adaptive_core_linking") <- list(stage = "round", batch_index = b, round_index = r, new_ids = new_ids)
      fits[[length(fits) + 1L]] <- current_fit

      # Compute metrics on new IDs (and optional drift)
      m <- bt_stop_metrics(
        current_fit,
        ids = new_ids,
        prev_fit = prev_fit_for_drift,
        core_ids = if (drift_active) core_ids else NULL,
        se_probs = se_probs,
        fit_bounds = fit_bounds
      )

      decision <- do.call(
        bt_should_stop,
        c(list(metrics = m, prev_metrics = prev_metrics), stop_params)
      )

      stop_now <- isTRUE(decision$stop)
      stop_reason <- if (stop_now) "stopped" else NA_character_

      m$batch_index <- as.integer(b)
      m$round_index <- as.integer(r)
      m$stage <- "round"
      m$stop <- stop_now
      m$stop_reason <- stop_reason
      m$n_pairs_proposed <- nrow(pairs_next)
      m$n_core_new <- sum(pairs_next$pair_type == "core_new")
      m$n_new_new <- sum(pairs_next$pair_type == "new_new")
      m$n_core_core <- sum(pairs_next$pair_type == "core_core")

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

      if (stop_now) {
        break
      }

      prev_metrics <- m
      if (r == max_rounds_per_batch) {
        stop_reason <- "max_rounds"
      }
    }

    if (is.na(stop_reason)) {
      stop_reason <- "max_rounds"
    }

    # Save final fit for batch
    final_name <- paste0("batch", b)
    final_fits[[final_name]] <- current_fit

    batch_summary_row <- tibble::tibble(
      batch_index = as.integer(b),
      n_new_ids = length(new_ids),
      rounds_used = as.integer(rounds_used),
      stop_reason = stop_reason,
      n_results_total = nrow(results)
    )
    batch_summary <- dplyr::bind_rows(batch_summary, batch_summary_row)
  }

  bt_data <- build_bt_fun(results, judge = judge)

  metrics <- if (length(metrics_rows) == 0L) {
    tibble::tibble()
  } else {
    dplyr::bind_rows(metrics_rows)
  }

  state <- dplyr::bind_rows(state_rows)

  list(
    core_ids = core_ids,
    batches = batches,
    results = results,
    bt_data = bt_data,
    fits = fits,
    final_fits = final_fits,
    metrics = metrics,
    batch_summary = batch_summary,
    state = state
  )
}
