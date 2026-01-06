#' Summarize a pairwiseLLM run object
#'
#' The runner functions in pairwiseLLM return a rich list of artifacts (results, fits,
#' per-round metrics, and state snapshots). \code{run_summary()} produces a compact,
#' human-readable summary and (when available) includes per-judge diagnostics.
#'
#' @param x A run object returned by \code{bt_run_adaptive()}, \code{bt_run_core_linking()},
#'   or \code{bt_run_adaptive_core_linking()} (or a similarly-structured list).
#' @param fit_bounds Numeric length-2 vector giving acceptable fit bounds for judge misfit
#'   classification; passed to \code{judge_fit_summary()}.
#' @param top_n Integer; number of worst judges to include in the judge fit summary.
#'
#' @return A list with class \code{"pairwiseLLM_run_summary"}.
#'
#' @examples
#' # A minimal run-like object
#' run <- list(
#'   results = tibble::tibble(
#'     ID1 = c("A", "B"),
#'     ID2 = c("B", "A"),
#'     better_id = c("A", "B")
#'   ),
#'   metrics = tibble::tibble(round = 1, reliability = 0.9),
#'   state = tibble::tibble(round = 1)
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' run_summary(run)
#'
#' @export
run_summary <- function(x, fit_bounds = c(0.7, 1.3), top_n = 5L) {
  .run_summary_impl(x, fit_bounds = fit_bounds, top_n = top_n)
}

#' @export
summary.pairwiseLLM_run <- function(object, ...) {
  run_summary(object, ...)
}

# Internal: add class to runner outputs without breaking list-ness
.as_pairwise_run <- function(x, run_type = NULL) {
  if (is.null(run_type)) run_type <- NA_character_
  attr(x, "run_type") <- run_type
  if (!inherits(x, "pairwiseLLM_run")) {
    class(x) <- unique(c("pairwiseLLM_run", class(x)))
  }
  x
}

# Internal: core summary logic
.run_summary_impl <- function(x, fit_bounds = c(0.7, 1.3), top_n = 5L) {
  run_type <- attr(x, "run_type", exact = TRUE)
  if (is.null(run_type) || is.na(run_type) || !nzchar(run_type)) {
    # Heuristic fallback
    if (is.list(x) && !is.null(x$bt_data)) {
      run_type <- "adaptive_core_linking"
    } else if (is.list(x) && !is.null(x$core_ids) && !is.null(x$batches)) {
      run_type <- "core_linking"
    } else {
      run_type <- "adaptive"
    }
  }

  results <- NULL
  if (is.list(x) && !is.null(x$results)) results <- x$results

  counts <- list(
    n_results = if (is.null(results)) 0L else nrow(results),
    n_unique_ids = NA_integer_,
    n_unique_unordered_pairs = NA_integer_
  )

  if (!is.null(results) && all(c("ID1", "ID2") %in% names(results))) {
    ids <- unique(c(as.character(results$ID1), as.character(results$ID2)))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    counts$n_unique_ids <- length(ids)

    ok <- !is.na(results$ID1) & !is.na(results$ID2)
    if (any(ok)) {
      a <- pmin(as.character(results$ID1[ok]), as.character(results$ID2[ok]))
      b <- pmax(as.character(results$ID1[ok]), as.character(results$ID2[ok]))
      counts$n_unique_unordered_pairs <- length(unique(paste0(a, "\u001f", b)))
    } else {
      counts$n_unique_unordered_pairs <- 0L
    }
  }

  # Rounds/state/metrics
  rounds <- list(n_rounds = NULL)
  metrics_last <- NULL
  state_last <- NULL

  if (is.list(x) && !is.null(x$metrics)) {
    metrics <- tibble::as_tibble(x$metrics)
    if (nrow(metrics) > 0L) metrics_last <- metrics[nrow(metrics), , drop = FALSE]
    if ("round" %in% names(metrics)) rounds$n_rounds <- max(metrics$round, na.rm = TRUE)
  }
  if (is.list(x) && !is.null(x$state)) {
    state <- tibble::as_tibble(x$state)
    if (nrow(state) > 0L) state_last <- state[nrow(state), , drop = FALSE]
    if (is.null(rounds$n_rounds) && ("round" %in% names(state))) rounds$n_rounds <- max(state$round, na.rm = TRUE)
  }

  # Stopping info (adaptive runner only)
  stopping <- list(stop_reason = NULL, stop_round = NULL)
  if (is.list(x) && !is.null(x$stop_reason)) {
    stopping$stop_reason <- x$stop_reason
    stopping$stop_round <- x$stop_round
  }

  # Judge summaries when available
  judge <- list(
    has_judges = FALSE,
    per_judge = NULL,
    fit = NULL
  )

  if (!is.null(results) && ("judge" %in% names(results))) {
    judge$has_judges <- TRUE
    judge$per_judge <- tryCatch(
      judge_summary(results, judge_col = "judge", compute_reverse = TRUE),
      error = function(e) NULL
    )
  }

  # Judge fit diagnostics if present
  final_fit <- NULL
  if (is.list(x) && !is.null(x$final_fit)) {
    final_fit <- x$final_fit
  } else if (is.list(x) && !is.null(x$final_fits) && length(x$final_fits) > 0L) {
    # use last batch fit
    final_fit <- x$final_fits[[length(x$final_fits)]]
  }

  if (!is.null(final_fit)) {
    # Try to extract judge fit from typical structures
    jf <- NULL
    if (is.list(final_fit) && !is.null(final_fit$diagnostics) && !is.null(final_fit$diagnostics$judge_fit)) {
      jf <- final_fit$diagnostics$judge_fit
    } else if (is.list(final_fit) && !is.null(final_fit$fit) && !is.null(final_fit$fit$fit_judges)) {
      jf <- final_fit$fit$fit_judges
    }
    if (!is.null(jf)) {
      judge$fit <- tryCatch(
        judge_fit_summary(jf, fit_bounds = fit_bounds, top_n = top_n),
        error = function(e) NULL
      )
    }
  }

  out <- list(
    run_type = run_type,
    counts = counts,
    rounds = rounds,
    stopping = stopping,
    metrics_last = metrics_last,
    state_last = state_last,
    judge = judge
  )
  class(out) <- c("pairwiseLLM_run_summary", "list")
  out
}

#' @export
print.pairwiseLLM_run_summary <- function(x, ...) {
  cat("<pairwiseLLM run summary>\n")
  cat("  type:        ", x$run_type, "\n", sep = "")
  cat("  results:     ", x$counts$n_results, " rows\n", sep = "")
  if (!is.na(x$counts$n_unique_ids)) {
    cat("  unique IDs:  ", x$counts$n_unique_ids, "\n", sep = "")
  }
  if (!is.na(x$counts$n_unique_unordered_pairs)) {
    cat("  unique pairs:", x$counts$n_unique_unordered_pairs, " (unordered)\n", sep = " ")
  }
  if (!is.null(x$rounds$n_rounds)) {
    cat("  rounds:      ", x$rounds$n_rounds, "\n", sep = "")
  }
  if (!is.null(x$stopping$stop_reason)) {
    cat(
      "  stop:        ",
      x$stopping$stop_reason,
      " (round ",
      x$stopping$stop_round,
      ")\n",
      sep = ""
    )
  }

  if (isTRUE(x$judge$has_judges) &&
    is.list(x$judge$per_judge) &&
    is.data.frame(x$judge$per_judge$by_judge)) {
    cat("  judges:      ", nrow(x$judge$per_judge$by_judge), "\n", sep = "")
  }

  invisible(x)
}
