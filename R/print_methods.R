#' Print a pairwiseLLM run
#'
#' This is a minimal print method for objects returned by the `pairwiseLLM`
#' runner functions. It avoids dumping internals and instead shows a small,
#' stable "headline" view: runner type, number of results, stop reason/round,
#' theta engine, and (when available) the top few IDs by theta.
#'
#' @param x A run object returned by [bt_run_adaptive()],
#'   [bt_run_core_linking()], or [bt_run_adaptive_core_linking()] (or a
#'   similarly-structured list).
#' @param ... Passed to [base::print()].
#'
#' @return `x`, invisibly.
#'
#' @method print pairwiseLLM_run
#'
#' @examples
#' run <- list(
#'   results = tibble::tibble(ID = c("A", "B"), win = c(1L, 0L)),
#'   theta = tibble::tibble(ID = c("A", "B"), theta = c(0.5, -0.5)),
#'   estimates = NULL,
#'   pairing_diagnostics = NULL,
#'   stop_reason = "max_rounds",
#'   stop_round = 2L,
#'   theta_engine = "bt"
#' )
#' class(run) <- "pairwiseLLM_run"
#'
#' print(run)
#'
#' @export
print.pairwiseLLM_run <- function(x, ...) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a list-like run object.")
  }

  run_type <- .bt_infer_run_type(x)

  n_results <- NA_integer_
  if (!is.null(x$results) && is.data.frame(x$results)) {
    n_results <- nrow(x$results)
  }

  stop_tbl <- tryCatch(
    bt_stop_summary(x),
    error = function(e) {
      tibble::tibble(
        stop_reason = NA_character_,
        stop_round = NA_integer_,
        theta_engine = NA_character_,
        degree_min = NA_real_,
        largest_component_frac = NA_real_,
        rms_theta_delta = NA_real_,
        topk_overlap = NA_real_,
        stop_blocked_by = NA_character_,
        stop_blocked_candidates = NA_character_
      )
    }
  )

  stop_reason <- stop_tbl$stop_reason[[1]]
  stop_round <- stop_tbl$stop_round[[1]]
  theta_engine <- stop_tbl$theta_engine[[1]]

  if (is.na(stop_reason) || !nzchar(stop_reason)) stop_reason <- "<unknown>"
  if (is.na(theta_engine) || !nzchar(theta_engine)) theta_engine <- "<unknown>"

  cat("<pairwiseLLM run>\n")
  cat("  type:        ", run_type, "\n", sep = "")
  if (is.na(n_results)) {
    cat("  results:     <unknown>\n")
  } else {
    cat("  results:     ", n_results, " rows\n", sep = "")
  }

  if (is.na(stop_round)) {
    cat("  stop:        ", stop_reason, "\n", sep = "")
  } else {
    cat("  stop:        ", stop_reason, " (round ", stop_round, ")\n", sep = "")
  }
  cat("  theta engine:", theta_engine, "\n")

  theta_tbl <- tryCatch(bt_get_theta(x), error = function(e) NULL)
  if (!is.null(theta_tbl) && nrow(theta_tbl) > 0L && all(c("ID", "theta") %in% names(theta_tbl))) {
    # Avoid tidy-eval dependencies inside print; keep it base.
    ord <- order(-as.numeric(theta_tbl$theta), na.last = TRUE)
    top <- theta_tbl[ord, , drop = FALSE]
    top <- utils::head(top, 3L)
    ids <- as.character(top$ID)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) > 0L) {
      cat("  top theta:   ", paste(ids, collapse = ", "), "\n", sep = "")
    }
  }

  invisible(x)
}


# ---- internal helpers ------------------------------------------------------

.bt_infer_run_type <- function(run) {
  run_type <- attr(run, "run_type", exact = TRUE)
  if (!is.null(run_type) && !is.na(run_type) && nzchar(run_type)) {
    return(as.character(run_type))
  }

  # Heuristic fallback (for run-like objects created manually).
  if (is.list(run) && !is.null(run$bt_data)) {
    return("adaptive_core_linking")
  }
  if (is.list(run) && !is.null(run$core_ids) && !is.null(run$batches)) {
    return("core_linking")
  }
  "adaptive"
}
