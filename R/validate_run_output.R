#' Validate a pairwiseLLM run output object
#'
#' This is an internal, pragmatic validator for runner outputs. It enforces that
#' required fields exist and have stable, runner-agnostic types. Extra fields are
#' allowed for forward compatibility.
#'
#' @param x A list-like run output (typically returned by a runner) prior to
#'   class decoration.
#' @param strict Logical. If TRUE, treat some warnings as errors.
#'
#' @return Invisibly returns `x` if valid; otherwise errors. May emit warnings
#'   when `strict = FALSE`.
#' @keywords internal
validate_pairwise_run_output <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    rlang::abort("Run output must be a list.")
  }

  required_fields <- c(
    "results",
    "estimates",
    "theta",
    "theta_engine",
    "fit_provenance",
    "stop_reason",
    "stop_round",
    "pairing_diagnostics"
  )

  missing <- setdiff(required_fields, names(x))
  if (length(missing) > 0L) {
    rlang::abort(paste0("Run output is missing required field(s): ", paste(missing, collapse = ", "), "."))
  }

  # --- helpers --------------------------------------------------------------
  is_scalar_chr <- function(z) is.character(z) && length(z) == 1L
  is_scalar_int <- function(z) is.integer(z) && length(z) == 1L

  assert_tibble_or_null <- function(z, name) {
    if (!is.null(z) && !inherits(z, "tbl_df")) {
      rlang::abort(sprintf("`%s` must be a tibble or NULL.", name))
    }
  }

  assert_scalar_chr_na <- function(z, name) {
    if (!is_scalar_chr(z)) {
      rlang::abort(sprintf("`%s` must be a scalar character (or NA_character_).", name))
    }
  }

  assert_scalar_int_na <- function(z, name) {
    if (!is_scalar_int(z)) {
      rlang::abort(sprintf("`%s` must be a scalar integer (or NA_integer_).", name))
    }
  }

  # --- top-level field types ------------------------------------------------
  assert_tibble_or_null(x$results, "results")
  assert_tibble_or_null(x$estimates, "estimates")
  assert_tibble_or_null(x$theta, "theta")
  assert_tibble_or_null(x$pairing_diagnostics, "pairing_diagnostics")

  assert_scalar_chr_na(x$theta_engine, "theta_engine")
  assert_scalar_chr_na(x$stop_reason, "stop_reason")
  assert_scalar_int_na(x$stop_round, "stop_round")

  if (!is.list(x$fit_provenance)) {
    rlang::abort("`fit_provenance` must be a list (possibly empty).")
  }

  # --- schema validations when present -------------------------------------
  if (!is.null(x$estimates)) {
    .validate_estimates_tbl(x$estimates)
  }

  if (!is.null(x$theta)) {
    theta_tbl <- x$theta

    if (!("ID" %in% names(theta_tbl)) || !("theta" %in% names(theta_tbl))) {
      rlang::abort("`theta` must include columns `ID` and `theta`.")
    }

    if (!is.character(theta_tbl$ID)) {
      rlang::abort("`theta$ID` must be character.")
    }
    if (!is.double(theta_tbl$theta)) {
      rlang::abort("`theta$theta` must be double.")
    }

    if ("se" %in% names(theta_tbl) && !is.double(theta_tbl$se)) {
      rlang::abort("`theta$se` must be double when present.")
    }
    if ("rank" %in% names(theta_tbl) && !is.integer(theta_tbl$rank)) {
      rlang::abort("`theta$rank` must be integer when present.")
    }
  } else {
    # Pragmatic NULL-allow rule: theta may be NULL only for non-trivial = FALSE.
    results_empty <- is.null(x$results) || nrow(x$results) == 0L
    estimates_empty <- is.null(x$estimates) || nrow(x$estimates) == 0L

    if (!(results_empty && estimates_empty)) {
      rlang::warn("Theta is NULL but the run produced results/estimates. This is allowed, but runners should generally include a final theta tibble.")

      if (isTRUE(strict)) {
        rlang::abort("`theta` is NULL for a non-trivial run (strict validation).")
      }
    }
  }

  if (!is.null(x$pairing_diagnostics)) {
    diag_tbl <- x$pairing_diagnostics

    required_cols <- c(
      "round",
      "n_pairs_planned",
      "n_pairs_completed",
      "degree_min",
      "largest_component_frac",
      "rms_theta_delta",
      "topk_overlap",
      "stop",
      "stop_reason",
      "stop_blocked_by",
      "stop_blocked_candidates"
    )

    missing_cols <- setdiff(required_cols, names(diag_tbl))
    if (length(missing_cols) > 0L) {
      rlang::abort(paste0("`pairing_diagnostics` is missing required column(s): ", paste(missing_cols, collapse = ", "), "."))
    }

    if (!is.integer(diag_tbl$round)) rlang::abort("`pairing_diagnostics$round` must be integer.")
    if (!is.integer(diag_tbl$n_pairs_planned)) rlang::abort("`pairing_diagnostics$n_pairs_planned` must be integer.")
    if (!is.integer(diag_tbl$n_pairs_completed)) rlang::abort("`pairing_diagnostics$n_pairs_completed` must be integer.")

    if (!is.double(diag_tbl$degree_min)) rlang::abort("`pairing_diagnostics$degree_min` must be double.")
    if (!is.double(diag_tbl$largest_component_frac)) rlang::abort("`pairing_diagnostics$largest_component_frac` must be double.")
    if (!is.double(diag_tbl$rms_theta_delta)) rlang::abort("`pairing_diagnostics$rms_theta_delta` must be double.")
    if (!is.double(diag_tbl$topk_overlap)) rlang::abort("`pairing_diagnostics$topk_overlap` must be double.")

    if (!is.logical(diag_tbl$stop)) rlang::abort("`pairing_diagnostics$stop` must be logical.")
    if (!is.character(diag_tbl$stop_reason)) rlang::abort("`pairing_diagnostics$stop_reason` must be character.")
    if (!is.character(diag_tbl$stop_blocked_by)) rlang::abort("`pairing_diagnostics$stop_blocked_by` must be character.")
    if (!is.character(diag_tbl$stop_blocked_candidates)) rlang::abort("`pairing_diagnostics$stop_blocked_candidates` must be character.")
  }

  # --- stop reason taxonomy: warn (not fail) by default ---------------------
  known_stop_reasons <- c(
    "stability_reached",
    "precision_reached",
    "graph_unhealthy",
    "pair_budget_exhausted",
    "max_rounds_reached",
    "no_new_pairs",
    "round_size_zero"
  )

  if (!is.na(x$stop_reason) && !(x$stop_reason %in% known_stop_reasons)) {
    rlang::warn(paste0("Non-standard stop_reason: ", x$stop_reason, ". Known taxonomy includes: ", paste(known_stop_reasons, collapse = ", "), "."))

    if (isTRUE(strict)) {
      rlang::abort("Non-standard `stop_reason` (strict validation).")
    }
  }

  invisible(x)
}
