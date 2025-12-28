# Internal helpers for standardizing metrics outputs across runners

#' Create a 0-row metrics tibble with standardized columns
#'
#' @param se_probs Numeric vector of probabilities used for se quantiles.
#'
#' @return A 0-row tibble with the standardized metrics schema.
#' @keywords internal
.bt_metrics_template <- function(se_probs = c(0.5, 0.9, 0.95)) {
  se_probs <- as.numeric(se_probs)
  if (length(se_probs) < 1L || any(!is.finite(se_probs))) {
    se_probs <- c(0.5, 0.9, 0.95)
  }
  se_cols <- paste0("se_p", as.integer(round(100 * se_probs)))

  tmpl <- tibble::tibble(
    # Bookkeeping
    batch_index = integer(),
    round_index = integer(),
    stage = character(),
    stop = logical(),
    stop_reason = character(),

    # Allocation and pairing diagnostics
    allocation = character(),
    allocation_source = character(),
    within_batch_frac = double(),
    core_audit_frac = double(),
    round_size = integer(),
    n_pairs_proposed = integer(),
    n_results_total = integer(),
    n_new_ids = integer(),
    n_core_new = integer(),
    n_new_new = integer(),
    n_core_core = integer(),

    # Stop metrics (bt_stop_metrics)
    engine = character(),
    n_items = integer(),
    n_total_items = integer(),
    theta_sd = double(),
    se_mean = double(),
    se_max = double(),
    rel_se_mean = double(),
    rel_se_p90 = double(),
    reliability = double(),
    sepG = double(),
    item_misfit_prop = double(),
    judge_misfit_prop = double(),

    # Drift metrics (bt_drift_metrics with prefix core_)
    core_n = integer(),
    core_theta_cor = double(),
    core_theta_spearman = double(),
    core_mean_abs_shift = double(),
    core_p90_abs_shift = double(),
    core_p95_abs_shift = double(),
    core_max_abs_shift = double(),
    core_mean_signed_shift = double(),

    # Linking drift (baseline drift used to decide/apply linking)
    linking_theta_cor = double(),
    linking_theta_spearman = double(),
    linking_mean_abs_shift = double(),
    linking_p90_abs_shift = double(),
    linking_p95_abs_shift = double(),
    linking_max_abs_shift = double(),
    linking_mean_signed_shift = double()
  )

  for (nm in se_cols) {
    if (!(nm %in% names(tmpl))) {
      tmpl[[nm]] <- double()
    }
  }

  tmpl
}

#' Align a metrics tibble to the standardized schema
#'
#' Ensures all standard columns exist (adding typed NA columns when missing),
#' and orders columns with the standardized schema first (keeping any extras).
#'
#' @param metrics A tibble/data.frame of metrics rows.
#' @param se_probs The `se_probs` used for computing se quantiles.
#'
#' @return A tibble with standardized columns.
#' @keywords internal
.bt_align_metrics <- function(metrics, se_probs = c(0.5, 0.9, 0.95)) {
  tmpl <- .bt_metrics_template(se_probs)
  m <- tibble::as_tibble(metrics)

  out <- dplyr::bind_rows(tmpl, m)

  extra <- setdiff(names(out), names(tmpl))
  out <- out[, c(names(tmpl), extra), drop = FALSE]

  out
}
