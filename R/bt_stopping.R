#' Compute stopping metrics from a Bradley–Terry model fit
#'
#' This helper computes round-level summary metrics used for adaptive sampling and
#' stopping decisions. It is designed to work with the object returned by
#' \code{\link{fit_bt_model}} (which includes \code{fit$theta} with columns
#' \code{ID}, \code{theta}, and \code{se}).
#'
#' The output is a one-row tibble with:
#' \itemize{
#'   \item Precision summaries (e.g., SE mean, max, and quantiles),
#'   \item Scale summaries (SD of \code{theta}),
#'   \item Scale-free precision metrics (SE divided by SD of \code{theta}),
#'   \item Optional fit/diagnostic summaries (separation index and misfit proportions),
#'   \item Optional drift metrics for a core set relative to a prior fit.
#' }
#'
#' You can compute precision summaries on a subset of IDs (e.g., only newly-added
#' items) via \code{ids}. Drift metrics are added when both \code{prev_fit} and
#' \code{core_ids} are provided.
#'
#' @param fit A list returned by \code{\link{fit_bt_model}} containing a \code{$theta}
#'   tibble/data frame with columns \code{ID}, \code{theta}, \code{se}.
#' @param ids Optional character vector of item IDs to compute precision summaries on.
#'   If \code{NULL}, uses all items in \code{fit$theta}.
#' @param prev_fit Optional prior fit (same structure as \code{fit}) used for drift metrics.
#'   Must be provided together with \code{core_ids}.
#' @param core_ids Optional character vector of core IDs used for drift metrics.
#'   Must be provided together with \code{prev_fit}.
#' @param se_probs Numeric vector of probabilities for SE quantiles. Default:
#'   \code{c(0.5, 0.9, 0.95)}.
#' @param fit_bounds Numeric length-2 vector giving lower/upper bounds for acceptable
#'   infit/outfit when diagnostics are available. Default: \code{c(0.7, 1.3)}.
#'
#' @return A one-row tibble of stopping metrics.
#'
#' @examples
#' # A minimal, CRAN-safe "mock fit" with the required structure:
#' fit <- list(
#'   engine = "mock",
#'   theta = tibble::tibble(
#'     ID = c("A", "B", "C", "D"),
#'     theta = c(0, 1, 2, 3),
#'     se = c(0.20, 0.30, 0.40, 0.50)
#'   )
#' )
#'
#' # Compute metrics on all items
#' bt_stop_metrics(fit)
#'
#' # Compute metrics only on a subset (e.g., newly-added items)
#' bt_stop_metrics(fit, ids = c("A", "C"))
#'
#' # Add core drift metrics relative to a previous fit
#' prev_fit <- list(
#'   engine = "mock",
#'   theta = tibble::tibble(
#'     ID = c("A", "B", "C", "D"),
#'     theta = c(0, 0.5, 2.5, 3),
#'     se = c(0.20, 0.20, 0.20, 0.20)
#'   )
#' )
#' bt_stop_metrics(
#'   fit,
#'   prev_fit = prev_fit,
#'   core_ids = c("A", "B", "C", "D")
#' )
#'
#' @export
bt_stop_metrics <- function(fit,
                            ids = NULL,
                            prev_fit = NULL,
                            core_ids = NULL,
                            se_probs = c(0.5, 0.9, 0.95),
                            fit_bounds = c(0.7, 1.3)) {
  if (!is.list(fit) || is.null(fit$theta)) {
    stop(
      "`fit` must be a list returned by `fit_bt_model()` and contain a `$theta` tibble.",
      call. = FALSE
    )
  }

  theta_tbl <- tibble::as_tibble(fit$theta)
  theta_tbl_all <- theta_tbl
  n_total_items <- nrow(theta_tbl_all)

  # Validate drift inputs
  if (is.null(prev_fit) != is.null(core_ids)) {
    stop("Provide both `prev_fit` and `core_ids`, or neither.", call. = FALSE)
  }

  # Optional: compute precision summaries on a subset of IDs
  if (!is.null(ids)) {
    if (!is.character(ids)) {
      stop("`ids` must be a character vector when provided.", call. = FALSE)
    }
    ids_u <- unique(ids)
    missing_ids <- setdiff(ids_u, as.character(theta_tbl_all$ID))
    if (length(missing_ids) > 0L) {
      stop("All `ids` must be present in `fit$theta$ID`.", call. = FALSE)
    }
    theta_tbl <- theta_tbl_all[match(ids_u, as.character(theta_tbl_all$ID)), , drop = FALSE]
  }

  required_cols <- c("ID", "theta", "se")
  if (!all(required_cols %in% names(theta_tbl))) {
    stop(
      "`fit$theta` must contain columns: ",
      paste(required_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.numeric(se_probs) || length(se_probs) < 1L || any(!is.finite(se_probs))) {
    stop("`se_probs` must be a numeric vector of finite probabilities.", call. = FALSE)
  }

  if (!is.numeric(fit_bounds) || length(fit_bounds) != 2L || any(!is.finite(fit_bounds))) {
    stop("`fit_bounds` must be a numeric length-2 vector (lower, upper).", call. = FALSE)
  }
  lower <- min(fit_bounds)
  upper <- max(fit_bounds)

  theta_num <- as.double(unname(theta_tbl$theta))
  se_num <- as.double(unname(theta_tbl$se))

  n_items <- nrow(theta_tbl)
  theta_sd <- stats::sd(theta_num, na.rm = TRUE)

  safe_mean <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }
  safe_max <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    max(x, na.rm = TRUE)
  }
  safe_q <- function(x, p) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
  }

  se_mean <- safe_mean(se_num)
  se_max <- safe_max(se_num)

  qmap <- list()
  for (p in se_probs) {
    nm <- paste0("se_p", as.integer(round(100 * p)))
    qmap[[nm]] <- safe_q(se_num, p)
  }

  rel_se_mean <- if (is.finite(theta_sd) && theta_sd > 0) se_mean / theta_sd else NA_real_
  rel_se_p90 <- if (!is.null(qmap$se_p90) && is.finite(theta_sd) && theta_sd > 0) qmap$se_p90 / theta_sd else NA_real_

  engine <- if (!is.null(fit$engine)) fit$engine else NA_character_
  reliability <- if (!is.null(fit$reliability)) fit$reliability else NA_real_

  diag <- if (!is.null(fit$diagnostics) && is.list(fit$diagnostics)) fit$diagnostics else NULL

  sepG <- NA_real_
  if (!is.null(diag) && !is.null(diag$sepG) && length(diag$sepG) == 1L) {
    sepG <- as.numeric(diag$sepG)
  }

  item_misfit_prop <- NA_real_
  if (!is.null(diag) && !is.null(diag$item_fit)) {
    item_fit <- tibble::as_tibble(diag$item_fit)
    if (all(c("infit", "outfit") %in% names(item_fit))) {
      infit <- as.double(unname(item_fit$infit))
      outfit <- as.double(unname(item_fit$outfit))
      bad <- (infit < lower | infit > upper) | (outfit < lower | outfit > upper)
      item_misfit_prop <- if (length(bad) == 0L) NA_real_ else mean(bad, na.rm = TRUE)
    }
  }

  judge_misfit_prop <- NA_real_
  if (!is.null(diag) && !is.null(diag$judge_fit)) {
    judge_fit <- tibble::as_tibble(diag$judge_fit)
    if (all(c("infit", "outfit") %in% names(judge_fit))) {
      infit <- as.double(unname(judge_fit$infit))
      outfit <- as.double(unname(judge_fit$outfit))
      bad <- (infit < lower | infit > upper) | (outfit < lower | outfit > upper)
      judge_misfit_prop <- if (length(bad) == 0L) NA_real_ else mean(bad, na.rm = TRUE)
    }
  }

  out <- tibble::tibble(
    engine = engine,
    n_items = n_items,
    n_total_items = n_total_items,
    theta_sd = theta_sd,
    se_mean = se_mean,
    se_max = se_max,
    rel_se_mean = rel_se_mean,
    rel_se_p90 = rel_se_p90,
    reliability = as.double(reliability),
    sepG = sepG,
    item_misfit_prop = item_misfit_prop,
    judge_misfit_prop = judge_misfit_prop
  )

  for (nm in names(qmap)) {
    out[[nm]] <- as.double(qmap[[nm]])
  }

  if (!is.null(prev_fit) && !is.null(core_ids)) {
    drift <- bt_drift_metrics(
      current = fit,
      previous = prev_fit,
      ids = core_ids,
      prefix = "core_"
    )
    out <- dplyr::bind_cols(out, drift)
  }

  out
}

#' Decide whether to stop adaptive sampling based on stop metrics
#'
#' Applies combined stopping criteria to the output of \code{\link{bt_stop_metrics}}.
#' Intended use is round-based adaptive sampling:
#'
#' \enumerate{
#'   \item Fit or update the model,
#'   \item Compute metrics with \code{bt_stop_metrics()},
#'   \item Decide stop/continue with \code{bt_should_stop()}.
#' }
#'
#' The decision can incorporate:
#' \itemize{
#'   \item Reliability and separation thresholds (when available),
#'   \item Fit thresholds (item/judge misfit proportions; when available),
#'   \item Precision target (\code{rel_se_p90 <= rel_se_p90_target}),
#'   \item Optional stability criterion vs \code{prev_metrics},
#'   \item Optional drift guardrails for core linking workflows (disabled by default).
#' }
#'
#' Core drift guardrails are enabled by setting one or more \code{core_*_target}
#' arguments (otherwise they default to \code{NA} and are ignored).
#'
#' @param metrics A one-row tibble returned by \code{\link{bt_stop_metrics}}.
#' @param prev_metrics Optional one-row tibble of prior-round metrics (same shape as
#'   \code{metrics}). Used to compute percent improvement for the stability criterion.
#' @param reliability_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$reliability >= reliability_target}.
#' @param sepG_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$sepG >= sepG_target}.
#' @param rel_se_p90_target Optional numeric. If not \code{NA}, precision target is met when
#'   \code{metrics$rel_se_p90 <= rel_se_p90_target}.
#' @param rel_se_p90_min_improve Optional numeric. If not \code{NA} and \code{prev_metrics}
#'   is provided, compute percent improvement \code{(prev - current) / prev}. Stalling is
#'   defined as \code{improve_pct <= rel_se_p90_min_improve}.
#' @param max_item_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$item_misfit_prop <= max_item_misfit_prop} (when metric is available).
#' @param max_judge_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$judge_misfit_prop <= max_judge_misfit_prop} (when metric is available).
#' @param core_theta_cor_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$core_theta_cor >= core_theta_cor_target}.
#' @param core_theta_spearman_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$core_theta_spearman >= core_theta_spearman_target}.
#' @param core_max_abs_shift_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$core_max_abs_shift <= core_max_abs_shift_target}.
#' @param core_p90_abs_shift_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$core_p90_abs_shift <= core_p90_abs_shift_target}.
#'
#' @return A list with:
#' \describe{
#'   \item{stop}{Logical; \code{TRUE} if stopping criteria are met.}
#'   \item{details}{A tibble listing each criterion, its value, threshold, and pass/fail.}
#'   \item{improve}{A tibble with computed percent improvement (if \code{prev_metrics} supplied).}
#' }
#'
#' @examples
#' # Example metrics (as if returned by bt_stop_metrics())
#' m <- tibble::tibble(
#'   reliability = 0.92,
#'   sepG = 3.2,
#'   rel_se_p90 = 0.25,
#'   item_misfit_prop = 0.00,
#'   judge_misfit_prop = 0.00
#' )
#'
#' # Stop if precision target is met and other thresholds pass
#' bt_should_stop(m, rel_se_p90_target = 0.30)$stop
#'
#' # Include a previous round to evaluate stability (diminishing returns)
#' prev_m <- tibble::tibble(
#'   reliability = 0.91,
#'   sepG = 3.1,
#'   rel_se_p90 = 0.26,
#'   item_misfit_prop = 0.00,
#'   judge_misfit_prop = 0.00
#' )
#' bt_should_stop(m, prev_metrics = prev_m, rel_se_p90_min_improve = 0.01)$stop
#'
#' # Drift gating example: only stop if core drift guardrails pass
#' m2 <- dplyr::bind_cols(
#'   m,
#'   tibble::tibble(
#'     core_theta_cor = 0.80,
#'     core_theta_spearman = 1.00,
#'     core_max_abs_shift = 0.60,
#'     core_p90_abs_shift = 0.50
#'   )
#' )
#'
#' # This will NOT stop because correlation guardrail fails (0.80 < 0.90)
#' bt_should_stop(m2, core_theta_cor_target = 0.90)$stop
#'
#' # This WILL stop because drift thresholds are relaxed
#' bt_should_stop(
#'   m2,
#'   core_theta_cor_target = 0.70,
#'   core_max_abs_shift_target = 0.70,
#'   core_p90_abs_shift_target = 0.60
#' )$stop
#'
#' @export
bt_should_stop <- function(metrics,
                           prev_metrics = NULL,
                           reliability_target = 0.90,
                           sepG_target = 3.0,
                           rel_se_p90_target = 0.30,
                           rel_se_p90_min_improve = 0.01,
                           max_item_misfit_prop = 0.05,
                           max_judge_misfit_prop = 0.05,
                           core_theta_cor_target = NA_real_,
                           core_theta_spearman_target = NA_real_,
                           core_max_abs_shift_target = NA_real_,
                           core_p90_abs_shift_target = NA_real_) {
  metrics <- tibble::as_tibble(metrics)
  if (nrow(metrics) != 1L) {
    stop("`metrics` must be a one-row tibble (e.g., output of `bt_stop_metrics()`).", call. = FALSE)
  }

  require_col <- function(col, active) {
    if (isTRUE(active) && !(col %in% names(metrics))) {
      stop("`metrics` is missing required column: ", col, call. = FALSE)
    }
  }

  require_col("reliability", !is.na(reliability_target))
  require_col("sepG", !is.na(sepG_target))
  require_col("item_misfit_prop", !is.na(max_item_misfit_prop))
  require_col("judge_misfit_prop", !is.na(max_judge_misfit_prop))
  require_col("rel_se_p90", !is.na(rel_se_p90_target) || (!is.null(prev_metrics) && !is.na(rel_se_p90_min_improve)))
  require_col("core_theta_cor", !is.na(core_theta_cor_target))
  require_col("core_theta_spearman", !is.na(core_theta_spearman_target))
  require_col("core_max_abs_shift", !is.na(core_max_abs_shift_target))
  require_col("core_p90_abs_shift", !is.na(core_p90_abs_shift_target))

  get1 <- function(col) as.numeric(metrics[[col]][[1]])

  reliability <- if ("reliability" %in% names(metrics)) get1("reliability") else NA_real_
  sepG <- if ("sepG" %in% names(metrics)) get1("sepG") else NA_real_
  rel_se_p90 <- if ("rel_se_p90" %in% names(metrics)) get1("rel_se_p90") else NA_real_
  item_misfit_prop <- if ("item_misfit_prop" %in% names(metrics)) get1("item_misfit_prop") else NA_real_
  judge_misfit_prop <- if ("judge_misfit_prop" %in% names(metrics)) get1("judge_misfit_prop") else NA_real_

  core_theta_cor <- if ("core_theta_cor" %in% names(metrics)) get1("core_theta_cor") else NA_real_
  core_theta_spearman <- if ("core_theta_spearman" %in% names(metrics)) get1("core_theta_spearman") else NA_real_
  core_max_abs_shift <- if ("core_max_abs_shift" %in% names(metrics)) get1("core_max_abs_shift") else NA_real_
  core_p90_abs_shift <- if ("core_p90_abs_shift" %in% names(metrics)) get1("core_p90_abs_shift") else NA_real_

  improve_pct <- NA_real_
  if (!is.null(prev_metrics)) {
    prev_metrics <- tibble::as_tibble(prev_metrics)
    if (nrow(prev_metrics) != 1L) {
      stop("`prev_metrics` must be a one-row tibble when provided.", call. = FALSE)
    }
    if (!("rel_se_p90" %in% names(prev_metrics))) {
      stop("`prev_metrics` is missing required column: rel_se_p90", call. = FALSE)
    }

    prev_rel_se <- as.numeric(prev_metrics$rel_se_p90[[1]])
    if (is.finite(prev_rel_se) && prev_rel_se > 0 && is.finite(rel_se_p90)) {
      improve_pct <- (prev_rel_se - rel_se_p90) / prev_rel_se
    }
  }

  pass_reliability <- if (is.na(reliability_target)) TRUE else (is.finite(reliability) && reliability >= reliability_target)
  pass_sepG <- if (is.na(sepG_target)) TRUE else (is.finite(sepG) && sepG >= sepG_target)

  pass_item_fit <- if (is.na(max_item_misfit_prop)) TRUE else (is.na(item_misfit_prop) || item_misfit_prop <= max_item_misfit_prop)
  pass_judge_fit <- if (is.na(max_judge_misfit_prop)) TRUE else (is.na(judge_misfit_prop) || judge_misfit_prop <= max_judge_misfit_prop)

  pass_core_theta_cor <- if (is.na(core_theta_cor_target)) TRUE else (is.finite(core_theta_cor) && core_theta_cor >= core_theta_cor_target)
  pass_core_theta_spearman <- if (is.na(core_theta_spearman_target)) TRUE else (is.finite(core_theta_spearman) && core_theta_spearman >= core_theta_spearman_target)
  pass_core_max_abs_shift <- if (is.na(core_max_abs_shift_target)) TRUE else (is.finite(core_max_abs_shift) && core_max_abs_shift <= core_max_abs_shift_target)
  pass_core_p90_abs_shift <- if (is.na(core_p90_abs_shift_target)) TRUE else (is.finite(core_p90_abs_shift) && core_p90_abs_shift <= core_p90_abs_shift_target)

  pass_precision <- if (is.na(rel_se_p90_target)) TRUE else (is.finite(rel_se_p90) && rel_se_p90 <= rel_se_p90_target)

  pass_stability <- FALSE
  if (!is.na(rel_se_p90_min_improve) && is.finite(improve_pct)) {
    pass_stability <- improve_pct <= rel_se_p90_min_improve
  }

  stop_now <- isTRUE(pass_reliability) &&
    isTRUE(pass_sepG) &&
    isTRUE(pass_item_fit) &&
    isTRUE(pass_judge_fit) &&
    isTRUE(pass_core_theta_cor) &&
    isTRUE(pass_core_theta_spearman) &&
    isTRUE(pass_core_max_abs_shift) &&
    isTRUE(pass_core_p90_abs_shift) &&
    (isTRUE(pass_precision) || isTRUE(pass_stability))

  details <- tibble::tibble(
    criterion = c(
      "reliability",
      "sepG",
      "item_misfit_prop",
      "judge_misfit_prop",
      "core_theta_cor",
      "core_theta_spearman",
      "core_max_abs_shift",
      "core_p90_abs_shift",
      "rel_se_p90_precision",
      "rel_se_p90_stability"
    ),
    value = c(
      reliability,
      sepG,
      item_misfit_prop,
      judge_misfit_prop,
      core_theta_cor,
      core_theta_spearman,
      core_max_abs_shift,
      core_p90_abs_shift,
      rel_se_p90,
      improve_pct
    ),
    threshold = c(
      reliability_target,
      sepG_target,
      max_item_misfit_prop,
      max_judge_misfit_prop,
      core_theta_cor_target,
      core_theta_spearman_target,
      core_max_abs_shift_target,
      core_p90_abs_shift_target,
      rel_se_p90_target,
      rel_se_p90_min_improve
    ),
    pass = c(
      pass_reliability,
      pass_sepG,
      pass_item_fit,
      pass_judge_fit,
      pass_core_theta_cor,
      pass_core_theta_spearman,
      pass_core_max_abs_shift,
      pass_core_p90_abs_shift,
      pass_precision,
      pass_stability
    )
  )

  improve_tbl <- tibble::tibble(rel_se_p90_improve_pct = improve_pct)

  list(stop = stop_now, details = details, improve = improve_tbl)
}
