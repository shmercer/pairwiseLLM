#' Compute stopping metrics from a Bradley–Terry model fit
#'
#' This helper computes round-level summary metrics that are useful for adaptive
#' sampling and stopping decisions. It is designed to work with the object returned
#' by \code{\link{fit_bt_model}}.
#'
#' The output includes precision summaries (SE distribution), scale summaries
#' (SD of \code{theta}), and scale-free precision metrics (SE / SD(theta)).
#' If diagnostic outputs are available (from \code{fit_bt_model(..., return_diagnostics = TRUE)}
#' using the \pkg{sirt} engine), the output also includes separation index (\code{sepG})
#' and item/judge misfit proportions based on infit/outfit bounds.
#'
#' @param fit A list returned by \code{\link{fit_bt_model}}.
#' @param se_probs Numeric vector of probabilities for SE quantiles. Default:
#'   \code{c(0.5, 0.9, 0.95)}.
#' @param fit_bounds Numeric length-2 vector giving lower/upper bounds for
#'   acceptable infit/outfit. Default: \code{c(0.7, 1.3)}.
#'
#' @return A one-row tibble with summary metrics. Key columns include:
#' \describe{
#'   \item{engine}{Modeling engine used ("sirt", "BradleyTerry2", or \code{NA}).}
#'   \item{n_items}{Number of objects in \code{fit$theta}.}
#'   \item{theta_sd}{SD of \code{theta}.}
#'   \item{se_mean}{Mean SE.}
#'   \item{se_p90}{90th percentile SE (if requested in \code{se_probs}).}
#'   \item{se_p95}{95th percentile SE (if requested in \code{se_probs}).}
#'   \item{se_max}{Maximum SE.}
#'   \item{rel_se_mean}{\code{se_mean / theta_sd} (scale-free; \code{NA} if \code{theta_sd <= 0}).}
#'   \item{rel_se_p90}{\code{se_p90 / theta_sd} (scale-free; \code{NA} if \code{theta_sd <= 0}).}
#'   \item{reliability}{MLE reliability (typically for \pkg{sirt}) or \code{NA}.}
#'   \item{sepG}{Separation index if available, otherwise \code{NA}.}
#'   \item{item_misfit_prop}{Proportion of items with infit/outfit outside \code{fit_bounds}.}
#'   \item{judge_misfit_prop}{Proportion of judges with infit/outfit outside \code{fit_bounds}.}
#' }
#'
#' @examples
#' # Minimal example using a mock fit object (runs without sirt installed)
#' fit_mock <- list(
#'   engine = "mock",
#'   reliability = 0.90,
#'   theta = tibble::tibble(
#'     ID = c("A", "B", "C"),
#'     theta = c(0.0, 1.0, -1.0),
#'     se = c(0.3, 0.2, 0.4)
#'   )
#' )
#' bt_stop_metrics(fit_mock)
#'
#' # Real example (only runs if sirt is installed)
#' if (requireNamespace("sirt", quietly = TRUE)) {
#'   data("example_writing_pairs", package = "pairwiseLLM")
#'   bt <- build_bt_data(example_writing_pairs)
#'   fit <- fit_bt_model(bt, engine = "sirt", verbose = FALSE, return_diagnostics = TRUE)
#'   bt_stop_metrics(fit)
#' }
#'
#' @import tibble
#' @export
bt_stop_metrics <- function(fit,
                            se_probs = c(0.5, 0.9, 0.95),
                            fit_bounds = c(0.7, 1.3)) {
  if (!is.list(fit) || is.null(fit$theta)) {
    stop(
      "`fit` must be a list returned by `fit_bt_model()` and contain a `$theta` tibble.",
      call. = FALSE
    )
  }

  theta_tbl <- tibble::as_tibble(fit$theta)

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

  # Relative SE metrics (scale-free)
  rel_se_mean <- if (is.finite(theta_sd) && theta_sd > 0) se_mean / theta_sd else NA_real_
  rel_se_p90 <- if (!is.null(qmap$se_p90) && is.finite(theta_sd) && theta_sd > 0) qmap$se_p90 / theta_sd else NA_real_

  engine <- if (!is.null(fit$engine)) fit$engine else NA_character_
  reliability <- if (!is.null(fit$reliability)) fit$reliability else NA_real_

  # Diagnostics (optional)
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

  # Bind in requested SE quantiles as columns (se_p50, se_p90, etc.)
  for (nm in names(qmap)) {
    out[[nm]] <- as.double(qmap[[nm]])
  }

  out
}

#' Decide whether to stop adaptive sampling based on stop metrics
#'
#' This helper applies combined stopping criteria to the output of
#' \code{\link{bt_stop_metrics}}. It is intended for round-based adaptive sampling:
#' compute metrics each round, then call this function to decide whether to continue.
#'
#' Stopping requires:
#' \itemize{
#'   \item Reliability and separation thresholds (if provided), AND
#'   \item Fit thresholds (item/judge misfit proportions, if provided), AND
#'   \item Either precision target is met (\code{rel_se_p90 <= rel_se_p90_target}), OR
#'     improvement has stalled relative to the previous round
#'     (\code{rel_se_p90_improve_pct <= rel_se_p90_min_improve}).
#' }
#'
#' @param metrics A one-row tibble returned by \code{\link{bt_stop_metrics}}.
#' @param prev_metrics Optional one-row tibble of prior-round metrics (same shape as
#'   \code{metrics}). Used to compute percent improvement criteria.
#' @param reliability_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$reliability >= reliability_target}.
#' @param sepG_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$sepG >= sepG_target}.
#' @param rel_se_p90_target Optional numeric. If not \code{NA}, precision target is met when
#'   \code{metrics$rel_se_p90 <= rel_se_p90_target}.
#' @param rel_se_p90_min_improve Optional numeric. If not \code{NA} and \code{prev_metrics}
#'   is provided, compute percent improvement
#'   \code{(prev - current) / prev}. Stalling is defined as
#'   \code{improve_pct <= rel_se_p90_min_improve}.
#' @param max_item_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$item_misfit_prop <= max_item_misfit_prop} (when metric is available).
#' @param max_judge_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$judge_misfit_prop <= max_judge_misfit_prop} (when metric is available).
#'
#' @return A list with:
#' \describe{
#'   \item{stop}{Logical; \code{TRUE} if stopping criteria are met.}
#'   \item{details}{A tibble giving each criterion, its value, threshold, and pass/fail.}
#'   \item{improve}{A tibble with computed percent improvement (if \code{prev_metrics} supplied).}
#' }
#'
#' @examples
#' m1 <- tibble::tibble(
#'   reliability = 0.92, sepG = 3.2, rel_se_p90 = 0.25,
#'   item_misfit_prop = 0.00, judge_misfit_prop = 0.00
#' )
#' res <- bt_should_stop(m1, reliability_target = 0.90, sepG_target = 3.0, rel_se_p90_target = 0.30)
#' res$stop
#' res$details
#'
#' @import tibble
#' @export
bt_should_stop <- function(metrics,
                           prev_metrics = NULL,
                           reliability_target = 0.90,
                           sepG_target = 3.0,
                           rel_se_p90_target = 0.30,
                           rel_se_p90_min_improve = 0.01,
                           max_item_misfit_prop = 0.05,
                           max_judge_misfit_prop = 0.05) {
  metrics <- tibble::as_tibble(metrics)
  if (nrow(metrics) != 1L) {
    stop("`metrics` must be a one-row tibble (e.g., output of `bt_stop_metrics()`).", call. = FALSE)
  }

  # Validate required columns based on which thresholds are active
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

  get1 <- function(col) as.numeric(metrics[[col]][[1]])

  reliability <- if ("reliability" %in% names(metrics)) get1("reliability") else NA_real_
  sepG <- if ("sepG" %in% names(metrics)) get1("sepG") else NA_real_
  rel_se_p90 <- if ("rel_se_p90" %in% names(metrics)) get1("rel_se_p90") else NA_real_
  item_misfit_prop <- if ("item_misfit_prop" %in% names(metrics)) get1("item_misfit_prop") else NA_real_
  judge_misfit_prop <- if ("judge_misfit_prop" %in% names(metrics)) get1("judge_misfit_prop") else NA_real_

  # Optional: compute percent improvement in rel_se_p90 vs previous round
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

  pass_item_fit <- if (is.na(max_item_misfit_prop)) {
    TRUE
  } else {
    if (is.na(item_misfit_prop)) TRUE else item_misfit_prop <= max_item_misfit_prop
  }

  pass_judge_fit <- if (is.na(max_judge_misfit_prop)) {
    TRUE
  } else {
    if (is.na(judge_misfit_prop)) TRUE else judge_misfit_prop <= max_judge_misfit_prop
  }

  pass_precision <- if (is.na(rel_se_p90_target)) TRUE else (is.finite(rel_se_p90) && rel_se_p90 <= rel_se_p90_target)

  pass_stability <- FALSE
  if (!is.na(rel_se_p90_min_improve) && is.finite(improve_pct)) {
    pass_stability <- improve_pct <= rel_se_p90_min_improve
  }

  stop_now <- isTRUE(pass_reliability) &&
    isTRUE(pass_sepG) &&
    isTRUE(pass_item_fit) &&
    isTRUE(pass_judge_fit) &&
    (isTRUE(pass_precision) || isTRUE(pass_stability))

  details <- tibble::tibble(
    criterion = c(
      "reliability",
      "sepG",
      "item_misfit_prop",
      "judge_misfit_prop",
      "rel_se_p90_precision",
      "rel_se_p90_stability"
    ),
    value = c(
      reliability,
      sepG,
      item_misfit_prop,
      judge_misfit_prop,
      rel_se_p90,
      improve_pct
    ),
    threshold = c(
      reliability_target,
      sepG_target,
      max_item_misfit_prop,
      max_judge_misfit_prop,
      rel_se_p90_target,
      rel_se_p90_min_improve
    ),
    pass = c(
      pass_reliability,
      pass_sepG,
      pass_item_fit,
      pass_judge_fit,
      pass_precision,
      pass_stability
    )
  )

  improve_tbl <- tibble::tibble(rel_se_p90_improve_pct = improve_pct)

  list(stop = stop_now, details = details, improve = improve_tbl)
}
