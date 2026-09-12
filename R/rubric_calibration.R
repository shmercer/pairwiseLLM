# Shared rubric API and label alignment. Statistical backends are added separately.

.rubric_dispatch <- function(method, calibration_design) {
  .rubric_choice(method, c("percentile", "ordinal_linear", "ordinal_monotone"), "method")
  .rubric_choice(calibration_design, c("same_set", "linked_anchors"), "calibration_design")
  if (method == "percentile" && calibration_design == "linked_anchors") {
    rlang::abort("Percentile scoring is distribution-matched and does not support `linked_anchors`.")
  }
  invisible(TRUE)
}

.rubric_levels <- function(scores = NULL, levels = NULL, K = NULL, target_distribution = NULL) {
  if (!is.null(K) && (!is.numeric(K) || length(K) != 1L || !is.finite(K) ||
    K < 3 || K > .Machine$integer.max || K != as.integer(K))) {
    rlang::abort("`K` must be a single integer >= 3.")
  }
  if (!is.null(scores) && (!(is.numeric(scores) || is.character(scores) || is.factor(scores)) ||
    !is.null(dim(scores)))) {
    rlang::abort("`rubric_score` must contain numeric, character, or ordered-factor labels.")
  }
  if (is.numeric(scores) && any(is.infinite(scores) | is.nan(scores))) {
    rlang::abort("Numeric rubric labels must be finite or NA for unlabeled items.")
  }
  if (is.null(levels)) {
    levels <- if (is.ordered(scores)) {
      base::levels(scores)
    } else if (is.numeric(scores)) {
      sort(unique(scores[!is.na(scores)]))
    } else if (!is.null(scores)) {
      rlang::abort("Supply ordered `levels` for character or unordered-factor rubric labels.")
    } else {
      n <- K %||% if (!is.null(target_distribution)) length(target_distribution) else NULL
      if (is.null(n)) rlang::abort("Supply `levels`, `K`, or `target_distribution` for percentile scoring.")
      seq_len(n)
    }
  }
  if (!(is.character(levels) || is.numeric(levels)) || !is.null(dim(levels)) ||
    length(levels) < 3L || anyNA(levels) || anyDuplicated(as.character(levels)) ||
    any(!nzchar(trimws(as.character(levels)))) || (is.numeric(levels) && any(!is.finite(levels)))) {
    rlang::abort("`levels` must contain at least three distinct, nonmissing ordered labels.")
  }
  if (is.ordered(scores) && !identical(as.character(levels), base::levels(scores))) {
    rlang::abort("Explicit `levels` must agree with the ordered-factor rubric levels.")
  }
  if (!is.null(K) && K != length(levels)) rlang::abort("`K` does not match the requested rubric levels.")
  if (!is.null(target_distribution)) {
    if (!is.numeric(target_distribution) || !is.null(dim(target_distribution)) ||
      length(target_distribution) != length(levels) || any(!is.finite(target_distribution)) ||
      any(target_distribution <= 0) || abs(sum(target_distribution) - 1) > 1e-8) {
      rlang::abort("`target_distribution` must give one positive proportion per level and sum to one.")
    }
    if (!is.null(names(target_distribution)) &&
      !identical(names(target_distribution), as.character(levels))) {
      rlang::abort("Named target proportions must follow the requested level order.")
    }
  }
  list(levels = levels, K = length(levels), target_distribution = target_distribution)
}

.rubric_align_labels <- function(rubric, cj, levels = NULL, K = NULL) {
  if (!is.data.frame(rubric) || !all(c("item_id", "rubric_score") %in% names(rubric))) {
    rlang::abort("`rubric` must be a data frame containing `item_id` and `rubric_score`.")
  }
  ids <- .rubric_ids(rubric$item_id, "rubric item_id")
  if (!all(ids %in% cj$items$item_id)) rlang::abort("Every rubric item ID must be present in the CJ result.")
  .rubric_trait(cj$trait, rubric[["trait"]])
  parsed <- .rubric_levels(rubric$rubric_score, levels, K)
  category <- match(as.character(rubric$rubric_score), as.character(parsed$levels))
  if (any(!is.na(rubric$rubric_score) & is.na(category))) {
    rlang::abort("Rubric scores contain labels outside the requested levels.")
  }
  counts <- tabulate(category, nbins = parsed$K)
  if (any(counts == 0L)) {
    rlang::abort(paste0("Missing requested rubric categories: ",
      paste(parsed$levels[counts == 0L], collapse = ", "), ". Categories are not collapsed."))
  }
  idx <- match(cj$items$item_id, ids)
  data <- tibble::tibble(item_id = cj$items$item_id, theta = cj$items$theta,
    rubric_score = rubric$rubric_score[idx], category = category[idx])
  list(levels = parsed$levels, K = parsed$K, data = data,
    counts = stats::setNames(counts, as.character(parsed$levels)),
    range = range(data$theta[!is.na(data$category)]))
}

.rubric_prepare_calibration <- function(cj, rubric = NULL, method = "ordinal_linear",
                                        calibration_design = "same_set", trait = NULL,
                                        levels = NULL, K = NULL, target_distribution = NULL) {
  .rubric_dispatch(method, calibration_design)
  scales <- if (calibration_design == "linked_anchors") {
    "phase_a_reference"
  } else {
    c("within_set", "phase_a_reference")
  }
  cj <- .rubric_normalize_cj(cj, trait, scale_status = scales)
  if (method == "percentile") {
    if (!is.null(rubric)) rlang::abort("Percentile scoring does not fit human rubric labels; omit `rubric`.")
    parsed <- .rubric_levels(levels = levels, K = K, target_distribution = target_distribution)
    calibration <- list(data = NULL, counts = NULL, range = range(cj$items$theta))
  } else {
    if (!is.null(target_distribution)) rlang::abort("`target_distribution` is only used for percentile scoring.")
    calibration <- .rubric_align_labels(rubric, cj, levels, K)
    parsed <- calibration
  }
  .rubric_new_calibration(cj, method, calibration_design, parsed$levels, calibration,
    target_distribution = target_distribution)
}

.rubric_new_calibration <- function(cj, method, calibration_design, levels, calibration,
                                    target_distribution = NULL) {
  out <- structure(list(
    format_version = 1L, status = "unfitted", method = method, calibration_design = calibration_design,
    trait = cj$trait, orientation = cj$orientation, levels = levels, K = length(levels), cj = cj,
    calibration_data = calibration$data, category_counts = calibration$counts,
    calibration_range = calibration$range, reference = cj$reference,
    target_distribution = target_distribution, transformation = NULL, backend = NULL,
    diagnostics = list(cj = cj$diagnostics), warnings = character()
  ), class = "pairwiseLLM_rubric_calibration")
  .rubric_validate_calibration(out)
  out
}

.rubric_validate_calibration <- function(object) {
  required <- c("format_version", "status", "method", "calibration_design", "trait", "orientation",
    "levels", "K", "cj", "calibration_data", "category_counts", "calibration_range", "reference",
    "target_distribution", "transformation", "backend", "diagnostics", "warnings")
  if (!inherits(object, "pairwiseLLM_rubric_calibration") || !is.list(object) ||
    !all(required %in% names(object)) || !identical(object$format_version, 1L)) {
    rlang::abort("Invalid rubric calibration object.")
  }
  .rubric_dispatch(object$method, object$calibration_design)
  .rubric_choice(object$status, c("unfitted", "fitted"), "calibration status")
  .rubric_validate_cj(object$cj)
  .rubric_trait(object$trait, object$cj$trait)
  .rubric_choice(object$orientation, "higher_is_better", "orientation")
  .rubric_levels(levels = object$levels, K = object$K, target_distribution = object$target_distribution)
  if (object$status == "fitted" && is.null(object$backend)) {
    rlang::abort("A fitted rubric calibration requires a statistical backend result.")
  }
  if (object$status == "unfitted" && (!is.null(object$backend) || !is.null(object$transformation))) {
    rlang::abort("An unfitted calibration cannot contain statistical results.")
  }
  if (object$status == "fitted" && object$method == "percentile") .rubric_validate_percentile(object)
  invisible(object)
}

#' Fit a rubric calibration to completed comparative judgments
#'
#' @description
#' Establish a trait-specific rubric conversion from completed Bayesian BTL
#' results. Percentile scoring assigns deterministic, distribution-matched
#' performance levels. The ordinal method backends are not yet implemented.
#'
#' @param cj A completed [fit_bayes_btl_mcmc()] result, a completed within-set
#'   [adaptive_rank()] result (or its `adaptive_state`), or an import-ready Phase A
#'   artifact list. Fixed results with several refits use the last refit.
#'   Raw score tables and intermediate adaptive states are not accepted.
#' @param rubric Data frame with `item_id` and `rubric_score`, optionally `trait`.
#'   Rows align by ID, never position. Missing scores denote unlabeled items.
#'   Use global item IDs for adaptive results and Phase A artifacts that provide
#'   them; otherwise use the original fixed-fit IDs. Omit for percentile scoring.
#' @param method One of `"ordinal_linear"` (default), `"ordinal_monotone"`, or
#'   `"percentile"`. Percentile scoring is norm-referenced/distribution-matched.
#' @param calibration_design `"same_set"` for one completed CJ scale, or
#'   `"linked_anchors"` for a reusable Phase A rubric reference artifact.
#'   Percentile scoring supports only `"same_set"`.
#' @param trait Single trait identifier. Required when absent from CJ metadata.
#'   Each analytic trait requires its own CJ analysis and calibration.
#' @param levels Ordered original rubric labels, from lowest to highest quality.
#'   When omitted, use ordered-factor levels or ascending numeric rubric labels.
#'   Character and unordered-factor scores require explicit levels.
#' @param K Optional number of levels, an integer >= 3. Must agree with `levels`
#'   and any target proportions. Typically 3--6.
#' @param target_distribution Optional positive proportions in level order,
#'   summing to one, for percentile scoring. Omission requests equal proportions.
#'   The absolute sum tolerance is `1e-8`; accepted proportions are normalized
#'   to sum to one for computation, while the requested values are retained.
#'   Named proportions must match the labels in their specified order.
#' @param ... Reserved for future method arguments; currently must be empty.
#'
#' @details
#' Higher CJ locations and higher ordered rubric levels must mean better
#' performance; no orientation is silently reversed. All requested categories
#' must be observed for ordinal calibration.
#'
#' Percentile scoring uses [stats::quantile()] with type 8 at cumulative target
#' proportions. This quantile type is fixed. A score equal to a cutpoint enters
#' the higher category: category is one plus the number of cutpoints less than
#' or equal to the score. Repeated cutpoints can leave categories empty; all
#' exact-score ties receive the same category, even when every score is equal
#' (in which case all items enter the highest category). Requested proportions
#' may be unattainable because of ties or finite sample size. Cutpoint tie counts
#' count all source observations exactly equal to each cutpoint, including a
#' count of one when only one observation equals that cutpoint.
#'
#' These are norm-referenced CJ-derived performance levels, not criterion-referenced
#' scores. Matching a historical rubric distribution does not establish agreement
#' with rubric raters. Recomputing cutpoints on another cohort changes the reference
#' distribution. Stored percentile cutpoints support reuse of the original completed
#' result only; independent cohorts and linked target prediction are not supported.
#' Scoring conditions on accepted point locations, even when CJ posterior draws
#' are available. Category probabilities and uncertainty propagation are unavailable.
#'
#' Phase A artifacts are checked using existing import-readiness rules, including
#' the existing explicit quality-gate override. Import readiness does not assert
#' that the originating adaptive run terminated. Existing CJ diagnostics are
#' retained; failed diagnostics produce a warning.
#'
#' Historical prediction must consume accepted Phase B common-scale scores and
#' reuse the stored reference transformation. Phase B's `theta_link_eap` field
#' represents its accepted MAP location with Laplace/Hessian uncertainty.
#' Rubric scoring is downstream of CJ estimation and does not run comparisons
#' or change Phase B estimation. Ordinal calibration will condition on accepted
#' CJ locations; joint CJ/rubric likelihood estimation is outside this API.
#'
#' @return For percentile scoring, a fitted `pairwiseLLM_rubric_calibration`
#'   containing labels, `K`, normalized `cj`, source `calibration_range`, trait,
#'   provenance, diagnostics, and an identity `transformation` (center 0, scale 1).
#'   Its `backend` stores `quantile_type`, `cutpoints`, `cumulative_probs`,
#'   `requested_proportions`, normalized `effective_proportions`,
#'   `achieved_proportions`, `achieved_counts`, and `cutpoint_tie_counts`.
#'   Category summaries include every requested level, including empty levels.
#'   `target_distribution` retains the requested proportions (or equal defaults).
#'   `calibration_data` and `category_counts` remain `NULL` because no human labels
#'   are fitted. `diagnostics$category_probabilities_available` is `FALSE`.
#'   Valid ordinal fitting calls still raise `pairwiseLLM_rubric_backend_unavailable`.
#' @seealso [predict.pairwiseLLM_rubric_calibration()]
#' @examples
#' \dontrun{
#' # Starting from an already completed CJ result; no sampling occurs here.
#' fit <- fit_rubric_calibration(completed_cj, method = "percentile",
#'   trait = "organization", levels = c("developing", "proficient", "advanced"),
#'   target_distribution = c(0.2, 0.5, 0.3))
#' predict(fit)
#' fit$backend$achieved_proportions
#' fit$backend$cutpoint_tie_counts
#' }
#' @export
fit_rubric_calibration <- function(cj, rubric = NULL, method = "ordinal_linear",
                                   calibration_design = "same_set", trait = NULL,
                                   levels = NULL, K = NULL, target_distribution = NULL, ...) {
  rlang::check_dots_empty()
  object <- .rubric_prepare_calibration(cj, rubric, method, calibration_design, trait, levels, K,
    target_distribution)
  if (method == "percentile") return(.rubric_fit_percentile(object))
  rlang::abort(paste0("The `", method, "` rubric backend is not implemented yet."),
    class = "pairwiseLLM_rubric_backend_unavailable")
}

#' Predict rubric scores from a calibration
#'
#' @param object A `pairwiseLLM_rubric_calibration` object.
#' @param newdata Completed CJ result to predict, or `NULL` for the original
#'   items. Percentile scoring accepts the original completed result with unchanged
#'   item IDs, exact accepted scores and uncertainty, and matching fit/reference
#'   evidence. Item reordering is allowed; collection provenance does not affect
#'   scoring. Raw tables, independent cohorts, refits, and Phase B targets are not
#'   supported for percentile prediction. Future ordinal linked prediction requires
#'   accepted Phase B scores on the stored reference scale.
#' @param hard_score Ordinal hard-score rule: `"median"` (default) or `"mode"`.
#'   Ordinal output will also retain all category probabilities and expected level.
#'   Both choices give the same deterministic category for percentile scoring.
#' @param ... Reserved arguments; currently must be empty.
#' @return For percentile scoring, a tibble with `item_id`, accepted source `theta`,
#'   integer `category` in `1:K`, original-label `rubric_score`, and `extrapolated`
#'   (outside the fitted CJ range). Under the original-result restriction,
#'   extrapolation flags are always false. Stored cutpoints are reused unchanged.
#'   No category probabilities or probabilistic summaries are returned.
#'   Unfitted objects fail clearly; ordinal prediction backends remain unavailable.
#' @seealso [fit_rubric_calibration()]
#' @export
predict.pairwiseLLM_rubric_calibration <- function(object, newdata = NULL,
                                                 hard_score = c("median", "mode"), ...) {
  rlang::check_dots_empty()
  .rubric_validate_calibration(object)
  if (missing(hard_score)) hard_score <- "median"
  .rubric_choice(hard_score, c("median", "mode"), "hard_score")
  if (object$status != "fitted") rlang::abort("Cannot predict from an unfitted rubric calibration.")
  if (object$method == "percentile") return(.rubric_predict_percentile(object, newdata))
  rlang::abort("Rubric prediction backends are not implemented yet.",
    class = "pairwiseLLM_rubric_backend_unavailable")
}
