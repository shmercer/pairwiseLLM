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
  if (object$calibration_design == "linked_anchors") .rubric_validate_linked_calibration(object)
  if (object$status == "fitted" && is.null(object$backend)) {
    rlang::abort("A fitted rubric calibration requires a statistical backend result.")
  }
  if (object$status == "unfitted" && (!is.null(object$backend) || !is.null(object$transformation))) {
    rlang::abort("An unfitted calibration cannot contain statistical results.")
  }
  if (object$status == "fitted" && object$method == "percentile") .rubric_validate_percentile(object)
  if (object$status == "fitted" && object$method == "ordinal_linear") .rubric_validate_ordinal_linear(object)
  if (object$status == "fitted" && object$method == "ordinal_monotone") .rubric_validate_ordinal_monotone(object)
  invisible(object)
}

.rubric_same_set_fit_evidence <- function(cj) {
  contract <- cj$fit_contract
  # A completed fit may be supplied with its item axis reordered.
  for (field in grep("^theta_", names(contract), value = TRUE)) {
    value <- contract[[field]]
    if (is.numeric(value) && is.null(dim(value)) && !is.null(names(value))) {
      contract[[field]] <- value[order(names(value))]
    }
  }
  list(model_variant = cj$model_variant, estimation_mode = cj$estimation_mode,
    orientation = cj$orientation, fit_contract = contract, fit_contract_hash = cj$fit_contract_hash,
    reference = cj$reference, provenance = cj$provenance[setdiff(names(cj$provenance), "collection")])
}

.rubric_same_set_prediction_items <- function(object, newdata) {
  if (is.null(newdata)) return(object$cj$items)
  cj <- .rubric_normalize_cj(newdata, object$trait, scale_status = object$cj$scale_status,
    include_draws = FALSE)
  source <- object$cj$items
  items <- cj$items
  # Configuration hashes alone do not identify a CJ metric. Require the same
  # item domain, exact accepted locations/uncertainty, and original fit evidence.
  if (!setequal(items$item_id, source$item_id) ||
    !identical(items[order(items$item_id), ], source[order(source$item_id), ]) ||
    !identical(.rubric_same_set_fit_evidence(cj), .rubric_same_set_fit_evidence(object$cj))) {
    rlang::abort(paste0("Same-set `newdata` must reuse the original completed CJ result with unchanged ",
      "items, accepted scores, and fit evidence. Independent cohorts and refits are not supported."))
  }
  items
}

#' Fit a rubric calibration to completed comparative judgments
#'
#' @description
#' Establish a trait-specific rubric conversion from completed Bayesian BTL
#' results. Percentile scoring assigns deterministic, distribution-matched
#' performance levels. Linear ordinal calibration fits human rubric labels
#' with a proportional-odds cumulative-logit model. Monotone ordinal calibration
#' replaces its linear effect with a nondecreasing penalized cubic spline.
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
#'   Both ordinal methods support both designs. Percentile scoring supports
#'   only `"same_set"`.
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
#' @param ... For `ordinal_monotone` only, uniquely named `k` and `sp` controls.
#'   `k` is the cubic basis dimension (default 6, integer >= 5). It is reduced
#'   with a warning to the number of unique labeled CJ scores if necessary;
#'   fewer than five unique scores fail. `sp = NULL` (default) estimates smoothing
#'   by extended Fellner-Schall (EFS); a finite positive scalar fixes the penalty
#'   parameter. Other methods require empty dots. Unknown controls fail.
#'
#' @details
#' Higher CJ locations and higher ordered rubric levels must mean better
#' performance; no orientation is silently reversed. All requested categories
#' must be observed for ordinal calibration.
#'
#' Fitting `ordinal_linear` requires the optional package \pkg{ordinal}.
#' Install it with `install.packages("ordinal")`. Percentile scoring and
#' prediction from an already fitted linear calibration do not require it.
#'
#' Monotone ordinal fitting and prediction require optional \pkg{mgcv} >= 1.9-4
#' (`install.packages("mgcv")`). Fitting also requires optional \pkg{withr}
#' (`install.packages("withr")`) to isolate backend RNG use with a fixed internal
#' seed. Saved monotone models require \pkg{mgcv} for spline prediction.
#'
#' For both ordinal methods, calibration items must belong to one completed
#' trait-specific CJ fit. Only labeled rows estimate the calibration:
#' `z = (theta - mu_cal) / sigma_cal`, where `mu_cal` is their mean and
#' `sigma_cal` their sample standard deviation. Both are stored and reused.
#' [ordinal::clm()] fits `logit P(Y <= k | z) = tau_k - beta * z` with flexible
#' ordered thresholds. Zero or negative slopes produce a diagnostic warning;
#' coding is never reversed. Missing categories or degenerate calibration scores
#' fail. Singleton categories and numerical convergence/identification problems
#' produce warnings; no universal minimum calibration sample size is enforced.
#' Finite ordered estimates can be retained with warnings even when convergence
#' or uncertainty is unreliable. Such warnings require review before use.
#' Standard errors condition on estimated CJ point locations; CJ measurement
#' uncertainty is not propagated. [evaluate_rubric_predictions()] supplies
#' metrics and optional assumption diagnostics; diagnostic refits run only when
#' requested during evaluation. Internal rubric-label validation holds CJ fixed
#' and refits calibration from training labels alone.
#'
#' `ordinal_monotone` uses [mgcv::scasm()] with
#' `s(z, bs = "sc", xt = "m+", k = k)` and [mgcv::ocat()] with integer categories
#' `1:K`. The model is `logit P(Y <= k | z) = tau_k - eta(z)`, where `eta` includes
#' an intercept and a nondecreasing smooth. The backend fixes the first threshold
#' at -1 for identification; its identity link describes the latent location,
#' while category cumulative probabilities follow the logistic distribution.
#' Bootstrap is disabled. Smoothing is estimated by EFS unless `sp` is fixed.
#' A 1,001-point calibration grid verifies nondecreasing latent locations and
#' nonincreasing cumulative probabilities with tolerance `1e-8`. Invalid
#' probabilities or monotonicity failures abort; no unconstrained fallback is used.
#' Essentially flat effects, singleton categories, and convergence problems warn.
#' Small bases and some reversed/separated label patterns can fail in the backend;
#' these return contextual fit errors. The five-unique-score requirement is a
#' backend feasibility guard, not a recommended calibration sample size.
#' Threshold uncertainty is unavailable from this wrapper (the fixed first
#' threshold has standard error zero; the remaining entries are `NA`).
#' Numerical median cutpoints are reported only for unique crossings within the
#' calibration range. Flat or unresolved crossings and thresholds outside the
#' range have `NA` cutpoints and an explanatory status; scoring always uses
#' probabilities. Predictions beyond the labeled range are flagged and retain
#' the backend's spline extrapolation; their calibration is not established.
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
#' are available. Percentile category probabilities and uncertainty propagation
#' are unavailable.
#'
#' Phase A artifacts are checked using existing import-readiness rules, including
#' the existing explicit quality-gate override. Import readiness does not assert
#' that the originating adaptive run terminated. Existing CJ diagnostics are
#' retained; failed diagnostics produce a warning.
#'
#' With `linked_anchors`, first obtain an import-ready Phase A artifact for the
#' human-scored `rubric_reference_set` and fit its ordinal calibration. Next run
#' Phase A for the target set, then explicitly select an E1--E3 estimator in
#' [prepare_link_input()], supplying the reference artifact as the hub input.
#' Pass the [fit_link()] result or [start_link_session()] session to `predict()`.
#' E1 needs points, E2 needs posterior draws, and E3 needs raw within-set rows.
#' Rubric labels are used for calibration; Phase B does not require them.
#' A rubric reference set contains externally scored material; a Phase B
#' **hub anchor** is an item selected for routing recurring comparisons.
#'
#' Historical prediction validates the stored reference set, stable item IDs,
#' exact reference locations and uncertainty, within-set evidence, trait,
#' orientation, and canonical fit contract. A configuration hash alone does not
#' establish metric identity. Original hashes are retained as provenance;
#' compatible legacy hashes follow the existing Phase A import rules.
#' Target-only Phase A scores cannot be used with the stored calibration.
#' Prediction consumes accepted Phase B common-scale scores and reuses the
#' stored reference transformation, never the target cohort's mean or SD.
#' `theta_link_eap` aliases the estimator's point estimate: posterior mean for
#' E1/E3-MCMC, MAP for E2/E3 Laplace. Prediction propagates the reported
#' uncertainty scope (conditional on fixed shapes for E1; shapes and offset for
#' E2/E3). Scores are translated from the centered linking origin to the original
#' reference origin by adding the frozen hub mean, without rescaling or using
#' target labels. E2/E3 hub posteriors may update; calibration stays attached to
#' the original Phase A hub artifact. Changed artifact identity requires a new
#' link. Invalid or unidentified fits and legacy anchored-joint sessions fail.
#' Rubric scoring is downstream of CJ estimation and does not run comparisons
#' or change Phase B estimation. Ordinal calibration conditions on accepted
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
#'   For linear ordinal calibration, the same class retains ID-aligned
#'   `calibration_data` (including unlabeled items), `category_counts`, and the
#'   labeled `calibration_range`. `transformation$center` and `$scale` store
#'   `mu_cal` and `sigma_cal`. The `backend` contains `name`, `version`, `link`,
#'   `threshold`, the fitted `model`, `thresholds`, `slope`, `vcov`,
#'   `standard_errors`, and `convergence`. With positive slope, `cutpoints_z`
#'   stores `tau / beta` and `cutpoints_theta` maps these to the original scale.
#'   Otherwise cutpoints are omitted. Unavailable coefficient uncertainty is
#'   represented by `NA`, with a warning. `diagnostics$ordinal` reports numerical
#'   convergence, gradient, Hessian condition, covariance availability,
#'   nonpositive slope, singleton categories, and conditioning on CJ locations.
#'   `warnings` retains ordinal diagnostic messages and
#'   `diagnostics$category_probabilities_available` is `TRUE`.
#'   Monotone ordinal calibration uses the same class, data, transformation,
#'   and probability availability flag. Its `backend` stores `name`, `version`,
#'   cumulative `link`, `latent_link`, fitted `model`, `thresholds`, `intercept`,
#'   `basis` (requested/effective dimension, constraint, order, knots),
#'   `smoothing` (method, requested/fitted penalty parameters, penalty matrices),
#'   total `edf`, `smooth_edf`, `convergence`, `threshold_standard_errors`,
#'   `cutpoints_z`, `cutpoints_theta`, and `cutpoint_status` (`"unique"`,
#'   `"outside_range"`, or `"flat_or_unresolved"`). `diagnostics$ordinal` stores
#'   convergence, singleton categories, conditioning on CJ, unavailable threshold
#'   uncertainty, and numerical `monotonicity` results.
#'   Linked calibrations additionally retain `reference`: the reference `set_id`,
#'   sorted stable item IDs with original Phase A locations/SDs, canonical
#'   `fit_contract`, original `fit_contract_hash`, and within-set evidence/hash.
#'   Original reference SDs are retained separately from estimator-specific linked SDs.
#' @family rubric calibration
#' @seealso [predict.pairwiseLLM_rubric_calibration()], [evaluate_rubric_predictions()],
#'   `vignette("rubric-calibration", package = "pairwiseLLM")` for practical workflows.
#' @examples
#' \dontrun{
#' # Starting from an already completed CJ result; no sampling occurs here.
#' fit <- fit_rubric_calibration(completed_cj, method = "percentile",
#'   trait = "organization", levels = c("developing", "proficient", "advanced"),
#'   target_distribution = c(0.2, 0.5, 0.3))
#' predict(fit)
#' fit$backend$achieved_proportions
#' fit$backend$cutpoint_tie_counts
#' # Human labels cover a subset of this same completed CJ result.
#' if (requireNamespace("ordinal", quietly = TRUE)) {
#'   ordinal_fit <- fit_rubric_calibration(completed_cj, rubric = rubric_labels,
#'     trait = "organization", levels = c("developing", "proficient", "advanced"))
#'   predictions <- predict(ordinal_fit)
#'   predictions$probabilities
#'   predictions$expected_level
#' }
#' if (requireNamespace("mgcv", quietly = TRUE) &&
#'     packageVersion("mgcv") >= "1.9.4" && requireNamespace("withr", quietly = TRUE)) {
#'   monotone_fit <- fit_rubric_calibration(completed_cj, rubric = rubric_labels,
#'     method = "ordinal_monotone", trait = "organization",
#'     levels = c("developing", "proficient", "advanced"), k = 6)
#'   predict(monotone_fit)$probabilities
#' }
#' # Reference and target Phase A, followed by Phase B, are completed upstream.
#' if (requireNamespace("ordinal", quietly = TRUE)) {
#'   linked_fit <- fit_rubric_calibration(reference_phase_a, reference_labels,
#'     calibration_design = "linked_anchors", trait = "organization",
#'     levels = c("developing", "proficient", "advanced"))
#'   predict(linked_fit) # Original reference items.
#'   target_scores <- predict(linked_fit, completed_phase_b)
#'   target_scores$set_id
#'   attr(target_scores, "linking")$diagnostics
#' }
#' }
#' @export
fit_rubric_calibration <- function(cj, rubric = NULL, method = "ordinal_linear",
                                   calibration_design = "same_set", trait = NULL,
                                   levels = NULL, K = NULL, target_distribution = NULL, ...) {
  controls <- if (isTRUE(method == "ordinal_monotone")) .rubric_monotone_controls(...) else rlang::check_dots_empty()
  object <- .rubric_prepare_calibration(cj, rubric, method, calibration_design, trait, levels, K,
    target_distribution)
  if (method == "percentile") return(.rubric_fit_percentile(object))
  if (method == "ordinal_linear") {
    return(.rubric_fit_ordinal_linear(object))
  }
  if (method == "ordinal_monotone") {
    return(.rubric_fit_ordinal_monotone(object, controls))
  }
  rlang::abort(paste0("The `", method, "` rubric backend is not implemented yet for `", calibration_design, "`."),
    class = "pairwiseLLM_rubric_backend_unavailable")
}

#' Predict rubric scores from a calibration
#'
#' @param object A `pairwiseLLM_rubric_calibration` object.
#' @param newdata Completed CJ result to predict, or `NULL` for the original
#'   items. Same-set scoring accepts the original completed result with unchanged
#'   item IDs, exact accepted scores and uncertainty, and matching fit/reference
#'   evidence. Item reordering is allowed; collection provenance does not affect
#'   scoring. Raw tables, independent cohorts, refits, and Phase B targets are not
#'   supported for same-set prediction. For `linked_anchors`, `NULL` scores the
#'   original reference items; explicit `newdata` must be a completed Phase B
#'   result (or its state) on the stored reference scale. It returns only target
#'   items across all spokes, in their input order. Every spoke must have an
#'   accepted Phase B refit and committed active hub-spoke evidence.
#' @param hard_score Ordinal hard-score rule: `"median"` (default) or `"mode"`.
#'   The median is the lowest category whose cumulative probability is at least
#'   0.5; equality at a median cutpoint therefore selects the lower category.
#'   Modal ties select the lowest category. Ordinal output always retains all
#'   category probabilities, both decision rules, and expected level.
#'   Both choices give the same deterministic category for percentile scoring.
#' @param ... Reserved arguments; currently must be empty.
#' @return For percentile scoring, a tibble with `item_id`, accepted source `theta`,
#'   integer `category` in `1:K`, original-label `rubric_score`, and `extrapolated`
#'   (outside the fitted CJ range). Under the original-result restriction,
#'   extrapolation flags are always false. Stored cutpoints are reused unchanged.
#'   No category probabilities or probabilistic summaries are returned.
#'   Both ordinal methods include the same five columns, plus
#'   `probabilities`, a list-column of numeric K-vectors named by the original
#'   ordered labels; integer `median_category` and `modal_category`; and
#'   `expected_level = sum(k * P(Y = k))` for internal indices `k` in `1:K`,
#'   regardless of the original labels' numerical spacing. This expected rubric
#'   level is a continuous summary, not a replacement for the ordinal result.
#'   `category` and `rubric_score` use the requested `hard_score` rule.
#'   Ordinal `extrapolated` flags use the labeled calibration range, so unlabeled
#'   source items can be extrapolated; endpoints are included in the range.
#'   Stored standardization is reused. Unfitted objects fail clearly.
#'   Linked target output adds `set_id` (target spoke), `source_item_id`,
#'   `global_item_id`, and `theta_sd` (accepted Phase B uncertainty, possibly
#'   `NA`). Its `linking` attribute contains normalized `reference`, `fit_contract`,
#'   `provenance` (including hub/spoke IDs and linking stage logs), `diagnostics`,
#'   and `reliability`. Provenance estimation/uncertainty methods reflect validated
#'   spoke contracts, including accepted-state reuse. Each is a single string when
#'   all spokes agree, otherwise the distinct strings in spoke order; per-spoke
#'   contracts retain the complete attribution. Failed upstream diagnostics warn and remain available;
#'   accepted scores do not assert adequate precision or successful stopping.
#'   CJ uncertainty is retained as metadata and is not propagated into category
#'   probabilities. Reference predictions and same-set output keep their existing
#'   columns and have no `linking` attribute.
#' @family rubric calibration
#' @seealso [fit_rubric_calibration()], [evaluate_rubric_predictions()]
#' @export
predict.pairwiseLLM_rubric_calibration <- function(object, newdata = NULL,
                                                 hard_score = c("median", "mode"), ...) {
  rlang::check_dots_empty()
  .rubric_validate_calibration(object)
  if (missing(hard_score)) hard_score <- "median"
  .rubric_choice(hard_score, c("median", "mode"), "hard_score")
  if (object$status != "fitted") rlang::abort("Cannot predict from an unfitted rubric calibration.")
  if (object$calibration_design == "linked_anchors" && !is.null(newdata)) {
    return(.rubric_predict_linked(object, newdata, hard_score))
  }
  if (object$method == "percentile") return(.rubric_predict_percentile(object, newdata))
  if (object$method == "ordinal_linear") return(.rubric_predict_ordinal_linear(object, newdata, hard_score))
  if (object$method == "ordinal_monotone") return(.rubric_predict_ordinal_monotone(object, newdata, hard_score))
  rlang::abort("Rubric prediction backends are not implemented yet.",
    class = "pairwiseLLM_rubric_backend_unavailable")
}
