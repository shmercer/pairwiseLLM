#' Construct reusable warm-start cross-validation partitions
#'
#' @param ids Unique item IDs in the same order as `theta`.
#' @param theta Finite numeric scores from one assessment.
#' @param task_id One nonblank assessment label.
#' @param seed Integer random seed, default 1.
#' @param outer_folds,inner_folds Fold counts, each at least two. Counts are never
#'   reduced automatically; every training split needs three nonconstant outcomes.
#' @return A portable `pairwiseLLM_warm_cv_plan` list, format 1. It contains exact
#'   ordered IDs and outcomes, task identity, seed/RNG provenance, named outer and
#'   inner assignments, and an integrity digest. Save with [saveRDS()].
#' @details
#' Partitions use outcome-ranked blocks with randomized ties and fold labels.
#' Draw order is outer folds, inner folds for outer training sets in fold order,
#' then full-data inner folds. The caller's RNG kind and seed state are preserved.
#' Plans are independent of feature schema and engine. A supplied plan is checked
#' before extraction or fitting and is never regenerated or silently realigned.
#' Its digest detects accidental changes, not authorship. Plans contain outcomes
#' and item IDs; they are development evidence, not anonymized artifacts.
#' @family adaptive warm start
#' @seealso [fit_warm_start_model()]
#' @examples
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   plan <- make_warm_start_cv_plan(as.character(1:20), seq_len(20), "example")
#'   plan$outer_foldid
#' }
#' @export
make_warm_start_cv_plan <- function(ids, theta, task_id, seed = 1L,
                                    outer_folds = 5L, inner_folds = 5L) {
  ids <- .warm_start_ids(ids)
  .warm_start_outcome_values(theta)
  .warm_start_task_id(task_id)
  if (length(theta) != length(ids)) rlang::abort("Supply one theta value per ID in the supplied ID order.")
  theta <- as.numeric(theta)
  .warm_start_outcome_fit(theta)
  .warm_start_plan_seed(seed)
  .warm_start_fold_count(outer_folds, length(ids))
  .warm_start_fold_count(inner_folds, length(ids) - ceiling(length(ids) / outer_folds))
  if (!requireNamespace("withr", quietly = TRUE)) {
    rlang::abort("CV plans require optional package 'withr'. Install it explicitly first.")
  }
  plan <- .pairwiseLLM_with_seed(seed, function() {
    outer <- .warm_start_folds(theta, outer_folds)
    inner <- lapply(seq_len(outer_folds), function(fold) {
      train <- which(outer != fold)
      stats::setNames(.warm_start_folds(theta[train], inner_folds), ids[train])
    })
    list(format_version = 1L, task_id = task_id, ids = ids, theta = theta,
      outcome = list(definition = "within_task_z", sd_convention = "sample"),
      seed = as.integer(seed), rng_kind = RNGkind(), outer_folds = as.integer(outer_folds),
      inner_folds = as.integer(inner_folds), outer_foldid = stats::setNames(outer, ids),
      outer_inner_foldid = inner,
      full_inner_foldid = stats::setNames(.warm_start_folds(theta, inner_folds), ids))
  })
  plan$digest <- .warm_start_prior_hash(plan)
  class(plan) <- "pairwiseLLM_warm_cv_plan"
  .validate_warm_start_cv_plan(plan)
  plan
}

.warm_start_plan_seed <- function(seed) {
  if (!.warm_start_number(seed, 0, .Machine$integer.max) || seed != floor(seed)) {
    rlang::abort("`seed` must be an integer from zero through .Machine$integer.max.")
  }
}

.warm_start_plan_fields <- function() {
  c("format_version", "task_id", "ids", "theta", "outcome", "seed", "rng_kind",
    "outer_folds", "inner_folds", "outer_foldid", "outer_inner_foldid", "full_inner_foldid", "digest")
}

.validate_warm_start_plan_split <- function(foldid, ids, theta, k) {
  if (!is.integer(foldid) || !is.null(dim(foldid)) || is.object(foldid) ||
      !identical(names(foldid), ids) || length(foldid) != length(ids) || anyNA(foldid) ||
      !identical(sort(unique(unname(foldid))), seq_len(k))) {
    rlang::abort("Invalid CV plan fold assignments or ordered IDs.")
  }
  .warm_start_fold_count(k, length(ids))
  for (fold in seq_len(k)) {
    train <- theta[foldid != fold]
    if (length(train) < 3L) rlang::abort("CV plan training splits need at least three rows.")
    .warm_start_outcome_fit(train)
  }
}

.validate_warm_start_cv_plan <- function(plan, ids = plan$ids, theta = plan$theta,
                                         task_id = plan$task_id) {
  invalid <- function() rlang::abort("Invalid CV plan identity, structure or integrity digest.")
  if (!identical(class(plan), "pairwiseLLM_warm_cv_plan") || !is.list(plan) ||
      !identical(names(plan), .warm_start_plan_fields()) || !.warm_start_portable(plan) ||
      !identical(plan$format_version, 1L)) invalid()
  .warm_start_task_id(plan$task_id)
  .warm_start_outcome_values(plan$theta)
  if (!identical(plan$ids, .warm_start_ids(plan$ids)) ||
      !identical(plan$theta, as.numeric(plan$theta)) || length(plan$theta) != length(plan$ids) ||
      !identical(plan$ids, ids) || !identical(plan$theta, as.numeric(theta)) ||
      !identical(plan$task_id, task_id) ||
      !identical(plan$outcome, list(definition = "within_task_z", sd_convention = "sample"))) invalid()
  .warm_start_plan_seed(plan$seed)
  if (!identical(plan$seed, as.integer(plan$seed))) invalid()
  .warm_start_plan_rng(plan$rng_kind)
  .warm_start_fold_count(plan$outer_folds, length(ids))
  .warm_start_fold_count(plan$inner_folds, length(ids) - ceiling(length(ids) / plan$outer_folds))
  if (!is.integer(plan$outer_folds) || !is.integer(plan$inner_folds) ||
      !is.list(plan$outer_inner_foldid) || !is.null(names(plan$outer_inner_foldid)) ||
      length(plan$outer_inner_foldid) != plan$outer_folds) invalid()
  .validate_warm_start_plan_split(plan$outer_foldid, ids, theta, plan$outer_folds)
  for (fold in seq_len(plan$outer_folds)) {
    train <- which(plan$outer_foldid != fold)
    .validate_warm_start_plan_split(plan$outer_inner_foldid[[fold]], ids[train], theta[train], plan$inner_folds)
  }
  .validate_warm_start_plan_split(plan$full_inner_foldid, ids, theta, plan$inner_folds)
  canonical <- unclass(plan)[setdiff(.warm_start_plan_fields(), "digest")]
  if (!identical(plan$digest, .warm_start_prior_hash(canonical))) invalid()
  invisible(plan)
}

.warm_start_plan_rng <- function(kind) {
  allowed <- list(c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister",
    "Knuth-TAOCP", "user-supplied", "Knuth-TAOCP-2002", "L'Ecuyer-CMRG"),
    c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller", "user-supplied", "Inversion", "Kinderman-Ramage"),
    c("Rounding", "Rejection"))
  if (!is.character(kind) || !is.null(attributes(kind)) || length(kind) != 3L || anyNA(kind) ||
      !all(vapply(seq_len(3L), function(i) kind[i] %in% allowed[[i]], logical(1)))) {
    rlang::abort("Invalid CV plan RNG provenance.")
  }
}

.warm_start_cv_identity <- function(plan) {
  list(format_version = 1L, digest = plan$digest, task_id = plan$task_id, n = length(plan$ids),
    outcome_digest = .warm_start_prior_hash(list(ids = plan$ids, theta = plan$theta, outcome = plan$outcome)),
    seed = plan$seed, outer_folds = plan$outer_folds, inner_folds = plan$inner_folds, rng_kind = plan$rng_kind)
}
