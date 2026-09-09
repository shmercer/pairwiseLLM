#' Convert warm-start predictions to Bayesian BTL priors
#'
#' @param predictions Calibrated single-model or ensemble predictions, or finite
#'   numeric scores with names or explicit `ids`. Numeric input is an expert choice
#'   of relative prior location, not an automatic calibration method.
#' @param ids Active item IDs. For prediction tables and named scores these must
#'   match the input ID set exactly and determine output order. For unnamed scores
#'   they identify input positions. Defaults to the IDs in the input.
#' @param prior_sd Positive finite scalar or vector. An unnamed vector follows
#'   input order; a named vector aligns by ID. Defaults to 0.5.
#' @return A version-1 `pairwiseLLM_warm_prior` list containing `item_id`, `scores`,
#'   `prior_mean`, `prior_sd`, compact `diagnostics`, `provenance`, and an integrity
#'   `digest`. Pass this object to [fit_bayes_btl_mcmc()] or [adaptive_rank_start()].
#' @details
#' Single-model values must have learned OOF calibration. Ensembles use their
#' equal-weight calibrated mean. Scores are centered over the active items in R;
#' adaptive scoped refits subset the saved scores and center again in that scope.
#' Calibration is not applied twice and original training BTL units are not used.
#' Ensemble component predictions and sample disagreement SD remain diagnostics;
#' disagreement never supplies the Bayesian prior SD automatically.
#'
#' The normal prior applies to `theta_raw`; Stan centers this to obtain `theta`.
#' Centering induces dependence, so the supplied SD is not the marginal SD of
#' centered theta. Without predictive input, BTL retains raw prior mean 0 and SD 1.
#' This estimation prior is separate from initial warm-start pairing schedules.
#' @examples
#' prior <- make_warm_start_prior(c(a = -1, b = 0, c = 1))
#' prior$prior_mean
#' @export
make_warm_start_prior <- function(predictions, ids = NULL, prior_sd = 0.5) {
  diagnostics <- data.frame()
  provenance <- list(source = "numeric", schema = NULL, model = NULL,
    sd_rule = if (length(prior_sd) == 1L) "scalar" else "per_item")
  if (is.numeric(predictions) && is.null(dim(predictions))) {
    input_ids <- names(predictions) %||% ids
    scores <- as.double(predictions)
  } else {
    parsed <- .warm_start_prior_predictions(predictions)
    input_ids <- predictions$item_id
    scores <- parsed$scores
    diagnostics <- parsed$diagnostics
    provenance$source <- "predictions"
    provenance$schema <- attr(predictions, "warm_start_schema", exact = TRUE)
    provenance$model <- attr(predictions, "warm_start_model", exact = TRUE)
  }
  input_ids <- .warm_start_prior_ids(input_ids)
  ids <- .warm_start_prior_ids(ids %||% input_ids)
  if (!setequal(ids, input_ids)) rlang::abort("Prior input IDs must match active IDs exactly.")
  .warm_start_prior_numeric(scores, length(input_ids), "scores")
  sd <- .warm_start_prior_sd(prior_sd, input_ids)
  index <- match(ids, input_ids)
  if (nrow(diagnostics)) diagnostics <- diagnostics[index, , drop = FALSE]
  .warm_start_prior_new(ids, scores[index], sd[index], diagnostics, provenance)
}

.warm_start_prior_ids <- function(ids) {
  if ((!is.character(ids) && !is.numeric(ids)) || !is.null(dim(ids)) || length(ids) < 2L ||
      anyNA(ids) || any(!nzchar(trimws(as.character(ids)))) || anyDuplicated(as.character(ids))) {
    rlang::abort("Prior IDs must contain at least two unique nonmissing, nonblank IDs.")
  }
  as.character(ids)
}

.warm_start_prior_numeric <- function(x, n, label, positive = FALSE) {
  if (!is.numeric(x) || !is.null(dim(x)) || length(x) != n || any(!is.finite(x)) ||
      (positive && any(x <= 0))) {
    rlang::abort(paste0("Prior ", label, " must be finite numeric values",
      if (positive) " greater than zero" else "", ", one per item."))
  }
  invisible(x)
}

.warm_start_prior_sd <- function(sd, ids) {
  if (!is.null(names(sd))) {
    sd_ids <- as.character(names(sd))
    if (anyNA(sd_ids) || anyDuplicated(sd_ids) || !setequal(sd_ids, ids)) {
      rlang::abort("Named prior SD IDs must match input IDs exactly.")
    }
    sd <- sd[match(ids, sd_ids)]
  } else if (length(sd) == 1L) {
    sd <- rep(sd, length(ids))
  }
  .warm_start_prior_numeric(sd, length(ids), "SDs", positive = TRUE)
  unname(as.double(sd))
}

.warm_start_prior_predictions <- function(x) {
  if (!is.data.frame(x) || !"item_id" %in% names(x) || anyDuplicated(names(x))) {
    rlang::abort("Supply calibrated prediction tables or numeric scores with IDs.")
  }
  .warm_start_prior_ids(x$item_id)
  schema <- attr(x, "warm_start_schema", exact = TRUE)
  if (is.null(schema)) rlang::abort("Prediction feature schema is missing.")
  warm_start_feature_schema(schema)
  meta <- attr(x, "warm_start_model", exact = TRUE)
  if (!is.list(meta) || !identical(meta$outcome_definition, "within_task_z")) {
    rlang::abort("Prediction metadata requires the within_task_z outcome definition.")
  }
  check_model <- function(m) {
    if (!is.list(m) || !isTRUE(m$format_version %in% c(1L, 2L)) ||
        !identical(m$outcome_definition, "within_task_z") ||
        !identical(m$calibration_status, "oof_linear")) {
      rlang::abort("Model predictions require supported format and learned oof_linear calibration.")
    }
  }
  if (identical(meta$artifact_type, "ensemble")) {
    columns <- attr(x, "component_columns", exact = TRUE)
    if (!identical(meta$format_version, 1L) || !identical(meta$weighting, "equal") ||
        !is.character(columns) || length(columns) < 2L || is.null(names(columns)) ||
        anyDuplicated(columns) || !all(c(columns, "ensemble_mean", "ensemble_sd") %in% names(x)) ||
        !identical(names(meta$components), names(columns))) {
      rlang::abort("Invalid ensemble prediction contract.")
    }
    .warm_start_component_names(names(columns))
    lapply(meta$components, check_model)
    lapply(x[, columns, drop = FALSE], .warm_start_prior_numeric, n = nrow(x), label = "components")
    .warm_start_prior_numeric(x$ensemble_mean, nrow(x), "ensemble means")
    .warm_start_prior_numeric(x$ensemble_sd, nrow(x), "ensemble disagreement SDs")
    scores <- rowMeans(as.matrix(x[, columns, drop = FALSE]))
    sd <- apply(as.matrix(x[, columns, drop = FALSE]), 1L, stats::sd)
    if (!isTRUE(all.equal(scores, x$ensemble_mean, check.attributes = FALSE)) ||
        !isTRUE(all.equal(sd, x$ensemble_sd, check.attributes = FALSE))) {
      rlang::abort("Ensemble mean/SD disagree with component predictions.")
    }
    diagnostics <- as.data.frame(lapply(x[, c(columns, "ensemble_sd"), drop = FALSE], as.double))
  } else {
    check_model(meta)
    scores <- x$calibrated_prediction
    .warm_start_prior_numeric(x$raw_prediction, nrow(x), "raw predictions")
    diagnostics <- data.frame(raw_prediction = x$raw_prediction)
  }
  .warm_start_prior_numeric(scores, nrow(x), "calibrated predictions")
  list(scores = as.double(scores), diagnostics = diagnostics)
}

.warm_start_prior_hash <- function(x) {
  path <- tempfile("warm-prior-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  # Version-2 XDR is portable; omit the writer/minimum-reader R version words
  # (bytes 7:14) so a runtime upgrade does not change an unchanged prior digest.
  bytes <- serialize(x, NULL, version = 2, xdr = TRUE)
  writeBin(bytes[-(7:14)], path)
  unname(tools::md5sum(path))
}

.warm_start_prior_new <- function(ids, scores, sd, diagnostics, provenance) {
  if (!.warm_start_portable(provenance)) rlang::abort("Prior provenance must contain only portable values.")
  means <- scores - mean(scores)
  .warm_start_prior_numeric(means, length(ids), "centered means")
  rownames(diagnostics) <- NULL
  out <- structure(list(format_version = 1L, item_id = ids, scores = unname(scores),
    prior_mean = unname(means), prior_sd = unname(sd), diagnostics = diagnostics,
    provenance = provenance), class = "pairwiseLLM_warm_prior")
  out$digest <- .warm_start_prior_hash(out)
  out
}

.validate_warm_start_prior <- function(prior, ids = NULL) {
  fields <- c("format_version", "item_id", "scores", "prior_mean", "prior_sd",
    "diagnostics", "provenance", "digest")
  if (!inherits(prior, "pairwiseLLM_warm_prior") || !is.list(prior) ||
      !identical(names(prior), fields) || !identical(prior$format_version, 1L)) {
    rlang::abort("Invalid or unsupported warm-start prior contract/version.")
  }
  input_ids <- .warm_start_prior_ids(prior$item_id)
  if (!is.null(ids) && !setequal(input_ids, .warm_start_prior_ids(ids))) {
    rlang::abort("Prior IDs must match active IDs exactly.")
  }
  for (field in c("scores", "prior_mean", "prior_sd")) {
    .warm_start_prior_numeric(prior[[field]], length(input_ids), field, field == "prior_sd")
  }
  if (!identical(prior$prior_mean, unname(prior$scores - mean(prior$scores))) ||
      !is.data.frame(prior$diagnostics) || !nrow(prior$diagnostics) %in% c(0L, length(input_ids)) ||
      !is.list(prior$provenance) || !.warm_start_portable(prior)) {
    rlang::abort("Invalid warm-start prior centering, diagnostics, or provenance.")
  }
  lapply(prior$diagnostics, .warm_start_prior_numeric, n = length(input_ids), label = "diagnostics")
  value <- prior
  value$digest <- NULL
  if (!identical(prior$digest, .warm_start_prior_hash(value))) {
    rlang::abort("Warm-start prior integrity digest mismatch.")
  }
  invisible(prior)
}

.warm_start_prior_scope <- function(prior, ids, exact = FALSE) {
  ids <- .warm_start_prior_ids(ids)
  if (is.null(prior)) return(NULL)
  .validate_warm_start_prior(prior, if (exact) ids else NULL)
  index <- match(ids, prior$item_id)
  if (anyNA(index)) rlang::abort("Active BTL IDs are missing from the saved prior.")
  diagnostics <- prior$diagnostics
  if (nrow(diagnostics)) diagnostics <- diagnostics[index, , drop = FALSE]
  .warm_start_prior_new(ids, prior$scores[index], prior$prior_sd[index], diagnostics, prior$provenance)
}

.warm_start_btl_prior_data <- function(prior, ids) {
  prior <- .warm_start_prior_scope(prior, ids, exact = TRUE)
  list(prior_mean = if (is.null(prior)) rep(0, length(ids)) else prior$prior_mean,
    prior_sd = if (is.null(prior)) rep(1, length(ids)) else prior$prior_sd)
}

.warm_start_validate_fit_prior <- function(prior, ids) {
  if (!is.list(prior) || !identical(names(prior), c("item_id", "prior_mean", "prior_sd")) ||
      !identical(prior$item_id, ids)) {
    rlang::abort("Fit theta prior IDs must match fitted IDs in order.")
  }
  .warm_start_prior_numeric(prior$prior_mean, length(ids), "means")
  .warm_start_prior_numeric(prior$prior_sd, length(ids), "SDs", positive = TRUE)
  invisible(prior)
}
