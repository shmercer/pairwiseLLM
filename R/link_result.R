# Common results, dispatch, prediction, and append-only continuation.

.link_validate_input <- function(input) {
  .link_check(inherits(input, "pairwiseLLM_link_input") && identical(input$schema_version, 1L),
    "Unsupported linking input schema.")
  .link_fields(input, c("schema_version", "estimator", "hub", "spoke", "phase_a", "cross",
    "judge", "control", "basis", "item_transform", "counts", "hashes", "provenance"),
    c("schema_version", "estimator", "hub", "spoke", "phase_a", "cross", "judge", "control",
      "basis", "item_transform", "counts", "hashes", "provenance"), "link input")
  .link_fields(input$phase_a, c("hub", "spoke"), c("hub", "spoke"), "phase_a")
  for (a in input$phase_a) {
    .link_fields(a, c("kind", "value", "source", "centering"),
      c("kind", "value", "source", "centering"), "normalized Phase A (single-use evidence)")
  }
  backend <- .link_resolve(input$estimator)
  raw <- lapply(input$phase_a, function(x) {
    out <- stats::setNames(list(x$value), backend$kind)
    out$source <- x$source
    out
  })
  checked <- prepare_link_input(input$estimator, input$hub, input$spoke, raw,
    input$cross, input$judge, input$control,
    provenance = list(source_commit = input$provenance$source_commit))
  for (k in c("hub", "spoke")) {
    a <- input$phase_a[[k]]
    b <- checked$phase_a[[k]]
    .link_check(identical(a$kind, b$kind) && identical(a$source, b$source) &&
      isTRUE(all.equal(a$value, b$value, tolerance = 1e-10)), "Phase A payload must be normalized and centered.")
    .link_fields(a$centering, c("method", "removed"), c("method", "removed"), "centering metadata")
    .link_check(identical(a$centering$method, b$centering$method), "Invalid centering method.")
    removed <- a$centering$removed
    .link_check(if (a$kind == "observations") is.null(removed) else
      is.numeric(removed) && all(is.finite(removed)) &&
        length(removed) == if (a$kind == "points") 1L else nrow(a$value), "Invalid recorded centering offsets.")
  }
  for (k in c("hub", "spoke", "cross", "judge", "control", "basis", "item_transform", "counts")) {
    .link_check(identical(input[[k]], checked[[k]]), paste0("Link input ", k, " was modified or is not normalized."))
  }
  hashes <- checked$hashes
  hashes$phase_a_hub <- .link_hash(input$phase_a$hub)
  hashes$phase_a_spoke <- .link_hash(input$phase_a$spoke)
  hashes$input <- NULL
  hashes$input <- .link_hash(list(hashes = hashes, counts = input$counts))
  .link_check(identical(input$hashes, hashes), "Link input evidence/configuration hash mismatch.")
  .link_fields(input$provenance,
    c("package_version", "source_commit", "hash_scheme", "hash_engine_version"),
    c("package_version", "source_commit", "hash_scheme", "hash_engine_version"), "input provenance")
  for (k in c("package_version", "hash_engine_version")) {
    .link_check(is.character(input$provenance[[k]]) && length(input$provenance[[k]]) == 1L &&
      !is.na(input$provenance[[k]]) && nzchar(input$provenance[[k]]), "Invalid version provenance.")
  }
  .link_check(identical(input$provenance$hash_scheme, "link-v1-rlang"), "Unsupported input hash scheme.")
  invisible(TRUE)
}

.link_data_only <- function(x) {
  if (is.null(x)) return(TRUE)
  attributes_ok <- all(vapply(attributes(x), .link_data_only, logical(1)))
  if (is.list(x)) return(attributes_ok && all(vapply(x, .link_data_only, logical(1))))
  attributes_ok && is.atomic(x) && !is.object(x)
}

.link_diagnostics <- function(x, n_parameters) {
  defaults <- list(fit_attempted = FALSE, fit_valid = FALSE, convergence_code = NA_integer_,
    finite_objective = NA, finite_gradient = NA, hessian_pd = NA,
    covariance_valid = NA, covariance_jitter = NA_real_, warning_code = NA_character_,
    failure_code = NA_character_, uncertainty_scope = "unavailable", sampler = list(),
    elapsed_seconds = NA_real_, cpu_seconds = NA_real_, peak_memory_bytes = NA_real_,
    n_parameters = as.integer(n_parameters))
  .link_fields(x, c(names(defaults), "quadrature"), label = "diagnostics")
  if (!is.null(x$quadrature)) {
    .link_check(is.list(x$quadrature) && .link_data_only(x$quadrature),
      "Quadrature diagnostics must be serializable data.")
  }
  out <- utils::modifyList(defaults, x, keep.null = TRUE)
  for (k in c("fit_attempted", "fit_valid", "finite_objective", "finite_gradient", "hessian_pd", "covariance_valid")) {
    .link_check(is.logical(out[[k]]) && length(out[[k]]) == 1L &&
      (!k %in% c("fit_attempted", "fit_valid") || !is.na(out[[k]])), paste0("Invalid diagnostic ", k, "."))
  }
  for (k in c("covariance_jitter", "elapsed_seconds", "cpu_seconds", "peak_memory_bytes")) {
    out[[k]] <- .link_scalar(out[[k]], k, 0, missing = TRUE)
  }
  .link_check(is.integer(out$convergence_code) && length(out$convergence_code) == 1L,
    "convergence_code must be an integer or NA_integer_.")
  .link_check(identical(out$n_parameters, as.integer(n_parameters)), "Diagnostic parameter count mismatch.")
  for (k in c("warning_code", "failure_code", "uncertainty_scope")) {
    .link_check(is.character(out[[k]]) && length(out[[k]]) == 1L &&
      (is.na(out[[k]]) || nzchar(out[[k]])), paste0("Invalid diagnostic ", k, "."))
  }
  .link_check(is.list(out$sampler) && .link_data_only(out$sampler), "Sampler diagnostics must be serializable data.")
  if (out$fit_valid) {
    .link_check(is.na(out$failure_code) && (is.na(out$convergence_code) || out$convergence_code == 0L) &&
      !identical(out$finite_objective, FALSE) &&
      !identical(out$finite_gradient, FALSE) && !identical(out$hessian_pd, FALSE) &&
      !identical(out$covariance_valid, FALSE), "A valid fit cannot hide failure diagnostics.")
  } else {
    .link_check(!is.na(out$failure_code), "Invalid fits require an explicit failure_code.")
  }
  out
}

.link_summary_vector <- function(x, n, label, nonnegative = FALSE) {
  if (is.null(x)) x <- rep(NA_real_, n)
  .link_check(is.numeric(x) && is.null(dim(x)) && length(x) == n &&
    all(is.finite(x) | (is.na(x) & !is.nan(x))) &&
    (!nonnegative || all(is.na(x) | x >= 0)), paste0("Invalid ", label, "."))
  as.double(x)
}

.link_intervals <- function(lower, upper, label) {
  .link_check(identical(is.na(lower), is.na(upper)) && all(is.na(lower) | lower <= upper),
    paste0(label, " interval endpoints must be jointly missing or ordered."))
}

.link_new_result <- function(input, theta_mean, delta, theta_sd = NULL,
                             lower = NULL, upper = NULL, covariance = NULL,
                             prediction = list(), diagnostics = list(), mode = NULL) {
  .link_validate_input(input)
  backend <- .link_resolve(input$estimator)
  identities <- dplyr::bind_rows(
    tibble::add_column(input$hub$items, set_id = input$hub$set_id, .before = 1L),
    tibble::add_column(input$spoke$items, set_id = input$spoke$set_id, .before = 1L))
  n <- nrow(identities)
  identities$theta_link_mean <- .link_summary_vector(theta_mean, n, "theta means")
  identities$theta_link_sd <- .link_summary_vector(theta_sd, n, "theta SDs", TRUE)
  identities$theta_link_lower <- .link_summary_vector(lower, n, "theta lower intervals")
  identities$theta_link_upper <- .link_summary_vector(upper, n, "theta upper intervals")
  identities$rank_link <- as.double(rank(-identities$theta_link_mean, ties.method = "average", na.last = "keep"))
  diagnostics <- .link_diagnostics(diagnostics, ncol(input$item_transform))
  .link_fields(delta, c("mean", "sd", "lower", "upper", "identification"), c("mean", "identification"), "delta")
  offset <- list(delta_mean = .link_scalar(delta$mean, "delta mean", missing = TRUE),
    delta_sd = .link_scalar(delta$sd %||% NA_real_, "delta SD", 0, missing = TRUE),
    delta_lower = .link_scalar(delta$lower %||% NA_real_, "delta lower", missing = TRUE),
    delta_upper = .link_scalar(delta$upper %||% NA_real_, "delta upper", missing = TRUE),
    identification = delta$identification)
  result <- structure(list(schema_version = 1L, estimator_id = input$estimator,
    estimator_version = backend$version, items = identities, offset = offset,
    uncertainty = list(covariance = covariance, basis = input$basis, item_transform = input$item_transform),
    prediction = list(estimator_id = input$estimator, estimator_version = backend$version,
      input_hash = input$hashes$input, state = prediction), diagnostics = diagnostics,
    provenance = c(input$provenance, list(hashes = input$hashes, counts = input$counts,
      phase_a_sources = lapply(input$phase_a, `[[`, "source"), judge_source = input$judge$source)),
    continuation = list(input = input, mode = mode)), class = "pairwiseLLM_link_result")
  .link_validate_result(result)
  result
}

.link_validate_result <- function(result) {
  .link_check(inherits(result, "pairwiseLLM_link_result") && identical(result$schema_version, 1L),
    "Unsupported linking result schema; legacy Phase B state cannot be reinterpreted.")
  input <- result$continuation$input
  .link_validate_input(input)
  backend <- .link_resolve(result$estimator_id)
  .link_check(identical(result$estimator_id, input$estimator) &&
    identical(result$estimator_version, backend$version), "Result estimator identity/version mismatch.")
  .link_check(identical(result$provenance, c(input$provenance,
    list(hashes = input$hashes, counts = input$counts,
      phase_a_sources = lapply(input$phase_a, `[[`, "source"), judge_source = input$judge$source))),
    "Result provenance does not reconcile to input evidence.")
  expected <- dplyr::bind_rows(
    tibble::add_column(input$hub$items, set_id = input$hub$set_id, .before = 1L),
    tibble::add_column(input$spoke$items, set_id = input$spoke$set_id, .before = 1L))
  items <- result$items
  .link_check(is.data.frame(items) && all(names(expected) %in% names(items)) &&
    identical(items[, names(expected)], expected), "Result item ordering/identity mismatch.")
  for (k in c("theta_link_mean", "theta_link_sd", "theta_link_lower", "theta_link_upper")) {
    .link_check(identical(items[[k]], .link_summary_vector(items[[k]], nrow(expected), k, k == "theta_link_sd")),
      "Result summaries must use double vectors and typed missing values.")
  }
  .link_check(identical(items$rank_link, as.double(rank(-items$theta_link_mean, ties.method = "average", na.last = "keep"))),
    "Result ranks do not match item means.")
  .link_intervals(items$theta_link_lower, items$theta_link_upper, "Item")
  d <- result$offset
  .link_fields(d, c("delta_mean", "delta_sd", "delta_lower", "delta_upper", "identification"),
    c("delta_mean", "delta_sd", "delta_lower", "delta_upper", "identification"), "offset")
  for (k in c("delta_mean", "delta_sd", "delta_lower", "delta_upper")) {
    .link_check(identical(d[[k]], .link_scalar(d[[k]], k, if (k == "delta_sd") 0 else -Inf, missing = TRUE)),
      "Offset summaries must use double scalars and typed missing values.")
  }
  .link_check(is.character(d$identification) && length(d$identification) == 1L &&
    d$identification %in% c("prior_only", "cross_set", "unidentified", "failed"), "Invalid offset identification status.")
  .link_intervals(d$delta_lower, d$delta_upper, "Offset")
  diag <- .link_diagnostics(result$diagnostics, ncol(input$item_transform))
  .link_check(identical(diag, result$diagnostics), "Diagnostics must use normalized types.")
  uncertainty <- result$uncertainty
  .link_check(identical(uncertainty$basis, input$basis) &&
    identical(uncertainty$item_transform, input$item_transform), "Result coordinate metadata mismatch.")
  cov <- uncertainty$covariance
  if (!is.null(cov)) {
    .link_check(identical(cov, .link_covariance(cov, colnames(input$item_transform))), "Covariance ordering mismatch.")
    .link_check(isTRUE(diag$covariance_valid), "A returned covariance must be marked valid.")
    item_sd <- sqrt(pmax(0, diag(input$item_transform %*% cov %*% t(input$item_transform))))
    .link_check(isTRUE(all.equal(items$theta_link_sd, item_sd, tolerance = 1e-8)) &&
      isTRUE(all.equal(d$delta_sd, sqrt(cov[1L, 1L]), tolerance = 1e-8)), "SDs must reconcile with full covariance.")
  } else {
    .link_check(!isTRUE(diag$covariance_valid), "Missing covariance cannot be marked valid.")
  }
  .link_check(identical(result$prediction$estimator_id, result$estimator_id) &&
    identical(result$prediction$estimator_version, result$estimator_version) &&
    identical(result$prediction$input_hash, input$hashes$input) &&
    is.list(result$prediction$state) && .link_data_only(result$prediction$state), "Invalid prediction identity or nonserializable state.")
  mode <- result$continuation$mode
  if (!is.null(mode)) {
    .link_check(is.numeric(mode) && is.null(dim(mode)), "Continuation mode must be a numeric vector.")
    .link_check(identical(mode, .link_align_numeric(mode, colnames(input$item_transform), "Continuation mode")),
      "Continuation mode must follow the canonical free-coordinate order.")
  }
  nh <- nrow(input$hub$items)
  h <- seq_len(nh)
  s <- nh + seq_len(nrow(input$spoke$items))
  if (diag$fit_valid) {
    .link_check(all(is.finite(items$theta_link_mean)) && is.finite(d$delta_mean), "Valid fits require finite means.")
    .link_check(input$counts$cross == 0L || diag$fit_attempted, "Positive-budget valid fits must record a fit attempt.")
    .link_check(abs(mean(items$theta_link_mean[h])) < 1e-8 &&
      abs(mean(items$theta_link_mean[s]) - d$delta_mean) < 1e-8, "Means violate the centered-shape offset convention.")
    scope <- if (input$estimator == "fixed_shape_offset") "offset_only_conditional_on_fixed_shapes" else "joint_shapes_and_offset"
    .link_check(identical(diag$uncertainty_scope, scope), "Incorrect estimator uncertainty scope.")
    if (input$estimator == "fixed_shape_offset") {
      .link_check(isTRUE(all.equal(items$theta_link_mean,
        c(unname(input$phase_a$hub$value), unname(input$phase_a$spoke$value) + d$delta_mean), tolerance = 1e-8)),
        "E1 must preserve fixed Phase A shapes.")
      .link_check(all(is.na(items$theta_link_sd[h]) | items$theta_link_sd[h] == 0),
        "E1 hub shapes have zero conditional variance.")
      .link_check(all(is.na(items$theta_link_sd[s]) |
        (!is.na(d$delta_sd) & abs(items$theta_link_sd[s] - d$delta_sd) < 1e-8)),
        "E1 spoke SDs must equal offset SD when defined.")
    }
  }
  if (input$counts$cross == 0L) {
    .link_check(d$identification != "cross_set", "Zero cross edges cannot identify the offset.")
    if (diag$fit_valid) {
      prior <- input$control$delta_prior
      interval <- stats::qnorm(c(.025, .975), prior$mean, prior$sd)
      .link_check(identical(d$identification, "prior_only") &&
        isTRUE(all.equal(c(d$delta_mean, d$delta_sd, d$delta_lower, d$delta_upper),
          c(prior$mean, prior$sd, interval), tolerance = 1e-8)), "Zero-edge offset must equal the configured Normal prior.")
      if (!is.null(cov)) {
        .link_check(all(abs(cov[1L, -1L]) < 1e-8), "Zero-edge offset must be independent of shapes.")
        if (input$estimator != "fixed_shape_offset") {
          hu <- if (nh > 1L) 1L + seq_len(nh - 1L) else integer()
          su <- if (length(s) > 1L) nh + seq_len(length(s) - 1L) else integer()
          .link_check(all(abs(cov[hu, su, drop = FALSE]) < 1e-8), "Zero-edge hub/spoke shapes must be independent.")
        }
      }
      if (input$estimator == "gaussian_posterior_bridge") {
        expected_means <- c(colMeans(input$phase_a$hub$value), colMeans(input$phase_a$spoke$value) + prior$mean)
        .link_check(isTRUE(all.equal(items$theta_link_mean, unname(expected_means), tolerance = 1e-8)),
          "Zero-edge E2 must retain the Phase A bridge means.")
        if (!is.null(cov)) {
          for (k in c("hub", "spoke")) {
            cols <- if (k == "hub") hu else su
            draws <- .link_to_reduced(input$phase_a[[k]]$value, input$basis[[k]])
            expected_cov <- if (ncol(draws)) stats::cov(draws) else matrix(numeric(), 0L, 0L)
            .link_check(isTRUE(all.equal(unname(cov[cols, cols, drop = FALSE]), unname(expected_cov), tolerance = 1e-8)),
              "Zero-edge E2 must retain full Phase A bridge covariance.")
          }
        }
      }
    }
  }
  invisible(TRUE)
}

.link_previous_mode <- function(input, previous) {
  if (is.null(previous)) return(NULL)
  .link_validate_result(previous)
  old <- previous$continuation$input
  for (k in c("estimator", "hub", "spoke", "phase_a", "judge")) {
    .link_check(identical(input[[k]], old[[k]]), paste0("Continuation changed frozen ", k, "."))
  }
  .link_check(identical(input$control$delta_prior, old$control$delta_prior), "Continuation changed the offset prior.")
  n <- nrow(old$cross)
  .link_check(nrow(input$cross) >= n && identical(input$cross[seq_len(n), , drop = FALSE], old$cross),
    "Continuation requires the unchanged old evidence prefix followed by new judgments.")
  if (isTRUE(previous$diagnostics$fit_valid)) previous$continuation$mode else NULL
}

#' Fit a linker using prepared explicit evidence
#'
#' @param input A validated object from [prepare_link_input()].
#' @param previous Optional common linking result for numerical warm starting.
#'   Supply the complete cumulative cross-set evidence in `input`. The old prefix,
#'   identities, Phase A inputs, judge surface, and offset prior must be unchanged.
#'   Only the previous numerical mode is passed to an engine, never its posterior
#'   as a new prior. Invalid previous fits supply no warm start.
#' @return A `pairwiseLLM_link_result` containing common-scale item summaries,
#'   offset summaries, joint uncertainty, prediction data, diagnostics, provenance,
#'   and continuation inputs. Unavailable uncertainty is typed missing, never
#'   implicitly zero. Numerical failures remain invalid method-specific results.
#' @details E1 (`fixed_shape_offset`) uses adaptive one-dimensional quadrature.
#'   E2 and E3 raise `pairwiseLLM_link_not_implemented`; there is no default or
#'   fallback. Result schema version 1 uses `theta_H = H_H u_H` and
#'   `theta_S = delta + H_S u_S`, 95 percent interval endpoints, descending ranks
#'   with average ties, and free coordinates delta, hub shape, then spoke shape.
#'   E1 has only the delta free coordinate. Prediction and continuation state
#'   contain serializable data, with functions resolved by estimator ID/version.
#'
#'   E1 holds separately centered Phase A EAP shapes fixed and updates only
#'   delta under the configured Normal prior and the supplied cross-set
#'   likelihood. Reported means, SDs, and equal-tailed 95 percent intervals come
#'   from adaptive Gauss-Kronrod quadrature, with CDF integration and root
#'   finding for interval endpoints. Both infinite tails are integrated using
#'   rational coordinate maps; there is no finite-domain truncation or Laplace
#'   approximation. Monotone likelihood bounds guard against missed distant mass.
#'   With zero edges, or epsilon equal to one, summaries use the exact Normal
#'   prior. Positive-budget epsilon-one fits report `unidentified`.
#'
#'   E1 uncertainty is conditional on fixed shapes: hub variance is truly zero,
#'   every spoke SD equals delta SD, and all spoke uncertainty is perfectly
#'   correlated. It does not propagate Phase A estimation uncertainty. E1 is
#'   useful with strong Phase A estimates or when computation must be inexpensive.
#'   Numerical initial values are ignored; continuation recomputes the posterior
#'   from the original prior and cumulative evidence, deterministically.
#' @section Result fields:
#' `items` contains `set_id`, `item_id`, `global_item_id`, `theta_link_mean`,
#' `theta_link_sd`, `theta_link_lower`, `theta_link_upper`, and `rank_link`.
#' `offset` contains `delta_mean`, `delta_sd`, `delta_lower`, `delta_upper`, and
#' `identification` (`prior_only`, `cross_set`, `unidentified`, or `failed`).
#' At zero cross edges a valid result retains the configured Normal offset prior
#' and reports `prior_only`. E1 retains fixed shapes; E2 retains independent full
#' Gaussian Phase A bridges; E3 fits only the centered Phase A shape posterior.
#'
#' `uncertainty` contains the named free-coordinate `covariance` (or `NULL`),
#' `basis`, and `item_transform`. For transform T and covariance V, item-scale
#' covariance is `T %*% V %*% t(T)`. E1's item transform has only a delta column;
#' its fixed shapes supply the deterministic part of item means.
#'
#' `diagnostics` records `fit_attempted`, `fit_valid`, `convergence_code`,
#' `finite_objective`, `finite_gradient`, `hessian_pd`, `covariance_valid`,
#' `covariance_jitter`, `warning_code`, `failure_code`, `uncertainty_scope`,
#' `sampler`, `elapsed_seconds`, `cpu_seconds`, `peak_memory_bytes`, and
#' `n_parameters`. Undefined fields use typed missing values. Invalid fits carry
#' a failure code and cannot be used for prediction. E1 adds optional
#' `quadrature` diagnostics: method, domain, coordinate, effective controls,
#' integration partitions, subdivision count, log normalizer, estimated mass/
#' moment errors, CDF error and root brackets when computed, summary method,
#' and status. Partial diagnostics remain available after numerical failure.
#' Quadrature errors are numerical estimates, not posterior SDs. Valid E1 scope is
#' `offset_only_conditional_on_fixed_shapes`; valid E2/E3 scope is
#' `joint_shapes_and_offset`. A true zero conditional variance is allowed.
#'
#' `provenance` retains package/source versions, all input hashes and counts,
#' Phase A sources, and the judge source. `prediction` binds serializable
#' estimator data to estimator ID/version and input hash. `continuation` retains
#' the original normalized `input` and optional free-coordinate numerical `mode`.
#' Results can be round-tripped with [saveRDS()] and [readRDS()].
#' @seealso [prepare_link_input()], [predict_link()]
#' @export
fit_link <- function(input, previous = NULL) {
  .link_validate_input(input)
  mode <- .link_previous_mode(input, previous)
  backend <- .link_resolve(input$estimator)
  if (is.null(backend$fit)) rlang::abort(paste0("Estimator '", input$estimator,
    "' is not implemented yet; its explicit-evidence contract is available."), class = "pairwiseLLM_link_not_implemented")
  result <- backend$fit(input, initial = mode %||% input$control$initial)
  .link_validate_result(result)
  .link_check(identical(result$continuation$input, input), "Estimator returned a result for different evidence.")
  result
}

.link_probability <- function(theta_A, theta_B, judge) {
  judge <- .link_judge(judge)
  .link_check(is.numeric(theta_A) && is.numeric(theta_B) && length(theta_A) == length(theta_B) &&
    all(is.finite(theta_A)) && all(is.finite(theta_B)), "Prediction locations must be finite and equally sized.")
  (1 - judge$epsilon) * stats::plogis(theta_A - theta_B + judge$beta) + judge$epsilon / 2
}

#' Predict oriented hub-spoke comparison probabilities
#'
#' @param result A valid common result returned by [fit_link()].
#' @param pairs Data frame with `observation_id`, `A_set`, `A_item`, `B_set`, and
#'   `B_item`, using the identities in [prepare_link_input()]. No outcomes are
#'   accepted. A and B encode presentation orientation; both directions are legal.
#' @return A double vector of probabilities that presented A wins, aligned with
#'   the input rows. The estimator's prediction hook integrates its uncertainty;
#'   the dispatcher never silently substitutes probabilities at posterior means.
#' @details Conditional probabilities are
#'   `(1-epsilon) * plogis(theta_A-theta_B+beta) + epsilon/2`.
#'   Reversing presentation is complementary only when beta is zero. Prediction
#'   requires no refit or provider calls. E1 integrates over delta using the
#'   serialized posterior quadrature nodes. It refines numerical integration
#'   for a requested pair when necessary, without updating the posterior or
#'   modifying the result. There is no plug-in-mean prediction mode. Integration
#'   failure raises `pairwiseLLM_e1_numerical_error`, rather than substituting
#'   another prediction. E2/E3 engines are added in subsequent issues.
#' @export
predict_link <- function(result, pairs) {
  .link_validate_result(result)
  .link_check(isTRUE(result$diagnostics$fit_valid), "Cannot predict from an invalid linking fit.")
  input <- result$continuation$input
  pairs <- .link_observations(pairs, input$hub, input$spoke, outcomes = FALSE)
  backend <- .link_resolve(result$estimator_id)
  if (is.null(backend$predict)) rlang::abort(paste0("Prediction for estimator '", result$estimator_id,
    "' is not implemented yet."), class = "pairwiseLLM_link_not_implemented")
  p <- backend$predict(result$prediction$state, pairs, input)
  .link_check(is.numeric(p) && is.null(dim(p)) && length(p) == nrow(pairs) &&
    all(is.finite(p)) && all(p >= 0 & p <= 1), "Estimator returned invalid prediction probabilities.")
  as.double(p)
}
