# Resolve predictions only at assessment creation, never while resuming/refitting.
.warm_start_mode <- function(mode, has_predictive) {
  if (is.null(mode)) return(if (has_predictive) "btl_only" else "cold")
  if (!is.character(mode) || length(mode) != 1L || is.na(mode) ||
      !is.null(dim(mode)) || !mode %in% c("cold", "btl_only", "trueskill_only", "both")) {
    rlang::abort("`warm_start_mode` must be NULL or one of cold, btl_only, trueskill_only, both.")
  }
  mode <- unname(mode)
  if (identical(mode, "cold") && has_predictive) {
    rlang::abort("`warm_start_mode = cold` cannot be combined with predictive input.")
  }
  if (!identical(mode, "cold") && !has_predictive) {
    rlang::abort("Non-cold `warm_start_mode` requires `warm_start_model` or `warm_start_prior`.")
  }
  unname(mode)
}

.warm_start_uses_btl <- function(mode) mode %in% c("btl_only", "both")
.warm_start_uses_trueskill <- function(mode) mode %in% c("trueskill_only", "both")

.warm_start_trueskill_policy <- function(policy, mode) {
  if (is.null(policy)) return(NULL)
  if (!is.character(policy) || length(policy) != 1L || is.na(policy) ||
      !is.null(dim(policy)) || policy != "predictive_distribution") {
    rlang::abort("`warm_start_trueskill` must be NULL or predictive_distribution.")
  }
  if (!isTRUE(.warm_start_uses_trueskill(mode))) {
    rlang::abort("`warm_start_trueskill` requires warm_start_mode = trueskill_only or both.")
  }
  unname(policy)
}

# This pure mapping has no graph, outcome, or selection-strategy inputs.
.warm_start_trueskill_distribution <- function(prior, ids) {
  .validate_warm_start_prior(prior, ids)
  defaults <- .trueskill_defaults()
  index <- match(ids, prior$item_id)
  mu <- defaults$mu0 + defaults$sigma0 * prior$prior_mean[index]
  sigma <- defaults$sigma0 * prior$prior_sd[index]
  .warm_start_prior_numeric(mu, length(ids), "mapped TrueSkill means")
  .warm_start_prior_numeric(sigma, length(ids), "mapped TrueSkill SDs", positive = TRUE)
  list(item_id = as.character(ids), mu = mu, sigma = sigma, beta = defaults$beta)
}

.warm_start_trueskill_mapping <- function(prior, mode, sd_source) {
  distribution <- .warm_start_trueskill_distribution(prior, prior$item_id)
  defaults <- .trueskill_defaults()
  mapping <- list(format_version = 1L, policy = "predictive_distribution",
    warm_start_mode = mode, mu_offset = defaults$mu0, scale = defaults$sigma0,
    beta = defaults$beta, clipping = "none", sd_source = sd_source,
    sd_rule = prior$provenance$sd_rule, calibration = "upstream_not_verified",
    predictive_prior_digest = prior$digest, item_id = prior$item_id,
    distribution_digest = .warm_start_prior_hash(distribution))
  mapping$digest <- .warm_start_prior_hash(mapping)
  mapping
}

.warm_start_trueskill_validate <- function(state, metadata = NULL) {
  policy <- state$meta$warm_start_trueskill
  mapping <- state$meta$trueskill_mapping
  if (!is.null(metadata) &&
      (!identical(metadata$warm_start_trueskill, policy) ||
       !identical(metadata$trueskill_mapping, mapping))) {
    rlang::abort("Session metadata TrueSkill mapping integrity mismatch.")
  }
  if (is.null(policy) && is.null(mapping)) return(invisible(NULL))
  mode <- state$meta$warm_start_mode
  .warm_start_trueskill_policy(policy, mode)
  if (is.null(policy) || !is.list(mapping) ||
      !is.character(mapping$sd_source) || length(mapping$sd_source) != 1L ||
      !mapping$sd_source %in% c("model_argument", "prior_object") ||
      !identical(state$meta$trueskill_initialized_from_predictive, TRUE)) {
    rlang::abort("Missing or invalid predictive TrueSkill mapping metadata.")
  }
  defaults <- .trueskill_defaults()
  legacy_mapping <- list(trueskill_warm_scale = 1.0,
    trueskill_mu0_used = defaults$mu0, trueskill_sigma0_used = defaults$sigma0)
  if (!identical(state$meta[names(legacy_mapping)], legacy_mapping)) {
    rlang::abort("Predictive TrueSkill mapping conflicts with initialization metadata.")
  }
  prior <- state$predictive_prior
  .validate_warm_start_prior(prior, state$item_ids)
  if (!identical(prior$item_id, state$item_ids) ||
      !identical(mapping, .warm_start_trueskill_mapping(prior, mode, mapping$sd_source))) {
    rlang::abort("Predictive TrueSkill mapping integrity mismatch.")
  }
  ts <- validate_trueskill_state(state$trueskill_state)
  .validate_warm_start_prior(prior, ts$items$item_id)
  if (!identical(ts$beta, mapping$beta)) {
    rlang::abort("Predictive TrueSkill mapping requires beta = 25/6.")
  }
  # Check untouched initialization, but never compare evolved ratings to a prior.
  if (nrow(state$history_pairs) == 0L) {
    initial <- .warm_start_trueskill_distribution(prior, ts$items$item_id)
    if (!identical(ts$items$mu, initial$mu) || !identical(ts$items$sigma, initial$sigma)) {
      rlang::abort("Initial TrueSkill state disagrees with the saved predictive distribution.")
    }
  }
  invisible(NULL)
}

.warm_start_btl_prior_for_state <- function(state) {
  # Legacy states retain historical BTL-only semantics; never reinitialize TrueSkill.
  mode <- .warm_start_mode(state$meta$warm_start_mode, !is.null(state$predictive_prior))
  if (.warm_start_uses_btl(mode)) state$predictive_prior else NULL
}

.warm_start_adaptive_init <- function(state, model = NULL, prior = NULL, features = NULL,
                                      python = NULL, prior_sd = NULL, mode = NULL,
                                      trueskill = NULL) {
  if (!is.null(model) && !is.null(prior)) {
    rlang::abort("Supply only one of `warm_start_model` and `warm_start_prior`.")
  }
  mode <- .warm_start_mode(mode, !is.null(model) || !is.null(prior))
  trueskill <- .warm_start_trueskill_policy(trueskill, mode)
  if (!is.null(state$meta$warm_start_trueskill) || !is.null(state$meta$trueskill_mapping) ||
      (!is.null(trueskill) && (!is.null(state$meta$warm_start_mode) || nrow(state$history_pairs) > 0L))) {
    rlang::abort("Cannot reinitialize predictive TrueSkill; resume must use the saved state.")
  }
  if (identical(mode, "trueskill_only") && !is.null(prior_sd) && is.null(trueskill)) {
    rlang::abort("`warm_start_prior_sd` controls BTL priors and cannot be used with trueskill_only.")
  }
  if (is.null(model) && any(!vapply(list(features, python, prior_sd), is.null, logical(1)))) {
    rlang::abort("Warm-start features, Python, and prior SD arguments require `warm_start_model`.")
  }
  if (!is.null(trueskill) && !is.null(model)) {
    if (is.null(prior_sd)) {
      rlang::abort("Predictive TrueSkill distribution with model input requires explicit `warm_start_prior_sd`.")
    }
    # Validate before model loading/extraction; final alignment follows prediction IDs.
    .warm_start_prior_numeric(prior_sd, length(prior_sd), "SDs", positive = TRUE)
    .warm_start_prior_sd(prior_sd, state$item_ids)
  }
  if (!is.null(model)) {
    resolved <- .warm_start_prior_resolve_model(model)
    if (is.null(features)) {
      if (!"text" %in% names(state$items)) rlang::abort("Warm-start model input requires item texts or features.")
      features <- extract_warm_start_features(texts = state$items$text, ids = state$item_ids,
        schema = resolved$model$schema, python = python)
    } else if (!is.null(python)) {
      rlang::abort("`warm_start_python` applies only to text extraction.")
    }
    predictions <- stats::predict(resolved$model, features)
    prior <- make_warm_start_prior(predictions, ids = state$item_ids, prior_sd = prior_sd %||% 0.5)
    prior$provenance$artifact <- resolved$identity
    prior$digest <- NULL
    prior$digest <- .warm_start_prior_hash(prior)
  }
  prior <- .warm_start_prior_scope(prior, state$item_ids, exact = TRUE)
  state$predictive_prior <- prior
  state$meta$predictive_prior_digest <- if (is.null(prior)) NULL else prior$digest
  state$meta$warm_start_mode <- mode
  state$meta$trueskill_initialized_from_predictive <- .warm_start_uses_trueskill(mode)
  if (.warm_start_uses_trueskill(mode)) {
    ts <- validate_trueskill_state(state$trueskill_state)
    .validate_warm_start_prior(prior, ts$items$item_id)
    defaults <- .trueskill_defaults()
    ts$items$mu <- defaults$mu0 + defaults$sigma0 *
      prior$prior_mean[match(ts$items$item_id, prior$item_id)]
    if (!is.null(trueskill)) {
      distribution <- .warm_start_trueskill_distribution(prior, ts$items$item_id)
      ts$items$mu <- distribution$mu
      ts$items$sigma <- distribution$sigma
      state$meta$warm_start_trueskill <- trueskill
      state$meta$trueskill_mapping <- .warm_start_trueskill_mapping(prior, mode,
        if (is.null(model)) "prior_object" else "model_argument")
    }
    state$trueskill_state <- validate_trueskill_state(ts)
    state$meta$trueskill_warm_scale <- 1.0
    state$meta$trueskill_mu0_used <- defaults$mu0
    state$meta$trueskill_sigma0_used <- defaults$sigma0
  }
  .warm_start_trueskill_validate(state)
  state
}

.warm_start_prior_resolve_model <- function(model) {
  reference <- NULL
  path <- NULL
  source <- "object"
  if (is.character(model)) model <- list(path = model)
  if (is.list(model) && !is.object(model)) {
    reference <- model
    if (is.null(names(reference)) || anyDuplicated(names(reference)) ||
        !all(names(reference) %in% c("path", "name", "source"))) {
      rlang::abort("Invalid model reference; supply path or name and optional source.")
    }
    model <- do.call(load_warm_start_model, reference)
    if (!is.null(reference$path)) {
      path <- .warm_start_file_path(reference$path)
      source <- "path"
    } else {
      path <- .warm_start_resolve_name(reference$name, reference$source %||% "auto")
      source <- if (.warm_start_within(path, .warm_start_registry_root("bundled"))) "bundled" else "user"
      reference$name <- .warm_start_model_name(reference$name)
      reference$source <- source
    }
  }
  .validate_warm_start_artifact(model)
  identity <- list(reference = reference, source = source,
    name = model$metadata$name %||% NULL, version = model$metadata$version %||% NULL,
    format_version = model$format_version, package_version = as.character(utils::packageVersion("pairwiseLLM")),
    artifact_digest = .warm_start_prior_hash(model),
    file_md5 = if (is.null(path)) NULL else unname(tools::md5sum(path)))
  if (identical(source, "bundled")) {
    entry <- .warm_start_bundle_manifest(.warm_start_registry_root("bundled"))[[reference$name]]
    identity$manifest <- list(version = 1L, checksum = entry$checksum, built_at = entry$built_at)
  }
  list(model = model, identity = identity)
}

.warm_start_adaptive_validate <- function(state) {
  prior <- state$predictive_prior %||% NULL
  digest <- state$meta$predictive_prior_digest %||% NULL
  if (!is.null(prior)) .validate_warm_start_prior(prior, state$item_ids)
  if (!identical(digest, if (is.null(prior)) NULL else prior$digest)) {
    rlang::abort("Adaptive predictive prior integrity mismatch; saved predictions must remain authoritative.")
  }
  .warm_start_trueskill_validate(state)
  invisible(state)
}

.warm_start_resume_inputs <- function(...) {
  if (any(!vapply(list(...), is.null, logical(1)))) {
    rlang::abort(paste0("Resume uses saved predictive priors. Omit all warm-start model, prior, ",
      "feature, Python, SD, and mode arguments; start a new session to change them."))
  }
  invisible(NULL)
}

.warm_start_prior_fit_metadata <- function(prior) {
  if (is.null(prior)) return(NULL)
  list(digest = prior$digest, item_id = prior$item_id, prior_mean = prior$prior_mean,
    prior_sd = prior$prior_sd, provenance = prior$provenance)
}

.warm_start_phase_a_identity <- function(state, set_id) {
  prior <- state$predictive_prior %||% NULL
  if (is.null(prior)) return(NULL)
  ids <- as.character(state$items$item_id[state$items$set_id == set_id])
  .warm_start_prior_scope(prior, ids)$digest
}
