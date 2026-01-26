# -------------------------------------------------------------------------
# Adaptive v3 fit contracts.
# -------------------------------------------------------------------------

#' Build the v3 adaptive fit contract
#'
#' @description
#' Construct the canonical adaptive v3 fit contract that sits between MCMC
#' inference outputs and downstream adaptive consumers. The contract includes
#' posterior draws, summaries, diagnostics, and MCMC metadata in a single
#' validated structure.
#'
#' @param theta_draws Numeric matrix of posterior draws
#'   \eqn{[n\_draws x n\_items]} with column names matching item ids.
#' @param epsilon_draws Optional numeric vector of epsilon draws or \code{NULL}
#'   when epsilon is not applicable.
#' @param beta_draws Optional numeric vector of beta draws or \code{NULL} when
#'   the beta parameter is not applicable.
#' @param diagnostics List with diagnostic summaries (divergences, max_rhat,
#'   min_ess_bulk).
#' @param model_variant Optional model variant label.
#' @param mcmc_config_used List with the MCMC configuration snapshot.
#' @param diagnostics_pass Optional logical scalar indicating diagnostics gate
#'   status, or \code{NA}.
#'
#' @return A validated list containing draws, summaries, diagnostics, and
#'   metadata for adaptive v3 consumers.
#'
#' @keywords internal
#' @noRd
build_v3_fit_contract <- function(theta_draws,
    epsilon_draws = NULL,
    beta_draws = NULL,
    diagnostics = NULL,
    model_variant = NA_character_,
    mcmc_config_used = NULL,
    diagnostics_pass = NA) {
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")
  ids <- colnames(theta_draws)
  if (is.null(ids) || anyNA(ids) || any(ids == "")) {
    rlang::abort("`theta_draws` must have non-empty column names.")
  }

  probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
  theta_mean <- stats::setNames(as.double(colMeans(theta_draws)), ids)
  theta_sd <- stats::setNames(as.double(apply(theta_draws, 2, stats::sd)), ids)

  epsilon_summary <- .v3_contract_vector_summary(
    draws = epsilon_draws,
    name = "epsilon_draws",
    bounds = c(0, 1),
    probs = probs
  )
  beta_summary <- .v3_contract_vector_summary(
    draws = beta_draws,
    name = "beta_draws",
    bounds = NULL,
    probs = probs
  )

  diagnostics <- .v3_contract_diagnostics_defaults(diagnostics)
  diagnostics_pass <- .v3_contract_diagnostics_pass(diagnostics_pass)
  mcmc_config_used <- .v3_contract_mcmc_defaults(mcmc_config_used)

  model_variant <- if (is.na(model_variant %||% NA_character_)) {
    NA_character_
  } else {
    normalize_model_variant(model_variant)
  }

  fit <- list(
    theta_draws = theta_draws,
    epsilon_draws = epsilon_summary$draws,
    beta_draws = beta_summary$draws,
    theta_mean = theta_mean,
    theta_sd = theta_sd,
    epsilon_mean = epsilon_summary$mean,
    epsilon_p2.5 = epsilon_summary$p2.5,
    epsilon_p5 = epsilon_summary$p5,
    epsilon_p50 = epsilon_summary$p50,
    epsilon_p95 = epsilon_summary$p95,
    epsilon_p97.5 = epsilon_summary$p97.5,
    beta_mean = beta_summary$mean,
    beta_p2.5 = beta_summary$p2.5,
    beta_p5 = beta_summary$p5,
    beta_p50 = beta_summary$p50,
    beta_p95 = beta_summary$p95,
    beta_p97.5 = beta_summary$p97.5,
    diagnostics = diagnostics,
    diagnostics_pass = diagnostics_pass,
    n_items = as.integer(ncol(theta_draws)),
    n_draws = as.integer(nrow(theta_draws)),
    model_variant = as.character(model_variant),
    mcmc_config_used = mcmc_config_used
  )
  validate_v3_fit_contract(fit, ids = ids)
  fit
}

percentiles_2_5_50_95_97_5 <- function(x) {
  probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
  qs <- stats::quantile(x, probs = probs, names = FALSE)
  stats::setNames(as.double(qs), c("p2.5", "p5", "p50", "p95", "p97.5"))
}

na_param_summary <- function() {
  c(
    mean = NA_real_,
    p2.5 = NA_real_,
    p5 = NA_real_,
    p50 = NA_real_,
    p95 = NA_real_,
    p97.5 = NA_real_
  )
}

.v3_contract_vector_summary <- function(draws, name, bounds, probs) {
  if (is.null(draws)) {
    summary <- na_param_summary()
    return(list(
      draws = NULL,
      mean = summary[["mean"]],
      p2.5 = summary[["p2.5"]],
      p5 = summary[["p5"]],
      p50 = summary[["p50"]],
      p95 = summary[["p95"]],
      p97.5 = summary[["p97.5"]]
    ))
  }
  if (!is.numeric(draws)) {
    rlang::abort(paste0("`", name, "` must be numeric or NULL."))
  }
  draws <- as.double(draws)
  if (any(!is.finite(draws))) {
    rlang::warn(paste0("Non-finite values detected in `", name, "`; dropping before summarising."))
    draws <- draws[is.finite(draws)]
  }
  if (length(draws) < 2L) {
    rlang::abort(paste0("`", name, "` must contain at least two finite values."))
  }
  if (!is.null(bounds)) {
    if (any(draws < bounds[[1L]] | draws > bounds[[2L]])) {
      rlang::abort(paste0("`", name, "` must be within [", bounds[[1L]], ", ", bounds[[2L]], "]."))
    }
  }

  qs <- percentiles_2_5_50_95_97_5(draws)
  list(
    draws = draws,
    mean = as.double(mean(draws)),
    p2.5 = as.double(qs[["p2.5"]]),
    p5 = as.double(qs[["p5"]]),
    p50 = as.double(qs[["p50"]]),
    p95 = as.double(qs[["p95"]]),
    p97.5 = as.double(qs[["p97.5"]])
  )
}

.v3_contract_diagnostics_defaults <- function(diagnostics) {
  if (is.null(diagnostics)) {
    diagnostics <- list()
  }
  if (!is.list(diagnostics)) {
    rlang::abort("`diagnostics` must be a list.")
  }
  diagnostics$divergences <- as.integer(diagnostics$divergences %||% NA_integer_)
  diagnostics$max_rhat <- as.double(diagnostics$max_rhat %||% NA_real_)
  diagnostics$min_ess_bulk <- as.double(diagnostics$min_ess_bulk %||% NA_real_)
  diagnostics
}

.v3_contract_diagnostics_pass <- function(diagnostics_pass) {
  if (is.null(diagnostics_pass)) {
    return(NA)
  }
  if (!is.logical(diagnostics_pass) || length(diagnostics_pass) != 1L) {
    rlang::abort("`diagnostics_pass` must be TRUE, FALSE, or NA.")
  }
  diagnostics_pass
}

.v3_contract_mcmc_defaults <- function(mcmc_config_used) {
  mcmc_config_used <- mcmc_config_used %||% list()
  if (!is.list(mcmc_config_used)) {
    rlang::abort("`mcmc_config_used` must be a list.")
  }
  list(
    chains = as.integer(mcmc_config_used$chains %||% NA_integer_),
    parallel_chains = as.integer(mcmc_config_used$parallel_chains %||% NA_integer_),
    core_fraction = as.double(mcmc_config_used$core_fraction %||% NA_real_),
    cores_detected_physical = as.integer(mcmc_config_used$cores_detected_physical %||% NA_integer_),
    cores_detected_logical = as.integer(mcmc_config_used$cores_detected_logical %||% NA_integer_),
    threads_per_chain = as.integer(mcmc_config_used$threads_per_chain %||% NA_integer_),
    cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_)
  )
}

#' @keywords internal
#' @noRd
reorder_theta_draws <- function(theta_draws, ids) {
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`theta_draws` must be a numeric matrix.")
  }
  ids <- as.character(ids)
  if (length(ids) < 1L || anyNA(ids) || any(ids == "")) {
    rlang::abort("`ids` must be a non-empty character vector.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.")
  }
  cols <- colnames(theta_draws)
  if (is.null(cols) || anyNA(cols) || any(cols == "")) {
    rlang::abort("`theta_draws` must have non-empty column names.")
  }
  if (!setequal(cols, ids) || length(cols) != length(ids)) {
    rlang::abort("`theta_draws` column names must match `ids`.")
  }
  theta_draws[, ids, drop = FALSE]
}

#' @keywords internal
#' @noRd
validate_v3_fit_contract <- function(fit, ids, where = rlang::caller_env()) {
  if (!is.list(fit)) {
    rlang::abort("`fit` must be a list.", call = where)
  }
  ids <- as.character(ids)
  if (length(ids) < 1L || anyNA(ids) || any(ids == "")) {
    rlang::abort("`ids` must be a non-empty character vector.", call = where)
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.", call = where)
  }

  required <- c(
    "theta_draws", "theta_mean", "theta_sd",
    "epsilon_draws", "beta_draws",
    "epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", "epsilon_p95", "epsilon_p97.5",
    "beta_mean", "beta_p2.5", "beta_p5", "beta_p50", "beta_p95", "beta_p97.5",
    "diagnostics", "diagnostics_pass",
    "n_items", "n_draws", "model_variant", "mcmc_config_used"
  )
  missing <- setdiff(required, names(fit))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0("`fit` is missing required fields: ", paste(missing, collapse = ", "), "."),
      call = where
    )
  }

  theta_draws <- fit$theta_draws %||% NULL
  if (is.null(theta_draws) || !is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix.", call = where)
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`fit$theta_draws` must have at least two draws.", call = where)
  }
  if (ncol(theta_draws) != length(ids)) {
    rlang::abort("`fit$theta_draws` must have one column per `id`.", call = where)
  }
  draws_ids <- colnames(theta_draws)
  if (is.null(draws_ids) || anyNA(draws_ids) || any(draws_ids == "")) {
    rlang::abort("`fit$theta_draws` must have non-empty column names.", call = where)
  }
  if (!identical(draws_ids, ids)) {
    rlang::abort("`fit$theta_draws` column names must match `ids` in order.", call = where)
  }

  theta_mean <- fit$theta_mean %||% NULL
  if (is.null(theta_mean) || !is.numeric(theta_mean)) {
    rlang::abort("`fit$theta_mean` must be a numeric vector.", call = where)
  }
  if (length(theta_mean) != length(ids)) {
    rlang::abort("`fit$theta_mean` must be length `ids`.", call = where)
  }
  if (is.null(names(theta_mean))) {
    rlang::abort("`fit$theta_mean` must be named.", call = where)
  }
  if (!identical(names(theta_mean), ids)) {
    rlang::abort("`fit$theta_mean` names must match `ids`.", call = where)
  }
  if (any(!is.finite(theta_mean))) {
    rlang::abort("`fit$theta_mean` must be finite.", call = where)
  }

  theta_sd <- fit$theta_sd %||% NULL
  if (is.null(theta_sd) || !is.numeric(theta_sd)) {
    rlang::abort("`fit$theta_sd` must be a numeric vector.", call = where)
  }
  if (length(theta_sd) != length(ids)) {
    rlang::abort("`fit$theta_sd` must be length `ids`.", call = where)
  }
  if (is.null(names(theta_sd))) {
    rlang::abort("`fit$theta_sd` must be named.", call = where)
  }
  if (!identical(names(theta_sd), ids)) {
    rlang::abort("`fit$theta_sd` names must match `ids`.", call = where)
  }
  if (any(!is.finite(theta_sd))) {
    rlang::abort("`fit$theta_sd` must be finite.", call = where)
  }

  n_items <- fit$n_items
  n_draws <- fit$n_draws
  if (!.adaptive_v3_intish(n_items) || !.adaptive_v3_intish(n_draws)) {
    rlang::abort("`fit$n_items` and `fit$n_draws` must be integer scalars.", call = where)
  }
  if (as.integer(n_items) != ncol(theta_draws)) {
    rlang::abort("`fit$n_items` must match `theta_draws` columns.", call = where)
  }
  if (as.integer(n_draws) != nrow(theta_draws)) {
    rlang::abort("`fit$n_draws` must match `theta_draws` rows.", call = where)
  }

  .v3_contract_validate_scalar <- function(value, name, allow_na = FALSE) {
    if (!is.numeric(value) || length(value) != 1L) {
      rlang::abort(paste0("`fit$", name, "` must be a numeric scalar."), call = where)
    }
    if (!allow_na && !is.finite(value)) {
      rlang::abort(paste0("`fit$", name, "` must be finite."), call = where)
    }
    if (allow_na && !(is.finite(value) || is.na(value))) {
      rlang::abort(paste0("`fit$", name, "` must be finite or NA."), call = where)
    }
  }

  epsilon_draws <- fit$epsilon_draws %||% NULL
  epsilon_fields <- c("epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", "epsilon_p95", "epsilon_p97.5")
  if (is.null(epsilon_draws)) {
    for (nm in epsilon_fields) {
      .v3_contract_validate_scalar(fit[[nm]], nm, allow_na = TRUE)
    }
  } else {
    if (!is.numeric(epsilon_draws)) {
      rlang::abort("`fit$epsilon_draws` must be a numeric vector or NULL.", call = where)
    }
    epsilon_draws <- as.double(epsilon_draws)
    if (length(epsilon_draws) < 2L || any(!is.finite(epsilon_draws))) {
      rlang::abort("`fit$epsilon_draws` must contain at least two finite draws.", call = where)
    }
    if (any(epsilon_draws < 0 | epsilon_draws > 1)) {
      rlang::abort("`fit$epsilon_draws` must be within [0, 1].", call = where)
    }
    for (nm in epsilon_fields) {
      .v3_contract_validate_scalar(fit[[nm]], nm, allow_na = FALSE)
    }
  }

  beta_draws <- fit$beta_draws %||% NULL
  beta_fields <- c("beta_mean", "beta_p2.5", "beta_p5", "beta_p50", "beta_p95", "beta_p97.5")
  if (is.null(beta_draws)) {
    for (nm in beta_fields) {
      .v3_contract_validate_scalar(fit[[nm]], nm, allow_na = TRUE)
    }
  } else {
    if (!is.numeric(beta_draws)) {
      rlang::abort("`fit$beta_draws` must be a numeric vector or NULL.", call = where)
    }
    beta_draws <- as.double(beta_draws)
    if (length(beta_draws) < 2L || any(!is.finite(beta_draws))) {
      rlang::abort("`fit$beta_draws` must contain at least two finite draws.", call = where)
    }
    for (nm in beta_fields) {
      .v3_contract_validate_scalar(fit[[nm]], nm, allow_na = FALSE)
    }
  }

  diagnostics <- fit$diagnostics %||% NULL
  if (is.null(diagnostics) || !is.list(diagnostics)) {
    rlang::abort("`fit$diagnostics` must be a list.", call = where)
  }
  if (!all(c("divergences", "max_rhat", "min_ess_bulk") %in% names(diagnostics))) {
    rlang::abort("`fit$diagnostics` must include divergences, max_rhat, and min_ess_bulk.", call = where)
  }
  if (!is.logical(fit$diagnostics_pass) || length(fit$diagnostics_pass) != 1L) {
    rlang::abort("`fit$diagnostics_pass` must be TRUE, FALSE, or NA.", call = where)
  }

  model_variant <- fit$model_variant
  if (!is.character(model_variant) || length(model_variant) != 1L) {
    rlang::abort("`fit$model_variant` must be a length-1 character value.", call = where)
  }

  mcmc_config_used <- fit$mcmc_config_used %||% NULL
  if (is.null(mcmc_config_used) || !is.list(mcmc_config_used)) {
    rlang::abort("`fit$mcmc_config_used` must be a list.", call = where)
  }
  required_mcmc <- c(
    "chains", "parallel_chains", "core_fraction",
    "cores_detected_physical", "cores_detected_logical",
    "threads_per_chain", "cmdstanr_version"
  )
  missing_mcmc <- setdiff(required_mcmc, names(mcmc_config_used))
  if (length(missing_mcmc) > 0L) {
    rlang::abort(
      paste0("`fit$mcmc_config_used` is missing: ", paste(missing_mcmc, collapse = ", "), "."),
      call = where
    )
  }

  invisible(TRUE)
}
