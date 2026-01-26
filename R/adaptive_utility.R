# -------------------------------------------------------------------------
# Adaptive utility computation helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.adaptive_epsilon_mean_from_state <- function(state, fit = NULL) {
  if (inherits(state, "adaptive_state") && is.list(fit)) {
    validate_v3_fit_contract(fit, ids = state$ids)
  }
  prior_alpha <- NULL
  prior_beta <- NULL
  if (!is.null(state) && is.list(state$config) && is.list(state$config$v3)) {
    prior_alpha <- state$config$v3$epsilon_prior_alpha %||% NULL
    prior_beta <- state$config$v3$epsilon_prior_beta %||% NULL
  }
  if (is.null(prior_alpha) || is.null(prior_beta)) {
    prior_alpha <- 2
    prior_beta <- 20
  }
  if (!is.numeric(prior_alpha) || length(prior_alpha) != 1L || !is.finite(prior_alpha)) {
    rlang::abort("`epsilon_prior_alpha` must be a finite numeric scalar.")
  }
  if (!is.numeric(prior_beta) || length(prior_beta) != 1L || !is.finite(prior_beta)) {
    rlang::abort("`epsilon_prior_beta` must be a finite numeric scalar.")
  }
  if (prior_alpha <= 0 || prior_beta <= 0) {
    rlang::abort("`epsilon_prior_alpha` and `epsilon_prior_beta` must be positive.")
  }

  eps <- NULL
  if (is.list(fit)) {
    eps <- fit$epsilon_mean %||% NULL
  }
  if (is.null(eps) && !is.null(state) && is.list(state$posterior)) {
    eps <- state$posterior$epsilon_mean %||% NULL
  }
  if (is.null(eps) && !is.null(state) && is.list(state$config) && is.list(state$config$v3)) {
    eps <- state$config$v3$epsilon_mean %||% NULL
  }
  if (is.null(eps)) {
    rlang::warn("Using epsilon prior mean fallback; no posterior epsilon available.")
    eps <- prior_alpha / (prior_alpha + prior_beta)
  }
  if (!is.numeric(eps) || length(eps) != 1L || !is.finite(eps)) {
    rlang::abort("`epsilon_mean` must be a finite numeric scalar.")
  }
  if (eps < 0 || eps > 1) {
    rlang::abort("`epsilon_mean` must be in [0, 1].")
  }
  as.double(eps)
}

#' @keywords internal
#' @noRd
compute_pair_stats_from_draws <- function(draws, candidates) {
  if (!is.matrix(draws) || !is.numeric(draws)) {
    rlang::abort("`draws` must be a numeric matrix.")
  }
  if (nrow(draws) < 2L) {
    rlang::abort("`draws` must have at least two draws.")
  }
  if (any(!is.finite(draws))) {
    rlang::abort("`draws` must contain only finite values.")
  }
  ids <- colnames(draws)
  if (is.null(ids) || any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`draws` must have non-empty column names.")
  }

  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  candidates <- tibble::as_tibble(candidates)

  id_cols <- NULL
  if (all(c("i_id", "j_id") %in% names(candidates))) {
    id_cols <- c("i_id", "j_id")
  } else if (all(c("i", "j") %in% names(candidates))) {
    id_cols <- c("i", "j")
  }
  if (is.null(id_cols)) {
    rlang::abort("`candidates` must include `i_id`/`j_id` or `i`/`j` columns.")
  }

  if (nrow(candidates) == 0L) {
    return(tibble::tibble(
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double()
    ))
  }

  i_id <- as.character(candidates[[id_cols[[1L]]]])
  j_id <- as.character(candidates[[id_cols[[2L]]]])
  if (anyNA(i_id) || anyNA(j_id) || any(i_id == "") || any(j_id == "")) {
    rlang::abort("`candidates` ids must be non-missing.")
  }

  missing_ids <- setdiff(unique(c(i_id, j_id)), ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates` ids must be present in `draws`.")
  }

  i_idx <- match(i_id, ids)
  j_idx <- match(j_id, ids)

  diffs <- draws[, i_idx, drop = FALSE] - draws[, j_idx, drop = FALSE]
  mean_d <- as.double(colMeans(diffs))
  var_d <- as.double(apply(diffs, 2, stats::var))
  p_mean <- as.double(colMeans(stats::plogis(diffs)))

  if (any(!is.finite(mean_d)) || any(!is.finite(var_d)) || any(!is.finite(p_mean))) {
    rlang::abort("Pair statistics must be finite.")
  }

  tibble::tibble(
    i = i_id,
    j = j_id,
    mean_d = mean_d,
    var_d = var_d,
    p_mean = p_mean
  )
}

#' @keywords internal
#' @noRd
compute_pair_utility_from_draws <- function(theta_draws,
                                            candidate_i,
                                            candidate_j,
                                            deg,
                                            model_variant,
                                            epsilon_draws = NULL,
                                            beta_draws = NULL,
                                            chunk_size = 2000L) {
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`theta_draws` must have at least two draws.")
  }
  if (any(!is.finite(theta_draws))) {
    rlang::abort("`theta_draws` must contain only finite values.")
  }

  if (!is.numeric(candidate_i) || !is.numeric(candidate_j)) {
    rlang::abort("`candidate_i` and `candidate_j` must be numeric.")
  }
  if (length(candidate_i) != length(candidate_j)) {
    rlang::abort("`candidate_i` and `candidate_j` must be the same length.")
  }
  if (length(candidate_i) == 0L) {
    return(tibble::tibble(
      i = integer(),
      j = integer(),
      u0 = double(),
      u = double(),
      p_mean = double(),
      mean_d = double(),
      var_d = double()
    ))
  }
  if (any(!is.finite(candidate_i)) || any(!is.finite(candidate_j))) {
    rlang::abort("`candidate_i` and `candidate_j` must be finite.")
  }
  if (any(abs(candidate_i - round(candidate_i)) > 1e-8) ||
    any(abs(candidate_j - round(candidate_j)) > 1e-8)) {
    rlang::abort("`candidate_i` and `candidate_j` must be integer-like.")
  }

  candidate_i <- as.integer(round(candidate_i))
  candidate_j <- as.integer(round(candidate_j))

  n_items <- ncol(theta_draws)
  if (any(candidate_i < 1L | candidate_i > n_items) ||
    any(candidate_j < 1L | candidate_j > n_items)) {
    rlang::abort("`candidate_i` and `candidate_j` must index `theta_draws` columns.")
  }
  if (any(candidate_i == candidate_j)) {
    rlang::abort("`candidate_i` and `candidate_j` must be different.")
  }

  if (!is.numeric(deg) || length(deg) != n_items) {
    rlang::abort("`deg` must be a numeric vector with length matching `theta_draws` columns.")
  }
  deg <- as.double(deg)
  if (any(!is.finite(deg))) {
    rlang::abort("`deg` must contain only finite values.")
  }
  if (any(deg < 0)) {
    rlang::abort("`deg` must be non-negative.")
  }

  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L) {
    rlang::abort("`chunk_size` must be a positive integer.")
  }

  model_variant <- normalize_model_variant(model_variant)

  if (model_has_e(model_variant)) {
    if (is.null(epsilon_draws)) {
      rlang::abort("`epsilon_draws` must be supplied for epsilon model variants.")
    }
    if (!is.numeric(epsilon_draws)) {
      rlang::abort("`epsilon_draws` must be numeric.")
    }
    epsilon_draws <- as.double(epsilon_draws)
    if (length(epsilon_draws) != nrow(theta_draws)) {
      rlang::abort("`epsilon_draws` must have the same length as `theta_draws` rows.")
    }
    if (any(!is.finite(epsilon_draws)) || any(epsilon_draws < 0 | epsilon_draws > 1)) {
      rlang::abort("`epsilon_draws` must be finite and within [0, 1].")
    }
  } else {
    epsilon_draws <- NULL
  }

  if (model_has_b(model_variant)) {
    if (is.null(beta_draws)) {
      rlang::abort("`beta_draws` must be supplied for beta model variants.")
    }
    if (!is.numeric(beta_draws)) {
      rlang::abort("`beta_draws` must be numeric.")
    }
    beta_draws <- as.double(beta_draws)
    if (length(beta_draws) != nrow(theta_draws)) {
      rlang::abort("`beta_draws` must have the same length as `theta_draws` rows.")
    }
    if (any(!is.finite(beta_draws))) {
      rlang::abort("`beta_draws` must be finite.")
    }
  } else {
    beta_draws <- NULL
  }

  n_candidates <- length(candidate_i)
  u0 <- numeric(n_candidates)
  u <- numeric(n_candidates)
  p_mean <- numeric(n_candidates)
  mean_d <- numeric(n_candidates)
  var_d <- numeric(n_candidates)

  for (start in seq(1L, n_candidates, by = chunk_size)) {
    end <- min(n_candidates, start + chunk_size - 1L)
    idx <- start:end
    i_idx <- candidate_i[idx]
    j_idx <- candidate_j[idx]

    d_mat <- theta_draws[, i_idx, drop = FALSE] - theta_draws[, j_idx, drop = FALSE]
    mean_d[idx] <- as.double(colMeans(d_mat))
    var_d[idx] <- as.double(apply(d_mat, 2, stats::var))

    if (!is.null(beta_draws)) {
      d_mat <- d_mat + beta_draws
    }

    p_mat <- stats::plogis(d_mat)
    if (!is.null(epsilon_draws)) {
      p_mat <- p_mat * (1 - epsilon_draws) + 0.5 * epsilon_draws
    }

    p_mean[idx] <- as.double(colMeans(p_mat))
    p2_mean <- as.double(colMeans(p_mat^2))
    u0_chunk <- p2_mean - p_mean[idx]^2
    u0_chunk[u0_chunk < 0 & u0_chunk > -1e-12] <- 0
    u0[idx] <- as.double(u0_chunk)
  }

  if (any(!is.finite(u0)) || any(u0 < 0)) {
    rlang::abort("Draw-based utility values must be finite and non-negative.")
  }
  if (any(!is.finite(p_mean)) || any(!is.finite(mean_d)) || any(!is.finite(var_d))) {
    rlang::abort("Draw-based utility moments must be finite.")
  }

  denom <- sqrt((deg[candidate_i] + 1) * (deg[candidate_j] + 1))
  u <- u0 / denom

  tibble::tibble(
    i = candidate_i,
    j = candidate_j,
    u0 = as.double(u0),
    u = as.double(u),
    p_mean = as.double(p_mean),
    mean_d = as.double(mean_d),
    var_d = as.double(var_d)
  )
}

#' @keywords internal
#' @noRd
utility_delta_var_p <- function(mean_d, var_d, epsilon_mean) {
  if (!is.numeric(mean_d) || !is.numeric(var_d)) {
    rlang::abort("`mean_d` and `var_d` must be numeric.")
  }
  if (length(mean_d) != length(var_d)) {
    rlang::abort("`mean_d` and `var_d` must be the same length.")
  }
  if (length(mean_d) == 0L) {
    return(double())
  }
  if (any(!is.finite(mean_d)) || any(!is.finite(var_d))) {
    rlang::abort("`mean_d` and `var_d` must be finite.")
  }
  if (any(var_d < 0)) {
    rlang::abort("`var_d` must be non-negative.")
  }
  if (!is.numeric(epsilon_mean) || length(epsilon_mean) != 1L || !is.finite(epsilon_mean)) {
    rlang::abort("`epsilon_mean` must be a finite numeric scalar.")
  }
  if (epsilon_mean < 0 || epsilon_mean > 1) {
    rlang::abort("`epsilon_mean` must be in [0, 1].")
  }

  p0 <- stats::plogis(mean_d)
  slope <- (1 - epsilon_mean) * p0 * (1 - p0)
  utility <- (slope^2) * var_d
  utility[var_d == 0] <- 0

  if (any(!is.finite(utility))) {
    rlang::abort("Utility values must be finite.")
  }
  utility
}

# Draw-based var(p) is the primary utility; delta method is fallback only.
#' @keywords internal
#' @noRd
compute_pair_utility <- function(draws, candidates, epsilon_mean) {
  stats_tbl <- compute_pair_stats_from_draws(draws, candidates)
  if (nrow(stats_tbl) == 0L) {
    return(tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    ))
  }

  utility <- utility_delta_var_p(stats_tbl$mean_d, stats_tbl$var_d, epsilon_mean)
  if (any(!is.finite(utility)) || any(utility < 0)) {
    rlang::abort("Utility values must be finite and non-negative.")
  }

  unordered_key <- if (is.data.frame(candidates) && "unordered_key" %in% names(candidates)) {
    as.character(candidates$unordered_key)
  } else {
    make_unordered_key(stats_tbl$i, stats_tbl$j)
  }

  tibble::tibble(
    unordered_key = unordered_key,
    i_id = stats_tbl$i,
    j_id = stats_tbl$j,
    i = stats_tbl$i,
    j = stats_tbl$j,
    mean_d = stats_tbl$mean_d,
    var_d = stats_tbl$var_d,
    p_mean = stats_tbl$p_mean,
    utility = as.double(utility),
    utility_raw = as.double(utility)
  )
}

#' @keywords internal
#' @noRd
compute_pair_utility_dispatch <- function(fit,
                                          candidates,
                                          state,
                                          config,
                                          diagnostics_pass = NULL,
                                          chunk_size = 2000L) {
  validate_state(state)
  if (!is.data.frame(candidates)) {
    rlang::abort("`candidates` must be a data frame or tibble.")
  }
  candidates <- tibble::as_tibble(candidates)

  id_cols <- NULL
  if (all(c("i_id", "j_id") %in% names(candidates))) {
    id_cols <- c("i_id", "j_id")
  } else if (all(c("i", "j") %in% names(candidates))) {
    id_cols <- c("i", "j")
  }
  if (is.null(id_cols)) {
    rlang::abort("`candidates` must include `i_id`/`j_id` or `i`/`j` columns.")
  }
  if (nrow(candidates) == 0L) {
    return(tibble::tibble(
      unordered_key = character(),
      i_id = character(),
      j_id = character(),
      i = character(),
      j = character(),
      mean_d = double(),
      var_d = double(),
      p_mean = double(),
      utility = double(),
      utility_raw = double()
    ))
  }

  if (!is.list(fit) || is.null(fit$theta_draws)) {
    rlang::abort("`fit` must be a list with `theta_draws`.")
  }
  theta_draws <- fit$theta_draws
  if (!is.matrix(theta_draws) || nrow(theta_draws) < 2L) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix with at least two draws.")
  }

  diagnostics_pass <- diagnostics_pass %||% fit$diagnostics_pass %||% FALSE
  use_draws <- isTRUE(diagnostics_pass)

  if (!isTRUE(use_draws)) {
    epsilon_mean <- .adaptive_epsilon_mean_from_state(state, fit)
    utilities <- compute_pair_utility(theta_draws, candidates, epsilon_mean)
    utilities <- apply_degree_penalty(utilities, state)
    return(utilities)
  }

  ids <- colnames(theta_draws)
  if (is.null(ids) || anyNA(ids) || any(ids == "")) {
    rlang::abort("`fit$theta_draws` must have non-empty column names.")
  }

  i_id <- as.character(candidates[[id_cols[[1L]]]])
  j_id <- as.character(candidates[[id_cols[[2L]]]])
  if (anyNA(i_id) || anyNA(j_id) || any(i_id == "") || any(j_id == "")) {
    rlang::abort("`candidates` ids must be non-missing.")
  }

  missing_ids <- setdiff(unique(c(i_id, j_id)), ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`candidates` ids must be present in `fit$theta_draws`.")
  }

  i_idx <- match(i_id, ids)
  j_idx <- match(j_id, ids)
  deg <- state$deg[ids]

  draw_utilities <- compute_pair_utility_from_draws(
    theta_draws = theta_draws,
    candidate_i = i_idx,
    candidate_j = j_idx,
    deg = deg,
    model_variant = config$model_variant,
    epsilon_draws = fit$epsilon_draws %||% NULL,
    beta_draws = fit$beta_draws %||% NULL,
    chunk_size = chunk_size
  )

  unordered_key <- if ("unordered_key" %in% names(candidates)) {
    as.character(candidates$unordered_key)
  } else {
    make_unordered_key(i_id, j_id)
  }

  tibble::tibble(
    unordered_key = unordered_key,
    i_id = i_id,
    j_id = j_id,
    i = i_id,
    j = j_id,
    mean_d = draw_utilities$mean_d,
    var_d = draw_utilities$var_d,
    p_mean = draw_utilities$p_mean,
    utility = as.double(draw_utilities$u),
    utility_raw = as.double(draw_utilities$u0)
  )
}

#' @keywords internal
#' @noRd
apply_degree_penalty <- function(utility_tbl, state) {
  validate_state(state)
  if (!is.data.frame(utility_tbl)) {
    rlang::abort("`utility_tbl` must be a data frame or tibble.")
  }
  utility_tbl <- tibble::as_tibble(utility_tbl)
  required <- c("i_id", "j_id", "utility_raw")
  .adaptive_required_cols(utility_tbl, "utility_tbl", required)
  if (!is.numeric(utility_tbl$utility_raw)) {
    rlang::abort("`utility_tbl$utility_raw` must be numeric.")
  }

  i_id <- as.character(utility_tbl$i_id)
  j_id <- as.character(utility_tbl$j_id)
  missing_ids <- setdiff(unique(c(i_id, j_id)), state$ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("`utility_tbl` ids must exist in `state$ids`.")
  }

  deg_i <- state$deg[i_id]
  deg_j <- state$deg[j_id]
  denom <- sqrt((deg_i + 1) * (deg_j + 1))
  utility <- utility_tbl$utility_raw / denom
  utility_tbl$utility <- as.double(utility)
  utility_tbl
}
