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
