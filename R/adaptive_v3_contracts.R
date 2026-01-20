# -------------------------------------------------------------------------
# Adaptive v3 fit contracts.
# -------------------------------------------------------------------------

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

  epsilon_mean <- fit$epsilon_mean %||% NULL
  if (!is.numeric(epsilon_mean) || length(epsilon_mean) != 1L || !is.finite(epsilon_mean)) {
    rlang::abort("`fit$epsilon_mean` must be a finite numeric scalar.", call = where)
  }

  diagnostics <- fit$diagnostics %||% NULL
  if (is.null(diagnostics) || !is.list(diagnostics)) {
    rlang::abort("`fit$diagnostics` must be a list.", call = where)
  }

  invisible(TRUE)
}
