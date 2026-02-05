# -------------------------------------------------------------------------
# Adaptive v2 TrueSkill state helpers.
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
new_trueskill_state <- function(items,
                                mu0 = 25,
                                sigma0 = 25 / 3,
                                beta = 25 / 6) {
  items <- .adaptive_state_normalize_items(items)
  mu0 <- .validate_trueskill_scalar(mu0, "mu0", allow_nonpositive = TRUE)
  sigma0 <- .validate_trueskill_scalar(sigma0, "sigma0", allow_nonpositive = FALSE)
  beta <- .validate_trueskill_scalar(beta, "beta", allow_nonpositive = FALSE)

  has_mu <- "mu" %in% names(items)
  has_sigma <- "sigma" %in% names(items)

  if (has_mu) {
    if (!is.numeric(items$mu)) {
      rlang::abort("`items$mu` must be numeric when provided.")
    }
    if (any(!is.finite(items$mu), na.rm = TRUE)) {
      rlang::abort("`items$mu` must be finite when provided.")
    }
    items$mu[is.na(items$mu)] <- mu0
  } else {
    items$mu <- rep_len(mu0, nrow(items))
  }

  if (has_sigma) {
    if (!is.numeric(items$sigma)) {
      rlang::abort("`items$sigma` must be numeric when provided.")
    }
    if (any(!is.finite(items$sigma), na.rm = TRUE)) {
      rlang::abort("`items$sigma` must be finite when provided.")
    }
    items$sigma[is.na(items$sigma)] <- sigma0
  } else {
    items$sigma <- rep_len(sigma0, nrow(items))
  }

  if (any(items$sigma <= 0)) {
    rlang::abort("`items$sigma` must be > 0.")
  }

  items <- dplyr::relocate(items, "item_id", "mu", "sigma")

  structure(
    list(
      items = items,
      beta = beta
    ),
    class = "trueskill_state"
  )
}

#' @keywords internal
#' @noRd
validate_trueskill_state <- function(trueskill_state) {
  if (!inherits(trueskill_state, "trueskill_state")) {
    rlang::abort("`trueskill_state` must inherit from class `trueskill_state`.")
  }
  if (!is.list(trueskill_state)) {
    rlang::abort("`trueskill_state` must be a list.")
  }
  if (!is.data.frame(trueskill_state$items)) {
    rlang::abort("`trueskill_state$items` must be a data frame.")
  }

  items <- trueskill_state$items
  required <- c("item_id", "mu", "sigma")
  if (!all(required %in% names(items))) {
    rlang::abort("`trueskill_state$items` must include `item_id`, `mu`, and `sigma`.")
  }

  item_id <- as.character(items$item_id)
  if (any(is.na(item_id) | item_id == "")) {
    rlang::abort("`trueskill_state$items$item_id` must be non-missing.")
  }
  if (anyDuplicated(item_id)) {
    rlang::abort("`trueskill_state$items$item_id` must be unique.")
  }

  if (!is.numeric(items$mu) || any(!is.finite(items$mu))) {
    rlang::abort("`trueskill_state$items$mu` must be finite numeric values.")
  }
  if (!is.numeric(items$sigma) || any(!is.finite(items$sigma))) {
    rlang::abort("`trueskill_state$items$sigma` must be finite numeric values.")
  }
  if (any(items$sigma <= 0)) {
    rlang::abort("`trueskill_state$items$sigma` must be > 0.")
  }

  beta <- trueskill_state$beta
  .validate_trueskill_scalar(beta, "beta", allow_nonpositive = FALSE)

  trueskill_state
}

#' @keywords internal
#' @noRd
trueskill_win_probability <- function(i, j, trueskill_state) {
  validate_trueskill_state(trueskill_state)

  if (length(i) != 1L || length(j) != 1L) {
    rlang::abort("`i` and `j` must be scalar item ids.")
  }
  if (identical(i, j)) {
    rlang::abort("`i` and `j` must be distinct item ids.")
  }

  items <- trueskill_state$items
  item_ids <- as.character(items$item_id)
  i_id <- as.character(i)
  j_id <- as.character(j)

  i_pos <- match(i_id, item_ids)
  j_pos <- match(j_id, item_ids)
  if (is.na(i_pos) || is.na(j_pos)) {
    rlang::abort("`i` and `j` must be present in `trueskill_state$items`.")
  }

  mu_i <- items$mu[[i_pos]]
  mu_j <- items$mu[[j_pos]]
  sigma_i <- items$sigma[[i_pos]]
  sigma_j <- items$sigma[[j_pos]]

  s2 <- sigma_i^2 + sigma_j^2 + 2 * trueskill_state$beta^2
  stats::pnorm((mu_i - mu_j) / sqrt(s2))
}

.validate_trueskill_scalar <- function(value, name, allow_nonpositive) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    rlang::abort(paste0("`", name, "` must be a finite numeric scalar."))
  }
  if (!allow_nonpositive && value <= 0) {
    rlang::abort(paste0("`", name, "` must be > 0."))
  }
  value
}
