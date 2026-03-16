# -------------------------------------------------------------------------
# Adaptive printing, summaries, and log accessors.
# -------------------------------------------------------------------------

.adaptive_item_log_columns <- function() {
  c(
    "refit_id",
    "item_id",
    "set_id",
    "phase_scope",
    "phase_scope_set_id",
    "in_phase_scope",
    "is_hub_item",
    "is_spoke_item",
    "theta_raw_eap",
    "theta_raw_p2.5",
    "theta_raw_p5",
    "theta_raw_p50",
    "theta_raw_p95",
    "theta_raw_p97.5",
    "theta_raw_sd",
    "rank_raw",
    "theta_link_eap",
    "theta_link_p2.5",
    "theta_link_p5",
    "theta_link_p50",
    "theta_link_p95",
    "theta_link_p97.5",
    "theta_link_sd",
    "rank_link",
    "degree",
    "pos_count_A",
    "pos_count_B"
  )
}

.adaptive_empty_item_log_tbl <- function() {
  tibble::tibble(
    refit_id = integer(),
    item_id = character(),
    set_id = integer(),
    phase_scope = character(),
    phase_scope_set_id = integer(),
    in_phase_scope = logical(),
    is_hub_item = logical(),
    is_spoke_item = logical(),
    theta_raw_eap = double(),
    theta_raw_p2.5 = double(),
    theta_raw_p5 = double(),
    theta_raw_p50 = double(),
    theta_raw_p95 = double(),
    theta_raw_p97.5 = double(),
    theta_raw_sd = double(),
    rank_raw = integer(),
    theta_link_eap = double(),
    theta_link_p2.5 = double(),
    theta_link_p5 = double(),
    theta_link_p50 = double(),
    theta_link_p95 = double(),
    theta_link_p97.5 = double(),
    theta_link_sd = double(),
    rank_link = integer(),
    degree = integer(),
    pos_count_A = integer(),
    pos_count_B = integer()
  )
}

.adaptive_item_log_na_value <- function(col) {
  int_cols <- c(
    "refit_id", "set_id", "phase_scope_set_id", "rank_raw", "rank_link",
    "degree", "pos_count_A", "pos_count_B"
  )
  if (col %in% int_cols) {
    return(NA_integer_)
  }
  if (col %in% c("item_id", "phase_scope")) {
    return(NA_character_)
  }
  if (col %in% c("is_hub_item", "is_spoke_item", "in_phase_scope")) {
    return(NA)
  }
  NA_real_
}

.adaptive_log_factor_specs_step <- function() {
  list(
    run_mode = c("within_set", "link_one_spoke", "link_multi_spoke", "link_probe_holdout", "link_probe"),
    link_estimation_mode = .adaptive_link_estimation_mode_levels(),
    link_stage = c("anchor_link", "long_link", "mid_link", "local_link", "probe_panel"),
    link_transform_policy = .adaptive_link_transform_policy_levels(),
    link_transform_state = .adaptive_link_transform_state_levels(),
    utility_mode = c("pairing_trueskill_u0", "linking_d_optimal"),
    hub_lock_mode = c("hard_lock", "soft_lock")
  )
}

.adaptive_log_factor_specs_link_stage <- function() {
  list(
    link_estimation_mode = .adaptive_link_estimation_mode_levels(),
    link_transform_policy = .adaptive_link_transform_policy_levels(),
    link_transform_state = .adaptive_link_transform_state_levels(),
    link_refit_mode = c("shift_only", "joint_refit"),
    shift_only_theta_treatment = .adaptive_shift_only_theta_treatment_levels(),
    shift_only_theta_treatment_resolved = .adaptive_shift_only_theta_treatment_levels(),
    hub_lock_mode = c("hard_lock", "soft_lock"),
    anchored_joint_init_state_method = .adaptive_anchored_joint_init_state_method_levels()
  )
}

.adaptive_cast_log_factors <- function(log_tbl, specs, log_name) {
  out <- tibble::as_tibble(log_tbl)
  if (!is.list(specs) || length(specs) == 0L) {
    return(out)
  }
  for (col in names(specs)) {
    if (!col %in% names(out)) {
      next
    }
    allowed <- as.character(specs[[col]])
    vals <- as.character(out[[col]])
    invalid <- !is.na(vals) & !vals %in% allowed
    if (any(invalid)) {
      bad_vals <- sort(unique(vals[invalid]))
      rlang::abort(paste0(
        "`",
        log_name,
        "$",
        col,
        "` has invalid levels: ",
        paste(bad_vals, collapse = ", "),
        "."
      ))
    }
    out[[col]] <- factor(vals, levels = allowed)
  }
  out
}

.adaptive_link_anchored_joint_quantiles <- function(theta_mean, theta_sd, probs) {
  theta_mean <- as.double(theta_mean)
  theta_sd <- as.double(theta_sd)
  out <- matrix(
    NA_real_,
    nrow = length(probs),
    ncol = length(theta_mean),
    dimnames = list(NULL, names(theta_mean))
  )
  if (length(theta_mean) < 1L) {
    return(out)
  }

  finite_mean <- is.finite(theta_mean)
  if (any(finite_mean)) {
    out[probs == 0.5, finite_mean] <- theta_mean[finite_mean]
  }

  point_mass <- finite_mean & is.finite(theta_sd) & theta_sd <= 0
  if (any(point_mass)) {
    out[, point_mass] <- matrix(
      theta_mean[point_mass],
      nrow = length(probs),
      ncol = sum(point_mass),
      byrow = TRUE
    )
  }

  approx_idx <- finite_mean & is.finite(theta_sd) & theta_sd > 0
  if (any(approx_idx)) {
    z <- stats::qnorm(probs)
    out[, approx_idx] <- vapply(
      which(approx_idx),
      function(idx) theta_mean[[idx]] + z * theta_sd[[idx]],
      numeric(length(probs))
    )
  }

  out
}

.adaptive_link_item_raw_link_summaries <- function(state,
                                                   ids,
                                                   set_id,
                                                   theta_raw_eap,
                                                   theta_raw_sd,
                                                   theta_raw_quantiles,
                                                   is_link_phase_a = FALSE) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")

  link_eap <- as.double(theta_raw_eap)
  link_sd <- as.double(theta_raw_sd)
  link_quantiles <- theta_raw_quantiles

  if (!isTRUE(is_link_mode)) {
    return(list(
      theta_link_eap = as.double(link_eap),
      theta_link_sd = as.double(link_sd),
      theta_link_quantiles = link_quantiles
    ))
  }

  if (isTRUE(is_link_phase_a)) {
    return(list(
      theta_link_eap = rep_len(NA_real_, length(ids)),
      theta_link_sd = rep_len(NA_real_, length(ids)),
      theta_link_quantiles = matrix(
        NA_real_,
        nrow = nrow(theta_raw_quantiles),
        ncol = ncol(theta_raw_quantiles),
        dimnames = dimnames(theta_raw_quantiles)
      )
    ))
  }

  if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
    link_eap <- rep_len(NA_real_, length(ids))
    link_sd <- rep_len(NA_real_, length(ids))
    names(link_eap) <- ids
    names(link_sd) <- ids
    link_quantiles <- matrix(NA_real_, nrow = length(probs), ncol = length(ids))
    rownames(link_quantiles) <- c("q2.5", "q5", "q50", "q95", "q97.5")
    colnames(link_quantiles) <- ids

    hub_id <- as.integer(controller$hub_id %||% 1L)
    spoke_ids <- sort(unique(as.integer(set_id[as.integer(set_id) != hub_id])))
    if (length(spoke_ids) < 1L) {
      return(list(
        theta_link_eap = as.double(link_eap),
        theta_link_sd = as.double(link_sd),
        theta_link_quantiles = link_quantiles
      ))
    }

    hub_state <- .adaptive_link_anchored_joint_resolve_state(
      state = state,
      spoke_id = as.integer(spoke_ids[[1L]]),
      controller = controller
    )
    hub_mean <- as.double(hub_state$theta_hub_fixed)
    names(hub_mean) <- names(hub_state$theta_hub_fixed)
    hub_ids <- intersect(ids[as.integer(set_id) == hub_id], names(hub_mean))
    if (length(hub_ids) > 0L) {
      hub_quantiles <- .adaptive_link_anchored_joint_quantiles(
        theta_mean = hub_mean,
        theta_sd = rep(0, length(hub_mean)),
        probs = probs
      )
      hub_match <- match(hub_ids, colnames(hub_quantiles))
      link_eap[hub_ids] <- hub_mean[hub_ids]
      link_sd[hub_ids] <- 0
      link_quantiles[, hub_ids] <- hub_quantiles[, hub_match, drop = FALSE]
    }

    for (spoke_id in spoke_ids) {
      accepted_state <- .adaptive_link_anchored_joint_resolve_state(
        state = state,
        spoke_id = as.integer(spoke_id),
        controller = controller
      )
      spoke_mean <- as.double(accepted_state$theta_spoke_global_mean)
      names(spoke_mean) <- names(accepted_state$theta_spoke_global_mean)
      spoke_sd <- as.double(accepted_state$theta_spoke_global_sd)
      names(spoke_sd) <- names(accepted_state$theta_spoke_global_sd)
      spoke_item_ids <- intersect(ids[as.integer(set_id) == as.integer(spoke_id)], names(spoke_mean))
      if (length(spoke_item_ids) < 1L) {
        next
      }
      spoke_quantiles <- .adaptive_link_anchored_joint_quantiles(
        theta_mean = spoke_mean,
        theta_sd = spoke_sd,
        probs = probs
      )
      spoke_match <- match(spoke_item_ids, colnames(spoke_quantiles))
      link_eap[spoke_item_ids] <- spoke_mean[spoke_item_ids]
      link_sd[spoke_item_ids] <- spoke_sd[spoke_item_ids]
      link_quantiles[, spoke_item_ids] <- spoke_quantiles[, spoke_match, drop = FALSE]
    }

    return(list(
      theta_link_eap = as.double(link_eap),
      theta_link_sd = as.double(link_sd),
      theta_link_quantiles = link_quantiles
    ))
  }

  hub_id <- as.integer(controller$hub_id %||% 1L)
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  spoke_ids <- sort(unique(as.integer(set_id[as.integer(set_id) != hub_id])))
  for (spoke_id in spoke_ids) {
    spoke_idx <- which(as.integer(set_id) == as.integer(spoke_id))
    if (length(spoke_idx) < 1L) {
      next
    }
    stats_row <- link_stats[[as.character(spoke_id)]] %||% list()
    mode <- as.character(stats_row$link_transform_state %||%
      .adaptive_link_transform_state_for_spoke(controller, spoke_id))
    if (!mode %in% c("shift_only", "shift_scale")) {
      link_eap[spoke_idx] <- NA_real_
      link_sd[spoke_idx] <- NA_real_
      link_quantiles[, spoke_idx] <- NA_real_
      next
    }
    delta <- as.double(stats_row$delta_spoke_mean %||% NA_real_)
    if (!is.finite(delta)) {
      link_eap[spoke_idx] <- NA_real_
      link_sd[spoke_idx] <- NA_real_
      link_quantiles[, spoke_idx] <- NA_real_
      next
    }
    alpha <- 1
    if (identical(mode, "shift_scale")) {
      log_alpha <- as.double(stats_row$log_alpha_spoke_mean %||% NA_real_)
      if (!is.finite(log_alpha)) {
        link_eap[spoke_idx] <- NA_real_
        link_sd[spoke_idx] <- NA_real_
        link_quantiles[, spoke_idx] <- NA_real_
        next
      }
      alpha <- exp(log_alpha)
    }
    link_eap[spoke_idx] <- as.double(delta + alpha * theta_raw_eap[spoke_idx])
    link_sd[spoke_idx] <- as.double(abs(alpha) * theta_raw_sd[spoke_idx])
    link_quantiles[, spoke_idx] <- as.double(delta + alpha * theta_raw_quantiles[, spoke_idx, drop = FALSE])
  }

  list(
    theta_link_eap = as.double(link_eap),
    theta_link_sd = as.double(link_sd),
    theta_link_quantiles = link_quantiles
  )
}

.adaptive_build_item_log_refit <- function(state, refit_id) {
  fit <- state$btl_fit %||% list()
  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws)) {
    return(.adaptive_empty_item_log_tbl())
  }

  ids <- as.character(state$item_ids)
  set_id <- as.integer(state$items$set_id[match(ids, as.character(state$items$item_id))])
  if (is.null(colnames(draws))) {
    if (ncol(draws) == length(ids)) {
      colnames(draws) <- ids
    }
  }
  draw_ids <- intersect(ids, as.character(colnames(draws)))
  theta_raw_eap <- rep_len(NA_real_, length(ids))
  theta_raw_sd <- rep_len(NA_real_, length(ids))
  names(theta_raw_eap) <- ids
  names(theta_raw_sd) <- ids
  theta_raw_quantiles <- matrix(NA_real_, nrow = 5L, ncol = length(ids))
  rownames(theta_raw_quantiles) <- c("q2.5", "q5", "q50", "q95", "q97.5")
  colnames(theta_raw_quantiles) <- ids

  if (length(draw_ids) > 0L) {
    draws <- draws[, draw_ids, drop = FALSE]
    draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")

    probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
    theta_mean_vals <- as.double(colMeans(draws))
    theta_sd_vals <- as.double(apply(draws, 2, stats::sd))
    theta_quantile_vals <- vapply(
      seq_len(ncol(draws)),
      function(idx) stats::quantile(draws[, idx], probs = probs, names = FALSE),
      numeric(length(probs))
    )
    theta_raw_eap[draw_ids] <- theta_mean_vals
    theta_raw_sd[draw_ids] <- theta_sd_vals
    theta_raw_quantiles[, draw_ids] <- theta_quantile_vals
  }
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_phase_a <- run_mode %in% c("link_one_spoke", "link_multi_spoke") &&
    !identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")
  phase_scope <- if (isTRUE(is_link_phase_a)) "phase_a_set" else "global"
  active_set <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
  phase_scope_set_id <- if (isTRUE(is_link_phase_a) && is.finite(active_set)) active_set else NA_integer_
  in_phase_scope <- rep_len(TRUE, length(ids))
  if (isTRUE(is_link_phase_a) && is.finite(active_set)) {
    in_phase_scope <- as.integer(set_id) == active_set
  }

  link_summary <- .adaptive_link_item_raw_link_summaries(
    state = state,
    ids = ids,
    set_id = set_id,
    theta_raw_eap = as.double(theta_raw_eap),
    theta_raw_sd = as.double(theta_raw_sd),
    theta_raw_quantiles = theta_raw_quantiles,
    is_link_phase_a = is_link_phase_a
  )
  rank_raw <- as.integer(rank(-as.double(theta_raw_eap), ties.method = "first"))
  rank_link <- as.integer(rank(-as.double(link_summary$theta_link_eap), ties.method = "first"))
  hub_id <- as.integer((controller %||% list())$hub_id %||% 1L)
  is_hub_item <- as.logical(set_id == hub_id)
  is_spoke_item <- as.logical(set_id != hub_id)

  counts <- .adaptive_pair_counts(.adaptive_history_tbl(state), ids)
  degree <- as.integer(counts$deg[ids])
  pos_count_A <- as.integer(counts$posA[ids])
  pos_count_B <- as.integer(counts$posB[ids])

  tibble::tibble(
    refit_id = as.integer(refit_id),
    item_id = as.character(ids),
    set_id = as.integer(set_id),
    phase_scope = as.character(phase_scope),
    phase_scope_set_id = as.integer(phase_scope_set_id),
    in_phase_scope = as.logical(in_phase_scope),
    is_hub_item = as.logical(is_hub_item),
    is_spoke_item = as.logical(is_spoke_item),
    theta_raw_eap = as.double(theta_raw_eap),
    `theta_raw_p2.5` = as.double(theta_raw_quantiles[1L, ]),
    `theta_raw_p5` = as.double(theta_raw_quantiles[2L, ]),
    `theta_raw_p50` = as.double(theta_raw_quantiles[3L, ]),
    `theta_raw_p95` = as.double(theta_raw_quantiles[4L, ]),
    `theta_raw_p97.5` = as.double(theta_raw_quantiles[5L, ]),
    theta_raw_sd = as.double(theta_raw_sd),
    rank_raw = as.integer(rank_raw),
    theta_link_eap = as.double(link_summary$theta_link_eap),
    `theta_link_p2.5` = as.double(link_summary$theta_link_quantiles[1L, ]),
    `theta_link_p5` = as.double(link_summary$theta_link_quantiles[2L, ]),
    `theta_link_p50` = as.double(link_summary$theta_link_quantiles[3L, ]),
    `theta_link_p95` = as.double(link_summary$theta_link_quantiles[4L, ]),
    `theta_link_p97.5` = as.double(link_summary$theta_link_quantiles[5L, ]),
    theta_link_sd = as.double(link_summary$theta_link_sd),
    rank_link = as.integer(rank_link),
    degree = as.integer(degree),
    pos_count_A = as.integer(pos_count_A),
    pos_count_B = as.integer(pos_count_B)
  )
}

.adaptive_canonicalize_item_log <- function(item_log, state, refit_id = NULL) {
  if (!is.data.frame(item_log)) {
    rlang::abort("`item_log` entries must be data frames.")
  }
  item_log <- tibble::as_tibble(item_log)

  if (!"item_id" %in% names(item_log) && "ID" %in% names(item_log)) {
    item_log$item_id <- as.character(item_log$ID)
  }

  if (!"degree" %in% names(item_log) && "deg" %in% names(item_log)) {
    item_log$degree <- as.integer(item_log$deg)
  }

  if (!"refit_id" %in% names(item_log)) {
    item_log$refit_id <- as.integer(refit_id %||% NA_integer_)
  }

  ids <- as.character(state$item_ids)
  counts <- .adaptive_pair_counts(.adaptive_history_tbl(state), ids)
  idx <- match(as.character(item_log$item_id), ids)
  if (!"set_id" %in% names(item_log)) {
    state_set <- state$items$set_id[match(ids, as.character(state$items$item_id))]
    item_log$set_id <- as.integer(state_set[idx])
  }
  if (!"pos_count_A" %in% names(item_log)) {
    item_log$pos_count_A <- as.integer(counts$posA[idx])
  }
  if (!"pos_count_B" %in% names(item_log)) {
    item_log$pos_count_B <- as.integer(counts$posB[idx])
  }
  if (!"degree" %in% names(item_log)) {
    item_log$degree <- as.integer(counts$deg[idx])
  }

  missing <- setdiff(.adaptive_item_log_columns(), names(item_log))
  if (length(missing) > 0L) {
    for (col in missing) {
      item_log[[col]] <- rep_len(.adaptive_item_log_na_value(col), nrow(item_log))
    }
  }

  item_log <- item_log[, .adaptive_item_log_columns(), drop = FALSE]
  item_log$refit_id <- as.integer(item_log$refit_id)
  item_log$item_id <- as.character(item_log$item_id)
  item_log$set_id <- as.integer(item_log$set_id)
  item_log$phase_scope <- as.character(item_log$phase_scope)
  item_log$phase_scope_set_id <- as.integer(item_log$phase_scope_set_id)
  item_log$in_phase_scope <- as.logical(item_log$in_phase_scope)
  item_log$is_hub_item <- as.logical(item_log$is_hub_item)
  item_log$is_spoke_item <- as.logical(item_log$is_spoke_item)
  item_log$theta_raw_eap <- as.double(item_log$theta_raw_eap)
  item_log$`theta_raw_p2.5` <- as.double(item_log$`theta_raw_p2.5`)
  item_log$`theta_raw_p5` <- as.double(item_log$`theta_raw_p5`)
  item_log$`theta_raw_p50` <- as.double(item_log$`theta_raw_p50`)
  item_log$`theta_raw_p95` <- as.double(item_log$`theta_raw_p95`)
  item_log$`theta_raw_p97.5` <- as.double(item_log$`theta_raw_p97.5`)
  item_log$theta_raw_sd <- as.double(item_log$theta_raw_sd)
  item_log$rank_raw <- as.integer(item_log$rank_raw)
  item_log$theta_link_eap <- as.double(item_log$theta_link_eap)
  item_log$`theta_link_p2.5` <- as.double(item_log$`theta_link_p2.5`)
  item_log$`theta_link_p5` <- as.double(item_log$`theta_link_p5`)
  item_log$`theta_link_p50` <- as.double(item_log$`theta_link_p50`)
  item_log$`theta_link_p95` <- as.double(item_log$`theta_link_p95`)
  item_log$`theta_link_p97.5` <- as.double(item_log$`theta_link_p97.5`)
  item_log$theta_link_sd <- as.double(item_log$theta_link_sd)
  item_log$rank_link <- as.integer(item_log$rank_link)
  item_log$degree <- as.integer(item_log$degree)
  item_log$pos_count_A <- as.integer(item_log$pos_count_A)
  item_log$pos_count_B <- as.integer(item_log$pos_count_B)
  tibble::as_tibble(item_log)
}

.adaptive_append_item_log <- function(state, item_log_tbl) {
  if (is.null(item_log_tbl) || nrow(item_log_tbl) == 0L) {
    return(state)
  }
  item_logs <- state$item_log
  if (is.null(item_logs)) {
    item_logs <- list()
  }
  if (!is.list(item_logs)) {
    rlang::abort("`state$item_log` must be a list.")
  }
  state$item_log <- c(item_logs, list(item_log_tbl))
  state
}

#' Retrieve canonical adaptive logs.
#'
#' @details
#' Returns the canonical adaptive logs as currently held in memory:
#' \code{step_log}, \code{round_log}, \code{item_log}, and
#' \code{link_stage_log}. These correspond to step attempts, posterior refit
#' rounds, item-level refit summaries, and per-refit linking summaries.
#'
#' @param state Adaptive state.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{step_log}{A tibble with one row per attempted step.}
#'   \item{round_log}{A tibble with one row per BTL refit round.}
#'   \item{item_log}{A list of per-refit item tibbles.}
#'   \item{link_stage_log}{A tibble with one row per \code{(refit_id, spoke_id)}
#'   linking summary when linking mode is active.}
#' }
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' logs <- adaptive_get_logs(state)
#' names(logs)
#'
#' @seealso [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive logs
#' @export
adaptive_get_logs <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (is.null(state$step_log)) {
    rlang::abort("`state$step_log` is missing.")
  }
  if (is.null(state$round_log)) {
    rlang::abort("`state$round_log` is missing.")
  }
  if (is.null(state$item_log)) {
    rlang::abort("`state$item_log` is missing.")
  }
  item_log <- if (length(state$item_log) == 0L) {
    list()
  } else {
    lapply(seq_along(state$item_log), function(idx) {
      .adaptive_canonicalize_item_log(state$item_log[[idx]], state, refit_id = idx)
    })
  }
  list(
    step_log = .adaptive_cast_log_factors(
      state$step_log,
      specs = .adaptive_log_factor_specs_step(),
      log_name = "step_log"
    ),
    round_log = tibble::as_tibble(state$round_log),
    item_log = item_log,
    link_stage_log = .adaptive_cast_log_factors(
      state$link_stage_log %||% new_link_stage_log(),
      specs = .adaptive_log_factor_specs_link_stage(),
      log_name = "link_stage_log"
    )
  )
}

#' Adaptive step log accessor.
#'
#' @details
#' \code{step_log} is the canonical per-step audit log for the adaptive
#' workflow. It records candidate pipeline outcomes, selected pair/order, and
#' commit status. A step with invalid judge response keeps committed fields
#' as \code{NA} and must not update model state.
#'
#' Core columns:
#' \itemize{
#'   \item Identity/outcome: \code{step_id}, \code{timestamp}, \code{pair_id},
#'   \code{i}, \code{j}, \code{A}, \code{B}, \code{Y}, \code{status}.
#'   \item Routing/scheduling: \code{round_id}, \code{round_stage},
#'   \code{pair_type}, \code{stage_committed_so_far}, \code{stage_quota}.
#'   \item Exposure/strata: \code{used_in_round_i}, \code{used_in_round_j},
#'   \code{is_anchor_i}, \code{is_anchor_j}, \code{stratum_i},
#'   \code{stratum_j}, \code{dist_stratum}.
#'   \item Candidate health: \code{is_explore_step}, \code{explore_mode},
#'   \code{explore_reason}, \code{explore_rate_used},
#'   \code{local_priority_mode}, \code{long_gate_pass},
#'   \code{long_gate_reason}, \code{star_override_used},
#'   \code{star_override_reason}, \code{candidate_starved},
#'   \code{fallback_used}, \code{fallback_path}, \code{starvation_reason}.
#'   \item Candidate counts: \code{n_candidates_generated},
#'   \code{n_candidates_after_hard_filters}, \code{n_candidates_after_duplicates},
#'   \code{n_candidates_after_star_caps}, \code{n_candidates_scored}.
#'   \item Endpoint diagnostics: \code{deg_i}, \code{deg_j},
#'   \code{recent_deg_i}, \code{recent_deg_j}, \code{mu_i}, \code{mu_j},
#'   \code{sigma_i}, \code{sigma_j}, \code{p_ij}, \code{U0_ij}.
#'   \item Star-cap diagnostics: \code{star_cap_rejects},
#'   \code{star_cap_reject_items}.
#' }
#'
#' @param state Adaptive state.
#'
#' @return A tibble with one row per attempted step, in execution order.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_step_log(state)
#'
#' @seealso [adaptive_get_logs()], [adaptive_round_log()], [adaptive_rank_run_live()]
#'
#' @family adaptive logs
#' @export
adaptive_step_log <- function(state) {
  if (is.null(state$step_log)) {
    rlang::abort("`state$step_log` is missing.")
  }
  .adaptive_cast_log_factors(
    state$step_log,
    specs = .adaptive_log_factor_specs_step(),
    log_name = "step_log"
  )
}

#' Adaptive round log accessor.
#'
#' @details
#' \code{round_log} is the canonical per-refit audit log for the adaptive
#' pairing workflow.
#' Each row summarizes one Bayesian BTL refit and includes
#' diagnostics, reliability, and stopping-gate fields used to justify stop
#' decisions.
#'
#' Core columns:
#' \itemize{
#'   \item Refit identity/state: \code{refit_id}, \code{round_id_at_refit},
#'   \code{step_id_at_refit}, \code{timestamp}, \code{model_variant},
#'   \code{n_items}, \code{total_pairs_done}, \code{new_pairs_since_last_refit},
#'   \code{n_unique_pairs_seen}.
#'   \item Candidate health: \code{proposed_pairs_mode},
#'   \code{starve_rate_since_last_refit}, \code{fallback_rate_since_last_refit},
#'   \code{fallback_used_mode}, \code{starvation_reason_mode}.
#'   \item Identifiability/quota adaptation: \code{global_identified},
#'   \code{global_identified_reliability_min},
#'   \code{global_identified_rank_corr_min}, \code{long_quota_raw},
#'   \code{long_quota_effective}, \code{long_quota_removed},
#'   \code{realloc_to_mid}, \code{realloc_to_local},
#'   \code{phase_scope}, \code{phase_scope_set_id}, \code{phase_scope_n_items}.
#'   \item Coverage/imbalance: \code{mean_degree}, \code{min_degree},
#'   \code{mean_degree_scope}, \code{min_degree_scope},
#'   \code{pos_balance_sd}, \code{star_cap_rejects_since_last_refit},
#'   \code{star_cap_reject_rate_since_last_refit},
#'   \code{recent_deg_median_since_last_refit},
#'   \code{recent_deg_max_since_last_refit}.
#'   \item Posterior parameter summaries:
#'   \code{epsilon_mean}/percentiles and \code{b_mean}/percentiles.
#'   \item Audit diagnostics: \code{ts_sigma_mean}, \code{ts_sigma_max},
#'   \code{ts_degree_sigma_corr}, \code{ts_btl_theta_corr},
#'   \code{ts_btl_rank_spearman}, \code{ci95_theta_width_*},
#'   \code{near_tie_adj_frac}, \code{near_tie_adj_count}, \code{p_adj_median},
#'   \code{cov_trace_theta}, \code{cov_logdet_diag_theta},
#'   \code{post_sd_theta_p10}, \code{post_sd_theta_p50},
#'   \code{post_sd_theta_p90}, \code{top20_boundary_entropy_*},
#'   \code{nn_diff_sd_*}.
#'   \item Stopping diagnostics: \code{diagnostics_pass},
#'   \code{diagnostics_divergences_pass}, \code{diagnostics_rhat_pass},
#'   \code{diagnostics_ess_pass}, \code{divergences},
#'   \code{divergences_max_allowed}, \code{max_rhat},
#'   \code{max_rhat_allowed}, \code{min_ess_bulk},
#'   \code{ess_bulk_required}, \code{near_stop_active},
#'   \code{reliability_EAP}, \code{reliability_EAP_scope},
#'   \code{eap_reliability_min}, \code{eap_pass}, \code{eap_pass_scope},
#'   \code{theta_sd_eap}, \code{theta_sd_eap_scope},
#'   \code{rho_theta}, \code{rho_theta_scope},
#'   \code{lag_eligible}, \code{lag_eligible_scope},
#'   \code{theta_corr_min}, \code{theta_corr_pass}, \code{theta_corr_pass_scope},
#'   \code{delta_sd_theta}, \code{delta_sd_theta_scope},
#'   \code{theta_sd_rel_change_max}, \code{delta_sd_theta_pass},
#'   \code{delta_sd_theta_pass_scope}, \code{rho_rank}, \code{rho_rank_scope},
#'   \code{rank_spearman_min}, \code{rho_rank_pass}, \code{rho_rank_pass_scope}.
#'   \item Refit execution metadata: \code{mcmc_chains},
#'   \code{mcmc_parallel_chains}, \code{mcmc_core_fraction},
#'   \code{mcmc_cores_detected_physical}, \code{mcmc_cores_detected_logical},
#'   \code{mcmc_threads_per_chain}, \code{mcmc_cmdstanr_version}.
#'   \item Stop output: \code{stop_decision}, \code{stop_reason},
#'   \code{max_pairs_after_stop}, \code{pairs_committed_after_stop}.
#' }
#'
#' @param state Adaptive state.
#'
#' @return A tibble with one row per completed posterior refit round.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_round_log(state)
#'
#' @seealso [adaptive_get_logs()], [summarize_refits()], [adaptive_rank_run_live()]
#'
#' @family adaptive logs
#' @export
adaptive_round_log <- function(state) {
  if (is.null(state$round_log)) {
    rlang::abort("`state$round_log` is missing.")
  }
  tibble::as_tibble(state$round_log)
}

#' Adaptive item log accessor.
#'
#' @details
#' \code{item_log} stores per-item posterior summaries by refit.
#' The underlying state stores a list of refit tables; this
#' accessor can return one refit table (default: most recent) or stack all
#' refits into a single tibble.
#'
#' Item-level summaries are domain-explicit:
#' \itemize{
#'   \item \code{theta_raw_*}: raw/within-set posterior summaries (EAP, fixed
#'   quantiles, SD, rank) at the current refit.
#'   \item \code{theta_link_*}: linked/global posterior summaries (EAP, fixed
#'   quantiles, SD, rank) after transform application.
#'   \item During linking Phase A (\code{phase_scope = "phase_a_set"}),
#'   \code{theta_link_*} is typed \code{NA} by design.
#'   \item \code{phase_scope}, \code{phase_scope_set_id}, and
#'   \code{in_phase_scope} indicate which item domain is currently optimized.
#' }
#'
#' @param state Adaptive state.
#' @param refit_id Optional refit index.
#' @param stack When TRUE, stack all refits.
#'
#' @return A tibble of item-level summaries. When \code{stack = FALSE}, one row
#'   per item for the selected refit. When \code{stack = TRUE}, one row per item
#'   per refit with \code{refit_id} identifying source refit.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_item_log(state)
#'
#' @seealso [adaptive_get_logs()], [summarize_items()], [adaptive_round_log()]
#'
#' @family adaptive logs
#' @export
adaptive_item_log <- function(state, refit_id = NULL, stack = FALSE) {
  if (is.null(state$item_log)) {
    rlang::abort("`state$item_log` is missing.")
  }
  if (!is.list(state$item_log)) {
    rlang::abort("`state$item_log` must be a list of data frames.")
  }

  item_logs <- state$item_log
  if (length(item_logs) == 0L) {
    return(.adaptive_empty_item_log_tbl())
  }

  if (isTRUE(stack)) {
    stacked <- lapply(seq_along(item_logs), function(idx) {
      .adaptive_canonicalize_item_log(item_logs[[idx]], state, refit_id = idx)
    })
    return(dplyr::bind_rows(stacked))
  }

  idx <- refit_id %||% length(item_logs)
  idx <- as.integer(idx)
  if (length(idx) != 1L || is.na(idx) || idx < 1L || idx > length(item_logs)) {
    rlang::abort("`refit_id` is not available in `state$item_log`.")
  }

  .adaptive_canonicalize_item_log(item_logs[[idx]], state, refit_id = idx)
}

#' Adaptive results history in build_bt_data() format.
#'
#' @details
#' Converts adaptive step outcomes into the three-column format used by
#' [build_bt_data()] (\code{object1}, \code{object2}, \code{result}). With
#' \code{committed_only = TRUE}, only committed steps (\code{pair_id} not
#' missing) are retained. This preserves the transactional invariant that
#' invalid steps do not contribute to inferred comparisons.
#'
#' @param state Adaptive state.
#' @param committed_only Use only committed comparisons.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{object1}{Character item id shown in position A.}
#'   \item{object2}{Character item id shown in position B.}
#'   \item{result}{Numeric outcome in \code{\{0, 1\}} where \code{1} means
#'     \code{object1} wins.}
#' }
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_results_history(state)
#'
#' @seealso [build_bt_data()], [adaptive_step_log()]
#'
#' @family adaptive logs
#' @export
adaptive_results_history <- function(state, committed_only = TRUE) {
  step_log <- adaptive_step_log(state)
  ids <- as.character(state$item_ids)

  if (isTRUE(committed_only)) {
    step_log <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  } else {
    ok <- !is.na(step_log$A) & !is.na(step_log$B) & !is.na(step_log$Y)
    step_log <- step_log[ok, , drop = FALSE]
  }
  if (nrow(step_log) == 0L) {
    return(tibble::tibble(object1 = character(), object2 = character(), result = double()))
  }

  tibble::tibble(
    object1 = as.character(ids[step_log$A]),
    object2 = as.character(ids[step_log$B]),
    result = as.numeric(step_log$Y)
  )
}

#' Summarize an adaptive state.
#'
#' @details
#' Returns a compact run-level summary from canonical logs: attempted steps,
#' committed comparisons, refit count, and last stop decision/reason. This is a
#' pure view and does not recompute model quantities.
#'
#' @param state Adaptive state.
#'
#' @return A one-row tibble with columns \code{n_items},
#'   \code{steps_attempted}, \code{committed_pairs}, \code{n_refits},
#'   \code{last_stop_decision}, and \code{last_stop_reason}.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' summarize_adaptive(state)
#'
#' @seealso [adaptive_get_logs()], [base::print()]
#'
#' @family adaptive ranking
#' @export
summarize_adaptive <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  committed <- sum(!is.na(step_log$pair_id))

  last_stop_decision <- as.logical(state$meta$stop_decision %||% NA)
  last_stop_reason <- as.character(state$meta$stop_reason %||% NA_character_)

  tibble::tibble(
    n_items = as.integer(state$n_items),
    steps_attempted = as.integer(nrow(step_log)),
    committed_pairs = as.integer(committed),
    n_refits = as.integer(nrow(round_log)),
    last_stop_decision = as.logical(last_stop_decision),
    last_stop_reason = as.character(last_stop_reason)
  )
}

.adaptive_print_compact_values <- function(x) {
  vals <- unique(as.character(x))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) < 1L) {
    return(NA_character_)
  }
  paste(sort(vals), collapse = ",")
}

.adaptive_latest_link_stage_rows <- function(state) {
  link_stage_log <- tibble::as_tibble(state$link_stage_log %||% tibble::tibble())
  if (nrow(link_stage_log) < 1L) {
    return(link_stage_log)
  }
  if (!all(c("spoke_id", "refit_id") %in% names(link_stage_log))) {
    return(link_stage_log[0, , drop = FALSE])
  }
  ord <- order(
    as.integer(link_stage_log$spoke_id),
    as.integer(link_stage_log$refit_id),
    seq_len(nrow(link_stage_log))
  )
  link_stage_log <- link_stage_log[ord, , drop = FALSE]
  keep <- !duplicated(as.integer(link_stage_log$spoke_id), fromLast = TRUE)
  tibble::as_tibble(link_stage_log[keep, , drop = FALSE])
}

.adaptive_print_link_state_line <- function(state, phase_ctx) {
  latest_rows <- .adaptive_latest_link_stage_rows(state)
  if (nrow(latest_rows) < 1L) {
    return(character())
  }

  fit_methods <- .adaptive_print_compact_values(latest_rows$link_fit_method)
  uncertainty <- .adaptive_print_compact_values(latest_rows$link_uncertainty_approximation)
  probe_panel_id <- .adaptive_print_compact_values(latest_rows$probe_panel_id)
  probe_planned <- sum(as.integer(latest_rows$probe_edges_planned %||% 0L), na.rm = TRUE)
  probe_realized <- sum(as.integer(latest_rows$probe_edges_realized %||% 0L), na.rm = TRUE)
  gate_open <- sum(latest_rows$link_stop_gate_open %in% TRUE, na.rm = TRUE)
  lag_open <- sum(latest_rows$link_lag_eligible %in% TRUE, na.rm = TRUE)
  frozen <- sum(latest_rows$link_state_frozen %in% TRUE, na.rm = TRUE)
  estimation_mode <- .adaptive_print_compact_values(latest_rows$link_estimation_mode)
  init_method <- .adaptive_print_compact_values(latest_rows$anchored_joint_init_state_method)
  phase_a_hub_edges <- sum(as.integer(latest_rows$phase_a_within_edges_hub_used %||% 0L), na.rm = TRUE)
  phase_a_spoke_edges <- sum(as.integer(latest_rows$phase_a_within_edges_spoke_used %||% 0L), na.rm = TRUE)
  phase_b_active_edges <- sum(as.integer(latest_rows$phase_b_active_edges_used %||% 0L), na.rm = TRUE)

  blocker_codes <- unique(as.character(latest_rows$stop_blocker_codes %||% character()))
  blocker_codes <- blocker_codes[!is.na(blocker_codes) & nzchar(blocker_codes)]
  blockers <- unique(unlist(strsplit(blocker_codes, "[,|]", fixed = FALSE), use.names = FALSE))
  blockers <- blockers[!is.na(blockers) & nzchar(blockers)]

  details <- c(
    if (!is.na(fit_methods) && nzchar(fit_methods)) {
      paste0("fit_method=", fit_methods)
    },
    if (!is.na(uncertainty) && nzchar(uncertainty)) {
      paste0("uncertainty=", uncertainty)
    },
    if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
      paste0("mode=", estimation_mode)
    },
    if (!is.na(init_method) && nzchar(init_method)) {
      paste0("init_state=", init_method)
    },
    if (!is.na(probe_panel_id) && nzchar(probe_panel_id)) {
      paste0("probe_panel_id=", probe_panel_id)
    },
    if (isTRUE(any(as.character(latest_rows$link_estimation_mode) == "anchored_joint", na.rm = TRUE))) {
      paste0(
        "evidence_edges=",
        phase_a_hub_edges,
        "+",
        phase_a_spoke_edges,
        "+",
        phase_b_active_edges
      )
    },
    paste0("probe_edges=", probe_realized, "/", probe_planned),
    paste0("lag_open=", lag_open, "/", nrow(latest_rows)),
    paste0("stop_gate_open=", gate_open, "/", nrow(latest_rows)),
    if (length(phase_ctx$stopped_spokes) > 0L) {
      paste0("stopped_spokes=", paste(phase_ctx$stopped_spokes, collapse = ","))
    },
    if (frozen > 0L) {
      paste0("link_state_frozen=", frozen, "/", nrow(latest_rows))
    },
    if (length(blockers) > 0L) {
      paste0("stop_blockers=", paste(sort(blockers), collapse = ","))
    }
  )

  if (length(details) < 1L) {
    return(character())
  }
  paste0("link review: ", paste(details, collapse = "; "))
}

.adaptive_print_link_phase_line <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(character())
  }

  estimation_mode <- as.character(controller$link_estimation_mode %||% NA_character_)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  policy <- as.character(controller$link_transform_policy %||% NA_character_)
  state_map <- controller$link_transform_state_by_spoke %||% list()
  state_vals <- .adaptive_print_compact_values(state_map)
  epoch_map <- controller$link_epoch_id_by_spoke %||% list()
  frozen_map <- controller$link_state_frozen_by_spoke %||% list()
  frozen_spokes <- as.integer(names(frozen_map)[vapply(frozen_map, isTRUE, logical(1L))])
  frozen_spokes <- frozen_spokes[is.finite(frozen_spokes)]

  phase_line <- paste0("linking: ", phase_ctx$phase, " (run_mode=", run_mode, ")")
  details <- c(
    if (is.finite(phase_ctx$active_phase_a_set)) {
      paste0("phase_a_set=", phase_ctx$active_phase_a_set)
    },
    if (length(phase_ctx$ready_spokes) > 0L) {
      paste0("ready_spokes=", paste(phase_ctx$ready_spokes, collapse = ","))
    },
    if (length(phase_ctx$stopped_spokes) > 0L) {
      paste0("stopped_spokes=", paste(phase_ctx$stopped_spokes, collapse = ","))
    },
    if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
      paste0("estimation_mode=", estimation_mode)
    },
    if (!is.na(policy) && nzchar(policy)) {
      paste0("transform_policy=", policy)
    },
    if (!is.na(state_vals) && nzchar(state_vals)) {
      paste0("transform_state=", state_vals)
    },
    if (length(epoch_map) > 0L) {
      paste0("link_epoch=", .adaptive_print_compact_values(epoch_map))
    },
    if (length(frozen_spokes) > 0L) {
      paste0("frozen_spokes=", paste(sort(unique(frozen_spokes)), collapse = ","))
    }
  )
  if (length(details) > 0L) {
    phase_line <- paste0(phase_line, " [", paste(details, collapse = "; "), "]")
  }
  c(
    phase_line,
    .adaptive_print_link_state_line(
      state = state,
      phase_ctx = phase_ctx
    )
  )
}

#' Print an adaptive state summary.
#'
#' @description
#' S3 method for printing \code{adaptive_state} objects.
#'
#' @param x An \code{adaptive_state} object.
#' @param ... Unused.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' print(state)
#'
#' @seealso [summarize_adaptive()]
#'
#' @export
print.adaptive_state <- function(x, ...) {
  summary <- summarize_adaptive(x)
  header <- "Adaptive state"
  lines <- c(
    header,
    paste0("items: ", summary$n_items),
    paste0("steps: ", summary$steps_attempted, " (committed=", summary$committed_pairs, ")"),
    paste0("refits: ", summary$n_refits)
  )
  lines <- c(lines, .adaptive_print_link_phase_line(x))

  if (!is.na(summary$last_stop_decision)) {
    decision <- if (isTRUE(summary$last_stop_decision)) "stop" else "continue"
    reason <- summary$last_stop_reason %||% NA_character_
    suffix <- if (!is.na(reason) && reason != "") paste0(" (", reason, ")") else ""
    lines <- c(lines, paste0("last stop: ", decision, suffix))
  }

  cat(paste(lines, collapse = "\n"))
  invisible(x)
}

.adaptive_progress_config <- function(progress, progress_redraw_every, progress_show_events, progress_errors) {
  progress <- match.arg(progress, c("all", "refits", "steps", "none"))
  redraw <- as.integer(progress_redraw_every)
  if (is.na(redraw) || redraw < 1L) {
    rlang::abort("`progress_redraw_every` must be a positive integer.")
  }
  show_events <- isTRUE(progress_show_events) && progress %in% c("all", "steps")
  show_errors <- isTRUE(progress_errors) && progress %in% c("all", "steps")
  list(
    progress = progress,
    progress_redraw_every = redraw,
    progress_show_events = show_events,
    progress_errors = show_errors,
    refit_pairs_target = NA_integer_,
    stop_thresholds = list()
  )
}

.adaptive_progress_metrics <- function(state, refit_pairs_target) {
  step_log <- adaptive_step_log(state)
  last_step <- state$refit_meta$last_refit_step %||% 0L

  subset <- step_log[step_log$step_id > last_step, , drop = FALSE]
  committed <- sum(!is.na(step_log$pair_id))
  new_pairs <- sum(!is.na(subset$pair_id))
  starved <- sum(subset$candidate_starved %in% TRUE, na.rm = TRUE)
  invalid <- sum(subset$status == "invalid", na.rm = TRUE)
  fallback_rate <- if (nrow(subset) > 0L) {
    mean(!subset$fallback_used %in% c("base", "warm_start"), na.rm = TRUE)
  } else {
    NA_real_
  }

  list(
    steps_attempted = as.integer(nrow(step_log)),
    committed_pairs_done = as.integer(committed),
    new_pairs_since_last_refit = as.integer(new_pairs),
    refit_pairs_target = as.integer(refit_pairs_target),
    n_starved_since_last_refit = as.integer(starved),
    n_invalid_since_last_refit = as.integer(invalid),
    fallback_rate_since_last_refit = as.double(fallback_rate)
  )
}

.adaptive_meets_threshold <- function(value, threshold, direction = c("ge", "le")) {
  direction <- match.arg(direction)
  if (!is.finite(value) || !is.finite(threshold)) {
    return(FALSE)
  }
  if (identical(direction, "ge")) {
    return(value >= threshold)
  }
  value <= threshold
}

.adaptive_progress_col_value <- function(tbl, col, idx = 1L, default = NA) {
  if (!col %in% names(tbl)) {
    return(default)
  }
  tbl[[col]][[idx]] %||% default
}

.adaptive_progress_fmt_num <- function(x, digits = 3L, inactive = "inactive") {
  if (!is.finite(x)) {
    return(inactive)
  }
  formatC(as.double(x), digits = digits, format = "f")
}

.adaptive_progress_fmt_state <- function(x, true = "pass", false = "fail", inactive = "inactive") {
  if (isTRUE(x)) {
    return(true)
  }
  if (identical(x, FALSE)) {
    return(false)
  }
  inactive
}

.adaptive_progress_indent <- function(lines, spaces = 2L) {
  if (length(lines) < 1L) {
    return(character())
  }
  paste0(strrep(" ", as.integer(spaces %||% 0L)), lines)
}

.adaptive_progress_gate_detail <- function(label,
                                           value,
                                           threshold,
                                           pass,
                                           direction = c("ge", "le"),
                                           digits = 3L) {
  direction <- match.arg(direction)
  value_txt <- .adaptive_progress_fmt_num(value, digits = digits, inactive = "inactive")
  threshold_txt <- .adaptive_progress_fmt_num(threshold, digits = digits, inactive = "NA")
  relation <- if (identical(direction, "ge")) ">=" else "<="
  paste0(
    label,
    "=",
    value_txt,
    "/",
    threshold_txt,
    " ",
    .adaptive_progress_fmt_state(pass),
    " (",
    relation,
    ")"
  )
}

.adaptive_progress_link_diag_pass <- function(link_row) {
  isTRUE(.adaptive_progress_col_value(link_row, "link_diagnostics_divergences_pass", default = NA)) &&
    isTRUE(.adaptive_progress_col_value(link_row, "link_diagnostics_rhat_pass", default = NA)) &&
    isTRUE(.adaptive_progress_col_value(link_row, "link_diagnostics_ess_pass", default = NA))
}

.adaptive_progress_selection_notes <- function(row, link_stage_rows) {
  notes <- character()

  fallback_rate <- as.double(.adaptive_progress_col_value(
    row,
    "fallback_rate_since_last_refit",
    default = NA_real_
  ))
  fallback_mode <- as.character(.adaptive_progress_col_value(
    row,
    "fallback_used_mode",
    default = NA_character_
  ))
  if (is.finite(fallback_rate) && fallback_rate > 0) {
    if (!is.na(fallback_mode) && nzchar(fallback_mode) && !fallback_mode %in% c("base", "warm_start")) {
      notes <- c(
        notes,
        paste0(
          "fallback=",
          fallback_mode,
          " (rate=",
          .adaptive_progress_fmt_num(fallback_rate, digits = 2L, inactive = "0.00"),
          ")"
        )
      )
    } else {
      notes <- c(
        notes,
        paste0(
          "fallback_rate=",
          .adaptive_progress_fmt_num(fallback_rate, digits = 2L, inactive = "0.00")
        )
      )
    }
  }

  starve_rate <- as.double(.adaptive_progress_col_value(
    row,
    "starve_rate_since_last_refit",
    default = NA_real_
  ))
  starvation_reason <- as.character(.adaptive_progress_col_value(
    row,
    "starvation_reason_mode",
    default = NA_character_
  ))
  if (is.finite(starve_rate) && starve_rate > 0) {
    note <- paste0(
      "candidate_starved=",
      .adaptive_progress_fmt_num(starve_rate, digits = 2L, inactive = "0.00")
    )
    if (!is.na(starvation_reason) && nzchar(starvation_reason)) {
      note <- paste0(note, " (", starvation_reason, ")")
    }
    notes <- c(notes, note)
  }

  if (nrow(link_stage_rows) > 0L) {
    budget_shortfall <- sum(as.integer(
      if ("stage_budget_unfilled" %in% names(link_stage_rows)) {
        link_stage_rows$stage_budget_unfilled
      } else {
        0L
      }
    ), na.rm = TRUE)
    if (budget_shortfall > 0L) {
      notes <- c(notes, paste0("budget_shortfall=", as.integer(budget_shortfall)))
    }

    probe_shortfall <- sum(as.integer(
      if ("probe_panel_shortfall" %in% names(link_stage_rows)) {
        link_stage_rows$probe_panel_shortfall
      } else {
        0L
      }
    ), na.rm = TRUE)
    if (probe_shortfall > 0L) {
      probe_reasons <- if ("probe_shortfall_reason" %in% names(link_stage_rows)) {
        reasons <- as.character(link_stage_rows$probe_shortfall_reason)
        reasons <- reasons[!is.na(reasons) & nzchar(reasons) & reasons != "none"]
        .adaptive_print_compact_values(reasons)
      } else {
        NA_character_
      }
      note <- paste0("probe_shortfall=", as.integer(probe_shortfall))
      if (!is.na(probe_reasons) && nzchar(probe_reasons)) {
        note <- paste0(note, " (", probe_reasons, ")")
      }
      notes <- c(notes, note)
    }
  }

  notes
}

.adaptive_progress_diagnostics_lines <- function(row, link_stage_rows) {
  diagnostics <- character()

  global_pass <- isTRUE(.adaptive_progress_col_value(row, "diagnostics_pass", default = NA))
  if (!global_pass) {
    diagnostics <- c(
      diagnostics,
      paste0(
        "Diagnostics: global divergences=",
        .adaptive_progress_col_value(row, "divergences", default = NA_integer_),
        "/",
        .adaptive_progress_col_value(row, "divergences_max_allowed", default = NA_integer_),
        " ",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(row, "diagnostics_divergences_pass", default = NA)
        ),
        "  max_rhat=",
        .adaptive_progress_fmt_num(
          .adaptive_progress_col_value(row, "max_rhat", default = NA_real_),
          digits = 3L
        ),
        "/",
        .adaptive_progress_fmt_num(
          .adaptive_progress_col_value(row, "max_rhat_allowed", default = NA_real_),
          digits = 3L,
          inactive = "NA"
        ),
        " ",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(row, "diagnostics_rhat_pass", default = NA)
        ),
        "  min_ess_bulk=",
        .adaptive_progress_fmt_num(
          .adaptive_progress_col_value(row, "min_ess_bulk", default = NA_real_),
          digits = 0L
        ),
        "/",
        .adaptive_progress_fmt_num(
          .adaptive_progress_col_value(row, "ess_bulk_required", default = NA_real_),
          digits = 0L,
          inactive = "NA"
        ),
        " ",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(row, "diagnostics_ess_pass", default = NA)
        )
      )
    )
  }

  if (nrow(link_stage_rows) > 0L) {
    bad_idx <- which(!vapply(
      seq_len(nrow(link_stage_rows)),
      function(idx) .adaptive_progress_link_diag_pass(link_stage_rows[idx, , drop = FALSE]),
      logical(1L)
    ))
    if (length(bad_idx) > 0L) {
      for (idx in bad_idx) {
        link_row <- link_stage_rows[idx, , drop = FALSE]
        diagnostics <- c(
          diagnostics,
          paste0(
            "Diagnostics: spoke=",
            as.integer(.adaptive_progress_col_value(link_row, "spoke_id", default = NA_integer_)),
            " link divergences=",
            .adaptive_progress_col_value(link_row, "link_diagnostics_divergences", default = NA_integer_),
            " ",
            .adaptive_progress_fmt_state(
              .adaptive_progress_col_value(link_row, "link_diagnostics_divergences_pass", default = NA)
            ),
            "  max_rhat=",
            .adaptive_progress_fmt_num(
              .adaptive_progress_col_value(link_row, "link_diagnostics_max_rhat", default = NA_real_),
              digits = 3L
            ),
            " ",
            .adaptive_progress_fmt_state(
              .adaptive_progress_col_value(link_row, "link_diagnostics_rhat_pass", default = NA)
            ),
            "  min_ess_bulk=",
            .adaptive_progress_fmt_num(
              .adaptive_progress_col_value(link_row, "link_diagnostics_min_ess_bulk", default = NA_real_),
              digits = 0L
            ),
            " ",
            .adaptive_progress_fmt_state(
              .adaptive_progress_col_value(link_row, "link_diagnostics_ess_pass", default = NA)
            )
          )
        )
      }
    }
  }

  diagnostics
}

.adaptive_progress_phase_a_blocker <- function(row, use_scope_metrics, values) {
  if (!isTRUE(values$diagnostics_pass)) {
    return("diagnostics_pass")
  }
  if (!isTRUE(values$eap_pass)) {
    return(values$reliability_label)
  }
  if (!isTRUE(values$lag_eligible)) {
    return(if (isTRUE(use_scope_metrics)) "lag_eligible_scope" else "lag_eligible")
  }
  if (!isTRUE(values$theta_pass)) {
    return(values$theta_label)
  }
  if (!isTRUE(values$delta_pass)) {
    return(values$delta_label)
  }
  if (!isTRUE(values$rank_pass)) {
    return(values$rank_label)
  }
  if (!isTRUE(.adaptive_progress_col_value(row, "stop_decision", default = NA))) {
    return("stop_pending")
  }
  NA_character_
}

.adaptive_progress_phase_a_lines <- function(row, thresholds) {
  phase_scope <- as.character(.adaptive_progress_col_value(row, "phase_scope", default = "global"))
  use_scope_metrics <- identical(phase_scope, "phase_a_set")
  scope_set_id <- as.integer(.adaptive_progress_col_value(row, "phase_scope_set_id", default = NA_integer_))

  reliability_label <- if (isTRUE(use_scope_metrics)) "reliability_EAP_scope" else "reliability_EAP"
  theta_label <- if (isTRUE(use_scope_metrics)) "rho_theta_scope" else "rho_theta"
  delta_label <- if (isTRUE(use_scope_metrics)) "delta_sd_theta_scope" else "delta_sd_theta"
  rank_label <- if (isTRUE(use_scope_metrics)) "rho_rank_scope" else "rho_rank"

  reliability_value <- as.double(.adaptive_progress_col_value(row, reliability_label, default = NA_real_))
  theta_value <- as.double(.adaptive_progress_col_value(row, theta_label, default = NA_real_))
  delta_value <- as.double(.adaptive_progress_col_value(row, delta_label, default = NA_real_))
  rank_value <- as.double(.adaptive_progress_col_value(row, rank_label, default = NA_real_))

  reliability_min <- as.double(.adaptive_progress_col_value(
    row,
    "eap_reliability_min",
    default = thresholds$eap_reliability_min %||% NA_real_
  ))
  theta_min <- as.double(.adaptive_progress_col_value(
    row,
    "theta_corr_min",
    default = thresholds$theta_corr_min %||% NA_real_
  ))
  delta_max <- as.double(.adaptive_progress_col_value(
    row,
    "theta_sd_rel_change_max",
    default = thresholds$theta_sd_rel_change_max %||% NA_real_
  ))
  rank_min <- as.double(.adaptive_progress_col_value(
    row,
    "rank_spearman_min",
    default = thresholds$rank_spearman_min %||% NA_real_
  ))

  diagnostics_pass <- isTRUE(.adaptive_progress_col_value(row, "diagnostics_pass", default = NA))
  eap_pass_col <- if (isTRUE(use_scope_metrics)) "eap_pass_scope" else "eap_pass"
  theta_pass_col <- if (isTRUE(use_scope_metrics)) "theta_corr_pass_scope" else "theta_corr_pass"
  delta_pass_col <- if (isTRUE(use_scope_metrics)) "delta_sd_theta_pass_scope" else "delta_sd_theta_pass"
  rank_pass_col <- if (isTRUE(use_scope_metrics)) "rho_rank_pass_scope" else "rho_rank_pass"
  lag_col <- if (isTRUE(use_scope_metrics)) "lag_eligible_scope" else "lag_eligible"

  lag_eligible <- isTRUE(.adaptive_progress_col_value(row, lag_col, default = NA))
  eap_pass <- .adaptive_progress_col_value(row, eap_pass_col, default = NA)
  theta_pass <- .adaptive_progress_col_value(row, theta_pass_col, default = NA)
  delta_pass <- .adaptive_progress_col_value(row, delta_pass_col, default = NA)
  rank_pass <- .adaptive_progress_col_value(row, rank_pass_col, default = NA)

  if (!is.logical(eap_pass) || is.na(eap_pass)) {
    eap_pass <- .adaptive_meets_threshold(reliability_value, reliability_min, "ge")
  }
  if (!is.logical(theta_pass) || is.na(theta_pass)) {
    theta_pass <- .adaptive_meets_threshold(theta_value, theta_min, "ge")
  }
  if (!is.logical(delta_pass) || is.na(delta_pass)) {
    delta_pass <- .adaptive_meets_threshold(delta_value, delta_max, "le")
  }
  if (!is.logical(rank_pass) || is.na(rank_pass)) {
    rank_pass <- .adaptive_meets_threshold(rank_value, rank_min, "ge")
  }

  refit_line <- paste0(
    "Refit ",
    sprintf("%04d", as.integer(.adaptive_progress_col_value(row, "refit_id", default = NA_integer_))),
    "  step=",
    .adaptive_progress_col_value(row, "step_id_at_refit", default = NA_integer_),
    if (isTRUE(use_scope_metrics) && is.finite(scope_set_id)) {
      paste0("  phase_scope=phase_a_set(set_id=", scope_set_id, ")")
    } else {
      ""
    }
  )
  pairs_line <- paste0(
    "Pairs: new=",
    .adaptive_progress_col_value(row, "new_pairs_since_last_refit", default = NA_integer_),
    "  committed_pairs=",
    .adaptive_progress_col_value(row, "total_pairs_done", default = NA_integer_)
  )

  global_parts <- c(
    paste0("diagnostics=", .adaptive_progress_fmt_state(diagnostics_pass)),
    .adaptive_progress_gate_detail(
      reliability_label,
      reliability_value,
      reliability_min,
      eap_pass,
      direction = "ge"
    )
  )
  if (isTRUE(lag_eligible)) {
    global_parts <- c(
      global_parts,
      .adaptive_progress_gate_detail(theta_label, theta_value, theta_min, theta_pass, "ge"),
      .adaptive_progress_gate_detail(delta_label, delta_value, delta_max, delta_pass, "le"),
      .adaptive_progress_gate_detail(rank_label, rank_value, rank_min, rank_pass, "ge")
    )
  } else {
    global_parts <- c(global_parts, "lagged=inactive")
  }
  global_parts <- c(
    global_parts,
    paste0(
      "stop=",
      .adaptive_progress_fmt_state(
        .adaptive_progress_col_value(row, "stop_decision", default = NA),
        true = "pass",
        false = "continue"
      )
    )
  )

  blocker <- .adaptive_progress_phase_a_blocker(
    row = row,
    use_scope_metrics = use_scope_metrics,
    values = list(
      diagnostics_pass = diagnostics_pass,
      eap_pass = eap_pass,
      lag_eligible = lag_eligible,
      theta_pass = theta_pass,
      delta_pass = delta_pass,
      rank_pass = rank_pass,
      reliability_label = reliability_label,
      theta_label = theta_label,
      delta_label = delta_label,
      rank_label = rank_label
    )
  )

  lines <- c(
    refit_line,
    pairs_line,
    "Global stop:",
    .adaptive_progress_indent(global_parts, spaces = 2L)
  )
  if (!is.na(blocker) && blocker != "stop_pending") {
    lines <- c(lines, paste0("Blocker: ", blocker))
  }
  lines
}

.adaptive_progress_phase_b_spoke_lines <- function(link_stage_rows,
                                                   thresholds,
                                                   stability_window_refits,
                                                   stability_passes_required) {
  lines <- "Spokes:"
  if (nrow(link_stage_rows) < 1L) {
    return(c(lines, "  none"))
  }

  rows <- link_stage_rows[order(as.integer(link_stage_rows$spoke_id)), , drop = FALSE]
  for (idx in seq_len(nrow(rows))) {
    link_row <- rows[idx, , drop = FALSE]
    spoke_id <- as.integer(.adaptive_progress_col_value(link_row, "spoke_id", default = NA_integer_))
    estimation_mode <- as.character(.adaptive_progress_col_value(
      link_row,
      "link_estimation_mode",
      default = NA_character_
    ))
    transform_state <- as.character(.adaptive_progress_col_value(
      link_row,
      "link_transform_state",
      default = NA_character_
    ))
    init_method <- as.character(.adaptive_progress_col_value(
      link_row,
      "anchored_joint_init_state_method",
      default = NA_character_
    ))

    if (isTRUE(.adaptive_progress_col_value(link_row, "link_state_frozen", default = NA))) {
      frozen_refit <- .adaptive_progress_col_value(
        link_row,
        "link_state_frozen_refit_id",
        default = NA_integer_
      )
      lines <- c(
        lines,
        paste0(
          "  spoke=",
          spoke_id,
          " frozen",
          if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
            paste0("  mode=", estimation_mode)
          } else {
            ""
          },
          if (!is.na(transform_state) && nzchar(transform_state)) {
            paste0("  state=", transform_state)
          } else {
            ""
          },
          if (is.finite(frozen_refit)) {
            paste0("  frozen_refit=", frozen_refit)
          } else {
            ""
          }
        ),
        paste0("  spoke=", spoke_id, " frozen"),
        if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
          paste0("    mode=", estimation_mode)
        } else {
          character()
        },
        if (!is.na(transform_state) && nzchar(transform_state)) {
          paste0("    state=", transform_state)
        } else {
          character()
        },
        if (!is.na(init_method) && nzchar(init_method)) {
          paste0("    init_state=", init_method)
        } else {
          character()
        },
        if (is.finite(frozen_refit)) {
          paste0("    frozen_refit=", frozen_refit)
        } else {
          character()
        }
      )
      next
    }

    stop_count <- as.integer(.adaptive_progress_col_value(
      link_row,
      "stop_recent_pass_count",
      default = 0L
    ))
    stop_window_size <- as.integer(.adaptive_progress_col_value(
      link_row,
      "stop_recent_window_size",
      default = 0L
    ))
    stop_window_used <- as.integer(.adaptive_progress_col_value(
      link_row,
      "stability_window_refits_used",
      default = stability_window_refits
    ))
    stop_passes_used <- as.integer(.adaptive_progress_col_value(
      link_row,
      "stability_passes_required_used",
      default = stability_passes_required
    ))
    probes_realized <- as.integer(.adaptive_progress_col_value(
      link_row,
      "probe_edges_realized",
      default = 0L
    ))
    probes_min <- as.integer(.adaptive_progress_col_value(
      link_row,
      "probe_edges_min_for_stop_used",
      default = NA_integer_
    ))

    reliability_min <- as.double(.adaptive_progress_col_value(
      link_row,
      "link_stop_reliability_min_used",
      default = thresholds$link_stop_reliability_min %||% 0.90
    ))
    probe_pred_max <- as.double(.adaptive_progress_col_value(
      link_row,
      "probe_pred_rmse_max_used",
      default = thresholds$probe_pred_rmse_max %||% 0.015
    ))
    theta_rmse_max <- as.double(.adaptive_progress_col_value(
      link_row,
      "theta_global_rmse_max_used",
      default = thresholds$theta_global_rmse_max %||% 0.05
    ))

    lines <- c(
      lines,
      paste0(
        "  spoke=",
        spoke_id,
        " active",
        if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
          paste0("  mode=", estimation_mode)
        } else {
          ""
        },
        "  eligible=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_stop_eligible", default = NA),
          true = "yes",
          false = "no"
        ),
        "  gate_open=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_stop_gate_open", default = NA),
          true = "yes",
          false = "no"
        ),
        "  probes=",
        probes_realized,
        "/",
        if (is.na(probes_min)) "NA" else probes_min
      ),
      paste0("  spoke=", spoke_id, " active"),
      if (!is.na(estimation_mode) && nzchar(estimation_mode)) {
        paste0("    mode=", estimation_mode)
      } else {
        character()
      },
      if (!is.na(transform_state) && nzchar(transform_state)) {
        paste0("    state=", transform_state)
      } else {
        character()
      },
      if (!is.na(init_method) && nzchar(init_method)) {
        paste0("    init_state=", init_method)
      } else {
        character()
      },
      paste0(
        "    stop_eligible=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_stop_eligible", default = NA),
          true = "yes",
          false = "no"
        )
      ),
      paste0(
        "    stop_gate_open=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_stop_gate_open", default = NA),
          true = "yes",
          false = "no"
        )
      ),
      paste0(
        "    lag=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_lag_eligible", default = NA)
        )
      ),
      paste0(
        "    min_refit=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "link_min_refit_eligible", default = NA)
        )
      ),
      paste0(
        "    probes=",
        probes_realized,
        "/",
        if (is.na(probes_min)) "NA" else probes_min
      ),
      paste0(
        "    stop_window=",
        stop_count,
        "/",
        stop_window_size,
        " need ",
        stop_passes_used,
        "/",
        stop_window_used
      ),
      paste0(
        "    diagnostics=",
        .adaptive_progress_fmt_state(.adaptive_progress_link_diag_pass(link_row))
      ),
      paste0(
        "    hub_anchored=",
        .adaptive_progress_fmt_state(
          .adaptive_progress_col_value(link_row, "hub_anchored", default = NA)
        )
      ),
      paste0(
        "    ",
        .adaptive_progress_gate_detail(
          "reliability_link_global",
          as.double(.adaptive_progress_col_value(link_row, "reliability_link_global", default = NA_real_)),
          reliability_min,
          .adaptive_progress_col_value(link_row, "reliability_stop_pass", default = NA),
          direction = "ge"
        )
      ),
      paste0(
        "    ",
        .adaptive_progress_gate_detail(
          "probe_pred_rmse_lagged",
          as.double(.adaptive_progress_col_value(link_row, "probe_pred_rmse_lagged", default = NA_real_)),
          probe_pred_max,
          .adaptive_progress_col_value(link_row, "probe_pred_rmse_pass", default = NA),
          direction = "le"
        )
      ),
      paste0(
        "    ",
        .adaptive_progress_gate_detail(
          "theta_global_rmse_lagged",
          as.double(.adaptive_progress_col_value(link_row, "theta_global_rmse_lagged", default = NA_real_)),
          theta_rmse_max,
          .adaptive_progress_col_value(link_row, "theta_global_rmse_pass", default = NA),
          direction = "le"
        )
      )
    )
  }

  lines
}

.adaptive_progress_phase_b_lines <- function(row, link_stage_rows, thresholds) {
  stability_window_refits <- as.integer(thresholds$stability_window_refits %||% 3L)
  stability_passes_required <- as.integer(
    thresholds$stability_passes_required %||%
      thresholds$stability_consecutive_k %||%
      2L
  )

  refit_line <- paste0(
    "Refit ",
    sprintf("%04d", as.integer(.adaptive_progress_col_value(row, "refit_id", default = NA_integer_))),
    "  round=",
    .adaptive_progress_col_value(row, "round_id_at_refit", default = NA_integer_),
    "  step=",
    .adaptive_progress_col_value(row, "step_id_at_refit", default = NA_integer_)
  )

  pairs_line <- paste0(
    "Pairs: new=",
    .adaptive_progress_col_value(row, "new_pairs_since_last_refit", default = NA_integer_),
    "  committed_pairs=",
    .adaptive_progress_col_value(row, "total_pairs_done", default = NA_integer_),
    "  active=",
    .adaptive_progress_col_value(row, "new_active_pairs_since_last_refit", default = 0L),
    "  probe=",
    .adaptive_progress_col_value(row, "new_probe_pairs_since_last_refit", default = 0L),
    "  total_cross=",
    .adaptive_progress_col_value(row, "new_total_cross_pairs_since_last_refit", default = 0L)
  )

  reliability_value <- as.double(.adaptive_progress_col_value(row, "reliability_EAP", default = NA_real_))
  reliability_min <- as.double(.adaptive_progress_col_value(
    row,
    "eap_reliability_min",
    default = thresholds$eap_reliability_min %||% NA_real_
  ))
  theta_value <- as.double(.adaptive_progress_col_value(row, "rho_theta", default = NA_real_))
  theta_min <- as.double(.adaptive_progress_col_value(
    row,
    "theta_corr_min",
    default = thresholds$theta_corr_min %||% NA_real_
  ))
  delta_value <- as.double(.adaptive_progress_col_value(row, "delta_sd_theta", default = NA_real_))
  delta_max <- as.double(.adaptive_progress_col_value(
    row,
    "theta_sd_rel_change_max",
    default = thresholds$theta_sd_rel_change_max %||% NA_real_
  ))
  rank_value <- as.double(.adaptive_progress_col_value(row, "rho_rank", default = NA_real_))
  rank_min <- as.double(.adaptive_progress_col_value(
    row,
    "rank_spearman_min",
    default = thresholds$rank_spearman_min %||% NA_real_
  ))
  lag_eligible <- isTRUE(.adaptive_progress_col_value(row, "lag_eligible", default = NA))

  global_parts <- c(
    "audit_only",
    paste0(
      "diagnostics=",
      .adaptive_progress_fmt_state(.adaptive_progress_col_value(row, "diagnostics_pass", default = NA))
    ),
    .adaptive_progress_gate_detail(
      "reliability_EAP",
      reliability_value,
      reliability_min,
      .adaptive_progress_col_value(row, "eap_pass", default = NA),
      direction = "ge"
    )
  )
  if (isTRUE(lag_eligible)) {
    global_parts <- c(
      global_parts,
      .adaptive_progress_gate_detail(
        "rho_theta",
        theta_value,
        theta_min,
        .adaptive_progress_col_value(row, "theta_corr_pass", default = NA),
        direction = "ge"
      ),
      .adaptive_progress_gate_detail(
        "delta_sd_theta",
        delta_value,
        delta_max,
        .adaptive_progress_col_value(row, "delta_sd_theta_pass", default = NA),
        direction = "le"
      ),
      .adaptive_progress_gate_detail(
        "rho_rank",
        rank_value,
        rank_min,
        .adaptive_progress_col_value(row, "rho_rank_pass", default = NA),
        direction = "ge"
      )
    )
  } else {
    global_parts <- c(global_parts, "lagged=inactive")
  }
  global_parts <- c(
    global_parts,
    paste0(
      "global_btl_stop=",
      .adaptive_progress_fmt_state(
        .adaptive_progress_col_value(row, "stop_decision", default = NA),
        true = "pass",
        false = "continue"
      )
    )
  )

  c(
    refit_line,
    pairs_line,
    "Global:",
    .adaptive_progress_indent(global_parts, spaces = 2L),
    .adaptive_progress_phase_b_spoke_lines(
      link_stage_rows = link_stage_rows,
      thresholds = thresholds,
      stability_window_refits = stability_window_refits,
      stability_passes_required = stability_passes_required
    )
  )
}

adaptive_progress_init <- function(state, cfg) {
  if (cfg$progress %in% c("none", "refits")) {
    return(NULL)
  }
  total <- as.integer(cfg$refit_pairs_target %||% NA_integer_)
  if (!is.finite(total) || total < 1L) {
    total <- as.integer(adaptive_defaults(state$n_items)$refit_pairs_target)
  }
  id <- cli::cli_progress_bar(
    total = total,
    format = "Adaptive {cli::pb_bar} {current}/{total} pairs {extra}",
    .envir = rlang::caller_env()
  )
  list(id = id, last_redraw = 0L, total = total)
}

adaptive_progress_update <- function(handle, state, cfg) {
  if (is.null(handle)) {
    return(handle)
  }
  if (!cfg$progress %in% c("all", "steps")) {
    return(handle)
  }
  step_id <- nrow(state$step_log)
  if ((step_id - handle$last_redraw) < cfg$progress_redraw_every) {
    return(handle)
  }
  metrics <- .adaptive_progress_metrics(state, cfg$refit_pairs_target)
  label <- paste0(
    "to next refit (steps=",
    metrics$steps_attempted,
    " committed=",
    metrics$committed_pairs_done,
    " starved=",
    metrics$n_starved_since_last_refit,
    " invalid=",
    metrics$n_invalid_since_last_refit,
    if (is.finite(metrics$fallback_rate_since_last_refit)) {
      paste0(" fallback_rate=", sprintf("%.2f", metrics$fallback_rate_since_last_refit))
    } else {
      ""
    },
    ")"
  )

  tryCatch(
    cli::cli_progress_update(
      handle$id,
      set = metrics$new_pairs_since_last_refit,
      extra = label
    ),
    error = function(e) NULL
  )
  if (isTRUE(cfg$progress_show_events)) {
    cli::cli_inform(paste0(
      "step ",
      step_id,
      ": new_pairs_since_last_refit=",
      metrics$new_pairs_since_last_refit,
      "/",
      handle$total,
      " committed=",
      metrics$committed_pairs_done,
      " invalid=",
      metrics$n_invalid_since_last_refit,
      " starved=",
      metrics$n_starved_since_last_refit
    ))
  }
  handle$last_redraw <- step_id
  handle
}

adaptive_progress_finish <- function(handle) {
  if (is.null(handle)) {
    return(invisible(NULL))
  }
  tryCatch(
    cli::cli_progress_done(handle$id),
    error = function(e) NULL
  )
  invisible(NULL)
}

adaptive_progress_step_event <- function(step_row, cfg) {
  if (!isTRUE(cfg$progress_show_events) || nrow(step_row) == 0L) {
    return(NULL)
  }
  step_id <- step_row$step_id[[1L]]
  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  stage_txt <- if (!is.na(stage) && stage != "") paste0(" stage=", stage) else ""
  run_mode <- if ("run_mode" %in% names(step_row)) {
    as.character(step_row$run_mode[[1L]] %||% NA_character_)
  } else {
    NA_character_
  }
  is_probe_step <- if ("is_probe_step" %in% names(step_row)) {
    isTRUE(step_row$is_probe_step[[1L]] %||% FALSE)
  } else {
    FALSE
  }
  link_txt <- character()
  if ("is_cross_set" %in% names(step_row) && isTRUE(step_row$is_cross_set[[1L]] %||% FALSE)) {
    spoke <- as.integer(step_row$link_spoke_id[[1L]] %||% NA_integer_)
    if (is.finite(spoke)) {
      link_txt <- c(link_txt, paste0("spoke=", spoke))
    }
    if (!(isTRUE(is_probe_step) || run_mode %in% c("link_probe_holdout", "link_probe"))) {
      link_txt <- c(link_txt, "link=active")
    }
    if ("link_transform_state" %in% names(step_row)) {
      mode <- as.character(step_row$link_transform_state[[1L]] %||% NA_character_)
      if (!is.na(mode) && nzchar(mode)) {
        link_txt <- c(link_txt, paste0("transform=", mode))
      }
    }
  }
  if (isTRUE(is_probe_step) || run_mode %in% c("link_probe_holdout", "link_probe")) {
    probe_label <- if (identical(run_mode, "link_probe_holdout") ||
      ("is_holdout_probe_step" %in% names(step_row) &&
        isTRUE(step_row$is_holdout_probe_step[[1L]] %||% FALSE))) {
      "holdout"
    } else if (identical(run_mode, "link_probe") ||
      ("is_drift_probe_step" %in% names(step_row) &&
        isTRUE(step_row$is_drift_probe_step[[1L]] %||% FALSE))) {
      "drift_followup"
    } else {
      "probe"
    }
    link_txt <- c(link_txt, paste0("probe=", probe_label))
  }
  link_txt <- if (length(link_txt) > 0L) {
    paste0(" ", paste(link_txt, collapse = " "))
  } else {
    ""
  }
  if (isTRUE(step_row$candidate_starved[[1L]])) {
    return(paste0("step ", step_id, ":", stage_txt, link_txt, " candidate_starved=TRUE; pair_id=NA"))
  }
  if (identical(step_row$status[[1L]], "invalid") && isTRUE(cfg$progress_errors)) {
    reason <- step_row$starvation_reason[[1L]]
    if (is.na(reason) || reason == "") {
      reason <- "invalid"
    }
    return(paste0(
      "step ", step_id, ":", stage_txt, link_txt,
      " invalid judge (", reason, "); pair_id=NA"
    ))
  }
  if (!is.na(step_row$fallback_used[[1L]]) &&
    !step_row$fallback_used[[1L]] %in% c("base", "warm_start")) {
    return(paste0(
      "step ", step_id, ":", stage_txt, link_txt,
      " fallback_used=", step_row$fallback_used[[1L]]
    ))
  }
  NULL
}

adaptive_progress_refit_block <- function(round_row, cfg, link_stage_rows = NULL) {
  if (nrow(round_row) == 0L) {
    return(character())
  }
  row <- round_row[1L, , drop = FALSE]
  link_stage_rows <- tibble::as_tibble(link_stage_rows %||% tibble::tibble())
  has_linking_rows <- nrow(link_stage_rows) > 0L
  thresholds <- cfg$stop_thresholds %||% list()
  lines <- if (isTRUE(has_linking_rows)) {
    .adaptive_progress_phase_b_lines(
      row = row,
      link_stage_rows = link_stage_rows,
      thresholds = thresholds
    )
  } else {
    .adaptive_progress_phase_a_lines(row = row, thresholds = thresholds)
  }

  selection_notes <- .adaptive_progress_selection_notes(row = row, link_stage_rows = link_stage_rows)
  if (length(selection_notes) > 0L) {
    lines <- c(lines, paste0("Selection: ", paste(selection_notes, collapse = "; ")))
  }

  diagnostics <- .adaptive_progress_diagnostics_lines(row = row, link_stage_rows = link_stage_rows)
  if (length(diagnostics) > 0L) {
    lines <- c(lines, diagnostics)
  }

  lines
}
