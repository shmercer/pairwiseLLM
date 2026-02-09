# -------------------------------------------------------------------------
# Adaptive Bayesian BTL refits (inference-only) + stopping rules.
# -------------------------------------------------------------------------

.adaptive_btl_defaults <- function(N) {
  N <- as.integer(N)
  if (is.na(N) || N < 2L) {
    rlang::abort("`N` must be a positive integer >= 2.")
  }

  ess_bulk_min <- max(400L, as.integer(round(20 * sqrt(N))))
  ess_bulk_min_near_stop <- max(1000L, as.integer(round(50 * sqrt(N))))

  list(
    model_variant = "btl_e_b",
    ess_bulk_min = as.double(ess_bulk_min),
    ess_bulk_min_near_stop = as.double(ess_bulk_min_near_stop),
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.90,
    stability_lag = 2L,
    theta_corr_min = 0.95,
    theta_sd_rel_change_max = 0.10,
    rank_spearman_min = 0.95,
    near_tie_p_low = 0.40,
    near_tie_p_high = 0.60,
    global_identified_reliability_min = 0.80,
    global_identified_rank_corr_min = 0.90
  )
}

.adaptive_btl_resolve_config <- function(state, config) {
  defaults <- .adaptive_btl_defaults(state$n_items)
  if (is.null(config)) {
    return(defaults)
  }
  if (!is.list(config)) {
    rlang::abort("`config` must be a list when provided.")
  }
  utils::modifyList(defaults, config)
}

.adaptive_refit_pairs_target <- function(state, config) {
  refit_pairs_target <- config$refit_pairs_target %||% .btl_mcmc_clamp(
    100L,
    5000L,
    as.integer(ceiling(state$n_items / 2))
  )
  as.integer(refit_pairs_target)
}

.adaptive_refit_eligibility <- function(total_committed, last_refit_committed, refit_pairs_target) {
  total_committed <- as.integer(total_committed %||% 0L)
  last_refit_committed <- as.integer(last_refit_committed %||% 0L)
  refit_pairs_target <- as.integer(refit_pairs_target %||% 0L)
  new_pairs_since_last_refit <- as.integer(total_committed - last_refit_committed)
  eligible <- new_pairs_since_last_refit >= refit_pairs_target
  list(
    eligible = isTRUE(eligible),
    new_pairs_since_last_refit = as.integer(new_pairs_since_last_refit)
  )
}

.adaptive_results_from_step_log <- function(state) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) == 0L) {
    return(tibble::tibble())
  }
  ok <- !is.na(step_log$pair_id)
  step_log <- step_log[ok, , drop = FALSE]
  if (nrow(step_log) == 0L) {
    return(tibble::tibble())
  }

  ids <- as.character(state$item_ids)
  A_id <- ids[step_log$A]
  B_id <- ids[step_log$B]
  winner_pos <- ifelse(step_log$Y == 1L, 1L, 2L)
  better_id <- ifelse(step_log$Y == 1L, A_id, B_id)

  tibble::tibble(
    pair_uid = paste0("pair_", step_log$pair_id),
    unordered_key = make_unordered_key(A_id, B_id),
    ordered_key = make_ordered_key(A_id, B_id),
    A_id = as.character(A_id),
    B_id = as.character(B_id),
    better_id = as.character(better_id),
    winner_pos = as.integer(winner_pos),
    phase = rep("phase2", nrow(step_log)),
    iter = as.integer(step_log$step_id),
    received_at = step_log$timestamp,
    backend = rep("adaptive", nrow(step_log)),
    model = rep("adaptive", nrow(step_log))
  )
}

.adaptive_btl_adapt_fit <- function(fit) {
  if (!is.list(fit)) {
    rlang::abort("`fit` must be a list.")
  }
  adaptive_fit <- fit
  if (!"btl_posterior_draws" %in% names(adaptive_fit)) {
    if (!"theta_draws" %in% names(adaptive_fit)) {
      rlang::abort("`fit` must include `btl_posterior_draws` or `theta_draws`.")
    }
    adaptive_fit$btl_posterior_draws <- adaptive_fit$theta_draws
  }
  if ("theta_draws" %in% names(adaptive_fit)) {
    adaptive_fit$theta_draws <- NULL
  }
  adaptive_fit
}

.adaptive_btl_extract_fit_contract <- function(fit_out) {
  fit_contract <- NULL
  if (is.list(fit_out) && "fit" %in% names(fit_out)) {
    fit_contract <- fit_out[["fit"]]
  }
  if (!is.null(fit_contract)) {
    return(fit_contract)
  }
  fits <- NULL
  if (is.list(fit_out) && "fits" %in% names(fit_out)) {
    fits <- fit_out[["fits"]]
  }
  if (is.list(fits) && length(fits) >= 1L) {
    return(fits[[length(fits)]])
  }
  fit_out
}

.adaptive_btl_fit_theta_mean <- function(fit) {
  theta_mean <- fit$theta_mean %||% NULL
  if (!is.null(theta_mean)) {
    theta_mean <- as.double(theta_mean)
    if (!is.null(names(fit$theta_mean))) {
      names(theta_mean) <- names(fit$theta_mean)
    }
  } else {
    draws <- fit$btl_posterior_draws %||% NULL
    if (!is.matrix(draws) || !is.numeric(draws)) {
      rlang::abort("`fit$btl_posterior_draws` must be a numeric matrix.")
    }
    theta_mean <- colMeans(draws)
  }
  theta_mean
}

.adaptive_ts_btl_rank_spearman <- function(state, theta_mean) {
  if (is.null(state$trueskill_state) ||
    is.null(state$trueskill_state$items) ||
    !is.data.frame(state$trueskill_state$items)) {
    return(NA_real_)
  }
  ids <- as.character(state$item_ids)
  theta_names <- names(theta_mean)
  theta_mean <- as.double(theta_mean)
  names(theta_mean) <- theta_names
  if (is.null(theta_names) || !all(ids %in% theta_names)) {
    return(NA_real_)
  }
  theta_vals <- theta_mean[ids]
  ts_ids <- as.character(state$trueskill_state$items$item_id)
  ts_mu <- as.double(state$trueskill_state$items$mu[match(ids, ts_ids)])
  if (any(!is.finite(theta_vals)) || any(!is.finite(ts_mu))) {
    return(NA_real_)
  }
  rank_theta <- rank(theta_vals, ties.method = "average")
  rank_mu <- rank(ts_mu, ties.method = "average")
  as.double(stats::cor(rank_mu, rank_theta, method = "spearman", use = "pairwise.complete.obs"))
}

.adaptive_update_identifiability_state <- function(state, config) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  if (!is.null(config$global_identified_reliability_min)) {
    controller$global_identified_reliability_min <- as.double(config$global_identified_reliability_min)
  }
  if (!is.null(config$global_identified_rank_corr_min)) {
    controller$global_identified_rank_corr_min <- as.double(config$global_identified_rank_corr_min)
  }

  draws <- out$btl_fit$btl_posterior_draws %||% NULL
  theta_mean <- .adaptive_btl_fit_theta_mean(out$btl_fit %||% list())
  reliability <- compute_reliability_EAP(draws)
  rho_rank <- .adaptive_ts_btl_rank_spearman(out, theta_mean)

  controller$reliability_EAP <- as.double(reliability)
  controller$ts_btl_rank_spearman <- as.double(rho_rank)
  controller$global_identified <- is.finite(reliability) &&
    is.finite(rho_rank) &&
    reliability >= as.double(controller$global_identified_reliability_min) &&
    rho_rank >= as.double(controller$global_identified_rank_corr_min)

  out$controller <- controller
  out
}

.adaptive_mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) {
    return(NA_character_)
  }
  tab <- table(x)
  names(tab)[[which.max(tab)]]
}

.adaptive_btl_refit_context <- function(state, last_refit_M_done, last_refit_step) {
  step_id_at_refit <- as.integer(nrow(state$step_log))
  list(
    step_id_at_refit = step_id_at_refit,
    timestamp = (state$meta$now_fn %||% function() Sys.time())(),
    last_refit_M_done = as.integer(last_refit_M_done),
    last_refit_step = as.integer(last_refit_step)
  )
}

.adaptive_round_log_row <- function(state, metrics, stop_decision, stop_reason, refit_context, config) {
  ids <- as.character(state$item_ids)
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, ids)

  deg_vals <- as.double(counts$deg[ids])
  mean_degree <- if (length(deg_vals) > 0L) mean(deg_vals) else NA_real_
  min_degree <- if (length(deg_vals) > 0L) min(deg_vals) else NA_integer_
  pos_balance <- as.double(counts$posA[ids] - counts$posB[ids])
  pos_balance_sd <- if (length(pos_balance) > 1L) stats::sd(pos_balance) else 0

  n_unique_pairs_seen <- sum(counts$pair_count >= 1L)
  total_pairs_done <- nrow(history)
  new_pairs_since_last_refit <- total_pairs_done - refit_context$last_refit_M_done

  last_step <- refit_context$last_refit_step
  step_id_at_refit <- refit_context$step_id_at_refit
  step_log <- tibble::as_tibble(state$step_log)
  step_subset <- step_log[step_log$step_id > last_step &
    step_log$step_id <= step_id_at_refit, , drop = FALSE]

  proposed_pairs <- step_subset$n_candidates_scored
  proposed_pairs_mode <- if (length(proposed_pairs) > 0L && any(!is.na(proposed_pairs))) {
    stats::median(proposed_pairs, na.rm = TRUE)
  } else {
    NA_real_
  }

  starve_rate <- if (nrow(step_subset) > 0L) {
    mean(step_subset$candidate_starved %in% TRUE, na.rm = TRUE)
  } else {
    NA_real_
  }
  fallback_rate <- if (nrow(step_subset) > 0L) {
    mean(!step_subset$fallback_used %in% c("base", "warm_start"), na.rm = TRUE)
  } else {
    NA_real_
  }

  fallback_used_mode <- .adaptive_mode_value(step_subset$fallback_used)
  starved_rows <- step_subset[step_subset$candidate_starved %in% TRUE, , drop = FALSE]
  starvation_reason_mode <- .adaptive_mode_value(starved_rows$starvation_reason)
  total_after_dup <- sum(step_subset$n_candidates_after_duplicates, na.rm = TRUE)
  total_star_cap_rejects <- sum(step_subset$star_cap_rejects, na.rm = TRUE)
  star_cap_reject_rate <- if (is.finite(total_after_dup) && total_after_dup > 0) {
    total_star_cap_rejects / total_after_dup
  } else {
    NA_real_
  }

  fit <- state$btl_fit %||% list()
  model_variant <- fit$model_variant %||% NA_character_

  ts_sigma_mean <- NA_real_
  ts_sigma_max <- NA_real_
  ts_degree_sigma_corr <- NA_real_
  ts_btl_theta_corr <- NA_real_
  ts_btl_rank_spearman <- NA_real_
  ci95_theta_width_mean <- NA_real_
  ci95_theta_width_median <- NA_real_
  ci95_theta_width_p90 <- NA_real_
  ci95_theta_width_max <- NA_real_
  near_tie_adj_frac <- NA_real_
  near_tie_adj_count <- NA_integer_
  p_adj_median <- NA_real_
  cov_trace_theta <- NA_real_
  cov_logdet_diag_theta <- NA_real_
  post_sd_theta_p10 <- NA_real_
  post_sd_theta_p50 <- NA_real_
  post_sd_theta_p90 <- NA_real_
  top20_boundary_entropy_mean <- NA_real_
  top20_boundary_entropy_p90 <- NA_real_
  nn_diff_sd_mean <- NA_real_
  nn_diff_sd_p90 <- NA_real_

  trueskill_state <- state$trueskill_state %||% NULL
  defaults <- adaptive_defaults(length(ids))
  recent_deg_summary <- .adaptive_recent_deg(history, ids, defaults$W_cap)
  recent_deg_vals <- as.double(recent_deg_summary[ids])
  recent_deg_median <- if (length(recent_deg_vals) > 0L) {
    stats::median(recent_deg_vals)
  } else {
    NA_real_
  }
  recent_deg_max <- if (length(recent_deg_vals) > 0L) {
    as.integer(max(recent_deg_vals))
  } else {
    NA_integer_
  }
  if (!is.null(trueskill_state) && is.data.frame(trueskill_state$items)) {
    ts_items <- trueskill_state$items
    ts_ids <- as.character(ts_items$item_id)
    idx <- match(ids, ts_ids)
    ts_sigma <- ts_items$sigma[idx]
    ts_mu <- ts_items$mu[idx]
    if (length(ts_sigma) > 0L && all(is.finite(ts_sigma))) {
      ts_sigma_mean <- mean(ts_sigma)
      ts_sigma_max <- max(ts_sigma)
      if (length(ts_sigma) > 1L) {
        sigma_sd <- stats::sd(ts_sigma)
        deg_sd <- stats::sd(deg_vals)
        if (is.finite(sigma_sd) && is.finite(deg_sd) && sigma_sd > 0 && deg_sd > 0) {
          ts_degree_sigma_corr <- stats::cor(ts_sigma, deg_vals, use = "pairwise.complete.obs")
        }
      }
    }
  }

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  if (length(theta_mean) == length(ids)) {
    theta_mean <- as.double(theta_mean[ids])
  } else {
    theta_mean <- NULL
  }

  if (!is.null(theta_mean) && !is.null(trueskill_state)) {
    ts_mu <- trueskill_state$items$mu[match(ids, trueskill_state$items$item_id)]
    if (length(ts_mu) == length(theta_mean)) {
      ts_btl_theta_corr <- stats::cor(ts_mu, theta_mean, use = "pairwise.complete.obs")
      rank_theta <- rank(theta_mean, ties.method = "average")
      rank_mu <- rank(ts_mu, ties.method = "average")
      ts_btl_rank_spearman <- stats::cor(rank_mu, rank_theta,
        method = "spearman",
        use = "pairwise.complete.obs"
      )
    }
  }

  draws <- fit$btl_posterior_draws %||% NULL
  if (is.matrix(draws) && is.numeric(draws)) {
    if (is.null(colnames(draws))) {
      colnames(draws) <- ids
    }
    draws <- draws[, ids, drop = FALSE]
    draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")

    ci_bounds <- apply(
      draws,
      2,
      stats::quantile,
      probs = c(0.025, 0.975),
      names = FALSE
    )
    ci_widths <- ci_bounds[2L, ] - ci_bounds[1L, ]
    ci95_theta_width_mean <- mean(ci_widths)
    ci95_theta_width_median <- stats::median(ci_widths)
    ci95_theta_width_p90 <- stats::quantile(ci_widths, probs = 0.90, names = FALSE)
    ci95_theta_width_max <- max(ci_widths)

    cov_theta <- stats::cov(draws)
    cov_diag <- diag(cov_theta)
    cov_trace_theta <- sum(cov_diag)
    cov_logdet_diag_theta <- sum(log(pmax(cov_diag, .Machine$double.eps)))
    post_sd <- sqrt(pmax(cov_diag, 0))
    post_sd_theta_p10 <- stats::quantile(post_sd, probs = 0.10, names = FALSE)
    post_sd_theta_p50 <- stats::quantile(post_sd, probs = 0.50, names = FALSE)
    post_sd_theta_p90 <- stats::quantile(post_sd, probs = 0.90, names = FALSE)

    rank_draws <- t(apply(draws, 1, function(row) rank(-row, ties.method = "average")))
    top_k <- min(20L, ncol(rank_draws))
    if (top_k >= 1L) {
      in_top <- rank_draws <= top_k
      p_top <- colMeans(in_top)
      entropy <- -(p_top * log(pmax(p_top, .Machine$double.eps)) +
        (1 - p_top) * log(pmax(1 - p_top, .Machine$double.eps)))
      boundary_lo <- max(1L, top_k - 2L)
      boundary_hi <- min(length(entropy), top_k + 2L)
      boundary_idx <- boundary_lo:boundary_hi
      top20_boundary_entropy_mean <- mean(entropy[boundary_idx])
      top20_boundary_entropy_p90 <- stats::quantile(entropy[boundary_idx], probs = 0.90, names = FALSE)
    }

    if (!is.null(theta_mean) && length(theta_mean) == length(ids) && length(ids) >= 2L) {
      rank_order <- order(-theta_mean, ids)
      p_adj <- vapply(seq_len(length(rank_order) - 1L), function(k) {
        lhs <- rank_order[[k]]
        rhs <- rank_order[[k + 1L]]
        mean(draws[, lhs] > draws[, rhs])
      }, numeric(1L))
      near_low <- as.double(config$near_tie_p_low)
      near_high <- as.double(config$near_tie_p_high)
      near_tie <- p_adj >= near_low & p_adj <= near_high
      near_tie_adj_frac <- mean(near_tie)
      near_tie_adj_count <- sum(near_tie)
      p_adj_median <- stats::median(p_adj)

      nn_diff_draws <- draws[, rank_order[-length(rank_order)], drop = FALSE] -
        draws[, rank_order[-1L], drop = FALSE]
      nn_diff_sd <- apply(nn_diff_draws, 2, stats::sd)
      nn_diff_sd_mean <- mean(nn_diff_sd)
      nn_diff_sd_p90 <- stats::quantile(nn_diff_sd, probs = 0.90, names = FALSE)
    }
  }

  mcmc_config_used <- fit$mcmc_config_used %||% list()

  round_id_current <- as.integer(state$round$round_id %||% NA_integer_)
  round_committed <- as.integer(state$round$round_committed %||% NA_integer_)
  round_id_at_refit <- if (!is.na(round_id_current) &&
    !is.na(round_committed) &&
    round_committed == 0L &&
    total_pairs_done > 0L) {
    as.integer(max(1L, round_id_current - 1L))
  } else {
    as.integer(round_id_current)
  }
  controller <- .adaptive_controller_resolve(state)
  round_summary <- state$refit_meta$last_completed_round_summary %||% list()
  if (!is.na(round_id_at_refit) && !is.na(round_summary$round_id %||% NA_integer_) &&
    as.integer(round_summary$round_id) == round_id_at_refit) {
    quota_source <- round_summary
  } else {
    quota_source <- state$round %||% list()
  }

  row <- list(
    refit_id = as.integer(nrow(state$round_log) + 1L),
    round_id_at_refit = round_id_at_refit,
    step_id_at_refit = as.integer(step_id_at_refit),
    timestamp = refit_context$timestamp,
    model_variant = as.character(model_variant),
    n_items = as.integer(state$n_items),
    total_pairs_done = as.integer(total_pairs_done),
    new_pairs_since_last_refit = as.integer(new_pairs_since_last_refit),
    n_unique_pairs_seen = as.integer(n_unique_pairs_seen),
    proposed_pairs_mode = as.double(proposed_pairs_mode),
    starve_rate_since_last_refit = as.double(starve_rate),
    fallback_rate_since_last_refit = as.double(fallback_rate),
    fallback_used_mode = as.character(fallback_used_mode),
    starvation_reason_mode = as.character(starvation_reason_mode),
    global_identified = as.logical(controller$global_identified %||% FALSE),
    global_identified_reliability_min = as.double(controller$global_identified_reliability_min %||% NA_real_),
    global_identified_rank_corr_min = as.double(controller$global_identified_rank_corr_min %||% NA_real_),
    long_quota_raw = as.integer(quota_source$long_quota_raw %||% NA_integer_),
    long_quota_effective = as.integer(quota_source$long_quota_effective %||% NA_integer_),
    long_quota_removed = as.integer(quota_source$long_quota_removed %||% NA_integer_),
    realloc_to_mid = as.integer(quota_source$realloc_to_mid %||% NA_integer_),
    realloc_to_local = as.integer(quota_source$realloc_to_local %||% NA_integer_),
    mean_degree = as.double(mean_degree),
    min_degree = as.integer(min_degree),
    pos_balance_sd = as.double(pos_balance_sd),
    epsilon_mean = as.double(fit$epsilon_mean %||% NA_real_),
    epsilon_p2.5 = as.double(fit$epsilon_p2.5 %||% NA_real_),
    epsilon_p5 = as.double(fit$epsilon_p5 %||% NA_real_),
    epsilon_p50 = as.double(fit$epsilon_p50 %||% NA_real_),
    epsilon_p95 = as.double(fit$epsilon_p95 %||% NA_real_),
    epsilon_p97.5 = as.double(fit$epsilon_p97.5 %||% NA_real_),
    b_mean = as.double(fit$beta_mean %||% NA_real_),
    b_p2.5 = as.double(fit$beta_p2.5 %||% NA_real_),
    b_p5 = as.double(fit$beta_p5 %||% NA_real_),
    b_p50 = as.double(fit$beta_p50 %||% NA_real_),
    b_p95 = as.double(fit$beta_p95 %||% NA_real_),
    b_p97.5 = as.double(fit$beta_p97.5 %||% NA_real_),
    ts_sigma_mean = as.double(ts_sigma_mean),
    ts_sigma_max = as.double(ts_sigma_max),
    ts_degree_sigma_corr = as.double(ts_degree_sigma_corr),
    ts_btl_theta_corr = as.double(ts_btl_theta_corr),
    ts_btl_rank_spearman = as.double(ts_btl_rank_spearman),
    star_cap_rejects_since_last_refit = as.integer(total_star_cap_rejects),
    star_cap_reject_rate_since_last_refit = as.double(star_cap_reject_rate),
    recent_deg_median_since_last_refit = as.double(recent_deg_median),
    recent_deg_max_since_last_refit = as.integer(recent_deg_max),
    ci95_theta_width_mean = as.double(ci95_theta_width_mean),
    ci95_theta_width_median = as.double(ci95_theta_width_median),
    ci95_theta_width_p90 = as.double(ci95_theta_width_p90),
    ci95_theta_width_max = as.double(ci95_theta_width_max),
    near_tie_adj_frac = as.double(near_tie_adj_frac),
    near_tie_adj_count = as.integer(near_tie_adj_count),
    p_adj_median = as.double(p_adj_median),
    cov_trace_theta = as.double(cov_trace_theta),
    cov_logdet_diag_theta = as.double(cov_logdet_diag_theta),
    post_sd_theta_p10 = as.double(post_sd_theta_p10),
    post_sd_theta_p50 = as.double(post_sd_theta_p50),
    post_sd_theta_p90 = as.double(post_sd_theta_p90),
    top20_boundary_entropy_mean = as.double(top20_boundary_entropy_mean),
    top20_boundary_entropy_p90 = as.double(top20_boundary_entropy_p90),
    nn_diff_sd_mean = as.double(nn_diff_sd_mean),
    nn_diff_sd_p90 = as.double(nn_diff_sd_p90),
    diagnostics_pass = as.logical(metrics$diagnostics_pass %||% NA),
    diagnostics_divergences_pass = as.logical(metrics$diagnostics_divergences_pass %||% NA),
    diagnostics_rhat_pass = as.logical(metrics$diagnostics_rhat_pass %||% NA),
    diagnostics_ess_pass = as.logical(metrics$diagnostics_ess_pass %||% NA),
    divergences = as.integer(metrics$divergences %||% NA_integer_),
    divergences_max_allowed = as.integer(metrics$divergences_max_allowed %||% NA_integer_),
    max_rhat = as.double(metrics$max_rhat %||% NA_real_),
    max_rhat_allowed = as.double(metrics$max_rhat_allowed %||% NA_real_),
    min_ess_bulk = as.double(metrics$min_ess_bulk %||% NA_real_),
    ess_bulk_required = as.double(metrics$ess_bulk_required %||% NA_real_),
    near_stop_active = as.logical(metrics$near_stop_active %||% NA),
    reliability_EAP = as.double(metrics$reliability_EAP %||% NA_real_),
    eap_reliability_min = as.double(metrics$eap_reliability_min %||% NA_real_),
    eap_pass = as.logical(metrics$eap_pass %||% NA),
    theta_sd_eap = as.double(metrics$theta_sd_eap %||% NA_real_),
    rho_theta = as.double(metrics$rho_theta %||% NA_real_),
    lag_eligible = as.logical(metrics$lag_eligible %||% NA),
    theta_corr_min = as.double(metrics$theta_corr_min %||% NA_real_),
    theta_corr_pass = as.logical(metrics$theta_corr_pass %||% NA),
    delta_sd_theta = as.double(metrics$delta_sd_theta %||% NA_real_),
    theta_sd_rel_change_max = as.double(metrics$theta_sd_rel_change_max %||% NA_real_),
    delta_sd_theta_pass = as.logical(metrics$delta_sd_theta_pass %||% NA),
    rho_rank = as.double(metrics$rho_rank %||% NA_real_),
    rank_spearman_min = as.double(metrics$rank_spearman_min %||% NA_real_),
    rho_rank_pass = as.logical(metrics$rho_rank_pass %||% NA),
    mcmc_chains = as.integer(mcmc_config_used$chains %||% NA_integer_),
    mcmc_parallel_chains = as.integer(mcmc_config_used$parallel_chains %||% NA_integer_),
    mcmc_core_fraction = as.double(mcmc_config_used$core_fraction %||% NA_real_),
    mcmc_cores_detected_physical = as.integer(mcmc_config_used$cores_detected_physical %||% NA_integer_),
    mcmc_cores_detected_logical = as.integer(mcmc_config_used$cores_detected_logical %||% NA_integer_),
    mcmc_threads_per_chain = as.integer(mcmc_config_used$threads_per_chain %||% NA_integer_),
    mcmc_cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_),
    stop_decision = as.logical(stop_decision),
    stop_reason = if (isTRUE(stop_decision)) as.character(stop_reason) else NA_character_
  )

  row
}

#' @keywords internal
#' @noRd
default_btl_fit_fn <- function(state, config) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  config <- .adaptive_btl_resolve_config(state, config)
  results <- .adaptive_results_from_step_log(state)
  if (nrow(results) < 1L) {
    rlang::abort("BTL refit requires at least one committed comparison.")
  }

  fit_out <- fit_bayes_btl_mcmc(
    results = results,
    ids = as.character(state$item_ids),
    model_variant = config$model_variant %||% "btl_e_b",
    cmdstan = config$cmdstan %||% list()
  )

  fit_contract <- .adaptive_btl_extract_fit_contract(fit_out)

  .adaptive_btl_adapt_fit(fit_contract)
}

#' @keywords internal
#' @noRd
maybe_refit_btl <- function(state, config, fit_fn = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  config <- .adaptive_btl_resolve_config(state, config)

  M_done <- as.integer(nrow(state$history_pairs))
  last_refit_M_done <- state$refit_meta$last_refit_M_done %||% 0L
  last_refit_step <- state$refit_meta$last_refit_step %||% 0L

  refit_pairs_target <- .adaptive_refit_pairs_target(state, config)
  config$refit_pairs_target <- refit_pairs_target
  eligibility <- .adaptive_refit_eligibility(
    total_committed = M_done,
    last_refit_committed = last_refit_M_done,
    refit_pairs_target = refit_pairs_target
  )
  if (!isTRUE(eligibility$eligible)) {
    return(list(
      state = state,
      refit_performed = FALSE,
      config = config
    ))
  }

  fit_fn <- fit_fn %||% default_btl_fit_fn
  if (!is.function(fit_fn)) {
    rlang::abort("`fit_fn` must be a function.")
  }

  fit <- fit_fn(state, config)
  if (!is.list(fit) || is.null(fit$btl_posterior_draws)) {
    rlang::abort("`fit_fn` must return a list with `btl_posterior_draws`.")
  }

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  history <- state$refit_meta$theta_mean_history %||% list()
  state$refit_meta$theta_mean_history <- c(history, list(theta_mean))

  refit_context <- .adaptive_btl_refit_context(state, last_refit_M_done, last_refit_step)

  state$btl_fit <- fit
  state$refit_meta$last_refit_M_done <- M_done
  state$refit_meta$last_refit_step <- refit_context$step_id_at_refit
  state$refit_meta$last_refit_round_id <- as.integer(nrow(state$round_log) + 1L)
  state <- .adaptive_update_identifiability_state(state, config)

  list(
    state = state,
    refit_performed = TRUE,
    refit_context = refit_context,
    config = config
  )
}

#' @keywords internal
#' @noRd
compute_stop_metrics <- function(state, config) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  config <- .adaptive_btl_resolve_config(state, config)

  fit <- state$btl_fit %||% NULL
  if (is.null(fit)) {
    return(NULL)
  }

  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws)) {
    rlang::abort("`btl_posterior_draws` must be a numeric matrix.")
  }
  if (nrow(draws) < 2L) {
    rlang::abort("`btl_posterior_draws` must have at least two draws.")
  }

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  theta_mean <- as.double(theta_mean)
  theta_sd_eap <- stats::sd(theta_mean)
  reliability_EAP <- compute_reliability_EAP(draws)

  diagnostics <- fit$diagnostics %||% list()
  divergences <- as.integer(diagnostics$divergences %||% NA_integer_)
  max_rhat <- as.double(diagnostics$max_rhat %||% NA_real_)
  min_ess_bulk <- as.double(diagnostics$min_ess_bulk %||% NA_real_)
  near_stop_active <- isTRUE(state$refit_meta$near_stop)

  ess_bulk_required <- if (isTRUE(near_stop_active)) {
    as.double(config$ess_bulk_min_near_stop)
  } else {
    as.double(config$ess_bulk_min)
  }

  max_rhat_allowed <- as.double(config$max_rhat)
  divergences_max <- as.integer(config$divergences_max)
  diagnostics_divergences_pass <- !is.na(divergences) && divergences <= divergences_max
  diagnostics_rhat_pass <- !is.na(max_rhat) && max_rhat <= max_rhat_allowed
  diagnostics_ess_pass <- !is.na(min_ess_bulk) && !is.na(ess_bulk_required) && min_ess_bulk >= ess_bulk_required

  diagnostics_pass <- isTRUE(diagnostics_divergences_pass) &&
    isTRUE(diagnostics_rhat_pass) &&
    isTRUE(diagnostics_ess_pass)

  eap_min <- as.double(config$eap_reliability_min)
  eap_pass <- isTRUE(diagnostics_pass) &&
    is.finite(reliability_EAP) &&
    reliability_EAP >= eap_min

  history <- state$refit_meta$theta_mean_history %||% list()
  current_refit <- length(history)
  stability_lag <- as.integer(config$stability_lag)
  lag_eligible <- !is.na(stability_lag) &&
    stability_lag >= 1L &&
    current_refit > stability_lag

  rho_theta <- NA_real_
  theta_corr_pass <- NA
  delta_sd_theta <- NA_real_
  delta_sd_theta_pass <- NA
  rho_rank <- NA_real_
  rho_rank_pass <- NA

  if (isTRUE(lag_eligible)) {
    lag_idx <- current_refit - stability_lag
    lag_theta <- history[[lag_idx]]
    if (length(lag_theta) == length(theta_mean)) {
      lag_theta <- as.double(lag_theta)
      rho_theta <- stats::cor(theta_mean, lag_theta, use = "pairwise.complete.obs")
      sd_current <- stats::sd(theta_mean)
      sd_lag <- stats::sd(lag_theta)
      if (is.finite(sd_current) && is.finite(sd_lag) && sd_lag > 0) {
        delta_sd_theta <- abs(sd_current - sd_lag) / sd_lag
      }
      rank_current <- rank(theta_mean, ties.method = "average")
      rank_lag <- rank(lag_theta, ties.method = "average")
      rho_rank <- stats::cor(rank_current, rank_lag, method = "spearman", use = "pairwise.complete.obs")
    }

    theta_corr_pass <- if (is.finite(rho_theta)) {
      rho_theta >= as.double(config$theta_corr_min)
    } else {
      NA
    }
    delta_sd_theta_pass <- if (is.finite(delta_sd_theta)) {
      delta_sd_theta <= as.double(config$theta_sd_rel_change_max)
    } else {
      NA
    }
    rho_rank_pass <- is.finite(rho_rank) && rho_rank >= as.double(config$rank_spearman_min)
  }

  list(
    diagnostics_pass = diagnostics_pass,
    diagnostics_divergences_pass = diagnostics_divergences_pass,
    diagnostics_rhat_pass = diagnostics_rhat_pass,
    diagnostics_ess_pass = diagnostics_ess_pass,
    divergences = divergences,
    divergences_max_allowed = divergences_max,
    max_rhat = max_rhat,
    max_rhat_allowed = max_rhat_allowed,
    min_ess_bulk = min_ess_bulk,
    ess_bulk_required = ess_bulk_required,
    near_stop_active = as.logical(near_stop_active),
    reliability_EAP = reliability_EAP,
    eap_reliability_min = eap_min,
    eap_pass = eap_pass,
    theta_sd_eap = theta_sd_eap,
    rho_theta = rho_theta,
    theta_corr_min = as.double(config$theta_corr_min),
    theta_corr_pass = theta_corr_pass,
    delta_sd_theta = delta_sd_theta,
    theta_sd_rel_change_max = as.double(config$theta_sd_rel_change_max),
    delta_sd_theta_pass = delta_sd_theta_pass,
    rho_rank = rho_rank,
    rank_spearman_min = as.double(config$rank_spearman_min),
    rho_rank_pass = rho_rank_pass,
    lag_eligible = lag_eligible
  )
}

#' @keywords internal
#' @noRd
.adaptive_maybe_enter_phase3 <- function(state, metrics, config) {
  if (isTRUE(state$refit_meta$near_stop)) {
    return(state)
  }
  if (!isTRUE(metrics$diagnostics_pass)) {
    return(state)
  }
  eap_min <- as.double(config$eap_reliability_min)
  threshold <- eap_min - 0.05
  if (is.finite(metrics$reliability_EAP) && metrics$reliability_EAP >= threshold) {
    state$refit_meta$near_stop <- TRUE
  }
  state
}

#' @keywords internal
#' @noRd
should_stop <- function(metrics, config) {
  if (is.null(metrics) || !is.list(metrics)) {
    return(FALSE)
  }
  if (is.null(config) || !is.list(config)) {
    return(FALSE)
  }

  if (!isTRUE(metrics$diagnostics_pass)) {
    return(FALSE)
  }
  eap_min <- as.double(config$eap_reliability_min)
  if (!is.finite(metrics$reliability_EAP) || metrics$reliability_EAP < eap_min) {
    return(FALSE)
  }
  if (!isTRUE(metrics$lag_eligible)) {
    return(FALSE)
  }

  theta_corr_min <- as.double(config$theta_corr_min)
  theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
  rank_spearman_min <- as.double(config$rank_spearman_min)

  if (!is.finite(metrics$rho_theta) || metrics$rho_theta < theta_corr_min) {
    return(FALSE)
  }
  if (!is.finite(metrics$delta_sd_theta) || metrics$delta_sd_theta > theta_sd_rel_change_max) {
    return(FALSE)
  }
  if (!is.finite(metrics$rho_rank) || metrics$rho_rank < rank_spearman_min) {
    return(FALSE)
  }

  TRUE
}
