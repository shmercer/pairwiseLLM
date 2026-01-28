# -------------------------------------------------------------------------
# Adaptive v3 configuration, validation, and schema contracts
# -------------------------------------------------------------------------

.adaptive_v3_clamp <- function(lower, upper, value) {
  value <- max(lower, min(upper, value))
  value
}

.adaptive_v3_round_int <- function(value) {
  as.integer(round(value))
}

.adaptive_v3_intish <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && abs(x - round(x)) < 1e-8
}

.adaptive_v3_check <- function(ok, message) {
  if (!isTRUE(ok)) {
    rlang::abort(message)
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
adaptive_v3_defaults <- function(N) {
  N <- as.integer(N)
  .adaptive_v3_check(!is.na(N) && N >= 2L, "`N` must be an integer >= 2.")

  W <- .adaptive_v3_clamp(5L, 60L, .adaptive_v3_round_int(2 * sqrt(N)))
  explore_rate <- .adaptive_v3_clamp(0.10, 0.25, 0.20 - 0.02 * log10(N))
  batch_size <- .adaptive_v3_clamp(10L, 80L, .adaptive_v3_round_int(4 * sqrt(N)))
  refit_B <- .adaptive_v3_clamp(100L, 800L, .adaptive_v3_round_int(10 * sqrt(N)))
  A_anchors <- .adaptive_v3_clamp(25L, 120L, .adaptive_v3_round_int(2 * sqrt(N)))
  min_degree <- if (N < 3L) 1L else 2L

  list(
    N = N,
    model_variant = "btl_e_b",
    W = as.integer(W),
    A_anchors = as.integer(A_anchors),
    C_max = 20000L,
    refit_B = as.integer(refit_B),
    batch_size = as.integer(batch_size),
    explore_rate = as.double(explore_rate),
    min_degree = as.integer(min_degree),
    target_mean_degree = NULL,
    dup_p_margin = 0.10,
    dup_max_count = 6L,
    dup_utility_quantile = 0.90,
    hard_cap_frac = 0.40,
    eap_reliability_min = 0.95,
    stability_lag = 2L,
    theta_corr_min = 0.995,
    theta_sd_rel_change_max = 0.01,
    rank_spearman_min = 0.995,
    max_rhat = 1.01,
    min_ess_bulk = 300,
    min_ess_bulk_near_stop = 1000,
    require_divergences_zero = TRUE,
    repair_max_cycles = 3L,
    progress = TRUE,
    progress_every_iter = 1L,
    progress_every_refit = 1L,
    progress_level = "refit",
    write_outputs = FALSE,
    output_dir = NULL,
    keep_draws = FALSE,
    thin_draws = 1L,
    cmdstan = list(core_fraction = 0.8, threads_per_chain = 1L)
  )
}

#' @keywords internal
#' @noRd
adaptive_v3_config <- function(N, ...) {
  overrides <- list(...)
  if (length(overrides) == 1L &&
    is.null(names(overrides)) &&
    is.null(overrides[[1L]])) {
    overrides <- NULL
  }
  if (length(overrides) == 1L &&
    is.list(overrides[[1L]]) &&
    is.null(names(overrides))) {
    overrides <- overrides[[1L]]
  } else if (length(overrides) == 1L &&
    is.null(names(overrides)) &&
    !is.null(overrides[[1L]]) &&
    !is.list(overrides[[1L]])) {
    rlang::abort("`overrides` must be a list.")
  }
  if (is.null(overrides)) {
    overrides <- list()
  }
  if (!is.list(overrides)) {
    rlang::abort("`overrides` must be a list.")
  }

  defaults <- adaptive_v3_defaults(N)
  config <- utils::modifyList(defaults, overrides)
  config$N <- defaults$N
  config$model_variant <- normalize_model_variant(config$model_variant %||% defaults$model_variant)
  validate_config(config)
  config
}

.adaptive_v3_check_named_int <- function(x, name, ids = NULL) {
  .adaptive_v3_check(is.integer(x), paste0("`", name, "` must be integer."))
  .adaptive_v3_check(!is.null(names(x)) || length(x) == 0L,
    paste0("`", name, "` must be named when non-empty.")
  )
  if (!is.null(ids)) {
    .adaptive_v3_check(
      length(x) == length(ids) && !is.null(names(x)) &&
        identical(names(x), ids),
      paste0("`", name, "` must be a named vector over `ids`.")
    )
  }
  .adaptive_v3_check(all(x >= 0L), paste0("`", name, "` must be non-negative."))
  invisible(x)
}

.adaptive_v3_validate_pair_keys <- function(keys, ids, ordered, name) {
  if (length(keys) == 0L) return(invisible(keys))
  .adaptive_v3_check(!is.null(keys), paste0("`", name, "` must be named."))
  .adaptive_v3_check(!any(is.na(keys) | keys == ""), paste0("`", name, "` has empty keys."))
  parts <- strsplit(keys, ":", fixed = TRUE)
  bad_len <- lengths(parts) != 2L
  .adaptive_v3_check(!any(bad_len), paste0("`", name, "` keys must be `A:B`."))
  left <- vapply(parts, `[[`, character(1L), 1L)
  right <- vapply(parts, `[[`, character(1L), 2L)
  .adaptive_v3_check(all(left %in% ids) && all(right %in% ids),
    paste0("`", name, "` keys must reference valid ids.")
  )
  .adaptive_v3_check(!any(left == right), paste0("`", name, "` cannot include self-pairs."))
  if (!ordered) {
    normalized <- make_unordered_key(left, right)
    .adaptive_v3_check(all(normalized == keys),
      paste0("`", name, "` keys must be unordered `min:max` form.")
    )
  }
  invisible(keys)
}

#' @keywords internal
#' @noRd
validate_config <- function(config) {
  if (!is.list(config)) {
    rlang::abort("`config` must be a list.")
  }

  required <- c(
    "N", "model_variant", "W", "A_anchors", "C_max",
    "refit_B", "batch_size", "explore_rate",
    "min_degree", "target_mean_degree",
    "dup_p_margin", "dup_max_count", "dup_utility_quantile",
    "hard_cap_frac",
    "eap_reliability_min", "stability_lag",
    "theta_corr_min", "theta_sd_rel_change_max", "rank_spearman_min",
    "max_rhat", "min_ess_bulk", "min_ess_bulk_near_stop",
    "require_divergences_zero", "repair_max_cycles",
    "progress", "progress_every_iter", "progress_every_refit", "progress_level",
    "write_outputs", "output_dir", "keep_draws", "thin_draws"
  )
  missing <- setdiff(required, names(config))
  .adaptive_v3_check(length(missing) == 0L,
    paste0("Missing v3 config fields: ", paste(missing, collapse = ", "), ".")
  )

  .adaptive_v3_check(.adaptive_v3_intish(config$N) && config$N >= 2L, "`N` must be >= 2.")
  .adaptive_v3_check(is.character(config$model_variant) && length(config$model_variant) == 1L,
    "`model_variant` must be a length-1 character value.")
  normalize_model_variant(config$model_variant)
  .adaptive_v3_check(.adaptive_v3_intish(config$W) && config$W >= 1L, "`W` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$A_anchors) && config$A_anchors >= 1L, "`A_anchors` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$C_max) && config$C_max >= 1L, "`C_max` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$refit_B) && config$refit_B >= 1L, "`refit_B` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$batch_size) && config$batch_size >= 1L, "`batch_size` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$explore_rate) && length(config$explore_rate) == 1L &&
    config$explore_rate >= 0 && config$explore_rate <= 1, "`explore_rate` must be in [0, 1].")
  .adaptive_v3_check(.adaptive_v3_intish(config$min_degree) && config$min_degree >= 1L,
    "`min_degree` must be >= 1.")
  if (config$N >= 3L) {
    .adaptive_v3_check(config$min_degree >= 2L, "`min_degree` must be >= 2 for N >= 3.")
  }
  .adaptive_v3_check(config$min_degree <= (config$N - 1L), "`min_degree` must be <= N - 1.")
  if (!is.null(config$target_mean_degree)) {
    .adaptive_v3_check(is.numeric(config$target_mean_degree) && length(config$target_mean_degree) == 1L &&
      is.finite(config$target_mean_degree),
    "`target_mean_degree` must be a finite numeric scalar or NULL.")
    .adaptive_v3_check(config$target_mean_degree > 0, "`target_mean_degree` must be > 0.")
    .adaptive_v3_check(config$target_mean_degree <= (config$N - 1L), "`target_mean_degree` must be <= N - 1.")
  }

  .adaptive_v3_check(is.numeric(config$dup_p_margin) && length(config$dup_p_margin) == 1L &&
    config$dup_p_margin >= 0 && config$dup_p_margin <= 1, "`dup_p_margin` must be in [0, 1].")
  .adaptive_v3_check(.adaptive_v3_intish(config$dup_max_count) && config$dup_max_count >= 1L,
    "`dup_max_count` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$dup_utility_quantile) &&
    length(config$dup_utility_quantile) == 1L &&
    config$dup_utility_quantile >= 0 && config$dup_utility_quantile <= 1,
  "`dup_utility_quantile` must be in [0, 1].")
  .adaptive_v3_check(is.numeric(config$hard_cap_frac) && length(config$hard_cap_frac) == 1L &&
    config$hard_cap_frac > 0 && config$hard_cap_frac <= 1, "`hard_cap_frac` must be in (0, 1].")

  .adaptive_v3_check(is.numeric(config$eap_reliability_min) &&
    length(config$eap_reliability_min) == 1L &&
    config$eap_reliability_min >= 0 && config$eap_reliability_min <= 1,
  "`eap_reliability_min` must be in [0, 1].")
  .adaptive_v3_check(.adaptive_v3_intish(config$stability_lag) &&
    config$stability_lag >= 1L,
  "`stability_lag` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$theta_corr_min) &&
    length(config$theta_corr_min) == 1L &&
    config$theta_corr_min >= 0 && config$theta_corr_min <= 1,
  "`theta_corr_min` must be in [0, 1].")
  .adaptive_v3_check(is.numeric(config$theta_sd_rel_change_max) &&
    length(config$theta_sd_rel_change_max) == 1L &&
    is.finite(config$theta_sd_rel_change_max) &&
    config$theta_sd_rel_change_max >= 0,
  "`theta_sd_rel_change_max` must be a finite value >= 0.")
  .adaptive_v3_check(is.numeric(config$rank_spearman_min) &&
    length(config$rank_spearman_min) == 1L &&
    config$rank_spearman_min >= 0 && config$rank_spearman_min <= 1,
  "`rank_spearman_min` must be in [0, 1].")
  .adaptive_v3_check(is.numeric(config$max_rhat) && length(config$max_rhat) == 1L &&
    config$max_rhat >= 1, "`max_rhat` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$min_ess_bulk) && length(config$min_ess_bulk) == 1L &&
    config$min_ess_bulk > 0, "`min_ess_bulk` must be > 0.")
  .adaptive_v3_check(is.numeric(config$min_ess_bulk_near_stop) &&
    length(config$min_ess_bulk_near_stop) == 1L &&
    config$min_ess_bulk_near_stop > 0,
  "`min_ess_bulk_near_stop` must be > 0.")
  .adaptive_v3_check(is.logical(config$require_divergences_zero) &&
    length(config$require_divergences_zero) == 1L,
  "`require_divergences_zero` must be logical.")
  .adaptive_v3_check(.adaptive_v3_intish(config$repair_max_cycles) && config$repair_max_cycles >= 1L,
    "`repair_max_cycles` must be >= 1.")

  .adaptive_v3_check(is.logical(config$progress) && length(config$progress) == 1L,
    "`progress` must be logical.")
  .adaptive_v3_check(.adaptive_v3_intish(config$progress_every_iter) && config$progress_every_iter >= 1L,
    "`progress_every_iter` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$progress_every_refit) && config$progress_every_refit >= 1L,
    "`progress_every_refit` must be >= 1.")
  .adaptive_v3_check(is.character(config$progress_level) && length(config$progress_level) == 1L &&
    config$progress_level %in% c("basic", "refit", "full"),
  "`progress_level` must be one of 'basic', 'refit', or 'full'.")

  .adaptive_v3_check(is.logical(config$write_outputs) && length(config$write_outputs) == 1L,
    "`write_outputs` must be logical.")
  if (!is.null(config$output_dir)) {
    .adaptive_v3_check(is.character(config$output_dir) && length(config$output_dir) == 1L,
      "`output_dir` must be a length-1 character path or NULL.")
  }
  .adaptive_v3_check(is.logical(config$keep_draws) && length(config$keep_draws) == 1L,
    "`keep_draws` must be logical.")
  .adaptive_v3_check(.adaptive_v3_intish(config$thin_draws) && config$thin_draws >= 1L,
    "`thin_draws` must be >= 1.")
  if (!is.list(config$cmdstan)) {
    rlang::abort("`config$cmdstan` must be a list when provided.")
  }
  cmdstan_output_dir <- config$cmdstan$output_dir %||% NULL
  if (!is.null(cmdstan_output_dir)) {
    .adaptive_v3_check(is.character(cmdstan_output_dir) && length(cmdstan_output_dir) == 1L,
      "`config$cmdstan$output_dir` must be a length-1 character path.")
  }

  invisible(config)
}

#' @keywords internal
#' @noRd
round_log_schema <- function() {
  tibble::tibble(
    round_id = integer(),
    iter_at_refit = integer(),
    mode = character(),
    model_variant = character(),
    n_items = integer(),
    total_pairs = integer(),
    hard_cap_threshold = integer(),
    n_unique_pairs_seen = integer(),
    scheduled_pairs = integer(),
    completed_pairs = integer(),
    backlog_unjudged = integer(),
    new_pairs = integer(),
    proposed_pairs = integer(),
    batch_size = integer(),
    window_W = integer(),
    exploration_rate = double(),
    mean_degree = double(),
    min_degree = integer(),
    pos_balance_mean = double(),
    pos_balance_sd = double(),
    mean_degree_scheduled = double(),
    min_degree_scheduled = integer(),
    pos_balance_sd_scheduled = double(),
    epsilon_mean = double(),
    epsilon_p2.5 = double(),
    epsilon_p5 = double(),
    epsilon_p50 = double(),
    epsilon_p95 = double(),
    epsilon_p97.5 = double(),
    beta_mean = double(),
    beta_p2.5 = double(),
    beta_p5 = double(),
    beta_p50 = double(),
    beta_p95 = double(),
    beta_p97.5 = double(),
    divergences = integer(),
    max_rhat = double(),
    min_ess_bulk = double(),
    diagnostics_pass = logical(),
    reliability_EAP = double(),
    eap_pass = logical(),
    theta_sd_eap = double(),
    rho_theta_lag = double(),
    theta_corr_pass = logical(),
    delta_sd_theta_lag = double(),
    delta_sd_theta_pass = logical(),
    rho_rank_lag = double(),
    rho_rank_pass = logical(),
    rank_stability_pass = logical(),
    stop_eligible = logical(),
    stop_decision = logical(),
    stop_reason = character(),
    starve_rate_since_last_refit = double(),
    fallback_rate_since_last_refit = double(),
    fallback_used_mode = character(),
    starvation_reason_mode = character(),
    mcmc_chains = integer(),
    mcmc_parallel_chains = integer(),
    mcmc_core_fraction = double(),
    mcmc_cores_detected_physical = integer(),
    mcmc_cores_detected_logical = integer(),
    mcmc_threads_per_chain = integer(),
    mcmc_cmdstanr_version = character()
  )
}

#' @keywords internal
#' @noRd
batch_log_schema <- function() {
  tibble::tibble(
    iter = integer(),
    phase = character(),
    mode = character(),
    created_at = as.POSIXct(character(), tz = "UTC"),
    batch_size_target = integer(),
    n_pairs_selected = integer(),
    n_pairs_completed = integer(),
    n_pairs_failed = integer(),
    backlog_unjudged = integer(),
    n_explore_target = integer(),
    n_explore_selected = integer(),
    n_exploit_target = integer(),
    n_exploit_selected = integer(),
    safe_no_utility = logical(),
    n_candidates_generated = integer(),
    n_candidates_after_filters = integer(),
    candidate_starved = logical(),
    fallback_exhausted = logical(),
    fallback_used = character(),
    fallback_path = character(),
    a1_stage = character(),
    a1_W_used = integer(),
    a1_anchor_pool = character(),
    a1_n_generated = integer(),
    a1_n_survive = integer(),
    a1_n_selected = integer(),
    a2_stage = character(),
    a2_W_used = integer(),
    a2_anchor_pool = character(),
    a2_n_generated = integer(),
    a2_n_survive = integer(),
    a2_n_selected = integer(),
    a3_stage = character(),
    a3_W_used = integer(),
    a3_anchor_pool = character(),
    a3_n_generated = integer(),
    a3_n_survive = integer(),
    a3_n_selected = integer(),
    aN_tried = integer(),
    aN_best_stage = character(),
    aN_best_n_generated = integer(),
    aN_best_n_survive = integer(),
    aN_best_n_selected = integer(),
    starvation_reason = character(),
    reason_short_batch = character(),
    W_used = integer(),
    explore_rate_used = double(),
    utility_selected_p50 = double(),
    utility_selected_p90 = double(),
    utility_candidate_p90 = double(),
    iter_exit_path = character()
  )
}

#' @keywords internal
#' @noRd
item_summary_schema <- function() {
  tibble::tibble(
    ID = character(),
    deg = integer(),
    posA_prop = double(),
    theta_mean = double(),
    theta_p2.5 = double(),
    theta_p5 = double(),
    theta_p50 = double(),
    theta_p95 = double(),
    theta_p97.5 = double(),
    theta_sd = double(),
    rank_mean = double(),
    rank_p2.5 = double(),
    rank_p5 = double(),
    rank_p50 = double(),
    rank_p95 = double(),
    rank_p97.5 = double(),
    rank_sd = double()
  )
}

#' @keywords internal
#' @noRd
compute_reliability_EAP <- function(draws) {
  if (is.null(draws) || !is.matrix(draws) || !is.numeric(draws)) {
    return(NA_real_)
  }
  if (nrow(draws) < 2L || ncol(draws) < 2L) {
    return(NA_real_)
  }
  if (any(!is.finite(draws))) {
    return(NA_real_)
  }

  theta_mean <- colMeans(draws)
  theta_var <- apply(draws, 2, stats::var)
  mean_var <- mean(theta_var)
  var_mean <- stats::var(theta_mean)

  if (!is.finite(mean_var) || !is.finite(var_mean) || var_mean <= 0) {
    return(NA_real_)
  }

  reliability <- var_mean / (var_mean + mean_var)
  reliability <- min(1, max(0, reliability))
  as.double(reliability)
}

#' @keywords internal
#' @noRd
compute_gini_degree <- function(deg) {
  if (is.null(deg)) {
    return(NA_real_)
  }
  deg <- as.double(deg)
  deg <- deg[is.finite(deg)]
  if (length(deg) == 0L) {
    return(NA_real_)
  }
  if (any(deg < 0)) {
    rlang::abort("`deg` must be non-negative.")
  }

  total <- sum(deg)
  n <- length(deg)
  if (total == 0) {
    return(0)
  }
  if (n == 1L) {
    return(0)
  }

  deg <- sort(deg)
  idx <- seq_len(n)
  gini <- (2 * sum(deg * idx) / (n * total)) - (n + 1) / n
  as.double(max(0, gini))
}

#' @keywords internal
#' @noRd
compute_gini_posA <- function(posA_counts, deg = NULL) {
  if (is.null(posA_counts)) {
    return(NA_real_)
  }
  posA_counts <- as.double(posA_counts)
  if (length(posA_counts) == 0L) {
    return(NA_real_)
  }
  if (any(is.finite(posA_counts) & posA_counts < 0)) {
    rlang::abort("`posA_counts` must be non-negative.")
  }

  values <- posA_counts
  if (!is.null(deg)) {
    deg <- as.double(deg)
    if (length(deg) != length(posA_counts)) {
      rlang::abort("`deg` must be the same length as `posA_counts`.")
    }
    if (any(is.finite(deg) & deg < 0)) {
      rlang::abort("`deg` must be non-negative.")
    }
    posA_rate <- rep(NA_real_, length(posA_counts))
    positive <- is.finite(deg) & deg > 0 & is.finite(posA_counts)
    posA_rate[positive] <- posA_counts[positive] / deg[positive]
    values <- posA_rate[is.finite(posA_rate)]
  } else {
    values <- posA_counts[is.finite(posA_counts)]
  }

  if (length(values) == 0L) {
    return(NA_real_)
  }

  total <- sum(values)
  n <- length(values)
  if (total == 0) {
    return(0)
  }
  if (n == 1L) {
    return(0)
  }

  values <- sort(values)
  idx <- seq_len(n)
  gini <- (2 * sum(values * idx) / (n * total)) - (n + 1) / n
  as.double(max(0, gini))
}

.adaptive_log_defaults_from_schema <- function(schema) {
  defaults <- lapply(schema, function(col) {
    if (is.integer(col)) {
      NA_integer_
    } else if (inherits(col, "POSIXct")) {
      as.POSIXct(NA, tz = "UTC")
    } else if (is.double(col)) {
      NA_real_
    } else if (is.logical(col)) {
      NA
    } else if (is.character(col)) {
      NA_character_
    } else {
      NA
    }
  })
  tibble::as_tibble(defaults)
}

.adaptive_round_log_defaults <- function() {
  schema <- round_log_schema()
  .adaptive_log_defaults_from_schema(schema)
}

.adaptive_batch_log_defaults <- function() {
  schema <- batch_log_schema()
  .adaptive_log_defaults_from_schema(schema)
}

.adaptive_item_summary_defaults <- function(n_rows = 0L) {
  schema <- item_summary_schema()
  if (n_rows < 1L) {
    return(schema)
  }
  schema[rep_len(1L, n_rows), , drop = FALSE]
}

.adaptive_stop_metrics_defaults <- function() {
  list(
    hard_cap_reached = NA,
    hard_cap_threshold = NA_integer_,
    n_unique_pairs_seen = NA_integer_,
    scheduled_pairs = NA_integer_,
    proposed_pairs = NA_integer_,
    completed_pairs = NA_integer_,
    diagnostics_pass = NA,
    divergences = NA_integer_,
    min_ess_bulk = NA_real_,
    max_rhat = NA_real_,
    reliability_EAP = NA_real_,
    eap_pass = NA,
    theta_sd_eap = NA_real_,
    rho_theta_lag = NA_real_,
    theta_corr_pass = NA,
    delta_sd_theta_lag = NA_real_,
    delta_sd_theta_pass = NA,
    rho_rank_lag = NA_real_,
    rho_rank_pass = NA,
    rank_stability_pass = NA,
    stop_eligible = NA,
    refit_performed = NA,
    candidate_starved = NA,
    reason_short_batch = NA_character_
  )
}

.adaptive_stop_metrics_align <- function(metrics) {
  defaults <- .adaptive_stop_metrics_defaults()
  if (!is.list(metrics)) {
    return(defaults)
  }
  for (nm in names(defaults)) {
    if (!nm %in% names(metrics) || is.null(metrics[[nm]])) {
      metrics[[nm]] <- defaults[[nm]]
    }
  }
  metrics
}

.adaptive_mode_non_na <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & values != ""]
  if (length(values) == 0L) {
    return(NA_character_)
  }
  tab <- table(values)
  names(tab)[[which.max(tab)]]
}

.adaptive_mean_or_na <- function(values) {
  values <- as.double(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NA_real_)
  }
  as.double(mean(values))
}

#' @keywords internal
#' @noRd
build_round_log_row <- function(state,
    fit = NULL,
    metrics = NULL,
    stop_out = NULL,
    config = NULL,
    round_id = NULL,
    batch_size = NULL,
    window_W = NULL,
    exploration_rate = NULL,
    new_pairs = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }
  config <- config %||% state$config$v3 %||% list()

  row <- .adaptive_round_log_defaults()
  if (is.null(round_id)) {
    prior <- state$config$round_log
    round_id <- if (is.data.frame(prior)) nrow(prior) + 1L else 1L
  }

  metrics <- metrics %||% state$posterior$stop_metrics %||% list()
  metrics <- .adaptive_stop_metrics_align(metrics)
  total_pairs <- state$N * (state$N - 1L) / 2
  mean_degree <- mean(as.double(state$deg))
  min_degree <- min(as.integer(state$deg))

  deg <- as.double(state$deg)
  pos1 <- as.double(state$pos1)
  pos_balance <- rep(NA_real_, length(deg))
  positive <- deg > 0
  pos_balance[positive] <- (pos1[positive] / deg[positive]) - 0.5
  pos_balance_mean <- .adaptive_mean_or_na(pos_balance)
  pos_balance_sd <- if (all(is.na(pos_balance))) NA_real_ else stats::sd(pos_balance, na.rm = TRUE)
  scheduled_exposure <- .adaptive_compute_scheduled_exposure(state)

  epsilon_mean <- state$posterior$epsilon_mean %||% NA_real_
  epsilon_p2.5 <- NA_real_
  epsilon_p5 <- NA_real_
  epsilon_p50 <- NA_real_
  epsilon_p95 <- NA_real_
  epsilon_p97.5 <- NA_real_
  if (is.list(fit)) {
    epsilon_mean <- fit$epsilon_mean %||% epsilon_mean
    epsilon_p2.5 <- fit$epsilon_p2.5 %||% NA_real_
    epsilon_p5 <- fit$epsilon_p5 %||% NA_real_
    epsilon_p50 <- fit$epsilon_p50 %||% NA_real_
    epsilon_p95 <- fit$epsilon_p95 %||% NA_real_
    epsilon_p97.5 <- fit$epsilon_p97.5 %||% NA_real_
  }

  beta_mean <- NA_real_
  beta_p2.5 <- NA_real_
  beta_p5 <- NA_real_
  beta_p50 <- NA_real_
  beta_p95 <- NA_real_
  beta_p97.5 <- NA_real_
  if (is.list(fit)) {
    beta_mean <- fit$beta_mean %||% NA_real_
    beta_p2.5 <- fit$beta_p2.5 %||% NA_real_
    beta_p5 <- fit$beta_p5 %||% NA_real_
    beta_p50 <- fit$beta_p50 %||% NA_real_
    beta_p95 <- fit$beta_p95 %||% NA_real_
    beta_p97.5 <- fit$beta_p97.5 %||% NA_real_
  }

  theta_draws <- NULL
  if (is.list(fit) && !is.null(fit$theta_draws)) {
    theta_draws <- fit$theta_draws
  }
  reliability_EAP <- metrics$reliability_EAP %||% compute_reliability_EAP(theta_draws)

  diagnostics <- fit$diagnostics %||% list()
  divergences <- diagnostics$divergences %||% NA_integer_
  min_ess_bulk <- diagnostics$min_ess_bulk %||% NA_real_
  max_rhat <- diagnostics$max_rhat %||% NA_real_
  mcmc_config_used <- state$posterior$mcmc_config_used %||% list()

  stop_decision <- stop_out$stop_decision %||% NA
  stop_reason <- stop_out$stop_reason %||% state$stop_reason %||% NA_character_
  stop_eligible <- metrics$stop_eligible %||% NA

  batch_log <- state$batch_log %||% tibble::tibble()
  if (!is.data.frame(batch_log)) {
    batch_log <- tibble::tibble()
  }
  batch_log <- .adaptive_align_log_schema(batch_log, batch_log_schema())
  batch_log <- tibble::as_tibble(batch_log)
  if (nrow(batch_log) > 0L) {
    prior_log <- state$config$round_log %||% tibble::tibble()
    last_iter_at_refit <- if (is.data.frame(prior_log) && nrow(prior_log) > 0L) {
      prior_log$iter_at_refit[[nrow(prior_log)]]
    } else {
      NA_integer_
    }
    if (!is.na(last_iter_at_refit)) {
      batch_log <- batch_log[batch_log$iter > last_iter_at_refit, , drop = FALSE]
    }
  }

  starve_rate_since_last_refit <- .adaptive_mean_or_na(batch_log$candidate_starved)
  fallback_rate_since_last_refit <- .adaptive_mean_or_na(batch_log$fallback_used != "base_window")
  fallback_used_mode <- .adaptive_mode_non_na(batch_log$fallback_used)
  starvation_reason_mode <- .adaptive_mode_non_na(batch_log$starvation_reason)

  row$round_id <- as.integer(round_id)
  row$iter_at_refit <- as.integer(state$iter %||% NA_integer_)
  row$mode <- as.character(state$mode %||% NA_character_)
  row$model_variant <- as.character(fit$model_variant %||%
    state$posterior$model_variant %||% NA_character_)
  row$n_items <- as.integer(state$N)
  row$total_pairs <- as.integer(total_pairs)
  row$hard_cap_threshold <- as.integer(metrics$hard_cap_threshold %||% NA_integer_)
  row$n_unique_pairs_seen <- as.integer(metrics$n_unique_pairs_seen %||% NA_integer_)
  row$scheduled_pairs <- as.integer(metrics$scheduled_pairs %||% NA_integer_)
  row$completed_pairs <- as.integer(metrics$completed_pairs %||% NA_integer_)
  row$backlog_unjudged <- as.integer(
    if (is.na(row$scheduled_pairs) || is.na(row$completed_pairs)) {
      NA_integer_
    } else {
      row$scheduled_pairs - row$completed_pairs
    }
  )
  row$new_pairs <- as.integer(new_pairs %||% NA_integer_)
  row$proposed_pairs <- as.integer(metrics$proposed_pairs %||% NA_integer_)
  row$batch_size <- as.integer(batch_size %||% config$batch_size %||% NA_integer_)
  row$window_W <- as.integer(window_W %||% config$W %||% NA_integer_)
  row$exploration_rate <- as.double(exploration_rate %||% config$explore_rate %||% NA_real_)
  row$mean_degree <- as.double(mean_degree)
  row$min_degree <- as.integer(min_degree)
  row$pos_balance_mean <- as.double(pos_balance_mean)
  row$pos_balance_sd <- as.double(pos_balance_sd)
  row$mean_degree_scheduled <- as.double(scheduled_exposure$mean_degree_scheduled %||% NA_real_)
  row$min_degree_scheduled <- as.integer(scheduled_exposure$min_degree_scheduled %||% NA_integer_)
  row$pos_balance_sd_scheduled <- as.double(scheduled_exposure$pos_balance_sd_scheduled %||% NA_real_)
  row$epsilon_mean <- as.double(epsilon_mean)
  row$epsilon_p2.5 <- as.double(epsilon_p2.5)
  row$epsilon_p5 <- as.double(epsilon_p5)
  row$epsilon_p50 <- as.double(epsilon_p50)
  row$epsilon_p95 <- as.double(epsilon_p95)
  row$epsilon_p97.5 <- as.double(epsilon_p97.5)
  row$beta_mean <- as.double(beta_mean)
  row$beta_p2.5 <- as.double(beta_p2.5)
  row$beta_p5 <- as.double(beta_p5)
  row$beta_p50 <- as.double(beta_p50)
  row$beta_p95 <- as.double(beta_p95)
  row$beta_p97.5 <- as.double(beta_p97.5)
  row$divergences <- as.integer(divergences)
  row$max_rhat <- as.double(max_rhat)
  row$min_ess_bulk <- as.double(min_ess_bulk)
  row$diagnostics_pass <- as.logical(metrics$diagnostics_pass %||% NA)
  row$reliability_EAP <- as.double(reliability_EAP)
  row$eap_pass <- as.logical(metrics$eap_pass %||% NA)
  row$theta_sd_eap <- as.double(metrics$theta_sd_eap %||% NA_real_)
  row$rho_theta_lag <- as.double(metrics$rho_theta_lag %||% NA_real_)
  row$theta_corr_pass <- as.logical(metrics$theta_corr_pass %||% NA)
  row$delta_sd_theta_lag <- as.double(metrics$delta_sd_theta_lag %||% NA_real_)
  row$delta_sd_theta_pass <- as.logical(metrics$delta_sd_theta_pass %||% NA)
  row$rho_rank_lag <- as.double(metrics$rho_rank_lag %||% NA_real_)
  row$rho_rank_pass <- as.logical(metrics$rho_rank_pass %||% NA)
  row$rank_stability_pass <- as.logical(metrics$rank_stability_pass %||% NA)
  row$stop_eligible <- as.logical(stop_eligible)
  row$stop_decision <- as.logical(stop_decision)
  row$stop_reason <- as.character(stop_reason)
  row$starve_rate_since_last_refit <- as.double(starve_rate_since_last_refit)
  row$fallback_rate_since_last_refit <- as.double(fallback_rate_since_last_refit)
  row$fallback_used_mode <- as.character(fallback_used_mode)
  row$starvation_reason_mode <- as.character(starvation_reason_mode)
  row$mcmc_chains <- as.integer(mcmc_config_used$chains %||% NA_integer_)
  row$mcmc_parallel_chains <- as.integer(mcmc_config_used$parallel_chains %||% NA_integer_)
  row$mcmc_core_fraction <- as.double(mcmc_config_used$core_fraction %||% NA_real_)
  row$mcmc_cores_detected_physical <- as.integer(mcmc_config_used$cores_detected_physical %||% NA_integer_)
  row$mcmc_cores_detected_logical <- as.integer(mcmc_config_used$cores_detected_logical %||% NA_integer_)
  threads_per_chain <- mcmc_config_used$threads_per_chain %||%
    config$cmdstan$threads_per_chain %||% 1L
  row$mcmc_threads_per_chain <- as.integer(threads_per_chain %||% NA_integer_)
  row$mcmc_cmdstanr_version <- as.character(mcmc_config_used$cmdstanr_version %||% NA_character_)
  row
}

#' @keywords internal
#' @noRd
build_batch_log_row <- function(iter,
    phase,
    mode,
    created_at,
    batch_size_target,
    n_pairs_selected,
    n_pairs_completed,
    n_pairs_failed,
    backlog_unjudged,
    n_explore_target,
    n_explore_selected,
    n_exploit_target,
    n_exploit_selected,
    safe_no_utility = NA,
    n_candidates_generated,
    n_candidates_after_filters,
    candidate_starved,
    fallback_exhausted = NA,
    fallback_used = NA_character_,
    fallback_path = NA_character_,
    a1_stage = NA_character_,
    a1_W_used = NA_integer_,
    a1_anchor_pool = NA_character_,
    a1_n_generated = NA_integer_,
    a1_n_survive = NA_integer_,
    a1_n_selected = NA_integer_,
    a2_stage = NA_character_,
    a2_W_used = NA_integer_,
    a2_anchor_pool = NA_character_,
    a2_n_generated = NA_integer_,
    a2_n_survive = NA_integer_,
    a2_n_selected = NA_integer_,
    a3_stage = NA_character_,
    a3_W_used = NA_integer_,
    a3_anchor_pool = NA_character_,
    a3_n_generated = NA_integer_,
    a3_n_survive = NA_integer_,
    a3_n_selected = NA_integer_,
    aN_tried = NA_integer_,
    aN_best_stage = NA_character_,
    aN_best_n_generated = NA_integer_,
    aN_best_n_survive = NA_integer_,
    aN_best_n_selected = NA_integer_,
    starvation_reason = NA_character_,
    reason_short_batch,
    W_used,
    explore_rate_used,
    utility_selected_p50,
    utility_selected_p90,
    utility_candidate_p90,
    iter_exit_path = NULL) {
  row <- .adaptive_batch_log_defaults()
  row$iter <- as.integer(iter)
  row$phase <- as.character(phase)
  row$mode <- as.character(mode)
  row$created_at <- as.POSIXct(created_at, tz = "UTC")
  row$batch_size_target <- as.integer(batch_size_target)
  row$n_pairs_selected <- as.integer(n_pairs_selected)
  row$n_pairs_completed <- as.integer(n_pairs_completed)
  row$n_pairs_failed <- as.integer(n_pairs_failed)
  row$backlog_unjudged <- as.integer(backlog_unjudged)
  row$n_explore_target <- as.integer(n_explore_target)
  row$n_explore_selected <- as.integer(n_explore_selected)
  row$n_exploit_target <- as.integer(n_exploit_target)
  row$n_exploit_selected <- as.integer(n_exploit_selected)
  row$safe_no_utility <- as.logical(safe_no_utility)
  row$n_candidates_generated <- as.integer(n_candidates_generated)
  row$n_candidates_after_filters <- as.integer(n_candidates_after_filters)
  row$candidate_starved <- as.logical(candidate_starved)
  row$fallback_exhausted <- as.logical(fallback_exhausted)
  row$fallback_used <- as.character(fallback_used)
  row$fallback_path <- as.character(fallback_path)
  row$a1_stage <- as.character(a1_stage)
  row$a1_W_used <- as.integer(a1_W_used)
  row$a1_anchor_pool <- as.character(a1_anchor_pool)
  row$a1_n_generated <- as.integer(a1_n_generated)
  row$a1_n_survive <- as.integer(a1_n_survive)
  row$a1_n_selected <- as.integer(a1_n_selected)
  row$a2_stage <- as.character(a2_stage)
  row$a2_W_used <- as.integer(a2_W_used)
  row$a2_anchor_pool <- as.character(a2_anchor_pool)
  row$a2_n_generated <- as.integer(a2_n_generated)
  row$a2_n_survive <- as.integer(a2_n_survive)
  row$a2_n_selected <- as.integer(a2_n_selected)
  row$a3_stage <- as.character(a3_stage)
  row$a3_W_used <- as.integer(a3_W_used)
  row$a3_anchor_pool <- as.character(a3_anchor_pool)
  row$a3_n_generated <- as.integer(a3_n_generated)
  row$a3_n_survive <- as.integer(a3_n_survive)
  row$a3_n_selected <- as.integer(a3_n_selected)
  row$aN_tried <- as.integer(aN_tried)
  row$aN_best_stage <- as.character(aN_best_stage)
  row$aN_best_n_generated <- as.integer(aN_best_n_generated)
  row$aN_best_n_survive <- as.integer(aN_best_n_survive)
  row$aN_best_n_selected <- as.integer(aN_best_n_selected)
  row$starvation_reason <- as.character(starvation_reason)
  row$reason_short_batch <- as.character(reason_short_batch)
  row$W_used <- as.integer(W_used)
  row$explore_rate_used <- as.double(explore_rate_used)
  row$utility_selected_p50 <- as.double(utility_selected_p50)
  row$utility_selected_p90 <- as.double(utility_selected_p90)
  row$utility_candidate_p90 <- as.double(utility_candidate_p90)
  row$iter_exit_path <- as.character(iter_exit_path %||% NA_character_)
  row
}

#' @keywords internal
#' @noRd
build_item_summary <- function(state, fit = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state.")
  }

  theta_draws <- NULL
  if (is.list(fit) && !is.null(fit$theta_draws)) {
    theta_draws <- fit$theta_draws
  }

  if (is.null(theta_draws) || !is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    return(.adaptive_item_summary_defaults())
  }

  if (is.null(colnames(theta_draws))) {
    colnames(theta_draws) <- as.character(state$ids)
  }

  theta_draws <- theta_draws[, state$ids, drop = FALSE]
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(theta_draws, name = "theta_draws")
  theta_mean <- as.double(colMeans(theta_draws))
  theta_sd <- as.double(apply(theta_draws, 2, stats::sd))
  probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
  theta_quantiles <- vapply(
    seq_len(ncol(theta_draws)),
    function(idx) stats::quantile(theta_draws[, idx], probs = probs, names = FALSE),
    numeric(length(probs))
  )
  theta_p2.5 <- as.double(theta_quantiles[1L, ])
  theta_p5 <- as.double(theta_quantiles[2L, ])
  theta_p50 <- as.double(theta_quantiles[3L, ])
  theta_p95 <- as.double(theta_quantiles[4L, ])
  theta_p97.5 <- as.double(theta_quantiles[5L, ])

  rank_mat <- t(apply(theta_draws, 1, function(row) rank(-row, ties.method = "average")))
  colnames(rank_mat) <- state$ids
  rank_mean <- as.double(colMeans(rank_mat))
  rank_sd <- as.double(apply(rank_mat, 2, stats::sd))
  rank_quantiles <- vapply(
    seq_len(ncol(rank_mat)),
    function(idx) stats::quantile(rank_mat[, idx], probs = probs, names = FALSE),
    numeric(length(probs))
  )
  rank_p2.5 <- as.double(rank_quantiles[1L, ])
  rank_p5 <- as.double(rank_quantiles[2L, ])
  rank_p50 <- as.double(rank_quantiles[3L, ])
  rank_p95 <- as.double(rank_quantiles[4L, ])
  rank_p97.5 <- as.double(rank_quantiles[5L, ])

  deg <- as.integer(state$deg)
  pos1 <- as.double(state$pos1)
  posA_prop <- rep(NA_real_, length(deg))
  positive <- deg > 0
  posA_prop[positive] <- pos1[positive] / deg[positive]

  tibble::tibble(
    ID = as.character(state$ids),
    deg = deg,
    posA_prop = as.double(posA_prop),
    theta_mean = theta_mean,
    theta_p2.5 = theta_p2.5,
    theta_p5 = theta_p5,
    theta_p50 = theta_p50,
    theta_p95 = theta_p95,
    theta_p97.5 = theta_p97.5,
    theta_sd = theta_sd,
    rank_mean = rank_mean,
    rank_p2.5 = rank_p2.5,
    rank_p5 = rank_p5,
    rank_p50 = rank_p50,
    rank_p95 = rank_p95,
    rank_p97.5 = rank_p97.5,
    rank_sd = rank_sd
  )
}
