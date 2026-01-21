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

.adaptive_v3_tau_fn <- function(N) {
  N <- as.integer(N)
  .adaptive_v3_check(!is.na(N) && N >= 2L, "`tau_fn` requires `N >= 2`.")
  .adaptive_v3_clamp(0.8, 3.0, sqrt(N) / 25)
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
  S_subset <- .adaptive_v3_clamp(100L, 400L, .adaptive_v3_round_int(6 * sqrt(N)))
  K_top <- .adaptive_v3_clamp(20L, 200L, .adaptive_v3_round_int(2 * W))
  U_abs <- .adaptive_v3_clamp(0.0015, 0.006, 0.004 * (30 / N)^0.25)

  min_degree <- if (N < 3L) 1L else 2L

  list(
    N = N,
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
    S_subset = as.integer(S_subset),
    tau_fn = .adaptive_v3_tau_fn,
    K_top = as.integer(K_top),
    U_abs = as.double(U_abs),
    checks_passed_target = 2L,
    min_new_pairs_for_check = 5L,
    rank_weak_adj_threshold = 0.95,
    rank_weak_adj_frac_max = 0.05,
    rank_min_adj_prob = 0.60,
    max_rhat = 1.01,
    min_ess_bulk = 300,
    min_ess_bulk_near_stop = 1000,
    require_divergences_zero = TRUE,
    repair_max_cycles = 3L,
    write_outputs = FALSE,
    output_dir = NULL,
    keep_draws = FALSE,
    thin_draws = 1L
  )
}

#' @keywords internal
#' @noRd
adaptive_v3_config <- function(N, ...) {
  overrides <- list(...)
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
    "N", "W", "A_anchors", "C_max",
    "refit_B", "batch_size", "explore_rate",
    "min_degree", "target_mean_degree",
    "dup_p_margin", "dup_max_count", "dup_utility_quantile",
    "hard_cap_frac",
    "S_subset", "tau_fn", "K_top", "U_abs", "checks_passed_target",
    "min_new_pairs_for_check", "rank_weak_adj_threshold",
    "rank_weak_adj_frac_max", "rank_min_adj_prob",
    "max_rhat", "min_ess_bulk", "min_ess_bulk_near_stop",
    "require_divergences_zero", "repair_max_cycles",
    "write_outputs", "output_dir", "keep_draws", "thin_draws"
  )
  missing <- setdiff(required, names(config))
  .adaptive_v3_check(length(missing) == 0L,
    paste0("Missing v3 config fields: ", paste(missing, collapse = ", "), ".")
  )

  .adaptive_v3_check(.adaptive_v3_intish(config$N) && config$N >= 2L, "`N` must be >= 2.")
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

  .adaptive_v3_check(.adaptive_v3_intish(config$S_subset) && config$S_subset >= 1L,
    "`S_subset` must be >= 1.")
  .adaptive_v3_check(is.function(config$tau_fn), "`tau_fn` must be a function.")
  .adaptive_v3_check(.adaptive_v3_intish(config$K_top) && config$K_top >= 1L, "`K_top` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$U_abs) && length(config$U_abs) == 1L &&
    config$U_abs > 0, "`U_abs` must be > 0.")
  .adaptive_v3_check(.adaptive_v3_intish(config$checks_passed_target) && config$checks_passed_target >= 1L,
    "`checks_passed_target` must be >= 1.")
  .adaptive_v3_check(.adaptive_v3_intish(config$min_new_pairs_for_check) &&
    config$min_new_pairs_for_check >= 1L,
  "`min_new_pairs_for_check` must be >= 1.")
  .adaptive_v3_check(is.numeric(config$rank_weak_adj_threshold) &&
    length(config$rank_weak_adj_threshold) == 1L &&
    config$rank_weak_adj_threshold > 0 && config$rank_weak_adj_threshold < 1,
  "`rank_weak_adj_threshold` must be in (0, 1).")
  .adaptive_v3_check(is.numeric(config$rank_weak_adj_frac_max) &&
    length(config$rank_weak_adj_frac_max) == 1L &&
    config$rank_weak_adj_frac_max >= 0 && config$rank_weak_adj_frac_max <= 1,
  "`rank_weak_adj_frac_max` must be in [0, 1].")
  .adaptive_v3_check(is.numeric(config$rank_min_adj_prob) &&
    length(config$rank_min_adj_prob) == 1L &&
    config$rank_min_adj_prob >= 0 && config$rank_min_adj_prob <= 1,
  "`rank_min_adj_prob` must be in [0, 1].")

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

  invisible(config)
}

#' @keywords internal
#' @noRd
round_log_schema <- function() {
  tibble::tibble(
    round_id = integer(),
    iter_at_refit = integer(),
    n_items = integer(),
    total_pairs = integer(),
    new_pairs = integer(),
    batch_size = integer(),
    window_W = integer(),
    exploration_rate = double(),
    mean_degree = double(),
    min_degree = integer(),
    pos_balance_sd = double(),
    epsilon_mean = double(),
    epsilon_ci90_lo = double(),
    epsilon_ci90_hi = double(),
    reliability_EAP = double(),
    theta_sd_median = double(),
    tau = double(),
    theta_sd_pass = logical(),
    U0 = double(),
    U_top_median = double(),
    U_abs = double(),
    U_pass = logical(),
    U_dup_threshold = double(),
    hard_cap_reached = logical(),
    hard_cap_threshold = integer(),
    n_unique_pairs_seen = integer(),
    scheduled_pairs = integer(),
    proposed_pairs = integer(),
    completed_pairs = integer(),
    rank_stability_pass = logical(),
    frac_weak_adj = double(),
    min_adj_prob = double(),
    weak_adj_threshold = double(),
    weak_adj_frac_max = double(),
    min_adj_prob_threshold = double(),
    min_new_pairs_for_check = integer(),
    divergences = integer(),
    min_ess_bulk = double(),
    max_rhat = double(),
    diagnostics_pass = logical(),
    stop_decision = logical(),
    stop_reason = character(),
    mode = character()
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
    n_candidates_generated = integer(),
    n_candidates_after_filters = integer(),
    candidate_starved = logical(),
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
    theta_mean = double(),
    theta_sd = double(),
    theta_ci90_lo = double(),
    theta_ci90_hi = double(),
    theta_ci95_lo = double(),
    theta_ci95_hi = double(),
    rank_mean = double(),
    rank_sd = double(),
    deg = integer(),
    posA_prop = double()
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

.adaptive_round_log_defaults <- function() {
  schema <- round_log_schema()
  defaults <- lapply(schema, function(col) {
    if (is.integer(col)) {
      NA_integer_
    } else if (is.double(col)) {
      NA_real_
    } else if (is.logical(col)) {
      NA
    } else if (is.character(col)) {
      NA_character_
    } else if (inherits(col, "POSIXct")) {
      as.POSIXct(NA, tz = "UTC")
    } else {
      NA
    }
  })
  tibble::as_tibble(defaults)
}

.adaptive_batch_log_defaults <- function() {
  schema <- batch_log_schema()
  defaults <- lapply(schema, function(col) {
    if (is.integer(col)) {
      NA_integer_
    } else if (is.double(col)) {
      NA_real_
    } else if (is.logical(col)) {
      NA
    } else if (is.character(col)) {
      NA_character_
    } else if (inherits(col, "POSIXct")) {
      as.POSIXct(NA, tz = "UTC")
    } else {
      NA
    }
  })
  tibble::as_tibble(defaults)
}

.adaptive_item_summary_defaults <- function(n_rows = 0L) {
  schema <- item_summary_schema()
  if (n_rows < 1L) {
    return(schema)
  }
  schema[rep_len(1L, n_rows), , drop = FALSE]
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
  total_pairs <- state$N * (state$N - 1L) / 2
  mean_degree <- mean(as.double(state$deg))
  min_degree <- min(as.integer(state$deg))

  deg <- as.double(state$deg)
  pos1 <- as.double(state$pos1)
  pos_balance <- rep(NA_real_, length(deg))
  positive <- deg > 0
  pos_balance[positive] <- (pos1[positive] / deg[positive]) - 0.5
  pos_balance_sd <- if (all(is.na(pos_balance))) NA_real_ else stats::sd(pos_balance, na.rm = TRUE)

  epsilon_mean <- state$posterior$epsilon_mean %||% NA_real_
  epsilon_ci90_lo <- NA_real_
  epsilon_ci90_hi <- NA_real_
  if (is.list(fit) && !is.null(fit$epsilon_summary)) {
    eps_summary <- tibble::as_tibble(fit$epsilon_summary)
    if (nrow(eps_summary) >= 1L) {
      epsilon_mean <- eps_summary$epsilon_mean[[1L]] %||% epsilon_mean
      epsilon_ci90_lo <- eps_summary$epsilon_ci90_low[[1L]] %||% NA_real_
      epsilon_ci90_hi <- eps_summary$epsilon_ci90_high[[1L]] %||% NA_real_
    }
  }

  theta_draws <- NULL
  if (is.list(fit) && !is.null(fit$theta_draws)) {
    theta_draws <- fit$theta_draws
  } else if (is.list(fit) && !is.null(fit$draws)) {
    theta_draws <- tryCatch(
      .btl_mcmc_v3_theta_draws(fit$draws, item_id = state$ids),
      error = function(e) NULL
    )
  }
  reliability_EAP <- compute_reliability_EAP(theta_draws)

  diagnostics <- fit$diagnostics %||% list()
  divergences <- diagnostics$divergences %||% NA_integer_
  min_ess_bulk <- diagnostics$min_ess_bulk %||% NA_real_
  max_rhat <- diagnostics$max_rhat %||% NA_real_

  stop_decision <- stop_out$stop_decision %||% NA
  stop_reason <- stop_out$stop_reason %||% state$stop_reason %||% NA_character_

  row$round_id <- as.integer(round_id)
  row$iter_at_refit <- as.integer(state$iter %||% NA_integer_)
  row$n_items <- as.integer(state$N)
  row$total_pairs <- as.integer(total_pairs)
  row$new_pairs <- as.integer(new_pairs %||% NA_integer_)
  row$batch_size <- as.integer(batch_size %||% config$batch_size %||% NA_integer_)
  row$window_W <- as.integer(window_W %||% config$W %||% NA_integer_)
  row$exploration_rate <- as.double(exploration_rate %||% config$explore_rate %||% NA_real_)
  row$mean_degree <- as.double(mean_degree)
  row$min_degree <- as.integer(min_degree)
  row$pos_balance_sd <- as.double(pos_balance_sd)
  row$epsilon_mean <- as.double(epsilon_mean)
  row$epsilon_ci90_lo <- as.double(epsilon_ci90_lo)
  row$epsilon_ci90_hi <- as.double(epsilon_ci90_hi)
  row$reliability_EAP <- as.double(reliability_EAP)
  row$theta_sd_median <- as.double(metrics$theta_sd_median_S %||% NA_real_)
  row$tau <- as.double(metrics$tau %||% NA_real_)
  row$theta_sd_pass <- as.logical(metrics$theta_sd_pass %||% NA)
  row$U0 <- as.double(metrics$U0 %||% state$U0 %||% NA_real_)
  row$U_top_median <- as.double(metrics$U_top_median %||% NA_real_)
  row$U_abs <- as.double(config$U_abs %||% NA_real_)
  row$U_pass <- as.logical(metrics$U_pass %||% NA)
  row$U_dup_threshold <- as.double(state$posterior$U_dup_threshold %||% NA_real_)
  row$hard_cap_reached <- as.logical(metrics$hard_cap_reached %||% NA)
  row$hard_cap_threshold <- as.integer(metrics$hard_cap_threshold %||% NA_integer_)
  row$n_unique_pairs_seen <- as.integer(metrics$n_unique_pairs_seen %||% NA_integer_)
  row$scheduled_pairs <- as.integer(metrics$scheduled_pairs %||% NA_integer_)
  row$proposed_pairs <- as.integer(metrics$proposed_pairs %||% NA_integer_)
  row$completed_pairs <- as.integer(metrics$completed_pairs %||% NA_integer_)
  row$rank_stability_pass <- as.logical(metrics$rank_stability_pass %||% NA)
  row$frac_weak_adj <- as.double(metrics$frac_weak_adj %||% NA_real_)
  row$min_adj_prob <- as.double(metrics$min_adj_prob %||% NA_real_)
  row$weak_adj_threshold <- as.double(metrics$weak_adj_threshold %||% NA_real_)
  row$weak_adj_frac_max <- as.double(metrics$weak_adj_frac_max %||% NA_real_)
  row$min_adj_prob_threshold <- as.double(metrics$min_adj_prob_threshold %||% NA_real_)
  row$min_new_pairs_for_check <- as.integer(metrics$min_new_pairs_for_check %||% NA_integer_)
  row$divergences <- as.integer(divergences)
  row$min_ess_bulk <- as.double(min_ess_bulk)
  row$max_rhat <- as.double(max_rhat)
  row$diagnostics_pass <- as.logical(metrics$diagnostics_pass %||% NA)
  row$stop_decision <- as.logical(stop_decision)
  row$stop_reason <- as.character(stop_reason)
  row$mode <- as.character(state$mode %||% NA_character_)
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
    n_candidates_generated,
    n_candidates_after_filters,
    candidate_starved,
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
  row$n_candidates_generated <- as.integer(n_candidates_generated)
  row$n_candidates_after_filters <- as.integer(n_candidates_after_filters)
  row$candidate_starved <- as.logical(candidate_starved)
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
  } else if (is.list(fit) && !is.null(fit$draws)) {
    theta_draws <- tryCatch(
      .btl_mcmc_v3_theta_draws(fit$draws, item_id = state$ids),
      error = function(e) NULL
    )
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
  theta_ci90_lo <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.05, names = FALSE))
  theta_ci90_hi <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.95, names = FALSE))
  theta_ci95_lo <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.025, names = FALSE))
  theta_ci95_hi <- as.double(apply(theta_draws, 2, stats::quantile, probs = 0.975, names = FALSE))

  rank_mat <- t(apply(theta_draws, 1, function(row) rank(-row, ties.method = "average")))
  colnames(rank_mat) <- state$ids
  rank_mean <- as.double(colMeans(rank_mat))
  rank_sd <- as.double(apply(rank_mat, 2, stats::sd))

  deg <- as.integer(state$deg)
  pos1 <- as.double(state$pos1)
  posA_prop <- rep(NA_real_, length(deg))
  positive <- deg > 0
  posA_prop[positive] <- pos1[positive] / deg[positive]

  tibble::tibble(
    ID = as.character(state$ids),
    theta_mean = theta_mean,
    theta_sd = theta_sd,
    theta_ci90_lo = theta_ci90_lo,
    theta_ci90_hi = theta_ci90_hi,
    theta_ci95_lo = theta_ci95_lo,
    theta_ci95_hi = theta_ci95_hi,
    rank_mean = rank_mean,
    rank_sd = rank_sd,
    deg = deg,
    posA_prop = as.double(posA_prop)
  )
}
