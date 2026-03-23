make_test_items <- function(n) {
  n <- as.integer(n)
  tibble::tibble(item_id = seq_len(n))
}

make_test_trueskill_state <- function(items, mu = NULL, sigma = NULL) {
  items <- tibble::as_tibble(items)
  if (is.null(mu)) {
    mu <- rep(25, nrow(items))
  }
  if (is.null(sigma)) {
    sigma <- rep(25 / 3, nrow(items))
  }
  items$mu <- mu
  items$sigma <- sigma
  pairwiseLLM:::new_trueskill_state(items)
}

make_history <- function(pairs) {
  pairs <- tibble::as_tibble(pairs)
  if (nrow(pairs) == 0L && ncol(pairs) == 0L) {
    return(tibble::tibble(A_id = character(), B_id = character()))
  }
  if (!all(c("A_id", "B_id") %in% names(pairs))) {
    if (all(c("A", "B") %in% names(pairs))) {
      pairs <- dplyr::rename(pairs, A_id = .data$A, B_id = .data$B)
    } else if (all(c("i", "j") %in% names(pairs))) {
      pairs <- dplyr::rename(pairs, A_id = .data$i, B_id = .data$j)
    } else {
      rlang::abort("`pairs` must include A_id/B_id, A/B, or i/j.")
    }
  }
  pairs <- dplyr::mutate(pairs, A_id = as.character(.data$A_id), B_id = as.character(.data$B_id))
  pairs[, c("A_id", "B_id"), drop = FALSE]
}

make_test_state <- function(items, trueskill_state, history = tibble::tibble()) {
  state <- pairwiseLLM:::new_adaptive_state(items)
  state$trueskill_state <- trueskill_state
  state$history_pairs <- make_history(history)
  state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  state$warm_start_idx <- 1L
  state$warm_start_done <- TRUE
  state
}

make_deterministic_judge <- function(always = c("i_wins", "j_wins", "invalid")) {
  always <- match.arg(always)
  force(always)

  function(A, B, state, ...) {
    if (identical(always, "invalid")) {
      return(list(is_valid = FALSE, invalid_reason = "invalid_fixture"))
    }
    if (identical(always, "i_wins")) {
      return(list(is_valid = TRUE, Y = 1L))
    }
    list(is_valid = TRUE, Y = 0L)
  }
}

snapshot_state_core <- function(state) {
  state[c(
    "item_ids",
    "item_index",
    "n_items",
    "items",
    "history_pairs",
    "item_log",
    "item_step_log",
    "trueskill_state",
    "warm_start_pairs",
    "warm_start_idx",
    "warm_start_done",
    "btl_fit",
    "stop_metrics",
    "config",
    "meta"
  )]
}

make_test_btl_fit <- function(ids,
                              draws = NULL,
                              diagnostics = NULL,
                              model_variant = "btl_e_b",
                              mcmc_config_used = NULL) {
  ids <- as.character(ids)
  if (is.null(draws)) {
    draws <- matrix(rep(seq_along(ids), each = 10L), nrow = 10L)
  }
  draws <- as.matrix(draws)
  colnames(draws) <- ids

  diagnostics <- diagnostics %||% list(
    divergences = 0L,
    max_rhat = 1.0,
    min_ess_bulk = 1000
  )

  list(
    btl_posterior_draws = draws,
    theta_mean = stats::setNames(as.double(colMeans(draws)), ids),
    theta_sd = stats::setNames(as.double(apply(draws, 2, stats::sd)), ids),
    diagnostics = diagnostics,
    model_variant = model_variant,
    epsilon_mean = NA_real_,
    epsilon_p2.5 = NA_real_,
    epsilon_p5 = NA_real_,
    epsilon_p50 = NA_real_,
    epsilon_p95 = NA_real_,
    epsilon_p97.5 = NA_real_,
    beta_mean = NA_real_,
    beta_p2.5 = NA_real_,
    beta_p5 = NA_real_,
    beta_p50 = NA_real_,
    beta_p95 = NA_real_,
    beta_p97.5 = NA_real_,
    mcmc_config_used = mcmc_config_used
  )
}

make_deterministic_fit_fn <- function(ids, fit = NULL) {
  env <- new.env(parent = emptyenv())
  env$calls <- 0L
  ids <- as.character(ids)
  fit <- fit %||% make_test_btl_fit(ids)

  fit_fn <- function(state, config) {
    env$calls <- env$calls + 1L
    fit
  }

  list(
    fit_fn = fit_fn,
    get_calls = function() env$calls
  )
}

make_test_link_cmdstan_fit_fn <- function() {
  function(stan_data, variable_names, cmdstan, seed, model_fn = NULL) {
    n_draws <- 4L
    draw_offsets <- c(-0.03, -0.01, 0.01, 0.03)
    delta_center <- if (is.numeric(stan_data$hub_ref_cross) && is.numeric(stan_data$spoke_ref_cross)) {
      mean(as.double(stan_data$hub_ref_cross) - as.double(stan_data$spoke_ref_cross), na.rm = TRUE)
    } else {
      0
    }
    if (!is.finite(delta_center)) {
      delta_center <- 0
    }
    hub_prior_signal <- mean(as.double(stan_data$hub_prior_sd %||% numeric()), na.rm = TRUE)
    if (isTRUE(as.integer(stan_data$hub_prior_active %||% 0L) == 1L) &&
      is.finite(hub_prior_signal)) {
      delta_center <- delta_center + (hub_prior_signal * 0.01)
    }

    build_theta_draws <- function(base_vals, prefix) {
      base_vals <- as.double(base_vals %||% numeric())
      if (length(base_vals) < 1L) {
        return(NULL)
      }
      out <- vapply(
        seq_along(base_vals),
        function(idx) base_vals[[idx]] + draw_offsets + ((idx - 1L) * 0.005),
        numeric(n_draws)
      )
      colnames(out) <- paste0(prefix, "[", seq_along(base_vals), "]")
      out
    }

    draws <- matrix(nrow = n_draws, ncol = 0L)
    if ("delta" %in% variable_names) {
      draws <- cbind(draws, delta = delta_center + draw_offsets)
    }
    if ("log_alpha" %in% variable_names) {
      draws <- cbind(draws, log_alpha = c(-0.04, -0.01, 0.01, 0.04))
    }

    theta_hub_draws <- build_theta_draws(stan_data$hub_ref, "theta_hub")
    if (!is.null(theta_hub_draws) &&
      ("theta_hub" %in% variable_names || any(grepl("^theta_hub\\[", variable_names)))) {
      keep <- if ("theta_hub" %in% variable_names) {
        rep(TRUE, ncol(theta_hub_draws))
      } else {
        colnames(theta_hub_draws) %in% variable_names
      }
      draws <- cbind(draws, theta_hub_draws[, keep, drop = FALSE])
    }

    theta_spoke_draws <- build_theta_draws(stan_data$spoke_ref, "theta_spoke")
    if (!is.null(theta_spoke_draws) &&
      ("theta_spoke" %in% variable_names || any(grepl("^theta_spoke\\[", variable_names)))) {
      keep <- if ("theta_spoke" %in% variable_names) {
        rep(TRUE, ncol(theta_spoke_draws))
      } else {
        colnames(theta_spoke_draws) %in% variable_names
      }
      draws <- cbind(draws, theta_spoke_draws[, keep, drop = FALSE])
    }

    if (ncol(draws) < 1L) {
      draws <- matrix(delta_center + draw_offsets, ncol = 1L)
      colnames(draws) <- "delta"
    }

    list(
      fit = NULL,
      draws_matrix = draws,
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1.0,
        min_ess_bulk = 1000
      ),
      mcmc_config_used = list(
        chains = as.integer(cmdstan$chains %||% 4L),
        parallel_chains = as.integer(cmdstan$parallel_chains %||% cmdstan$chains %||% 4L),
        threads_per_chain = as.integer(cmdstan$threads_per_chain %||% 1L),
        cmdstanr_version = "test"
      )
    )
  }
}

test_link_btl_config <- function(x = list()) {
  utils::modifyList(
    list(cmdstan_fit_fn = make_test_link_cmdstan_fit_fn()),
    x %||% list()
  )
}

make_linking_score_judge_fixture <- function(scores) {
  score_names <- names(scores)
  scores <- as.double(scores)
  names(scores) <- score_names

  default_score <- function(item_id) {
    item_id <- as.character(item_id)
    if (grepl("^h\\d+$", item_id)) {
      rank <- as.integer(sub("^h", "", item_id))
      return(-1.0 + (0.16 * rank))
    }
    if (grepl("^s\\d\\d+$", item_id)) {
      set_id <- as.integer(substr(item_id, 2L, 2L))
      rank <- as.integer(sub("^s\\d", "", item_id))
      return((0.1 * set_id) + (0.22 * rank))
    }
    0
  }

  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    a_score <- scores[a]
    b_score <- scores[b]
    a_score <- if (!is.na(a_score)) as.double(a_score) else default_score(a)
    b_score <- if (!is.na(b_score)) as.double(b_score) else default_score(b)
    list(is_valid = TRUE, Y = as.integer(a_score >= b_score), invalid_reason = NA_character_)
  }
}

make_positive_probe_acceleration_runtime_state <- function() {
  withr::with_seed(20260320, {
    items <- tibble::tibble(
      item_id = c(
        paste0("h", seq_len(10L)),
        paste0("s2", seq_len(6L)),
        paste0("s3", seq_len(6L))
      ),
      set_id = c(rep(1L, 10L), rep(2L, 6L), rep(3L, 6L)),
      global_item_id = c(
        paste0("gh", seq_len(10L)),
        paste0("gs2", seq_len(6L)),
        paste0("gs3", seq_len(6L))
      )
    )
    state <- adaptive_rank_start(items, seed = 19L)
    state$warm_start_done <- TRUE
    state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())

    ids <- as.character(state$item_ids)
    draws <- matrix(seq_along(ids), nrow = 4L, ncol = length(ids), byrow = TRUE)
    colnames(draws) <- ids
    state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")

    artifacts <- lapply(sort(unique(as.integer(state$items$set_id))), function(set_id) {
      artifact <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
      if (!identical(as.integer(set_id), 1L)) {
        artifact$items$theta_raw_mean <- as.double(artifact$items$theta_raw_mean - 1)
      }
      artifact$quality_gate_accepted <- TRUE
      artifact
    })
    names(artifacts) <- as.character(sort(unique(as.integer(state$items$set_id))))

    fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
    judge <- make_linking_score_judge_fixture(c(
      h1 = -0.6, h2 = 0.0, h3 = 0.6,
      s21 = -0.3, s22 = 0.2, s23 = 1.0,
      s31 = -0.4, s32 = 0.1, s33 = 0.9
    ))

    adaptive_config <- list(
      run_mode = "link_multi_spoke",
      hub_id = 1L,
      multi_spoke_mode = "concurrent",
      min_cross_set_pairs_per_spoke_per_refit = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts,
      probe_pairs_per_refit_per_spoke = 1L,
      probe_pairs_per_refit_per_spoke_bootstrap_max = 3L,
      probe_edges_min_for_stop = 12L,
      probe_accel_bootstrap_target = 12L,
      probe_active_floor_min = 1L,
      probe_active_floor_frac = 0,
      probe_active_floor_requires_anchor_progress = FALSE
    )
    btl_config <- test_link_btl_config(list(refit_pairs_target = 4L))

    out <- adaptive_rank_run_live(
      state = state,
      judge = judge,
      n_steps = 24L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = adaptive_config,
      btl_config = btl_config,
      progress = "none"
    )

    accelerated_rows <- out$step_log[
      out$step_log$run_mode %in% "link_probe_holdout" &
        out$step_log$fallback_used %in% "probe_panel_acceleration",
      ,
      drop = FALSE
    ]
    if (nrow(accelerated_rows) < 1L) {
      rlang::abort(
        "Positive probe acceleration fixture failed to commit live accelerated holdout work."
      )
    }

    later_active_rows <- out$step_log[
      out$step_log$step_id > accelerated_rows$step_id[[1L]] &
        out$step_log$run_mode %in% "link_multi_spoke" &
        out$step_log$is_probe_step %in% FALSE,
      ,
      drop = FALSE
    ]
    if (nrow(later_active_rows) < 1L) {
      rlang::abort(
        "Positive probe acceleration fixture regressed into a probe-first regime after acceleration."
      )
    }

    accelerated_refits <- out$link_stage_log[
      out$link_stage_log$probe_acceleration_used %in% TRUE,
      ,
      drop = FALSE
    ]
    extra_chunks <- 0L
    while (nrow(accelerated_refits) < 1L &&
      extra_chunks < 4L &&
      !isTRUE(out$meta$stop_decision %||% FALSE)) {
      out <- adaptive_rank_run_live(
        state = out,
        judge = judge,
        n_steps = 8L,
        fit_fn = fit_stub$fit_fn,
        adaptive_config = adaptive_config,
        btl_config = btl_config,
        progress = "none"
      )
      accelerated_refits <- out$link_stage_log[
        out$link_stage_log$probe_acceleration_used %in% TRUE,
        ,
        drop = FALSE
      ]
      extra_chunks <- extra_chunks + 1L
    }
    if (nrow(accelerated_refits) < 1L) {
      rlang::abort(
        "Positive probe acceleration fixture failed to emit canonical accelerated link-stage rows."
      )
    }

    out
  })
}
