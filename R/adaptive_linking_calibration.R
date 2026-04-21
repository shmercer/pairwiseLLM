# -------------------------------------------------------------------------
# Offline linking calibration helpers.
# -------------------------------------------------------------------------

.adaptive_calibration_function_surface <- function(fn) {
  env <- environment(fn)
  env_label <- "closure"
  if (identical(env, emptyenv())) {
    env_label <- "emptyenv"
  } else if (identical(env, baseenv())) {
    env_label <- "baseenv"
  } else if (identical(env, globalenv())) {
    env_label <- "globalenv"
  } else if (isNamespace(env)) {
    env_label <- paste0("namespace:", getNamespaceName(env))
  }

  list(
    kind = "function",
    formals = .adaptive_calibration_canonicalize(as.list(formals(fn))),
    body = paste(deparse(body(fn), width.cutoff = 500L), collapse = "\n"),
    environment = env_label
  )
}

.adaptive_calibration_canonicalize <- function(x) {
  if (is.function(x)) {
    return(.adaptive_calibration_function_surface(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    nm <- names(x)
    if (is.null(nm)) {
      return(lapply(x, .adaptive_calibration_canonicalize))
    }
    ord <- order(nm)
    x <- x[ord]
    return(stats::setNames(lapply(x, .adaptive_calibration_canonicalize), names(x)))
  }
  if (is.data.frame(x)) {
    out <- as.list(x)
    out <- lapply(out, .adaptive_calibration_canonicalize)
    return(out)
  }
  x
}

.adaptive_calibration_hash_object <- function(x) {
  tmp <- tempfile("adaptive_calibration_hash_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(.adaptive_calibration_canonicalize(x), tmp)
  as.character(unname(tools::md5sum(tmp)))
}

.adaptive_calibration_default_artifact_path <- function() {
  path <- system.file("extdata", "adaptive_linking_calibration_default.json", package = "pairwiseLLM")
  if (is.character(path) && length(path) == 1L && nzchar(path) && file.exists(path)) {
    return(path)
  }
  src_path <- file.path("inst", "extdata", "adaptive_linking_calibration_default.json")
  if (file.exists(src_path)) {
    return(src_path)
  }
  NA_character_
}

.adaptive_linking_default_calibration <- function() {
  fallback <- list(
    cross_set_ppc_brier_max = 0.20,
    ppc_calibration_id = "default_p95_brier_active"
  )
  path <- .adaptive_calibration_default_artifact_path()
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(fallback)
  }
  payload <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(payload) || !is.list(payload)) {
    return(fallback)
  }

  threshold <- as.double(payload$cross_set_ppc_brier_max %||% NA_real_)
  calib_id <- as.character(payload$ppc_calibration_id %||% NA_character_)
  if (!is.finite(threshold) || threshold < 0 || threshold > 1) {
    threshold <- fallback$cross_set_ppc_brier_max
  }
  if (!is.character(calib_id) || length(calib_id) != 1L || is.na(calib_id) || calib_id == "") {
    calib_id <- fallback$ppc_calibration_id
  }

  list(
    cross_set_ppc_brier_max = as.double(threshold),
    ppc_calibration_id = as.character(calib_id)
  )
}

.adaptive_calibration_parse_set_sizes <- function(set_sizes) {
  if (!.adaptive_is_integerish(set_sizes) || length(set_sizes) != 2L || any(is.na(set_sizes))) {
    rlang::abort("`set_sizes` must be an integer vector of length 2: c(hub_n, spoke_n).")
  }
  out <- as.integer(set_sizes)
  if (any(out < 2L)) {
    rlang::abort("Each element of `set_sizes` must be >= 2.")
  }
  out
}

.adaptive_calibration_build_items <- function(set_sizes) {
  set_sizes <- .adaptive_calibration_parse_set_sizes(set_sizes)
  hub_n <- as.integer(set_sizes[[1L]])
  spoke_n <- as.integer(set_sizes[[2L]])
  hub_ids <- paste0("h", seq_len(hub_n))
  spoke_ids <- paste0("s", seq_len(spoke_n))
  tibble::tibble(
    item_id = c(hub_ids, spoke_ids),
    set_id = c(rep.int(1L, hub_n), rep.int(2L, spoke_n)),
    global_item_id = c(paste0("g", hub_ids), paste0("g", spoke_ids))
  )
}

.adaptive_calibration_truth <- function(items, seed, true_delta, true_alpha) {
  items <- tibble::as_tibble(items)
  seed <- as.integer(seed)
  if (!is.finite(seed) || is.na(seed)) {
    rlang::abort("`seed` must be a single integer.")
  }
  true_delta <- as.double(true_delta)
  true_alpha <- as.double(true_alpha)
  if (!is.finite(true_delta)) {
    rlang::abort("`true_delta` must be finite.")
  }
  if (!is.finite(true_alpha) || true_alpha <= 0) {
    rlang::abort("`true_alpha` must be finite and > 0.")
  }

  ids <- as.character(items$item_id)
  set_ids <- as.integer(items$set_id)
  hub_ids <- ids[set_ids == 1L]
  spoke_ids <- ids[set_ids == 2L]

  withr::with_seed(seed, {
    hub_raw <- stats::setNames(stats::rnorm(length(hub_ids), mean = 0, sd = 1), hub_ids)
    spoke_raw <- stats::setNames(stats::rnorm(length(spoke_ids), mean = 0, sd = 1), spoke_ids)
    theta_raw <- c(hub_raw, spoke_raw)
    theta_global <- c(hub_raw, (true_alpha * spoke_raw) + true_delta)
    list(
      theta_raw = theta_raw,
      theta_global = theta_global,
      true_delta = true_delta,
      true_alpha = true_alpha
    )
  })
}

.adaptive_calibration_judge <- function(theta_global, judge_b, judge_eps, judge_seed) {
  theta_names <- names(theta_global)
  theta_global <- as.double(theta_global)
  names(theta_global) <- theta_names
  judge_b <- as.double(judge_b)
  judge_eps <- as.double(judge_eps)
  judge_seed <- as.integer(judge_seed)
  if (!is.finite(judge_b)) {
    rlang::abort("`judge_b` must be finite.")
  }
  if (!is.finite(judge_eps) || judge_eps < 0 || judge_eps >= 1) {
    rlang::abort("`judge_eps` must be in [0, 1).")
  }

  function(A, B, state, ...) {
    step_id <- as.integer(nrow(state$step_log %||% tibble::tibble()) + 1L)
    a_id <- as.character(A$item_id[[1L]])
    b_id <- as.character(B$item_id[[1L]])
    theta_a <- as.double(theta_global[a_id][[1L]] %||% NA_real_)
    theta_b <- as.double(theta_global[b_id][[1L]] %||% NA_real_)
    if (!is.finite(theta_a) || !is.finite(theta_b)) {
      return(list(is_valid = FALSE, Y = NA_integer_, invalid_reason = "missing_true_theta"))
    }

    p_a <- (1 - judge_eps) * stats::plogis(theta_a - theta_b + judge_b) + (judge_eps * 0.5)
    key <- paste0(a_id, "|", b_id, "|", step_id)
    seed_offset <- sum(utf8ToInt(key) * seq_along(utf8ToInt(key)))
    u <- withr::with_seed(as.integer(judge_seed + seed_offset), stats::runif(1L))

    list(
      is_valid = TRUE,
      Y = as.integer(u < p_a),
      invalid_reason = NA_character_
    )
  }
}

.adaptive_calibration_fit_fn <- function(theta_raw, fit_seed, n_draws = 64L, draw_sd = 0.15) {
  theta_names <- names(theta_raw)
  theta_raw <- as.double(theta_raw)
  names(theta_raw) <- theta_names
  fit_seed <- as.integer(fit_seed)
  n_draws <- as.integer(n_draws)
  draw_sd <- as.double(draw_sd)
  if (!is.finite(draw_sd) || draw_sd <= 0) {
    rlang::abort("`draw_sd` must be > 0.")
  }
  if (!is.finite(n_draws) || n_draws < 20L) {
    rlang::abort("`n_draws` must be >= 20.")
  }

  function(state, config) {
    ids <- as.character(state$item_ids)
    mu <- as.double(theta_raw[ids])
    if (any(!is.finite(mu))) {
      rlang::abort("Calibration fit function could not resolve theta values for all state items.")
    }
    draw_seed <- as.integer(fit_seed + as.integer(nrow(state$step_log %||% tibble::tibble())))
    draws <- withr::with_seed(draw_seed, {
      mat <- matrix(
        stats::rnorm(length(ids) * n_draws, mean = rep(mu, each = n_draws), sd = draw_sd),
        nrow = n_draws,
        byrow = FALSE
      )
      colnames(mat) <- ids
      mat
    })
    list(
      btl_posterior_draws = draws,
      theta_mean = stats::setNames(as.double(colMeans(draws)), ids),
      theta_sd = stats::setNames(as.double(apply(draws, 2, stats::sd)), ids),
      diagnostics = list(
        divergences = 0L,
        max_rhat = 1.0,
        min_ess_bulk = max(1000, n_draws)
      ),
      model_variant = "btl_e_b",
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
      mcmc_config_used = config %||% list()
    )
  }
}

.adaptive_calibration_extract_replicate_metrics <- function(state, replicate_id) {
  link_log <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  if (nrow(link_log) < 1L) {
    return(tibble::tibble(
      replicate_id = integer(),
      refit_id = integer(),
      spoke_id = integer(),
      ppc_brier_cross_active = double(),
      n_cross_edges_active_since_last_refit = integer(),
      eligible = logical()
    ))
  }
  out <- tibble::tibble(
    replicate_id = as.integer(replicate_id),
    refit_id = as.integer(link_log$refit_id),
    spoke_id = as.integer(link_log$spoke_id),
    ppc_brier_cross_active = as.double(link_log$ppc_brier_cross_active),
    n_cross_edges_active_since_last_refit = as.integer(link_log$n_cross_edges_active_since_last_refit %||% NA_integer_),
    eligible = is.finite(as.double(link_log$ppc_brier_cross_active)) &
      as.integer(link_log$n_cross_edges_active_since_last_refit %||% 0L) > 0L
  )
  out
}

.adaptive_calibration_run_replicate <- function(replicate_id,
                                                seed,
                                                set_sizes,
                                                true_delta,
                                                true_alpha,
                                                judge_b,
                                                judge_eps,
                                                n_steps,
                                                btl_config = NULL,
                                                adaptive_config = NULL,
                                                progress = "none") {
  replicate_id <- as.integer(replicate_id)
  seed <- as.integer(seed)
  n_steps <- as.integer(n_steps)
  if (!is.finite(n_steps) || n_steps < 1L) {
    rlang::abort("`n_steps` must be >= 1.")
  }

  items <- .adaptive_calibration_build_items(set_sizes)
  rep_seed <- as.integer(seed + (replicate_id * 10007L))
  truth <- .adaptive_calibration_truth(items, seed = rep_seed, true_delta = true_delta, true_alpha = true_alpha)
  judge <- .adaptive_calibration_judge(
    theta_global = truth$theta_global,
    judge_b = judge_b,
    judge_eps = judge_eps,
    judge_seed = as.integer(rep_seed + 101L)
  )
  fit_fn <- .adaptive_calibration_fit_fn(
    theta_raw = truth$theta_raw,
    fit_seed = as.integer(rep_seed + 303L)
  )

  run_cfg <- utils::modifyList(
    list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run",
      link_estimation_mode = "transform",
      link_transform_policy = "auto",
      hub_lock_mode = "soft_lock",
      probe_pairs_per_refit_per_spoke = 2L,
      cross_set_utility = "linking_d_optimal"
    ),
    adaptive_config %||% list()
  )
  run_btl <- utils::modifyList(
    list(
      refit_pairs_target = 1L,
      stability_lag = 1L,
      eap_reliability_min = 0,
      theta_corr_min = 0,
      rank_spearman_min = 0
    ),
    btl_config %||% list()
  )

  state <- adaptive_rank_start(items = items, seed = rep_seed, adaptive_config = run_cfg)
  state <- adaptive_rank_run_live(
    state = state,
    judge = judge,
    n_steps = n_steps,
    fit_fn = fit_fn,
    adaptive_config = run_cfg,
    btl_config = run_btl,
    progress = progress
  )

  list(
    state = state,
    metrics = .adaptive_calibration_extract_replicate_metrics(state, replicate_id = replicate_id)
  )
}

.adaptive_calibration_summarize <- function(metrics, replicate_count, seed, config_payload) {
  metrics <- tibble::as_tibble(metrics)
  eligible <- metrics[metrics$eligible %in% TRUE, , drop = FALSE]
  if (nrow(eligible) < 1L) {
    rlang::abort("Calibration produced no eligible refit-level `ppc_brier_cross_active` values.")
  }

  values <- as.double(eligible$ppc_brier_cross_active)
  threshold <- as.double(stats::quantile(values, probs = 0.95, na.rm = TRUE, names = FALSE, type = 7))
  stats_tbl <- tibble::tibble(
    quantile_p95 = as.double(threshold),
    mean = as.double(mean(values)),
    sd = as.double(stats::sd(values)),
    min = as.double(min(values)),
    max = as.double(max(values)),
    n_eligible_rows = as.integer(length(values)),
    n_replicates = as.integer(replicate_count),
    n_replicates_with_eligible = as.integer(length(unique(eligible$replicate_id))),
    n_replicates_without_eligible = as.integer(replicate_count - length(unique(eligible$replicate_id)))
  )

  sidecar_payload <- list(
    cross_set_ppc_brier_max = as.double(threshold),
    ppc_calibration_id = .adaptive_calibration_hash_object(list(config = config_payload, seed = as.integer(seed))),
    calibration_quantile = 0.95,
    run_metadata = list(
      replicate_count = as.integer(replicate_count),
      seed = as.integer(seed),
      timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    summary_stats = as.list(stats_tbl[1, , drop = FALSE]),
    config = config_payload
  )

  list(summary = stats_tbl, sidecar = sidecar_payload)
}

.adaptive_calibration_write_artifacts <- function(summary_tbl, metrics_tbl, sidecar_payload, output_dir) {
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || output_dir == "") {
    rlang::abort("`output_dir` must be a single, non-empty string.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  summary_path <- file.path(output_dir, "link_ppc_brier_calibration_summary.csv")
  replicate_path <- file.path(output_dir, "link_ppc_brier_calibration_replicates.csv")
  sidecar_path <- file.path(output_dir, "link_ppc_brier_calibration_summary.json")

  utils::write.csv(summary_tbl, summary_path, row.names = FALSE)
  utils::write.csv(metrics_tbl, replicate_path, row.names = FALSE)
  jsonlite::write_json(sidecar_payload, sidecar_path, auto_unbox = TRUE, pretty = TRUE)

  list(
    summary_csv = summary_path,
    replicates_csv = replicate_path,
    sidecar_json = sidecar_path
  )
}

.adaptive_linking_calibrate_offline <- function(replicates = 10L,
                                                seed = 1L,
                                                set_sizes = c(8L, 8L),
                                                true_delta = -0.5,
                                                true_alpha = 1.0,
                                                judge_b = 0.0,
                                                judge_eps = 0.05,
                                                n_steps = 120L,
                                                btl_config = NULL,
                                                adaptive_config = NULL,
                                                output_dir = NULL,
                                                progress = "none") {
  replicates <- as.integer(replicates)
  seed <- as.integer(seed)
  if (!is.finite(replicates) || replicates < 1L) {
    rlang::abort("`replicates` must be >= 1.")
  }

  set_sizes <- .adaptive_calibration_parse_set_sizes(set_sizes)
  adaptive_cfg <- adaptive_config %||% list()
  cfg_payload <- list(
    set_sizes = as.integer(set_sizes),
    true_transform = list(delta = as.double(true_delta), alpha = as.double(true_alpha)),
    judge_settings = list(b = as.double(judge_b), eps = as.double(judge_eps), model = "model_d"),
    d_opt_knobs = list(lambda = 1e-6, ordering_mode = "linking_d_optimal"),
    probe_pairs_per_refit_per_spoke = as.integer(adaptive_cfg$probe_pairs_per_refit_per_spoke %||% 2L),
    stopping_and_quota = list(
      link_transform_escalation_window_refits = as.integer(
        adaptive_cfg$link_transform_escalation_window_refits %||%
          adaptive_cfg$link_transform_escalation_refits_required %||%
          3L
      ),
      link_transform_escalation_passes_required = as.integer(
        adaptive_cfg$link_transform_escalation_passes_required %||%
          adaptive_cfg$link_transform_escalation_refits_required %||%
          2L
      ),
      min_cross_set_pairs_per_spoke_per_refit = as.integer(
        adaptive_cfg$min_cross_set_pairs_per_spoke_per_refit %||% 5L
      ),
      btl_config = btl_config %||% list()
    ),
    code_knobs = list(
      run_mode = "link_one_spoke",
      phase_a_mode = "run",
      cross_set_utility = "linking_d_optimal"
    )
  )
  withr::with_seed(seed, {
    replicate_rows <- vector("list", replicates)
    for (idx in seq_len(replicates)) {
      rep_out <- .adaptive_calibration_run_replicate(
        replicate_id = idx,
        seed = seed,
        set_sizes = set_sizes,
        true_delta = true_delta,
        true_alpha = true_alpha,
        judge_b = judge_b,
        judge_eps = judge_eps,
        n_steps = n_steps,
        btl_config = btl_config,
        adaptive_config = adaptive_cfg,
        progress = progress
      )
      replicate_rows[[idx]] <- rep_out$metrics
    }
    replicate_tbl <- dplyr::bind_rows(replicate_rows) |>
      dplyr::arrange(.data$replicate_id, .data$refit_id, .data$spoke_id)

    agg <- .adaptive_calibration_summarize(
      metrics = replicate_tbl,
      replicate_count = replicates,
      seed = seed,
      config_payload = cfg_payload
    )
    sidecar <- agg$sidecar
    summary_tbl <- dplyr::bind_cols(
      tibble::tibble(
        ppc_calibration_id = as.character(sidecar$ppc_calibration_id),
        cross_set_ppc_brier_max = as.double(sidecar$cross_set_ppc_brier_max)
      ),
      agg$summary
    )

    files <- NULL
    if (is.character(output_dir) && length(output_dir) == 1L && !is.na(output_dir) && nzchar(output_dir)) {
      files <- .adaptive_calibration_write_artifacts(
        summary_tbl = summary_tbl,
        metrics_tbl = replicate_tbl,
        sidecar_payload = sidecar,
        output_dir = output_dir
      )
    }

    list(
      summary = summary_tbl,
      replicate_metrics = replicate_tbl,
      sidecar = sidecar,
      files = files
    )
  })
}
