# -------------------------------------------------------------------------
# Bayesian BTL v3 (epsilon-lapse mixture) via CmdStanR.
# -------------------------------------------------------------------------

.btl_mcmc_v3_intish <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(abs(x - round(x)) < 1e-8)
}

.btl_mcmc_detect_cores <- function() {
  physical <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )
  logical <- tryCatch(
    parallel::detectCores(logical = TRUE),
    error = function(e) NA_integer_
  )
  if (is.na(physical) || physical < 1L) {
    physical <- NA_integer_
  } else {
    physical <- as.integer(physical)
  }
  if (is.na(logical) || logical < 1L) {
    logical <- NA_integer_
  } else {
    logical <- as.integer(logical)
  }
  effective <- physical
  if (is.na(effective) || effective < 1L) {
    effective <- logical
  }
  if (is.na(effective) || effective < 1L) {
    effective <- 1L
  }
  list(
    physical = physical,
    logical = logical,
    effective = as.integer(effective)
  )
}

.btl_mcmc_resolve_cmdstan_config <- function(cmdstan) {
  cmdstan <- cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`cmdstan` must be a list.")
  }

  threads_per_chain <- as.integer(cmdstan$threads_per_chain %||% 1L)
  if (is.na(threads_per_chain) || threads_per_chain < 1L) {
    rlang::abort("`cmdstan$threads_per_chain` must be a positive integer.")
  }

  core_fraction <- cmdstan$core_fraction %||% 0.8
  if (!is.numeric(core_fraction) || length(core_fraction) != 1L ||
    !is.finite(core_fraction) || core_fraction <= 0 || core_fraction > 1) {
    rlang::abort("`cmdstan$core_fraction` must be in (0, 1].")
  }

  cores <- .btl_mcmc_detect_cores()
  chains <- as.integer(cmdstan$chains %||% min(8L, cores$effective))
  if (is.na(chains) || chains < 1L) {
    return(list(
      chains = chains,
      parallel_chains = NA_integer_,
      core_fraction = as.double(core_fraction),
      cores_detected_physical = cores$physical,
      cores_detected_logical = cores$logical,
      threads_per_chain = as.integer(threads_per_chain),
      cmdstanr_version = .btl_mcmc_cmdstanr_version()
    ))
  }

  parallel_chains <- cmdstan$parallel_chains %||% NULL
  if (is.null(parallel_chains)) {
    core_budget <- max(1L, floor(cores$effective * core_fraction))
    parallel_chains <- min(chains, core_budget)
  } else {
    parallel_chains <- as.integer(parallel_chains)
    if (is.na(parallel_chains) || parallel_chains < 1L) {
      rlang::abort("`cmdstan$parallel_chains` must be a positive integer.")
    }
  }
  if (parallel_chains > chains) {
    parallel_chains <- chains
  }

  list(
    chains = chains,
    parallel_chains = as.integer(parallel_chains),
    core_fraction = as.double(core_fraction),
    cores_detected_physical = cores$physical,
    cores_detected_logical = cores$logical,
    threads_per_chain = as.integer(threads_per_chain),
    cmdstanr_version = .btl_mcmc_cmdstanr_version()
  )
}

.btl_mcmc_cmdstanr_version <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion("cmdstanr"))
}

.btl_validate_ids <- function(ids) {
  ids <- as.character(ids)
  if (length(ids) < 2L) {
    rlang::abort("`ids` must contain at least two ids.")
  }
  if (any(is.na(ids)) || any(ids == "")) {
    rlang::abort("`ids` must not contain missing or empty values.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.")
  }
  ids
}

.btl_mcmc_v3_validate_bt_data <- function(bt_data) {
  if (!is.list(bt_data)) {
    rlang::abort("`bt_data` must be a list.")
  }
  required <- c("A", "B", "Y", "N")
  missing <- setdiff(required, names(bt_data))
  if (length(missing) > 0L) {
    rlang::abort(paste0("`bt_data` is missing: ", paste(missing, collapse = ", "), "."))
  }

  N <- bt_data$N
  if (!.btl_mcmc_v3_intish(N) || length(N) != 1L || is.na(N) || N < 2L) {
    rlang::abort("`bt_data$N` must be an integer >= 2.")
  }
  N <- as.integer(N)

  A <- bt_data$A
  B <- bt_data$B
  Y <- bt_data$Y
  if (!.btl_mcmc_v3_intish(A) || !.btl_mcmc_v3_intish(B)) {
    rlang::abort("`bt_data$A` and `bt_data$B` must be integer vectors.")
  }
  if (!.btl_mcmc_v3_intish(Y)) {
    rlang::abort("`bt_data$Y` must be an integer vector.")
  }
  A <- as.integer(A)
  B <- as.integer(B)
  Y <- as.integer(Y)

  if (length(A) < 1L || length(B) < 1L || length(Y) < 1L) {
    rlang::abort("`bt_data` must contain at least one comparison.")
  }
  if (!identical(length(A), length(B)) || !identical(length(A), length(Y))) {
    rlang::abort("`bt_data$A`, `bt_data$B`, and `bt_data$Y` must match in length.")
  }
  if (any(A < 1L | A > N) || any(B < 1L | B > N)) {
    rlang::abort("`bt_data$A` and `bt_data$B` must be in 1..N.")
  }
  if (!all(Y %in% c(0L, 1L))) {
    rlang::abort("`bt_data$Y` must contain only 0/1 values.")
  }

  item_id <- bt_data$item_id %||% NULL
  if (!is.null(item_id)) {
    if (!is.character(item_id) || length(item_id) != N || any(is.na(item_id)) || any(item_id == "")) {
      rlang::abort("`bt_data$item_id` must be length N with non-empty strings.")
    }
  }

  list(
    A = A,
    B = B,
    Y = Y,
    N = N,
    item_id = item_id
  )
}

.btl_mcmc_v3_prepare_bt_data <- function(results, ids) {
  results <- tibble::as_tibble(results)
  required <- c("A_id", "B_id", "better_id")
  .adaptive_required_cols(results, "results", required)
  validate_results_tbl(results)

  ids <- .btl_validate_ids(ids)
  result_ids <- unique(c(results$A_id, results$B_id, results$better_id))
  missing_ids <- setdiff(result_ids, ids)
  if (length(missing_ids) > 0L) {
    rlang::abort("All ids in `results` must be contained in `ids`.")
  }

  A_idx <- match(results$A_id, ids)
  B_idx <- match(results$B_id, ids)
  if (any(is.na(A_idx)) || any(is.na(B_idx))) {
    rlang::abort("`results` contains ids not present in `ids`.")
  }

  Y <- as.integer(results$better_id == results$A_id)
  list(
    A = as.integer(A_idx),
    B = as.integer(B_idx),
    Y = Y,
    N = as.integer(length(ids)),
    item_id = as.character(ids)
  )
}

.btl_mcmc_v3_model_code <- function(path_override = NULL) {
  if (!is.null(path_override)) {
    if (!is.character(path_override) || length(path_override) != 1L || is.na(path_override)) {
      rlang::abort("`path_override` must be a length-1 character path or NULL.")
    }
  }
  model_file <- paste0("btl_mcmc_", "v3.stan")
  path <- if (is.null(path_override)) {
    system.file("stan", model_file, package = "pairwiseLLM")
  } else {
    path_override
  }
  if (!nzchar(path)) {
    rlang::abort(paste0("Stan model file `", model_file, "` not found."))
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

stan_file_for_variant <- function(model_variant) {
  model_variant <- normalize_model_variant(model_variant)
  path <- system.file("stan", paste0(model_variant, ".stan"), package = "pairwiseLLM")
  if (!nzchar(path)) {
    rlang::abort(paste0("Stan model file for `", model_variant, "` not found."))
  }
  path
}

.btl_mcmc_v3_unpack_draws <- function(draws, model_variant = NULL) {
  if (!is.null(model_variant)) {
    model_variant <- normalize_model_variant(model_variant)
  }
  has_e <- isTRUE(!is.null(model_variant) && model_has_e(model_variant))
  has_b <- isTRUE(!is.null(model_variant) && model_has_b(model_variant))

  epsilon_draws <- NULL
  beta_draws <- NULL

  if (is.list(draws) && (!is.null(draws$theta) || !is.null(draws$theta_draws))) {
    theta_draws <- draws$theta %||% draws$theta_draws
    epsilon_draws <- draws$epsilon %||% draws$epsilon_draws %||% NULL
    beta_draws <- draws$beta %||% draws$beta_draws %||% draws$b %||% draws$b_draws %||% NULL
  } else if (is.matrix(draws)) {
    cols <- colnames(draws)
    if (is.null(cols)) {
      rlang::abort("`draws` matrix must have column names.")
    }
    theta_cols <- grep("^theta\\[", cols)
    epsilon_col <- which(cols == "epsilon")
    beta_col <- which(cols == "beta")
    if (length(theta_cols) == 0L) {
      rlang::abort("`draws` matrix must include theta columns.")
    }
    theta_draws <- draws[, theta_cols, drop = FALSE]
    if (length(epsilon_col) == 1L) {
      epsilon_draws <- draws[, epsilon_col, drop = TRUE]
    }
    if (length(beta_col) == 1L) {
      beta_draws <- draws[, beta_col, drop = TRUE]
    }
  } else {
    rlang::abort("`draws` must be a list or matrix containing theta draws.")
  }

  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`draws$theta` must be a numeric matrix.")
  }
  if (has_e && is.null(epsilon_draws)) {
    rlang::abort("`draws` must include epsilon draws for this model variant.")
  }
  if (has_b && is.null(beta_draws)) {
    rlang::abort("`draws` must include beta draws for this model variant.")
  }
  if (!is.null(epsilon_draws) && !is.numeric(epsilon_draws)) {
    rlang::abort("`draws$epsilon` must be numeric.")
  }
  if (!is.null(beta_draws) && !is.numeric(beta_draws)) {
    rlang::abort("`draws$beta` must be numeric.")
  }

  list(
    theta_draws = theta_draws,
    epsilon_draws = if (!is.null(epsilon_draws)) as.double(epsilon_draws) else NULL,
    beta_draws = if (!is.null(beta_draws)) as.double(beta_draws) else NULL
  )
}

.btl_mcmc_v3_collect_diagnostics <- function(fit, model_variant) {
  model_variant <- normalize_model_variant(model_variant)
  diagnostics <- list(
    divergences = NA_integer_,
    max_rhat = NA_real_,
    min_ess_bulk = NA_real_,
    min_ess_tail = NA_real_
  )
  notes <- character()

  diag_tbl <- tryCatch(fit$diagnostic_summary(), error = function(e) NULL)
  if (!is.null(diag_tbl) && "num_divergent" %in% names(diag_tbl)) {
    divergences <- sum(diag_tbl$num_divergent, na.rm = TRUE)
    diagnostics$divergences <- as.integer(divergences)
    if (!is.finite(diagnostics$divergences)) {
      diagnostics$divergences <- NA_integer_
      notes <- c(notes, "Divergence count not finite.")
    }
  } else {
    notes <- c(notes, "CmdStan diagnostics missing num_divergent.")
  }

  vars <- c("theta")
  if (model_has_e(model_variant)) {
    vars <- c(vars, "epsilon")
  }
  if (model_has_b(model_variant)) {
    vars <- c(vars, "beta")
  }
  summary_tbl <- tryCatch(
    withCallingHandlers(
      fit$summary(variables = vars),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (!is.null(summary_tbl) && nrow(summary_tbl) > 0L) {
    if ("rhat" %in% names(summary_tbl)) {
      rhat_vals <- summary_tbl$rhat
      rhat_vals <- rhat_vals[is.finite(rhat_vals)]
      if (length(rhat_vals) > 0L) {
        diagnostics$max_rhat <- max(rhat_vals)
      } else {
        notes <- c(notes, "Rhat values missing or non-finite.")
      }
    } else {
      notes <- c(notes, "CmdStan summary missing rhat.")
    }

    if ("ess_bulk" %in% names(summary_tbl)) {
      ess_bulk_vals <- summary_tbl$ess_bulk
      ess_bulk_vals <- ess_bulk_vals[is.finite(ess_bulk_vals)]
      if (length(ess_bulk_vals) > 0L) {
        diagnostics$min_ess_bulk <- min(ess_bulk_vals)
      } else {
        notes <- c(notes, "ESS bulk values missing or non-finite.")
      }
    } else {
      notes <- c(notes, "CmdStan summary missing ess_bulk.")
    }

    if ("ess_tail" %in% names(summary_tbl)) {
      ess_tail_vals <- summary_tbl$ess_tail
      ess_tail_vals <- ess_tail_vals[is.finite(ess_tail_vals)]
      if (length(ess_tail_vals) > 0L) {
        diagnostics$min_ess_tail <- min(ess_tail_vals)
      } else {
        notes <- c(notes, "ESS tail values missing or non-finite.")
      }
    } else {
      notes <- c(notes, "CmdStan summary missing ess_tail.")
    }
  } else {
    notes <- c(notes, "CmdStan summary not available.")
  }

  if (length(notes) > 0L) {
    diagnostics$notes <- notes
  }
  diagnostics
}

.btl_mcmc_v3_theta_draws <- function(draws, item_id = NULL) {
  unpacked <- .btl_mcmc_v3_unpack_draws(draws)
  theta_draws <- unpacked$theta_draws
  if (!is.null(item_id)) {
    colnames(theta_draws) <- as.character(item_id)
  }
  theta_draws
}

#' @keywords internal
#' @noRd
.btl_mcmc_v3_infer_variant <- function(draws) {
  has_e <- FALSE
  has_b <- FALSE
  if (is.list(draws)) {
    has_e <- !is.null(draws$epsilon) || !is.null(draws$epsilon_draws)
    has_b <- !is.null(draws$beta) || !is.null(draws$beta_draws) ||
      !is.null(draws$b) || !is.null(draws$b_draws)
  } else if (is.matrix(draws)) {
    cols <- colnames(draws) %||% character()
    has_e <- any(cols == "epsilon")
    has_b <- any(cols == "beta")
  }
  if (isTRUE(has_e) && isTRUE(has_b)) {
    return("btl_e_b")
  }
  if (isTRUE(has_e)) {
    return("btl_e")
  }
  if (isTRUE(has_b)) {
    return("btl_b")
  }
  "btl"
}

summarize_draws <- function(draws, model_variant = NULL) {
  if (is.null(model_variant)) {
    model_variant <- .btl_mcmc_v3_infer_variant(draws)
  }
  model_variant <- normalize_model_variant(model_variant)
  unpacked <- .btl_mcmc_v3_unpack_draws(draws, model_variant = model_variant)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(unpacked$theta_draws, name = "theta_draws")
  epsilon_draws <- unpacked$epsilon_draws %||% NULL

  item_id <- colnames(theta_draws)
  if (is.null(item_id)) {
    item_id <- as.character(seq_len(ncol(theta_draws)))
  }

  theta_summary <- tibble::tibble(
    item_id = as.character(item_id),
    theta_mean = as.double(colMeans(theta_draws)),
    theta_sd = as.double(apply(theta_draws, 2, stats::sd)),
    theta_ci90_low = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.05, names = FALSE)),
    theta_ci90_high = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.95, names = FALSE)),
    theta_ci95_low = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.025, names = FALSE)),
    theta_ci95_high = as.double(apply(theta_draws, 2, stats::quantile, probs = 0.975, names = FALSE))
  )

  if (model_has_e(model_variant)) {
    epsilon_draws <- as.double(epsilon_draws)
    if (any(!is.finite(epsilon_draws))) {
      rlang::warn("Non-finite values detected in `epsilon_draws`; dropping before summarising.")
      epsilon_draws <- epsilon_draws[is.finite(epsilon_draws)]
    }
    if (length(epsilon_draws) < 2L) {
      rlang::abort("`epsilon_draws` must contain at least two finite values.")
    }
    epsilon_summary <- tibble::tibble(
      epsilon_mean = as.double(mean(epsilon_draws)),
      epsilon_p2.5 = as.double(stats::quantile(epsilon_draws, probs = 0.025, names = FALSE)),
      epsilon_p5 = as.double(stats::quantile(epsilon_draws, probs = 0.05, names = FALSE)),
      epsilon_p50 = as.double(stats::quantile(epsilon_draws, probs = 0.5, names = FALSE)),
      epsilon_p95 = as.double(stats::quantile(epsilon_draws, probs = 0.95, names = FALSE)),
      epsilon_p97.5 = as.double(stats::quantile(epsilon_draws, probs = 0.975, names = FALSE))
    )
  } else {
    epsilon_summary <- tibble::tibble(
      epsilon_mean = NA_real_,
      epsilon_p2.5 = NA_real_,
      epsilon_p5 = NA_real_,
      epsilon_p50 = NA_real_,
      epsilon_p95 = NA_real_,
      epsilon_p97.5 = NA_real_
    )
  }

  list(
    theta_summary = theta_summary,
    epsilon_summary = epsilon_summary
  )
}

#' @keywords internal
#' @noRd
as_v3_fit_contract_from_mcmc <- function(mcmc_fit, ids) {
  if (!is.list(mcmc_fit)) {
    rlang::abort("`mcmc_fit` must be a list.")
  }
  ids <- as.character(ids)
  if (length(ids) < 1L || anyNA(ids) || any(ids == "")) {
    rlang::abort("`ids` must be a non-empty character vector.")
  }
  if (anyDuplicated(ids)) {
    rlang::abort("`ids` must be unique.")
  }

  model_variant <- normalize_model_variant(mcmc_fit$model_variant %||% "btl_e_b")
  draws <- mcmc_fit$draws %||% NULL
  if (is.null(draws) || !is.list(draws)) {
    rlang::abort("`mcmc_fit$draws` must be a list.")
  }
  theta_draws <- draws$theta %||% draws$theta_draws %||% NULL
  if (is.null(theta_draws) || !is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`mcmc_fit$draws$theta` must be a numeric matrix.")
  }
  if (is.null(colnames(theta_draws))) {
    rlang::abort("`mcmc_fit$draws$theta` must have column names.")
  }
  theta_draws <- reorder_theta_draws(theta_draws, ids)

  epsilon_draws <- NULL
  if (model_has_e(model_variant)) {
    epsilon_draws <- draws$epsilon %||% draws$epsilon_draws %||% NULL
    if (is.null(epsilon_draws) || !is.numeric(epsilon_draws)) {
      rlang::abort("`mcmc_fit$draws$epsilon` must be numeric when provided.")
    }
  }

  beta_draws <- NULL
  if (model_has_b(model_variant)) {
    beta_draws <- draws$beta %||% draws$beta_draws %||% draws$b %||% draws$b_draws %||% NULL
    if (is.null(beta_draws) || !is.numeric(beta_draws)) {
      rlang::abort("`mcmc_fit$draws$beta` must be numeric when provided.")
    }
  }

  build_v3_fit_contract(
    theta_draws = theta_draws,
    epsilon_draws = epsilon_draws,
    beta_draws = beta_draws,
    diagnostics = mcmc_fit$diagnostics %||% NULL,
    model_variant = model_variant,
    mcmc_config_used = mcmc_fit$mcmc_config_used %||% NULL
  )
}

#' @keywords internal
#' @noRd
.fit_bayes_btl_mcmc_adaptive <- function(bt_data, config, seed = NULL) {
  .btl_mcmc_require_cmdstanr()

  if (!is.list(config)) {
    rlang::abort("`config` must be a list.")
  }
  validate_config(config)
  model_variant <- normalize_model_variant(config$model_variant %||% "btl_e_b")

  bt_data <- .btl_mcmc_v3_validate_bt_data(bt_data)
  M <- length(bt_data$A)
  stan_data <- list(
    N = as.integer(bt_data$N),
    M = as.integer(M),
    A = as.integer(bt_data$A),
    B = as.integer(bt_data$B),
    Y = as.integer(bt_data$Y)
  )

  cmdstan <- config$cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`config$cmdstan` must be a list when provided.")
  }
  resolved_cmdstan <- .btl_mcmc_resolve_cmdstan_config(cmdstan)
  chains <- resolved_cmdstan$chains
  iter_warmup <- as.integer(cmdstan$iter_warmup %||% 1000L)
  iter_sampling <- as.integer(cmdstan$iter_sampling %||% 1000L)
  output_dir <- cmdstan$output_dir %||% NULL
  parallel_chains <- resolved_cmdstan$parallel_chains

  if (any(is.na(c(chains, iter_warmup, iter_sampling))) || chains < 1L ||
    iter_warmup < 1L || iter_sampling < 1L) {
    rlang::abort("CmdStan settings must be positive integers.")
  }
  if (is.na(parallel_chains) || parallel_chains < 1L) {
    rlang::abort("`cmdstan$parallel_chains` must be a positive integer.")
  }

  stan_file <- stan_file_for_variant(model_variant)
  model <- cmdstanr::cmdstan_model(stan_file)

  sample_args <- list(
    data = stan_data,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
    threads_per_chain = resolved_cmdstan$threads_per_chain,
    refresh = 0
  )
  if (!is.null(seed)) {
    sample_args$seed <- as.integer(seed)
  }
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
      rlang::abort("`config$cmdstan$output_dir` must be a length-1 character path.")
    }
    sample_args$output_dir <- output_dir
  }

  fit <- do.call(model$sample, sample_args)

  vars <- c("theta")
  if (model_has_e(model_variant)) {
    vars <- c(vars, "epsilon")
  }
  if (model_has_b(model_variant)) {
    vars <- c(vars, "beta")
  }
  draws_matrix <- fit$draws(variables = vars, format = "matrix")
  theta_cols <- paste0("theta[", seq_len(bt_data$N), "]")
  if (!all(theta_cols %in% colnames(draws_matrix))) {
    rlang::abort("CmdStan output missing theta draws.")
  }
  if (model_has_e(model_variant) && !"epsilon" %in% colnames(draws_matrix)) {
    rlang::abort("CmdStan output missing epsilon draws.")
  }
  if (model_has_b(model_variant) && !"beta" %in% colnames(draws_matrix)) {
    rlang::abort("CmdStan output missing beta draws.")
  }

  theta_draws <- draws_matrix[, theta_cols, drop = FALSE]
  colnames(theta_draws) <- bt_data$item_id %||% as.character(seq_len(bt_data$N))
  epsilon_draws <- if (model_has_e(model_variant)) {
    draws_matrix[, "epsilon", drop = TRUE]
  } else {
    NULL
  }
  beta_draws <- if (model_has_b(model_variant)) {
    draws_matrix[, "beta", drop = TRUE]
  } else {
    NULL
  }

  thin_draws <- as.integer(config$thin_draws %||% 1L)
  if (is.na(thin_draws) || thin_draws < 1L) {
    rlang::abort("`config$thin_draws` must be a positive integer.")
  }
  if (thin_draws > 1L) {
    keep_idx <- seq(1L, nrow(theta_draws), by = thin_draws)
    theta_draws <- theta_draws[keep_idx, , drop = FALSE]
    if (!is.null(epsilon_draws)) {
      epsilon_draws <- epsilon_draws[keep_idx]
    }
    if (!is.null(beta_draws)) {
      beta_draws <- beta_draws[keep_idx]
    }
  }

  draws <- list(
    theta = theta_draws,
    epsilon = if (is.null(epsilon_draws)) NULL else as.double(epsilon_draws),
    beta = if (is.null(beta_draws)) NULL else as.double(beta_draws)
  )

  summaries <- summarize_draws(draws, model_variant = model_variant)
  diagnostics <- .btl_mcmc_v3_collect_diagnostics(fit, model_variant = model_variant)

  list(
    draws = draws,
    theta_summary = summaries$theta_summary,
    epsilon_summary = summaries$epsilon_summary,
    diagnostics = diagnostics,
    mcmc_config_used = resolved_cmdstan,
    model_variant = model_variant
  )
}
