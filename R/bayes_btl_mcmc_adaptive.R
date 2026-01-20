# -------------------------------------------------------------------------
# Bayesian BTL v3 (epsilon-lapse mixture) via CmdStanR.
# -------------------------------------------------------------------------

.btl_mcmc_v3_intish <- function(x) {
  is.numeric(x) && all(is.finite(x)) && all(abs(x - round(x)) < 1e-8)
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

  ids <- .btl_fast_validate_ids(ids)
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

.btl_mcmc_v3_model_code <- function() {
  model_file <- paste0("btl_mcmc_", "v3.stan")
  path <- system.file("stan", model_file, package = "pairwiseLLM")
  if (!nzchar(path)) {
    rlang::abort(paste0("Stan model file `", model_file, "` not found."))
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

.btl_mcmc_v3_unpack_draws <- function(draws) {
  if (is.list(draws) &&
    (!is.null(draws$theta) || !is.null(draws$theta_draws)) &&
    (!is.null(draws$epsilon) || !is.null(draws$epsilon_draws))) {
    theta_draws <- draws$theta %||% draws$theta_draws
    epsilon_draws <- draws$epsilon %||% draws$epsilon_draws
  } else if (is.matrix(draws)) {
    cols <- colnames(draws)
    if (is.null(cols)) {
      rlang::abort("`draws` matrix must have column names.")
    }
    theta_cols <- grep("^theta\\[", cols)
    epsilon_col <- which(cols == "epsilon")
    if (length(theta_cols) == 0L || length(epsilon_col) != 1L) {
      rlang::abort("`draws` matrix must include theta and epsilon columns.")
    }
    theta_draws <- draws[, theta_cols, drop = FALSE]
    epsilon_draws <- draws[, epsilon_col, drop = TRUE]
  } else {
    rlang::abort("`draws` must be a list or matrix containing theta and epsilon draws.")
  }

  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`draws$theta` must be a numeric matrix.")
  }
  if (!is.numeric(epsilon_draws)) {
    rlang::abort("`draws$epsilon` must be numeric.")
  }

  list(theta_draws = theta_draws, epsilon_draws = as.double(epsilon_draws))
}

.btl_mcmc_v3_collect_diagnostics <- function(fit) {
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

  summary_tbl <- tryCatch(fit$summary(variables = c("theta", "epsilon")), error = function(e) NULL)
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
summarize_draws <- function(draws) {
  unpacked <- .btl_mcmc_v3_unpack_draws(draws)
  theta_draws <- .pairwiseLLM_sanitize_draws_matrix(unpacked$theta_draws, name = "theta_draws")
  epsilon_draws <- unpacked$epsilon_draws

  item_id <- colnames(theta_draws)
  if (is.null(item_id)) {
    item_id <- as.character(seq_len(ncol(theta_draws)))
  }

  epsilon_draws <- as.double(epsilon_draws)
  if (any(!is.finite(epsilon_draws))) {
    rlang::warn("Non-finite values detected in `epsilon_draws`; dropping before summarising.")
    epsilon_draws <- epsilon_draws[is.finite(epsilon_draws)]
  }
  if (length(epsilon_draws) < 2L) {
    rlang::abort("`epsilon_draws` must contain at least two finite values.")
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

  epsilon_summary <- tibble::tibble(
    epsilon_mean = as.double(mean(epsilon_draws)),
    epsilon_ci90_low = as.double(stats::quantile(epsilon_draws, probs = 0.05, names = FALSE)),
    epsilon_ci90_high = as.double(stats::quantile(epsilon_draws, probs = 0.95, names = FALSE)),
    epsilon_ci95_low = as.double(stats::quantile(epsilon_draws, probs = 0.025, names = FALSE)),
    epsilon_ci95_high = as.double(stats::quantile(epsilon_draws, probs = 0.975, names = FALSE))
  )

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

  theta_summary <- mcmc_fit$theta_summary %||% NULL
  if (is.null(theta_summary) || !is.data.frame(theta_summary)) {
    rlang::abort("`mcmc_fit$theta_summary` must be a data frame.")
  }
  required <- c("item_id", "theta_mean")
  .adaptive_required_cols(theta_summary, "theta_summary", required)
  theta_ids <- as.character(theta_summary$item_id)
  if (anyNA(theta_ids) || any(theta_ids == "")) {
    rlang::abort("`theta_summary$item_id` must be non-missing.")
  }
  idx <- match(ids, theta_ids)
  if (anyNA(idx)) {
    rlang::abort("`theta_summary$item_id` must cover all `ids`.")
  }
  theta_mean <- as.double(theta_summary$theta_mean[idx])
  names(theta_mean) <- ids

  epsilon_summary <- mcmc_fit$epsilon_summary %||% NULL
  if (is.null(epsilon_summary) || !is.data.frame(epsilon_summary)) {
    rlang::abort("`mcmc_fit$epsilon_summary` must be a data frame.")
  }
  if (!"epsilon_mean" %in% names(epsilon_summary)) {
    rlang::abort("`mcmc_fit$epsilon_summary` must include `epsilon_mean`.")
  }
  epsilon_mean <- epsilon_summary$epsilon_mean[[1L]]
  if (!is.numeric(epsilon_mean) || length(epsilon_mean) != 1L || !is.finite(epsilon_mean)) {
    rlang::abort("`mcmc_fit$epsilon_summary$epsilon_mean` must be a finite numeric scalar.")
  }

  fit <- list(
    theta_draws = theta_draws,
    theta_mean = theta_mean,
    epsilon_mean = as.double(epsilon_mean),
    diagnostics = mcmc_fit$diagnostics %||% NULL,
    raw_mcmc_fit = mcmc_fit
  )
  validate_v3_fit_contract(fit, ids)
  fit
}

#' @keywords internal
#' @noRd
.fit_bayes_btl_mcmc_adaptive <- function(bt_data, config, seed = NULL) {
  .btl_mcmc_require_cmdstanr()

  if (!is.list(config)) {
    rlang::abort("`config` must be a list.")
  }
  validate_config(config)

  bt_data <- .btl_mcmc_v3_validate_bt_data(bt_data)
  K <- length(bt_data$A)
  stan_data <- list(
    N = as.integer(bt_data$N),
    K = as.integer(K),
    A = as.integer(bt_data$A),
    B = as.integer(bt_data$B),
    Y = as.integer(bt_data$Y)
  )

  cmdstan <- config$cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`config$cmdstan` must be a list when provided.")
  }
  chains <- as.integer(cmdstan$chains %||% 4L)
  iter_warmup <- as.integer(cmdstan$iter_warmup %||% 1000L)
  iter_sampling <- as.integer(cmdstan$iter_sampling %||% 1000L)
  core_fraction <- cmdstan$core_fraction %||% 0.6
  output_dir <- cmdstan$output_dir %||% NULL
  parallel_chains <- compute_core_budget(core_fraction = core_fraction, min_cores = 1L)

  if (any(is.na(c(chains, iter_warmup, iter_sampling))) || chains < 1L ||
    iter_warmup < 1L || iter_sampling < 1L) {
    rlang::abort("CmdStan settings must be positive integers.")
  }

  stan_file <- cmdstanr::write_stan_file(.btl_mcmc_v3_model_code())
  model <- cmdstanr::cmdstan_model(stan_file)

  sample_args <- list(
    data = stan_data,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
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

  draws_matrix <- fit$draws(variables = c("theta", "epsilon"), format = "matrix")
  theta_cols <- paste0("theta[", seq_len(bt_data$N), "]")
  if (!all(theta_cols %in% colnames(draws_matrix))) {
    rlang::abort("CmdStan output missing theta draws.")
  }
  if (!"epsilon" %in% colnames(draws_matrix)) {
    rlang::abort("CmdStan output missing epsilon draws.")
  }

  theta_draws <- draws_matrix[, theta_cols, drop = FALSE]
  colnames(theta_draws) <- bt_data$item_id %||% as.character(seq_len(bt_data$N))
  epsilon_draws <- draws_matrix[, "epsilon", drop = TRUE]

  thin_draws <- as.integer(config$thin_draws %||% 1L)
  if (is.na(thin_draws) || thin_draws < 1L) {
    rlang::abort("`config$thin_draws` must be a positive integer.")
  }
  if (thin_draws > 1L) {
    keep_idx <- seq(1L, nrow(theta_draws), by = thin_draws)
    theta_draws <- theta_draws[keep_idx, , drop = FALSE]
    epsilon_draws <- epsilon_draws[keep_idx]
  }

  draws <- list(
    theta = theta_draws,
    epsilon = as.double(epsilon_draws)
  )

  summaries <- summarize_draws(draws)
  diagnostics <- .btl_mcmc_v3_collect_diagnostics(fit)

  list(
    draws = draws,
    theta_summary = summaries$theta_summary,
    epsilon_summary = summaries$epsilon_summary,
    diagnostics = diagnostics
  )
}
