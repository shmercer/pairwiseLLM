# -------------------------------------------------------------------------
# CPU allocation for CmdStan sampling
# -------------------------------------------------------------------------

.btl_mcmc_positive_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.null(dim(x)) ||
    !is.finite(x) || x < 1 || x > .Machine$integer.max || x != floor(x)) {
    rlang::abort(paste0("`cmdstan$", name, "` must be a positive integer."))
  }
  as.integer(x)
}

.btl_mcmc_parallelly_available <- function() {
  requireNamespace("parallelly", quietly = TRUE)
}

.btl_mcmc_available_cores <- function() {
  if (!.btl_mcmc_parallelly_available()) {
    rlang::abort(
      paste0("The 'parallelly' package is required for MCMC resource detection. ",
        "Install it with install.packages(\"parallelly\")."),
      class = "pairwiseLLM_mcmc_dependency_missing"
    )
  }
  available <- tryCatch(
    parallelly::availableCores(logical = FALSE, which = "min"),
    error = function(e) NA_integer_
  )
  if (!is.numeric(available) || length(available) != 1L ||
    !is.finite(available) || available < 1 || available > .Machine$integer.max ||
    available != floor(available)) {
    rlang::warn(
      "Could not determine the available CPU allocation; using one CPU slot.",
      class = "pairwiseLLM_mcmc_resource_detection"
    )
    available <- 1L
  }
  as.integer(available)
}

.btl_mcmc_resolve_cmdstan_config <- function(cmdstan) {
  cmdstan <- cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`cmdstan` must be a list.")
  }
  threads_per_chain <- .btl_mcmc_positive_integer(
    cmdstan$threads_per_chain %||% 1L, "threads_per_chain"
  )
  core_fraction <- cmdstan$core_fraction %||% 0.8
  if (!is.numeric(core_fraction) || length(core_fraction) != 1L ||
    !is.finite(core_fraction) || core_fraction <= 0 || core_fraction > 1) {
    rlang::abort("`cmdstan$core_fraction` must be in (0, 1].")
  }

  # Hardware detection retains the historical chain-count rule. Allocated CPUs
  # affect scheduling only, never the number of chains or posterior draws.
  cores <- .btl_mcmc_detect_cores()
  chains <- .btl_mcmc_positive_integer(
    cmdstan$chains %||% min(8L, cores$effective), "chains"
  )
  requested <- cmdstan$parallel_chains
  if (!is.null(requested)) {
    requested <- .btl_mcmc_positive_integer(requested, "parallel_chains")
  }
  available <- .btl_mcmc_available_cores()
  check_limit <- tolower(trimws(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))
  if (!check_limit %in% c("", "false", "f", "0")) {
    available <- min(available, 2L)
  }

  if (is.null(requested)) {
    budget <- min(2L, max(1L, floor(available * core_fraction)))
    parallel_chains <- min(chains, floor(budget / threads_per_chain))
    if (parallel_chains < 1L) {
      rlang::abort(
        paste0("`cmdstan$threads_per_chain` exceeds the automatic CPU budget (", budget,
          "). Reduce threads_per_chain or explicitly set parallel_chains ",
          "within the available allocation (", available, ")."),
        class = "pairwiseLLM_mcmc_resource_limit"
      )
    }
  } else {
    budget <- available
    parallel_chains <- min(chains, requested)
    if (as.double(parallel_chains) * threads_per_chain > budget) {
      rlang::abort(
        paste0("Requested MCMC concurrency (", parallel_chains, " chains x ",
          threads_per_chain, " threads) exceeds the available CPU budget (", budget,
          "). Reduce cmdstan$parallel_chains or cmdstan$threads_per_chain, ",
          "or request a larger CPU allocation."),
        class = "pairwiseLLM_mcmc_resource_limit"
      )
    }
  }

  list(
    chains = chains,
    parallel_chains = as.integer(parallel_chains),
    core_fraction = as.double(core_fraction),
    cores_detected_physical = cores$physical,
    cores_detected_logical = cores$logical,
    threads_per_chain = threads_per_chain,
    cores_available = as.integer(available),
    parallel_chains_requested = requested %||% NA_integer_,
    concurrency_budget = as.integer(budget),
    concurrency_used = as.integer(as.double(parallel_chains) * threads_per_chain),
    cmdstanr_version = .btl_mcmc_cmdstanr_version()
  )
}

.btl_mcmc_resource_message <- function(config) {
  if (isTRUE(config$concurrency_used > 2L)) {
    rlang::inform(
      paste0("Using user-requested MCMC concurrency: ", config$parallel_chains,
        " parallel chains x ", config$threads_per_chain, " threads (",
        config$concurrency_used, " of ", config$cores_available, " available CPU slots)."),
      class = "pairwiseLLM_mcmc_explicit_concurrency"
    )
  }
  invisible(NULL)
}

.btl_mcmc_resource_log_defaults <- function(log) {
  fields <- c("mcmc_cores_available", "mcmc_parallel_chains_requested",
    "mcmc_concurrency_budget", "mcmc_concurrency_used")
  for (field in setdiff(fields, names(log))) {
    log[[field]] <- rep(NA_integer_, nrow(log))
  }
  log
}
