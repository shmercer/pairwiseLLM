#!/usr/bin/env Rscript
# Internal cell runner. The Python driver supplies an isolated package library.
# No providers, study data, or changes to an existing installation.
args <- commandArgs(TRUE)
stopifnot(length(args) == 11L)
output <- args[[1L]]
n <- as.integer(args[[2L]])
B <- as.integer(args[[3L]])
strategy <- args[[4L]]
engine <- args[[5L]]
implementation <- args[[6L]]
workers <- as.integer(args[[7L]])
n_rep <- as.integer(args[[8L]])
parallel_chains <- as.integer(args[[9L]])
profile <- identical(args[[10L]], "profile")
execution <- args[[11L]]
conditions <- as.integer(Sys.getenv("BT_SCHEDULE_CONDITIONS", "1"))
stopifnot(conditions >= 1L, n_rep %% conditions == 0L, n_rep / conditions >= 2L)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
library(pairwiseLLM)
stopifnot(n_rep >= 2L, B %in% c(4L, 8L, 16L), execution %in% c("managed", "pool"))
ns <- asNamespace("pairwiseLLM")
ids <- sprintf("item%03d", seq_len(n))
theta <- stats::setNames(seq(-1, 1, length.out = n), ids)
initial <- adaptive_rank_start(ids, seed = 325L,
  adaptive_config = list(pairing_strategy = strategy))
config <- list(refit_pairs_target = n, model_variant = "btl_e_b",
  cmdstan = list(chains = 4L, parallel_chains = parallel_chains,
    threads_per_chain = 1L, iter_warmup = 1000L, iter_sampling = 1000L,
    core_fraction = 0.8))

# Compile in this isolated installed package, before workload timing. Model
# construction/cached executable loading during each native refit remains timed.
compile_seconds <- system.time(cmdstanr::cmdstan_model(
  get("stan_file_for_variant", ns)("btl_e_b"),
  cpp_options = list(stan_threads = TRUE)))[["elapsed"]]
audit_dir <- file.path(output, "native")
dir.create(audit_dir, showWarnings = FALSE)
native_fit <- local({
  directory <- normalizePath(audit_dir)
  slots <- parallel_chains
  function(state, config) {
    # The driver has reserved these slots across all simultaneous trajectories.
    # Future otherwise advertises mc.cores=1 and the sampler correctly refuses 4.
    withr::local_options(list(mc.cores = slots))
    start <- proc.time()
    fit <- pairwiseLLM:::default_btl_fit_fn(state, config)
    elapsed <- unname((proc.time() - start)[["elapsed"]])
    scientific <- fit[setdiff(names(fit), "mcmc_config_used")]
    evidence <- list(seed = config$cmdstan$seed, committed = nrow(state$history_pairs),
      elapsed = elapsed, fields = names(fit),
      scientific_sha256 = digest::digest(scientific, algo = "sha256"),
      posterior_sha256 = digest::digest(fit$btl_posterior_draws, algo = "sha256"),
      diagnostics = fit$diagnostics)
    saveRDS(evidence, file.path(directory, paste0(config$cmdstan$seed, ".rds")))
    fit
  }
})

call_args <- list(object = theta, mode = "adaptive", n_rep = as.integer(n_rep / conditions), seed = 325L,
  initial_state = initial, budget = as.integer(B * n / 2L), estimator = engine,
  estimator_args = if (engine == "alpha") list(alpha = .5) else list(),
  btl_config = config, schedule_fit_fn = native_fit, workers = workers,
  keep = "full")
# Full evidence is used for qualification cells. The driver uses summary cells
# for throughput separately; scientific hashes are captured without raw draws.
retention <- Sys.getenv("BT_SCHEDULE_KEEP", "full")
call_args$keep <- retention
pool_start <- 0
if (execution == "pool") {
  stopifnot("executor" %in% names(formals(bootstrap_bt_model)))
  pool_start <- system.time({
    future::plan(future::multisession, workers = workers)
    future.apply::future_lapply(seq_len(workers), function(i) {
      stopifnot(requireNamespace("pairwiseLLM", quietly = TRUE))
      Sys.getpid()
    }, future.seed = TRUE)
  })[["elapsed"]]
  call_args$executor <- function(X, FUN, ...) {
    future.apply::future_lapply(X, FUN, ..., future.seed = TRUE)
  }
}
withr::local_seed(325L)
rng_before <- .Random.seed
gc()
profile_path <- file.path(output, "Rprof.out")
if (profile) utils::Rprof(profile_path, interval = .01, memory.profiling = TRUE,
  event = if (.Platform$OS.type == "unix") "elapsed" else "default")
warning_messages <- character()
writeLines("started", file.path(output, "workload.started"))
condition_times <- numeric(conditions)
elapsed <- system.time(results <- lapply(seq_len(conditions), function(condition) {
  current_args <- call_args
  current_args$seed <- as.integer(325L + (condition - 1L) * 1009L)
  timed <- system.time(result <- withCallingHandlers(
    tryCatch(do.call(bootstrap_bt_model, current_args),
      pairwiseLLM_bt_bootstrap_error = function(e) {
        if (is.null(e$result)) stop(e)
        e$result
      }), warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }))
  condition_times[[condition]] <<- unname(timed[["elapsed"]])
  result
}))
writeLines("finished", file.path(output, "workload.finished"))
if (profile) utils::Rprof(NULL)
stopifnot(identical(rng_before, .Random.seed))
if (execution == "pool") future::plan(future::sequential)
result <- results[[1L]]
saveRDS(result, file.path(output, "result.rds"))
saveRDS(results, file.path(output, "conditions.rds"))
write.csv(data.frame(condition = seq_len(conditions), n_rep = n_rep / conditions,
  seed = 325L + (seq_len(conditions) - 1L) * 1009L, elapsed_seconds = condition_times),
  file.path(output, "conditions.csv"), row.names = FALSE)
native_files <- list.files(audit_dir, full.names = TRUE, pattern = "[.]rds$")
native <- lapply(native_files, readRDS)
saveRDS(native, file.path(output, "native-audit.rds"))
native_seconds <- sum(vapply(native, `[[`, 0, "elapsed"))
n_success <- sum(vapply(results, `[[`, 0L, "n_success"))
n_failed <- sum(vapply(results, `[[`, 0L, "n_failed"))
row <- data.frame(n = n, B = B, strategy = strategy, engine = engine,
  implementation = implementation, workers = workers, parallel_chains = parallel_chains,
  execution = execution, retention = retention, profiled = profile, n_rep = n_rep, conditions = conditions,
  n_success = n_success, n_failed = n_failed,
  elapsed_seconds = unname(elapsed[["elapsed"]]),
  parent_cpu_seconds = unname(sum(elapsed[1:2])),
  successful_replicates_per_second = n_success / unname(elapsed[["elapsed"]]),
  successful_replicates_per_second_including_pool_start = n_success /
    (unname(elapsed[["elapsed"]]) + pool_start),
  native_refits = length(native), native_seconds = native_seconds,
  compile_seconds = unname(compile_seconds), pool_start_seconds = unname(pool_start))
write.csv(row, file.path(output, "cell.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
source_functions <- c("bootstrap_bt_model", ".bt_bootstrap_collect", ".bt_bootstrap_schedule",
  "adaptive_rank_run_live", ".adaptive_run_prepare", ".adaptive_run_execute")
source_functions <- source_functions[vapply(source_functions, exists, logical(1L), envir = ns, inherits = FALSE)]
jsonlite::write_json(list(namespace_path = getNamespaceInfo("pairwiseLLM", "path"),
  function_sha256 = lapply(stats::setNames(source_functions, source_functions), function(name) {
    digest::digest(paste(deparse(body(get(name, ns))), collapse = "\n"), algo = "sha256", serialize = FALSE)
  })), file.path(output, "loaded-code.json"), auto_unbox = TRUE, pretty = TRUE)
writeLines(warning_messages, file.path(output, "warnings.txt"))
if (profile) {
  summary <- summaryRprof(profile_path, memory = "both")
  write.csv(summary$by.total, file.path(output, "profile-functions.csv"))
  lines <- readLines(profile_path, warn = FALSE)[-1L]
  # Disjoint sampled categories, in priority order. These are wall-time samples
  # (including waits), not additive inclusive function totals.
  groups <- c(native_refits = 'default_btl_fit_fn"',
    final_estimation = '".bt_bootstrap_refit"',
    prepared_design = '".bt_bootstrap_prepare"',
    outcome_generation = '".bt_bootstrap_(simulate|judge)"',
    selection = '"(select_next_pair|.adaptive_warm_start_selection)"',
    trueskill = '"(update_trueskill_state|trueskill_update|.adaptive_trueskill[^\"]*)"',
    validation = '"[^\"]*(validate|check_judge|resolve_config)[^\"]*"',
    state_logs_checkpoints = '"[^\"]*(controller|stop_boundary|phase_a|persist_session|append_[^\"]*log|apply_step_update)[^\"]*"',
    clone_hash_serialization = '"(serialize|unserialize|.warm_start_prior_hash|digest)"')
  category <- rep("other", length(lines))
  for (label in names(groups)) {
    select <- category == "other" & grepl(groups[[label]], lines)
    category[select] <- label
  }
  sampled <- as.data.frame(table(category))
  sampled$seconds <- sampled$Freq * .01
  sampled$fraction <- sampled$Freq / sum(sampled$Freq)
  write.csv(sampled, file.path(output, "profile-components.csv"), row.names = FALSE)
}
print(row)
