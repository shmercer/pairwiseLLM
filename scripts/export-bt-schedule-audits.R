#!/usr/bin/env Rscript
# Export compact actual native fit audits. Raw draws/trajectories stay local.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
root <- args[[1L]]
output <- args[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
folders <- c("timing-post328", "timing-optimized", "serial-matrix", "strategies-matrix",
  "firth-matrix", "worker-matrix", "chain-parity", "chain-parity-w12", "profile-optimized", "large-chain-parity")
records <- list()
contracts <- list()
for (folder in folders) {
  paths <- list.files(file.path(root, folder), pattern = "^native-audit[.]rds$",
    recursive = TRUE, full.names = TRUE)
  for (path in paths) {
    if (!file.exists(file.path(dirname(path), "cell.csv"))) next
    run <- substring(dirname(path), nchar(root) + 2L)
    native <- readRDS(path)
    for (row in native) {
      records[[length(records) + 1L]] <- data.frame(run = run,
        seed = row$seed, committed = row$committed, elapsed_seconds = row$elapsed,
        fields = paste(row$fields, collapse = ";"),
        posterior_sha256 = row$posterior_sha256, scientific_sha256 = row$scientific_sha256,
        diagnostics_sha256 = digest::digest(row$diagnostics, algo = "sha256"),
        diagnostics = paste(deparse(row$diagnostics), collapse = " "))
    }
    conditions <- readRDS(file.path(dirname(path), "conditions.rds"))
    stopifnot(length(native) == sum(vapply(conditions,
      function(x) sum(x$replicates$n_scheduling_refits), 0)))
    for (condition in seq_along(conditions)) {
      result <- conditions[[condition]]
      cfg <- result$provenance$btl_config
      n <- length(result$provenance$theta)
      stopifnot(cfg$refit_pairs_target == n, cfg$max_rhat == 1.01,
        identical(cfg$model_variant, "btl_e_b"), cfg$cmdstan$chains == 4L,
        cfg$cmdstan$iter_warmup == 1000L, cfg$cmdstan$iter_sampling == 1000L,
        cfg$cmdstan$threads_per_chain == 1L, cfg$cmdstan$core_fraction == .8)
      for (artifact in result$artifacts) {
        fit <- artifact$state$btl_fit
        if (is.null(fit)) next
        stopifnot(fit$mcmc_config_used$chains == 4L,
          fit$mcmc_config_used$parallel_chains == cfg$cmdstan$parallel_chains,
          fit$mcmc_config_used$threads_per_chain == 1L,
          identical(dim(fit$btl_posterior_draws), c(4000L, as.integer(n))))
      }
      contracts[[length(contracts) + 1L]] <- data.frame(run = run, condition = condition,
        n = n, budget = result$provenance$budget, native_refit_target = cfg$refit_pairs_target,
        chains = cfg$cmdstan$chains, parallel_chains = cfg$cmdstan$parallel_chains,
        iter_warmup = cfg$cmdstan$iter_warmup, iter_sampling = cfg$cmdstan$iter_sampling,
        threads_per_chain = cfg$cmdstan$threads_per_chain, native_max_rhat = cfg$max_rhat,
        n_success = result$n_success, n_failed = result$n_failed, status = result$status,
        completed_scheduling_refits = sum(result$replicates$n_scheduling_refits),
        retained_state_runtime_contract_checked = !is.null(result$artifacts))
    }
  }
}
write.csv(do.call(rbind, records), file.path(output, "native-fit-audits.csv"), row.names = FALSE)
write.csv(do.call(rbind, contracts), file.path(output, "native-contracts.csv"), row.names = FALSE)
cat("Exported", length(records), "actual native refit records\n")
