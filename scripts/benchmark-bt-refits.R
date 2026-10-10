#!/usr/bin/env Rscript
# Cell runner for benchmark-bt-refits.py. Uses its isolated installed library.
# Twelve identical-seed replicates per cell; two fresh runs per worker setting.
args <- commandArgs(TRUE)
output <- args[[1L]]
n <- as.integer(args[[2L]])
engine <- args[[3L]]
implementation <- args[[4L]]
workers <- as.integer(args[[5L]])
run <- as.integer(args[[6L]])
library(pairwiseLLM)
source("tests/testthat/helper-bt-refits-324.R")
dat <- bt_fixture_324(n)
ids <- sort(unique(c(dat[[1L]], dat[[2L]])))
theta <- setNames(seq(-.5, .5, length.out = n), ids)
fun <- bootstrap_bt_model
estimator_args <- if (engine == "alpha") list(alpha = .5) else list()
withr::local_seed(324L)
seed_before <- .Random.seed
# Warm package/engine loading on a separate two-item fit, outside timing.
fit_bt_model(data.frame(a = rep("a", 4), b = rep("b", 4), y = c(1, 0, 1, 0)),
  engine, verbose = FALSE, alpha = if (engine == "alpha") .5 else NULL)
gc()
elapsed <- system.time(value <- fun(theta, "fixed", n_rep = 12L, seed = 324L,
  schedule = dat, estimator = engine, estimator_args = estimator_args,
  min_success = 12L, workers = workers, keep = "theta"))
stopifnot(identical(seed_before, .Random.seed), value$n_success == 12L)
scientific <- value[c("theta", "n_success", "n_failed", "status", "replicates", "failures", "draws")]
oracle <- file.path(output, paste0("oracle-", n, "-", engine, ".rds"))
if (implementation == "baseline" && workers == 1L && run == 1L) {
  saveRDS(scientific, oracle)
} else {
  stopifnot(identical(scientific, readRDS(oracle)))
}
row <- data.frame(n = n, engine = engine, implementation = implementation,
  workers = workers, run = run, n_rep = 12L, n_success = value$n_success,
  elapsed_seconds = unname(elapsed[["elapsed"]]),
  parent_cpu_seconds = unname(sum(elapsed[1:2])),
  seconds_per_success = unname(elapsed[["elapsed"]]) / value$n_success,
  successes_per_second = value$n_success / unname(elapsed[["elapsed"]]), exact_bootstrap_agreement = TRUE)
write.csv(row, file.path(output, "cell.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
