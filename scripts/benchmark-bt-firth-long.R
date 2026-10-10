#!/usr/bin/env Rscript
# Sensitivity check prompted by the short N=91 Firth bootstrap regressions.
# Uses the SAME isolated installed libraries as benchmark-bt-refits.py.
# R_LIBS=/tmp/bt-workers/{baseline,prepared}-library Rscript --vanilla \
#   scripts/benchmark-bt-firth-long.R OUTPUT baseline 1 1
# Run both implementations twice, at workers=1 and 12; alternate their order.
args <- commandArgs(TRUE)
output <- args[[1L]]
implementation <- args[[2L]]
workers <- as.integer(args[[3L]])
run <- as.integer(args[[4L]])
dir.create(output, recursive = TRUE, showWarnings = FALSE)
library(pairwiseLLM)
source("tests/testthat/helper-bt-refits-324.R")
dat <- bt_fixture_324(91L)
theta <- setNames(seq(-.5, .5, length.out = 91L), sort(unique(c(dat[[1L]], dat[[2L]]))))
fit_bt_model(data.frame(a = rep("a", 4), b = rep("b", 4), y = c(1, 0, 1, 0)), "brglm2", FALSE)
elapsed <- system.time(value <- bootstrap_bt_model(theta, "fixed", 200L, 324L,
  schedule = dat, estimator = "brglm2", min_success = 200L, workers = workers, keep = "theta"))
scientific <- value[c("theta", "n_success", "n_failed", "status", "replicates", "failures", "draws")]
oracle <- file.path(output, "oracle.rds")
if (implementation == "baseline" && workers == 1L && run == 1L) {
  saveRDS(scientific, oracle)
} else {
  stopifnot(identical(scientific, readRDS(oracle)))
}
stopifnot(value$n_success == 200L)
row <- data.frame(implementation = implementation, workers = workers, run = run,
  n_rep = 200L, n_success = value$n_success, elapsed_seconds = unname(elapsed[["elapsed"]]),
  successes_per_second = value$n_success / unname(elapsed[["elapsed"]]), exact_bootstrap_agreement = TRUE)
write.csv(row, file.path(output, paste(implementation, workers, run, "csv", sep = ".")), row.names = FALSE)
print(row)
