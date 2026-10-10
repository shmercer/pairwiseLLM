#!/usr/bin/env Rscript
# Usage: Rscript scripts/summarize-bt-refits.R COMPONENT_DIR WORKER_DIR OUTPUT
args <- commandArgs(TRUE)
fits <- read.csv(file.path(args[[1L]], "fits.csv"))
boot <- read.csv(file.path(args[[2L]], "bootstrap.csv"))
output <- args[[3L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
fit_groups <- split(fits, interaction(fits$n, fits$engine, drop = TRUE))
fit_summary <- do.call(rbind, lapply(fit_groups, function(x) {
  old <- x$seconds[x$implementation == "baseline"]
  new <- x$seconds[x$implementation == "prepared"]
  data.frame(n = x$n[1], engine = x$engine[1], baseline_mean = mean(old), baseline_median = median(old),
    prepared_mean = mean(new), prepared_median = median(new), baseline_fits_per_second = 1 / mean(old),
    prepared_fits_per_second = 1 / mean(new), mean_speedup = mean(old) / mean(new),
    median_speedup = median(old) / median(new))
}))
worker_groups <- split(boot, interaction(boot$n, boot$engine, boot$workers, drop = TRUE))
worker_summary <- do.call(rbind, lapply(worker_groups, function(x) {
  old <- x[x$implementation == "baseline", ]
  new <- x[x$implementation == "prepared", ]
  data.frame(n = x$n[1], engine = x$engine[1], workers = x$workers[1],
    baseline_mean_seconds_per_fit = mean(old$seconds_per_success),
    prepared_mean_seconds_per_fit = mean(new$seconds_per_success),
    baseline_median_seconds_per_fit = median(old$seconds_per_success),
    prepared_median_seconds_per_fit = median(new$seconds_per_success),
    baseline_min_successes_per_second = min(old$successes_per_second),
    prepared_min_successes_per_second = min(new$successes_per_second),
    baseline_mean_successes_per_second = mean(old$successes_per_second),
    prepared_mean_successes_per_second = mean(new$successes_per_second),
    mean_time_speedup = mean(old$elapsed_seconds) / mean(new$elapsed_seconds),
    baseline_peak_tree_rss_bytes = max(old$peak_tree_rss_bytes),
    prepared_peak_tree_rss_bytes = max(new$peak_tree_rss_bytes))
}))
write.csv(fit_summary, file.path(output, "fit-summary.csv"), row.names = FALSE)
write.csv(worker_summary, file.path(output, "worker-summary.csv"), row.names = FALSE)
print(fit_summary, row.names = FALSE)
print(worker_summary, row.names = FALSE)
