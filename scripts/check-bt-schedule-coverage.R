#!/usr/bin/env Rscript
# Coverage uses only a separate installation. No native sampling or providers.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
output <- args[[1L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
root <- normalizePath(".")
coverage <- covr::package_coverage(root, type = "none",
  install_path = file.path(output, "library"), clean = FALSE,
  code = sprintf(paste0("testthat::test_dir(%s, ",
    "filter='003[8-9]|004[0-5]|50|511[3-5]|5120|310[3-6]|9003|910', ",
    "package='pairwiseLLM', reporter='summary')"), deparse(file.path(root, "tests/testthat"))))
saveRDS(coverage, file.path(output, "coverage.rds"))
lines <- covr::tally_coverage(coverage, by = "line")
files <- c("adaptive_run.R", "bt_bootstrap.R", "bt_bootstrap_schedule.R")
lines <- lines[basename(lines$filename) %in% files, ]
summary <- aggregate(as.numeric(lines$value > 0), list(file = basename(lines$filename)), mean)
names(summary)[2L] <- "fraction_covered"
write.csv(summary, file.path(output, "coverage.csv"), row.names = FALSE)
counts <- do.call(rbind, lapply(split(lines, basename(lines$filename)), function(x) {
  data.frame(file = basename(x$filename[[1L]]), covered = sum(x$value > 0),
    total = nrow(x), percent = 100 * mean(x$value > 0))
}))
write.csv(counts, file.path(output, "coverage-counts.csv"), row.names = FALSE)
write.csv(lines[lines$value == 0, ], file.path(output, "uncovered.csv"), row.names = FALSE)
print(summary)
