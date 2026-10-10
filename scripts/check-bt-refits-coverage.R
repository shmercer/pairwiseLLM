#!/usr/bin/env Rscript
# Scoped line coverage; installs only into the requested temporary output path.
# Usage: Rscript --vanilla scripts/check-bt-refits-coverage.R /tmp/bt-coverage
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
output <- args[[1L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
path <- normalizePath(".")
coverage <- covr::package_coverage(path, type = "none",
  install_path = file.path(output, "library"), clean = FALSE,
  code = sprintf(paste0("testthat::test_dir(%s, filter='003[5-9]|004[0-4]|511[3-5]', ",
    "package='pairwiseLLM', reporter='summary')"), deparse(file.path(path, "tests", "testthat"))))
saveRDS(coverage, file.path(output, "coverage.rds"))
# Expand trace expressions to source lines and remove blank lines, as covr's
# percent_coverage() does. Expression counts are not source-line coverage.
df <- covr::tally_coverage(coverage, by = "line")
files <- c("bt_common.R", "bt_alpha.R", "bt_firth.R", "bt_bootstrap.R",
  "bt_bootstrap_contracts.R", "bt_bootstrap_schedule.R")
df <- df[basename(df$filename) %in% files, ]
summary <- aggregate(as.numeric(df$value > 0), list(file = basename(df$filename)), mean)
names(summary)[2L] <- "fraction_covered"
write.csv(summary, file.path(output, "coverage.csv"), row.names = FALSE)
write.csv(df[df$value == 0, ], file.path(output, "uncovered.csv"), row.names = FALSE)
print(summary)
