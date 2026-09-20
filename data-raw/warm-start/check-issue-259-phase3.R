# Focused Phase 3 collection, not a full-package coverage claim.
# Rscript --vanilla data-raw/warm-start/check-issue-259-phase3.R /tmp/issue259-phase3
# Append --summarize-only to recover tables from an already saved coverage object.
local({
  arguments <- commandArgs(trailingOnly = TRUE)
  if (!length(arguments) || length(arguments) > 2L ||
      (length(arguments) == 2L && arguments[[2]] != "--summarize-only")) {
    stop("Supply an output prefix and optionally --summarize-only.")
  }
  prefix <- arguments[[1]]
  if (!dir.exists(dirname(prefix))) stop("The output parent directory must exist.")
  for (package in c("pkgload", "testthat", "covr")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install the check dependency: ", package)
  }
  coverage_path <- paste0(prefix, "-coverage.rds")
  results_path <- paste0(prefix, "-test-results.csv")
  if (length(arguments) == 1L) {
    pkgload::load_all(quiet = TRUE)
    runner <- paste0(prefix, "-focused.R")
    writeLines(c(
      'results <- testthat::test_dir("tests/testthat",',
      '  filter = "^(010[0-3]|011[3-5]|3107|9104)-",',
      '  reporter = "summary", load_package = "none", stop_on_failure = TRUE,',
      "  stop_on_warning = TRUE)",
      "results <- as.data.frame(results)",
      'columns <- c("file", "test", "nb", "failed", "skipped", "error", "warning", "passed")',
      paste0("utils::write.csv(results[, columns], ", encodeString(results_path, quote = '"'),
        ", row.names = FALSE)")
    ), runner)
    coverage <- covr::environment_coverage(asNamespace("pairwiseLLM"), test_files = runner)
    saveRDS(coverage, coverage_path)
  } else {
    coverage <- readRDS(coverage_path)
  }
  lines <- covr::tally_coverage(coverage, by = "line")
  lines <- lines[grepl("(^|/)R/warm_start_(feature_schema|features|python)[.]R$", lines$filename), ]
  lines$filename <- sub("^.*R/", "R/", lines$filename)
  files <- split(lines, lines$filename)
  ledger <- do.call(rbind, lapply(names(files), function(name) {
    counts <- tapply(files[[name]]$value, files[[name]]$line, sum)
    data.frame(file = name, lines = length(counts), covered = sum(counts > 0),
      percent = 100 * mean(counts > 0))
  }))
  utils::write.csv(ledger, paste0(prefix, "-coverage-ledger.csv"), row.names = FALSE)
  utils::write.csv(lines, paste0(prefix, "-coverage-lines.csv"), row.names = FALSE)
  results <- utils::read.csv(results_path)
  cat("Focused files:", length(unique(results$file)), "| Test blocks:", nrow(results), "\n")
  print(colSums(results[, c("failed", "skipped", "error", "warning", "passed")]))
  print(ledger, row.names = FALSE)
})
