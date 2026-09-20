# Full release tests/coverage, using only the existing offline test suite.
# Rscript --vanilla data-raw/warm-start/check-issue-259-phase7.R tests OUTPUT_DIR
# Rscript --vanilla data-raw/warm-start/check-issue-259-phase7.R coverage OUTPUT_DIR
# Set PAIRWISELLM_TEST_PYTHON to the already provisioned pinned interpreter.
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || !args[[1L]] %in% c("tests", "coverage")) {
    stop("Supply tests or coverage and an output directory.")
  }
  mode <- args[[1L]]
  dir.create(args[[2L]], recursive = TRUE, showWarnings = FALSE)
  output <- normalizePath(args[[2L]], winslash = "/")
  packages <- sort(unique(c("devtools", "covr", "rcmdcheck", "roxygen2", "pkgdown",
    "lintr", "sessioninfo", "pairwiseLLM", tools::package_dependencies("pairwiseLLM",
      db = read.dcf("DESCRIPTION"), which = c("Depends", "Imports", "Suggests"))[[1L]])))
  packages <- setdiff(packages, "R")
  versions <- vapply(packages, function(name) {
    if (name == "pairwiseLLM") return(read.dcf("DESCRIPTION")[1L, "Version"])
    if (requireNamespace(name, quietly = TRUE)) as.character(utils::packageVersion(name)) else NA_character_
  }, character(1))
  utils::write.csv(data.frame(package = packages, version = versions),
    file.path(output, paste0(mode, "-dependencies.csv")), row.names = FALSE)
  writeLines(c(system("git rev-parse HEAD", intern = TRUE),
    paste("R", getRversion()), capture.output(sessionInfo())),
    file.path(output, paste0(mode, "-session.txt")))
  if (mode == "tests") {
    results <- devtools::test(reporter = "summary", stop_on_failure = FALSE, stop_on_warning = FALSE)
    results <- as.data.frame(results)
    columns <- c("file", "test", "nb", "failed", "skipped", "error", "warning", "passed")
    utils::write.csv(results[, columns], file.path(output, "test-results.csv"), row.names = FALSE)
    totals <- colSums(results[, c("failed", "skipped", "error", "warning", "passed")])
    print(totals)
    if (any(totals[c("failed", "error", "warning")] > 0)) stop("Full tests reported failures or warnings.")
  } else {
    coverage <- covr::package_coverage(path = ".", type = "tests", quiet = FALSE)
    saveRDS(coverage, file.path(output, "coverage.rds"))
    lines <- covr::tally_coverage(coverage, by = "line")
    files <- split(lines, lines$filename)
    ledger <- do.call(rbind, lapply(names(files), function(name) {
      counts <- tapply(files[[name]]$value, files[[name]]$line, sum)
      data.frame(file = name, lines = length(counts), covered = sum(counts > 0),
        percent = 100 * mean(counts > 0))
    }))
    utils::write.csv(ledger, file.path(output, "coverage-ledger.csv"), row.names = FALSE)
    utils::write.csv(lines, file.path(output, "coverage-lines.csv"), row.names = FALSE)
    cat("Package coverage:", covr::percent_coverage(coverage), "\n")
    print(ledger[grepl("warm_start_", ledger$file), ], row.names = FALSE)
  }
})
