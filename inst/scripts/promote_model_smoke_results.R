#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0L) return(default)
  sub(paste0("^--", name, "="), "", hit[[1L]])
}

arg_flag <- function(name, default = FALSE) {
  value <- tolower(arg_value(name, if (default) "true" else "false"))
  if (!value %in% c("true", "false")) {
    stop("--", name, " must be true or false.", call. = FALSE)
  }
  identical(value, "true")
}

if ("--help" %in% args || arg_flag("help")) {
  cat(paste0(paste(
    "Promote a completed compatibility smoke run into package evidence.",
    "",
    "Usage:",
    "  Rscript inst/scripts/promote_model_smoke_results.R [options]",
    "",
    "Options:",
    "  --input=PATH                 Combined smoke results.",
    "  --matrix=PATH                Current smoke matrix.",
    "  --extdata-dir=PATH           Destination directory.",
    "  --registry=PATH              Compatibility registry destination.",
    "  --allow-incomplete=true      Permit incomplete rows (not recommended).",
    "  --update-references=false    Do not update dated vignette/test paths.",
    "  --dry-run=true               Validate and summarize without writing.",
    sep = "\n"
  ), "\n"))
  quit(status = 0L, save = "no")
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else ""
script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else file.path("inst", "scripts")
source(file.path(script_dir, "promote_model_smoke_results_helpers.R"))

input_path <- arg_value(
  "input", file.path("tasklists", "evidence", "model-smoke-results.csv")
)
matrix_path <- arg_value(
  "matrix", file.path("inst", "extdata", "model_smoke_matrix.csv")
)
extdata_dir <- arg_value("extdata-dir", file.path("inst", "extdata"))
registry_path <- arg_value(
  "registry", file.path(extdata_dir, "model_compatibility.csv")
)
allow_incomplete <- arg_flag("allow-incomplete")
update_references <- arg_flag("update-references", TRUE)
dry_run <- arg_flag("dry-run")

if (!file.exists(input_path)) stop("Smoke result file does not exist: ", input_path, call. = FALSE)
if (!file.exists(matrix_path)) stop("Smoke matrix does not exist: ", matrix_path, call. = FALSE)

results <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
smoke_matrix <- utils::read.csv(matrix_path, stringsAsFactors = FALSE, check.names = FALSE)
validate_promotion_inputs(results, smoke_matrix, allow_incomplete)

existing_registry <- if (file.exists(registry_path)) {
  utils::read.csv(registry_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  NULL
}
registry <- build_compatibility_registry(results, existing_registry)
test_date <- unique(results$test_date)
live_path <- file.path(extdata_dir, paste0("model_smoke_results_", test_date, ".csv"))
batch_path <- file.path(extdata_dir, paste0("model_batch_smoke_results_", test_date, ".csv"))

cat("Promotion date:", test_date, "\n")
cat("Live evidence:", live_path, "\n")
cat("Batch evidence:", batch_path, "\n")
cat("Compatibility registry:", registry_path, "\n")
cat("Passed live:", sum(results$mode == "live" & results$status == "passed"), "\n")
cat("Passed batch:", sum(results$mode == "batch" & results$status == "passed"), "\n")

if (dry_run) quit(status = 0L, save = "no")

dir.create(extdata_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  results[results$mode == "live", , drop = FALSE], live_path,
  row.names = FALSE, na = ""
)
utils::write.csv(
  results[results$mode == "batch", , drop = FALSE], batch_path,
  row.names = FALSE, na = ""
)
utils::write.csv(registry, registry_path, row.names = FALSE, na = "")

if (update_references) {
  update_dated_evidence_references(c(
    file.path("vignettes", "model-compatibility.Rmd"),
    file.path("tests", "testthat", "test-0026-documentation-contracts.R")
  ), test_date)
}

cat("Promotion complete.\n")
