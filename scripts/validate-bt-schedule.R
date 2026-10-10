#!/usr/bin/env Rscript
# Exact native/posterior and trajectory comparison of independent installed runs.
# Usage: Rscript scripts/validate-bt-schedule.R BASE CANDIDATE OUTPUT [historical]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(3L, 4L))
baseline <- args[[1L]]
candidate <- args[[2L]]
output <- args[[3L]]
historical <- length(args) == 4L && identical(args[[4L]], "historical")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
# Load the recorded isolated candidate before deserializing namespace references.
# This never installs or changes the active package library.
manifest_path <- file.path(candidate, "provenance.json")
candidate_ns <- if (file.exists(manifest_path)) {
  manifest <- jsonlite::fromJSON(manifest_path)
  loadNamespace("pairwiseLLM", lib.loc = manifest$package_library)
} else NULL
source("tests/testthat/helper-bt-schedule-325.R")
resource_columns <- c("mcmc_parallel_chains", "mcmc_cores_available",
  "mcmc_parallel_chains_requested", "mcmc_concurrency_budget", "mcmc_concurrency_used")
resource_names <- sub("^mcmc_", "", resource_columns)
normalize_resources <- function(x) {
  if (is.list(x)) {
    x[names(x) %in% c(resource_columns, resource_names)] <- NULL
    for (i in seq_along(x)) x[i] <- list(normalize_resources(x[[i]]))
  }
  x
}
compare <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    details <- capture.output(print(all.equal(actual, expected, tolerance = 0)))
    writeLines(c(label, details), file.path(output, "first-difference.txt"))
    stop("Exact comparison failed: ", label)
  }
}
records <- list()
schemas <- list()
memo_proofs <- list()
memo_contexts <- function(state) {
  stopifnot(!is.null(candidate_ns), identical(state$controller$run_mode, "within_set"))
  memo <- state$linking$phase_a$prepare_context_by_set
  contexts <- vector("list", length(memo))
  names(contexts) <- names(memo)
  for (key in names(memo)) {
    capture <- new.env(parent = candidate_ns)
    original <- get(".adaptive_runtime_hash_object", candidate_ns)
    capture$.adaptive_runtime_hash_object <- function(x) {
      if (is.list(x) && all(c("set_id", "requested_source", "required_config_hash") %in% names(x))) {
        capture$context <- x
      }
      original(x)
    }
    hash_context <- get(".adaptive_phase_a_prepare_context_hash", candidate_ns)
    environment(hash_context) <- capture
    status <- state$linking$phase_a$set_status
    status <- status[status$set_id == as.integer(key), , drop = FALSE]
    stopifnot(nrow(status) == 1L, identical(status$source, "run"))
    actual_hash <- hash_context(state, as.integer(key), "run",
      get(".adaptive_controller_resolve", candidate_ns)(state), prior_status = status$status)
    # Never discard an unexplained or stale digest. Reconstruct its full input
    # and prove that the actual recorded memo is precisely its hash.
    compare(actual_hash, memo[[key]], paste("memo preimage", key))
    contexts[[key]] <- capture$context
  }
  contexts
}
paths <- list.files(candidate, pattern = "result[.]rds$", full.names = TRUE, recursive = TRUE)
measurement_path <- file.path(candidate, "measurements.json")
if (file.exists(measurement_path)) {
  measurements <- jsonlite::fromJSON(measurement_path)
  if (file.exists(manifest_path)) {
    dimensions <- manifest$arguments[c("n", "B", "strategies", "engines")]
    expected_cells <- prod(vapply(dimensions, function(x) length(strsplit(x, ",", fixed = TRUE)[[1L]]), 1L))
    stopifnot(nrow(measurements) == expected_cells)
  }
  stopifnot(all(measurements$status == "complete"),
    identical(sort(basename(dirname(paths))), sort(measurements$cell)))
}
for (path in paths) {
  base_cell <- basename(dirname(path))
  base_path <- file.path(baseline, base_cell, "result.rds")
  if (!file.exists(base_path)) stop("Missing independent baseline result: ", base_path)
  read_conditions <- function(p) {
    multiple <- file.path(dirname(p), "conditions.rds")
    if (file.exists(multiple)) readRDS(multiple) else list(readRDS(p))
  }
  old_conditions <- read_conditions(base_path)
  new_conditions <- read_conditions(path)
  compare(length(new_conditions), length(old_conditions), paste(base_cell, "condition count"))
  for (condition in seq_along(old_conditions)) {
  cell <- paste(base_cell, "condition", condition, sep = "/")
  old <- old_conditions[[condition]]
  new <- new_conditions[[condition]]
  # Retention differs only when explicitly requested; qualification uses full.
  fields <- c("theta", "n_success", "n_failed", "status", "replicates", "draws")
  compare(new[fields], old[fields], paste(cell, "estimates/seeds/failures"))
  old_audit <- readRDS(file.path(dirname(base_path), "native-audit.rds"))
  new_audit <- readRDS(file.path(dirname(path), "native-audit.rds"))
  audit_scientific <- function(x) {
    x <- x[order(vapply(x, `[[`, 1L, "seed"))]
    lapply(x, function(row) row[c("seed", "committed", "scientific_sha256",
      "posterior_sha256", "diagnostics")])
  }
  compare(audit_scientific(new_audit), audit_scientific(old_audit), paste(cell, "all native refits"))
  resource_changed <- !identical(old$provenance$btl_config$cmdstan$parallel_chains,
    new$provenance$btl_config$cmdstan$parallel_chains)
  if (resource_changed) {
    for (i in seq_along(old$artifacts)) {
      a <- old$artifacts[[i]]$state
      b <- new$artifacts[[i]]$state
      if (is.null(a) && is.null(b)) next
      ac <- memo_contexts(a)
      bc <- memo_contexts(b)
      compare(normalize_resources(bc), normalize_resources(ac), paste(cell, i, "memo inputs"))
      for (key in names(ac)) {
        canonical <- digest::digest(normalize_resources(ac[[key]]), algo = "sha256")
        memo_proofs[[length(memo_proofs) + 1L]] <- data.frame(cell = cell, replicate = i, set = key,
          baseline_hash = a$linking$phase_a$prepare_context_by_set[[key]],
          candidate_hash = b$linking$phase_a$prepare_context_by_set[[key]],
          committed = ac[[key]]$run_surface$n_pairs_committed,
          normalized_context_sha256 = canonical, exact_inputs_except_resources = TRUE)
        old$artifacts[[i]]$state$linking$phase_a$prepare_context_by_set[[key]] <- canonical
        new$artifacts[[i]]$state$linking$phase_a$prepare_context_by_set[[key]] <- canonical
      }
    }
  }
  old <- normalize_resources(bt_scientific_325(old))
  new <- normalize_resources(bt_scientific_325(new))
  if (!historical) {
    compare(new, old, paste(cell, "full scientific state"))
  } else {
    # Historical state predates new predictive-bootstrap metadata. Compare all
    # shared log columns and the complete established cold scientific contract,
    # and enumerate schema differences rather than pretending schemas match.
    compare(new$failures, old$failures, paste(cell, "failure details"))
    for (field in c("items", "item_ids", "warm_start_pairs", "warm_start_idx",
                    "warm_start_done", "trueskill_state", "history_pairs", "history_state")) {
      compare(new$provenance$initial_state[[field]], old$provenance$initial_state[[field]],
        paste(cell, "initialization", field))
    }
    for (i in seq_along(old$artifacts)) {
      a <- old$artifacts[[i]]
      b <- new$artifacts[[i]]
      compare(b$comparisons, a$comparisons, paste(cell, i, "presentations/outcomes"))
      for (field in c("warm_start_pairs", "warm_start_idx", "warm_start_done",
                       "history_pairs", "history_state", "trueskill_state", "stop_metrics", "btl_fit")) {
        compare(b$state[[field]], a$state[[field]], paste(cell, i, field))
      }
      for (field in c("step_log", "round_log")) {
        common <- intersect(names(a$state[[field]]), names(b$state[[field]]))
        compare(b$state[[field]][common], a$state[[field]][common], paste(cell, i, field))
        schemas[[length(schemas) + 1L]] <- data.frame(cell = cell, replicate = i, field = field,
          baseline_only = paste(setdiff(names(a$state[[field]]), common), collapse = ";"),
          candidate_only = paste(setdiff(names(b$state[[field]]), common), collapse = ";"))
      }
      shared_meta <- intersect(names(a$state$meta), names(b$state$meta))
      compare(b$state$meta[shared_meta], a$state$meta[shared_meta], paste(cell, i, "stop/seed metadata"))
    }
  }
  records[[length(records) + 1L]] <- data.frame(cell = cell, historical = historical,
    n_replicates = nrow(new$replicates), n_success = new$n_success,
    native_refits_all_conditions = length(new_audit),
    full_state_checked = !is.null(new$artifacts), resource_metadata_changed = resource_changed, exact = TRUE,
    estimator_sha256 = digest::digest(new[fields], algo = "sha256"),
    native_sha256 = digest::digest(audit_scientific(new_audit), algo = "sha256"))
  write.csv(do.call(rbind, records), file.path(output, "equivalence.csv"), row.names = FALSE)
  if (length(schemas)) write.csv(do.call(rbind, schemas), file.path(output, "schema-differences.csv"), row.names = FALSE)
  if (length(memo_proofs)) write.csv(do.call(rbind, memo_proofs),
    file.path(output, "memo-resource-proofs.csv"), row.names = FALSE)
  cat(cell, "exact\n")
  }
}
if (!length(records)) stop("No matching completed cells to compare")
