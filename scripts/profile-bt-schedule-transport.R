#!/usr/bin/env Rscript
# Stage microprofile only: transport real compact inputs/results from completed
# native cells. Complete native bootstrap throughput is measured separately.
# Run with Rscript --vanilla and R_LIBS pointing at an isolated package library.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L)
input <- args[[1L]]
output <- args[[2L]]
workers <- as.integer(args[[3L]])
stopifnot(workers >= 1L, workers <= 12L)
slots <- as.integer(system2("python3", c("-c", shQuote(paste0(
  "import runpy; print(runpy.run_path('scripts/benchmark-bt-schedule.py')",
  "['physical_slots']()[1])"))), stdout = TRUE))
stopifnot(workers <= slots)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
library(pairwiseLLM)
withr::local_seed(325L)
records <- list()
measure <- function(stage, n, code, repetitions = 1L, bytes = NA_real_) {
  gc()
  timing <- system.time(for (iteration in seq_len(repetitions)) value <- code())
  records[[length(records) + 1L]] <<- data.frame(stage = stage, n = n, workers = workers,
    repetitions = repetitions, elapsed_seconds = unname(timing[["elapsed"]]),
    parent_cpu_seconds = unname(sum(timing[1:2])), serialized_bytes_per_object = bytes)
  value
}
old_plan <- future::plan("list")
withr::defer(future::plan(old_plan))
pids <- measure("pool_initialization_and_namespace_load", NA_integer_, function() {
  future::plan(future::multisession, workers = workers)
  unlist(future.apply::future_lapply(seq_len(workers), function(i) {
    stopifnot(requireNamespace("pairwiseLLM", quietly = TRUE))
    Sys.getpid()
  }, future.seed = TRUE))
})
stopifnot(length(unique(pids)) == workers)
writeLines(as.character(pids), file.path(output, "worker-pids.txt"))

# Freeze the original reducer; replace only expensive replicate evaluation for
# this stage measurement. This is never used to claim end-to-end throughput or
# scientific schedule parity. Each returned theta/diagnostic is a real native
# workload result, and reduction must reproduce its original summary exactly.
reference <- new.env(parent = asNamespace("pairwiseLLM"))
sys.source("tests/testthat/fixtures/bt-schedule-325/bt_bootstrap.R", envir = reference)
reference$.bt_bootstrap_prepare <- function(spec) NULL
for (n in c(57L, 91L, 229L)) {
  path <- file.path(input, paste0("n", n, "-b4-trueskill_p50-alpha"), "result.rds")
  stopifnot(file.exists(path))
  result <- readRDS(path)
  spec <- result$provenance[c("theta", "mode", "schedule", "initial_state", "budget",
    "refit", "btl_config", "schedule_fit_fn", "seed", "keep")]
  spec$keep <- "summary"
  compact <- lapply(seq_len(nrow(result$replicates)), function(i) {
    list(theta = if (result$replicates$success[[i]]) result$draws[i, ] else NULL,
      diagnostics = result$replicates[i, , drop = FALSE], failure = result$failures[[i]],
      artifacts = NULL)
  })
  payload <- compact[[1L]]
  for (entry in list(list(label = "input_spec", value = spec),
                    list(label = "compact_result", value = payload))) {
    object <- entry$value
    bytes <- length(serialize(object, NULL))
    measure(paste0("serialize_", entry$label), n, function() serialize(object, NULL), 100L, bytes)
    # All workers receive one actual-size object and return it. This includes
    # two-way serialization/socket transport, dispatch and collection overhead.
    received <- measure(paste0("socket_roundtrip_", entry$label), n, function() {
      future.apply::future_lapply(rep(list(object), workers), identity, future.seed = TRUE)
    }, 10L, bytes)
    stopifnot(length(received) == workers)
  }
  reference$.bt_bootstrap_one <- local({
    values <- compact
    function(index, spec, prepared) values[[index]]
  })
  reduced <- measure("original_ordered_reduction", n, function() {
    reference$.bt_bootstrap_collect(spec, length(compact), length(compact), 1L)
  }, 100L)
  stopifnot(identical(reduced$theta, result$theta), identical(reduced$replicates, result$replicates))
  write.csv(do.call(rbind, records), file.path(output, "transport.csv"), row.names = FALSE)
}
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
jsonlite::write_json(list(namespace_path = getNamespaceInfo("pairwiseLLM", "path"),
  workers = workers, description = "Stage measurements only; no scheduled fits are bypassed in throughput cells"),
  file.path(output, "provenance.json"), auto_unbox = TRUE, pretty = TRUE)
