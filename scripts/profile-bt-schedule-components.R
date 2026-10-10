#!/usr/bin/env Rscript
# Complement native end-to-end profiles with short isolated stage measurements.
# R_LIBS must point to the frozen isolated baseline library.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
input <- args[[1L]]
output <- args[[2L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
library(pairwiseLLM)
ns <- asNamespace("pairwiseLLM")
jsonlite::write_json(list(namespace_path = getNamespaceInfo("pairwiseLLM", "path"),
  prepare_sha256 = digest::digest(paste(deparse(body(get(".bt_bootstrap_prepare", ns))),
    collapse = "\n"), algo = "sha256", serialize = FALSE)),
  file.path(output, "loaded-code.json"), auto_unbox = TRUE, pretty = TRUE)
records <- list()
measure <- function(label, n, B, engine, code, times = 1L) {
  path <- tempfile()
  gc()
  utils::Rprofmem(path)
  elapsed <- system.time(for (i in seq_len(times)) value <- code())
  utils::Rprofmem(NULL)
  lines <- readLines(path, warn = FALSE)
  unlink(path)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  records[[length(records) + 1L]] <<- data.frame(stage = label, n = n, B = B,
    engine = engine, repetitions = times, elapsed_seconds = unname(elapsed[["elapsed"]]),
    parent_cpu_seconds = unname(sum(elapsed[1:2])), allocated_bytes = sum(bytes, na.rm = TRUE))
  value
}
paths <- list.files(input, pattern = "result[.]rds$", full.names = TRUE, recursive = TRUE)
for (path in paths) {
  result <- readRDS(path)
  spec <- result$provenance
  n <- length(spec$theta)
  B <- 2L * spec$budget / n
  state <- spec$initial_state
  measure("validate_pristine_state", n, B, NA_character_,
    function() get(".bt_bootstrap_initial_state", ns)(state, names(spec$theta)), 10L)
  measure("serialize_pristine_state", n, B, NA_character_, function() serialize(state, NULL), 100L)
  raw_state <- serialize(state, NULL)
  measure("unserialize_pristine_state", n, B, NA_character_, function() unserialize(raw_state), 100L)
  pairs <- t(utils::combn(names(spec$theta), 2L))
  pairs <- data.frame(A_id = pairs[, 1L], B_id = pairs[, 2L])
  measure("all_edge_outcomes", n, B, NA_character_,
    function() get(".bt_bootstrap_simulate", ns)(pairs, spec$theta, 325L), 100L)
  measure("ordered_outcome_uniforms", n, B, NA_character_,
    function() get(".bt_bootstrap_judge", ns)(spec$theta, 325L, spec$budget), 100L)
  for (engine in c("alpha", "brglm2")) {
    spec$refit <- list(estimator = engine, args = if (engine == "alpha") list(alpha = .5) else list())
    prepared <- measure("prepare_design", n, B, engine,
      function() get(".bt_bootstrap_prepare", ns)(spec), 2L)
    records[[length(records)]]$retained_bytes <- as.numeric(object.size(prepared))
    for (i in which(result$replicates$success)) {
      dat <- result$artifacts[[i]]$comparisons
      fitted <- measure("prepared_final_fit", n, B, engine,
        function() get(".bt_bootstrap_refit", ns)(dat, names(spec$theta), spec$refit, prepared))
      stopifnot(all(is.finite(fitted)))
    }
  }
  # One successful trajectory is sufficient to measure serialization and
  # hashing of its actual growing state. No schedule is substituted for timing.
  good <- which(result$replicates$success)
  if (length(good)) {
    artifact <- result$artifacts[[good[[1L]]]]
    measure("serialize_final_artifact", n, B, NA_character_, function() serialize(artifact, NULL), 10L)
    measure("schedule_hash", n, B, NA_character_,
      function() get(".warm_start_prior_hash", ns)(artifact$comparisons[1:2]), 100L)
  }
  write.csv(dplyr::bind_rows(records), file.path(output, "components.csv"), row.names = FALSE)
  cat(n, B, "complete\n")
}
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
