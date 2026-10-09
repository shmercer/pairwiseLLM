#!/usr/bin/env Rscript
# Run from the package root; optional first argument is an output CSV path.
# Synthetic endpoints and initial predictions only. One worker at a time; no
# providers, sampling engines, package installation, or study execution.
# Unprofiled build timing and OS peak RSS come from a fresh R worker per case.
# RSS includes startup, package loading, fixture construction and the tree build;
# it is not incremental tree memory. Cumulative allocations are profiled separately.
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(quiet = TRUE)

fixture <- function(n, topology) {
  ids <- sprintf("i%04d", seq_len(n))
  if (topology %in% c("dense90", "dense99")) {
    pairs <- utils::combn(n, 2L)
    # Retain a path to guarantee connectivity, then sample remaining edges.
    path <- pairs[2L, ] - pairs[1L, ] == 1L
    keep <- withr::with_seed(315L, {
      extra <- which(!path)
      density <- if (topology == "dense90") 0.9 else 0.99
      c(which(path), sample(extra, floor(density * ncol(pairs)) - sum(path)))
    })
    pairs <- pairs[, keep, drop = FALSE]
  } else if (topology == "path") {
    pairs <- rbind(seq_len(n - 1L), seq.int(2L, n))
  } else {
    pairs <- rbind(rep(1L, n - 1L), seq.int(2L, n))
  }
  reverse <- seq_len(ncol(pairs)) %% 2L == 0L
  list(item_ids = ids,
    edges = data.frame(A_id = ids[ifelse(reverse, pairs[2L, ], pairs[1L, ])],
      B_id = ids[ifelse(reverse, pairs[1L, ], pairs[2L, ])]),
    initial_prediction = list(item_id = ids, mu = seq(-3, 3, length.out = n),
      sigma = seq(0.5, 1.5, length.out = n), beta = 1), seed = 315L)
}

# Private subprocess entrypoint. The parent never profiles this measurement.
if (length(args) == 2L && args[[1L]] == "--worker") {
  config <- readRDS(args[[2L]])
  f <- fixture(config$n, config$topology)
  gc()
  timing <- system.time(tree <- do.call(pairwiseLLM:::.adaptive_predictive_tree, f))
  saveRDS(list(elapsed_seconds = unname(timing[["elapsed"]]), tree = tree), config$result)
  quit(status = 0L)
}

measure_fresh <- function(n, topology) {
  root <- tempfile("tree-worker-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  result <- file.path(root, "result.rds")
  config <- file.path(root, "config.rds")
  memory <- file.path(root, "memory.txt")
  output <- file.path(root, "worker.log")
  saveRDS(list(n = n, topology = topology, result = result), config)
  rscript <- file.path(R.home("bin"), "Rscript")
  script <- normalizePath("scripts/benchmark-predictive-tree.R", winslash = "/")
  worker_args <- c("--vanilla", shQuote(script), "--worker", shQuote(config))
  platform <- Sys.info()[["sysname"]]
  supported <- file.exists("/usr/bin/time") && platform %in% c("Linux", "Darwin")
  if (supported) {
    time_args <- if (platform == "Linux") c("-f", shQuote("%M"), "-o", shQuote(memory)) else "-l"
    status <- system2("/usr/bin/time", c(time_args, shQuote(rscript), worker_args),
      stdout = output, stderr = if (platform == "Darwin") memory else output)
  } else {
    status <- system2(rscript, worker_args, stdout = output, stderr = output)
  }
  if (status != 0L || !file.exists(result)) {
    stop(paste(c("Tree worker failed:", readLines(output, warn = FALSE),
      if (file.exists(memory)) readLines(memory, warn = FALSE)), collapse = "\n"))
  }
  measured <- readRDS(result)
  measured$peak_rss_mib <- NA_real_
  measured$peak_rss_method <- "unavailable: /usr/bin/time requires Linux or macOS"
  if (supported) {
    lines <- readLines(memory, warn = FALSE)
    bytes <- if (platform == "Linux") as.numeric(lines[[1L]]) * 1024 else
      as.numeric(sub("^[[:space:]]*([0-9]+).*", "\\1", grep("maximum resident set size", lines, value = TRUE)))
    stopifnot(length(bytes) == 1L, is.finite(bytes), bytes > 0)
    measured$peak_rss_mib <- bytes / 1024^2
    measured$peak_rss_method <- paste(platform, "fresh-worker OS maximum resident set size")
  }
  measured
}

measure <- function(f, topology, repetition) {
  fresh <- measure_fresh(length(f$item_ids), topology)
  profile <- tempfile("tree-allocations-")
  on.exit(unlink(profile), add = TRUE)
  gc()
  utils::Rprofmem(profile)
  on.exit(utils::Rprofmem(NULL), add = TRUE)
  timing <- system.time(tree <- do.call(pairwiseLLM:::.adaptive_predictive_tree, f))
  utils::Rprofmem(NULL)
  allocations <- readLines(profile, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", allocations)))
  d <- attr(tree, "tree_diagnostics")
  n <- length(f$item_ids)
  e <- nrow(f$edges)
  stopifnot(identical(tree, fresh$tree), nrow(tree) == n - 1L, sum(d$degrees$degree) == 2L * (n - 1L),
    all(d$degrees$degree > 0L), d$operations$probability_evaluations == e,
    d$operations$component_checks <= e,
    d$operations$candidate_visits <= e * max(1, ceiling(log2(n - 1))))
  data.frame(topology = topology, n = n, edges = e, repetition = repetition,
    profiled_elapsed_seconds = unname(timing[["elapsed"]]),
    allocated_mib = sum(bytes, na.rm = TRUE) / 1024^2,
    probability_evaluations = d$operations$probability_evaluations,
    candidate_visits = d$operations$candidate_visits,
    component_checks = d$operations$component_checks, passes = d$operations$passes,
    max_degree = max(d$degrees$degree), relaxations = nrow(d$relaxations),
    degree_histogram = paste(d$degree_histogram$degree, d$degree_histogram$n_items,
      sep = ":", collapse = ";"),
    elapsed_seconds = fresh$elapsed_seconds, peak_rss_mib = fresh$peak_rss_mib,
    peak_rss_method = fresh$peak_rss_method,
    peak_rss_scope = "whole fresh worker including R/package startup, fixture and unprofiled build",
    seed = f$seed)
}

cases <- data.frame(n = c(rep(c(256L, 512L, 1164L), 2L), 1164L, 1164L),
  topology = c(rep("dense90", 3L), rep("dense99", 3L), "path", "hub"))
results <- list()
for (case in seq_len(nrow(cases))) {
  f <- fixture(cases$n[[case]], cases$topology[[case]])
  for (repetition in seq_len(3L)) {
    result <- measure(f, cases$topology[[case]], repetition)
    results[[length(results) + 1L]] <- result
    print(result, row.names = FALSE)
  }
}
results <- do.call(rbind, results)
results$package_version <- as.character(utils::packageVersion("pairwiseLLM"))
results$r_version <- R.version.string
results$platform <- R.version$platform
results$commit <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
results$working_tree_dirty <- length(system2("git", c("status", "--porcelain", "--untracked-files=no"),
  stdout = TRUE)) > 0L
if (length(args) > 0L) utils::write.csv(results, args[[1L]], row.names = FALSE)
cat("\n", R.version.string, "\n", sep = "")
cat("Tree: O(E log E) time, O(E + N) space. RSS is whole-worker peak, not allocated bytes.\n")
