#!/usr/bin/env Rscript
# Run from the package root; optional first argument is an output CSV path.
# Synthetic endpoints and initial predictions only. Single process, no providers,
# no sampling engines, no package installation, and no 96-cell study execution.
# Timings include allocation profiling; allocated bytes are cumulative, not RSS.
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(quiet = TRUE)

fixture <- function(n, topology) {
  ids <- sprintf("i%04d", seq_len(n))
  if (topology == "dense90") {
    pairs <- utils::combn(n, 2L)
    # Retain a path to guarantee connectivity, then sample remaining edges.
    path <- pairs[2L, ] - pairs[1L, ] == 1L
    keep <- withr::with_seed(315L, {
      extra <- which(!path)
      c(which(path), sample(extra, floor(0.9 * ncol(pairs)) - sum(path)))
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

measure <- function(f, topology, repetition) {
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
  stopifnot(nrow(tree) == n - 1L, sum(d$degrees$degree) == 2L * (n - 1L),
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
      sep = ":", collapse = ";"))
}

cases <- data.frame(n = c(256L, 512L, 1164L, 1164L, 1164L),
  topology = c("dense90", "dense90", "dense90", "path", "hub"))
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
if (length(args) > 0L) utils::write.csv(results, args[[1L]], row.names = FALSE)
cat("\n", R.version.string, "\n", sep = "")
