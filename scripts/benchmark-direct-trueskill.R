#!/usr/bin/env Rscript
# Run from the package root with an optional output directory argument.
# Offline synthetic scoring, full selection and 2N-comparison trajectories.
# No providers, sampler fits, study files or installation into existing libraries.
# Timing excludes fixture construction, equality checks and scoped mock setup.
# Allocation profiling is separate from unprofiled timing; it is not peak RSS.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile("direct-trueskill-benchmark-")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
stopifnot(requireNamespace("microbenchmark", quietly = TRUE))
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-direct-trueskill-326.R")
reference <- direct_scalar_reference_326("tests/testthat/fixtures/direct-trueskill-326")

results <- new.env(parent = emptyenv())
results$timings <- list()
results$allocations <- list()
record <- function(n, strategy, distribution, workload, implementation, seconds) {
  data.frame(n = n, strategy = strategy, distribution = distribution, workload = workload,
    implementation = implementation, repetition = seq_along(seconds), seconds = as.numeric(seconds))
}
allocate <- function(fun) {
  path <- tempfile("direct-trueskill-allocations-")
  on.exit(unlink(path), add = TRUE)
  gc()
  utils::Rprofmem(path)
  on.exit(utils::Rprofmem(NULL), add = TRUE)
  value <- fun()
  utils::Rprofmem(NULL)
  lines <- readLines(path, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  list(value = value, bytes = sum(bytes, na.rm = TRUE), allocations = sum(!is.na(bytes)))
}

# The scalar selector is the frozen original, including its original scoring
# loop. Overlay only that function for full selection; shared production helpers
# and the outer select_next_pair() stay identical for both implementations.
select_scalar <- pairwiseLLM:::select_next_pair
environment(select_scalar) <- list2env(list(.adaptive_select_direct = reference$.adaptive_select_direct),
  parent = environment(select_scalar))
select_vectorized <- pairwiseLLM:::select_next_pair

testthat::test_that("all timed workloads are exactly equivalent to the frozen scalar implementation", {
  withr::local_seed(326L)
  # Warm the complete run/update call graph before collecting any trajectory
  # timings, using a separate small fixture whose memo cache is never reused.
  for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
    testthat::with_mocked_bindings(direct_run_326(direct_fixture_326(8L, strategy)),
      .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
    direct_run_326(direct_fixture_326(8L, strategy))
  }
  for (n in c(57L, 91L, 229L)) {
    for (distribution in c("cold", "heterogeneous")) {
      for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
        initial <- direct_fixture_326(n, strategy, distribution)
        state <- initial
        state$warm_start_done <- TRUE
        ts <- state$trueskill_state
        focal <- state$item_ids[[1L]]
        partners <- rev(state$item_ids[-1L])
        scalar_score <- function() reference$probabilities(focal, partners, ts)
        vectorized_score <- function() pairwiseLLM:::.adaptive_direct_partner_probabilities(focal, partners, ts)
        scalar_select <- function() select_scalar(state)
        vectorized_select <- function() select_vectorized(state)
        before <- serialize(state, NULL)
        rng <- .Random.seed
        kind <- RNGkind()
        testthat::expect_identical(scalar_score(), vectorized_score())
        testthat::expect_identical(scalar_select(), vectorized_select())
        for (workload in c("scoring", "selection")) {
          old <- if (workload == "scoring") scalar_score else scalar_select
          new <- if (workload == "scoring") vectorized_score else vectorized_select
          for (warmup in seq_len(10L)) {
            old()
            new()
          }
          gc()
          measured <- microbenchmark::microbenchmark(scalar = old(), vectorized = new(),
            times = if (workload == "scoring") 100L else 30L,
            control = list(order = "inorder", warmup = 10L))
          for (implementation in c("scalar", "vectorized")) {
            results$timings[[length(results$timings) + 1L]] <- record(n, strategy, distribution, workload,
              implementation, measured$time[measured$expr == implementation] / 1e9)
            profiled <- allocate(if (implementation == "scalar") old else new)
            testthat::expect_identical(profiled$value, old())
            results$allocations[[length(results$allocations) + 1L]] <- data.frame(n = n, strategy = strategy,
              distribution = distribution, workload = workload, implementation = implementation,
              allocated_bytes = profiled$bytes, allocation_count = profiled$allocations)
          }
        }
        testthat::expect_identical(serialize(state, NULL), before)
        testthat::expect_identical(.Random.seed, rng)
        testthat::expect_identical(RNGkind(), kind)

        # A full bootstrap boundary followed by N+1 direct comparisons. The
        # existing test-only refit target avoids sampler calls; no scientific
        # configuration or stopping behavior is changed in production code.
        expected <- NULL
        expected_input <- NULL
        saved_initial <- serialize(initial, NULL)
        for (repetition in seq_len(3L)) {
          order <- if (repetition %% 2L) c("scalar", "vectorized") else c("vectorized", "scalar")
          for (implementation in order) {
            # Isolate the mutable memo environment; cloning is outside timing.
            input <- unserialize(saved_initial)
            run <- function() {
              gc()
              elapsed <- system.time(value <- direct_run_326(input))[["elapsed"]]
              list(value = value, seconds = elapsed)
            }
            measured <- if (implementation == "scalar") {
              testthat::with_mocked_bindings(run(),
                .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
            } else {
              run()
            }
            if (is.null(expected)) {
              expected <- serialize(measured$value, NULL)
              expected_input <- serialize(input, NULL)
            }
            testthat::expect_identical(serialize(measured$value, NULL), expected)
            testthat::expect_identical(serialize(input, NULL), expected_input)
            testthat::expect_identical(nrow(measured$value$history_pairs), 2L * n)
            testthat::expect_identical(sum(measured$value$step_log$round_stage == "direct_pairing"), n + 1L)
            testthat::expect_null(measured$value$btl_fit)
            testthat::expect_identical(serialize(initial, NULL), saved_initial)
            testthat::expect_identical(.Random.seed, rng)
            testthat::expect_identical(RNGkind(), kind)
            row <- record(n, strategy, distribution, "trajectory", implementation, measured$seconds)
            row$repetition <- repetition
            results$timings[[length(results$timings) + 1L]] <- row
          }
        }
        cat(sprintf("N=%d %s %s: exact scoring, selection and trajectory agreement\n", n, strategy, distribution))
        utils::write.csv(do.call(rbind, results$timings), file.path(output, "timings.csv"), row.names = FALSE)
        utils::write.csv(do.call(rbind, results$allocations), file.path(output, "allocations.csv"), row.names = FALSE)
      }
    }
  }
})

timings <- do.call(rbind, results$timings)
groups <- split(timings, interaction(timings$n, timings$strategy, timings$distribution, timings$workload, drop = TRUE))
summary <- do.call(rbind, lapply(groups, function(x) {
  old <- x$seconds[x$implementation == "scalar"]
  new <- x$seconds[x$implementation == "vectorized"]
  cbind(x[1L, c("n", "strategy", "distribution", "workload")],
    scalar_median_seconds = median(old), vectorized_median_seconds = median(new),
    scalar_iqr_seconds = IQR(old), vectorized_iqr_seconds = IQR(new),
    speedup = median(old) / median(new), saved_seconds = median(old) - median(new))
}))
utils::write.csv(summary, file.path(output, "summary.csv"), row.names = FALSE)
sources <- c("R/adaptive_pairing_strategy.R", "R/adaptive_trueskill.R",
  "tests/testthat/helper-direct-trueskill-326.R", "scripts/benchmark-direct-trueskill.R",
  "tests/testthat/fixtures/direct-trueskill-326/adaptive_pairing_strategy.R",
  "tests/testthat/fixtures/direct-trueskill-326/adaptive_trueskill.R")
provenance <- list(base_sha = "589cf73e073bcd9017ca14e5b3f8ceed62fce022",
  head_sha = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  tracked_changes = system2("git", c("status", "--short", "--untracked-files=no"), stdout = TRUE),
  source_md5 = as.list(tools::md5sum(sources)), seed = 326L,
  trajectory_committed_pairs = "2N, including N-1 bootstrap and N+1 direct selections",
  timing = "Unprofiled elapsed; setup/equality/profiling excluded; GC remains included during workload",
  memory = "Separate cumulative R allocations, not peak resident memory",
  limitations = "Synthetic serial trajectories with no sampler fits; not full bootstrap or study timing",
  session = capture.output(sessionInfo()))
jsonlite::write_json(provenance, file.path(output, "provenance.json"), pretty = TRUE, auto_unbox = TRUE)
print(summary, row.names = FALSE)
cat("Raw timings, allocations, summary and provenance:", normalizePath(output), "\n")
