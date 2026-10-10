#!/usr/bin/env Rscript
# Offline, unprofiled per-fit timing and SEPARATE cumulative R allocations.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else tempfile("bt-components-")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-bt-refits-324.R")
ref <- bt_reference_324("tests/testthat/fixtures/bt-refits-324")
timings <- allocations <- candidates <- equivalence <- list()
allocated <- function(fun) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  gc()
  utils::Rprofmem(path)
  on.exit(utils::Rprofmem(NULL), add = TRUE)
  value <- fun()
  utils::Rprofmem(NULL)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path, warn = FALSE))))
  c(bytes = sum(bytes, na.rm = TRUE), allocations = sum(!is.na(bytes)), result_bytes = as.numeric(object.size(value)))
}
for (n in c(57L, 91L, 229L)) {
  dat <- bt_fixture_324(n)
  for (engine in c("alpha", "brglm2")) {
    cache <- pairwiseLLM:::.bt_prepare_schedule(dat,
      pairwiseLLM:::.bt_prepare_items(unique(c(dat[[1L]], dat[[2L]])), engine == "alpha"))
    old <- function() ref$fit_bt_model(dat, engine, FALSE, alpha = if (engine == "alpha") .5 else NULL)
    new <- function() {
      if (engine == "alpha") {
        pairwiseLLM:::.bt_fit_alpha(dat, .5, FALSE, list(), cache)
      } else {
        pairwiseLLM:::.bt_fit_firth(dat, FALSE, list(), cache)
      }
    }
    expected <- old()
    actual <- new()
    stopifnot(identical(actual$theta$theta, expected$theta$theta),
      all(abs(actual$vcov - expected$vcov) <= 1e-10 * (1 + abs(expected$vcov))))
    equivalence[[length(equivalence) + 1L]] <- data.frame(n = n, engine = engine,
      theta_max_abs = max(abs(actual$theta$theta - expected$theta$theta)),
      se_max_abs = max(abs(actual$theta$se - expected$theta$se)),
      covariance_max_abs = max(abs(actual$vcov - expected$vcov)),
      information_max_abs = if (engine == "alpha") {
        max(abs(actual$diagnostics$information - expected$diagnostics$information))
      } else {
        0
      }, exact_convergence = identical(actual$provenance$convergence, expected$provenance$convergence))
    write.csv(do.call(rbind, equivalence), file.path(output, "equivalence.csv"), row.names = FALSE)
    # R may JIT-compile these wrapper closures on their second call. Warm both
    # paths before timing so the mean describes repeated fitting, not compilation.
    for (warmup in 1:3) {
      old()
      new()
    }
    for (run in 1:2) {
      for (implementation in if (run == 1L) c("baseline", "prepared") else c("prepared", "baseline")) {
        fun <- if (implementation == "baseline") old else new
        for (replicate in 1:3) {
          gc()
          elapsed <- system.time(fun())[["elapsed"]]
          timings[[length(timings) + 1L]] <- data.frame(n = n, engine = engine, implementation = implementation,
            run = run, replicate = replicate, seconds = elapsed)
        }
        memory <- allocated(fun)
        allocations[[length(allocations) + 1L]] <- data.frame(n = n, engine = engine,
          implementation = implementation, run = run, bytes = memory[[1L]], allocations = memory[[2L]],
          result_bytes = memory[[3L]],
          cache_bytes = if (implementation == "prepared") as.numeric(object.size(cache)) else 0)
      }
    }
    cat(n, engine, "fits complete\n")
    write.csv(do.call(rbind, timings), file.path(output, "fits.csv"), row.names = FALSE)
    write.csv(do.call(rbind, allocations), file.path(output, "allocations.csv"), row.names = FALSE)
  }
  prepared <- ref$.bt_binary_design(dat)
  kernel <- ref$.bt_alpha_kernel(prepared, .5)
  a <- match(dat[[1L]], prepared$ids)
  b <- match(dat[[2L]], prepared$ids)
  keys <- (pmin(a, b) - 1) * n + pmax(a, b)
  index <- match(keys, sort(unique(keys)))
  y <- ifelse(a < b, dat[[3L]], 1 - dat[[3L]])
  counts <- data.frame(first = pmin(a, b), second = pmax(a, b), wins = y, losses = 1 - y)
  weights <- (kernel$counts$wins + kernel$counts$losses) * .2
  sparse <- Matrix::Matrix(kernel$design, sparse = TRUE)
  control <- pairwiseLLM:::.bt_firth_control(list(), FALSE)
  functions <- list(
    aggregate_counts = function() stats::aggregate(cbind(wins, losses) ~ first + second, counts, sum),
    indexed_counts = function() {
      wins <- as.double(tabulate(index[y == 1], nrow(prepared$counts)))
      cbind(wins, tabulate(index, nrow(prepared$counts)) - wins)
    },
    rowsum_counts = function() rowsum(cbind(y, 1 - y), index),
    dense_information = function() crossprod(kernel$design, kernel$design * weights),
    sparse_information = function() as.matrix(Matrix::crossprod(sparse, sparse * weights)),
    indexed_information = function() pairwiseLLM:::.bt_pair_information(weights, kernel),
    firth_formula = function() ref$.bt_firth_glm(prepared$counts, prepared$design, control),
    firth_direct = function() {
      brglm2::brglmFit(prepared$design, as.matrix(prepared$counts[c("wins", "losses")]), family = binomial("logit"),
        start = rep(0, n - 1L), control = control, intercept = FALSE, singular.ok = FALSE)
    })
  for (candidate in names(functions)) {
    fun <- functions[[candidate]]
    fun()
    times <- microbenchmark::microbenchmark(fun(), times = 10L)$time / 1e9
    memory <- allocated(fun)
    candidates[[length(candidates) + 1L]] <- data.frame(n = n, candidate = candidate,
      mean_seconds = mean(times), median_seconds = median(times), bytes = memory[[1L]], allocations = memory[[2L]])
  }
  write.csv(do.call(rbind, candidates), file.path(output, "candidates.csv"), row.names = FALSE)
}
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
