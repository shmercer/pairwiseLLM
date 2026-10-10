#!/usr/bin/env Rscript
# Run BEFORE optimization or against the frozen baseline; offline and report-only.
args <- commandArgs(TRUE)
output <- if (length(args)) args[[1L]] else tempfile("bt-profile-")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-bt-refits-324.R")
ref <- bt_reference_324("tests/testthat/fixtures/bt-refits-324")

# Instrument top-level expressions in a PRIVATE baseline environment. Assignment
# and return values retain the original evaluation frame. Timings include nested
# work, so helper rows replace (rather than add to) their calling expression.
profile_records <- new.env(parent = emptyenv())
profile_records$rows <- list()
measure <- function(stage, expr, envir) {
  path <- tempfile()
  gc()
  utils::Rprofmem(path)
  time <- system.time(value <- eval(expr, envir))
  utils::Rprofmem(NULL)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path, warn = FALSE))))
  unlink(path)
  profile_records$rows[[length(profile_records$rows) + 1L]] <- data.frame(stage = stage,
    elapsed = unname(time[["elapsed"]]), cpu = unname(sum(time[1:2])),
    allocated_bytes = sum(bytes, na.rm = TRUE), allocations = sum(!is.na(bytes)))
  value
}
rows <- list()
fixtures <- stats::setNames(lapply(c(57L, 91L, 229L), bt_fixture_324), c("sparse57", "sparse91", "sparse229"))
public <- new.env()
utils::data("example_writing_pairs", package = "pairwiseLLM", envir = public)
fixtures$public_writing <- build_bt_data(public$example_writing_pairs)
if (length(args) > 1L) fixtures <- fixtures[args[[2L]]]
for (fixture in names(fixtures)) {
  dat <- fixtures[[fixture]]
  n <- length(unique(c(dat[[1L]], dat[[2L]])))
  for (engine in c("alpha", "brglm2")) {
    for (run in 1:2) {
      profile_records$rows <- list()
      env <- new.env(parent = ref)
      env$dat <- dat
      # Split baseline graph preparation at its exact expression boundary.
      expressions <- as.list(body(ref$.bt_binary_design))[-1L]
      prepared <- measure("mapping_and_counts", as.call(c(list(as.name("{")), expressions[1:7])), env)
      prepared <- measure("contrast_and_design",
        as.call(c(list(as.name("{")), expressions[8:length(expressions)])), env)
      env$prepared <- prepared
      env$control <- if (engine == "alpha") {
        ref$.bt_alpha_control(list(), FALSE)
      } else {
        ref$.bt_firth_control(list(), FALSE)
      }
      if (engine == "alpha") {
        env$kernel <- measure("alpha_structure", quote(.bt_alpha_kernel(prepared, 0.5)), env)
        env$fit <- measure("solver", quote(.bt_alpha_glm(kernel, control)), env)
        env$surface <- measure("uncertainty_and_diagnostics", quote({
          surface <- .bt_alpha_surface(fit$coefficients, kernel)
          penalized <- .bt_alpha_matrix(surface$hessian)
          original <- .bt_alpha_matrix(surface$information)
          correction <- backsolve(penalized$chol, forwardsolve(t(penalized$chol), surface$gradient))
          covariance <- .bt_item_covariance(chol2inv(original$chol), prepared$transform, prepared$ids, "Alpha")
          surface
        }), env)
      } else {
        env$fit <- measure("solver", quote(.bt_firth_glm(prepared$counts, prepared$design, control)), env)
        measure("uncertainty_and_diagnostics", quote({
          covariance <- .bt_firth_covariance(stats::vcov(fit), prepared$transform, prepared$ids)
          covariance
        }), env)
      }
      measure("result_components", quote({
        theta <- tibble::tibble(ID = prepared$ids, theta = as.vector(prepared$transform %*% fit$coefficients),
          se = unname(sqrt(diag(covariance))))
        ssr <- .bt_centered_ssr(theta$theta, theta$se, "BT")
        comparisons <- tibble::tibble(object1 = dat[[1L]], object2 = dat[[2L]])
        list(theta = theta, vcov = covariance, ssr = ssr, comparisons = comparisons)
      }), env)
      env$engine <- engine
      measure("full_fit", quote(fit_bt_model(dat, engine, verbose = FALSE,
        alpha = if (engine == "alpha") 0.5 else NULL)), env)
      profile <- file.path(output, paste(n, engine, run, "Rprof", sep = "."))
      utils::Rprof(profile, interval = 0.01, memory.profiling = TRUE)
      for (sample in seq_len(if (n < 57L) 10L else 1L)) {
        ref$fit_bt_model(dat, engine, verbose = FALSE, alpha = if (engine == "alpha") 0.5 else NULL)
      }
      utils::Rprof(NULL)
      capture.output(summaryRprof(profile, memory = "both"), file = paste0(profile, ".txt"))
      rows[[length(rows) + 1L]] <- cbind(fixture = fixture, n = n, engine = engine, run = run,
        do.call(rbind, profile_records$rows))
      utils::write.csv(do.call(rbind, rows), file.path(output, "baseline-profile.csv"), row.names = FALSE)
      cat(n, engine, run, "complete\n")
    }
  }
}
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
