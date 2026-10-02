# Frozen issue-305 qualification: do not tune this matrix to fitted outcomes.
# Run from the repository root; optional first argument is the output CSV path.
# No providers, private data, sampling engines, or optional engine installation.
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-bt-lapse.R")
destination <- commandArgs(trailingOnly = TRUE)
if (!length(destination)) destination <- file.path(tempdir(), "bt-lapse-305-boundary.csv")
if (file.exists(destination[1L])) stop("Use a new output path; qualification evidence must not be overwritten.")
designs <- data.frame(n = c(4L, 8L, 12L, 8L, 12L),
                      graph = c(rep("complete", 3L), rep("cycle_chords", 2L)))
grid <- expand.grid(design = seq_len(nrow(designs)), beta = c(0, -0.3, 0.3),
                     epsilon = c(0.001, 0.05, 0.2, 0.4), seed = c(NA_integer_, 30501:30505))
# Additional true-zero boundary cases are reported, never counted as recovered
# merely because the implementation correctly refuses ordinary joint SEs.
grid <- rbind(grid, expand.grid(design = seq_len(nrow(designs)), beta = c(0, -0.3, 0.3),
                                epsilon = 0, seed = c(NA_integer_, 30501:30505)))
records <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  spec <- grid[i, ]
  design <- designs[spec$design, ]
  population <- is.na(spec$seed)
  case <- lapse_case(design$n, spec$beta, spec$epsilon, design$graph,
                      seed = if (population) NULL else spec$seed, total = 2000)
  fit <- lapse_error(lapse_fit_case(case))
  valid <- inherits(fit, "pairwiseLLM_bt_lapse")
  errors <- if (is.null(fit$theta)) {
    rep(NA_real_, 3L)
  } else {
    c(max(abs(fit$theta$theta - case$theta)), abs(fit$beta - case$beta), abs(fit$epsilon - case$epsilon))
  }
  tolerance <- if (population) rep(1e-5, 3L) else c(0.35, 0.15, 0.08)
  records[[i]] <- data.frame(n = design$n, graph = design$graph, beta = spec$beta, epsilon = spec$epsilon,
    seed = spec$seed, population = population, per_ordered_edge = 2000L, valid_fit = valid,
    status = if (valid) fit$provenance$convergence$status else fit$failure_reason,
    fit_class = if (!valid) "numerical_failure" else if (fit$epsilon == 0) "boundary_valid" else "interior_valid",
    uncertainty_valid = fit$provenance$uncertainty$valid,
    uncertainty_status = fit$provenance$uncertainty$status,
    failed_joint_attempts = sum(vapply(fit$diagnostics$optimization$attempts, function(x) x$code != 0L, logical(1))),
    optimizer_warning_count = sum(vapply(fit$diagnostics$optimization$attempts,
      function(x) length(x$warnings), integer(1))),
    theta_error = errors[1L], beta_error = errors[2L], epsilon_error = errors[3L],
    beta_estimate = fit$beta, epsilon_estimate = fit$epsilon, objective = fit$diagnostics$value %||% NA_real_,
    recovery_pass = valid && all(is.finite(errors)) && all(errors <= tolerance),
    gradient_max = fit$diagnostics$gradient_max %||% NA_real_,
    hessian_rcond = fit$diagnostics$hessian_checks$rcond %||% NA_real_,
    boundary_epsilon_score = fit$diagnostics$boundary_zero_checks$epsilon_score %||% NA_real_,
    boundary_kkt_violation = fit$diagnostics$boundary_zero_checks$kkt_violation %||% NA_real_)
  if (i %% 30L == 0L) cat("Completed", i, "of", nrow(grid), "frozen cases\n")
}
result <- do.call(rbind, records)
result$recovery_failure <- result$valid_fit & !result$recovery_pass
utils::write.csv(result, destination[1L], row.names = FALSE, na = "NA")
print(with(result, table(population, status)))
print(with(result, table(epsilon, fit_class)))
cat("Recovery failures among valid fits:", sum(result$recovery_failure), "\n")
cat("Recovery passes:", sum(result$recovery_pass), "of", nrow(result), "\n")
cat("Full evidence:", destination[1L], "\n")
# A successful script exit means the audit ran, not that qualification passed.
