#!/usr/bin/env Rscript
# Optional BLAS-specific reproduction of the ill-conditioned diagnostic guard.
# Run under the BLAS being checked; OUTPUT is a CSV file, not an R library.
args <- commandArgs(TRUE)
pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-bt-refits-324.R")
ref <- bt_reference_324("tests/testthat/fixtures/bt-refits-324")
dat <- bt_fixture_324(8L, "near_separated")
old <- tryCatch(ref$fit_bt_model(dat, "alpha", FALSE, alpha = 1e-10), error = identity)
new <- tryCatch(fit_bt_model(dat, "alpha", FALSE, alpha = 1e-10), error = identity)
kernel <- pairwiseLLM:::.bt_alpha_kernel(pairwiseLLM:::.bt_binary_design(dat), 1e-10)
indexed <- pairwiseLLM:::.bt_alpha_surface(old$diagnostics$coefficients, kernel)
upper <- chol(indexed$hessian)
correction <- backsolve(upper, forwardsolve(t(upper), indexed$gradient))
indexed$newton_correction <- as.vector(kernel$transform %*% correction)
indexed$step_max <- max(abs(indexed$newton_correction))
fields <- c("hessian", "information", "newton_correction", "step_max")
result <- do.call(rbind, lapply(fields, function(field) {
  expected <- old$diagnostics[[field]]
  data.frame(field = field,
    indexed_max_abs_error = max(abs(indexed[[field]] - expected)),
    guarded_max_abs_error = max(abs(new$diagnostics[[field]] - expected)),
    hessian_rcond = old$diagnostics$hessian_checks$rcond,
    exact_guarded_diagnostics = identical(new$diagnostics, old$diagnostics))
}))
stopifnot(all(result$exact_guarded_diagnostics))
write.csv(result, args[[1L]], row.names = FALSE)
print(result, row.names = FALSE)
print(sessionInfo())
