# Explicit offline maintainer check. Run from the repository root with Rscript.
# Requires an existing CmdStan installation; never installs external software.
# Copies models to a temporary directory, leaving source Stan files unchanged.
root <- tempfile("task08-stan-")
dir.create(root)
results <- list()
for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
  src <- readLines(file.path("inst", "stan", paste0(variant, ".stan")))
  path <- file.path(root, paste0(variant, ".stan"))
  writeLines(src, path)
  old <- src[!grepl("vector.*prior_(mean|sd)", src)]
  old <- gsub("normal(prior_mean, prior_sd)", "normal(0, 1)", old, fixed = TRUE)
  old_path <- file.path(root, paste0(variant, "_old.stan"))
  writeLines(old, old_path)
  model <- cmdstanr::cmdstan_model(path, quiet = TRUE)
  old_model <- cmdstanr::cmdstan_model(old_path, quiet = TRUE)
  data <- list(N = 3L, M = 2L, A = c(1L, 1L), B = c(2L, 2L), Y = c(1L, 0L))
  sample_model <- function(m, d) {
    m$sample(data = d, seed = 808L,
    chains = 2L, parallel_chains = 2L, iter_warmup = 500L, iter_sampling = 1000L,
      refresh = 0, show_messages = FALSE)
  }
  old_fit <- sample_model(old_model, data)
  cold <- sample_model(model, c(data, list(prior_mean = rep(0, 3), prior_sd = rep(1, 3))))
  warm <- sample_model(model, c(data, list(prior_mean = c(1, -1, 0), prior_sd = rep(0.5, 3))))
  a <- old_fit$summary("theta", mean = mean, mcse_mean = posterior::mcse_mean, rhat = posterior::rhat)
  b <- cold$summary("theta", mean = mean, mcse_mean = posterior::mcse_mean, rhat = posterior::rhat)
  c <- warm$summary("theta", mean = mean, mcse_mean = posterior::mcse_mean, rhat = posterior::rhat)
  # Six pooled MCSEs plus 0.02 numerical allowance, on every centered theta mean.
  tol <- 6 * sqrt(a$mcse_mean^2 + b$mcse_mean^2) + 0.02
  stopifnot(length(tol) == 3L, all(is.finite(tol)), all(abs(a$mean - b$mean) < tol), c$mean[1] - c$mean[2] > 0.8,
    all(c$rhat < 1.05), all(b$rhat < 1.05))
  results[[variant]] <- data.frame(variant, max_cold_mean_difference = max(abs(a$mean - b$mean)),
    min_cold_tolerance = min(tol), warm_a_minus_b = c$mean[1] - c$mean[2],
    max_rhat = max(c(b$rhat, c$rhat)),
    divergences = sum(cold$diagnostic_summary()$num_divergent) + sum(warm$diagnostic_summary()$num_divergent))
  print(results[[variant]])
}
output <- commandArgs(trailingOnly = TRUE)
if (length(output)) write.csv(do.call(rbind, results), output[[1]], row.names = FALSE)
cat("All four Stan cold-start and informative-prior comparisons passed.\n")
