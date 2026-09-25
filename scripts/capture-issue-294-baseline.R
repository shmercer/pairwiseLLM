# Run with Rscript /path/to/this/script.R OLD_CHECKOUT OUTPUT_RDS.
# Only synthetic fixtures are fitted; no study artifacts or providers are used.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
repo <- normalizePath(args[1], mustWork = TRUE)
revision <- system2("git", c("-C", shQuote(repo), "rev-parse", "HEAD"), stdout = TRUE)
stopifnot(identical(revision, "03cc16c5953d9b1a74345c97324ebd6578248040"))
stopifnot(system2("git", c("-C", shQuote(repo), "diff", "--quiet", "HEAD", "--",
  "R", "tests/testthat/helper-link-*.R")) == 0L)
pkgload::load_all(repo, quiet = TRUE)
for (helper in c("contract", "e1", "e2", "e3")) {
  source(file.path(repo, "tests/testthat", paste0("helper-link-", helper, ".R")))
}
inputs <- list(E1 = do.call(prepare_link_input, link_e1_mixed_args()),
  E2 = link_e2_input(), E3 = link_e3_input())
saved <- lapply(inputs, function(input) {
  fit <- fit_link(input)
  pairs <- input$cross[, setdiff(names(input$cross), "y_A")]
  stopifnot(isTRUE(fit$diagnostics$fit_valid))
  list(fit = fit, pairs = pairs, probabilities = predict_link(fit, pairs))
})
scalars <- expand.grid(mu = c(-8, -2, 0, 2, 8), sd = c(0, .25, 1, 3))
control <- pairwiseLLM:::.link_e2_controls(list())
scalars$value <- mapply(function(mu, sd) pairwiseLLM:::.link_e2_integrate(mu, sd, control),
  scalars$mu, scalars$sd)
saveRDS(list(commit = revision, version = as.character(packageVersion("pairwiseLLM")),
  R = R.version.string, saved = saved, scalars = scalars), args[2], version = 3L)
