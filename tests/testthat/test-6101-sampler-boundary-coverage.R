test_that("adaptive sampler rejects incomplete engine draws and forwards output paths", {
  testthat::local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() NULL,
    .package = "pairwiseLLM")
  results <- build_btl_results_data(data.frame(ID1 = "a", ID2 = "b", better_id = "a"))
  data <- .btl_mcmc_prepare_bt_data(results, c("a", "b"))
  config <- btl_mcmc_config(2L, list(model_variant = "btl_e_b"))
  draws <- cbind(`theta[1]` = c(-1, -0.5), `theta[2]` = c(1, 0.5),
    beta = c(0, 0.1), epsilon = c(0.1, 0.2))
  captured <- list()
  model <- function(...) {
    list(sample = function(...) {
      captured <<- list(...)
      list(draws = function(...) draws, diagnostic_summary = function() data.frame(num_divergent = 0L),
        summary = function(...) data.frame(rhat = 1, ess_bulk = 1000, ess_tail = 1000))
    })
  }
  run <- function(cfg = config, engine = model) {
    .fit_bayes_btl_mcmc_adaptive(data, cfg, seed = 91L, model_fn = engine)
  }
  config$cmdstan$output_dir <- withr::local_tempdir()
  fit <- run()
  expect_identical(captured$output_dir, config$cmdstan$output_dir)
  expect_identical(captured$seed, 91L)
  expect_identical(colnames(fit$draws$theta), c("a", "b"))
  for (column in c("theta[1]", "beta", "epsilon")) {
    original <- draws
    draws <- draws[, setdiff(colnames(draws), column), drop = FALSE]
    expect_error(run(), paste0("missing ", sub("\\[.*", "", column), " draws"))
    draws <- original
  }
  config$cmdstan$output_dir <- 1
  expect_error(run(), "output_dir.*character path")
  config$cmdstan$output_dir <- NULL
  expect_error(run(engine = 1), "model_fn.*function")
  config$cmdstan$iter_warmup <- 0L
  expect_error(run(), "positive integers")
})
