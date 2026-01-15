testthat::test_that(".pairwiseLLM_with_seed validates seed input", {
  testthat::expect_error(
    .pairwiseLLM_with_seed(NA_real_, function() 1),
    "seed"
  )
})

testthat::test_that(".pairwiseLLM_with_seed removes transient RNG state", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL

  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  if (had_seed) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  out <- .pairwiseLLM_with_seed(123, function() stats::runif(1))
  testthat::expect_true(is.numeric(out))
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
