testthat::test_that("randomize_pair_order does not error when .Random.seed is missing", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples[1:4, ])

  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- NULL
  if (had) old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  on.exit(
    {
      if (had) {
        assign(".Random.seed", old, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  out <- randomize_pair_order(pairs, seed = 1)
  testthat::expect_equal(nrow(out), nrow(pairs))
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

testthat::test_that("estimate_llm_pairs_cost does not error when .Random.seed is missing", {
  data("example_writing_samples", package = "pairwiseLLM")
  pairs <- make_pairs(example_writing_samples[1:5, ]) |>
    sample_pairs(n_pairs = 6, seed = 1)

  fake_submit <- function(pairs,
                          model,
                          trait_name,
                          trait_description,
                          prompt_template,
                          backend,
                          endpoint,
                          ...) {
    tibble::tibble(
      prompt_tokens = rep(100L, nrow(pairs)),
      completion_tokens = rep(10L, nrow(pairs)),
      status_code = rep(200L, nrow(pairs))
    )
  }

  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- NULL
  if (had) old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  on.exit(
    {
      if (had) {
        assign(".Random.seed", old, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  est <- estimate_llm_pairs_cost(
    pairs = pairs,
    backend = "openai",
    model = "gpt-4.1",
    endpoint = "chat.completions",
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    mode = "live",
    n_test = 2,
    test_strategy = "random",
    seed = 1,
    cost_per_million_input = 1,
    cost_per_million_output = 1,
    .submit_fun = fake_submit
  )

  testthat::expect_true(inherits(est, "pairwiseLLM_cost_estimate"))
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
