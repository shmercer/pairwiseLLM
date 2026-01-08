test_that("55-01 make_llm_judge_fun defaults to return_mode = 'new' and returns results", {
  seen <- list()

  mock_submit <- function(...) {
    args <- list(...)
    seen <<- args

    list(
      results = tibble::tibble(custom_id = "x", winner = "ID1"),
      failed_pairs = tibble::tibble()
    )
  }

  testthat::local_mocked_bindings(
    submit_llm_pairs = mock_submit,
    .package = "pairwiseLLM"
  )

  judge_fun <- make_llm_judge_fun(
    backend = "openai",
    model = "gpt-4.1",
    save_path = "path.csv"
    # return_mode defaults to "new"
  )

  out <- judge_fun(tibble::tibble(ID1 = "a", text1 = "A", ID2 = "b", text2 = "B"))

  expect_equal(out$winner, "ID1")
  expect_equal(seen$backend, "openai")
  expect_equal(seen$model, "gpt-4.1")
  expect_equal(seen$save_path, "path.csv")
  expect_equal(seen$return_mode, "new")
})

test_that("55-02 make_llm_judge_fun can override return_mode", {
  seen <- list()

  mock_submit <- function(...) {
    args <- list(...)
    seen <<- args

    list(
      results = tibble::tibble(custom_id = "x", winner = "ID2"),
      failed_pairs = tibble::tibble()
    )
  }

  testthat::local_mocked_bindings(
    submit_llm_pairs = mock_submit,
    .package = "pairwiseLLM"
  )

  judge_fun <- make_llm_judge_fun(
    backend = "openai",
    model = "gpt-4.1",
    save_path = "path.csv",
    return_mode = "all"
  )

  out <- judge_fun(tibble::tibble(ID1 = "a", text1 = "A", ID2 = "b", text2 = "B"))

  expect_equal(out$winner, "ID2")
  expect_equal(seen$return_mode, "all")
})

test_that("55-03 make_llm_judge_fun forwards ... and freezes arguments", {
  seen <- list()

  mock_submit <- function(...) {
    args <- list(...)
    seen <<- args

    list(
      results = tibble::tibble(custom_id = "x", winner = "ID1"),
      failed_pairs = tibble::tibble()
    )
  }

  testthat::local_mocked_bindings(
    submit_llm_pairs = mock_submit,
    .package = "pairwiseLLM"
  )

  backend <- "anthropic"
  model <- "claude-3-5-sonnet-latest"
  temp <- 0.7
  top_p <- 0.9

  judge_fun <- make_llm_judge_fun(
    backend = backend,
    model = model,
    temperature = temp,
    top_p = top_p
  )

  # mutate the symbols after creation; closure should keep original values
  backend <- "openai"
  model <- "gpt-4.1"
  temp <- 0.1
  top_p <- 0.2

  judge_fun(tibble::tibble(ID1 = "a", text1 = "A", ID2 = "b", text2 = "B"))

  expect_equal(seen$backend, "anthropic")
  expect_equal(seen$model, "claude-3-5-sonnet-latest")
  expect_equal(seen$temperature, 0.7)
  expect_equal(seen$top_p, 0.9)
})
