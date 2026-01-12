test_that("ollama_compare_pair_live validates inputs", {
  expect_error(
    pairwiseLLM::ollama_compare_pair_live(
      ID1 = "A",
      text1 = "t1",
      ID2 = 1,
      text2 = "t2",
      model = "m",
      trait_name = "t",
      trait_description = "d"
    ),
    "`ID2` must be a single character",
    fixed = TRUE
  )
})

test_that("ensure_only_ollama_model_loaded handles ps failures and edge cases", {
  # ps error path
  testthat::local_mocked_bindings(
    .ollama_system2 = function(...) stop("nope"),
    .env = asNamespace("pairwiseLLM")
  )
  expect_identical(pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE), invisible(character(0)))

  # ps returns no output
  testthat::local_mocked_bindings(
    .ollama_system2 = function(...) structure(character(0), status = 0L),
    .env = asNamespace("pairwiseLLM")
  )
  expect_length(pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE), 0L)

  # ps returns header only
  testthat::local_mocked_bindings(
    .ollama_system2 = function(...) structure(c("NAME ID"), status = 0L),
    .env = asNamespace("pairwiseLLM")
  )
  expect_length(pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE), 0L)

  # could not parse model names
  testthat::local_mocked_bindings(
    .ollama_system2 = function(...) structure(c("NAME ID", "   "), status = 0L),
    .env = asNamespace("pairwiseLLM")
  )
  expect_length(pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE), 0L)

  # only target model active
  testthat::local_mocked_bindings(
    .ollama_system2 = function(...) structure(c("NAME ID", "m 123"), status = 0L),
    .env = asNamespace("pairwiseLLM")
  )
  expect_length(pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE), 0L)

  # unload other models
  unloaded <- character(0)
  testthat::local_mocked_bindings(
    .ollama_system2 = function(command, args, stdout = TRUE, stderr = TRUE, ...) {
      if (identical(args, "ps")) {
        structure(c("NAME ID", "m 123", "other 456"), status = 0L)
      } else {
        unloaded <<- c(unloaded, args[[2]])
        ""
      }
    },
    .env = asNamespace("pairwiseLLM")
  )
  out <- pairwiseLLM::ensure_only_ollama_model_loaded("m", verbose = FALSE)
  expect_identical(out, "other")
  expect_identical(unloaded, "other")
})
