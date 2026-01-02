test_that("submit_ollama_pairs_live returns empty result with raw_response when no pairs", {
  pairs <- tibble::tibble(
    ID1 = character(),
    text1 = character(),
    ID2 = character(),
    text2 = character()
  )

  out <- submit_ollama_pairs_live(
    pairs = pairs,
    model = "llama3",
    trait_name = "Trait",
    trait_description = "Desc",
    include_raw = TRUE,
    verbose = TRUE,
    progress = FALSE
  )

  expect_type(out, "list")
  expect_true(all(c("results", "failed_pairs") %in% names(out)))
  expect_true("raw_response" %in% names(out$results))
  expect_equal(nrow(out$results), 0)
})

test_that("submit_ollama_pairs_live creates save directory when needed", {
  testthat::skip_if_not_installed("readr")

  pairs <- tibble::tibble(
    ID1 = character(),
    text1 = character(),
    ID2 = character(),
    text2 = character()
  )

  tmp_dir <- tempfile("ollama_out_")
  save_path <- file.path(tmp_dir, "results.csv")

  expect_message(
    submit_ollama_pairs_live(
      pairs = pairs,
      model = "llama3",
      trait_name = "Trait",
      trait_description = "Desc",
      save_path = save_path,
      verbose = TRUE,
      progress = FALSE
    ),
    "Creating output directory"
  )

  expect_true(dir.exists(tmp_dir))
})
