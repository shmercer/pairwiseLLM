# -------------------------------------------------------------------------
# build_elo_data tests
# -------------------------------------------------------------------------

test_that("build_elo_data converts better_id into winner/loser", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1", "S2", "S3"),
    ID2       = c("S2", "S3", "S3", "S4"),
    better_id = c("S1", "S3", "S2", "S4")
  )

  elo <- build_elo_data(results)

  expect_s3_class(elo, "tbl_df")
  expect_identical(names(elo), c("winner", "loser"))

  # For each row, winner should be better_id;
  # loser should be the other ID in the pair.
  expect_equal(elo$winner, c("S1", "S3", "S2", "S4"))
  expect_equal(elo$loser, c("S2", "S1", "S3", "S3"))
})

test_that("build_elo_data drops rows with invalid or missing better_id", {
  results <- tibble::tibble(
    ID1       = c("S1", "S2", "S3", "S4"),
    ID2       = c("S2", "S3", "S4", "S1"),
    better_id = c("S1", "NOT_AN_ID", NA, "S1")
  )

  elo <- build_elo_data(results)

  # Only rows 1 and 4 are valid
  expect_equal(nrow(elo), 2)
  expect_equal(elo$winner, c("S1", "S1"))
  expect_equal(elo$loser, c("S2", "S4"))
})

test_that("build_elo_data errors when required columns are missing", {
  bad <- tibble::tibble(
    ID1 = c("S1", "S2"),
    ID2 = c("S3", "S4")
  )

  expect_error(
    build_elo_data(bad),
    "must contain columns",
    fixed = FALSE
  )
})

test_that("build_elo_data works on example_writing_pairs", {
  data("example_writing_pairs", package = "pairwiseLLM")

  elo <- build_elo_data(example_writing_pairs)

  expect_s3_class(elo, "tbl_df")
  expect_identical(names(elo), c("winner", "loser"))

  # example_writing_pairs should have valid better_id for all rows
  expect_equal(nrow(elo), nrow(example_writing_pairs))

  # Winner/loser should always be one of the IDs in the original pair
  all_ids <- unique(c(example_writing_pairs$ID1, example_writing_pairs$ID2))
  expect_true(all(elo$winner %in% all_ids))
  expect_true(all(elo$loser %in% all_ids))
})

# -------------------------------------------------------------------------
# fit_elo_model tests
# -------------------------------------------------------------------------

test_that("fit_elo_model errors helpfully when EloChoice is not installed", {
  # Only run this test when EloChoice is NOT available
  skip_if(
    condition = requireNamespace("EloChoice", quietly = TRUE),
    message   = "EloChoice is installed; cannot test missing-package behaviour."
  )

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  expect_error(
    fit_elo_model(elo_data),
    "Package 'EloChoice' must be installed",
    fixed = TRUE
  )
})

test_that("fit_elo_model fits an Elo model when EloChoice is available", {
  skip_if_not_installed("EloChoice")

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  fit <- fit_elo_model(elo_data, runs = 3)

  # Engine label
  expect_equal(fit$engine, "EloChoice")

  # Elo table structure
  expect_s3_class(fit$elo, "tbl_df")
  expect_true(all(c("ID", "elo") %in% names(fit$elo)))

  # Number of rows should match number of unique IDs
  expect_equal(
    nrow(fit$elo),
    length(unique(c(elo_data$winner, elo_data$loser)))
  )

  # Types
  expect_type(fit$elo$ID, "character")
  expect_type(fit$elo$elo, "double")

  # Reliability indices: numeric scalars (may be NA if EloChoice returns no data)
  expect_true(is.numeric(fit$reliability))
  expect_equal(length(fit$reliability), 1L)

  expect_true(is.numeric(fit$reliability_weighted))
  expect_equal(length(fit$reliability_weighted), 1L)
})

test_that("fit_elo_model errors when elo_data is malformed", {
  skip_if_not_installed("EloChoice")

  # Missing winner/loser columns
  bad <- tibble::tibble(
    object1 = c("S1", "S2"),
    object2 = c("S2", "S3")
  )

  expect_error(
    fit_elo_model(bad),
    "`elo_data` must contain columns",
    fixed = TRUE
  )
})

test_that("fit_elo_model validates runs argument", {
  skip_if_not_installed("EloChoice")

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  expect_error(
    fit_elo_model(elo_data, runs = 0),
    "`runs` must be a single positive numeric value",
    fixed = FALSE
  )

  expect_error(
    fit_elo_model(elo_data, runs = -3),
    "`runs` must be a single positive numeric value",
    fixed = FALSE
  )

  expect_error(
    fit_elo_model(elo_data, runs = c(1, 2)),
    "`runs` must be a single positive numeric value",
    fixed = FALSE
  )
})
