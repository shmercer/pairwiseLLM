# -------------------------------------------------------------------------
# build_elo_data tests
# -------------------------------------------------------------------------

build_elo_data <- pairwiseLLM:::build_elo_data
fit_elo_model <- pairwiseLLM:::fit_elo_model

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
    "must contain either",
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

test_that("build_elo_data accepts canonical A_id/B_id schema", {
  data("example_writing_results", package = "pairwiseLLM")

  elo <- build_elo_data(example_writing_results)

  expect_s3_class(elo, "tbl_df")
  expect_identical(names(elo), c("winner", "loser"))
  expect_equal(nrow(elo), nrow(example_writing_results))
})

# -------------------------------------------------------------------------
# fit_elo_model tests
# -------------------------------------------------------------------------

test_that("fit_elo_model errors helpfully when EloChoice is not installed", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("withr")

  # Force the missing-package branch regardless of local installation.
  withr::local_package("pairwiseLLM")
  mockery::stub(fit_elo_model, "requireNamespace", function(...) FALSE)

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

  # Reliability indices: numeric scalars (may be NA if EloChoice
  # returns no data)
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

test_that("fit_elo_model supports verbose = FALSE (quiet mode)", {
  skip_if_not_installed("EloChoice")

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  # Should still fit and return expected structure
  fit <- fit_elo_model(elo_data, runs = 3, verbose = FALSE)

  expect_equal(fit$engine, "EloChoice")
  expect_s3_class(fit$elo, "tbl_df")
  expect_true(all(c("ID", "elo") %in% names(fit$elo)))
})

test_that("fit_elo_model rejects non-numeric or NA runs", {
  skip_if_not_installed("EloChoice")

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  expect_error(
    fit_elo_model(elo_data, runs = NA),
    "`runs` must be a single positive numeric value",
    fixed = FALSE
  )

  expect_error(
    fit_elo_model(elo_data, runs = "3"),
    "`runs` must be a single positive numeric value",
    fixed = FALSE
  )
})

test_that("build_elo_data works when inputs are factors", {
  results <- tibble::tibble(
    ID1       = factor(c("S1", "S2")),
    ID2       = factor(c("S2", "S3")),
    better_id = factor(c("S1", "S3"))
  )

  elo <- build_elo_data(results)

  expect_s3_class(elo, "tbl_df")
  expect_identical(names(elo), c("winner", "loser"))
  expect_equal(elo$winner, c("S1", "S3"))
  expect_equal(elo$loser, c("S2", "S2"))
})

test_that("build_elo_data returns character winner/loser even if inputs are factors", {
  results <- tibble::tibble(
    ID1       = factor(c("S1")),
    ID2       = factor(c("S2")),
    better_id = factor(c("S2"))
  )

  elo <- build_elo_data(results)

  expect_type(elo$winner, "character")
  expect_type(elo$loser, "character")
})

test_that("fit_elo_model errors if EloChoice output lacks a valid ratmat matrix", {
  skip_if_not_installed("EloChoice")
  skip_if_not_installed("mockery")
  skip_if_not_installed("withr")

  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  # Stub EloChoice::elochoice() to return an object with invalid ratmat
  withr::local_package("pairwiseLLM")

  mockery::stub(
    where = fit_elo_model,
    what  = "EloChoice::elochoice",
    how   = list(ratmat = NULL) # invalid ratmat
  )

  expect_error(
    fit_elo_model(elo_data, runs = 3),
    "does not contain a valid `ratmat` matrix",
    fixed = TRUE
  )
})

test_that("build_elo_data returns character winner/loser", {
  results <- tibble::tibble(
    ID1       = factor(c("S1")),
    ID2       = factor(c("S2")),
    better_id = factor(c("S2"))
  )

  elo <- build_elo_data(results)
  expect_type(elo$winner, "character")
  expect_type(elo$loser, "character")
})

test_that("build_elo_data handles standard data.frame input", {
  # Ensure it works with base data.frame (not just tibble) and factors
  df <- data.frame(
    ID1 = factor(c("A", "B")),
    ID2 = factor(c("B", "C")),
    better_id = factor(c("A", "C"))
  )

  out <- build_elo_data(df)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$winner, c("A", "C"))
  expect_equal(out$loser, c("B", "B"))
})

test_that("fit_elo_model executes verbose branch", {
  skip_if_not_installed("EloChoice")
  data("example_writing_pairs", package = "pairwiseLLM")
  elo_data <- build_elo_data(example_writing_pairs)

  # Just verify it runs without error when verbose = TRUE
  # (capture_output would be needed to verify printing, but execution coverage is the goal)
  expect_error(fit_elo_model(elo_data, runs = 1, verbose = TRUE), NA)
})

test_that("fit_elo_model handles missing column names in ratmat (IDs fallback)", {
  skip_if_not_installed("EloChoice")
  skip_if_not_installed("mockery")

  elo_data <- tibble::tibble(winner = "A", loser = "B")

  # Create a matrix with no column names
  fake_mat <- matrix(c(1000, 1000), ncol = 2)
  mock_fit <- list(ratmat = fake_mat)

  # Stub elochoice to return this unnamed matrix
  mockery::stub(fit_elo_model, "EloChoice::elochoice", mock_fit)
  # Stub reliability to avoid errors there
  mockery::stub(fit_elo_model, "EloChoice::reliability", NULL)

  # Force verbose=TRUE to avoid capture.output complications with mocks
  res <- fit_elo_model(elo_data, runs = 1, verbose = TRUE)

  # IDs should default to "1", "2"
  expect_equal(res$elo$ID, c("1", "2"))
  expect_equal(res$elo$elo, c(1000, 1000))
})

test_that("fit_elo_model errors if ratmat is invalid type", {
  skip_if_not_installed("EloChoice")
  skip_if_not_installed("mockery")

  elo_data <- tibble::tibble(winner = "A", loser = "B")

  # Stub return with invalid ratmat (not a matrix)
  mock_fit <- list(ratmat = "invalid")
  mockery::stub(fit_elo_model, "EloChoice::elochoice", mock_fit)

  expect_error(
    fit_elo_model(elo_data, runs = 1, verbose = TRUE),
    "does not contain a valid `ratmat` matrix"
  )
})

test_that("fit_elo_model reliability calculation uses fallbacks", {
  skip_if_not_installed("EloChoice")
  skip_if_not_installed("mockery")

  elo_data <- tibble::tibble(winner = "A", loser = "B")
  # Valid ratmat to pass that check
  mock_fit <- list(ratmat = matrix(1000, ncol = 2, dimnames = list(NULL, c("A", "B"))))

  mockery::stub(fit_elo_model, "EloChoice::elochoice", mock_fit)

  # Case 1: Reliability DF has numeric columns but NO 'upset'/'upset.wgt' names
  # Should fallback to 1st numeric for unweighted, 2nd for weighted
  fake_rel_fallback <- data.frame(garbage = "txt", val1 = 0.1, val2 = 0.2)

  mockery::stub(fit_elo_model, "EloChoice::reliability", fake_rel_fallback)
  res1 <- fit_elo_model(elo_data, runs = 1, verbose = TRUE)

  expect_equal(res1$reliability, 0.1)
  expect_equal(res1$reliability_weighted, 0.2)

  # Case 2: Reliability is empty/NULL
  mockery::stub(fit_elo_model, "EloChoice::reliability", NULL)
  res2 <- fit_elo_model(elo_data, runs = 1, verbose = TRUE)

  expect_true(is.na(res2$reliability))
  expect_true(is.na(res2$reliability_weighted))

  # Case 3: Reliability has only 1 numeric col (covers 'sum(num_cols) >= 2L' check)
  fake_rel_single <- data.frame(val1 = 0.5)
  mockery::stub(fit_elo_model, "EloChoice::reliability", fake_rel_single)
  res3 <- fit_elo_model(elo_data, runs = 1, verbose = TRUE)

  expect_equal(res3$reliability, 0.5)
  expect_true(is.na(res3$reliability_weighted))
})
