test_that("build_bt_data converts better_id into binary result", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1", "S2", "S3"),
    ID2       = c("S2", "S3", "S3", "S4"),
    better_id = c("S1", "S3", "S2", "S4")
  )

  bt <- build_bt_data(results)

  expect_s3_class(bt, "tbl_df")
  expect_identical(names(bt), c("object1", "object2", "result"))
  expect_equal(bt$result, c(1, 0, 1, 0))
})

test_that("build_bt_data drops rows with invalid or missing better_id", {
  results <- tibble::tibble(
    ID1       = c("S1", "S2", "S3", "S4"),
    ID2       = c("S2", "S3", "S4", "S1"),
    better_id = c("S1", "NOT_AN_ID", NA, "S1")
  )

  bt <- build_bt_data(results)

  # Only rows 1 and 4 are valid
  expect_equal(nrow(bt), 2)
  expect_equal(bt$object1, c("S1", "S4"))
  expect_equal(bt$object2, c("S2", "S1"))
  expect_equal(bt$result, c(1, 0))
})

test_that("build_bt_data errors when required columns are missing", {
  bad <- tibble::tibble(
    ID1 = c("S1", "S2"),
    ID2 = c("S3", "S4")
  )

  expect_error(
    build_bt_data(bad),
    "must contain columns"
  )
})

test_that("build_bt_data works on example_writing_pairs", {
  data("example_writing_pairs", package = "pairwiseLLM")

  bt <- build_bt_data(example_writing_pairs)

  expect_s3_class(bt, "tbl_df")
  expect_identical(names(bt), c("object1", "object2", "result"))
  expect_equal(nrow(bt), nrow(example_writing_pairs))
  expect_true(all(bt$result %in% c(0, 1)))
})

test_that("fit_bt_model fits a BT model using sirt when requested", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")

  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "sirt")

  # Engine label
  expect_equal(fit$engine, "sirt")

  # Theta table structure
  expect_s3_class(fit$theta, "tbl_df")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))

  # Number of rows should match number of unique IDs
  expect_equal(
    nrow(fit$theta),
    length(unique(c(bt$object1, bt$object2)))
  )

  # Types and values
  expect_type(fit$theta$theta, "double")
  expect_type(fit$theta$se, "double")
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(fit$theta$se >= 0))

  # Reliability should be numeric and finite
  expect_true(is.numeric(fit$reliability))
  expect_true(length(fit$reliability) == 1)
})

test_that("fit_bt_model fits a BT model using BradleyTerry2 when requested", {
  skip_if_not_installed("BradleyTerry2")

  data("example_writing_pairs", package = "pairwiseLLM")

  bt <- build_bt_data(example_writing_pairs)
  fit <- suppressWarnings(fit_bt_model(bt, engine = "BradleyTerry2"))

  # Engine label
  expect_equal(fit$engine, "BradleyTerry2")

  # Theta table structure
  expect_s3_class(fit$theta, "tbl_df")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))

  # Number of rows should match number of unique IDs
  expect_equal(
    nrow(fit$theta),
    length(unique(c(bt$object1, bt$object2)))
  )

  # Types and values
  expect_type(fit$theta$theta, "double")
  expect_type(fit$theta$se, "double")
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(fit$theta$se >= 0))

  # Reliability should be NA for BT2 engine
  expect_true(is.na(fit$reliability))
})

test_that("fit_bt_model with engine = 'auto' uses sirt when available", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")

  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "auto")

  expect_equal(fit$engine, "sirt")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))
})

test_that("fit_bt_model errors when bt_data does not have exactly three
          columns", {
  # Too many columns
  bad <- tibble::tibble(
    object1 = c("S1", "S2"),
    object2 = c("S2", "S3"),
    result  = c(1, 0),
    extra   = c(1, 1)
  )

  expect_error(
    fit_bt_model(bad),
    "exactly three columns"
  )

  # Too few columns
  bad2 <- tibble::tibble(
    object1 = c("S1", "S2"),
    object2 = c("S2", "S3")
  )

  expect_error(
    fit_bt_model(bad2),
    "exactly three columns"
  )
})

# Helper to safely mock internal package functions
mock_internal <- function(name, value, code) {
  ns <- asNamespace("pairwiseLLM")

  # Check existence
  if (!exists(name, envir = ns)) {
    rlang::abort(sprintf("Function '%s' not found in pairwiseLLM namespace.", name))
  }

  # Backup original
  orig <- get(name, envir = ns)

  # Restore on exit
  on.exit(
    {
      utils::assignInNamespace(name, orig, ns = ns)
    },
    add = TRUE
  )

  # Inject mock
  utils::assignInNamespace(name, value, ns = ns)

  # Run code
  force(code)
}

testthat::test_that("fit_bt_model validates input columns", {
  # bt_data must have 3 columns
  bad_data <- tibble::tibble(object1 = "A", object2 = "B") # 2 columns

  testthat::expect_error(
    fit_bt_model(bad_data),
    "must have exactly three columns"
  )

  bad_data2 <- tibble::tibble(A = 1, B = 2, C = 3, D = 4) # 4 columns
  testthat::expect_error(
    fit_bt_model(bad_data2),
    "must have exactly three columns"
  )
})

testthat::test_that("summarize_bt_fit produces correct ranks and handles decreasing argument", {
  # Construct a mock fit object that mimics the output of fit_bt_model
  # We ensure types are strictly numeric to avoid coercion warnings
  theta_df <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = as.numeric(c(1.5, -0.5, 2.0)),
    se = as.numeric(c(0.1, 0.2, 0.1))
  )

  fit <- list(
    engine = "mock",
    reliability = 0.85,
    theta = theta_df
  )

  # Case 1: decreasing = TRUE (default). Highest theta = Rank 1.
  # Values: C(2.0) > A(1.5) > B(-0.5)
  # Ranks:  C=1,   A=2,    B=3
  # Suppress warnings to avoid "NAs introduced by coercion" noise
  sum_dec <- suppressWarnings(summarize_bt_fit(fit, decreasing = TRUE))

  testthat::expect_s3_class(sum_dec, "tbl_df")
  testthat::expect_equal(sum_dec$rank[sum_dec$ID == "C"], 1)
  testthat::expect_equal(sum_dec$rank[sum_dec$ID == "A"], 2)
  testthat::expect_equal(sum_dec$rank[sum_dec$ID == "B"], 3)

  # Case 2: decreasing = FALSE. Lowest theta = Rank 1.
  # Values: B(-0.5) < A(1.5) < C(2.0)
  # Ranks:  B=1,    A=2,     C=3
  sum_inc <- suppressWarnings(summarize_bt_fit(fit, decreasing = FALSE))

  testthat::expect_equal(sum_inc$rank[sum_inc$ID == "B"], 1)
  testthat::expect_equal(sum_inc$rank[sum_inc$ID == "A"], 2)
  testthat::expect_equal(sum_inc$rank[sum_inc$ID == "C"], 3)

  # Check metadata preservation
  testthat::expect_equal(sum_dec$engine[1], "mock")
  testthat::expect_equal(sum_dec$reliability[1], 0.85)
})

testthat::test_that("summarize_bt_fit validates fit object structure", {
  # Missing theta
  bad_fit <- list(engine = "mock")
  testthat::expect_error(
    summarize_bt_fit(bad_fit),
    "must be a list returned by `fit_bt_model\\(\\)`"
  )

  # Malformed theta (missing required columns)
  bad_fit2 <- list(
    theta = tibble::tibble(ID = "A", val = 1) # missing 'theta', 'se'
  )
  testthat::expect_error(
    summarize_bt_fit(bad_fit2),
    "must contain columns"
  )
})

testthat::test_that("build_bt_data handles NAs and ties correctly", {
  # build_bt_data expects: ID1, ID2, better_id
  # It converts better_id to binary (1 if ID1, 0 if ID2).
  # Rows with NA or non-matching better_id should be dropped.

  df <- tibble::tibble(
    ID1 = c("A", "B", "C", "D"),
    ID2 = c("B", "C", "A", "E"),
    better_id = c("A", "C", NA, "Tie")
  )

  # Row 1: A vs B, better=A -> result 1
  # Row 2: B vs C, better=C -> result 0
  # Row 3: C vs A, better=NA -> dropped
  # Row 4: D vs E, better="Tie" -> dropped (doesn't match D or E)

  res <- build_bt_data(df)

  testthat::expect_equal(nrow(res), 2L)

  # Check Row 1
  testthat::expect_equal(res$object1[1], "A")
  testthat::expect_equal(res$object2[1], "B")
  testthat::expect_equal(res$result[1], 1)

  # Check Row 2
  testthat::expect_equal(res$object1[2], "B")
  testthat::expect_equal(res$object2[2], "C")
  testthat::expect_equal(res$result[2], 0)
})

testthat::test_that("build_bt_data validates input columns", {
  bad_df <- tibble::tibble(ID1 = "A", ID2 = "B") # Missing better_id
  testthat::expect_error(
    build_bt_data(bad_df),
    "must contain columns"
  )
})
