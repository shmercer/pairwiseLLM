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

test_that("fit_bt_model errors when bt_data does not have exactly three columns", {
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
