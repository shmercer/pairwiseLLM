# -------------------------------------------------------------------------
# build_bt_data tests
# -------------------------------------------------------------------------

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

test_that("build_bt_data handles NAs and ties correctly", {
  df <- tibble::tibble(
    ID1 = c("A", "B", "C", "D"),
    ID2 = c("B", "C", "A", "E"),
    better_id = c("A", "C", NA, "Tie")
  )

  res <- build_bt_data(df)

  expect_equal(nrow(res), 2L)
  expect_equal(res$object1[1], "A")
  expect_equal(res$object2[1], "B")
  expect_equal(res$result[1], 1)
  expect_equal(res$object1[2], "B")
  expect_equal(res$object2[2], "C")
  expect_equal(res$result[2], 0)
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

test_that("build_bt_data works when inputs are factors and returns character columns", {
  results <- tibble::tibble(
    ID1       = factor(c("S1", "S1", "S2")),
    ID2       = factor(c("S2", "S3", "S3")),
    better_id = factor(c("S1", "S3", "S2"))
  )

  bt <- build_bt_data(results)

  expect_s3_class(bt, "tbl_df")
  expect_identical(names(bt), c("object1", "object2", "result"))
  expect_equal(bt$result, c(1, 0, 1))
  expect_type(bt$object1, "character")
  expect_type(bt$object2, "character")
})

# -------------------------------------------------------------------------
# fit_bt_model tests
# -------------------------------------------------------------------------

test_that("fit_bt_model validates input columns", {
  bad_data <- tibble::tibble(object1 = "A", object2 = "B") # 2 columns

  expect_error(
    fit_bt_model(bad_data),
    "must have exactly three columns"
  )

  bad_data2 <- tibble::tibble(A = 1, B = 2, C = 3, D = 4) # 4 columns
  expect_error(
    fit_bt_model(bad_data2),
    "must have exactly three columns"
  )
})

test_that("fit_bt_model fits a BT model using sirt when requested (quiet)", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "sirt", verbose = FALSE)

  expect_equal(fit$engine, "sirt")
  expect_s3_class(fit$theta, "tbl_df")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))
  expect_equal(nrow(fit$theta), length(unique(c(bt$object1, bt$object2))))
  expect_type(fit$theta$theta, "double")
  expect_type(fit$theta$se, "double")
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(fit$theta$se >= 0))
  expect_true(is.numeric(fit$reliability))
  expect_true(length(fit$reliability) == 1)
})

test_that("fit_bt_model fits a BT model using BradleyTerry2 when requested (quiet)", {
  skip_if_not_installed("BradleyTerry2")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)

  expect_equal(fit$engine, "BradleyTerry2")
  expect_s3_class(fit$theta, "tbl_df")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))
  expect_equal(nrow(fit$theta), length(unique(c(bt$object1, bt$object2))))
  expect_type(fit$theta$theta, "double")
  expect_type(fit$theta$se, "double")
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(fit$theta$se >= 0))
  expect_true(is.na(fit$reliability))
})

test_that("fit_bt_model with engine = 'auto' uses sirt when available (quiet)", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)
  fit <- fit_bt_model(bt, engine = "auto", verbose = FALSE)

  expect_equal(fit$engine, "sirt")
  expect_true(all(c("ID", "theta", "se") %in% names(fit$theta)))
})

test_that("fit_bt_model(sirt, verbose = FALSE) runs without warnings", {
  skip_if_not_installed("sirt")
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  expect_no_warning(
    fit_bt_model(bt, engine = "sirt", verbose = FALSE)
  )
})

test_that("fit_bt_model(BradleyTerry2, verbose = FALSE) runs without warnings", {
  skip_if_not_installed("BradleyTerry2")
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  expect_no_warning(
    fit_bt_model(bt, engine = "BradleyTerry2", verbose = FALSE)
  )
})

test_that("fit_bt_model(auto) falls back to BradleyTerry2 when sirt is unavailable", {
  skip_if_not_installed("BradleyTerry2")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  # Save originals and restore no matter what happens in the test
  ns <- asNamespace("pairwiseLLM")
  orig_require <- get(".require_ns", envir = ns, inherits = FALSE)
  on.exit(assign(".require_ns", orig_require, envir = ns), add = TRUE)

  testthat::local_mocked_bindings(
    .require_ns = function(pkg, quietly = TRUE) {
      if (identical(pkg, "sirt")) {
        return(FALSE)
      }
      base::requireNamespace(pkg, quietly = quietly)
    },
    .env = ns
  )

  fit <- fit_bt_model(bt, engine = "auto", verbose = FALSE)
  expect_equal(fit$engine, "BradleyTerry2")
})

test_that("fit_bt_model(auto) falls back to BradleyTerry2 when sirt btm errors", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  # Save originals and restore no matter what happens in the test
  ns <- asNamespace("pairwiseLLM")
  orig_btm <- get(".sirt_btm", envir = ns, inherits = FALSE)
  on.exit(assign(".sirt_btm", orig_btm, envir = ns), add = TRUE)

  testthat::local_mocked_bindings(
    .sirt_btm = function(...) stop("forced sirt failure for testing", call. = FALSE),
    .env = ns
  )

  fit <- fit_bt_model(bt, engine = "auto", verbose = FALSE)
  expect_equal(fit$engine, "BradleyTerry2")
})

# -------------------------------------------------------------------------
# summarize_bt_fit tests
# -------------------------------------------------------------------------

test_that("summarize_bt_fit returns a tidy tibble with expected columns", {
  skip_if_not_installed("sirt")

  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  fit <- fit_bt_model(bt, engine = "sirt", verbose = FALSE)
  summary_tbl <- summarize_bt_fit(fit, verbose = FALSE)

  expect_s3_class(summary_tbl, "tbl_df")

  expected_cols <- c("ID", "theta", "se", "rank", "engine", "reliability")
  expect_true(all(expected_cols %in% names(summary_tbl)))

  expect_equal(nrow(summary_tbl), nrow(fit$theta))
  expect_setequal(summary_tbl$ID, fit$theta$ID)

  expect_type(summary_tbl$theta, "double")
  expect_type(summary_tbl$se, "double")
  expect_type(summary_tbl$rank, "integer")
  expect_type(summary_tbl$engine, "character")
  expect_type(summary_tbl$reliability, "double")

  expect_true(all(summary_tbl$engine == fit$engine))
  expect_true(all(summary_tbl$reliability == fit$reliability))

  finite_idx <- which(is.finite(summary_tbl$theta))
  finite_ranks <- summary_tbl$rank[finite_idx]
  expect_setequal(finite_ranks, seq_along(finite_ranks))
})

test_that("summarize_bt_fit(validates fit structure and handles named numeric + non-finite)", {
  # missing theta
  bad_fit1 <- list()
  expect_error(summarize_bt_fit(bad_fit1))

  # malformed theta
  bad_fit2 <- list(theta = tibble::tibble(ID = "A", theta = 0.1))
  expect_error(summarize_bt_fit(bad_fit2))

  # named numeric and non-finite cases
  theta_df <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = stats::setNames(c(1.5, -0.5, 2.0, NA_real_), c("A", "B", "C", "D")),
    se = as.numeric(c(0.1, 0.2, 0.1, 0.3))
  )

  fit <- list(engine = "mock", reliability = 0.85, theta = theta_df)

  expect_no_warning(summarize_bt_fit(fit, verbose = FALSE))

  out <- summarize_bt_fit(fit, verbose = FALSE)
  expect_true(is.na(out$rank[out$ID == "D"])) # NA theta
})
