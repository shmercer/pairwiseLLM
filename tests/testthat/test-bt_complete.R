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

test_that("build_bt_data includes a judge column when judge_col is provided", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1", "S2"),
    ID2       = c("S2", "S3", "S3"),
    better_id = c("S1", "S3", "S2"),
    model     = factor(c("gpt-4o", "gpt-4o", "o1"))
  )

  bt <- build_bt_data(results, judge_col = "model")

  expect_identical(names(bt), c("object1", "object2", "result", "judge"))
  expect_type(bt$judge, "character")
  expect_equal(bt$judge, c("gpt-4o", "gpt-4o", "o1"))
})

test_that("build_bt_data errors if judge_col is specified but missing", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1"),
    ID2       = c("S2", "S3"),
    better_id = c("S1", "S3")
  )

  expect_error(
    build_bt_data(results, judge_col = "model"),
    "`judge_col` must name a column in `results`\\."
  )
})

test_that("build_bt_data errors if judge_col is not a single character name", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1"),
    ID2       = c("S2", "S3"),
    better_id = c("S1", "S3"),
    model     = c("a", "b")
  )

  expect_error(
    build_bt_data(results, judge_col = 123),
    "`judge_col` must be a single character column name\\."
  )

  expect_error(
    build_bt_data(results, judge_col = c("model", "other")),
    "`judge_col` must be a single character column name\\."
  )
})

test_that("build_bt_data with judge_col still filters invalid better_id rows", {
  results <- tibble::tibble(
    ID1       = c("S1", "S1", "S2"),
    ID2       = c("S2", "S3", "S3"),
    better_id = c("S1", "NO_MATCH", "S2"),
    model     = c("gpt-4o", "gpt-4o", "o1")
  )

  bt <- build_bt_data(results, judge_col = "model")

  # Middle row is invalid (better_id doesn't match ID1 or ID2), so it should drop
  expect_equal(nrow(bt), 2L)
  expect_equal(bt$judge, c("gpt-4o", "o1"))
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

test_that("summarize_bt_fit handles character theta coercion and verbose=TRUE", {
  # Mock a fit object where theta is character strings
  theta_df <- tibble::tibble(
    ID = c("A", "B"),
    theta = c("1.5", "-0.5"),
    se = c(0.1, 0.1)
  )
  fit <- list(engine = "mock", reliability = 0.9, theta = theta_df)

  # Check coercion occurs during ranking.
  # We suppress warnings because coercion of character to double might trigger
  # "NAs introduced by coercion" warnings (depending on input/R version),
  # which is expected when we force verbose=TRUE.
  res <- suppressWarnings(summarize_bt_fit(fit, verbose = TRUE))

  # Verify ranks are calculated correctly based on the coerced numeric values (1.5 > -0.5)
  expect_equal(res$rank, c(1, 2))

  # Verify the ID column is preserved
  expect_equal(res$ID, c("A", "B"))
})

test_that("summarize_bt_fit respects decreasing=FALSE", {
  theta_df <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(10, 5, 1),
    se = 0.1
  )
  fit <- list(engine = "mock", theta = theta_df)

  # With decreasing=FALSE, lowest theta gets rank 1
  res <- summarize_bt_fit(fit, decreasing = FALSE, verbose = FALSE)
  expect_equal(res$rank, c(3, 2, 1))
})

test_that("summarize_bt_fit handles missing engine/reliability metadata", {
  # Fit object missing optional fields
  theta_df <- tibble::tibble(ID = "A", theta = 1, se = 0.1)
  fit <- list(theta = theta_df)

  res <- summarize_bt_fit(fit, verbose = FALSE)
  expect_true(is.na(res$engine[1]))
  expect_true(is.na(res$reliability[1]))
})

test_that("fit_bt_model errors if explicit engine is missing (sirt)", {
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  ns <- asNamespace("pairwiseLLM")
  testthat::local_mocked_bindings(
    .require_ns = function(...) FALSE,
    .env = ns
  )

  expect_error(
    fit_bt_model(bt, engine = "sirt"),
    "Package 'sirt' must be installed"
  )
})

test_that("fit_bt_model errors if explicit engine is missing (BradleyTerry2)", {
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  ns <- asNamespace("pairwiseLLM")
  testthat::local_mocked_bindings(
    .require_ns = function(...) FALSE,
    .env = ns
  )

  expect_error(
    fit_bt_model(bt, engine = "BradleyTerry2"),
    "Package 'BradleyTerry2' must be installed"
  )
})

test_that("fit_bt_model (sirt) errors on malformed output", {
  # We mock .require_ns to return TRUE so we reach the sirt execution block
  # even if sirt isn't installed locally.
  ns <- asNamespace("pairwiseLLM")

  # Prepare dummy data
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  # Case 1: missing effects
  testthat::local_mocked_bindings(
    .require_ns = function(...) TRUE,
    .sirt_btm = function(...) list(effects = NULL),
    .env = ns
  )
  expect_error(
    fit_bt_model(bt, engine = "sirt"),
    "missing `effects`"
  )

  # Case 2: malformed effects columns
  testthat::local_mocked_bindings(
    .require_ns = function(...) TRUE,
    .sirt_btm = function(...) list(effects = data.frame(wrong = 1)),
    .env = ns
  )
  expect_error(
    fit_bt_model(bt, engine = "sirt"),
    "does not contain expected columns"
  )
})

test_that("fit_bt_model reports errors from both engines when engine='auto' fails", {
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  ns <- asNamespace("pairwiseLLM")

  # Mock to ensure:
  # 1. sirt check passes, but execution fails
  # 2. BT2 check fails (simulating missing package or execution failure)
  testthat::local_mocked_bindings(
    .require_ns = function(pkg, ...) {
      if (pkg == "sirt") TRUE else FALSE
    },
    .sirt_btm = function(...) stop("Sirt crashed"),
    .env = ns
  )

  expect_error(
    fit_bt_model(bt, engine = "auto"),
    "Both sirt and BradleyTerry2 failed"
  )
})

test_that("fit_bt_model executes verbose branches", {
  # This test ensures the verbose=TRUE paths run without error.
  # We explicitly mock the engines to ensure stability and avoid external dependencies.

  ns <- asNamespace("pairwiseLLM")
  data("example_writing_pairs", package = "pairwiseLLM")
  bt <- build_bt_data(example_writing_pairs)

  # 1. Test sirt verbose branch
  testthat::with_mocked_bindings(
    .require_ns = function(...) TRUE,
    .sirt_btm = function(...) {
      # Return a minimal valid sirt object
      list(
        effects = data.frame(
          individual = c("A", "B"),
          theta = c(0, 0),
          se.theta = c(0, 0)
        ),
        mle.rel = 0.8
      )
    },
    .env = ns,
    {
      expect_no_error(fit_bt_model(bt, engine = "sirt", verbose = TRUE))
    }
  )

  # 2. Test BradleyTerry2 verbose branch
  # We skip this if BT2 isn't installed because we can't easily mock the
  # direct :: calls to BradleyTerry2 functions without 'mockery'.
  skip_if_not_installed("BradleyTerry2")

  # We verify the function attempts to run. We assume valid data will pass.
  # We explicitly ensure .require_ns returns TRUE to avoid leakage from previous tests.
  testthat::with_mocked_bindings(
    .require_ns = function(...) TRUE,
    .env = ns,
    {
      expect_no_error(fit_bt_model(bt, engine = "BradleyTerry2", verbose = TRUE))
    }
  )
})
