test_that("fit_bt_bayes_rstanarm handles missing optional dependency", {
  pairs <- tibble::tibble(ID1 = "A", ID2 = "B", winner = "ID1")
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))

  testthat::local_mocked_bindings(
    .require_ns = function(...) FALSE,
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM:::.fit_bt_bayes_rstanarm(pairs, ids = c("A", "B"))
  expect_false(out$available)
})

test_that("fit_bt_bayes_rstanarm exercises internal wrappers", {
  bt_data <- tibble::tibble(object1 = "A", object2 = "B", result = 1L)
  ids <- c("A", "B")

  fake_fit <- matrix(0, nrow = 1, ncol = 1, dimnames = list(NULL, "object1B"))
  testthat::local_mocked_bindings(
    .require_ns = function(...) TRUE,
    .get_exported_value = function(ns, name) {
      if (name == "stan_glm") {
        return(function(...) fake_fit)
      }
      if (name == "normal") {
        return(function(...) list())
      }
      stop("unexpected")
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM:::.fit_bt_bayes_rstanarm(bt_data, ids = ids)
  expect_true(out$available)
  expect_true(is.data.frame(out$theta))
})
