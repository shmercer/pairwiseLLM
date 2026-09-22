test_that("fit dispatcher requires implementations without a legacy fallback", {
  expect_true(fit_link(link_contract_input("joint_offset"))$diagnostics$fit_valid)
  expect_true(fit_link(link_contract_input())$diagnostics$fit_valid)
  expect_true(fit_link(link_contract_input("gaussian_posterior_bridge"))$diagnostics$fit_valid)
  expect_error(fit_link(list()), "input schema")
})

test_that("continuation passes only a numerical mode to the current likelihood", {
  old <- link_contract_result(link_contract_input(edges = 1L))
  input <- link_contract_input(edges = 2L)
  expect_identical(pairwiseLLM:::.link_previous_mode(input, old), c(delta = 0))
  expect_null(pairwiseLLM:::.link_previous_mode(input, NULL))
  expect_null(pairwiseLLM:::.link_previous_mode(input, link_contract_result(link_contract_input(edges = 1L), valid = FALSE)))
  resolver <- pairwiseLLM:::.link_resolve
  local_mocked_bindings(.link_resolve = function(id) {
    backend <- resolver(id)
    backend$fit <- function(input, initial) {
      expect_identical(initial, c(delta = 0))
      expect_identical(nrow(input$cross), 2L)
      link_contract_result(input)
    }
    backend
  }, .package = "pairwiseLLM")
  result <- fit_link(input, old)
  expect_identical(result$provenance$counts$cross, 2L)
  expect_identical(result$continuation$input, input)
})

test_that("continuation rejects changed old evidence, priors, and identities", {
  old <- link_contract_result(link_contract_input(edges = 1L))
  args <- link_contract_args(edges = 2L)
  args$cross$y_A[1] <- 0L
  expect_error(fit_link(do.call(prepare_link_input, args), old), "unchanged old evidence prefix")
  expect_error(fit_link(link_contract_input(), old), "unchanged old evidence prefix")
  args <- link_contract_args(edges = 2L)
  args$judge$beta <- .3
  expect_error(fit_link(do.call(prepare_link_input, args), old), "frozen judge")
  args <- link_contract_args(edges = 2L)
  args$phase_a$hub$points <- c(a = -3, b = 3)
  expect_error(fit_link(do.call(prepare_link_input, args), old), "frozen phase_a")
  args <- link_contract_args(edges = 2L)
  args$control <- list(delta_prior = list(mean = 0, sd = 4))
  expect_error(fit_link(do.call(prepare_link_input, args), old), "offset prior")
  expect_error(fit_link(link_contract_input("joint_offset", 2L), old), "frozen estimator")
  expect_error(fit_link(link_contract_input(edges = 2L), list(anchored_joint = TRUE)), "legacy Phase B")
})

test_that("fit results cannot substitute another budget checkpoint", {
  input <- link_contract_input(edges = 2L)
  other <- link_contract_result(link_contract_input(edges = 1L))
  resolver <- pairwiseLLM:::.link_resolve
  local_mocked_bindings(.link_resolve = function(id) {
    backend <- resolver(id)
    backend$fit <- function(...) other
    backend
  }, .package = "pairwiseLLM")
  expect_error(fit_link(input), "different evidence")
})

test_that("continuation modes bind coordinate order and prediction state contains only data", {
  result <- link_contract_result(link_contract_input("joint_offset", 1L))
  result$continuation$mode <- result$continuation$mode[3:1]
  expect_error(pairwiseLLM:::.link_validate_result(result), "canonical free-coordinate order")
  result <- link_contract_result()
  attr(result$prediction$state$theta, "callback") <- function() 1
  expect_error(pairwiseLLM:::.link_validate_result(result), "nonserializable state")
})
