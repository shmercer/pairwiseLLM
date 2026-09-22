test_that("oriented prediction honors positional bias and lapse", {
  j <- link_contract_input()$judge
  p <- pairwiseLLM:::.link_probability(c(-1, 2), c(.5, -3), j)
  expect_equal(p, .9 * plogis(c(-1.5, 5) + .2) + .05)
  reverse <- pairwiseLLM:::.link_probability(c(.5, -3), c(-1, 2), j)
  expect_false(isTRUE(all.equal(reverse, 1 - p)))
  j$beta <- 0
  p <- pairwiseLLM:::.link_probability(c(-1, 2), c(.5, -3), j)
  expect_equal(pairwiseLLM:::.link_probability(c(.5, -3), c(-1, 2), j), 1 - p)
  j$epsilon <- 1
  expect_identical(pairwiseLLM:::.link_probability(c(-1e300, 1e300), c(0, 0), j), c(.5, .5))
  expect_error(pairwiseLLM:::.link_probability(1, c(1, 2), j), "equally sized")
})

test_that("prediction dispatch uses serialized state and validates probabilities", {
  result <- link_contract_result(link_contract_input(edges = 1L))
  pairs <- result$continuation$input$cross[, -6L]
  expect_error(predict_link(result, pairs), class = "pairwiseLLM_link_not_implemented")
  resolver <- pairwiseLLM:::.link_resolve
  local_mocked_bindings(.link_resolve = function(id) {
    backend <- resolver(id)
    backend$predict <- function(state, pairs, input) {
      expect_identical(state$theta, result$items$theta_link_mean)
      rep(.8, nrow(pairs))
    }
    backend
  }, .package = "pairwiseLLM")
  expect_identical(predict_link(result, pairs), .8)
  expect_identical(predict_link(result, pairs[FALSE, ]), numeric())
  bad <- pairs
  bad$A_set <- "S"
  expect_error(predict_link(result, bad), "illegal")
  expect_error(predict_link(link_contract_result(valid = FALSE), pairs), "invalid linking fit")
  expect_error(predict_link(result, result$continuation$input$cross), "incompatible fields")
})

test_that("prediction dispatch cannot silently accept invalid engine output", {
  result <- link_contract_result(link_contract_input(edges = 1L))
  pairs <- result$continuation$input$cross[, -6L]
  resolver <- pairwiseLLM:::.link_resolve
  local_mocked_bindings(.link_resolve = function(id) {
    backend <- resolver(id)
    backend$predict <- function(...) 1.1
    backend
  }, .package = "pairwiseLLM")
  expect_error(predict_link(result, pairs), "invalid prediction probabilities")
})
