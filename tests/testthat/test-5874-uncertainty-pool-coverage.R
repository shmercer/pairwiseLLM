testthat::test_that("select_uncertainty_anchors validates inputs", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(0.1, -0.1),
    theta_sd = c(0.2, 0.3)
  )

  testthat::expect_error(
    pairwiseLLM:::select_uncertainty_anchors(state, config = 1, theta_summary = theta_summary),
    "`config` must be a list"
  )
  testthat::expect_error(
    pairwiseLLM:::select_uncertainty_anchors(state, config = list()),
    "theta_summary"
  )
})

testthat::test_that("select_uncertainty_anchors errors when state N is invalid", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$N <- NA_integer_
  theta_summary <- tibble::tibble(
    item_id = state$ids,
    theta_mean = c(0.1, -0.1),
    theta_sd = c(0.2, 0.3)
  )

  ns <- asNamespace("pairwiseLLM")
  original_validate_state <- get("validate_state", envir = ns)
  base::unlockBinding("validate_state", ns)
  assign("validate_state", function(...) TRUE, envir = ns)
  base::lockBinding("validate_state", ns)
  on.exit({
    base::unlockBinding("validate_state", ns)
    assign("validate_state", original_validate_state, envir = ns)
    base::lockBinding("validate_state", ns)
  }, add = TRUE)

  testthat::expect_error(
    pairwiseLLM:::select_uncertainty_anchors(state, config = list(), theta_summary = theta_summary),
    "state\\$N"
  )
})

testthat::test_that("select_uncertainty_anchors returns empty when summary is empty", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  ns <- asNamespace("pairwiseLLM")
  original_summary <- get(".adaptive_v3_theta_summary", envir = ns)
  base::unlockBinding(".adaptive_v3_theta_summary", ns)
  assign(".adaptive_v3_theta_summary", function(...) {
    tibble::tibble(item_id = character(), theta_mean = double(), theta_sd = double(), deg = integer())
  }, envir = ns)
  base::lockBinding(".adaptive_v3_theta_summary", ns)
  on.exit({
    base::unlockBinding(".adaptive_v3_theta_summary", ns)
    assign(".adaptive_v3_theta_summary", original_summary, envir = ns)
    base::lockBinding(".adaptive_v3_theta_summary", ns)
  }, add = TRUE)

  out <- pairwiseLLM:::select_uncertainty_anchors(
    state,
    config = list(),
    theta_summary = tibble::tibble(item_id = character(), theta_mean = double(), theta_sd = double())
  )

  testthat::expect_identical(out, character())
})
