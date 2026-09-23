test_that("rubric identity and item guards reject malformed vectors without coercion", {
  for (ids in list(NULL, logical(), matrix("a"), c(1, Inf), c(1, NA), c("a", "a"))) {
    expect_error(pairwiseLLM:::.rubric_ids(ids), "IDs")
  }
  expect_error(pairwiseLLM:::.rubric_trait(c("a", "b")), "single trait")
  for (trait in list(1, NA_character_, " ")) {
    expect_error(pairwiseLLM:::.rubric_trait(trait), "Trait identity")
  }
  for (theta in list(matrix(1), "1", c(1, 2), NaN)) {
    expect_error(pairwiseLLM:::.rubric_items("a", theta), "theta")
  }
  expect_error(pairwiseLLM:::.rubric_items("a", 1, global_ids = c("ga", "gb")), "align")
  expect_error(pairwiseLLM:::.rubric_items("a", 1, set_id = c(1, 2)), "Set IDs")
  expect_error(pairwiseLLM:::.rubric_items("a", 1, set_id = NA_integer_), "Set IDs")
  for (sd in list(matrix(1), "1", c(1, 2), Inf)) {
    expect_error(pairwiseLLM:::.rubric_items("a", 1, sd), "theta_sd")
  }
  expect_null(pairwiseLLM:::.rubric_draws(NULL, "a", "a"))
  for (draws in list(1, matrix(1, 1), matrix("1", 2))) {
    expect_error(pairwiseLLM:::.rubric_draws(draws, "a", "a"), "finite numeric matrix")
  }
  expect_error(pairwiseLLM:::.rubric_draws(matrix(1, 2), "a", "a"), "column names")
})

test_that("normalized metadata and linked draws cannot bypass the common contract", {
  validate <- pairwiseLLM:::.rubric_validate_cj
  good <- pairwiseLLM:::.rubric_normalize_cj(rubric_test_fixed(), "organization")
  for (bad in list(NULL, list(), good$items)) expect_error(validate(bad), "Invalid normalized")
  for (field in c("fit_contract", "diagnostics", "provenance")) {
    bad <- good
    bad[[field]] <- "missing"
    expect_error(validate(bad), "metadata")
  }
  bad <- good
  bad$scale_status <- "phase_b_linked"
  expect_error(validate(bad), "Phase A posterior draws")
  bad$posterior_draws <- NULL
  expect_identical(validate(bad), bad)
})

test_that("fixed completion validates the final fit, logs and authoritative uncertainty", {
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  good <- rubric_test_fixed()
  for (field in c("fits", "item_log_list", "round_log")) {
    bad <- good
    bad[[field]] <- list()
    expect_error(normalize(bad, "organization"), "completed.*result")
  }
  bad <- good
  bad$fits[[1L]] <- 1
  expect_error(normalize(bad, "organization"), "Bayesian fit contract")
  bad <- good
  bad$item_log_list[[1L]]$theta_sd <- NULL
  expect_error(normalize(bad, "organization"), "authoritative item summary")
  bad <- good
  bad$item_log_list[[1L]]$theta_sd[[1L]] <- 99
  expect_error(normalize(bad, "organization"), "uncertainty summary")
  bad <- good
  bad$item_log_list[[1L]]$ID[[1L]] <- "unknown"
  expect_error(normalize(bad, "organization"), "summary IDs")
  bad <- good
  bad$round_log$round_id <- 2L
  expect_error(normalize(bad, "organization"), "refit IDs")
})

test_that("adaptive wrappers and terminal logs cannot hide conflicting identity or absent fits", {
  withr::local_seed(2070L)
  normalize <- pairwiseLLM:::.rubric_normalize_cj
  state <- rubric_test_adaptive()
  expect_error(normalize(list(state = state, orientation = "lower_is_better"), "trait"), "orientation")
  expect_error(normalize(list(state = state, trait = "a"), "b"), "Trait mismatch")
  bad <- state
  bad$round_log <- bad$round_log[FALSE, ]
  expect_error(normalize(bad, "trait"), "recorded Bayesian refit")
  bad <- state
  bad$btl_fit <- NULL
  expect_error(normalize(bad, "trait"), "Bayesian fit")
  bad <- state
  bad$items$item_id[[1L]] <- "unknown"
  expect_error(normalize(bad, "trait"), "complete item domain")
  bad <- state
  bad$round_log$stop_decision[] <- FALSE
  expect_error(normalize(bad, "trait"), "passing recorded stop boundary")
})

test_that("Phase A context rejects malformed scalar set IDs with a deliberate condition", {
  withr::local_seed(2071L)
  artifact <- rubric_test_link_state(2L)$linking$phase_a$artifacts[["1"]]
  for (set_id in list(matrix(1L), 1e100, -.Machine$integer.max - 1, NA_real_, Inf, 1.5, c(1L, 2L))) {
    bad <- artifact
    bad$set_id <- set_id
    expect_no_warning(expect_error(pairwiseLLM:::.rubric_cj_phase_a(bad, "trait"), "single integer set_id"))
  }
  bad <- artifact
  bad$items$global_item_id <- NULL
  expect_error(pairwiseLLM:::.rubric_cj_phase_a(bad, "trait"), "source and global")
  for (hash in list(NULL, NA_character_, "", c("a", "b"), 1)) {
    bad <- artifact
    bad$fit_config_hash <- hash
    expect_error(pairwiseLLM:::.rubric_cj_phase_a(bad, "trait"), "fit_config_hash")
  }
  bad <- artifact
  bad$fit_model_id <- NULL
  expect_error(pairwiseLLM:::.rubric_cj_phase_a(bad, "trait"), "fit_model_id")
})

test_that("linked rubric normalization validates every result before exposing scores", {
  data <- rubric_linked_fixture(n_sets = 3L)
  reference <- pairwiseLLM:::.rubric_normalize_cj(data$reference)$reference
  normalize <- function(x) pairwiseLLM:::.rubric_cj_estimator(x, reference, "organization")
  out <- normalize(data$state)
  expect_identical(out$provenance$estimator_id, "fixed_shape_offset")
  expect_null(out$posterior_draws)
  expect_true(out$diagnostics$diagnostics_pass)
  bad <- data$state
  bad$linking$estimator$diagnostics_by_spoke[["2"]]$fit_valid <- FALSE
  expect_error(normalize(bad), "identity hash mismatch")
  bad <- data$state
  bad$linking$estimator$accepted_state_by_spoke[["2"]]$items$theta_link_sd <- NULL
  expect_error(normalize(bad), "identity hash mismatch")
  bad <- data$state
  bad$linking$estimator$accepted_state_by_spoke[["2"]]$provenance <- NULL
  expect_error(normalize(bad), "identity hash mismatch")
})
