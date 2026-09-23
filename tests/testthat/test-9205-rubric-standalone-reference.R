test_that("one verified standalone reference transports calibration through E1--E3", {
  skip_if_not_installed("ordinal")
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  evidence <- rubric_reference_evidence()
  reference <- rubric_reference_prepare(rubric_reference_completed(evidence), evidence)
  calibration <- fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors")
  before <- serialize(calibration, NULL)
  hashes <- character()
  for (estimator in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    args <- rubric_reference_link_args(reference, estimator)
    input <- do.call(prepare_link_input, args)
    result <- fit_link(input)
    expect_true(result$diagnostics$fit_valid)
    prediction <- predict(calibration, result)
    expect_equal(nrow(prediction), 3L)
    expect_true(all(is.finite(prediction$expected_level)))
    target <- result$items[result$items$set_id == "S", ]
    expect_equal(prediction$theta, target$theta_link_mean + mean(reference$items$theta))
    expect_identical(prediction$theta_sd, target$theta_link_sd)
    session <- start_link_session(input)
    session_prediction <- predict(calibration, session)
    expect_identical(session_prediction$probabilities, prediction$probabilities)
    expect_identical(session_prediction,
      predict(calibration, session$linking$estimator$accepted_state_by_spoke$S))
    hashes <- c(hashes, result$provenance$phase_a_sources$hub$reference_hash)
    expect_identical(summary(session)$phase_a_hub_reference_hash, reference$reference_hash)
    # Copying the reference source onto changed payloads is insufficient.
    bad <- args
    kind <- input$phase_a$hub$kind
    if (kind == "observations") {
      bad$phase_a$hub[[kind]]$y_A[1] <- 1L - bad$phase_a$hub[[kind]]$y_A[1]
    } else {
      bad$phase_a$hub[[kind]][1] <- bad$phase_a$hub[[kind]][1] + .1
    }
    expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "hub payload")
    bad <- args
    bad$phase_a$hub$source$reference_hash <- "other-reference"
    expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "reference hub")
    bad <- args
    bad$hub$items$global_item_id[1] <- "other-item"
    expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "reference hub")
    bad <- args
    bad$cross <- bad$cross[FALSE, ]
    expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "cross-set identified")
    # A newly fitted changed outcome yields a different valid reference.
    changed <- evidence
    changed$better_id[1] <- changed$B_id[1]
    changed$winner_pos[1] <- 2L
    other <- rubric_reference_prepare(rubric_reference_completed(changed), changed)
    bad <- rubric_reference_link_args(other, estimator)
    expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "reference hub")
  }
  expect_identical(unique(hashes), reference$reference_hash)
  expect_identical(serialize(calibration, NULL), before)
  expect_error(predict(calibration, reference), "inappropriate")
})

test_that("standalone transport rejects incompatible contracts and missing target identities", {
  skip_if_not_installed("ordinal")
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  reference <- rubric_reference_prepare(rubric_reference_completed())
  calibration <- fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors")
  args <- rubric_reference_link_args(reference, "fixed_shape_offset")
  bad <- args
  bad$judge$model_variant <- "btl_b"
  bad$judge$beta <- .2
  expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "judge settings")
  bad <- args
  bad$spoke$items$global_item_id <- NULL
  expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "global item IDs")
  bad <- args
  bad$phase_a$spoke$source <- list(trait = "mechanics")
  expect_error(predict(calibration, fit_link(do.call(prepare_link_input, bad))), "Trait mismatch")
  expect_error(fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors", trait = "mechanics"), "Trait mismatch")
})

test_that("standalone reference, calibration and linked results survive RDS without live fits", {
  skip_if_not_installed("ordinal")
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  reference <- rubric_reference_prepare(rubric_reference_completed())
  calibration <- fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors")
  result <- fit_link(do.call(prepare_link_input, rubric_reference_link_args(reference, "joint_offset")))
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  bundle <- list(reference = reference, calibration = calibration, result = result)
  saveRDS(bundle, path)
  restored <- readRDS(path)
  expect_identical(restored, bundle)
  expect_identical(predict(restored$calibration, restored$result), predict(calibration, result))
  expect_true(.link_data_only(unclass(reference)))
})

test_that("E3-MCMC transport uses the same verified reference identity", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("posterior")
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  reference <- rubric_reference_prepare(rubric_reference_completed())
  calibration <- fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors")
  args <- rubric_reference_link_args(reference, "joint_offset")
  args$control <- list(estimator = list(engine = "mcmc"))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) sampled, .package = "pairwiseLLM")
  result <- fit_link(input)
  expect_true(result$diagnostics$fit_valid)
  expect_equal(nrow(predict(calibration, result)), 3L)
})

test_that("equivalent adaptive and standalone metrics give equivalent calibration", {
  skip_if_not_installed("ordinal")
  data <- rubric_linked_fixture()
  a <- data$reference
  rows <- .rubric_phase_a_validate(a)$evidence
  evidence <- build_btl_results_data(data.frame(ID1 = rows$A_item, ID2 = rows$B_item,
    better_id = ifelse(rows$y_A == 1L, rows$A_item, rows$B_item)))
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = function(bt_data, config, seed = NULL) {
    index <- match(bt_data$item_id, a$items$item_id)
    offsets <- c(-1, 1) * .2 / sqrt(2)
    draws <- outer(offsets, a$items$theta_raw_mean[index], `+`)
    colnames(draws) <- bt_data$item_id
    list(draws = list(theta = draws), model_variant = "btl",
      diagnostics = list(divergences = 0L, max_rhat = 1, min_ess_bulk = 1000),
      mcmc_config_used = list(chains = 2L))
  }, .package = "pairwiseLLM")
  cj <- fit_bayes_btl_mcmc(evidence, a$items$item_id, model_variant = "btl",
    cmdstan = list(chains = 2L, parallel_chains = 1L))
  reference <- prepare_linked_rubric_reference(cj, evidence, a$set_id,
    items = a$items[c("item_id", "global_item_id")], trait = "organization")
  adaptive <- rubric_linked_fit(data)
  standalone <- fit_rubric_calibration(reference, data$rubric, calibration_design = "linked_anchors",
    levels = data$levels)
  expect_equal(standalone$calibration_data, adaptive$calibration_data, tolerance = 1e-12)
  expect_equal(standalone$cj$items$theta_sd, adaptive$cj$items$theta_sd, tolerance = 1e-12)
  expect_equal(predict(standalone)$probabilities, predict(adaptive)$probabilities, tolerance = 1e-10)
})

test_that("legacy standalone fits retain same-set calibration and failed links remain rejected", {
  skip_if_not_installed("ordinal")
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  cj <- rubric_reference_completed()
  reference <- rubric_reference_prepare(cj)
  labels <- rubric_reference_labels(reference)
  legacy <- cj
  legacy$fits[[1]]$evidence_identity <- NULL
  legacy$fits[[1]]$reference_fit_config <- NULL
  old <- fit_rubric_calibration(legacy, labels, trait = "organization")
  current <- fit_rubric_calibration(cj, labels, trait = "organization")
  expect_identical(predict(old)$probabilities, predict(current)$probabilities)
  calibration <- fit_rubric_calibration(reference, labels, calibration_design = "linked_anchors")
  args <- rubric_reference_link_args(reference, "joint_offset")
  args$control <- list(estimator = list(maxit = 1L, gradient_tol = 1e-20))
  result <- fit_link(do.call(prepare_link_input, args))
  expect_false(result$diagnostics$fit_valid)
  expect_error(predict(calibration, result), "valid, cross-set identified")
})
