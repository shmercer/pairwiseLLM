test_that("both linked methods consume real one- and multi-spoke fits across all BTL variants", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(9200)
  before_rng <- .Random.seed
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    for (n_sets in 2:3) {
      data <- rubric_linked_fixture(n_sets, variant)
      original <- serialize(data$state, NULL)
      linked <- pairwiseLLM:::.rubric_normalize_cj(data$state)
      for (method in c("ordinal_linear", "ordinal_monotone")) {
        fit <- rubric_linked_fit(data, method)
        pred <- stats::predict(fit, data$state)
        expect_identical(stats::predict(fit, list(state = data$state)), pred)
        expect_identical(fit$cj$model_variant, variant)
        expect_identical(fit$reference, fit$cj$reference)
        expect_identical(fit$transformation,
          list(center = mean(data$reference$items$theta_raw_mean),
            scale = stats::sd(data$reference$items$theta_raw_mean)))
        target <- linked$items[linked$items$set_id != 1L, ]
        expect_identical(pred$theta, target$theta)
        expect_identical(pred$theta_sd, target$theta_sd)
        expect_identical(pred$item_id, target$item_id)
        expect_identical(pred$set_id, target$set_id)
        expect_identical(pred$source_item_id, target$source_item_id)
        expect_identical(pred$global_item_id, pred$item_id)
        expect_false(any(pred$item_id %in% data$rubric$item_id))
        z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
        direct <- if (method == "ordinal_linear") {
          stats::predict(fit$backend$model, data.frame(z = z), type = "prob")$fit
        } else {
          mgcv::predict.gam(fit$backend$model, data.frame(z = z), type = "response")
        }
        p <- do.call(rbind, pred$probabilities)
        expect_equal(unname(p), unname(direct), tolerance = 1e-12)
        expect_equal(rowSums(p), rep(1, nrow(p)), tolerance = 1e-12)
        expect_true(all(is.finite(p) & p >= 0 & p <= 1))
        expect_identical(colnames(p), data$levels)
        expect_identical(pred$category, pred$median_category)
        expect_identical(pred$rubric_score, data$levels[pred$category])
        expect_equal(pred$expected_level, as.vector(p %*% 1:3))
        mode <- stats::predict(fit, data$state, hard_score = "mode")
        expect_identical(mode$category, mode$modal_category)
        expect_identical(mode$probabilities, pred$probabilities)
        expect_identical(pred$extrapolated, pred$theta < -2 | pred$theta > 2)
        expect_true(any(pred$extrapolated))
        expect_identical(attr(pred, "linking"),
          list(reference = linked$reference, fit_contract = linked$fit_contract,
            provenance = linked$provenance, diagnostics = linked$diagnostics, reliability = linked$reliability))
        expect_identical(attr(pred, "linking")$provenance$estimation_method, "map_laplace")
        expect_identical(serialize(data$state, NULL), original)
        reference_pred <- stats::predict(fit)
        expect_identical(reference_pred$item_id, data$rubric$item_id)
        expect_null(attr(reference_pred, "linking"))
        expect_false("set_id" %in% names(reference_pred))
      }
    }
  }
  expect_identical(.Random.seed, before_rng)
})

test_that("linked fitting uses labeled reference rows and never target cohort standardization", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(9201)
  data <- rubric_linked_fixture()
  keep <- data$reference$items$theta_raw_mean <= 1.5
  data$rubric <- data$rubric[keep, ]
  shifted <- rubric_linked_fixture(target_shift = 2)
  for (method in c("ordinal_linear", "ordinal_monotone")) {
    fit <- rubric_linked_fit(data, method)
    expect_identical(fit$calibration_range, c(-2, 1.5))
    expect_identical(fit$transformation, list(center = -0.25,
      scale = stats::sd(data$reference$items$theta_raw_mean[keep])))
    immutable <- c("reference", "transformation", "calibration_range", "calibration_data", "category_counts", "cj")
    original <- serialize(fit[immutable], NULL)
    for (state in list(data$state, shifted$state)) {
      pred <- stats::predict(fit, state)
      z <- (pred$theta - fit$transformation$center) / fit$transformation$scale
      expect_false(isTRUE(all.equal(z, as.double(scale(pred$theta)))))
      direct <- if (method == "ordinal_linear") {
        stats::predict(fit$backend$model, data.frame(z = z), type = "prob")$fit
      } else {
        mgcv::predict.gam(fit$backend$model, data.frame(z = z), type = "response")
      }
      expect_equal(unname(do.call(rbind, pred$probabilities)), unname(direct), tolerance = 1e-12)
      expect_identical(pred$extrapolated, pred$theta < -2 | pred$theta > 1.5)
    }
    expect_identical(serialize(fit[immutable], NULL), original)
    expect_identical(stats::predict(fit)$extrapolated, !keep)
  }
})

test_that("linked predictions reject independent scales and unaccepted or malformed Phase B inputs", {
  skip_if_not_installed("ordinal")
  withr::local_seed(9202)
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  for (input in list(data$reference, data$state$linking$phase_a$artifacts[["2"]], rubric_test_fixed())) {
    expect_error(stats::predict(fit, input), "inappropriate")
  }
  expect_error(stats::predict(fit, data.frame(theta = 1)), "completed CJ")
  expect_error(stats::predict(fit, pairwiseLLM:::.rubric_normalize_cj(data$state)), "Unsupported CJ")
  bad <- data$state
  bad$meta$stop_decision <- FALSE
  expect_error(stats::predict(fit, bad), "incomplete")
  bad <- data$state
  bad$linking$anchored_joint$accepted_state_by_spoke[["2"]]$anchored_joint_init_state_method <- "artifact_copy_init"
  expect_error(stats::predict(fit, bad), "initialization")
  bad <- data$state
  bad$step_log$is_probe_step[] <- TRUE
  expect_error(stats::predict(fit, bad), "active hub-spoke evidence")
  bad <- data$state
  bad$item_log[[1L]]$theta_link_eap[[55L]] <- 100
  expect_error(stats::predict(fit, bad), "linked item summary")
  bad <- data$state
  bad$controller$link_refit_stats_by_spoke[["2"]]$fit_contract$uncertainty_approximation <- "mcmc"
  expect_error(stats::predict(fit, bad), "uncertainty_approximation")
  multi <- rubric_linked_fixture(3L)$state
  multi$linking$anchored_joint$accepted_state_by_spoke[["3"]]$anchored_joint_init_state_method <- "artifact_copy_init"
  expect_error(stats::predict(fit, multi), "initialization")
  expect_error(pairwiseLLM::fit_rubric_calibration(data$reference, method = "percentile",
    calibration_design = "linked_anchors", K = 3), "does not support")
  expect_error(pairwiseLLM::fit_rubric_calibration(data$state, data$rubric,
    calibration_design = "linked_anchors"), "inappropriate")
})

test_that("reference identity checks reject changed valid hubs and incompatible contracts", {
  skip_if_not_installed("ordinal")
  withr::local_seed(9203)
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  # This is another internally coherent accepted Phase B result: same IDs and
  # configuration, but a different hub metric. Hash equality is insufficient.
  other <- rubric_linked_fixture(reference_shift = 0.25)
  expect_identical(other$reference$fit_config_hash, fit$reference$fit_contract_hash)
  expect_no_warning(pairwiseLLM:::.rubric_normalize_cj(other$state))
  expect_error(stats::predict(fit, other$state), "stored rubric reference hub")
  other_variant <- rubric_linked_fixture(variant = "btl_b")
  expect_no_warning(pairwiseLLM:::.rubric_normalize_cj(other_variant$state))
  expect_error(stats::predict(fit, other_variant$state), "stored rubric reference hub")
  bad <- data$state
  bad$linking$phase_a$artifacts[["1"]]$items$theta_raw_sd[[1L]] <- 0.21
  expect_error(stats::predict(fit, bad), "stored rubric reference hub")
  bad <- data$state
  bad$linking$phase_a$artifacts[["1"]]$items$global_item_id[[1L]] <- "another_reference_item"
  bad$items$global_item_id[[1L]] <- "another_reference_item"
  expect_error(stats::predict(fit, bad), "stored rubric reference hub")
  bad <- data$state
  bad$linking$phase_a$artifacts[["1"]]$fit_config_surface$predictive_prior_digest <- "different_generation"
  expect_error(stats::predict(fit, bad), "stored rubric reference hub")
  bad <- data$state
  bad$linking$phase_a$artifacts[["2"]]$fit_model_id <- "btl_b"
  expect_error(stats::predict(fit, bad), "fit_model_id")
  bad <- data$state
  bad$controller$hub_id <- 2L
  expect_error(stats::predict(fit, bad), "Phase B refit|identifiers|hub")
  bad <- data$state
  bad$trait <- "mechanics"
  expect_error(stats::predict(fit, bad), "Trait mismatch")
  bad <- data$state
  bad$linking$phase_a$artifacts[["2"]]$trait <- "mechanics"
  expect_error(stats::predict(fit, bad), "Trait mismatch")
  bad <- data$state
  bad$orientation <- "lower_is_better"
  expect_error(stats::predict(fit, bad), "orientation")
  bad <- data$state
  artifact <- bad$linking$phase_a$artifacts[["1"]]
  artifact$phase_a_within_set_evidence$y_A[[1L]] <- 0L
  bad$linking$phase_a$artifacts[["1"]] <- artifact
  expect_error(stats::predict(fit, bad), "evidence hash")
  artifact$phase_a_within_set_evidence_hash <-
    pairwiseLLM:::.adaptive_phase_a_hash_object(artifact$phase_a_within_set_evidence)
  bad$linking$phase_a$artifacts[["1"]] <- artifact
  expect_error(stats::predict(fit, bad), "hub evidence hash")
  # Even if the accepted state acknowledges changed evidence, the saved
  # calibration must reject it. The adapter verifies upstream evidence first.
  bad$linking$anchored_joint$accepted_state_by_spoke[["2"]]$phase_a_evidence_hash_hub <-
    artifact$phase_a_within_set_evidence_hash
  expect_error(stats::predict(fit, bad), "stored rubric reference hub")
})

test_that("reference reordering and compatible legacy hashes retain the same predictions", {
  skip_if_not_installed("ordinal")
  withr::local_seed(9204)
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  expected <- stats::predict(fit, data$state)
  reordered <- data$state
  reordered$linking$phase_a$artifacts[["1"]]$items <- data$reference$items[54:1, ]
  expect_identical(stats::predict(fit, reordered), expected)
  data$reference$items <- data$reference$items[54:1, ]
  data$rubric <- data$rubric[54:1, ]
  reordered_fit <- rubric_linked_fit(data)
  expect_equal(stats::predict(reordered_fit, data$state), expected, tolerance = 1e-12)
  legacy <- data$state
  legacy$linking$phase_a$artifacts[["1"]]$fit_config_hash <- "legacy-config-hash"
  for (x in list(legacy, data$state)) {
    pred <- stats::predict(fit, x)
    expect_identical(pred$theta, expected$theta)
    expect_identical(pred$probabilities, expected$probabilities)
  }
  expect_identical(attr(stats::predict(fit, legacy), "linking")$reference$fit_contract_hash, "legacy-config-hash")
  data$reference$fit_config_hash <- "legacy-config-hash"
  legacy_fit <- rubric_linked_fit(data)
  expect_equal(stats::predict(legacy_fit, data$state), expected, tolerance = 1e-12)
})

test_that("stored linked reference metadata is validated before scoring", {
  withr::local_seed(9205)
  data <- rubric_linked_fixture()
  prepare <- pairwiseLLM:::.rubric_prepare_calibration
  object <- prepare(data$reference, data$rubric, calibration_design = "linked_anchors", levels = data$levels)
  validate <- pairwiseLLM:::.rubric_validate_calibration
  for (value in list(NULL, list(), 1)) {
    bad <- object
    bad["reference"] <- list(value)
    expect_error(validate(bad), "reference identity")
  }
  for (value in list(NA_real_, Inf, 1e100, 1.5, "1", c(1L, 2L), matrix(1L))) {
    bad <- object
    bad$reference$set_id <- value
    expect_error(validate(bad), "reference identity")
  }
  for (field in c("items", "fit_contract", "fit_contract_hash", "evidence", "evidence_hash")) {
    bad <- object
    bad$reference[[field]] <- NULL
    expect_error(validate(bad), "reference identity")
  }
  bad <- object
  bad$reference$items$item_id[[2L]] <- bad$reference$items$item_id[[1L]]
  expect_error(validate(bad), "unique")
  bad <- object
  bad$reference$evidence_hash <- "wrong"
  expect_error(validate(bad), "evidence hash")
  for (field in c("theta", "theta_sd")) {
    bad <- object
    bad$reference$items[[field]][[1L]] <- bad$reference$items[[field]][[1L]] + 1e-10
    bad$cj$reference <- bad$reference
    expect_error(validate(bad), "calibration CJ metric")
  }
  bad <- object
  bad$reference$set_id <- 2L
  bad$cj$reference <- bad$reference
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$items$set_id <- NULL
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$items$set_id[[1L]] <- NA_integer_
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$reference$fit_contract$model_variant <- "btl_b"
  bad$cj$reference <- bad$reference
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$fit_contract_hash <- "different"
  expect_error(validate(bad), "calibration CJ metric")
  bad <- object
  bad$cj$scale_status <- "within_set"
  expect_error(validate(bad), "linked reference scale")
  bad <- object
  bad$cj$estimation_mode <- "fixed"
  expect_error(validate(bad), "linked reference estimation mode")
})

test_that("linked prediction preserves failed diagnostic evidence and accepted-state reuse", {
  skip_if_not_installed("ordinal")
  withr::local_seed(9206)
  data <- rubric_linked_fixture()
  fit <- rubric_linked_fit(data)
  expected <- stats::predict(fit, data$state)
  state <- data$state
  state$controller$link_refit_stats_by_spoke[["2"]]$link_diagnostics_pass <- FALSE
  expect_warning(pred <- stats::predict(fit, state), class = "pairwiseLLM_rubric_cj_diagnostics")
  expect_identical(pred$probabilities, expected$probabilities)
  expect_false(attr(pred, "linking")$diagnostics$diagnostics_pass)
  expect_identical(attr(pred, "linking")$provenance$finalization, "all_spokes_exhausted")
  state <- data$state
  state$controller$link_refit_stats_by_spoke[["2"]]$fit_contract$estimation_method <- "accepted_state_reuse"
  state$controller$link_refit_stats_by_spoke[["2"]]$fit_contract$uncertainty_approximation <- "accepted_state"
  reused <- stats::predict(fit, state)
  expect_identical(reused$probabilities, expected$probabilities)
  expect_identical(attr(reused, "linking")$fit_contract$spokes[["2"]]$phase_b$estimation_method,
    "accepted_state_reuse")
})

test_that("linked calibrations serialize and preserve optional backend dependency behavior", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("mgcv", minimum_version = "1.9.4")
  skip_if_not_installed("withr")
  withr::local_seed(9207)
  data <- rubric_linked_fixture()
  directory <- withr::local_tempdir()
  for (method in c("ordinal_linear", "ordinal_monotone")) {
    fit <- rubric_linked_fit(data, method)
    path <- file.path(directory, paste0(method, ".rds"))
    saveRDS(fit, path)
    expect_identical(stats::predict(readRDS(path), data$state), stats::predict(fit, data$state))
  }
  linear <- rubric_linked_fit(data)
  expected <- stats::predict(linear, data$state)
  testthat::local_mocked_bindings(.rubric_ordinal_available = function() FALSE, .package = "pairwiseLLM")
  expect_identical(stats::predict(linear, data$state), expected)
  expect_error(rubric_linked_fit(data), class = "pairwiseLLM_rubric_dependency_missing")
  testthat::local_mocked_bindings(.rubric_monotone_dependencies = function(...) {
    rlang::abort("Install mgcv", class = "pairwiseLLM_rubric_dependency_missing")
  }, .package = "pairwiseLLM")
  expect_error(stats::predict(fit, data$state), class = "pairwiseLLM_rubric_dependency_missing")
  expect_error(rubric_linked_fit(data, "ordinal_monotone"), class = "pairwiseLLM_rubric_dependency_missing")
})
