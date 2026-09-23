# Provider-free interface smoke. Arguments: package checkout, output JSON.
# Synthetic sampler output tests plumbing only, not MCMC numerical correctness.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Supply package checkout and output JSON paths.")
repo <- normalizePath(args[1], mustWork = TRUE)
output <- normalizePath(args[2], mustWork = FALSE)
pkgload::load_all(repo, quiet = TRUE)
source(file.path(repo, "tests/testthat/helper-rubric-reference.R"), local = TRUE)
revision <- system2("git", c("-C", shQuote(repo), "rev-parse", "HEAD"), stdout = TRUE)
dirty <- length(system2("git", c("-C", shQuote(repo), "status", "--porcelain"), stdout = TRUE)) > 0L
report <- NULL
testthat::test_that("one verified standalone reference supports E1, E2 and E3", {
  testthat::local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler,
    .package = "pairwiseLLM")
  evidence <- rubric_reference_evidence()
  completed <- rubric_reference_completed(evidence)
  reference <- rubric_reference_prepare(completed, evidence)
  calibration <- fit_rubric_calibration(reference, rubric_reference_labels(reference),
    calibration_design = "linked_anchors")
  changed <- evidence
  changed$better_id[1] <- changed$B_id[1]
  changed$winner_pos[1] <- 2L
  testthat::expect_error(rubric_reference_prepare(completed, changed), "exact fitted rows")
  other <- rubric_reference_prepare(rubric_reference_completed(changed), changed)
  ids <- c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")
  fits <- lapply(ids, function(id) {
    input <- do.call(prepare_link_input, rubric_reference_link_args(reference, id))
    fit <- fit_link(input)
    prediction <- predict(calibration, fit)
    testthat::expect_true(fit$diagnostics$fit_valid)
    testthat::expect_equal(nrow(prediction), 3L)
    testthat::expect_true(all(is.finite(prediction$expected_level)))
    bad <- fit_link(do.call(prepare_link_input, rubric_reference_link_args(other, id)))
    testthat::expect_error(predict(calibration, bad), "reference hub")
    list(estimator_id = id, reference_hash = fit$provenance$phase_a_sources$hub$reference_hash,
      cross_hash = input$hashes$cross, fit_valid = fit$diagnostics$fit_valid,
      predicted_items = nrow(prediction), changed_reference_rejected = TRUE)
  })
  testthat::expect_identical(unique(vapply(fits, `[[`, "", "reference_hash")), reference$reference_hash)
  testthat::expect_length(unique(vapply(fits, `[[`, "", "cross_hash")), 1L)
  report <<- list(package_version = as.character(utils::packageVersion("pairwiseLLM")),
    commit = revision, dirty = dirty, provider_free = TRUE, sampler = "synthetic interface fixture",
    reference_hash = reference$reference_hash, evidence_hash = reference$fit_evidence$evidence_hash,
    estimators = fits)
})
jsonlite::write_json(report, output, auto_unbox = TRUE, pretty = TRUE)
cat("Standalone-reference smoke passed at", revision, "(dirty:", dirty, ")\n")
