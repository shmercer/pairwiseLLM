test_that("standalone fits record canonical evidence for every actual fitted subset", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  evidence <- rubric_reference_evidence()
  for (variant in c("btl", "btl_e", "btl_b", "btl_e_b")) {
    cj <- rubric_reference_completed(evidence, variant)
    reference <- rubric_reference_prepare(cj, evidence)
    expect_s3_class(reference, "pairwiseLLM_linked_rubric_reference")
    expect_identical(reference$fit_evidence, cj$fit$evidence_identity)
    expect_identical(reference$n_observations, nrow(evidence))
    expect_identical(reference$source$reference_hash, reference$reference_hash)
    expect_identical(reference$fit_contract$model_variant, variant)
    expect_identical(reference$judge$beta, if (model_has_b(variant)) .1 else 0)
  }
  cj <- rubric_reference_completed(evidence, pair_counts = c(25L, 50L))
  expect_identical(vapply(cj$fits, function(x) x$evidence_identity$n_observations, 1L), c(25L, 50L))
  expect_identical(rubric_reference_prepare(cj, evidence[1:50, ])$n_observations, 50L)
  expect_error(rubric_reference_prepare(cj, evidence), "exact fitted rows")
  cj <- rubric_reference_completed(evidence, pair_counts = c(25L, 50L), subset_method = "sample", seed = 291L)
  withr::local_seed(291L)
  selected <- sample(seq_len(nrow(evidence)))[1:50]
  expect_identical(rubric_reference_prepare(cj, evidence[selected, ])$n_observations, 50L)
  expect_error(rubric_reference_prepare(cj, evidence[1:50, ]), "exact fitted rows")
})

test_that("evidence mutations fail while harmless representation differences normalize", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  evidence <- rubric_reference_evidence()
  cj <- rubric_reference_completed(evidence)
  reference <- rubric_reference_prepare(cj, evidence)
  reordered <- as.data.frame(evidence[rev(seq_len(nrow(evidence))), ])
  rownames(reordered) <- paste0("row", seq_len(nrow(reordered)))
  attr(reordered, "note") <- "harmless"
  expect_identical(rubric_reference_prepare(cj, reordered), reference)
  changed <- evidence
  changed$better_id[1] <- changed$B_id[1]
  changed$winner_pos[1] <- 2L
  expect_error(rubric_reference_prepare(cj, changed), "exact fitted rows")
  expect_error(rubric_reference_prepare(cj, evidence[-1, ]), "exact fitted rows")
  expect_error(rubric_reference_prepare(cj, rbind(evidence, evidence[1, ])), "exact fitted rows")
  changed_cj <- rubric_reference_completed(changed)
  other <- rubric_reference_prepare(changed_cj, changed)
  expect_false(identical(other$reference_hash, reference$reference_hash))
  expect_false(identical(other$evidence_hash, reference$evidence_hash))
  repeated <- rbind(evidence, evidence[1, ])
  expect_identical(rubric_reference_prepare(rubric_reference_completed(repeated), repeated)$n_observations,
    nrow(repeated))
})

test_that("item order normalizes but changes to metric, identities and contracts do not", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  evidence <- rubric_reference_evidence()
  cj <- rubric_reference_completed(evidence)
  reference <- rubric_reference_prepare(cj)
  other_cj <- fit_bayes_btl_mcmc(evidence, rev(names(cj$fit$theta_mean)), model_variant = "btl",
    cmdstan = list(chains = 2L, parallel_chains = 1L))
  mapping <- reference$hub$items[rev(seq_len(nrow(reference$hub$items))), ]
  expect_identical(rubric_reference_prepare(other_cj, items = mapping), reference)
  mapping$global_item_id <- paste0("global-", mapping$item_id)
  other <- rubric_reference_prepare(cj, items = mapping)
  expect_false(identical(other$reference_hash, reference$reference_hash))
  expect_identical(other$items$item_id, sort(mapping$global_item_id))
  expect_error(rubric_reference_prepare(cj, items = mapping[-1, ]), "exact fitted item domain")
  mapping$global_item_id[1] <- NA_character_
  expect_error(rubric_reference_prepare(cj, items = mapping), "complete global")
  for (field in c("theta_mean", "theta_sd")) {
    changed <- cj
    changed$fits[[1]][[field]] <- changed$fits[[1]][[field]] + .01
    changed$fit <- changed$fits[[1]]
    changed$item_log_list[[1]][[field]] <- changed$item_log_list[[1]][[field]] + .01
    changed$item_summary[[field]] <- changed$item_summary[[field]] + .01
    expect_false(identical(rubric_reference_prepare(changed)$reference_hash, reference$reference_hash))
  }
  other <- prepare_linked_rubric_reference(cj, evidence, "H", trait = "mechanics")
  expect_false(identical(other$reference_hash, reference$reference_hash))
  other <- prepare_linked_rubric_reference(cj, evidence, 2L, trait = "organization")
  expect_identical(other$set_id, "2")
  expect_false(identical(other$reference_hash, reference$reference_hash))
  cj$orientation <- "lower_is_better"
  expect_error(rubric_reference_prepare(cj), "orientation")
})

test_that("legacy and incomplete fits cannot become reusable references", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  cj <- rubric_reference_completed()
  legacy <- cj
  legacy$fits[[1]]$evidence_identity <- NULL
  expect_error(rubric_reference_prepare(legacy), "Refit from the original evidence")
  # Existing same-set normalization still accepts the completed legacy result.
  expect_identical(.rubric_normalize_cj(legacy, "organization")$scale_status, "within_set")
  expect_error(rubric_reference_prepare(data.frame(theta = 1:3)), "supported completed")
  incomplete <- cj
  incomplete$item_log_list <- list()
  expect_error(rubric_reference_prepare(incomplete), "aligned refits")
  cj$fits[[1]]$inference_contract$judge_param_mode <- "phase_specific"
  expect_error(rubric_reference_prepare(cj), "judge_param_mode")
})

test_that("diagnostics warn and frozen references validate their contents after serialization", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  cj <- rubric_reference_completed()
  cj$fits[[1]]$diagnostics_pass <- FALSE
  expect_warning(reference <- rubric_reference_prepare(cj), class = "pairwiseLLM_rubric_cj_diagnostics")
  expect_false(reference$diagnostics$diagnostics_pass)
  expect_warning(.rubric_normalize_cj(reference), class = "pairwiseLLM_rubric_cj_diagnostics")
  path <- tempfile(fileext = ".rds")
  withr::defer(unlink(path))
  saveRDS(reference, path)
  expect_identical(readRDS(path), reference)
  expect_silent(.rubric_validate_standalone_reference(readRDS(path)))
  changed <- reference
  changed$points[1] <- changed$points[1] + 1
  expect_error(.rubric_normalize_cj(changed), "identity has changed")
  changed <- reference
  changed$source$reference_hash <- "caller-hash"
  expect_error(.rubric_normalize_cj(changed), "source identity")
  cj$fits[[1]]$diagnostics_pass <- TRUE
  expect_error(rubric_reference_prepare(cj, provenance = list(fn = identity)), "serializable data")
  reference <- rubric_reference_prepare(cj, provenance = list(reference_hash = "caller-hash", source_commit = "study"))
  expect_false(identical(reference$reference_hash, "caller-hash"))
  expect_identical(reference$provenance$supplied$source_commit, "study")
})

test_that("native configuration, draw identity and unsupported sources stay guarded", {
  local_mocked_bindings(.fit_bayes_btl_mcmc_adaptive = rubric_reference_sampler, .package = "pairwiseLLM")
  cj <- rubric_reference_completed()
  reference <- rubric_reference_prepare(cj)
  changed <- cj
  changed$fits[[1]]$theta_prior$prior_sd[1] <- 2
  expect_error(rubric_reference_prepare(changed), "Recorded fit configuration")
  changed <- cj
  changed$fits[[1]]$reference_fit_config <- NULL
  expect_error(rubric_reference_prepare(changed), "Refit from the original evidence")
  changed <- cj
  changed$fits[[1]]$theta_draws[1:2, 1] <- rev(changed$fits[[1]]$theta_draws[1:2, 1])
  expect_false(identical(rubric_reference_prepare(changed)$reference_hash, reference$reference_hash))
  expect_error(prepare_linked_rubric_reference(rubric_test_adaptive(), rubric_reference_evidence(),
    "H", trait = "organization"), "completed standalone")
  changed <- reference
  changed$format_version <- 2L
  expect_error(.rubric_normalize_cj(changed), "Invalid frozen")
  evidence <- rubric_reference_evidence()
  evidence$A_id[1] <- "unknown"
  evidence$better_id[1] <- "unknown"
  expect_error(rubric_reference_prepare(cj, evidence), "fitted item domain")
  scoped <- rubric_reference_evidence()
  scoped$judge_scope <- "shared"
  expect_s3_class(rubric_reference_prepare(rubric_reference_completed(scoped), scoped),
    "pairwiseLLM_linked_rubric_reference")
})
