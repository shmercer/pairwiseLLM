test_that("E1--E3 session identity, predictions, and append-only resume round-trip exactly", {
  dir <- withr::local_tempdir()
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    input <- link_contract_input(id, 2L)
    state <- start_link_session(input)
    path <- file.path(dir, paste0(id, ".rds"))
    save_link_session(state, path)
    restored <- load_link_session(path, input)
    expect_identical(restored, state)
    expect_identical(resume_link_session(restored, input), state)
    expect_error(save_link_session(state, path), "exists")
    expect_identical(save_link_session(state, path, TRUE), path)
    next_input <- link_contract_input(id, 4L)
    resumed <- resume_link_session(restored, next_input)
    actual <- resumed$linking$estimator$accepted_state_by_spoke$S
    fresh <- fit_link(next_input)
    expect_identical(actual$items, fresh$items)
    expect_identical(actual$offset, fresh$offset)
    pairs <- input$cross[, setdiff(names(input$cross), "y_A")]
    expect_identical(predict_link(actual, pairs), predict_link(fresh, pairs))
    expect_identical(resumed$link_stage_log$phase_b_active_edges_used, c(2L, 4L))
    expect_identical(resumed$link_stage_log$cross_evidence_hash, c(input$hashes$cross, next_input$hashes$cross))
    expect_identical(resumed$linking$estimator$identity_by_spoke$S, actual$provenance)
    expect_false(any(grepl("anchored_joint", names(state$linking$estimator))))
    folder <- file.path(dir, id)
    save_adaptive_session(state, folder)
    expect_identical(load_adaptive_session(folder), state)
    expect_identical(validate_session_dir(folder)$identity_hash, state$identity_hash)
  }
})

test_that("saved identity rejects changed estimator, order, configuration, evidence and continuation", {
  dir <- withr::local_tempdir()
  input <- link_contract_input(edges = 2L)
  state <- start_link_session(input)
  other <- link_contract_input("gaussian_posterior_bridge", 2L)
  expect_error(resume_link_session(state, other), "estimator mismatch")
  args <- link_contract_args(edges = 2L)
  args$control <- list(estimator = list(rel_tol = 1e-7))
  expect_error(resume_link_session(state, do.call(prepare_link_input, args)), "control/configuration")
  args <- link_contract_args(edges = 2L)
  args$phase_a$hub$points[1] <- 10
  expect_error(resume_link_session(state, do.call(prepare_link_input, args)), "phase_a/configuration")
  args <- link_contract_args(edges = 2L)
  args$cross$y_A[1] <- 0L
  expect_error(resume_link_session(state, do.call(prepare_link_input, args)), "unchanged old evidence prefix")
  args$cross <- args$cross[2:1, ]
  expect_error(resume_link_session(state, do.call(prepare_link_input, args)), "unchanged old evidence prefix")
  expect_error(resume_link_session(state, link_contract_input(edges = 1L)), "unchanged old evidence prefix")
  bad_input <- input
  bad_input$hub$items <- bad_input$hub$items[2:1, ]
  expect_error(resume_link_session(state, bad_input), "modified|normalized")
  path <- file.path(dir, "session.rds")
  save_link_session(state, path)
  expect_error(load_link_session(path, other), "does not match")
  for (mutate in list(
    function(x) { x$linking$estimator$id <- "joint_offset"; x },
    function(x) { x$linking$estimator$accepted_state_by_spoke$S$continuation$input$cross$y_A[1] <- 0L; x },
    function(x) { x$linking$estimator$accepted_state_by_spoke$S$items <- x$linking$estimator$accepted_state_by_spoke$S$items[4:1, ]; x },
    function(x) { x$linking$estimator$accepted_state_by_spoke$S$continuation$mode <- c(delta = 99); x },
    function(x) { x$link_stage_log$delta_spoke_mean <- 99; x })) {
    bad <- mutate(state)
    saveRDS(bad, path)
    expect_error(load_link_session(path), "identity hash mismatch")
    expect_error(save_link_session(bad, path, TRUE), "identity hash mismatch")
  }
})

test_that("status is explicit and frozen spokes require explicit reactivation", {
  input <- link_contract_input(edges = 1L)
  state <- start_link_session(input, "frozen")
  expect_error(resume_link_session(state, link_contract_input(edges = 2L)), "frozen spoke")
  active <- resume_link_session(state, link_contract_input(edges = 2L), "active")
  expect_identical(active$status_by_spoke$S, "active")
  probe <- resume_link_session(active, link_contract_input(edges = 2L), "probe")
  expect_identical(probe$linking$estimator, active$linking$estimator)
  expect_identical(probe$status_by_spoke$S, "probe")
  expect_true(summary(probe)$fit_reused)
  expect_false(summary(active)$fit_reused)
  expect_error(start_link_session(input, "unknown"), "status")
})

test_that("multi-spoke sessions preserve per-fit hub posteriors and reject incompatible shared inputs", {
  args <- link_contract_args("gaussian_posterior_bridge", 2L)
  input <- do.call(prepare_link_input, args)
  args$spoke$set_id <- "T"
  args$cross$B_set <- "T"
  args$cross$y_A[] <- 0L
  next_input <- do.call(prepare_link_input, args)
  state <- start_link_session(list(input, next_input))
  expect_named(state$linking$estimator$accepted_state_by_spoke, c("S", "T"))
  items <- summarize_items(state)
  expect_identical(items$link_spoke_id, rep(c("S", "T"), each = 4L))
  expect_false(identical(items$theta_link_mean[1:2], items$theta_link_mean[5:6]))
  expect_identical(summary(state)$spoke_set_id, c("S", "T"))
  expect_error(start_link_session(list(input, input)), "unique spoke")
  args$judge$beta <- .5
  expect_error(start_link_session(list(input, do.call(prepare_link_input, args))), "judge")
  args$judge$beta <- .2
  args$phase_a$hub$draws[1, 1] <- 5
  expect_error(start_link_session(list(input, do.call(prepare_link_input, args))), "hub Phase A")
  expect_error(resume_link_session(start_link_session(input), next_input), "spoke is absent")
})

test_that("legacy Phase B sessions fail with a restart path while Phase A is not rejected", {
  legacy <- structure(list(controller = list(link_estimation_mode = "anchored_joint"),
    linking = list(phase_a = list(phase = "phase_b"))), class = "adaptive_state")
  dir <- withr::local_tempdir()
  path <- file.path(dir, "legacy.rds")
  saveRDS(legacy, path)
  expect_error(load_link_session(path), "Restart linking from compatible Phase A", class = "pairwiseLLM_unsupported_legacy_link_state")
  expect_error(save_adaptive_session(legacy, dir), class = "pairwiseLLM_unsupported_legacy_link_state")
  expect_error(pairwiseLLM:::.adaptive_validate_state_for_resume(legacy), class = "pairwiseLLM_unsupported_legacy_link_state")
  missing_mode <- legacy
  missing_mode$controller <- NULL
  missing_mode$linking$anchored_joint <- list(accepted_state_by_spoke = list())
  expect_error(pairwiseLLM:::.link_reject_legacy(missing_mode), "Restart linking")
  missing_mode$linking$anchored_joint <- NULL
  missing_mode$link_stage_log <- data.frame(link_estimation_mode = "anchored_joint")
  expect_error(pairwiseLLM:::.link_reject_legacy(missing_mode), "Restart linking")
  legacy$linking$phase_a$phase <- "phase_a"
  expect_silent(pairwiseLLM:::.link_reject_legacy(legacy))
  legacy$linking$anchored_joint$accepted_state_by_spoke$S <- list(anchored_joint_init_state_method = "phase_b_refit")
  expect_error(pairwiseLLM:::.link_reject_legacy(legacy), "cannot be migrated")
})

test_that("MCMC session loading and identical resume never resample", {
  skip_if_not_installed("posterior")
  args <- link_e3_args(0)
  args$control <- list(estimator = list(engine = "mcmc", cmdstan = list(seed = 278L)))
  input <- do.call(prepare_link_input, args)
  sampled <- link_e3_mock_sampler(input)
  calls <- 0L
  local_mocked_bindings(.btl_mcmc_require_cmdstanr = function() invisible(NULL),
    .link_e3_sample = function(...) { calls <<- calls + 1L; sampled }, .package = "pairwiseLLM")
  state <- start_link_session(input)
  path <- file.path(withr::local_tempdir(), "mcmc.rds")
  save_link_session(state, path)
  restored <- load_link_session(path, input)
  expect_identical(restored, state)
  expect_identical(resume_link_session(restored, input), state)
  expect_identical(calls, 1L)
  r <- restored$linking$estimator$accepted_state_by_spoke$S
  expect_true(r$diagnostics$fit_valid)
  expect_gt(nrow(r$prediction$state$free_draws), 0L)
  pairs <- link_e3_args(2)$cross[, -6]
  expect_identical(predict_link(r, pairs),
    predict_link(state$linking$estimator$accepted_state_by_spoke$S, pairs))
})

test_that("invalid session metadata and mixed directories fail without overwriting artifacts", {
  input <- link_contract_input(edges = 1L)
  state <- start_link_session(input)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "state.rds")
  saveRDS(list(marker = TRUE), path)
  expect_error(save_adaptive_session(state, dir), "Cannot mix")
  save_link_session(state, file.path(dir, "link-session.rds"))
  expect_error(load_adaptive_session(dir), "Ambiguous")
  expect_error(validate_session_dir(dir), "Ambiguous")
  expect_identical(readRDS(path), list(marker = TRUE))
  expect_error(save_adaptive_session(adaptive_rank_start(letters[1:3]), dir), "Cannot mix")
  expect_error(save_link_session(state, NA_character_), "Invalid session path")
  expect_error(save_link_session(state, path, NA), "overwrite")
  expect_error(start_link_session(list()), "prepared linking inputs")
  expect_error(start_link_session(NULL), "prepared linking inputs")
  expect_error(pairwiseLLM:::.link_session_validate(list()), "Unsupported linking session")
  expect_silent(pairwiseLLM:::.link_reject_legacy(NULL))
  # Internal invariants are checked even when the outer integrity checksum is valid.
  for (change in list(
    function(x) { x$linking$estimator$version <- "future"; x },
    function(x) { names(x$status_by_spoke) <- "missing"; x },
    function(x) { x$linking$estimator$diagnostics_by_spoke$S <- list(); x },
    function(x) { x$status_by_spoke$S <- "unknown"; x })) {
    bad <- change(state)
    bad$identity_hash <- pairwiseLLM:::.link_session_hash(bad)
    expect_error(pairwiseLLM:::.link_session_validate(bad), class = "pairwiseLLM_link_contract_error")
  }
  expect_error(load_link_session(file.path(dir, "link-session.rds"),
    list(link_contract_input(), link_contract_input())), "unique spoke")
})
