test_that("explicit evidence is normalized, centered, and reconciled", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    x <- link_contract_input(id, 2L)
    expect_s3_class(x, "pairwiseLLM_link_input")
    expect_identical(x$hub$items$item_id, c("a", "b"))
    expect_identical(x$counts$cross, 2L)
    expect_identical(x$control$delta_prior, list(mean = 0, sd = 5))
    expect_true(pairwiseLLM:::.link_validate_input(x))
    args <- link_contract_args(id, 2L)
    args$provenance <- list(expected = list(hashes = x$hashes, counts = x$counts), source_commit = "fixture-sha")
    expect_identical(do.call(prepare_link_input, args)$hashes, x$hashes)
    args$provenance$expected$counts$cross <- 3L
    expect_error(do.call(prepare_link_input, args), "counts mismatch")
    args$provenance$expected <- list(hashes = list(cross = "wrong"))
    expect_error(do.call(prepare_link_input, args), "hashes mismatch")
  }
  x <- link_contract_input()
  expect_identical(x$phase_a$hub$value, c(a = -1, b = 1))
  expect_identical(x$phase_a$hub$centering$removed, 2)
  expect_identical(x$counts$phase_a_hub, 0L)
  expect_identical(x$provenance$source_commit, NA_character_)
  expect_identical(x$phase_a$hub$source$n_observations, NA_integer_)
  expect_error(prepare_link_input(), "explicit estimator")
  for (id in c("fixed", "anchored_joint", "", NA_character_)) {
    expect_error(pairwiseLLM:::.link_resolve(id), "explicit estimator")
  }
})

test_that("identity and payload permutations resolve to identical inputs", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    args <- link_contract_args(id, 2L)
    original <- do.call(prepare_link_input, args)
    args$hub$items <- args$hub$items[2:1, , drop = FALSE]
    args$spoke$items <- args$spoke$items[2:1, , drop = FALSE]
    if (id == "fixed_shape_offset") args$phase_a$hub$points <- args$phase_a$hub$points[2:1]
    if (id == "gaussian_posterior_bridge") args$phase_a$spoke$draws <- args$phase_a$spoke$draws[, 2:1]
    expect_identical(do.call(prepare_link_input, args), original)
  }
  args <- link_contract_args()
  args$hub$items$global_item_id <- c("gb", "ga")
  args$spoke$items$global_item_id <- c("sb", NA_character_)
  expect_identical(do.call(prepare_link_input, args)$hub$items$global_item_id, c("ga", "gb"))
  args$spoke$items$global_item_id[1L] <- "ga"
  expect_error(do.call(prepare_link_input, args), "Global item IDs")
  args <- link_contract_args()
  args$spoke$set_id <- "H"
  expect_error(do.call(prepare_link_input, args), "set IDs must differ")
  args$spoke$set_id <- "S"
  args$hub$items$item_id <- c("a", "a")
  expect_error(do.call(prepare_link_input, args), "unique")
})

test_that("accidental evidence reuse is rejected and genuine repeats remain", {
  args <- link_contract_args("joint_offset", 2L)
  x <- do.call(prepare_link_input, args)
  expect_identical(x$cross$observation_id, c("cross-1", "cross-2"))
  args$cross$observation_id[2] <- "cross-1"
  expect_error(do.call(prepare_link_input, args), "unique")
  args$cross$observation_id[2] <- "within-h"
  expect_error(do.call(prepare_link_input, args), "reuse evidence")
  args <- link_contract_args("joint_offset", 2L)
  args$cross$y_A <- c(1, .8)
  expect_error(do.call(prepare_link_input, args), "exact binary")
  args$cross$y_A <- c(1, NA)
  expect_error(do.call(prepare_link_input, args), "exact binary")
  args$cross$y_A <- c(1, 0)
  args$cross$B_set <- "H"
  expect_error(do.call(prepare_link_input, args), "illegal")
  args <- link_contract_args("joint_offset")
  args$phase_a$hub$observations$B_item <- "a"
  expect_error(do.call(prepare_link_input, args), "self comparisons")
  args$phase_a$hub$observations$B_item <- "absent"
  expect_error(do.call(prepare_link_input, args), "illegal")
  args <- link_contract_args("joint_offset", 2L)
  changed <- args
  changed$cross <- changed$cross[2:1, ]
  expect_false(identical(do.call(prepare_link_input, args)$hashes$cross, do.call(prepare_link_input, changed)$hashes$cross))
})

test_that("Phase A payloads enforce single-use and source count contracts", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    args <- link_contract_args(id)
    args$phase_a$hub$posterior_prior <- list(mean = 0, sd = 1)
    expect_error(do.call(prepare_link_input, args), "single-use")
  }
  args <- link_contract_args("gaussian_posterior_bridge")
  args$phase_a$hub$observations <- link_contract_args("joint_offset")$phase_a$hub$observations
  expect_error(do.call(prepare_link_input, args), "single-use")
  args <- link_contract_args("joint_offset")
  args$phase_a$hub$source <- list(n_observations = 2L)
  expect_error(do.call(prepare_link_input, args), "count mismatch")
  args$phase_a$hub$source <- list(n_observations = 1L, artifact_hash = "external", evidence_hash = "source-scheme:abc")
  expect_identical(do.call(prepare_link_input, args)$phase_a$hub$source$artifact_hash, "external")
  args$phase_a$hub$source$n_observations <- .5
  expect_error(do.call(prepare_link_input, args), "integer")
  args <- link_contract_args()
  args$phase_a$hub$points <- c(a = Inf, b = 2)
  expect_error(do.call(prepare_link_input, args), "finite named")
  args$phase_a$hub$points <- c(c = 1, b = 2)
  expect_error(do.call(prepare_link_input, args), "exactly match")
  args <- link_contract_args("gaussian_posterior_bridge")
  args$phase_a$hub$draws <- args$phase_a$hub$draws[1, , drop = FALSE]
  expect_error(do.call(prepare_link_input, args), "at least two")
  args <- link_contract_args("gaussian_posterior_bridge")
  colnames(args$phase_a$hub$draws) <- c("wrong", "b")
  expect_error(do.call(prepare_link_input, args), "exactly match")
})

test_that("judge surface, controls, and input tampering fail clearly", {
  args <- link_contract_args()
  args$control <- list(delta_prior = list(mean = 1, sd = 2), initial = c(delta = .5))
  x <- do.call(prepare_link_input, args)
  expect_identical(x$control$initial, c(delta = .5))
  expect_true(pairwiseLLM:::.link_validate_input(x))
  args$control$initial <- c(wrong = 1)
  expect_error(do.call(prepare_link_input, args), "free-coordinate")
  args$control <- list(delta_prior = list(mean = 0, sd = 0))
  expect_error(do.call(prepare_link_input, args), "positive")
  args$control <- list(estimator = list(posterior_prior = 1))
  expect_error(do.call(prepare_link_input, args), "estimator controls")
  args$control <- list()
  for (field in c("beta", "epsilon")) {
    changed <- args
    changed$judge[[field]] <- Inf
    expect_error(do.call(prepare_link_input, changed), "Invalid judge")
  }
  args$judge$link <- "probit"
  expect_error(do.call(prepare_link_input, args), "logit")
  args$judge$link <- "logit"
  args$judge$model_variant <- "btl"
  expect_error(do.call(prepare_link_input, args), "beta must be zero")
  args$judge$beta <- 0
  expect_error(do.call(prepare_link_input, args), "epsilon must be zero")
  args$judge$epsilon <- 0
  expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  x <- link_contract_input(edges = 1L)
  x$cross$y_A <- 0L
  expect_error(fit_link(x), "hash mismatch")
  x <- link_contract_input()
  x$basis$hub$H <- -x$basis$hub$H
  expect_error(fit_link(x), "basis was modified")
  x <- link_contract_input()
  x$phase_a$hub$value <- x$phase_a$hub$value + 1
  expect_error(fit_link(x), "normalized and centered")
})

test_that("serialized normalized payloads cannot add a second evidence channel", {
  x <- link_contract_input("joint_offset")
  x$phase_a$hub$posterior_prior <- list(mean = 0, sd = 1)
  expect_error(fit_link(x), "single-use evidence")
  x <- link_contract_input()
  x$phase_a$hub$centering$removed <- NA_real_
  expect_error(fit_link(x), "centering offsets")
  args <- link_contract_args()
  args$hub$items$global_item_id <- c(Inf, 1)
  expect_error(do.call(prepare_link_input, args), "global_item_id")
})

test_that("E1 canonical artifacts resolve EAP points without Phase A likelihood replay", {
  args <- link_contract_args(edges = 2L)
  direct <- fit_link(do.call(prepare_link_input, args))
  artifact <- link_e1_artifact(args$hub, args$phase_a$hub$points)
  args$phase_a$hub <- list(artifact = artifact)
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  expect_identical(input$phase_a$hub$kind, "points")
  expect_identical(input$phase_a$hub$value, c(a = -1, b = 1))
  expect_identical(input$phase_a$hub$centering$removed, 2)
  expect_identical(input$phase_a$hub$source$artifact_hash, .link_hash(artifact))
  expect_identical(input$counts$source_hub, 12L)
  expect_identical(input$counts$phase_a_hub, 0L)
  expect_identical(fit$offset, direct$offset)
  expect_identical(fit$items, direct$items)
  expect_null(input$phase_a$hub$artifact)
  args$phase_a$hub$artifact$phase_a_within_set_evidence$y_A <- c(1L, 0L)
  args$phase_a$hub$artifact$posterior_draws[] <- 999
  args$phase_a$hub$artifact$items$theta_raw_sd <- c(NA_real_, -100)
  changed <- fit_link(do.call(prepare_link_input, args))
  expect_identical(changed$offset, fit$offset)
  expect_false(identical(changed$provenance$hashes$phase_a_hub, fit$provenance$hashes$phase_a_hub))
  args$phase_a$hub$artifact$items <- args$phase_a$hub$artifact$items[2:1, ]
  expect_identical(fit_link(do.call(prepare_link_input, args))$items, fit$items)
  args$phase_a$hub$source <- do.call(prepare_link_input, args)$phase_a$hub$source
  expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  args$phase_a$hub$source$n_observations <- 99L
  expect_error(do.call(prepare_link_input, args), "Artifact source mismatch")
})

test_that("E1 rejects incompatible artifacts, identities and mixed payloads", {
  base <- link_contract_args()
  artifact <- link_e1_artifact(base$hub, base$phase_a$hub$points)
  alterations <- list(
    function(a) "artifact.rds",
    function(a) { a$set_id <- "wrong"; a },
    function(a) { a$fit_model_id <- "btl"; a },
    function(a) { a$phase_scope <- "phase_b"; a },
    function(a) { a$phase_scope_set_id <- "S"; a },
    function(a) { a$items$theta_raw_mean <- NULL; a },
    function(a) { a$items$item_id <- c("a", "a"); a },
    function(a) { a$n_items <- 3L; a },
    function(a) { a$items$theta_raw_mean[1] <- Inf; a },
    function(a) { a$n_pairs_committed <- 1.5; a })
  for (alter in alterations) {
    args <- base
    args$phase_a$hub <- list(artifact = alter(artifact))
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  for (field in c("points", "draws", "observations")) {
    args <- base
    args$phase_a$hub <- list(artifact = artifact)
    args$phase_a$hub[[field]] <- c(a = 1, b = 2)
    expect_error(do.call(prepare_link_input, args), "single-use")
  }
  args <- base
  args$hub$items$global_item_id <- c("gb", "ga")
  args$phase_a$hub <- list(artifact = artifact)
  expect_error(do.call(prepare_link_input, args), "missing required global")
  args$phase_a$hub$artifact$items$global_item_id <- c("gb", "ga")
  expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  args$phase_a$hub$artifact$items$global_item_id <- c("ga", "gb")
  expect_error(do.call(prepare_link_input, args), "global_item_id mapping")
  args$phase_a$hub$artifact$items$global_item_id <- c("ga", "ga")
  expect_error(do.call(prepare_link_input, args), "unique")
})
