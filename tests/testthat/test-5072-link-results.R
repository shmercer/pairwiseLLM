test_that("common zero-edge contracts retain the prior and typed uncertainty", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    result <- link_contract_result(link_contract_input(id))
    expect_true(pairwiseLLM:::.link_validate_result(result))
    expect_identical(result$offset$delta_sd, 5)
    expect_identical(result$offset$identification, "prior_only")
    expect_equal(result$offset$delta_lower, qnorm(.025, 0, 5))
    expect_identical(result$diagnostics$finite_gradient, NA)
    expect_identical(result$diagnostics$covariance_jitter, NA_real_)
    expect_identical(result$diagnostics$convergence_code, NA_integer_)
    expect_equal(result$items$rank_link, rank(-result$items$theta_link_mean))
    bad <- result
    bad$offset$identification <- "cross_set"
    expect_error(pairwiseLLM:::.link_validate_result(bad), "Zero cross edges")
    bad <- result
    bad$offset$delta_lower <- -1
    expect_error(pairwiseLLM:::.link_validate_result(bad), "Normal prior")
  }
  e1 <- link_contract_result()
  expect_identical(e1$items$theta_link_sd, c(0, 0, 5, 5))
  failed <- link_contract_result(valid = FALSE)
  expect_true(all(is.na(failed$items$theta_link_mean)))
  expect_identical(failed$items$theta_link_sd, rep(NA_real_, 4))
  expect_identical(failed$offset$delta_sd, NA_real_)
  expect_null(failed$uncertainty$covariance)
  expect_identical(failed$diagnostics$failure_code, "synthetic_failure")
  args <- link_contract_args()
  args$control <- list(delta_prior = list(mean = 2, sd = 3))
  custom <- link_contract_result(do.call(prepare_link_input, args))
  expect_equal(custom$offset$delta_mean, 2)
  expect_equal(custom$offset$delta_sd, 3)
})

test_that("results and inputs survive plain RDS serialization", {
  dir <- withr::local_tempdir()
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    result <- link_contract_result(link_contract_input(id, 2L))
    saveRDS(result, file.path(dir, "result.rds"))
    restored <- readRDS(file.path(dir, "result.rds"))
    expect_identical(restored, result)
    expect_true(pairwiseLLM:::.link_validate_result(restored))
  }
})

test_that("result validation exposes inconsistent statistics and failures", {
  base <- link_contract_result()
  alterations <- list(
    function(x) { x$items$theta_link_sd[1] <- -1; x },
    function(x) { x$items$rank_link[1] <- 99; x },
    function(x) { x$items$theta_link_lower[1] <- 0; x },
    function(x) { x$offset$delta_sd <- 0; x },
    function(x) { x$diagnostics$uncertainty_scope <- "joint_shapes_and_offset"; x },
    function(x) { x$diagnostics$finite_objective <- FALSE; x },
    function(x) { x$diagnostics$covariance_valid <- FALSE; x },
    function(x) { x$prediction$state <- list(callback = function() 1); x },
    function(x) { x$provenance$hashes$cross <- "wrong"; x },
    function(x) { x$items$item_id <- rev(x$items$item_id); x },
    function(x) { x$estimator_version <- "legacy"; x },
    function(x) { x$uncertainty$item_transform[1, 1] <- 1; x },
    function(x) { x$diagnostics$n_parameters <- 4L; x },
    function(x) { x$items$theta_link_mean[1] <- Inf; x },
    function(x) { x$items$theta_link_mean <- rep(NA_real_, 4); x$items$rank_link <- rep(NA_real_, 4); x }
  )
  for (alter in alterations) expect_error(pairwiseLLM:::.link_validate_result(alter(base)), class = "pairwiseLLM_link_contract_error")
  failed <- link_contract_result(valid = FALSE)
  failed$diagnostics$failure_code <- NA_character_
  expect_error(pairwiseLLM:::.link_validate_result(failed), "explicit failure_code")
  expect_error(pairwiseLLM:::.link_validate_result(list(anchored_joint = TRUE)), "legacy Phase B")
})

test_that("zero-edge E2 must retain full independent bridge covariance", {
  result <- link_contract_result(link_contract_input("gaussian_posterior_bridge"))
  result$uncertainty$covariance[2, 3] <- result$uncertainty$covariance[3, 2] <- .1
  result$items$theta_link_sd <- sqrt(diag(result$uncertainty$item_transform %*%
    result$uncertainty$covariance %*% t(result$uncertainty$item_transform)))
  expect_error(pairwiseLLM:::.link_validate_result(result), "shapes must be independent")
  result <- link_contract_result(link_contract_input("gaussian_posterior_bridge"))
  result$uncertainty$covariance[2, 2] <- 10
  result$items$theta_link_sd <- sqrt(diag(result$uncertainty$item_transform %*%
    result$uncertainty$covariance %*% t(result$uncertainty$item_transform)))
  expect_error(pairwiseLLM:::.link_validate_result(result), "full Phase A bridge covariance")
})

test_that("singleton sets retain explicit offset uncertainty without shape coordinates", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    args <- link_contract_args(id)
    args$hub$items <- args$spoke$items <- data.frame(item_id = "one")
    args$phase_a <- switch(id,
      fixed_shape_offset = list(hub = list(points = c(one = 2)), spoke = list(points = c(one = 3))),
      gaussian_posterior_bridge = list(hub = list(draws = matrix(c(1, 2), 2, dimnames = list(NULL, "one"))),
        spoke = list(draws = matrix(c(3, 4), 2, dimnames = list(NULL, "one")))),
      joint_offset = list(hub = list(observations = args$cross), spoke = list(observations = args$cross)))
    input <- do.call(prepare_link_input, args)
    result <- pairwiseLLM:::.link_new_result(input, theta_mean = c(0, 0), theta_sd = c(0, 5),
      delta = list(mean = 0, sd = 5, lower = qnorm(.025, 0, 5), upper = qnorm(.975, 0, 5), identification = "prior_only"),
      covariance = matrix(25, 1, 1, dimnames = list("delta", "delta")),
      diagnostics = list(fit_valid = TRUE, covariance_valid = TRUE,
        uncertainty_scope = if (id == "fixed_shape_offset") "offset_only_conditional_on_fixed_shapes" else "joint_shapes_and_offset"))
    expect_identical(result$items$rank_link, c(1.5, 1.5))
    expect_identical(result$diagnostics$n_parameters, 1L)
  }
})

test_that("zero-edge E2 retains within-shape correlations in larger sets", {
  args <- link_contract_args("gaussian_posterior_bridge")
  z <- rbind(c(-2, 1, 1), c(-1, 0, 1), c(1, 1, -2), c(2, -2, 0))
  colnames(z) <- c("a", "b", "c")
  args$hub$items <- args$spoke$items <- data.frame(item_id = c("c", "a", "b"))
  args$phase_a <- list(hub = list(draws = z), spoke = list(draws = z * .5))
  input <- do.call(prepare_link_input, args)
  V <- matrix(0, 5, 5, dimnames = list(colnames(input$item_transform), colnames(input$item_transform)))
  V[1, 1] <- 25
  V[2:3, 2:3] <- cov(pairwiseLLM:::.link_to_reduced(z, input$basis$hub))
  V[4:5, 4:5] <- V[2:3, 2:3] / 4
  expect_gt(abs(V[2, 3]), .1)
  result <- pairwiseLLM:::.link_new_result(input, rep(0, 6),
    theta_sd = sqrt(diag(input$item_transform %*% V %*% t(input$item_transform))),
    delta = list(mean = 0, sd = 5, lower = qnorm(.025, 0, 5), upper = qnorm(.975, 0, 5), identification = "prior_only"),
    covariance = V, diagnostics = list(fit_valid = TRUE, covariance_valid = TRUE, uncertainty_scope = "joint_shapes_and_offset"))
  expect_true(pairwiseLLM:::.link_validate_result(result))
  result$uncertainty$covariance[2, 3] <- result$uncertainty$covariance[3, 2] <- 0
  V <- result$uncertainty$covariance
  result$items$theta_link_sd <- sqrt(diag(input$item_transform %*% V %*% t(input$item_transform)))
  expect_error(pairwiseLLM:::.link_validate_result(result), "full Phase A bridge covariance")
})
