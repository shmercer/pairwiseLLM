test_that("E2 draws bind IDs, center separately, and retain full reduced moments", {
  args <- link_e2_args(0)
  args$phase_a$hub$draws <- args$phase_a$hub$draws + seq_len(6)
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  expect_true(fit$diagnostics$fit_valid)
  for (k in c("hub", "spoke")) {
    draws <- args$phase_a[[k]]$draws
    direct <- (draws - rowMeans(draws)) %*% input$basis[[k]]$H
    bridge <- fit$diagnostics$bridge[[k]]
    expect_equal(bridge$mean, colMeans(direct))
    expect_equal(bridge$covariance, cov(direct))
    expect_equal(bridge$jitter, 0)
    expect_gt(abs(bridge$covariance[1, 2]), .01)
  }
  args$phase_a$hub$sd <- rep(1, 3)
  expect_error(do.call(prepare_link_input, args), "single-use evidence")
})

test_that("canonical artifacts supply only draws and audited source metadata", {
  args <- link_e2_args()
  direct <- do.call(prepare_link_input, args)
  a <- link_e2_artifact(args$hub, args$phase_a$hub$draws)
  args$phase_a$hub <- list(artifact = a)
  input <- do.call(prepare_link_input, args)
  expect_equal(input$phase_a$hub$value, direct$phase_a$hub$value)
  expect_identical(input$phase_a$hub$source$artifact_hash, pairwiseLLM:::.link_hash(a))
  expect_identical(input$counts$source_hub, 19L)
  expect_identical(input$counts$phase_a_hub, 0L)
  expect_identical(input$counts$phase_a_spoke, 0L)
  expect_identical(input$counts$cross, 8L)
  expect_named(input$phase_a$hub, c("kind", "value", "source", "centering"))
  expect_equal(fit_link(input)$items, fit_link(direct)$items)
  args$phase_a$hub$artifact$phase_a_within_set_evidence$y_A <- c(0L, 0L)
  changed <- do.call(prepare_link_input, args)
  expect_equal(fit_link(changed)$items, fit_link(input)$items)
  expect_false(identical(changed$hashes$phase_a_hub, input$hashes$phase_a_hub))
  args$phase_a$hub$source <- changed$phase_a$hub$source
  expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  args$phase_a$hub$source$n_observations <- 18L
  expect_error(do.call(prepare_link_input, args), "Artifact source mismatch")
})

test_that("E2 artifacts reject incompatible identities, scope, model, and draws", {
  args <- link_e2_args()
  a <- link_e2_artifact(args$hub, args$phase_a$hub$draws)
  alterations <- list(
    function(x) {
      x$set_id <- "wrong"
      x
    },
    function(x) {
      x$fit_model_id <- "btl"
      x
    },
    function(x) {
      x$phase_scope <- "phase_b"
      x
    },
    function(x) {
      x$phase_scope_set_id <- "S"
      x
    },
    function(x) {
      x$items <- list()
      x
    },
    function(x) {
      x$n_items <- 4
      x
    },
    function(x) {
      x$items$item_id[1] <- "wrong"
      x
    },
    function(x) {
      x$posterior_draws <- NULL
      x
    },
    function(x) {
      x$posterior_draws <- 1:3
      x
    },
    function(x) {
      x$posterior_draws[1, 1] <- NA
      x
    },
    function(x) {
      colnames(x$posterior_draws) <- NULL
      x
    },
    function(x) {
      colnames(x$posterior_draws)[1] <- "wrong"
      x
    },
    function(x) {
      x$posterior_draws <- x$posterior_draws[1, , drop = FALSE]
      x
    })
  for (alter in alterations) {
    args$phase_a$hub <- list(artifact = alter(a))
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  args$hub$items$global_item_id <- paste0("global-", args$hub$items$item_id)
  args$phase_a$hub <- list(artifact = a)
  expect_error(do.call(prepare_link_input, args), "missing required global")
  a$items$global_item_id <- paste0("global-", a$items$item_id)
  args$phase_a$hub <- list(artifact = a)
  expect_s3_class(do.call(prepare_link_input, args), "pairwiseLLM_link_input")
  args$phase_a$hub$artifact$items$global_item_id[1] <- "wrong"
  expect_error(do.call(prepare_link_input, args), "mapping mismatch")
  args$phase_a$hub <- list(artifact = a, draws = a$posterior_draws)
  expect_error(do.call(prepare_link_input, args), "single-use evidence")
})

test_that("the bounded jitter ladder selects the smallest successful value", {
  stabilize <- pairwiseLLM:::.link_e2_stabilize
  for (scale in c(.1, 1, 100)) {
    for (j in c(1e-12, 1e-10, 1e-8, 1e-6)) {
      V <- diag(c(scale, -j * scale / 2))
      out <- stabilize(V)
      expect_equal(out$jitter, j * scale, tolerance = 1e-14)
      expect_equal(out$covariance, V + diag(j * scale, 2))
    }
    expect_error(stabilize(diag(c(scale, -2e-6 * scale))), "requires more than")
  }
  expect_equal(stabilize(matrix(0, 2, 2))$scale, 1)
  expect_equal(stabilize(matrix(numeric(), 0, 0))$jitter, 0)
  expect_error(stabilize(matrix(c(1, 0, 1, 1), 2)), "symmetric")
  expect_error(stabilize(matrix(Inf, 2, 2)), "finite and square")
  expect_error(stabilize(matrix(0, 2, 3)), "finite and square")
  expect_error(stabilize("bad"), "finite and square")
})

test_that("zero-edge jitter is explicit and independently reconciled", {
  args <- link_e2_args(0)
  args$phase_a$hub$draws[, ] <- 0
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_true(fit$diagnostics$fit_valid)
  expect_equal(fit$diagnostics$bridge$hub$jitter, 1e-12)
  expect_equal(fit$uncertainty$covariance[2:3, 2:3], diag(1e-12, 2), ignore_attr = TRUE)
  fit$diagnostics$bridge$hub$jitter <- 1e-5
  expect_error(pairwiseLLM:::.link_validate_result(fit), "jitter ladder")
})
