test_that("E3 is invariant to item/evidence order and relabeling", {
  args <- link_e3_args()
  base <- fit_link(do.call(prepare_link_input, args))
  for (k in c("hub", "spoke")) {
    args[[k]]$items <- args[[k]]$items[3:1, , drop = FALSE]
    x <- args$phase_a[[k]]$observations
    args$phase_a[[k]]$observations <- x[nrow(x):1, ]
  }
  args$cross <- args$cross[nrow(args$cross):1, ]
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items, base$items, tolerance = 1e-9)
  rename <- c(a = "z", b = "x", c = "y")
  relabel <- function(x) {
    x$A_item <- unname(rename[x$A_item])
    x$B_item <- unname(rename[x$B_item])
    x
  }
  for (k in c("hub", "spoke")) {
    args[[k]]$items$item_id <- unname(rename[args[[k]]$items$item_id])
    args$phase_a[[k]]$observations <- relabel(args$phase_a[[k]]$observations)
  }
  args$cross <- relabel(args$cross)
  fit <- fit_link(do.call(prepare_link_input, args))
  expect_equal(fit$items$theta_link_mean[c(3, 1, 2, 6, 4, 5)], base$items$theta_link_mean, tolerance = 2e-6)
  expect_equal(fit$items$theta_link_sd[c(3, 1, 2, 6, 4, 5)], base$items$theta_link_sd, tolerance = 2e-6)
  expect_equal(fit$offset, base$offset, tolerance = 2e-6)
})

test_that("E3 sign and presentation reversal include every evidence block", {
  args <- link_e3_args()
  base <- fit_link(do.call(prepare_link_input, args))
  for (presentation in c(FALSE, TRUE)) {
    reverse <- function(x) {
      if (presentation) {
        old <- x
        x$A_set <- old$B_set
        x$A_item <- old$B_item
        x$B_set <- old$A_set
        x$B_item <- old$A_item
      }
      x$y_A <- 1L - x$y_A
      x
    }
    changed <- args
    changed$judge$beta <- -args$judge$beta
    changed$cross <- reverse(args$cross)
    for (k in c("hub", "spoke")) changed$phase_a[[k]]$observations <- reverse(args$phase_a[[k]]$observations)
    fit <- fit_link(do.call(prepare_link_input, changed))
    sign <- if (presentation) 1 else -1
    expect_equal(fit$items$theta_link_mean, sign * base$items$theta_link_mean, tolerance = 2e-6)
    expect_equal(fit$offset$delta_mean, sign * base$offset$delta_mean, tolerance = 2e-6)
    expect_equal(fit$uncertainty$covariance, base$uncertainty$covariance, tolerance = 2e-6)
  }
})

test_that("E3 predictions integrate covariance and survive serialization and continuation", {
  args <- link_e3_args()
  input <- do.call(prepare_link_input, args)
  fit <- fit_link(input)
  pairs <- args$cross[, -6]
  X <- pairwiseLLM:::.link_pair_surface(pairs, input)
  mu <- as.double(X %*% fit$continuation$mode) + input$judge$beta
  sd <- sqrt(rowSums((X %*% fit$uncertainty$covariance) * X))
  ref <- vapply(seq_along(mu), function(i) .88 * integrate(function(z) {
    dnorm(z) * plogis(mu[i] + sd[i] * z)
  }, -Inf, Inf, rel.tol = 1e-10)$value + .06, numeric(1))
  expect_equal(predict_link(fit, pairs), ref, tolerance = 1e-8)
  expect_identical(predict_link(fit, pairs[FALSE, ]), numeric())
  path <- tempfile(tmpdir = withr::local_tempdir(), fileext = ".rds")
  saveRDS(fit, path)
  expect_identical(predict_link(readRDS(path), pairs), predict_link(fit, pairs))
  old <- fit_link(link_e3_input(4))
  continued <- fit_link(input, old)
  expect_identical(continued$items, fit$items)
  expect_identical(continued$provenance$hashes, fit$provenance$hashes)
  expect_identical(continued$uncertainty, fit$uncertainty)
  args$control <- list(initial = setNames(rep(50, ncol(input$item_transform)), colnames(input$item_transform)))
  expect_identical(fit_link(do.call(prepare_link_input, args))$items, fit$items)
})
