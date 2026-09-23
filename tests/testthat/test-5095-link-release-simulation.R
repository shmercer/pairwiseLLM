test_that("known-offset simulations recover direction, symmetry and information", {
  # Singleton sets isolate the offset likelihood, making the truth hand-checkable.
  # Systematic quantiles give deterministic Bernoulli proportions, without lucky seeds.
  for (id in link_release_ids) {
    for (delta in c(0, 1.5)) for (beta in c(0, .4)) for (epsilon in c(0, .12)) {
      args <- link_contract_args(id, 0L)
      for (k in c("hub", "spoke")) {
        args[[k]]$items <- data.frame(item_id = "one")
        args$phase_a[[k]] <- switch(id,
          fixed_shape_offset = list(points = c(one = 0)),
          gaussian_posterior_bridge = list(draws = matrix(0, 4, 1, dimnames = list(NULL, "one"))),
          joint_offset = list(observations = link_e3_args()$phase_a[[k]]$observations[FALSE, ]))
      }
      args$judge$beta <- beta
      args$judge$epsilon <- epsilon
      sds <- numeric()
      for (n in c(20L, 200L)) {
        p <- (1 - epsilon) * plogis(-delta + beta) + epsilon / 2
        args$cross <- data.frame(observation_id = paste0("cross-", seq_len(n)),
          A_set = "H", A_item = "one", B_set = "S", B_item = "one",
          y_A = as.integer((seq_len(n) - .5) / n < p))
        fit <- fit_link(do.call(prepare_link_input, args))
        expect_true(fit$diagnostics$fit_valid, info = id)
        if (n == 200L) expect_lt(abs(fit$offset$delta_mean - delta), .12)
        # Weak data can leave a skewed posterior, especially with lapse. Check
        # interval coverage rather than imposing a tight posterior-mean error.
        expect_lte(fit$offset$delta_lower, delta)
        expect_gte(fit$offset$delta_upper, delta)
        reflected <- fit_link(do.call(prepare_link_input, link_release_reflect(args)))
        expect_lt(abs(reflected$offset$delta_mean + fit$offset$delta_mean), 1e-6)
        sds <- c(sds, fit$offset$delta_sd)
      }
      expect_lt(sds[2], sds[1])
      # Nearly separated evidence must still give finite, honest uncertainty.
      args$cross$y_A <- c(1L, rep(0L, nrow(args$cross) - 1L))
      separated <- fit_link(do.call(prepare_link_input, args))
      expect_true(separated$diagnostics$fit_valid)
      expect_gt(separated$offset$delta_mean, 0)
      expect_true(is.finite(separated$offset$delta_sd) && separated$offset$delta_sd > 0)
    }
  }
})
