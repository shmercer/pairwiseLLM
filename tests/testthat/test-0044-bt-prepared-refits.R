# Declared before inspecting equivalence results: scores and solver iterates are
# bitwise identical (the solver input/order is unchanged). Algebraic information
# calculations allow 1e-10 * (1 + abs(reference)) elementwise, three orders below
# the 1e-7 stationarity gates. Flags, failures, IDs and integer counts are exact.
bt_close_324 <- function(actual, expected) {
  expect_identical(is.na(actual), is.na(expected))
  good <- is.finite(expected)
  expect_identical(actual[!good], expected[!good])
  expect_true(all(abs(actual[good] - expected[good]) <= 1e-10 * (1 + abs(expected[good]))))
}

bt_compare_324 <- function(actual, expected) {
  expect_identical(class(actual), class(expected))
  if (inherits(expected, "error")) {
    expect_identical(actual$message, expected$message)
    expect_identical(actual$failure_reason, expected$failure_reason)
    expect_identical(actual$theta, expected$theta)
  } else {
    expect_identical(actual$theta$ID, expected$theta$ID)
    expect_identical(actual$theta$theta, expected$theta$theta)
    expect_identical(actual$fit$coefficients, expected$fit$coefficients)
    expect_identical(actual$fit$iter, expected$fit$iter)
    expect_identical(actual$fit$converged, expected$fit$converged)
    expect_identical(actual$fit$deviance, expected$fit$deviance)
    bt_close_324(actual$theta$se, expected$theta$se)
    bt_close_324(actual$vcov, expected$vcov)
    bt_close_324(unlist(actual$ssr[1:4]), unlist(expected$ssr[1:4]))
    expect_identical(actual$ssr[5:8], expected$ssr[5:8])
  }
  expect_identical(actual$provenance, expected$provenance)
  for (name in names(expected$diagnostics)) {
    a <- actual$diagnostics[[name]]
    e <- expected$diagnostics[[name]]
    if (is.numeric(e)) {
      bt_close_324(a, e)
    } else if (is.list(e) && "rcond" %in% names(e)) {
      bt_close_324(a$rcond, e$rcond)
      expect_identical(a[setdiff(names(a), "rcond")], e[setdiff(names(e), "rcond")])
    } else {
      expect_identical(a, e)
    }
  }
}

test_that("prepared counts, orientation, designs and all-pairs pseudo-counts match the frozen source", {
  reference <- bt_reference_324()
  for (n in c(2L, 8L, 57L, 91L, 229L)) {
    for (graph in c("sparse", "complete", "reversed", "unbalanced")) {
      dat <- bt_fixture_324(n, graph)
      items <- .bt_prepare_items(unique(c(dat[[1L]], dat[[2L]])), all_pairs = TRUE)
      prepared <- .bt_prepare_schedule(dat, items)
      before <- serialize(prepared, NULL)
      expected <- reference$.bt_binary_design(dat)
      actual <- .bt_binary_design(dat, prepared)
      expect_identical(actual[names(expected)], expected)
      for (alpha in c(0, 1e-10, 0.5)) {
        old <- reference$.bt_alpha_kernel(expected, alpha)
        new <- .bt_alpha_kernel(actual, alpha)
        expect_identical(new[names(old)], old)
        expect_equal(nrow(new$counts), choose(n, 2L))
      }
      dat[[3L]] <- 1 - dat[[3L]]
      actual <- .bt_binary_design(dat, prepared)
      expected <- reference$.bt_binary_design(dat)
      expect_identical(actual[names(expected)], expected)
      expect_identical(serialize(prepared, NULL), before)
    }
  }
})

test_that("prepared schedules reject stale rows and invalid outcomes", {
  dat <- bt_fixture_324(5L)
  prepared <- .bt_prepare_schedule(dat)
  for (changed in list(dat[rev(seq_len(nrow(dat))), ], dat[c(2, 1, 3)], dat[-1L, ])) {
    expect_error(.bt_binary_design(changed, prepared), "does not match")
  }
  for (bad in list(NA_real_, Inf, 0.3, "1", 1i)) {
    changed <- dat
    changed[[3L]] <- rep(bad, nrow(dat))
    expect_error(.bt_binary_design(changed, prepared), "finite numeric")
  }
  dat[[3L]][1L] <- 0.5
  old <- bt_reference_324()$.bt_binary_design(dat)
  expect_identical(.bt_binary_design(dat, prepared)[names(old)], old)
})

test_that("zero-alpha preparation omits unused binary blocks, including complete graphs", {
  for (graph in c("sparse", "complete")) {
    dat <- bt_fixture_324(12L, graph)
    items <- .bt_prepare_items(unique(c(dat[[1L]], dat[[2L]])), TRUE, binary = FALSE)
    prepared <- .bt_prepare_schedule(dat, items)
    expect_null(prepared$alpha$binary_design)
    expect_identical(.bt_fit_alpha(dat, 0, FALSE, list(), prepared)$theta,
      bt_reference_324()$fit_bt_model(dat, "alpha", FALSE, alpha = 0)$theta)
  }
})

test_that("full fits and failures preserve the original estimator and numerical gates", {
  skip_if_not_installed("brglm2")
  reference <- bt_reference_324()
  large <- identical(Sys.getenv("PAIRWISELLM_TEST_BT_REFITS_LARGE"), "true")
  sizes <- if (large) c(8L, 57L, 91L, 229L) else 8L
  for (n in sizes) {
    for (graph in c("complete", "sparse", "reversed", "unbalanced", "near_separated", "near_zero", "separated")) {
      dat <- bt_fixture_324(n, graph)
      prepared <- .bt_prepare_schedule(dat, .bt_prepare_items(unique(c(dat[[1]], dat[[2]])), TRUE))
      for (engine in c("alpha", "brglm2")) {
        for (alpha in if (engine == "alpha") c(0, 1e-10, 0.5) else NA_real_) {
          old <- tryCatch(suppressWarnings(reference$fit_bt_model(dat, engine, verbose = FALSE,
            alpha = if (engine == "alpha") alpha else NULL)), error = identity)
          new <- tryCatch(suppressWarnings(if (engine == "alpha") {
            .bt_fit_alpha(dat, alpha, FALSE, list(), prepared)
          } else {
            .bt_fit_firth(dat, FALSE, list(), prepared)
          }), error = identity)
          bt_compare_324(new, old)
        }
      }
    }
  }
  # Explicit numerical stopping and conditioning failures use the same classes.
  dat <- bt_fixture_324(8L)
  for (control in list(list(maxit = 1L), list(min_rcond = 0.99), list(gradient_tol = 1e-25))) {
    old <- tryCatch(suppressWarnings(reference$fit_bt_model(dat, "alpha", FALSE, alpha = .5,
      control = control)), error = identity)
    new <- tryCatch(suppressWarnings(fit_bt_model(dat, "alpha", FALSE, alpha = .5,
      control = control)), error = identity)
    bt_compare_324(new, old)
  }
})

test_that("the packaged writing comparisons retain full-fit equivalence", {
  skip_if_not_installed("brglm2")
  data <- new.env()
  utils::data("example_writing_pairs", package = "pairwiseLLM", envir = data)
  dat <- build_bt_data(data$example_writing_pairs)
  reference <- bt_reference_324()
  for (engine in c("alpha", "brglm2")) {
    old <- reference$fit_bt_model(dat, engine, FALSE, alpha = if (engine == "alpha") .5 else NULL)
    new <- fit_bt_model(dat, engine, FALSE, alpha = if (engine == "alpha") .5 else NULL)
    bt_compare_324(new, old)
  }
})

test_that("small nonzero scores are preserved without thresholding", {
  dat <- bootstrap_data(5L, 8L)
  old <- bt_reference_324()$fit_bt_model(dat, "alpha", FALSE, alpha = 1e5)
  new <- fit_bt_model(dat, "alpha", FALSE, alpha = 1e5)
  expect_true(all(abs(old$theta$theta) > 0 & abs(old$theta$theta) < 1e-5))
  bt_compare_324(new, old)
})

test_that("fixed and changing adaptive graphs retain baseline bootstrap summaries and failures", {
  reference <- bt_reference_324()
  withr::local_seed(324)
  before <- .Random.seed
  for (engine in c("alpha", if (requireNamespace("brglm2", quietly = TRUE)) "brglm2")) {
    args <- if (engine == "alpha") list(alpha = .5) else list()
    dat <- bt_fixture_324(8L)
    theta <- setNames(seq(-.3, .3, length.out = 8L), sort(unique(c(dat[[1]], dat[[2]]))))
    old <- reference$bootstrap_bt_model(theta, "fixed", 4L, 324L, schedule = dat,
      estimator = engine, estimator_args = args, keep = "full")
    new <- bootstrap_bt_model(theta, "fixed", 4L, 324L, schedule = dat,
      estimator = engine, estimator_args = args, keep = "full")
    expect_identical(new, old)
    state <- bootstrap_state(ids = names(theta))
    old <- reference$bootstrap_bt_model(theta, "adaptive", 3L, 324L, initial_state = state,
      budget = 12L, estimator = engine, estimator_args = args,
      btl_config = list(refit_pairs_target = 100L), keep = "full")
    new <- bootstrap_bt_model(theta, "adaptive", 3L, 324L, initial_state = state,
      budget = 12L, estimator = engine, estimator_args = args,
      btl_config = list(refit_pairs_target = 100L), keep = "full")
    expect_identical(new$theta, old$theta)
    expect_identical(new$draws, old$draws)
    expect_identical(new$replicates, old$replicates)
  }
  run <- function(fun) {
    tryCatch(fun(c(a = 0, b = 0), "fixed", 3L, 18L,
      schedule = bootstrap_data(1L, 1L), estimator = "alpha", estimator_args = list(alpha = 0), keep = "full"),
      pairwiseLLM_bt_bootstrap_error = function(e) e$result)
  }
  expect_identical(run(bootstrap_bt_model), run(reference$bootstrap_bt_model))
  expect_identical(.Random.seed, before)
})
