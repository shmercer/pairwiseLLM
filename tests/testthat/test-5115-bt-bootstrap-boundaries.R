test_that("adaptive bootstrap rejects mismatched, fitted, and tampered initial states", {
  s <- bootstrap_state()
  invalid <- list(NULL, list(), bootstrap_state(ids = letters[1:5]))
  x <- s
  x$controller$run_mode <- "link_one_spoke"
  invalid <- c(invalid, list(x))
  for (x in invalid) {
    expect_error(bootstrap_bt_model(setNames(rep(0, 6), letters[1:6]), "adaptive", 2, 1,
      initial_state = x, budget = 6, estimator = "alpha", estimator_args = list(alpha = .5)), "within-set")
  }
  done <- adaptive_rank_run_live(s, make_deterministic_judge("i_wins"), n_steps = 1L, progress = "none")
  expect_error(bootstrap_adaptive(done), "pristine")
  for (field in c("trueskill_state", "history_state", "refit_meta", "round")) {
    x <- s
    x[[field]] <- done[[field]]
    expect_error(bootstrap_adaptive(x), "updated|pristine")
  }
  x <- s
  x$warm_start_pairs <- x$warm_start_pairs[-1, ]
  expect_error(bootstrap_adaptive(x), "N - 1")
  x <- s
  x$warm_start_pairs[2:5, ] <- x$warm_start_pairs[rep(1, 4), ]
  expect_error(bootstrap_adaptive(x), "disconnected")
  expect_error(bootstrap_adaptive(s, schedule = bootstrap_data()), "not a frozen schedule")
  expect_error(bootstrap_adaptive(s, budget = 4), "budget")
  expect_error(bootstrap_adaptive(s, schedule_fit_fn = 1), "function")
  expect_error(bootstrap_adaptive(s, btl_config = 1), "config")
  for (bad in list(1.5, Inf, NA_real_, 0L)) {
    expect_error(bootstrap_adaptive(s, btl_config = list(refit_pairs_target = bad)), "refit_pairs_target")
  }
})

test_that("scheduled fitter errors fail explicitly and pass deterministic engine seeds", {
  fitter <- function(state, config) {
    rlang::abort(paste("seed", config$cmdstan$seed), diagnostics = list(seed = config$cmdstan$seed))
  }
  out <- bootstrap_error(bootstrap_adaptive(n_rep = 2L, budget = 8L,
    btl_config = list(refit_pairs_target = 6L), schedule_fit_fn = fitter, keep = "full"))$result
  expect_identical(out$n_failed, 2L)
  expect_true(all(out$replicates$failure_phase == "schedule"))
  for (i in 1:2) {
    expected <- pairwiseLLM:::.adaptive_stage_seed(out$replicates$scheduling_refit[i], 6L, 1L)
    expect_identical(out$replicates$message[i], paste("seed", expected))
    expect_identical(out$failures[[i]]$diagnostics$seed, expected)
  }
})

test_that("default scheduled refits are preserved and never silently suppressed", {
  testthat::local_mocked_bindings(default_btl_fit_fn = function(...) stop("canonical fitter requested"),
    .package = "pairwiseLLM")
  out <- bootstrap_error(bootstrap_adaptive(n_rep = 2L, budget = 6L,
    btl_config = list(refit_pairs_target = 6L)))$result
  expect_true(all(out$replicates$message == "canonical fitter requested"))
  expect_no_error(bootstrap_adaptive(n_rep = 2L, budget = 5L,
    btl_config = list(refit_pairs_target = 6L)))
})

test_that("bootstrap normalizes persistence without writing to an input session", {
  s <- bootstrap_state()
  s$config$session_dir <- file.path(withr::local_tempdir(), "must-not-write")
  s$config$persist_item_log <- TRUE
  s$config$btl_config <- list(refit_pairs_target = 100L)
  out <- bootstrap_adaptive(s, n_rep = 2L)
  expect_false(dir.exists(s$config$session_dir))
  expect_null(out$provenance$initial_state$config$session_dir)
  expect_false(out$provenance$initial_state$config$persist_item_log)
  expect_identical(out$provenance$btl_config$refit_pairs_target, 100L)
})

test_that("canonical signed initialization seeds and item metadata remain intact", {
  items <- data.frame(item_id = letters[1:4], custom_covariate = 1:4)
  s <- adaptive_rank_start(items, seed = -17L, adaptive_config = list(pairing_strategy = "trueskill_p50"))
  out <- bootstrap_adaptive(s, n_rep = 2L, budget = 3L, btl_config = list(refit_pairs_target = 100L))
  expect_identical(out$provenance$initial_state$meta$initialization_seed, -17L)
  expect_identical(out$provenance$initial_state$items, s$items)
  expect_identical(out$provenance$initial_state$warm_start_pairs, s$warm_start_pairs)
})
