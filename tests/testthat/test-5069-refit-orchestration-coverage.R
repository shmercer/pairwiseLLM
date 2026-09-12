test_that("spoke refit scheduling merges the same deterministic state as sequential updates", {
  skip_on_os("windows")
  state <- .adaptive_anchored_joint_sync_scaffolding(task09_link_state())
  context <- list(last_refit_step = 0L)
  sequential <- .adaptive_linking_refit_update_state(state, context)
  calls <- list()
  testthat::local_mocked_bindings(mclapply = function(X, FUN, mc.cores, mc.set.seed) {
    calls[[length(calls) + 1L]] <<- list(spokes = X, workers = mc.cores)
    lapply(X, FUN)
  }, .package = "parallel")
  state$config$btl_config$phase_b_refit_parallel <- TRUE
  state$config$btl_config$phase_b_refit_workers <- 2L
  parallel_state <- .adaptive_linking_refit_update_state(state, context)
  expect_identical(calls[[1]], list(spokes = 2:3, workers = 2L))
  expect_equal(parallel_state$linking$anchored_joint, sequential$linking$anchored_joint)
  for (spoke in c("2", "3")) {
    expect_equal(parallel_state$controller$link_refit_stats_by_spoke[[spoke]]$reliability_link_global,
      sequential$controller$link_refit_stats_by_spoke[[spoke]]$reliability_link_global)
  }
  expect_identical(parallel_state$history_pairs, state$history_pairs)
  expect_identical(parallel_state$trueskill_state, state$trueskill_state)
})

test_that("failed spoke workers cannot be merged into the authoritative state", {
  skip_on_os("windows")
  state <- task09_link_state()
  state$config$btl_config <- list(phase_b_refit_parallel = TRUE, phase_b_refit_workers = 2L)
  testthat::local_mocked_bindings(mclapply = function(...) {
    list(structure("worker failure", class = "try-error"), state)
  }, .package = "parallel")
  before <- state
  expect_error(.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
    "parallel post-refit update failed for spoke.*2")
  expect_identical(state, before)
})

test_that("anchored refits preserve frozen states, legacy maps, and typed audit fields", {
  withr::local_seed(91L)
  rng <- .Random.seed
  for (n_sets in 2:3) {
    state <- task10_link_state(n_sets)
    state$controller$link_transform_frozen_delta_by_spoke <- list(`2` = 0.7)
    state$controller$link_transform_frozen_log_alpha_by_spoke <- list(`2` = 0.2)
    update <- function(s) .adaptive_linking_refit_update_state(s, list(last_refit_step = 0L))
    out <- update(state)
    expect_identical(out$history_pairs, state$history_pairs)
    expect_identical(out$trueskill_state, state$trueskill_state)
    expect_identical(out$controller$link_transform_frozen_delta_by_spoke, list(`2` = 0.7))
    expect_identical(out$controller$link_transform_frozen_log_alpha_by_spoke, list(`2` = 0.2))
    accepted <- out$linking$anchored_joint$accepted_state_by_spoke[["2"]]
    expect_equal(accepted$theta_hub_fixed, c(a = -1, b = 1))
    expect_true(all(is.finite(accepted$theta_spoke_global_mean)))
    expect_true(all(accepted$theta_spoke_global_sd > 0))
    stats <- out$controller$link_refit_stats_by_spoke[["2"]]
    expect_identical(stats$link_transform_state, NA_character_)
    expect_identical(stats$alternative_fit_method, NA_character_)
    expect_identical(stats$alt_eval_active_edges, NA_integer_)
    expect_identical(stats$alt_eval_converged, FALSE)
    expect_identical(stats$escalated_this_refit, FALSE)
    expect_identical(stats$scale_ready, FALSE)
    expect_true(stats$hub_anchored)
    expect_false(stats$link_stop_eligible)

    frozen <- out
    frozen$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
    frozen_out <- update(frozen)
    expect_identical(frozen_out$linking$anchored_joint$accepted_state_by_spoke[["2"]], accepted)
    expect_identical(frozen_out$controller$link_refit_stats_by_spoke[["2"]], stats)
    expect_false(2L %in% .adaptive_link_ranked_spokes(frozen_out, frozen_out$controller))
    expect_identical(.adaptive_link_probe_next_holdout_spoke(frozen_out,
      frozen_out$controller, eligible_spoke_ids = 2L), NA_integer_)

    reuse <- state
    reuse$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
    reused <- update(reuse)
    expect_identical(reused$controller$link_refit_stats_by_spoke[["2"]]$link_fit_method,
      "accepted_state_reuse")
    expect_equal(reused$linking$anchored_joint$accepted_state_by_spoke[["2"]]$theta_spoke_global_mean,
      state$linking$anchored_joint$accepted_state_by_spoke[["2"]]$theta_spoke_global_mean)

    broken <- state
    broken$linking$phase_a$artifacts[["2"]] <- NULL
    expect_error(update(broken), "artifact|Phase A")
    expect_identical(broken$history_pairs, state$history_pairs)
  }
  expect_identical(.Random.seed, rng)
})
