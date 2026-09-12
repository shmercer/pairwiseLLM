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
