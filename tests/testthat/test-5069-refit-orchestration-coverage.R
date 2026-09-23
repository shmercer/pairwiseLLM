

test_that("parallel settings cannot enable adaptive Phase B estimator fallback", {
  state <- task09_link_state()
  for (parallel in c(FALSE, TRUE)) {
    state$config$btl_config$phase_b_refit_parallel <- parallel
    state$config$btl_config$phase_b_refit_workers <- 2L
    expect_error(.adaptive_linking_refit_update_state(state, list(last_refit_step = 0L)),
      class = "pairwiseLLM_link_selector_unvalidated")
  }
})
