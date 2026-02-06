test_that("adaptive_rank_run_live prints refit blocks and stop criteria", {
  items <- make_test_items(4)
  state <- adaptive_rank_start(items)
  judge <- make_deterministic_judge("i_wins")
  stub <- make_deterministic_fit_fn(state$item_ids)

  btl_config <- list(refit_pairs_target = 2L, stability_lag = 1L)

  messages <- character()
  output <- capture.output({
    withCallingHandlers(
      {
        withr::local_seed(1)
        state <- adaptive_rank_run_live(
          state,
          judge,
          n_steps = 3L,
          fit_fn = stub$fit_fn,
          btl_config = btl_config,
          progress = "all",
          progress_redraw_every = 1L
        )
      },
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })

  combined <- c(output, messages)
  expect_true(any(grepl("REFIT #", combined)))
  expect_true(any(grepl("round_id", combined)))
  expect_true(any(grepl("step_id_at_refit", combined)))
  expect_true(any(grepl("total_pairs_done", combined)))
  expect_true(any(grepl("diagnostics_pass", combined)))
  expect_true(any(grepl("min_ess_bulk", combined)))
  expect_true(any(grepl("Decision:", combined)))
  expect_true(any(grepl("\\[x\\]|\\[ \\]", combined)))
  expect_true(any(grepl("^step [0-9]+: new_pairs_since_last_refit=", combined)))
})
