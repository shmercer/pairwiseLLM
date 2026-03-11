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
  expect_true(any(grepl("chains=", combined)))
  expect_true(any(grepl("parallel_chains=", combined)))
  expect_true(any(grepl("cov_trace_theta=", combined)))
  expect_true(any(grepl("Decision:", combined)))
  expect_true(any(grepl("\\[x\\]|\\[ \\]", combined)))
  expect_true(any(grepl("^step [0-9]+: new_pairs_since_last_refit=", combined)))
})

test_that("adaptive_rank_run_live prints linking-specific refit summary lines", {
  items <- tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
  state <- adaptive_rank_start(items = items, seed = 7L)
  ids <- as.character(state$item_ids)
  draws <- matrix(seq_along(ids), nrow = 4L, ncol = length(ids), byrow = TRUE)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  artifacts <- lapply(sort(unique(items$set_id)), function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
    art$quality_gate_accepted <- TRUE
    art
  })
  names(artifacts) <- as.character(sort(unique(items$set_id)))

  judge <- function(A, B, state, ...) {
    y <- as.integer(A$item_id[[1L]] >= B$item_id[[1L]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }
  stub <- make_deterministic_fit_fn(state$item_ids)

  messages <- character()
  output <- capture.output({
    withCallingHandlers(
      {
        withr::local_seed(4)
        state <- adaptive_rank_run_live(
          state,
          judge,
          n_steps = 10L,
          fit_fn = stub$fit_fn,
          adaptive_config = list(
            run_mode = "link_one_spoke",
            hub_id = 1L,
            phase_a_mode = "import",
            phase_a_artifacts = artifacts
          ),
          btl_config = list(refit_pairs_target = 2L, stability_lag = 1L),
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
  expect_true(any(grepl("Linking summary:", combined)))
  expect_true(any(grepl("active_spokes=", combined)))
  expect_true(any(grepl("transform_policy=", combined)))
  expect_true(any(grepl("transform_state=", combined)))
  expect_true(any(grepl("link_epoch_id=", combined)))
  expect_true(any(grepl("authoritative_link_fit_method=", combined)))
  expect_true(any(grepl("authoritative_link_uncertainty=", combined)))
  expect_true(any(grepl("link_refit_mode=", combined)))
  expect_true(any(grepl("cross_pairs_done=", combined)))
  expect_true(any(grepl("link_stop_pass=", combined)))
  expect_true(any(grepl("feasibility_budget_released=", combined)))
  expect_true(any(grepl("dominant_blocker=", combined)))
  expect_true(any(grepl("feasibility_capacity\\[a,l,m,loc\\]=", combined)))
  expect_true(any(grepl("blocker_weights\\[probe_shortfall,probe_brier,probe_rmse,theta_rmse,delta_sd\\]=", combined)))
  expect_true(any(grepl("reliability_link_global\\[min,max\\]=", combined)))
  expect_false(any(grepl("reliability_EAP_link\\[min,max\\]=", combined)))
})

test_that("adaptive progress step events label holdout and drift probes distinctly", {
  cfg <- pairwiseLLM:::.adaptive_progress_config(
    progress = "all",
    progress_redraw_every = 1L,
    progress_show_events = TRUE,
    progress_errors = TRUE
  )
  holdout <- tibble::tibble(
    step_id = 1L,
    round_stage = "local_link",
    run_mode = "link_probe_holdout",
    is_probe_step = TRUE,
    is_holdout_probe_step = TRUE,
    is_drift_probe_step = FALSE,
    is_cross_set = TRUE,
    link_spoke_id = 2L,
    link_transform_state = "shift_only",
    candidate_starved = FALSE,
    status = "ok",
    fallback_used = "refresh"
  )
  drift <- holdout
  drift$step_id <- 2L
  drift$run_mode <- "link_probe"
  drift$is_holdout_probe_step <- FALSE
  drift$is_drift_probe_step <- TRUE

  holdout_msg <- pairwiseLLM:::adaptive_progress_step_event(holdout, cfg)
  drift_msg <- pairwiseLLM:::adaptive_progress_step_event(drift, cfg)

  expect_match(holdout_msg, "probe=holdout")
  expect_match(drift_msg, "probe=drift_followup")
})

test_that("adaptive progress step events distinguish active linking from probe follow-up", {
  cfg <- pairwiseLLM:::.adaptive_progress_config(
    progress = "all",
    progress_redraw_every = 1L,
    progress_show_events = TRUE,
    progress_errors = TRUE
  )
  active <- tibble::tibble(
    step_id = 3L,
    round_stage = "mid_link",
    run_mode = "link_one_spoke",
    is_probe_step = FALSE,
    is_holdout_probe_step = FALSE,
    is_drift_probe_step = FALSE,
    is_cross_set = TRUE,
    link_spoke_id = 2L,
    link_transform_state = "shift_only",
    candidate_starved = FALSE,
    status = "ok",
    fallback_used = "refresh"
  )

  active_msg <- pairwiseLLM:::adaptive_progress_step_event(active, cfg)

  expect_match(active_msg, "link=active")
  expect_false(grepl("probe=", active_msg))
})
