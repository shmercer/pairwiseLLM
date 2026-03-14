golden_two_set_items <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
}

golden_score_judge <- function(scores) {
  score_names <- names(scores)
  scores <- as.double(scores)
  names(scores) <- score_names
  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    y <- as.integer(scores[[a]] >= scores[[b]])
    list(is_valid = TRUE, Y = y, invalid_reason = NA_character_)
  }
}

golden_import_artifacts <- function(state, spoke_shift = -0.8) {
  ids <- as.character(state$item_ids)
  draws <- matrix(seq_along(ids), nrow = 6L, ncol = length(ids), byrow = TRUE)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  out <- lapply(c(1L, 2L), function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = set_id)
    if (set_id == 2L) {
      art$items$theta_raw_mean <- as.double(art$items$theta_raw_mean + spoke_shift)
    }
    art$quality_gate_accepted <- TRUE
    art
  })
  names(out) <- c("1", "2")
  out
}

golden_e2e_run <- function() {
  out <- withr::with_seed(20260217, {
    items <- golden_two_set_items()
    base <- adaptive_rank_start(items, seed = 313L)
    base$warm_start_done <- TRUE
    base$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
    artifacts <- golden_import_artifacts(base, spoke_shift = -0.8)
    fit_stub <- make_deterministic_fit_fn(as.character(base$item_ids))
    judge <- golden_score_judge(c(h1 = -0.6, h2 = 0.1, h3 = 0.8, s21 = -0.7, s22 = 0.0, s23 = 0.9))

    out_a <- adaptive_rank_run_live(
      state = base,
      judge = judge,
      n_steps = 12L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "import",
        phase_a_artifacts = artifacts,
        link_transform_policy = "auto",
        link_stop_reliability_min = 0.0,
        link_rank_corr_min = 0.0,
        delta_sd_max = 100,
        delta_change_max = 100,
        probe_pairs_per_refit_per_spoke = 2L
      ),
      btl_config = test_link_btl_config(list(
        refit_pairs_target = 1L,
        stability_lag = 1L,
        eap_reliability_min = 0.0,
        theta_corr_min = 0.0,
        rank_spearman_min = 0.0
      )),
      progress = "none"
    )

    out_b <- pairwiseLLM:::.adaptive_link_apply_stop_state(
      out_a,
      tibble::tibble(
        refit_id = as.integer(nrow(out_a$round_log) + 1L),
        spoke_id = 2L,
        link_stop_pass = TRUE,
        link_transform_state = "shift_only",
        delta_spoke_mean = 0.0,
        log_alpha_spoke_mean = NA_real_
      )
    )
    adaptive_rank_run_live(
      state = out_b,
      judge = judge,
      n_steps = 18L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = list(
        run_mode = "link_one_spoke",
        hub_id = 1L,
        phase_a_mode = "import",
        phase_a_artifacts = artifacts,
        link_transform_policy = "auto",
        link_stop_reliability_min = 0.0,
        link_rank_corr_min = 0.0,
        delta_sd_max = 100,
        delta_change_max = 100,
        probe_pairs_per_refit_per_spoke = 2L
      ),
      btl_config = test_link_btl_config(list(
        refit_pairs_target = 1L,
        stability_lag = 1L,
        eap_reliability_min = 0.0,
        theta_corr_min = 0.0,
        rank_spearman_min = 0.0
      )),
      progress = "none"
    )
  })

  step_focus <- out$step_log[, c(
    "step_id", "run_mode", "is_probe_step", "is_cross_set", "link_spoke_id",
    "round_stage", "link_stage", "utility_mode"
  ), drop = FALSE]
  link_focus <- out$link_stage_log[, c(
    "refit_id", "spoke_id", "transform_frozen", "link_stop_eligible", "link_stop_pass",
    "n_cross_edges_active_since_last_refit", "n_cross_edges_probe_since_last_refit"
  ), drop = FALSE]

  list(
    state = out,
    step_focus = step_focus,
    link_focus = link_focus
  )
}

test_that("deterministic linking e2e run preserves canonical golden logs", {
  run <- golden_e2e_run()

  expect_true(any(run$state$step_log$is_cross_set %in% TRUE))

  # After the Phase B starvation fix, the last active spoke is no longer
  # retired wholesale on a single global-safe miss. This deterministic golden
  # now includes the final pooled_backfill starvation step before the run ends
  # with all spokes stopped.
  fixture_path <- testthat::test_path("fixtures", "linking-e2e-golden.rds")
  expect_true(file.exists(fixture_path))
  fixture <- readRDS(fixture_path)

  expect_equal(run$step_focus, fixture$step_focus)
  expect_equal(run$link_focus, fixture$link_focus)
})
