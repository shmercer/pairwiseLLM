matrix_two_set_items <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23")
  )
}

matrix_three_set_items <- function() {
  tibble::tibble(
    item_id = c("h1", "h2", "h3", "s21", "s22", "s23", "s31", "s32", "s33"),
    set_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    global_item_id = c("gh1", "gh2", "gh3", "gs21", "gs22", "gs23", "gs31", "gs32", "gs33")
  )
}

matrix_score_judge <- function(scores) {
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

matrix_import_artifacts <- function(state, spoke_shift = -1.0) {
  ids <- as.character(state$item_ids)
  draws <- matrix(seq_along(ids), nrow = 4L, ncol = length(ids), byrow = TRUE)
  colnames(draws) <- ids
  state$btl_fit <- make_test_btl_fit(ids, draws = draws, model_variant = "btl_e_b")
  set_ids <- sort(unique(as.integer(state$items$set_id)))
  out <- lapply(set_ids, function(set_id) {
    art <- pairwiseLLM:::.adaptive_phase_a_build_artifact(state, set_id = as.integer(set_id))
    if (!identical(as.integer(set_id), 1L)) {
      art$items$theta_raw_mean <- as.double(art$items$theta_raw_mean + spoke_shift)
    }
    art$quality_gate_accepted <- TRUE
    art
  })
  names(out) <- as.character(set_ids)
  out
}

test_that("regression matrix smoke covers baseline/linking modes and resume paths", {
  withr::local_seed(20260214)

  scenarios <- list(
    list(
      name = "single_set",
      items = make_test_items(8),
      adaptive_config = list(run_mode = "within_set"),
      linking = FALSE
    ),
    list(
      name = "link_one_spoke",
      items = matrix_two_set_items(),
      adaptive_config = list(run_mode = "link_one_spoke", hub_id = 1L, phase_a_mode = "import"),
      linking = TRUE
    ),
    list(
      name = "link_multi_independent",
      items = matrix_three_set_items(),
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        multi_spoke_mode = "independent",
        phase_a_mode = "import"
      ),
      linking = TRUE
    ),
    list(
      name = "link_multi_concurrent",
      items = matrix_three_set_items(),
      adaptive_config = list(
        run_mode = "link_multi_spoke",
        hub_id = 1L,
        multi_spoke_mode = "concurrent",
        min_cross_set_pairs_per_spoke_per_refit = 1L,
        phase_a_mode = "import"
      ),
      linking = TRUE
    )
  )

  for (sc in scenarios) {
    session_dir <- file.path(withr::local_tempdir(), sc$name)
    state <- adaptive_rank_start(sc$items, seed = 42L)
    fit_stub <- make_deterministic_fit_fn(as.character(state$item_ids))
    judge <- if (isTRUE(sc$linking)) {
      matrix_score_judge(stats::setNames(seq_len(nrow(sc$items)), as.character(sc$items$item_id)))
    } else {
      make_deterministic_judge("i_wins")
    }

    cfg <- sc$adaptive_config
    if (isTRUE(sc$linking)) {
      state$warm_start_done <- TRUE
      state$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
      cfg$phase_a_artifacts <- matrix_import_artifacts(state, spoke_shift = -1)
    }

    first <- adaptive_rank_run_live(
      state = state,
      judge = judge,
      n_steps = 8L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = cfg,
      btl_config = list(refit_pairs_target = 1L),
      session_dir = session_dir,
      progress = "none"
    )
    save_adaptive_session(first, session_dir = session_dir, overwrite = TRUE)
    resumed <- adaptive_rank_resume(session_dir = session_dir)
    prev <- resumed$step_log

    second <- adaptive_rank_run_live(
      state = resumed,
      judge = judge,
      n_steps = 4L,
      fit_fn = fit_stub$fit_fn,
      adaptive_config = cfg,
      btl_config = list(refit_pairs_target = 1L),
      session_dir = session_dir,
      progress = "none"
    )
    expect_true(nrow(second$step_log) >= nrow(prev))
    expect_equal(second$step_log[seq_len(nrow(prev)), , drop = FALSE], prev)

    if (isTRUE(sc$linking)) {
      expect_true(any(second$step_log$is_cross_set %in% TRUE))
      expect_true(nrow(second$link_stage_log) >= 1L)
    }
  }
})

test_that("phase A workflow matrix executes run/import/mixed paths", {
  withr::local_seed(20260214)
  items <- matrix_two_set_items()
  judge <- matrix_score_judge(c(h1 = -0.4, h2 = 0.0, h3 = 0.6, s21 = -0.3, s22 = 0.2, s23 = 0.8))
  base <- adaptive_rank_start(items, seed = 91L)
  base$warm_start_done <- TRUE
  base$warm_start_pairs <- tibble::tibble(i_id = character(), j_id = character())
  artifacts <- matrix_import_artifacts(base, spoke_shift = -1)
  fit_stub <- make_deterministic_fit_fn(as.character(base$item_ids))

  # import
  out_import <- adaptive_rank_run_live(
    state = base,
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "import",
      phase_a_artifacts = artifacts
    ),
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )
  status_import <- tibble::as_tibble(out_import$linking$phase_a$set_status)
  expect_true(all(status_import$source == "import"))

  # run
  out_run <- adaptive_rank_run_live(
    state = adaptive_rank_start(items, seed = 92L),
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "run"
    ),
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )
  status_run <- tibble::as_tibble(out_run$linking$phase_a$set_status)
  expect_true(all(status_run$source == "run"))

  # mixed
  out_mixed <- adaptive_rank_run_live(
    state = adaptive_rank_start(items, seed = 93L),
    judge = judge,
    n_steps = 10L,
    fit_fn = fit_stub$fit_fn,
    adaptive_config = list(
      run_mode = "link_one_spoke",
      hub_id = 1L,
      phase_a_mode = "mixed",
      phase_a_set_source = c(`1` = "import", `2` = "run"),
      phase_a_artifacts = list(`1` = artifacts[["1"]]),
      phase_a_compatible_config_hashes = artifacts[["1"]]$fit_config_hash
    ),
    btl_config = list(refit_pairs_target = 1L),
    progress = "none"
  )
  status_mixed <- tibble::as_tibble(out_mixed$linking$phase_a$set_status)
  expect_identical(status_mixed$source[match(1L, status_mixed$set_id)], "import")
  expect_identical(status_mixed$source[match(2L, status_mixed$set_id)], "run")
})
