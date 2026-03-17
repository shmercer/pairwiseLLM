golden_two_set_items <- function() {
  hub_ids <- paste0("h", seq_len(10L))
  spoke_ids <- paste0("s2", seq_len(6L))
  tibble::tibble(
    item_id = c(hub_ids, spoke_ids),
    set_id = c(rep(1L, length(hub_ids)), rep(2L, length(spoke_ids))),
    global_item_id = c(paste0("g", hub_ids), paste0("g", spoke_ids))
  )
}

golden_score_judge <- function(scores) {
  score_names <- names(scores)
  scores <- as.double(scores)
  names(scores) <- score_names
  default_score <- function(item_id) {
    item_id <- as.character(item_id)
    if (grepl("^h\\d+$", item_id)) {
      rank <- as.integer(sub("^h", "", item_id))
      return(-1.0 + (0.16 * rank))
    }
    if (grepl("^s\\d\\d+$", item_id)) {
      set_id <- as.integer(substr(item_id, 2L, 2L))
      rank <- as.integer(sub("^s\\d", "", item_id))
      return((0.1 * set_id) + (0.22 * rank))
    }
    0
  }
  function(A, B, state, ...) {
    a <- as.character(A$item_id[[1L]])
    b <- as.character(B$item_id[[1L]])
    a_score <- scores[a]
    b_score <- scores[b]
    a_score <- if (!is.na(a_score)) as.double(a_score) else default_score(a)
    b_score <- if (!is.na(b_score)) as.double(b_score) else default_score(b)
    y <- as.integer(a_score >= b_score)
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
    "refit_id", "spoke_id", "link_state_frozen", "link_stop_eligible", "link_stop_pass",
    "n_cross_edges_active_since_last_refit", "n_cross_edges_probe_since_last_refit"
  ), drop = FALSE]

  list(
    state = out,
    step_focus = step_focus,
    link_focus = link_focus
  )
}

run_golden_e2e_in_clean_r <- function() {
  pkg_root <- normalizePath(
    testthat::test_path("..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  helper_path <- normalizePath(
    testthat::test_path("helper-fixtures.R"),
    winslash = "/",
    mustWork = TRUE
  )
  test_path <- normalizePath(
    testthat::test_path("test-9004-linking-e2e-golden.R"),
    winslash = "/",
    mustWork = TRUE
  )
  out_path <- tempfile(fileext = ".rds")
  script_path <- tempfile(fileext = ".R")

  script_lines <- c(
    sprintf("pkgload::load_all(path = %s, quiet = TRUE)", shQuote(pkg_root)),
    sprintf("source(%s)", shQuote(helper_path)),
    sprintf("lines <- readLines(%s)", shQuote(test_path)),
    "test_start <- grep('^test_that\\\\(', lines)[1] - 1L",
    "eval(parse(text = paste(lines[seq_len(test_start)], collapse = '\\n')))",
    sprintf(
      paste0(
        "run <- golden_e2e_run(); ",
        "saveRDS(list(step_focus = run$step_focus, link_focus = run$link_focus), %s)"
      ),
      shQuote(out_path)
    )
  )
  writeLines(script_lines, script_path)

  output <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = script_path,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- as.integer(attr(output, "status") %||% 0L)
  if (!identical(status, 0L)) {
    rlang::abort(paste(c(
      "Clean-session golden run failed:",
      output
    ), collapse = "\n"))
  }

  readRDS(out_path)
}

test_that("deterministic linking e2e run preserves canonical golden logs", {
  run <- run_golden_e2e_in_clean_r()

  # After the Phase B starvation/runtime alignment work, this deterministic
  # trace includes the last committed anchor-link step before the final
  # pooled_backfill starvation step. The prior golden was missing that
  # committed Phase B step/refit window and is no longer canonical.
  fixture_path <- testthat::test_path("fixtures", "linking-e2e-golden.rds")
  expect_true(file.exists(fixture_path))
  fixture <- readRDS(fixture_path)

  expect_equal(run$step_focus, fixture$step_focus)
  expect_equal(run$link_focus, fixture$link_focus)
})
