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
  out <- make_positive_probe_acceleration_runtime_state()

  step_focus <- out$step_log[, c(
    "step_id", "run_mode", "is_probe_step", "is_cross_set", "link_spoke_id",
    "round_stage", "link_stage", "fallback_used", "candidate_starved", "utility_mode"
  ), drop = FALSE]
  link_focus <- out$link_stage_log[, c(
    "refit_id", "spoke_id", "link_state_frozen", "link_stop_eligible", "link_stop_pass",
    "n_cross_edges_active_since_last_refit", "n_cross_edges_probe_since_last_refit",
    "probe_acceleration_mode_used", "probe_active_floor_used", "probe_only_blocker_trigger",
    "probe_acceleration_used", "probe_effort_base_cap", "probe_effort_effective_cap",
    "probe_remaining_to_min_start"
  ), drop = FALSE]

  list(
    state = out,
    step_focus = step_focus,
    link_focus = link_focus
  )
}

run_golden_e2e_in_clean_r <- function() {
  as_r_string <- function(path) {
    encodeString(as.character(path), quote = "\"")
  }

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
  out_path <- normalizePath(out_path, winslash = "/", mustWork = FALSE)

  script_lines <- c(
    sprintf("pkg_root <- %s", as_r_string(pkg_root)),
    paste(
      "use_load_all <-",
      "requireNamespace(\"pkgload\", quietly = TRUE) &&",
      "file.exists(file.path(pkg_root, \"R\", \"adaptive_step.R\"))"
    ),
    "if (isTRUE(use_load_all)) {",
    "  pkgload::load_all(path = pkg_root, quiet = TRUE)",
    "} else {",
    "  library(pairwiseLLM)",
    "}",
    sprintf("source(%s)", as_r_string(helper_path)),
    sprintf("lines <- readLines(%s)", as_r_string(test_path)),
    "test_start <- grep('^test_that\\\\(', lines)[1] - 1L",
    "eval(parse(text = paste(lines[seq_len(test_start)], collapse = '\\n')))",
    sprintf(
      paste0(
        "run <- golden_e2e_run(); ",
        "saveRDS(list(step_focus = run$step_focus, link_focus = run$link_focus), %s)"
      ),
      as_r_string(out_path)
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

  # The golden covers the fixed-per-refit held-out probe schedule and the
  # canonical audit rows emitted by the current deterministic Phase B runtime.
  fixture_path <- testthat::test_path("fixtures", "linking-e2e-golden.rds")
  expect_true(file.exists(fixture_path))
  fixture <- readRDS(fixture_path)

  expect_equal(run$step_focus, fixture$step_focus)
  expect_equal(run$link_focus, fixture$link_focus)
})
