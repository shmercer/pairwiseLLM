make_adaptive_rank_covr_artifact <- function(set_id,
                                             item_ids = NULL,
                                             theta = NULL) {
  item_ids <- as.character(item_ids %||% paste0("g", set_id, "_", seq_len(2L)))
  theta <- as.double(theta %||% seq_along(item_ids))

  list(
    set_id = as.integer(set_id),
    diagnostics = list(
      diagnostics_pass = TRUE,
      reliability_EAP_within = 0.95
    ),
    n_pairs_committed = 4L,
    quality_gate_accepted = TRUE,
    items = tibble::tibble(
      global_item_id = item_ids,
      theta_raw_mean = theta,
      theta_raw_sd = rep(0.1, length(item_ids)),
      rank_mu_raw = seq_along(item_ids)
    )
  )
}

test_that("adaptive rank phase-a manifest and collection helpers cover fallback branches", {
  manifest <- pairwiseLLM:::.adaptive_rank_phase_a_manifest(
    artifacts = 1L,
    set_status = tibble::tibble(set_id = 1L),
    session_dir = "session-dir",
    artifact_dir = "artifact-dir",
    artifact_paths = c(`1` = "artifact-1.rds")
  )

  expect_s3_class(manifest, "adaptive_phase_a_manifest")
  expect_identical(length(manifest), 0L)
  expect_identical(attr(manifest, "session_dir"), "session-dir")
  expect_identical(attr(manifest, "artifact_dir"), "artifact-dir")
  expect_identical(attr(manifest, "artifact_paths"), "artifact-1.rds")

  state <- list(
    items = tibble::tibble(set_id = c(1L, 2L)),
    linking = list(
      phase_a = list(
        artifacts = list(
          `1` = make_adaptive_rank_covr_artifact(1L),
          bad = list(set_id = NA_integer_)
        )
      )
    )
  )

  collected <- testthat::with_mocked_bindings(
    .adaptive_phase_a_build_artifact = function(state, set_id) {
      if (identical(as.integer(set_id), 2L)) {
        return(make_adaptive_rank_covr_artifact(2L))
      }
      NULL
    },
    pairwiseLLM:::.adaptive_rank_collect_phase_a_artifacts(
      state,
      set_ids = c(1L, 2L)
    ),
    .package = "pairwiseLLM"
  )

  expect_setequal(names(collected$artifacts), c("1", "2"))
  expect_identical(as.integer(collected$artifacts[["2"]]$set_id), 2L)
  expect_identical(collected$errors, list())
})

test_that("adaptive rank phase-a surface derives canonical defaults from runtime artifacts", {
  artifact_dir <- withr::local_tempdir()
  filenames <- stats::setNames(
    paste0("phase-a-set-", c(1L, 2L, 3L, 4L), ".rds"),
    c("1", "2", "3", "4")
  )
  for (path in file.path(artifact_dir, filenames)) {
    saveRDS(list(), path)
  }

  state <- list(
    items = tibble::tibble(
      set_id = c(1L, 2L, 3L, 4L)
    ),
    linking = list(
      phase_a = list(
        set_status = tibble::tibble(set_id = 1L)
      )
    )
  )

  surface <- suppressWarnings(
    testthat::with_mocked_bindings(
      .adaptive_controller_resolve = function(state) {
        list(run_mode = "link_multi_spoke")
      },
      .adaptive_rank_collect_phase_a_artifacts = function(state, set_ids = NULL) {
        list(
          artifacts = list(
            `1` = make_adaptive_rank_covr_artifact(1L),
            `2` = make_adaptive_rank_covr_artifact(2L)
          ),
          errors = list(
            `3` = "Within-set summaries are unavailable for set 3.",
            `4` = "artifact build failed"
          )
        )
      },
      .adaptive_rank_phase_a_artifact_dir = function(session_dir) artifact_dir,
      .adaptive_write_phase_a_artifacts = function(artifacts, artifact_dir) invisible(NULL),
      .adaptive_phase_a_empty_state = function(set_ids = integer()) {
        tibble::tibble(
          set_id = as.integer(set_ids),
          source = rep(NA_character_, length(set_ids)),
          status = rep(NA_character_, length(set_ids)),
          validation_message = rep(NA_character_, length(set_ids)),
          artifact_path = rep(NA_character_, length(set_ids))
        )
      },
      .adaptive_phase_a_set_stop_passed = function(artifact, source, controller) {
        identical(as.integer(artifact$set_id), 1L)
      },
      .adaptive_phase_a_artifact_filename = function(set_id) filenames[[as.character(set_id)]],
      pairwiseLLM:::.adaptive_rank_phase_a_surface(
        state,
        session_dir = "session-dir"
      ),
      .package = "pairwiseLLM"
    )
  )

  expect_identical(surface$set_status$source, rep("run", 4L))
  expect_identical(surface$set_status$status, c(
    "ready",
    "pending_finalization",
    "pending_finalization",
    "failed"
  ))
  expect_identical(surface$set_status$validation_message, c(
    "wrapper_discovered",
    "pending_finalization: within-set stop criteria not yet met",
    "Within-set summaries are unavailable for set 3.",
    "artifact build failed"
  ))
  expect_true(all(file.exists(stats::na.omit(surface$set_status$artifact_path))))
  expect_s3_class(surface$manifest, "adaptive_phase_a_manifest")
})

test_that("adaptive rank phase-a surface coerces blank persisted source to run", {
  artifact_dir <- withr::local_tempdir()
  artifact_path <- file.path(artifact_dir, "phase-a-set-1.rds")
  saveRDS(list(), artifact_path)

  state <- list(
    items = tibble::tibble(set_id = 1L),
    linking = list(
      phase_a = list(
        set_status = tibble::tibble(
          set_id = 1L,
          source = "",
          status = NA_character_,
          validation_message = NA_character_,
          artifact_path = NA_character_
        )
      )
    )
  )

  surface <- testthat::with_mocked_bindings(
    .adaptive_controller_resolve = function(state) list(),
    .adaptive_rank_collect_phase_a_artifacts = function(state, set_ids = NULL) {
      list(
        artifacts = list(`1` = make_adaptive_rank_covr_artifact(1L)),
        errors = list()
      )
    },
    .adaptive_rank_phase_a_artifact_dir = function(session_dir) artifact_dir,
    .adaptive_write_phase_a_artifacts = function(artifacts, artifact_dir) invisible(NULL),
    .adaptive_phase_a_empty_state = function(set_ids = integer()) {
      tibble::tibble(
        set_id = as.integer(set_ids),
        source = rep(NA_character_, length(set_ids)),
        status = rep(NA_character_, length(set_ids)),
        validation_message = rep(NA_character_, length(set_ids)),
        artifact_path = rep(NA_character_, length(set_ids))
      )
    },
    .adaptive_phase_a_set_stop_passed = function(artifact, source, controller) TRUE,
    .adaptive_phase_a_artifact_filename = function(set_id) "phase-a-set-1.rds",
    pairwiseLLM:::.adaptive_rank_phase_a_surface(
      state,
      session_dir = "session-dir"
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(surface$set_status$source[[1L]], "run")
  expect_identical(surface$set_status$status[[1L]], "ready")
  expect_identical(surface$set_status$validation_message[[1L]], "wrapper_discovered")
})

test_that("adaptive rank phase-a directory resolution covers failure and session fallback paths", {
  expect_error(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_from_directory(
      file.path(tempdir(), "missing-phase-a-dir")
    ),
    "must be an existing directory"
  )

  empty_dir <- withr::local_tempdir()
  expect_error(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_from_directory(empty_dir),
    "No Phase A artifacts were found in directory"
  )

  session_dir <- withr::local_tempdir()
  paths <- pairwiseLLM:::.adaptive_session_paths(session_dir)
  dir.create(dirname(paths$state), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(mock = TRUE), paths$state)

  expect_error(
    testthat::with_mocked_bindings(
      load_adaptive_session = function(session_dir) {
        stop("broken session load")
      },
      pairwiseLLM:::.adaptive_rank_resolve_phase_a_from_directory(session_dir),
      .package = "pairwiseLLM"
    ),
    "Failed to load Phase A artifacts from session directory"
  )

  expect_error(
    testthat::with_mocked_bindings(
      load_adaptive_session = function(session_dir) {
        list(items = tibble::tibble(set_id = 1L), linking = list())
      },
      .adaptive_rank_collect_phase_a_artifacts = function(state, set_ids = NULL) {
        list(artifacts = list(), errors = list())
      },
      pairwiseLLM:::.adaptive_rank_resolve_phase_a_from_directory(session_dir),
      .package = "pairwiseLLM"
    ),
    "No reusable Phase A artifacts were discoverable in session directory"
  )

  resolved <- testthat::with_mocked_bindings(
    load_adaptive_session = function(session_dir) {
      list(items = tibble::tibble(set_id = c(1L, 2L)), linking = list())
    },
    .adaptive_rank_collect_phase_a_artifacts = function(state, set_ids = NULL) {
      list(
        artifacts = list(`1` = make_adaptive_rank_covr_artifact(1L)),
        errors = list()
      )
    },
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_from_directory(session_dir),
    .package = "pairwiseLLM"
  )

  expect_setequal(names(resolved), "1")
})

test_that("adaptive rank artifact source and normalization helpers cover list and resolver branches", {
  artifact_5 <- make_adaptive_rank_covr_artifact(5L)
  artifact_6 <- make_adaptive_rank_covr_artifact(6L)
  artifact_7 <- make_adaptive_rank_covr_artifact(7L)
  artifact_8 <- make_adaptive_rank_covr_artifact(8L)
  artifact_9 <- make_adaptive_rank_covr_artifact(9L)
  artifact_11 <- make_adaptive_rank_covr_artifact(11L)
  artifact_12 <- make_adaptive_rank_covr_artifact(12L)

  nested_rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(phase_a = list(`5` = artifact_5)), nested_rds)

  expect_identical(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_artifact_source(
      list(phase_a = list(`6` = artifact_6))
    ),
    list(`6` = artifact_6)
  )
  expect_identical(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_artifact_source(nested_rds),
    list(`5` = artifact_5)
  )
  expect_identical(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_artifact_source(list(foo = "bar")),
    list(foo = "bar")
  )
  expect_identical(
    pairwiseLLM:::.adaptive_rank_resolve_phase_a_artifact_source(42L),
    42L
  )

  direct <- pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(artifact_7)
  expect_setequal(names(direct), "7")
  expect_identical(as.integer(direct[["7"]]$set_id), 7L)

  surface_like <- list(
    manifest = list(`7` = artifact_7),
    set_status = tibble::tibble()
  )
  expect_identical(
    pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(surface_like),
    list(`7` = artifact_7)
  )
  expect_identical(pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(1L), 1L)

  unnamed <- pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(list(artifact_8))
  expect_identical(length(unnamed), 1L)
  expect_identical(as.integer(unnamed[[1L]]$set_id), 8L)

  single_named <- testthat::with_mocked_bindings(
    .adaptive_rank_resolve_phase_a_artifact_source = function(x) {
      list(`99` = artifact_9)
    },
    pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(
      list(`20` = nested_rds)
    ),
    .package = "pairwiseLLM"
  )
  expect_setequal(names(single_named), "20")
  expect_identical(as.integer(single_named[["20"]]$set_id), 9L)

  expect_error(
    testthat::with_mocked_bindings(
      .adaptive_rank_resolve_phase_a_artifact_source = function(x) {
        list(`11` = artifact_11, `12` = artifact_12)
      },
      pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(
        list(`20` = nested_rds)
      ),
      .package = "pairwiseLLM"
    ),
    "resolved to multiple artifacts"
  )

  flattened <- testthat::with_mocked_bindings(
    .adaptive_rank_resolve_phase_a_artifact_source = function(x) {
      list(`11` = artifact_11, `12` = artifact_12)
    },
    pairwiseLLM:::.adaptive_rank_normalize_phase_a_artifacts(list(nested_rds)),
    .package = "pairwiseLLM"
  )
  expect_setequal(names(flattened), c("11", "12"))
})

test_that("adaptive rank adaptive_config helpers reject invalid linking inputs", {
  items_multi <- tibble::tibble(
    item_id = c("h1", "h2", "s1", "s2", "t1", "t2"),
    set_id = c(1L, 1L, 2L, 2L, 3L, 3L)
  )

  expect_error(
    pairwiseLLM:::.adaptive_rank_normalize_adaptive_config(1L),
    "must be NULL or a named list"
  )
  expect_error(
    pairwiseLLM:::.adaptive_rank_validate_linking_config(items_multi, 1L),
    "must be NULL or a named list"
  )
  expect_error(
    pairwiseLLM:::.adaptive_rank_validate_linking_config(
      items_multi,
      list(run_mode = "bad_mode")
    ),
    "run_mode"
  )
  expect_error(
    pairwiseLLM:::.adaptive_rank_validate_linking_config(
      items_multi,
      list(run_mode = "link_multi_spoke", phase_a_mode = "bad_mode")
    ),
    "phase_a_mode"
  )
  expect_error(
    pairwiseLLM:::.adaptive_rank_validate_linking_config(
      items_multi,
      list(run_mode = "link_multi_spoke", hub_id = 99L)
    ),
    "hub_id"
  )
  expect_error(
    pairwiseLLM:::.adaptive_rank_validate_linking_config(
      items_multi,
      list(run_mode = "link_one_spoke", hub_id = 1L)
    ),
    "exactly one spoke set"
  )
})

test_that("adaptive_rank wrapper falls back to rank_mean item sorting when raw ranks are absent", {
  tracker <- new.env(parent = emptyenv())
  tracker$sort_by <- NULL

  out <- testthat::with_mocked_bindings(
    .adaptive_rank_read_data = function(data, id_col, text_col) {
      tibble::tibble(ID = c("a", "b"), text = c("A", "B"))
    },
    .adaptive_rank_normalize_adaptive_config = function(adaptive_config) adaptive_config,
    .adaptive_rank_validate_linking_config = function(items, adaptive_config) invisible(NULL),
    adaptive_rank_start = function(...) {
      structure(
        list(
          item_ids = c("a", "b"),
          config = list(session_dir = NULL)
        ),
        class = "adaptive_state"
      )
    },
    adaptive_rank_run_live = function(...) {
      structure(
        list(
          item_ids = c("a", "b"),
          config = list(session_dir = NULL)
        ),
        class = "adaptive_state"
      )
    },
    .adaptive_rank_phase_a_surface = function(...) {
      list(manifest = list())
    },
    adaptive_get_logs = function(...) {
      list(
        step_log = tibble::tibble(),
        round_log = tibble::tibble(),
        item_log = list(
          tibble::tibble(
            ID = c("a", "b"),
            rank_mean = c(2, 1)
          )
        )
      )
    },
    summarize_adaptive = function(...) tibble::tibble(summary = "ok"),
    summarize_refits = function(...) tibble::tibble(refits = 0L),
    summarize_items = function(state, sort_by = NULL, ...) {
      tracker$sort_by <- sort_by
      tibble::tibble(ID = c("b", "a"), rank_mean = c(1, 2))
    },
    pairwiseLLM::adaptive_rank(
      data = tibble::tibble(ID = c("a", "b"), text = c("A", "B")),
      id_col = "ID",
      text_col = "text",
      judge = function(A, B, state, ...) {
        list(
          is_valid = TRUE,
          Y = 1L,
          invalid_reason = NA_character_
        )
      },
      n_steps = 1L,
      progress = "none"
    ),
    .package = "pairwiseLLM"
  )

  expect_identical(tracker$sort_by, "rank_mean")
  expect_identical(out$items$ID, c("b", "a"))
})
