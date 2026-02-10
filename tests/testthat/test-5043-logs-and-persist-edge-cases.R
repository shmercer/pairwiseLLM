test_that("schema helpers reject unknown and malformed schema definitions", {
  expect_error(pairwiseLLM:::.adaptive_schema_empty_col("bogus"), "Unknown schema column type")
  expect_error(pairwiseLLM:::.adaptive_schema_typed_na("bogus"), "Unknown schema column type")
  expect_error(
    pairwiseLLM:::.adaptive_schema_empty_tbl(c("integer", "double")),
    "named list"
  )

  expect_false(pairwiseLLM:::.adaptive_is_integerish(c(1, NA_real_)))
})

test_that("append_canonical_row validates core preconditions and coercion safety", {
  schema <- c(a = "integer", when = "POSIXct", flag = "logical")
  log_tbl <- tibble::tibble(a = integer(), when = as.POSIXct(character(), tz = "UTC"), flag = logical())

  expect_error(
    pairwiseLLM:::append_canonical_row("bad", list(a = 1L), schema),
    "data frame"
  )
  expect_error(
    pairwiseLLM:::append_canonical_row(log_tbl, list(a = 1L), c("integer", "logical")),
    "named list"
  )
  expect_error(
    pairwiseLLM:::append_canonical_row(log_tbl, 1L, schema),
    "named list or data frame"
  )

  expect_error(
    pairwiseLLM:::append_canonical_row(
      log_tbl,
      tibble::tibble(a = integer(), when = as.POSIXct(character(), tz = "UTC"), flag = logical()),
      schema,
      allow_multirow = TRUE
    ),
    "at least one row"
  )

  expect_error(
    pairwiseLLM:::append_canonical_row(log_tbl, list(a = 1L, when = "bad", flag = TRUE), schema),
    "must be POSIXct"
  )
  expect_error(
    pairwiseLLM:::append_canonical_row(
      log_tbl,
      list(a = 1L, when = as.POSIXct("2026-01-01", tz = "UTC"), flag = "not_bool"),
      schema
    ),
    "could not be coerced"
  )

  expect_error(
    pairwiseLLM:::append_canonical_row(
      log_tbl,
      list(a = 1L, when = as.POSIXct("2026-01-01", tz = "UTC"), flag = TRUE),
      c(a = "integer", when = "POSIXct", flag = "mystery")
    ),
    "Unknown schema column type"
  )
})

test_that("new_item_step_log validates input table shape", {
  expect_error(pairwiseLLM:::new_item_step_log("bad"), "must be a data frame")
  expect_error(pairwiseLLM:::new_item_step_log(tibble::tibble(id = 1L)), "must include `item_id`")
})

test_that("write_log and read_log validate path constraints", {
  expect_error(pairwiseLLM:::write_log(1L, NA_character_), "single, non-missing string")
  expect_error(pairwiseLLM:::write_log(1L, "x.parquet"), "not supported")
  expect_error(pairwiseLLM:::write_log(1L, "x.txt"), "must end with .rds")

  expect_error(pairwiseLLM:::read_log(NA_character_), "single, non-missing string")
  expect_error(pairwiseLLM:::read_log("x.parquet"), "not supported")
  expect_error(pairwiseLLM:::read_log(file.path(withr::local_tempdir(), "missing.rds")), "Missing log file")
})

test_that("adaptive write helpers handle existence checks and write failures", {
  temp_dir <- withr::local_tempdir()
  existing <- file.path(temp_dir, "exists.rds")
  saveRDS(1L, existing)
  expect_error(
    pairwiseLLM:::.adaptive_abort_if_exists(c(existing, file.path(temp_dir, "nope.rds"))),
    "already contains saved artifacts"
  )

  out_path <- file.path(temp_dir, "nested", "value.rds")
  pairwiseLLM:::.adaptive_write_atomic(letters, out_path)
  expect_equal(readRDS(out_path), letters)

  failed_path <- file.path(temp_dir, "fail", "value.rds")
  expect_error(
    testthat::with_mocked_bindings(
      file.rename = function(from, to) FALSE,
      pairwiseLLM:::.adaptive_write_atomic(letters, failed_path),
      .package = "base"
    ),
    "Failed to write file"
  )
})

test_that("log schema and resume-state validators reject malformed inputs", {
  expect_error(
    pairwiseLLM:::.adaptive_validate_log_schema("bad", pairwiseLLM:::schema_step_log, "step_log"),
    "must be a data frame"
  )

  good <- pairwiseLLM:::new_step_log()
  bad_type <- good
  bad_type$step_id <- as.double(bad_type$step_id)
  expect_error(
    pairwiseLLM:::.adaptive_validate_log_schema(bad_type, pairwiseLLM:::schema_step_log, "step_log"),
    "does not match canonical type"
  )

  state <- pairwiseLLM:::new_adaptive_state(make_test_items(3))

  missing <- state
  missing$item_ids <- NULL
  expect_error(pairwiseLLM:::.adaptive_validate_state_for_resume(missing), "missing required fields")

  bad_item_log <- state
  bad_item_log$item_log <- 1L
  expect_error(pairwiseLLM:::.adaptive_validate_state_for_resume(bad_item_log), "must be a list")

  bad_item_step <- state
  bad_item_step$item_step_log <- data.frame(step_id = 1L)
  expect_error(pairwiseLLM:::.adaptive_validate_state_for_resume(bad_item_step), "must be a tibble")
})

test_that("item log read/write helpers cover empty and sorted discovery paths", {
  item_dir <- file.path(withr::local_tempdir(), "item_log")

  expect_identical(pairwiseLLM:::.adaptive_read_item_log_files(item_dir), list())
  expect_null(pairwiseLLM:::.adaptive_write_item_log_files(list(), item_dir))

  dir.create(item_dir, recursive = TRUE)
  saveRDS(tibble::tibble(refit_id = 2L), file.path(item_dir, "refit_0002.rds"))
  saveRDS(tibble::tibble(refit_id = 1L), file.path(item_dir, "refit_0001.rds"))

  out <- pairwiseLLM:::.adaptive_read_item_log_files(item_dir)
  expect_equal(length(out), 2L)
  expect_identical(out[[1L]]$refit_id[[1L]], 1L)
  expect_identical(out[[2L]]$refit_id[[1L]], 2L)
})

test_that("validate_session_dir and save/load session guard clauses are enforced", {
  expect_error(validate_session_dir(NA_character_), "single, non-missing string")
  expect_error(validate_session_dir(file.path(withr::local_tempdir(), "nope")), "does not exist")

  items <- make_test_items(3)
  state <- adaptive_rank_start(items)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  expect_error(save_adaptive_session(list(), session_dir, overwrite = TRUE), "adaptive_state")
  expect_error(save_adaptive_session(state, NA_character_, overwrite = TRUE), "single, non-missing string")
  expect_error(save_adaptive_session(state, session_dir, overwrite = NA), "TRUE or FALSE")

  meta_path <- file.path(session_dir, "metadata.rds")
  saveRDS("bad", meta_path)
  expect_error(validate_session_dir(session_dir), "metadata must be a named list")

  save_adaptive_session(state, session_dir, overwrite = TRUE)
  metadata <- readRDS(meta_path)
  metadata$n_items <- 0L
  saveRDS(metadata, meta_path)
  expect_error(validate_session_dir(session_dir), "must be a positive integer")
})

test_that("load_adaptive_session validates state payload and item index consistency", {
  items <- make_test_items(3)
  state <- adaptive_rank_start(items)
  session_dir <- withr::local_tempdir()
  save_adaptive_session(state, session_dir)

  saveRDS(list(not = "state"), file.path(session_dir, "state.rds"))
  expect_error(load_adaptive_session(session_dir), "does not contain an adaptive_state")

  save_adaptive_session(state, session_dir, overwrite = TRUE)
  metadata <- readRDS(file.path(session_dir, "metadata.rds"))
  metadata$n_items <- 99L
  saveRDS(metadata, file.path(session_dir, "metadata.rds"))
  expect_error(load_adaptive_session(session_dir), "does not match state item count")

  save_adaptive_session(state, session_dir, overwrite = TRUE)
  broken <- readRDS(file.path(session_dir, "state.rds"))
  broken$item_ids <- character()
  saveRDS(broken, file.path(session_dir, "state.rds"))
  expect_error(load_adaptive_session(session_dir), "missing `item_ids`")

  save_adaptive_session(state, session_dir, overwrite = TRUE)
  step_path <- file.path(session_dir, "step_log.rds")
  step_log <- readRDS(step_path)
  step_log <- pairwiseLLM:::append_step_log(
    step_log,
    list(
      step_id = 1L,
      timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
      pair_id = 1L,
      A = 99L,
      B = 1L,
      Y = 1L
    )
  )
  saveRDS(step_log, step_path)
  expect_error(load_adaptive_session(session_dir), "invalid item indices")
})
