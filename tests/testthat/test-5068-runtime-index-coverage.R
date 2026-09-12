test_that("probe index reconstruction retains latest evidence and detects each corrupted field", {
  state <- task09_link_state()
  realized <- tibble::tibble(step_id = c(1L, 2L), pair_id = c(1L, 2L),
    run_mode = "link_probe_holdout", spoke_id = 2L, link_epoch_id = 1L,
    probe_panel_id = "panel-2", hub_item_id = "a", spoke_item_id = "c", pair_key = "a:c", Y = 1L)
  state$linking$probe$realized_edges <- realized
  canonical <- .adaptive_link_probe_realized_index_build(realized)
  expect_identical(canonical[[1]]$row_ids, 2L)
  expect_identical(canonical[[1]]$realized_count, 1L)
  expect_identical(canonical[[1]]$last_realized_step_id, 2L)
  expect_equal(.adaptive_link_probe_realized_rows_from_entry(state, canonical[[1]]), realized[2, ])
  expect_error(.adaptive_link_probe_realized_index_compare(list(), canonical), "panel keys")
  for (field in c("spoke_id", "link_epoch_id", "probe_panel_id", "realized_count", "last_realized_step_id")) {
    bad <- canonical
    bad[[1]][[field]] <- if (field == "probe_panel_id") "wrong" else 9L
    expect_error(.adaptive_link_probe_realized_index_compare(bad, canonical), field, fixed = TRUE)
    expect_error(.adaptive_link_probe_realized_rows_from_entry(state, bad[[1]]),
      if (field == "realized_count") "realized count" else field, fixed = TRUE)
  }
  bad <- canonical
  bad[[1]]$row_ids <- 1L
  expect_error(.adaptive_link_probe_realized_index_compare(bad, canonical), "row ids")
  bad[[1]]$row_ids <- 9L
  expect_error(.adaptive_link_probe_realized_rows_from_entry(state, bad[[1]]), "out of range")
  state$linking$probe$realized_index_by_panel <- bad
  expect_error(.adaptive_link_probe_realized_log_for_epoch(state, 2L, 1L), "out of range")
  bad[[1]]$row_ids <- integer()
  state$linking$probe$realized_index_by_panel <- bad
  expect_equal(nrow(.adaptive_link_probe_realized_log_for_epoch(state, 2L, 1L)), 0L)
  realized$step_id <- NA_integer_
  state$linking$probe$realized_edges <- realized
  entry <- .adaptive_link_probe_realized_index_build(realized)[[1]]
  expect_true(is.na(entry$last_realized_step_id))
  expect_equal(nrow(.adaptive_link_probe_realized_rows_from_entry(state, entry)), 1L)
})

test_that("legacy prediction cache derives keys while malformed indices rebuild safely", {
  state <- task09_link_state()
  state$linking$probe$prediction_cache <- tibble::tibble(hub_item_id = "a", spoke_item_id = "c")
  state$linking$probe$realized_index_by_panel <- 1
  probe <- .adaptive_link_probe_state(state)
  expect_identical(probe$prediction_cache$pair_key, "a:c")
  expect_identical(probe$realized_index_by_panel, list())
  state$linking$probe$prediction_cache <- tibble::tibble(refit_id = 1L)
  expect_identical(.adaptive_link_probe_state(state)$prediction_cache$pair_key, "")
  state$linking$probe <- NULL
  expect_true(is.na(.adaptive_link_probe_realized_last_step_id(state, 2L)))
  panel <- tibble::tibble(probe_panel_id = "panel-2", link_epoch_id = 1L, spoke_id = 2L,
    pair_key = "a:c", realized = FALSE, realized_step_id = NA_integer_,
    realized_pair_id = NA_integer_, realized_run_mode = NA_character_)
  state$linking$probe <- list(panels_by_spoke = list(`2` = panel))
  expect_true(is.na(.adaptive_link_probe_realized_last_step_id(state, 2L)))
  row <- tibble::tibble(pair_id = 1L, step_id = 1L, is_probe_step = TRUE,
    run_mode = "link_probe_holdout", link_spoke_id = 2L, A = 1L, B = 4L, y_A = 1L)
  expect_error(.adaptive_link_probe_register_commit(state, row), "not present in the current")
  state$linking$probe$panels_by_spoke <- list()
  expect_error(.adaptive_link_probe_register_commit(state, row), "no current panel")
})

test_that("deferred round reporting tolerates absent payloads and respects existing values", {
  state <- task09_link_state()
  state$round_log <- tibble::tibble(refit_id = 1L)
  expect_equal(adaptive_round_log(state, reconstruct_deferred = TRUE), state$round_log)
  state$refit_meta$round_log_deferred_audit_payloads <- list(`1` = list())
  expect_equal(adaptive_round_log(state, reconstruct_deferred = TRUE), state$round_log)
  state$round_log$gini_degree <- 0.2
  state$refit_meta$round_log_deferred_audit_payloads <- list(`1` = 1)
  expect_equal(adaptive_round_log(state, reconstruct_deferred = TRUE), state$round_log)
  schema <- list(id = "integer", value = "double")
  log <- append_canonical_row(tibble::tibble(id = 1L), list(id = 2L, value = 0.4), schema)
  expect_identical(log$id, 1:2)
  expect_equal(log$value, c(NA_real_, 0.4))
})
