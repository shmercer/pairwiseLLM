test_that("probe resume rejects inconsistent persisted identities and construction metadata", {
  state <- task09_link_state()
  panel <- tibble::tibble(probe_panel_id = "panel-2", link_epoch_id = 1L, spoke_id = 2L,
    hub_item_id = "a", spoke_item_id = "c", pair_key = make_unordered_key("a", "c"),
    probe_edges_planned = 1L, probe_panel_reallocation_used = FALSE,
    realized = FALSE, realized_step_id = NA_integer_, realized_pair_id = NA_integer_,
    realized_run_mode = NA_character_)
  state$linking$probe$panels_by_spoke <- list(`2` = panel)
  state$controller$link_epoch_id_by_spoke <- list(`2` = 1L)
  validate <- function(x) .adaptive_link_probe_resume_validate_spoke(x, 2L)
  expect_invisible(validate(state))
  for (field in c("spoke_id", "link_epoch_id", "probe_panel_id")) {
    bad <- state
    bad$linking$probe$panels_by_spoke[["2"]][[field]] <- switch(field,
      spoke_id = 3L, link_epoch_id = NA_integer_, probe_panel_id = "")
    expect_error(validate(bad), field)
  }
  bad <- state
  bad$linking$probe$panels_by_spoke[["2"]] <- panel[c(1L, 1L), ]
  expect_error(validate(bad), "duplicate.*pair_key")
  row <- tibble::tibble(spoke_id = 2L, refit_id = 1L, link_epoch_id = 1L,
    probe_panel_id = "panel-2", probe_edges_planned = 1L,
    probe_panel_reallocation_used = FALSE, probe_edges_realized = 0L)
  state$link_stage_log <- row
  expect_invisible(validate(state))
  changes <- list(link_epoch_id = 2L, probe_panel_id = "wrong", probe_edges_planned = 2L,
    probe_panel_reallocation_used = TRUE, probe_edges_realized = 1L)
  for (field in names(changes)) {
    bad <- state
    bad$link_stage_log[[field]] <- changes[[field]]
    expect_error(validate(bad), field)
  }
  realized <- tibble::tibble(step_id = 1L, pair_id = 1L, run_mode = "link_probe_holdout",
    spoke_id = 2L, link_epoch_id = 1L, probe_panel_id = "panel-2",
    hub_item_id = "a", spoke_item_id = "c", pair_key = panel$pair_key, Y = 1L)
  for (field in c("pair_key", "probe_panel_id")) {
    bad <- state
    bad$linking$probe$realized_edges <- realized
    bad$linking$probe$realized_edges[[field]] <- "wrong"
    expect_error(validate(bad), if (field == "pair_key") "pair keys" else "probe_panel_id")
  }
  state$linking$probe$realized_edges <- realized
  state$step_log <- tibble::tibble(pair_id = 1L, step_id = 1L, link_spoke_id = 2L,
    A = 1L, B = 3L, run_mode = "link_probe_holdout")
  expect_invisible(validate(state))
  bad <- state
  bad$step_log$B <- 4L
  expect_error(validate(bad), "not contained in the current panel")
  bad <- state
  bad$linking$probe$realized_edges <- realized[FALSE, ]
  expect_error(validate(bad), "do not reconcile")
  expect_invisible(.adaptive_link_probe_resume_validate_current_window(list(), 2L, 1L, panel))
})
