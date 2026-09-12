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

test_that("fresh-process Phase B resume preserves current and legacy linking decisions", {
  root <- withr::local_tempdir()
  for (legacy in c(FALSE, TRUE)) {
    state <- task10_link_state()
    state <- .adaptive_linking_refit_update_state(state, list(last_refit_step = 0L))
    state$controller$link_state_frozen_by_spoke <- list(`2` = TRUE)
    state$controller$link_transform_frozen_by_spoke <- list(`2` = TRUE)
    if (legacy) {
      state$controller$link_estimation_mode <- "transform"
      state$controller$link_transform_policy <- "auto"
      state$controller$link_transform_state_by_spoke <- list(`2` = "shift_only", `3` = "shift_only")
      state$controller$link_refit_mode <- "shift_only"
      state$controller$shift_only_theta_treatment <- "normal_prior"
      state$controller$multi_spoke_mode <- "independent"
      state$controller$hub_lock_mode <- "soft_lock"
    }
    session <- file.path(root, if (legacy) "legacy" else "current")
    save_adaptive_session(state, session)
    resumed <- load_adaptive_session(session)
    expect_identical(resumed$trueskill_state, state$trueskill_state)
    expect_identical(resumed$history_pairs, state$history_pairs)
    expect_identical(resumed$warm_start_pairs, state$warm_start_pairs)
    expect_identical(resumed$warm_start_idx, state$warm_start_idx)
    expect_identical(resumed$round, state$round)
    inspect <- function(s) {
      out <- pairwiseLLM:::.adaptive_linking_refit_update_state(s, list(last_refit_step = 0L))
      list(accepted = out$linking$anchored_joint,
        controller = out$controller, step_log = out$step_log, link_stage_log = out$link_stage_log,
        history = out$history_pairs, trueskill = out$trueskill_state,
        ranked = pairwiseLLM:::.adaptive_link_ranked_spokes(out, out$controller),
        probe = pairwiseLLM:::.adaptive_link_probe_next_holdout_spoke(out, out$controller))
    }
    environment(inspect) <- baseenv()
    expected <- inspect(resumed)
    expect_false(2L %in% expected$ranked)
    inspect_path <- file.path(root, "inspect.rds")
    saveRDS(inspect, inspect_path)
    output_path <- file.path(root, "result.rds")
    quote_path <- function(x) encodeString(x, quote = "\"")
    dev <- pkgload::is_dev_package("pairwiseLLM")
    loader <- if (dev) {
      paste0("pkgload::load_all(",
        quote_path(getNamespaceInfo("pairwiseLLM", "path")), ", quiet = TRUE)")
    } else {
      "library(pairwiseLLM)"
    }
    script <- file.path(root, "resume.R")
    writeLines(c(
      paste0(".libPaths(c(", paste(vapply(.libPaths(), quote_path, character(1)),
        collapse = ","), "))"),
      loader,
      paste0("inspect <- readRDS(", quote_path(inspect_path), ")"),
      paste0("state <- pairwiseLLM::load_adaptive_session(", quote_path(session), ")"),
      paste0("saveRDS(inspect(state), ", quote_path(output_path), ")")
    ), script)
    child <- system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)), stdout = TRUE, stderr = TRUE)
    status <- as.integer(attr(child, "status") %||% 0L)
    if (status != 0L || !file.exists(output_path)) {
      rlang::abort(paste(c("Fresh Phase B resume failed:", child), collapse = "\n"))
    }
    expect_equal(readRDS(output_path), expected, tolerance = 0)
  }
})
