test_that("new linking construction requires explicit supported estimator selection", {
  items <- make_test_items(4)
  items$set_id <- rep(1:2, each = 2L)
  items$global_item_id <- paste0("g", items$item_id)
  expect_error(adaptive_rank_start(items, adaptive_config = list(run_mode = "link_one_spoke")),
    class = "pairwiseLLM_link_estimator_required")
  for (id in c("anchored_joint", "transform", "unknown")) {
    expect_error(adaptive_rank_start(items, adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = id)),
      class = "pairwiseLLM_unsupported_link_estimator")
    args <- link_contract_args(); args$estimator <- id
    expect_error(do.call(prepare_link_input, args), class = "pairwiseLLM_link_contract_error")
  }
  args <- link_contract_args(); args$estimator <- NULL
  expect_error(do.call(prepare_link_input, args), "explicit")
  for (id in .adaptive_link_estimation_mode_levels()) {
    state <- adaptive_rank_start(items, adaptive_config = list(
      run_mode = "link_one_spoke", link_estimation_mode = id))
    expect_identical(.adaptive_controller_resolve(state)$link_estimation_mode, id)
    expect_null(state$linking$anchored_joint)
  }
  for (key in c("anchored_joint_spoke_prior_scale", "anchored_joint_sd_floor",
      "anchored_joint_spoke_prior_fallback_sd", "anchored_joint_init_state_method", "hub_lock_mode")) {
    config <- list(run_mode = "link_one_spoke", link_estimation_mode = "fixed_shape_offset")
    config[[key]] <- 1
    expect_error(adaptive_rank_start(items, adaptive_config = config), "Unknown.*adaptive_config")
  }
})

test_that("legacy recognition precedes normalization, overrides, reporting and execution", {
  base <- task10_link_state(2L)
  base$linking$estimator <- NULL
  base$controller$link_estimation_mode <- "anchored_joint"
  variants <- list(base)
  x <- base; x$controller$link_estimation_mode <- NULL
  x$config$adaptive_config <- list(link_estimation_mode = "anchored_joint")
  variants[[2L]] <- x
  x$config$adaptive_config <- NULL
  x$linking$anchored_joint <- list(accepted_state_by_spoke = list())
  variants[[3L]] <- x
  x$linking$anchored_joint <- NULL
  x$link_stage_log <- data.frame(link_estimation_mode = "anchored_joint")
  variants[[4L]] <- x
  x$link_stage_log <- NULL
  variants[[5L]] <- x # Missing identity is not permission to choose E1.
  for (missing_mode in c(NA_character_, "")) {
    blank <- x; blank$controller$link_estimation_mode <- missing_mode
    variants[[length(variants) + 1L]] <- blank
  }
  x$linking$phase_a$phase <- NULL
  x$linking$anchored_joint <- list(accepted_state_by_spoke = list(
    `2` = list(anchored_joint_init_state_method = "artifact_copy_init")))
  variants[[length(variants) + 1L]] <- x
  x$linking$anchored_joint <- NULL
  x$step_log <- data.frame(is_cross_set = TRUE, run_mode = "link_one_spoke",
    link_estimation_mode = "anchored_joint")
  variants[[length(variants) + 1L]] <- x
  path <- file.path(withr::local_tempdir(), "legacy.rds")
  for (state in variants) {
    before <- state
    saveRDS(state, path)
    calls <- list(
      function() load_link_session(path),
      function() .adaptive_validate_state_for_resume(state),
      function() .adaptive_controller_resolve(state),
      function() .adaptive_apply_controller_config(state, list(link_estimation_mode = "joint_offset")),
      function() .adaptive_runtime_controller_resolve(state, list(run_mode = "within_set")),
      function() adaptive_get_logs(state), function() summarize_adaptive(state),
      function() summarize_items(state), function() summarize_refits(list(state = state)),
      function() print(state), function() select_next_pair(state),
      function() run_one_step(state, function(...) stop("must not judge")),
      function() adaptive_rank_run_live(state, function(...) stop("must not judge"),
        n_steps = 1L, progress = "none"),
      function() .rubric_cj_linked(state, NULL, state$controller))
    for (call in calls) expect_error(call(), "Restart linking from compatible Phase A",
      class = "pairwiseLLM_unsupported_legacy_link_state")
    expect_identical(state, before)
  }
})

test_that("old within-set state and compatible Phase A are distinct from legacy Phase B", {
  state <- adaptive_rank_start(letters[1:4])
  state$controller$link_estimation_mode <- "anchored_joint"
  state$linking$anchored_joint <- list(accepted_state_by_spoke = list(), fisher_t0_by_spoke = list())
  expect_silent(.link_reject_legacy(state))
  expect_null(.adaptive_controller_resolve(state)$link_estimation_mode)
  phase_a <- list(controller = list(run_mode = "link_one_spoke", link_estimation_mode = "anchored_joint"),
    linking = list(phase_a = list(phase = "phase_a")))
  expect_silent(.link_reject_legacy(phase_a))
  expect_silent(.link_reject_legacy(NULL))
  expect_silent(.link_reject_legacy(list(controller = "malformed")))
  for (id in .adaptive_link_estimation_mode_levels()) {
    args <- link_contract_args(id, 2L)
    if (id == "fixed_shape_offset") {
      args$phase_a$hub <- list(artifact = link_e1_artifact(args$hub, args$phase_a$hub$points))
      args$phase_a$hub$artifact$phase_scope <- NULL
      args$phase_a$hub$artifact$phase_scope_set_id <- NULL
      args$phase_a$hub$artifact$fit_package_version <- "1.5.2"
    }
    session <- start_link_session(do.call(prepare_link_input, args))
    expect_true(.link_session_results(session)[[1L]]$diagnostics$fit_valid)
    expect_silent(.link_reject_legacy(session))
  }
})

test_that("source cannot execute or fall back to the removed estimator", {
  ns <- asNamespace("pairwiseLLM")
  symbols <- ls(ns, all.names = TRUE)
  expect_false(any(grepl("anchored_joint", symbols)))
  for (name in symbols) {
    value <- get(name, ns)
    if (!is.function(value) || identical(name, ".link_reject_legacy")) next
    source <- paste(deparse(body(value)), collapse = "\n")
    expect_false(grepl("anchored_joint|fisher_t0_by_spoke|hard_lock", source), info = name)
  }
})

test_that("legacy identities in standalone persisted logs reject before schema repair", {
  for (kind in c("step_log", "link_stage_log")) {
    dir <- withr::local_tempdir()
    state <- adaptive_rank_start(letters[1:4])
    save_adaptive_session(state, dir)
    persisted <- readRDS(file.path(dir, "state.rds"))
    row <- if (kind == "step_log") {
      data.frame(is_cross_set = TRUE, run_mode = "link_one_spoke", link_estimation_mode = "anchored_joint")
    } else {
      data.frame(anchored_joint_init_state_method = "phase_b_refit")
    }
    saveRDS(row, file.path(dir, paste0(kind, ".rds")))
    for (read in list(load_adaptive_session, validate_session_dir, adaptive_rank_resume)) {
      expect_error(read(dir), class = "pairwiseLLM_unsupported_legacy_link_state")
    }
    expect_identical(readRDS(file.path(dir, "state.rds")), persisted)
    expect_identical(readRDS(file.path(dir, paste0(kind, ".rds"))), row)
  }
})
