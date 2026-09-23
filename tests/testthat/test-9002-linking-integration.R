test_that("E1--E3 public workflows integrate fitting, prediction, logs and persistence", {
  for (id in .adaptive_link_estimation_mode_levels()) {
    input <- link_contract_input(id, 2L)
    full <- link_contract_input(id, 4L)
    session <- start_link_session(input)
    dir <- withr::local_tempdir()
    save_adaptive_session(session, dir)
    restored <- load_adaptive_session(dir)
    expect_identical(restored, session)
    expect_identical(validate_session_dir(dir)$estimator_id, id)
    extended <- resume_link_session(restored, full)
    result <- .link_session_results(extended)[[1L]]
    independent <- fit_link(full)
    expect_equal(result$items, independent$items)
    expect_equal(predict_link(result, full$cross[, -6L]),
      predict_link(independent, full$cross[, -6L]))
    rows <- adaptive_get_logs(extended)$link_stage_log
    expect_identical(rows$estimator_id, rep(id, 2L))
    expect_identical(rows$phase_b_active_edges_used, c(2L, 4L))
    expect_identical(rows$phase_a_within_edges_hub_used,
      rep(if (id == "joint_offset") input$counts$phase_a_hub else 0L, 2L))
    expect_false(any(c("hub_lock_mode", "hub_lock_kappa") %in% names(rows)))
    expect_true(all(summary(extended)$fit_valid))
    expect_true(all(summarize_items(extended)$estimator_id == id))
    expect_error(select_next_pair(extended), class = "pairwiseLLM_link_selector_unvalidated")
    expect_error(run_one_step(extended, function(...) stop("must not judge")), "adaptive_state")
  }
})
