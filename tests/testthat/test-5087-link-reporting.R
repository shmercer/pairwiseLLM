test_that("logs and summaries expose estimator-neutral evidence and uncertainty", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    input <- link_contract_input(id, 2L)
    state <- start_link_session(input)
    result <- state$linking$estimator$accepted_state_by_spoke$S
    expect_silent(pairwiseLLM:::.adaptive_validate_log_schema(state$link_stage_log,
      pairwiseLLM:::schema_link_stage_log, "link_stage_log"))
    expect_false(any(grepl("anchored_joint", names(state$link_stage_log))))
    expect_identical(adaptive_get_logs(state)$link_stage_log, state$link_stage_log)
    expect_identical(summarize_refits(state, last_n = 1), state$link_stage_log)
    expect_identical(summarize_refits(result), summary(result))
    row <- summary(state)
    expect_identical(row, summary(result))
    expect_identical(row$estimator_id, id)
    expect_identical(row$phase_a_within_edges_hub_used, if (id == "joint_offset") 1L else 0L)
    expect_identical(row$phase_b_active_edges_used, 2L)
    expect_true(row$fit_valid)
    items <- summarize_items(state)
    expect_identical(items, summarize_items(result))
    expect_identical(items$theta_link_eap, items$theta_link_mean)
    expect_identical(items$uncertainty_scope, rep(result$diagnostics$uncertainty_scope, 4L))
    expect_equal(nrow(summarize_items(state, top_n = 2, sort_by = "rank_link")), 2)
    expect_error(summarize_items(state, sort_by = "bad"), "sort column")
    expect_error(summarize_items(state, refit = 1), "current accepted")
    printed <- capture.output(print(state))
    expect_true(any(grepl(id, printed, fixed = TRUE)))
    expect_true(any(grepl("Offset:", printed, fixed = TRUE)))
    expect_false(any(grepl("transform|hard.lock|anchored_joint", printed)))
    expect_identical(capture.output(print(result)), printed)
    prior <- start_link_session(link_contract_input(id))
    expect_identical(summary(prior)$identification, "prior_only")
    expect_false(summary(prior)$linking_identified)
  }
})

test_that("invalid fits remain inspectable with unavailable uncertainty", {
  args <- link_contract_args("joint_offset", 2L)
  args$control <- list(estimator = list(maxit = 1L, gradient_tol = 1e-20))
  state <- start_link_session(do.call(prepare_link_input, args))
  expect_false(summary(state)$fit_valid)
  expect_true(all(is.na(summarize_items(state)$theta_link_sd)))
  expect_match(paste(capture.output(print(state)), collapse = "\n"), "valid: FALSE")
})

test_that("pre-session common results retain exact inference and gain reporting aliases only in views", {
  result <- fit_link(link_contract_input(edges = 2L))
  old <- result
  old$items <- old$items[, setdiff(names(old$items), c("theta_link_eap", "estimator_id", "uncertainty_scope"))]
  before <- serialize(old, NULL)
  expect_silent(pairwiseLLM:::.link_validate_result(old))
  expect_equal(summarize_items(old)[names(summarize_items(result))], summarize_items(result))
  expect_identical(serialize(old, NULL), before)
  pairs <- old$continuation$input$cross[, -6]
  expect_identical(predict_link(old, pairs), predict_link(result, pairs))
})
