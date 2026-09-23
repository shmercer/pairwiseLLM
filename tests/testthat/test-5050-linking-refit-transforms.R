test_that("E1--E3 refits use appended explicit evidence and retain Phase A identity", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    input <- link_contract_input(id, 2L)
    extended <- link_contract_input(id, 4L)
    first <- start_link_session(input)
    resumed <- resume_link_session(first, extended)
    fresh <- start_link_session(extended)
    a <- .link_session_results(resumed)[[1L]]
    b <- .link_session_results(fresh)[[1L]]
    expect_equal(a$items, b$items)
    expect_equal(a$offset, b$offset)
    expect_identical(a$continuation$input$phase_a, input$phase_a)
    expect_identical(a$provenance$hashes, b$provenance$hashes)
    expect_identical(resume_link_session(resumed, extended), resumed)
    expect_identical(resumed$link_stage_log$phase_b_active_edges_used, c(2L, 4L))
    expect_null(resumed$linking$anchored_joint)
  }
})
