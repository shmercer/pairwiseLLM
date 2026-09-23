# The legacy adaptive D-optimal golden run is no longer an executable contract.
# Validate deterministic common logs on identical explicit evidence instead.
test_that("explicit-evidence linking logs are deterministic across fresh sessions", {
  for (id in c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")) {
    a <- start_link_session(link_contract_input(id, 2L))
    b <- start_link_session(link_contract_input(id, 2L))
    a <- resume_link_session(a, link_contract_input(id, 4L))
    b <- resume_link_session(b, link_contract_input(id, 4L))
    expect_identical(a$link_stage_log, b$link_stage_log)
    expect_identical(summarize_items(a), summarize_items(b))
    expect_identical(a$link_stage_log$phase_b_active_edges_used, c(2L, 4L))
    expect_identical(a$link_stage_log$estimator_id, rep(id, 2L))
    dir <- withr::local_tempdir()
    path <- file.path(dir, "link.rds")
    save_link_session(a, path)
    restored <- load_link_session(path)
    expect_identical(restored$link_stage_log, a$link_stage_log)
    expect_identical(resume_link_session(restored, link_contract_input(id, 4L)), restored)
  }
})
