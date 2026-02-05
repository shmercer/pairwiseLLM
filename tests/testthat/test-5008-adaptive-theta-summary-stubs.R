test_that("adaptive theta summary stubs abort with roadmap message", {
  expect_error(
    pairwiseLLM:::.adaptive_theta_summary_from_fit(list(), list()),
    "not implemented yet"
  )

  expect_error(
    pairwiseLLM:::.adaptive_v3_theta_summary(list(), list()),
    "not implemented yet"
  )
})
