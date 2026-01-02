test_that("select_core_link_pairs validation branches", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("t1", "t2", "t3"))
  theta <- tibble::tibble(ID = samples$ID, theta = c(0, 1, -1), se = c(0.2, 0.3, 0.4))

  expect_error(
    select_core_link_pairs(samples, theta, core_ids = "A", round_size = 1, within_batch_frac = 0.8, core_audit_frac = 0.3),
    "within_batch_frac \\+ core_audit_frac"
  )

  expect_error(
    select_core_link_pairs(samples, theta, core_ids = "A", round_size = 1, forbid_keys = 123),
    "forbid_keys"
  )
})
