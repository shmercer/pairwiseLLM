test_that("round_log_schema columns are stable", {
  round_log <- pairwiseLLM:::round_log_schema()

  expect_snapshot_value(
    colnames(round_log),
    style = "deparse"
  )

  expect_true(is.integer(round_log$round_id))
  expect_true(is.double(round_log$reliability_EAP))
  expect_true(is.logical(round_log$stop_decision))
  expect_true(is.character(round_log$stop_reason))
})

test_that("item_summary_schema columns are stable", {
  item_summary <- pairwiseLLM:::item_summary_schema()

  expect_snapshot_value(
    colnames(item_summary),
    style = "deparse"
  )

  expect_true(is.character(item_summary$ID))
  expect_true(is.double(item_summary$theta_mean))
  expect_true(is.integer(item_summary$deg))
  expect_true(is.double(item_summary$posA_prop))
})
