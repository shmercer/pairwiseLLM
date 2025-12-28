test_that(".bt_add_drift_aliases returns input for NULL/non-df/empty", {
  expect_null(pairwiseLLM:::.bt_add_drift_aliases(NULL))

  v <- c(1, 2, 3)
  expect_identical(pairwiseLLM:::.bt_add_drift_aliases(v), v)

  empty <- tibble::tibble()
  out_empty <- pairwiseLLM:::.bt_add_drift_aliases(empty)
  expect_true(is.data.frame(out_empty))
  expect_equal(nrow(out_empty), 0L)
})

test_that(".bt_add_drift_aliases adds core_n and creates missing core/linking aliases", {
  m <- tibble::tibble(
    core_mean_abs_shift = 0.1,
    linking_max_abs_shift = 0.2
  )

  out <- pairwiseLLM:::.bt_add_drift_aliases(m)

  # core_n added
  expect_true("core_n" %in% names(out))

  # core -> linking alias created when missing
  expect_true("linking_mean_abs_shift" %in% names(out))
  expect_equal(out$linking_mean_abs_shift, out$core_mean_abs_shift)

  # linking -> core alias created when missing
  expect_true("core_max_abs_shift" %in% names(out))
  expect_equal(out$core_max_abs_shift, out$linking_max_abs_shift)
})
