testthat::test_that(".bt_frac_scalar validates inputs", {
  testthat::expect_error(
    pairwiseLLM:::.bt_frac_scalar(NA_real_, "x"),
    "single finite numeric"
  )

  testthat::expect_equal(
    pairwiseLLM:::.bt_frac_scalar(0.25, "x"),
    0.25
  )
})

testthat::test_that(".bt_apply_allocation_fun handles NULL, validation, NULL return, and >1 sum constraint", {
  state <- list(foo = "bar")

  # allocation_fun is NULL -> pass-through (covers return branch)
  out0 <- pairwiseLLM:::.bt_apply_allocation_fun(
    allocation_fun = NULL,
    state = state,
    within_batch_frac = 0.10,
    core_audit_frac = 0.20
  )
  testthat::expect_equal(out0$within_batch_frac, 0.10)
  testthat::expect_equal(out0$core_audit_frac, 0.20)

  # allocation_fun not a function -> error
  testthat::expect_error(
    pairwiseLLM:::.bt_apply_allocation_fun(
      allocation_fun = 123,
      state = state,
      within_batch_frac = 0.10,
      core_audit_frac = 0.20
    ),
    "allocation_fun"
  )

  # allocation_fun returns NULL -> pass-through (covers return branch)
  f_null <- function(state) NULL
  out1 <- pairwiseLLM:::.bt_apply_allocation_fun(
    allocation_fun = f_null,
    state = state,
    within_batch_frac = 0.10,
    core_audit_frac = 0.20
  )
  testthat::expect_equal(out1$within_batch_frac, 0.10)
  testthat::expect_equal(out1$core_audit_frac, 0.20)

  # allocation_fun returns fractions summing to > 1 -> within_batch is adjusted to 1 - core_audit
  f_over <- function(state) list(within_batch_frac = 0.90, core_audit_frac = 0.40)
  out2 <- pairwiseLLM:::.bt_apply_allocation_fun(
    allocation_fun = f_over,
    state = state,
    within_batch_frac = 0.10,
    core_audit_frac = 0.20
  )
  testthat::expect_equal(out2$core_audit_frac, 0.40)
  testthat::expect_equal(out2$within_batch_frac, 0.60) # 1 - 0.40
})
