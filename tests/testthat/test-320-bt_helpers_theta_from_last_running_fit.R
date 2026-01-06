test_that("320-01 .theta_from_last_running_fit returns NULL for missing/invalid fits", {
  out0 <- pairwiseLLM:::.theta_from_last_running_fit(NULL, id_vec = c("a"))
  expect_null(out0$theta)
  expect_true(is.na(out0$engine))

  out1 <- pairwiseLLM:::.theta_from_last_running_fit(list(theta = NULL), id_vec = c("a"))
  expect_null(out1$theta)

  # Missing required columns -> NULL
  bad_fit <- list(theta = tibble::tibble(ID = "a", se = 0.1))
  out2 <- pairwiseLLM:::.theta_from_last_running_fit(bad_fit, id_vec = c("a"))
  expect_null(out2$theta)
})

test_that("320-02 .theta_from_last_running_fit aligns IDs and fills se/rank from engine-specific columns", {
  final_fit <- list(
    engine_used = "bt",
    theta = tibble::tibble(
      ID = c("a", "b"),
      theta = c(0.25, -0.25),
      se_bt = c(0.10, 0.20),
      rank_running = c(1L, 2L)
    )
  )

  out <- pairwiseLLM:::.theta_from_last_running_fit(final_fit, id_vec = c("a", "b", "c"))

  expect_equal(out$engine, "bt")
  expect_s3_class(out$theta, "tbl_df")
  expect_equal(out$theta$ID, c("a", "b", "c"))
  expect_equal(out$theta$se[1:2], c(0.10, 0.20))
  expect_true(is.na(out$theta$se[3]))
  expect_equal(out$theta$rank[1:2], c(1L, 2L))
})

test_that("320-03 .theta_from_last_running_fit falls back to se_bt_firth and engine_requested", {
  final_fit <- list(
    engine_requested = "bt_firth",
    theta = tibble::tibble(
      ID = c("a", "b"),
      theta = c(0.1, 0.2),
      se_bt_firth = c(0.11, 0.22),
      rank = c(2L, 1L)
    )
  )

  out <- pairwiseLLM:::.theta_from_last_running_fit(final_fit, id_vec = c("a", "b"))

  expect_equal(out$engine, "bt_firth")
  expect_equal(out$theta$se, c(0.11, 0.22))
  expect_equal(out$theta$rank, c(2L, 1L))
})
