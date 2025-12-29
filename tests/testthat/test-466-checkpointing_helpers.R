test_that("checkpointing helpers write, read, and validate payloads", {
  tmp <- withr::local_tempdir()

  payload1 <- list(
    run_type = "adaptive",
    ids = c("A", "B", "C"),
    next_round = 2L,
    completed = FALSE,
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")
  )

  path1 <- pairwiseLLM:::.bt_write_checkpoint(tmp, payload1, basename = "run_state", overwrite = TRUE)
  expect_true(file.exists(path1))

  chk1 <- pairwiseLLM:::.bt_read_checkpoint(tmp, basename = "run_state")
  expect_identical(chk1$run_type, payload1$run_type)
  expect_identical(chk1$ids, payload1$ids)
  expect_true(is.data.frame(chk1$results))

  # overwrite=FALSE should preserve existing payload
  payload2 <- payload1
  payload2$next_round <- 99L
  pairwiseLLM:::.bt_write_checkpoint(tmp, payload2, basename = "run_state", overwrite = FALSE)
  chk2 <- pairwiseLLM:::.bt_read_checkpoint(tmp, basename = "run_state")
  expect_identical(chk2$next_round, payload1$next_round)

  # per-round snapshot naming
  path_r1 <- pairwiseLLM:::.bt_write_checkpoint(tmp, payload1, basename = "run_state", round = 1L, overwrite = TRUE)
  expect_true(file.exists(path_r1))
  expect_match(basename(path_r1), "run_state_round_001\\.rds$")

  # validate success
  expect_invisible(pairwiseLLM:::.bt_validate_checkpoint(chk1, run_type = "adaptive", ids = c("A", "B", "C")))

  # validate errors
  expect_error(
    pairwiseLLM:::.bt_validate_checkpoint(chk1, run_type = "core_linking", ids = c("A", "B", "C")),
    "Resume checkpoint does not match `run_type`\\.(.|\\n)*Requested:(.|\\n)*Checkpoint:"
  )
  expect_error(
    pairwiseLLM:::.bt_validate_checkpoint(chk1, run_type = "adaptive", ids = c("A", "B", "D")),
    "`samples\\$ID`"
  )
  expect_error(
    pairwiseLLM:::.bt_validate_checkpoint("not a list", run_type = "adaptive", ids = c("A", "B", "C")),
    "not a list"
  )

  # read error when missing
  tmp2 <- withr::local_tempdir()
  expect_error(
    pairwiseLLM:::.bt_read_checkpoint(tmp2, basename = "run_state"),
    "No checkpoint found"
  )
})
