test_that(".bt_align_state returns template for NULL or empty input", {
  tmpl <- pairwiseLLM:::.bt_state_template()
  out1 <- pairwiseLLM:::.bt_align_state(NULL)
  expect_identical(names(out1), names(tmpl))
  expect_equal(nrow(out1), 0L)

  out2 <- pairwiseLLM:::.bt_align_state(tibble::tibble())
  expect_identical(names(out2), names(tmpl))
  expect_equal(nrow(out2), 0L)
})

test_that(".bt_align_state adds missing cols with correct types and coerces existing cols", {
  state <- tibble::tibble(
    batch_index = "1", # should become integer
    min_appearances = "2.5", # should become double
    stop = "TRUE", # should become logical
    stage = factor("live"), # should become character
    extra_col = "keepme"
  )

  out <- pairwiseLLM:::.bt_align_state(state)

  tmpl <- pairwiseLLM:::.bt_state_template()

  # Template columns first, then extras
  expect_true(all(names(tmpl) == names(out)[seq_along(names(tmpl))]))
  expect_true("extra_col" %in% names(out))

  # Coercions happened
  expect_true(is.integer(out$batch_index))
  expect_true(is.double(out$min_appearances))
  expect_true(is.logical(out$stop))
  expect_true(is.character(out$stage))

  expect_equal(out$batch_index, 1L)
  expect_equal(out$min_appearances, 2.5)
  expect_equal(out$stop, TRUE)
  expect_equal(out$stage, "live")

  # Missing columns exist with correct storage types
  expect_true("n_results" %in% names(out))
  expect_true(is.integer(out$n_results) || is.double(out$n_results)) # integer column in template -> integer
  expect_true(all(is.na(out$n_results)))
})
