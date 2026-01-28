testthat::test_that("refit progress block prints header and stop info", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("alpha", "bravo", "charlie")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L), seed = 1)
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(progress = TRUE, progress_every_refit = 1L, progress_level = "refit")
  )
  state$phase <- "phase2"
  state$batch_log <- tibble::tibble(iter = 4L, phase = "phase2")

  round_row <- pairwiseLLM:::.adaptive_round_log_defaults()
  round_row$round_id <- 1L
  round_row$iter_at_refit <- 1L
  round_row$total_pairs <- 3L
  round_row$new_pairs <- 1L
  round_row$stop_decision <- FALSE
  round_row$stop_reason <- NA_character_

  out <- capture.output({
    pairwiseLLM:::.adaptive_progress_emit_refit(state, round_row)
  })
  text <- paste(out, collapse = "\n")

  testthat::expect_true(grepl("\\[adaptive pairing\\] REFIT 1", text))
  testthat::expect_true(grepl("phase=phase2", text))
  testthat::expect_true(grepl("stop_decision : FALSE", text))
})
