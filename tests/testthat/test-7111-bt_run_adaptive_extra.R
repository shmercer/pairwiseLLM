test_that("bt_run_adaptive can resume from a completed checkpoint", {
  tmp <- withr::local_tempdir()

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("t1", "t2")
  )

  results <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A",
    better_sample = "SAMPLE_1",
    model = "m"
  )

  chk <- list(
    run_type = "adaptive",
    ids = c("A", "B"),
    completed = TRUE,
    results = results,
    out = list(
      results = results,
      bt_data = tibble::tibble(object1 = "A", object2 = "B", result = 1),
      fits = list(),
      final_fit = NULL,
      estimates = NULL,
      final_models = NULL,
      stop_reason = "completed",
      stop_round = 1L,
      rounds = tibble::tibble(),
      state = tibble::tibble(),
      pairs_bootstrap = tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
    )
  )

  saveRDS(chk, file.path(tmp, "run_state.rds"), compress = "xz")

  out <- pairwiseLLM::bt_run_adaptive(
    samples = samples,
    model = "m",
    trait_name = "t",
    trait_description = "d",
    judge_fun = function(...) {
      stop("judge_fun should not be called when resuming a completed checkpoint")
    },
    resume_from = tmp,
    api_key = "k",
    max_rounds = 1L,
    verbose = FALSE
  )

  # Package uses the S3 class `pairwiseLLM_run` for run outputs.
  expect_s3_class(out, "pairwiseLLM_run")
  expect_equal(nrow(out$results), 1L)
  expect_equal(out$stop_reason, "completed")
})
