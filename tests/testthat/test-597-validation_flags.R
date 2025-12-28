testthat::test_that("submit_llm_pairs forwards validate flags to backend submitter", {
  pairs <- tibble::tibble(
    ID1 = "A", text1 = "a",
    ID2 = "B", text2 = "b"
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  calls <- list()
  fake_out <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    failed_pairs = tibble::tibble()
  )

  testthat::with_mocked_bindings(
    submit_openai_pairs_live = function(...) {
      calls <<- append(calls, list(list(...)))
      fake_out
    },
    {
      out <- submit_llm_pairs(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "openai",
        endpoint          = "chat.completions",
        validate          = TRUE,
        validate_strict   = TRUE
      )

      testthat::expect_true(is.list(out))
      testthat::expect_equal(length(calls), 1L)
      testthat::expect_true(isTRUE(calls[[1]]$validate))
      testthat::expect_true(isTRUE(calls[[1]]$validate_strict))
    }
  )
})

testthat::test_that("llm_submit_pairs_batch can attach a validation_report when validate=TRUE", {
  pairs <- tibble::tibble(
    ID1 = "A", text1 = "a",
    ID2 = "B", text2 = "b"
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_pipeline_out <- list(
    results = tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A"),
    batch_input_path = tempfile(fileext = ".jsonl"),
    batch_output_path = tempfile(fileext = ".jsonl")
  )

  testthat::with_mocked_bindings(
    run_openai_batch_pipeline = function(...) fake_pipeline_out,
    {
      out <- llm_submit_pairs_batch(
        pairs = pairs,
        backend = "openai",
        model = "gpt-4.1",
        trait_name = td$name,
        trait_description = td$description,
        prompt_template = tmpl,
        validate = TRUE
      )

      testthat::expect_true(inherits(out, "pairwiseLLM_batch"))
      testthat::expect_true("validation_report" %in% names(out))
      testthat::expect_true(is.list(out$validation_report))
      testthat::expect_equal(out$validation_report$n_rows, 1L)
    }
  )
})
