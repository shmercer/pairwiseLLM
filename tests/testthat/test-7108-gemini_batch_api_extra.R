test_that(".parse_gemini_pair_response handles candidates as data.frame and parts coercions", {
  resp <- list(
    candidates = data.frame(
      content = I(list(list(parts = data.frame(text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>", stringsAsFactors = FALSE)))),
      stringsAsFactors = FALSE
    ),
    modelVersion = "m",
    usageMetadata = list(promptTokenCount = 1, candidatesTokenCount = 2)
  )

  out <- pairwiseLLM:::.parse_gemini_pair_response(
    custom_id = "c1",
    ID1 = "A",
    ID2 = "B",
    response = resp,
    include_thoughts = FALSE
  )

  expect_equal(out$custom_id[[1]], "c1")
  expect_equal(out$better_id[[1]], "A")
})

test_that("gemini_get_batch uses expected path and gemini_download_batch_results validates custom_id", {
  testthat::local_mocked_bindings(
    .gemini_request = function(path, api_key = NULL, ...) list(path = path, api_key = api_key),
    .gemini_req_perform = function(req) req,
    .gemini_resp_body_json = function(resp, ...) list(ok = TRUE),
    .env = asNamespace("pairwiseLLM")
  )

  b <- pairwiseLLM::gemini_get_batch("batches/123", api_key = "k")
  expect_true(is.list(b))

  bad_tbl <- tibble::tibble(ID1 = "A", ID2 = "B")
  expect_error(
    pairwiseLLM::gemini_download_batch_results(
      batch = list(name = "batches/123", response = list(inlinedResponses = list(inlinedResponses = list()))),
      requests_tbl = bad_tbl
    ),
    "custom_id",
    fixed = FALSE
  )
})

test_that("run_gemini_batch_pipeline prints create/poll messages", {
  pairs <- tibble::tibble(
    custom_id = "c1",
    ID1 = "A",
    ID2 = "B",
    text1 = "t1",
    text2 = "t2"
  )

  tmp <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    build_gemini_batch_requests = function(...) tibble::tibble(custom_id = "c1"),
    gemini_create_batch = function(...) list(name = "batches/123"),
    gemini_poll_batch_until_complete = function(...) list(name = "batches/123", response = list(inlinedResponses = list(inlinedResponses = list()))),
    gemini_download_batch_results = function(...) list(),
    parse_gemini_batch_output = function(...) tibble::tibble(custom_id = "c1"),
    .env = asNamespace("pairwiseLLM")
  )

  expect_message(
    pairwiseLLM::run_gemini_batch_pipeline(
      pairs = pairs,
      model = "gemini-1.5-pro",
      trait_name = "t",
      trait_description = "d",
      api_key = "k",
      output_dir = tmp,
      poll = TRUE,
      verbose = TRUE
    ),
    "Polling",
    fixed = TRUE
  )
})
