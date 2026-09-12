test_that("batch builders retain request identity and reject incompatible thinking", {
  pairs <- tibble::tibble(ID1 = "a", ID2 = "b", text1 = "one", text2 = "two", pair_uid = "exact-id")
  args <- list(pairs = pairs, model = "fixture", trait_name = "quality", trait_description = "quality")
  out <- do.call(pairwiseLLM:::build_openai_batch_requests, args)
  expect_identical(out$custom_id, "exact-id")
  args$model <- "claude-haiku-4-5"
  out <- do.call(pairwiseLLM:::build_anthropic_batch_requests,
    c(args, list(reasoning = "enabled", temperature = 1, thinking_budget_tokens = 1024L,
      max_tokens = 2048L)))
  expect_equal(out$params[[1]]$temperature, 1)
  expect_error(do.call(pairwiseLLM:::build_anthropic_batch_requests,
    c(args, list(reasoning = "enabled", thinking_budget_tokens = 1024L, max_tokens = 1024L))),
    "smaller than")
  args$model <- "gemini-3-pro-preview"
  expect_error(do.call(pairwiseLLM:::build_gemini_batch_requests,
    c(args, list(thinking_level = "minimal"))), "only supported.*Flash")
  expect_error(do.call(pairwiseLLM::run_gemini_batch_pipeline,
    c(args, list(thinking_level = "minimal"))), "only supported.*Flash")
  expect_error(pairwiseLLM::run_gemini_batch_pipeline(model = ""), "non-empty")
  args$model <- "gemini-3-flash-preview"
  out <- do.call(pairwiseLLM:::build_gemini_batch_requests,
    c(args, list(thinking_level = "minimal")))
  expect_match(jsonlite::toJSON(out, auto_unbox = TRUE), "Minimal")
})

test_that("batch parsers handle alternate content containers and absent files", {
  tag <- "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"
  parse <- pairwiseLLM:::.parse_gemini_pair_response
  contents <- list(list(parts = list(tag)), tag,
    data.frame(parts = I(list(list(list(text = tag))))),
    list(parts = list(data.frame(text = tag, thoughtSignature = "signature"))))
  for (content in contents) {
    out <- parse("id", "a", "b", list(candidates = list(list(content = content))))
    expect_identical(out$better_id, "a")
  }
  for (content in list(NULL, list(parts = list(NULL, 1)), list(parts = NULL))) {
    out <- parse("id", "a", "b", list(candidates = list(list(content = content))))
    expect_true(is.na(out$better_id))
  }
  out <- pairwiseLLM:::.parse_anthropic_pair_message(
    list(content = list(list(type = "text", text = tag))), "a", "b")
  expect_identical(out$better_id, "a")
  expect_true(is.na(pairwiseLLM:::.parse_ids_from_custom_id("EXP_missing-separator")$ID1))
  root <- withr::local_tempdir()
  missing <- file.path(root, "absent.jsonl")
  expect_error(pairwiseLLM:::parse_anthropic_batch_output(missing), "does not exist")
  expect_error(pairwiseLLM:::parse_gemini_batch_output(missing, data.frame()), "does not exist")
  writeLines("", missing)
  expect_error(pairwiseLLM:::parse_gemini_batch_output(missing, data.frame()), "requests_tbl")
  expect_equal(nrow(pairwiseLLM:::parse_anthropic_batch_output(missing)), 0L)
})

test_that("terminal polling returns immediately with auditable status", {
  testthat::local_mocked_bindings(
    openai_get_batch = function(...) list(id = "openai-fixture", status = "completed"),
    anthropic_get_batch = function(...) list(id = "anthropic-fixture", processing_status = "ended"),
    .anthropic_sleep = function(...) stop("terminal poll must not sleep"),
    .package = "pairwiseLLM")
  expect_message(out <- pairwiseLLM::openai_poll_batch_until_complete("openai-fixture", verbose = TRUE),
    "status: completed")
  expect_identical(out$status, "completed")
  out <- pairwiseLLM::anthropic_poll_batch_until_complete("anthropic-fixture", verbose = FALSE)
  expect_identical(out$processing_status, "ended")
})

test_that("batch dispatch validates results and materializes declared output paths", {
  for (backend in list(character(), NA_character_, "")) {
    expect_error(pairwiseLLM::llm_submit_pairs_batch(backend = backend), "non-empty")
    expect_error(pairwiseLLM::llm_submit_pairs_multi_batch(backend = backend), "non-empty")
  }
  expect_error(pairwiseLLM::llm_download_batch_results(structure(list(), class = "pairwiseLLM_batch")),
    "results")
  root <- withr::local_tempdir()
  response <- new.env(parent = emptyenv())
  response$value <- "bad"
  testthat::local_mocked_bindings(run_openai_batch_pipeline = function(..., endpoint) {
    expect_identical(endpoint, "responses")
    response$value
  }, .package = "pairwiseLLM")
  args <- list(pairs = tibble::tibble(ID1 = "a", ID2 = "b", text1 = "one", text2 = "two"),
    backend = "openai", model = "fixture", trait_name = "quality", trait_description = "quality",
    endpoint = "responses")
  expect_error(do.call(pairwiseLLM::llm_submit_pairs_batch, args), "did not return a list")
  response$value <- list(results = tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a"),
    batch_input_path = file.path(root, "input"),
    batch_output_path = file.path(root, "output"))
  out <- do.call(pairwiseLLM::llm_submit_pairs_batch, args)
  expect_true(file.exists(out$batch_input_path))
  expect_true(file.exists(out$batch_output_path))
  expect_s3_class(out, "pairwiseLLM_batch")
})

test_that("failed-attempt normalization retains caller evidence identities", {
  f <- pairwiseLLM:::.pairwiseLLM_failed_attempts_from_pairs
  expect_error(f(data.frame(ID1 = "a"), "fixture", "m", "error", "detail"), "Missing: ID2")
  pairs <- tibble::tibble(ID1 = c("a", "b"), ID2 = c("b", "a"),
    pair_uid = c("forward", "reverse"), phase = "pilot", iter = c(2, 3))
  out <- f(pairs, "fixture", "m", "error", "detail", as.POSIXct("2026-09-11", tz = "UTC"))
  expect_identical(out$pair_uid, pairs$pair_uid)
  expect_identical(out$ordered_key, c("a:b", "b:a"))
  expect_identical(out$phase, pairs$phase)
  expect_identical(out$iter, c(2L, 3L))
})
