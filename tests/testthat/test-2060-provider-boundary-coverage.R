test_that("provider response adapters decode real in-memory HTTP responses", {
  response <- httr2::response(status_code = 200L,
    headers = list(`content-type` = "application/json"), body = charToRaw('{"value":7}'))
  for (provider in c("anthropic", "gemini", "together")) {
    body <- get(paste0(".", provider, "_resp_body_json"), asNamespace("pairwiseLLM"))
    status <- get(paste0(".", provider, "_resp_status"), asNamespace("pairwiseLLM"))
    expect_identical(status(response), 200L)
    expect_equal(body(response)$value, 7)
  }
  testthat::local_mocked_bindings(.retry_httr2_request = function(req) response,
    .package = "pairwiseLLM")
  expect_identical(pairwiseLLM:::.gemini_req_perform(httr2::request("https://example.invalid")),
    response)
})

test_that("live backends reject malformed pair fields before requesting credentials", {
  args <- list(ID1 = "a", ID2 = "b", text1 = "one", text2 = "two", model = "fixture",
    trait_name = "quality", trait_description = "quality")
  for (provider in c("anthropic", "together")) {
    f <- get(paste0(provider, "_compare_pair_live"), asNamespace("pairwiseLLM"))
    for (field in c("ID2", "text1", "text2")) {
      bad <- args
      bad[[field]] <- 1
      expect_error(do.call(f, bad), field, fixed = TRUE)
    }
  }
  expect_error(do.call(pairwiseLLM::openai_compare_pair_live,
    c(args, list(max_output_tokens = 10L, endpoint = "chat.completions"))), "Responses endpoint")
  args$model <- "gemini-3-pro-preview"
  expect_error(do.call(pairwiseLLM::gemini_compare_pair_live,
    c(args, list(thinking_level = "minimal"))), "only supported.*Flash")
  for (backend in list(character(), NA_character_, "")) {
    expect_error(pairwiseLLM::llm_compare_pair(backend = backend), "non-empty character")
    expect_error(pairwiseLLM::submit_llm_pairs(backend = backend), "non-empty character")
  }
})

test_that("Gemini Flash failure rows retain raw-response and thinking contracts", {
  request_body <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    .gemini_req_body_json = function(req, body) {
      request_body$value <- body
      req
    },
    .gemini_req_perform = function(req) stop("offline failure"),
    .package = "pairwiseLLM")
  out <- pairwiseLLM::gemini_compare_pair_live("a", "one", "b", "two",
    model = "gemini-3-flash-preview", trait_name = "quality", trait_description = "quality",
    api_key = "fixture-key", thinking_level = "minimal", include_raw = TRUE)
  expect_identical(request_body$value$generationConfig$thinkingConfig$thinkingLevel, "Minimal")
  expect_match(out$error_message, "offline failure")
  expect_identical(out$raw_response, list(NULL))
  expect_true(is.na(out$better_id))
})

test_that("legacy pair_uid CSV resumes skip only the already saved pair", {
  pairs <- tibble::tibble(ID1 = c("a", "c"), ID2 = c("b", "d"), text1 = "one", text2 = "two")
  root <- withr::local_tempdir()
  called <- new.env(parent = emptyenv())
  compare <- function(ID1, ID2, ...) {
    called$ids <- c(called$ids, ID1)
    tibble::tibble(custom_id = paste0("EXP_", ID1, "_vs_", ID2),
      ID1 = ID1, ID2 = ID2, better_id = ID1, status_code = 200L, error_message = NA_character_,
      raw_response = list(list(content = "fixture")))
  }
  testthat::local_mocked_bindings(openai_compare_pair_live = compare,
    anthropic_compare_pair_live = compare, together_compare_pair_live = compare,
    ollama_compare_pair_live = compare, .package = "pairwiseLLM")
  for (provider in c("openai", "anthropic", "together", "ollama")) {
    for (legacy_key in c("pair_uid", "unrelated")) {
      path <- file.path(root, paste0(provider, "-", legacy_key, ".csv"))
      old <- tibble::tibble(ID1 = "a", ID2 = "b", better_id = "a", status_code = 200L)
      old[[legacy_key]] <- pairwiseLLM:::.pairwiseLLM_make_custom_id("a", "b")
      readr::write_csv(old, path)
      called$ids <- character()
      f <- get(paste0("submit_", provider, "_pairs_live"), asNamespace("pairwiseLLM"))
      out <- f(pairs, model = "fixture", trait_name = "quality", trait_description = "quality",
        save_path = path, parallel = FALSE, verbose = FALSE, progress = FALSE)
      expect_identical(called$ids, if (legacy_key == "pair_uid") "c" else c("a", "c"))
      expect_true(nrow(out$results) >= 1L)
    }
  }
})

test_that("Ollama process inspection handles blank output without stopping models", {
  testthat::local_mocked_bindings(.ollama_system2 = function(...) c("", " "),
    .package = "pairwiseLLM")
  expect_message(out <- pairwiseLLM::ensure_only_ollama_model_loaded("fixture", verbose = TRUE),
    "No non-empty output")
  expect_identical(out, character())
})
