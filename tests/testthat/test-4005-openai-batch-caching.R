cache_batch_args <- function(model = "gpt-5.6-luna") {
  list(
    pairs = tibble::tibble(
      ID1 = c("B", "A"), text1 = c("Second", "First"),
      ID2 = c("A", "B"), text2 = c("First", "Second"),
      pair_uid = c("forward", "reverse")
    ),
    model = model, trait_name = "quality", trait_description = "clear",
    prompt_template = "{TRAIT_NAME}: {SAMPLE_1} / {SAMPLE_2}"
  )
}

cache_output_line <- function(id = "forward", endpoint = "responses", details = list()) {
  tag <- "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>"
  if (endpoint == "responses") {
    body <- list(
      object = "response", model = "gpt-5.6-luna",
      output = list(list(type = "message", content = list(list(type = "output_text", text = tag)))),
      usage = list(input_tokens = 4000, output_tokens = 10, total_tokens = 4010,
                   input_tokens_details = details)
    )
  } else {
    body <- list(
      object = "chat.completion", model = "gpt-5.6-luna",
      choices = list(list(message = list(content = tag))),
      usage = list(prompt_tokens = 4000, completion_tokens = 10, total_tokens = 4010,
                   prompt_tokens_details = details)
    )
  }
  jsonlite::toJSON(list(custom_id = id, response = list(status_code = 200L, body = body)), auto_unbox = TRUE)
}

test_that("verified Batch models have exact per-request cache controls on both endpoints", {
  supported <- c("gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol",
                 "gpt-6-luna", "gpt-6-sol", "gpt-6-astra", "gpt-6.1-sol")
  path <- file.path(withr::local_tempdir(), "input.jsonl")
  for (model in supported) {
    for (endpoint in c("responses", "chat.completions")) {
      args <- c(cache_batch_args(model), list(endpoint = endpoint))
      for (policy in list(NULL, "disabled", "implicit")) {
        requests <- do.call(pairwiseLLM::build_openai_batch_requests,
                            c(args, list(prompt_caching = policy)))
        pairwiseLLM::write_openai_batch_file(requests, path)
        lines <- readLines(path)
        expect_length(lines, 2L)
        for (i in seq_along(lines)) {
          row <- jsonlite::fromJSON(lines[i], simplifyVector = FALSE)
          expect_named(row, c("custom_id", "method", "url", "body"))
          expect_identical(row$custom_id, c("forward", "reverse")[[i]])
          expect_identical(row$method, "POST")
          expect_identical(row$url, if (endpoint == "responses") "/v1/responses" else "/v1/chat/completions")
          expected <- list(model = model)
          prompt <- c("quality: Second / First", "quality: First / Second")[[i]]
          if (endpoint == "responses") {
            expected$input <- prompt
          } else {
            expected$messages <- list(list(role = "user", content = prompt))
          }
          if (!identical(policy, "implicit")) expected$prompt_cache_options <- list(mode = "explicit")
          expect_identical(row$body, expected)
          expect_false(grepl("prompt_cache_breakpoint", lines[i], fixed = TRUE))
        }
      }
    }
  }
})

test_that("earlier Batch request bodies retain fixed legacy fixtures", {
  legacy <- c("gpt-4.1", "gpt-4o-mini", "gpt-3.5-turbo-0125", "gpt-4-0613",
              "o3-mini", "gpt-5", "gpt-5-mini", "gpt-5.1", "gpt-5.5-pro",
              "gpt-5.1-2025-12-11", "gpt-5.4-2026-01-15")
  for (model in legacy) {
    for (endpoint in c("responses", "chat.completions")) {
      args <- c(cache_batch_args(model), list(endpoint = endpoint))
      expected <- list(model = model)
      if (endpoint == "responses") {
        expected$input <- "quality: Second / First"
      } else {
        expected$messages <- list(list(role = "user", content = "quality: Second / First"))
      }
      for (policy in list(NULL, "implicit")) {
        requests <- do.call(pairwiseLLM::build_openai_batch_requests,
                            c(args, list(prompt_caching = policy)))
        expect_identical(requests$body[[1]], expected)
      }
      expect_error(do.call(pairwiseLLM::build_openai_batch_requests,
                           c(args, list(prompt_caching = "disabled"))), "earlier OpenAI model")
    }
  }
  args <- cache_batch_args("gpt-4.1")
  args$pairs$pair_uid <- NULL
  expect_identical(do.call(pairwiseLLM::build_openai_batch_requests, args)$custom_id,
                   c("EXP_B_vs_A", "EXP_A_vs_B"))
})

test_that("cache policies and manual controls are validated even for empty inputs", {
  for (n in c(0L, 2L)) {
    args <- cache_batch_args()
    args$pairs <- args$pairs[seq_len(n), ]
    for (value in list(NA_character_, NA, TRUE, 1, "", "disable", "IMPLICIT",
                       character(), c("disabled", "implicit"), list("disabled"), matrix("disabled"))) {
      expect_error(do.call(pairwiseLLM::build_openai_batch_requests,
                           c(args, list(prompt_caching = value))), "`prompt_caching` must be")
    }
    for (model in list(NULL, NA_character_, "", "  ", 5, c("gpt-4", "gpt-5"), matrix("gpt-4"))) {
      args$model <- model
      # NULL as a list element must remain an explicit argument.
      args["model"] <- list(model)
      expect_error(do.call(pairwiseLLM::build_openai_batch_requests, args), "`model` must be")
    }
    args$model <- "gpt-5.6-luna"
    manual <- list(
      prompt_cache_options = list(mode = "explicit"),
      prompt_cache_breakpoint = list(mode = "explicit"),
      prompt_cache_key = "unchanged-key", prompt_cache_retention = "24h",
      prompt_cache_options.ttl = "30m", cache = FALSE
    )
    for (name in names(manual)) {
      for (policy in list(NULL, "disabled", "implicit")) {
        expect_error(do.call(pairwiseLLM::build_openai_batch_requests,
                             c(args, list(prompt_caching = policy), manual[name])), "manual cache controls")
      }
    }
    expect_error(do.call(pairwiseLLM::build_openai_batch_requests, c(args, list(typo = TRUE))),
                 "must be empty")
    result <- do.call(pairwiseLLM::build_openai_batch_requests, args)
    expect_equal(nrow(result), n)
    expect_named(result, c("custom_id", "method", "url", "body"))
  }
})

test_that("unlisted aliases and snapshots require deliberate implicit opt-in", {
  unknown <- c("gpt-5.6", "gpt-5.6-luna-2026-10-01", "gpt-5.7-sol", "gpt-7-sol",
               "gpt-6-terra", "gpt-5.5-magic", "ft:gpt-5.6-luna:custom", "custom-model",
               "GPT-5.6-LUNA", "gpt-5.6-luna ")
  for (model in unknown) {
    for (n in c(0L, 2L)) {
      args <- cache_batch_args(model)
      args$pairs <- args$pairs[seq_len(n), ]
      for (policy in list(NULL, "disabled")) {
        expect_error(do.call(pairwiseLLM::build_openai_batch_requests,
                             c(args, list(prompt_caching = policy))), "support is unknown")
      }
      requests <- do.call(pairwiseLLM::build_openai_batch_requests, c(args, list(prompt_caching = "implicit")))
      expect_equal(nrow(requests), n)
      if (n) expect_false("prompt_cache_options" %in% names(requests$body[[1]]))
    }
  }
  args <- c(cache_batch_args("custom-model"), list(prompt_caching = c(policy = "implicit")))
  expect_false("prompt_cache_options" %in% names(do.call(pairwiseLLM::build_openai_batch_requests, args)$body[[1]]))
})

test_that("all Batch submission paths reject cache errors before IO", {
  root <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    write_openai_batch_file = function(...) stop("unexpected write"),
    openai_upload_batch_file = function(...) stop("unexpected upload"),
    openai_create_batch = function(...) stop("unexpected submission"),
    .package = "pairwiseLLM"
  )
  cases <- list(
    list(model = "gpt-5.5", prompt_caching = "disabled"),
    list(model = "unknown"), list(prompt_caching = "bad"),
    list(prompt_cache_retention = "24h"),
    list(prompt_caching = "disabled", prompt_cache_options = list(mode = "implicit")),
    list(prompt_caching = "disabled", prompt_caching = "implicit")
  )
  for (n in c(0L, 2L)) {
    for (case in cases) {
      args <- cache_batch_args()
      args$pairs <- args$pairs[seq_len(n), ]
      if (!is.null(case$model)) args$model <- case$model
      case$model <- NULL
      args <- c(args, case)
      for (fun in list(pairwiseLLM::run_openai_batch_pipeline, pairwiseLLM::llm_submit_pairs_batch)) {
        expect_error(do.call(fun, c(args, list(poll = FALSE))),
                     "earlier OpenAI model|unknown|prompt_caching|manual cache controls")
      }
      output_dir <- file.path(root, "must-not-exist")
      expect_error(do.call(pairwiseLLM::llm_submit_pairs_multi_batch,
                           c(args, list(batch_size = 1L, output_dir = output_dir))),
                   "earlier OpenAI model|unknown|prompt_caching|manual cache controls")
      expect_false(dir.exists(output_dir))
    }
  }
})

test_that("single and generic Batch pipelines forward caching into actual JSONL", {
  root <- withr::local_tempdir()
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    openai_upload_batch_file = function(path, api_key) {
      seen$requests <- lapply(readLines(path), jsonlite::fromJSON, simplifyVector = FALSE)
      list(id = "file-fixture")
    },
    openai_create_batch = function(input_file_id, endpoint, completion_window, metadata, api_key) {
      expect_identical(metadata, list(label = "fixture"))
      expect_identical(input_file_id, "file-fixture")
      expect_identical(endpoint, seen$requests[[1]]$url)
      list(id = "batch-fixture")
    },
    .package = "pairwiseLLM"
  )
  for (fun in list(pairwiseLLM::run_openai_batch_pipeline, pairwiseLLM::llm_submit_pairs_batch)) {
    for (endpoint in c("responses", "chat.completions")) {
      for (policy in list(NULL, "disabled", "implicit")) {
        args <- c(cache_batch_args(), list(endpoint = endpoint, prompt_caching = policy,
                  store = FALSE, reasoning = "none", temperature = 0, top_p = 1, logprobs = TRUE,
                  poll = FALSE, metadata = list(label = "fixture"),
                  batch_input_path = file.path(root, "input.jsonl")))
        if (endpoint == "responses") args$max_output_tokens <- 64
        do.call(fun, args)
        expected <- do.call(pairwiseLLM::build_openai_batch_requests,
                            args[setdiff(names(args), c("poll", "metadata", "batch_input_path"))])
        expect_equal(lapply(seen$requests, `[[`, "body"), expected$body)
        expect_identical(vapply(seen$requests, `[[`, "", "custom_id"), expected$custom_id)
      }
    }
  }
})

test_that("segments and transient retries preserve the resolved caching policy", {
  root <- withr::local_tempdir()
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    openai_upload_batch_file = function(path, api_key) {
      seen$lines <- c(seen$lines, list(readLines(path)))
      list(id = "file-fixture")
    },
    openai_create_batch = function(...) {
      seen$attempt <- seen$attempt + 1L
      if (seen$attempt == 1L) rlang::abort("synthetic transient", class = "httr2_http_500")
      list(id = paste0("batch-", seen$attempt))
    },
    .package = "pairwiseLLM"
  )
  for (endpoint in c("responses", "chat.completions")) {
    for (policy in list(NULL, "disabled", "implicit")) {
      seen$lines <- list()
      seen$attempt <- 0L
      args <- c(cache_batch_args(), list(endpoint = endpoint, prompt_caching = policy))
      expect_message(jobs <- do.call(pairwiseLLM::llm_submit_pairs_multi_batch,
        c(args, list(batch_size = 1L, output_dir = root, openai_max_retries = 2L))), "Transient error")
      expect_length(jobs$jobs, 2L)
      expect_length(seen$lines, 3L)
      expect_identical(seen$lines[[1]], seen$lines[[2]])
      expected <- do.call(pairwiseLLM::build_openai_batch_requests, args)
      for (i in seq_len(2L)) {
        body <- jsonlite::fromJSON(seen$lines[[i + 1L]], simplifyVector = FALSE)$body
        expect_identical(body, expected$body[[i]])
      }
    }
  }
})

test_that("both endpoint usage schemas preserve reads, writes, zeros and missing counters", {
  path <- file.path(withr::local_tempdir(), "output.jsonl")
  for (endpoint in c("responses", "chat.completions")) {
    lines <- c(
      cache_output_line(endpoint = endpoint, details = list(cached_tokens = 1024, cache_write_tokens = 2048)),
      cache_output_line(endpoint = endpoint, details = list(cached_tokens = 0, cache_write_tokens = 0)),
      cache_output_line(endpoint = endpoint, details = list(cached_tokens = 512)),
      cache_output_line(endpoint = endpoint, details = list(cache_write_tokens = 256)),
      cache_output_line(endpoint = endpoint),
      '{"custom_id":"EXP_A_vs_B","error":{"message":"failed"}}'
    )
    writeLines(lines, path)
    parsed <- pairwiseLLM::parse_openai_batch_output(path)
    expect_identical(parsed$prompt_cached_tokens, c(1024, 0, 512, NA, NA, NA))
    expect_identical(parsed$prompt_cache_write_tokens, c(2048, 0, NA, 256, NA, NA))
    expect_identical(parsed$prompt_tokens, c(rep(4000, 5), NA_real_))
    expect_identical(parsed$completion_tokens, c(rep(10, 5), NA_real_))
    expect_identical(parsed$total_tokens, c(rep(4010, 5), NA_real_))
  }
  # Preserve the historical input_tokens_details precedence for mixed legacy records.
  obj <- jsonlite::fromJSON(cache_output_line(), simplifyVector = FALSE)
  obj$response$body$usage$input_tokens_details <- list(cached_tokens = 100, cache_write_tokens = 200)
  obj$response$body$usage$prompt_tokens_details <- list(cached_tokens = 300, cache_write_tokens = 400)
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE), path)
  parsed <- pairwiseLLM::parse_openai_batch_output(path)
  expect_identical(parsed$prompt_cached_tokens, 100)
  expect_identical(parsed$prompt_cache_write_tokens, 200)
  for (endpoint in c("responses", "chat.completions")) {
    obj <- jsonlite::fromJSON(cache_output_line(endpoint = endpoint), simplifyVector = FALSE)
    details_name <- if (endpoint == "responses") "input_tokens_details" else "prompt_tokens_details"
    obj$response$body$usage <- setNames(list(list(cached_tokens = 10, cache_write_tokens = 20)), details_name)
    writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE), path)
    parsed <- pairwiseLLM::parse_openai_batch_output(path)
    expect_equal(nrow(parsed), 1L)
    expect_identical(parsed$prompt_tokens, NA_real_)
    expect_identical(parsed$completion_tokens, NA_real_)
    expect_identical(parsed$total_tokens, NA_real_)
    expect_identical(parsed$prompt_cached_tokens, 10)
    expect_identical(parsed$prompt_cache_write_tokens, 20)
  }
})

test_that("cache counts survive polling through single and normalized generic pipelines", {
  root <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    openai_upload_batch_file = function(...) list(id = "file-fixture"),
    openai_create_batch = function(...) list(id = "batch-fixture"),
    openai_poll_batch_until_complete = function(...) list(id = "batch-fixture", status = "completed"),
    openai_download_batch_output = function(batch_id, path, api_key) {
      writeLines(c(
        cache_output_line("forward", details = list(cached_tokens = 0, cache_write_tokens = 3000)),
        cache_output_line("reverse", "chat.completions", list(cached_tokens = 3000, cache_write_tokens = 0))
      ), path)
    },
    .package = "pairwiseLLM"
  )
  for (fun in list(pairwiseLLM::run_openai_batch_pipeline, pairwiseLLM::llm_submit_pairs_batch)) {
    result <- do.call(fun, c(cache_batch_args(), list(
      batch_input_path = file.path(root, "input.jsonl"),
      batch_output_path = file.path(root, "output.jsonl")
    )))
    expect_identical(result$results$prompt_cached_tokens, c(0, 3000))
    expect_identical(result$results$prompt_cache_write_tokens, c(3000, 0))
  }
})

test_that("registry resume preserves cache usage without classifying or rebuilding old requests", {
  skip_if_not_installed("readr")
  root <- withr::local_tempdir()
  args <- cache_batch_args()
  pairs_path <- file.path(root, "pairs.rds")
  saveRDS(args$pairs, pairs_path)
  input_path <- file.path(root, "frozen-input.jsonl")
  writeLines("frozen historical request", input_path)
  readr::write_csv(tibble::tibble(
    segment_index = 1L, provider = "openai", model = "unlisted-historical-model",
    batch_id = "old-batch", batch_input_path = input_path,
    batch_output_path = file.path(root, "output.jsonl"),
    csv_path = file.path(root, "results.csv"), pairs_path = pairs_path, done = FALSE
  ), file.path(root, "jobs_registry.csv"))
  testthat::local_mocked_bindings(
    .openai_batch_cache_policy = function(...) stop("must not reclassify"),
    build_openai_batch_requests = function(...) stop("must not rebuild"),
    openai_upload_batch_file = function(...) stop("must not upload"),
    openai_create_batch = function(...) stop("must not submit"),
    openai_get_batch = function(...) list(status = "completed"),
    .openai_download_batch_output = function(batch_id, path, max_attempts) {
      writeLines(c(
        cache_output_line("forward", details = list(cached_tokens = 0, cache_write_tokens = 3000)),
        cache_output_line("reverse", "chat.completions", list(cached_tokens = 3000))
      ), path)
    },
    .package = "pairwiseLLM"
  )
  result <- pairwiseLLM::llm_resume_multi_batches(
    output_dir = root, interval_seconds = 0, per_job_delay = 0,
    write_results_csv = TRUE, write_combined_csv = TRUE, keep_jsonl = TRUE
  )
  expect_identical(readLines(input_path), "frozen historical request")
  expect_true(result$jobs[[1]]$done)
  expect_identical(result$combined$prompt_cached_tokens, c(0, 3000))
  expect_identical(result$combined$prompt_cache_write_tokens, c(3000, NA_real_))
  expect_identical(result$combined$better_id, c("B", "A"))
  csv <- readr::read_csv(file.path(root, "results.csv"), show_col_types = FALSE)
  expect_identical(csv$prompt_cache_write_tokens, c(3000, NA_real_))
  expect_identical(csv$prompt_cached_tokens, c(0, 3000))
})

test_that("the documented synthetic cost comparison uses separate rates and keeps unknown costs", {
  path <- testthat::test_path("..", "..", "vignettes", "advanced-batch-workflows.Rmd")
  skip_if_not(file.exists(path), "Vignette source is absent from the installed test bundle")
  lines <- readLines(path)
  start <- which(lines == "```{r openai-cache-cost-example}")
  expect_length(start, 1L)
  end <- which(seq_along(lines) > start & lines == "```")[1L]
  env <- new.env(parent = baseenv())
  invisible(eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), env))
  expect_equal(env$cache_costs$ordinary_tokens, c(1000, 1000, NA))
  expect_equal(env$cache_costs$input_charge, c(0.00475, 0.0013, NA))
  expect_equal(env$cache_costs$uncached_input_charge, rep(0.004, 3))
  expect_true(is.na(sum(env$cache_costs$input_charge)))
})
