# =====================================================================
# test-openai_batch_api.R
# Tests for internal and exported helpers in R/openai_batch_api.R
# =====================================================================

# ---------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------

testthat::test_that("Internal helpers work", {
  # .openai_api_key prioritization
  withr::with_envvar(c("OPENAI_API_KEY" = "env_key"), {
    testthat::expect_equal(.openai_api_key(), "env_key")
    testthat::expect_equal(.openai_api_key("explicit"), "explicit")
  })

  # .openai_base_url
  testthat::expect_equal(.openai_base_url(), "https://api.openai.com/v1")

  # .openai_request construction
  # Note: The source uses httr2::request, so we mock in "httr2"
  testthat::with_mocked_bindings(
    request = function(base_url) paste0("REQ:", base_url),
    req_auth_bearer_token = function(req, token) paste0(req, "|AUTH:", token),
    {
      res <- .openai_request("/test", api_key = "ABC")
      testthat::expect_equal(res, "REQ:https://api.openai.com/v1/test|AUTH:ABC")
    },
    .package = "httr2"
  )

  # .openai_req_body_json wrapper
  testthat::with_mocked_bindings(
    req_body_json = function(req, body, ...) list(req = req, body = body),
    {
      res <- .openai_req_body_json("REQ", list(a = 1))
      testthat::expect_equal(res, list(req = "REQ", body = list(a = 1)))
    },
    .package = "httr2"
  )

  # .openai_req_perform wrapper (calls .retry_httr2_request)
  # We mock the internal retry function (local to package)
  testthat::with_mocked_bindings(
    .retry_httr2_request = function(req) "RESP",
    {
      testthat::expect_equal(.openai_req_perform("REQ"), "RESP")
    }
  )

  # .openai_resp_body_json wrapper
  testthat::with_mocked_bindings(
    resp_body_json = function(resp, ...) "PARSED",
    {
      testthat::expect_equal(.openai_resp_body_json("RESP"), "PARSED")
    },
    .package = "httr2"
  )

  # .openai_resp_status wrapper
  testthat::with_mocked_bindings(
    resp_status = function(resp) 200,
    {
      testthat::expect_equal(.openai_resp_status("RESP"), 200)
    },
    .package = "httr2"
  )
})

# ---------------------------------------------------------------------
# Exported functions
# ---------------------------------------------------------------------

testthat::test_that("openai_upload_batch_file validates file existence and uploads", {
  tf <- tempfile()
  file.create(tf)
  on.exit(unlink(tf))

  # 1. Missing file check
  testthat::expect_error(
    openai_upload_batch_file("missing_file.jsonl"),
    "File does not exist"
  )

  # 2. Happy path
  # We mock the internal helper .openai_request and imported httr2 functions
  # strictly within the package namespace (default)
  captured_req <- NULL

  testthat::with_mocked_bindings(
    .openai_request = function(...) "REQ",
    req_body_multipart = function(...) "REQ_MULTI",
    req_perform = function(req) {
      captured_req <<- req
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) list(id = "file_1"),
    {
      res <- openai_upload_batch_file(tf, purpose = "batch", api_key = "k")
      testthat::expect_equal(res$id, "file_1")
      testthat::expect_equal(captured_req, "REQ_MULTI")
    }
  )
})

testthat::test_that("openai_create_batch constructs request correctly", {
  captured_body <- NULL

  # 1. Metadata NULL
  testthat::with_mocked_bindings(
    .openai_request = function(...) "REQ",
    req_body_json = function(req, body) {
      captured_body <<- body
      "REQ_BODY"
    },
    req_perform = function(...) "RESP",
    resp_body_json = function(...) list(id = "batch_1"),
    {
      res <- openai_create_batch("file_1", "/url", api_key = "k")
      testthat::expect_equal(res$id, "batch_1")
      testthat::expect_null(captured_body$metadata)
    }
  )

  # 2. Metadata present
  testthat::with_mocked_bindings(
    .openai_request = function(...) "REQ",
    req_body_json = function(req, body) {
      captured_body <<- body
      "REQ_BODY"
    },
    req_perform = function(...) "RESP",
    resp_body_json = function(...) list(id = "batch_1"),
    {
      res <- openai_create_batch("file_1", "/url", metadata = list(a = 1), api_key = "k")
      testthat::expect_equal(res$id, "batch_1")
      testthat::expect_equal(captured_body$metadata$a, 1)
    }
  )
})

testthat::test_that("openai_get_batch builds path correctly", {
  testthat::with_mocked_bindings(
    .openai_request = function(path, ...) {
      testthat::expect_true(grepl("batch_123", path))
      "REQ"
    },
    req_perform = function(...) "RESP",
    resp_body_json = function(...) list(id = "batch_123", status = "completed"),
    {
      res <- openai_get_batch("batch_123", api_key = "k")
      testthat::expect_equal(res$status, "completed")
    }
  )
})

testthat::test_that("openai_download_batch_output handles validation and download", {
  # 1. No output_file_id check
  testthat::with_mocked_bindings(
    openai_get_batch = function(...) list(id = "b1", status = "failed"),
    {
      testthat::expect_error(
        openai_download_batch_output("b1", "path"),
        "has no output_file_id"
      )
    }
  )

  # 2. Success path
  tf <- tempfile()
  on.exit(unlink(tf))
  testthat::with_mocked_bindings(
    openai_get_batch = function(...) list(id = "b1", output_file_id = "f1"),
    .openai_request = function(path, ...) {
      testthat::expect_equal(path, "/files/f1/content")
      "REQ"
    },
    req_perform = function(...) "RESP",
    resp_body_raw = function(...) charToRaw("AB"),
    {
      out <- openai_download_batch_output("b1", tf, api_key = "k")
      testthat::expect_equal(out, tf)
      testthat::expect_true(file.exists(tf))
      testthat::expect_equal(readLines(tf, warn = FALSE), "AB")
    }
  )
})

testthat::test_that("openai_poll_batch_until_complete handles loops and limits", {
  # 1. Immediate success
  testthat::with_mocked_bindings(
    openai_get_batch = function(...) list(id = "b1", status = "completed"),
    {
      res <- openai_poll_batch_until_complete("b1", interval = 0, verbose = FALSE)
      testthat::expect_equal(res$status, "completed")
    }
  )

  # 2. Max attempts exceeded
  cnt <- 0
  testthat::with_mocked_bindings(
    openai_get_batch = function(...) {
      cnt <<- cnt + 1
      list(id = "b1", status = "validating")
    },
    {
      testthat::expect_error(
        openai_poll_batch_until_complete("b1", interval = 0, max_attempts = 2, verbose = FALSE),
        "Reached max_attempts"
      )
      testthat::expect_equal(cnt, 2)
    }
  )

  # 3. Timeout exceeded
  testthat::with_mocked_bindings(
    openai_get_batch = function(...) list(id = "b1", status = "validating"),
    {
      testthat::expect_error(
        openai_poll_batch_until_complete("b1", interval = 0, timeout_seconds = 0, verbose = FALSE),
        "Timeout"
      )
    }
  )
})

testthat::test_that("run_openai_batch_pipeline exercises all steps", {
  pairs <- tibble::tibble(ID1 = "a", text1 = "a", ID2 = "b", text2 = "b")

  # 1. Endpoint auto-selection (include_thoughts=TRUE -> responses)
  #    and poll=FALSE branch
  testthat::with_mocked_bindings(
    build_openai_batch_requests = function(...) {
      args <- list(...)
      if (!is.null(args$endpoint) && args$endpoint == "responses") {
        return(tibble::tibble(jsonl = "json"))
      }
      stop("Wrong endpoint selected")
    },
    write_openai_batch_file = function(...) NULL,
    openai_upload_batch_file = function(...) list(id = "f1"),
    openai_create_batch = function(...) list(id = "b1", status = "validating"),
    {
      res <- run_openai_batch_pipeline(pairs, "mod", "tr", "desc", include_thoughts = TRUE, poll = FALSE)
      testthat::expect_equal(res$batch$id, "b1")
      testthat::expect_null(res$results)
    }
  )

  # 2. poll=TRUE branch (download + parse)
  testthat::with_mocked_bindings(
    build_openai_batch_requests = function(...) tibble::tibble(jsonl = "json"),
    write_openai_batch_file = function(...) NULL,
    openai_upload_batch_file = function(...) list(id = "f1"),
    openai_create_batch = function(...) list(id = "b1", status = "validating"),
    openai_poll_batch_until_complete = function(...) list(id = "b1", status = "completed"),
    openai_download_batch_output = function(...) NULL,
    parse_openai_batch_output = function(...) tibble::tibble(res = 1),
    {
      res <- run_openai_batch_pipeline(pairs, "mod", "tr", "desc", poll = TRUE, interval_seconds = 0)
      testthat::expect_equal(res$results$res, 1)
    }
  )
})

testthat::test_that("build_openai_batch_requests sets default reasoning for gpt-5 (Line 659)", {
  td <- trait_description("overall_quality") # Fixed: valid trait name
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  res <- build_openai_batch_requests(
    pairs,
    model = "gpt-5.1",
    trait_name = td$name,
    trait_description = td$description,
    endpoint = "responses",
    include_thoughts = TRUE,
    reasoning = NULL # This triggers Line 659
  )

  body <- res$body[[1]]
  testthat::expect_equal(body$reasoning$effort, "low")
  testthat::expect_equal(body$reasoning$summary, "auto")
})

testthat::test_that("write_openai_batch_file handles edge cases", {
  # Covers Lines 771-789
  tf <- tempfile()

  # 1. Invalid jsonl column type (Line 773)
  bad_jsonl <- tibble::tibble(jsonl = 1:3)
  testthat::expect_error(
    write_openai_batch_file(bad_jsonl, tf),
    "must be a character vector"
  )

  # 2. Missing columns (Line 780)
  bad_cols <- tibble::tibble(custom_id = "1")
  testthat::expect_error(
    write_openai_batch_file(bad_cols, tf),
    "must have either"
  )

  # 3. Empty table (Line 789)
  empty_tbl <- tibble::tibble(
    custom_id = character(), method = character(),
    url = character(), body = list()
  )
  write_openai_batch_file(empty_tbl, tf)
  testthat::expect_equal(readLines(tf), character(0))
})

testthat::test_that("parse_openai_batch_output handles ID parsing edge cases", {
  # Covers parse_ids logic (Lines 925, 943)
  tf <- tempfile()

  # Line 1: NA custom_id -> ID1=NA, ID2=NA (Line 925)
  # Line 2: No underscore prefix "A_vs_B" -> ID1="A" (Line 943)
  lines <- c(
    '{"custom_id": null, "response": {"status_code": 400}}',
    '{"custom_id": "A_vs_B", "response": {"status_code": 200, "body": {"object": "chat.completion"}}}'
  )
  writeLines(lines, tf)

  res <- parse_openai_batch_output(tf)

  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_true(is.na(res$ID1[1]))
  testthat::expect_equal(res$ID1[2], "A")
  testthat::expect_equal(res$ID2[2], "B")
})

testthat::test_that("parse_openai_batch_output extracts error messages", {
  # Covers Line 982
  tf <- tempfile()
  json <- '{"custom_id": "1", "error": {"message": "Something went wrong"}, "response": null}'
  writeLines(json, tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_equal(res$error_message, "Something went wrong")
})

testthat::test_that("parse_openai_batch_output handles empty content", {
  # Covers Line 1098
  tf <- tempfile()
  # Object with no content blocks
  json <- '{"custom_id": "1", "response": {"body": {"object": "response", "output": []}}}'
  writeLines(json, tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_true(is.na(res$content))
})

testthat::test_that("parse_openai_batch_output handles files where all lines fail parse", {
  # Covers Line 1165 (Filter returns empty list -> return empty tibble)
  tf <- tempfile()
  # Valid JSON but missing "response" or structure that leads to NULL in the loop
  # OR invalid JSON lines that are caught by tryCatch (error=function(e) NULL)
  writeLines(c("{invalid_json", "{also_invalid"), tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
})

testthat::test_that("build_openai_batch_requests sets default reasoning for gpt-5 (Line 659)", {
  td <- trait_description("overall_quality") # Fixed: valid trait name
  pairs <- tibble::tibble(ID1 = "A", text1 = "a", ID2 = "B", text2 = "b")

  res <- build_openai_batch_requests(
    pairs,
    model = "gpt-5.1",
    trait_name = td$name,
    trait_description = td$description,
    endpoint = "responses",
    include_thoughts = TRUE,
    reasoning = NULL # This triggers Line 659
  )

  body <- res$body[[1]]
  testthat::expect_equal(body$reasoning$effort, "low")
  testthat::expect_equal(body$reasoning$summary, "auto")
})

testthat::test_that("write_openai_batch_file handles validation edge cases (Lines 771-789)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # 1. Invalid jsonl column type (Lines 771-773)
  bad_jsonl <- tibble::tibble(jsonl = 1:3)
  testthat::expect_error(
    write_openai_batch_file(bad_jsonl, tf),
    "must be a character vector"
  )

  # 2. Missing columns (Lines 780-784)
  bad_cols <- tibble::tibble(custom_id = "1")
  testthat::expect_error(
    write_openai_batch_file(bad_cols, tf),
    "must have either"
  )

  # 3. Empty table (Line 789)
  empty_tbl <- tibble::tibble(
    custom_id = character(), method = character(),
    url = character(), body = list()
  )
  write_openai_batch_file(empty_tbl, tf)
  # File should be created but empty
  testthat::expect_true(file.exists(tf))
  testthat::expect_equal(readLines(tf), character(0))
})

testthat::test_that("parse_openai_batch_output handles ID parsing edge cases (Lines 925, 943)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # Line 1: NA custom_id -> Triggers Line 925
  # Line 2: "A_vs_B" (no prefix underscore) -> Triggers Line 943 (id1 <- left)
  lines <- c(
    '{"custom_id": null, "response": {"status_code": 400}}',
    '{"custom_id": "A_vs_B", "response": {"status_code": 200, "body": {"object": "chat.completion"}}}'
  )
  writeLines(lines, tf)

  res <- parse_openai_batch_output(tf)

  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_true(is.na(res$ID1[1]))
  testthat::expect_equal(res$ID1[2], "A")
  testthat::expect_equal(res$ID2[2], "B")
})

testthat::test_that("parse_openai_batch_output extracts error messages (Line 982)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  json <- '{"custom_id": "1", "error": {"message": "Something went wrong"}, "response": null}'
  writeLines(json, tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_equal(res$error_message, "Something went wrong")
})

testthat::test_that("parse_openai_batch_output handles varied reasoning structures (Lines 1038-1051)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # 1. Summary as List of objects (Lines 1038-1044)
  # This matches the standard behavior of jsonlite::fromJSON(simplifyVector = FALSE)
  # when the JSON is an array of objects: [{"text": "ListSummary"}]
  json_list <- '{"custom_id": "1", "response": {"body": {"object": "response", "output": [{"type": "reasoning", "summary": [{"text": "ListSummary"}]}]}}}'
  writeLines(json_list, tf)

  res_list <- parse_openai_batch_output(tf)
  testthat::expect_equal(res_list$thoughts, "ListSummary")

  # 2. Summary as Data Frame (Lines 1047-1051)
  # Standard jsonlite::fromJSON(simplifyVector=FALSE) returns lists, not DFs.
  # We use mocking to force the parser to return a structure where `summary` is a DF,
  # ensuring coverage of the `else if (is.data.frame(rs))` block.
  mock_obj <- list(
    custom_id = "2",
    response = list(
      body = list(
        object = "response",
        output = list(
          list(
            type = "reasoning",
            summary = data.frame(text = "DFSummary", stringsAsFactors = FALSE)
          )
        )
      )
    )
  )

  # We rewrite the file to have 1 line so the loop runs once.
  # The content doesn't matter because fromJSON is mocked.
  writeLines('{"dummy": "json"}', tf)

  testthat::with_mocked_bindings(
    fromJSON = function(...) mock_obj,
    .package = "jsonlite",
    {
      res_df <- parse_openai_batch_output(tf)
      testthat::expect_equal(res_df$thoughts, "DFSummary")
    }
  )
})

testthat::test_that("parse_openai_batch_output handles empty content (Line 1098)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # Object with no content blocks in output
  json <- '{"custom_id": "1", "response": {"body": {"object": "response", "output": []}}}'
  writeLines(json, tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_true(is.na(res$content))
})

testthat::test_that("parse_openai_batch_output returns empty tibble if all lines fail (Line 1165)", {
  tf <- tempfile()
  on.exit(unlink(tf))

  # Invalid JSON lines that get caught by tryCatch -> return NULL -> filtered out
  writeLines(c("{invalid_json", "{also_invalid"), tf)

  res <- parse_openai_batch_output(tf)
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
})
