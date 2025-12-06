# =====================================================================
#   test-ollama_live.R
#   Tests for ollama_compare_pair_live() and submit_ollama_pairs_live()
# =====================================================================

# ---------------------------------------------------------------------
# ollama_compare_pair_live: happy path
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live parses successful response
                    correctly", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_body <- list(
    model = "mistral-small3.2:24b",
    response =
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation.",
    prompt_eval_count = 10L,
    eval_count = 5L
  )

  captured_body <- NULL

  testthat::with_mocked_bindings(
    # We only need to intercept these; request/req_url_path_append/req_error
    # can remain as in the real code.
    req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    req_perform = function(req) structure(list(), class = "fake_resp"),
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        num_ctx           = 8192L,
        think             = FALSE,
        include_raw       = TRUE
      )

      # Basic shape
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$custom_id, sprintf("LIVE_%s_vs_%s", ID1, ID2))
      testthat::expect_equal(res$ID1, ID1)
      testthat::expect_equal(res$ID2, ID2)

      testthat::expect_equal(res$model, "mistral-small3.2:24b")
      testthat::expect_equal(res$object_type, "ollama.generate")
      testthat::expect_equal(res$status_code, 200L)
      testthat::expect_true(is.na(res$error_message))

      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Some explanation."
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)

      # Token counts from prompt_eval_count + eval_count
      testthat::expect_equal(res$prompt_tokens, 10)
      testthat::expect_equal(res$completion_tokens, 5)
      testthat::expect_equal(res$total_tokens, 15)

      # raw_response
      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_type(res$raw_response, "list")
      testthat::expect_equal(
        res$raw_response[[1]]$model, "mistral-small3.2:24b"
      )

      # Request body sanity checks
      testthat::expect_type(captured_body, "list")
      testthat::expect_equal(captured_body$model, "mistral-small3.2:24b")
      testthat::expect_false(isTRUE(captured_body$stream))

      # Default context window + temperature for non-Qwen models
      testthat::expect_equal(captured_body$options$num_ctx, 8192L)
      testthat::expect_equal(captured_body$options$temperature, 0)
    }
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: Qwen + think = TRUE → temperature = 0.6
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live sets Qwen temperature
                    when think = TRUE", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text A"
  text2 <- "Text B"

  fake_body <- list(
    model             = "qwen3:32b",
    response          = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> Explanation.",
    prompt_eval_count = 4L,
    eval_count        = 6L
  )

  captured_body <- NULL

  testthat::with_mocked_bindings(
    req_body_json = function(req, body) {
      captured_body <<- body
      req
    },
    req_perform = function(req) structure(list(), class = "fake_resp"),
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    resp_status = function(resp) 200L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "qwen3:32b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        think             = TRUE,
        num_ctx           = 4096L,
        include_raw       = FALSE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$better_sample, "SAMPLE_2")
      testthat::expect_equal(res$better_id, ID2)

      # Temperature logic: Qwen + think = TRUE → 0.6
      testthat::expect_type(captured_body, "list")
      testthat::expect_equal(captured_body$model, "qwen3:32b")
      testthat::expect_equal(captured_body$options$num_ctx, 4096L)
      testthat::expect_equal(captured_body$options$temperature, 0.6)
    }
  )
})

# ---------------------------------------------------------------------
# ollama_compare_pair_live: JSON parse failure
# ---------------------------------------------------------------------

testthat::test_that("ollama_compare_pair_live returns error row on
                    JSON parse failure", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"

  testthat::with_mocked_bindings(
    req_body_json = function(req, body) req,
    req_perform = function(req) structure(list(), class = "fake_resp"),
    resp_body_json = function(resp, simplifyVector = FALSE) stop("boom"),
    resp_status = function(resp) 500L,
    {
      res <- ollama_compare_pair_live(
        ID1               = ID1,
        text1             = "X",
        ID2               = ID2,
        text2             = "Y",
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        include_raw       = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(
        res$error_message,
        "Failed to parse response body as JSON."
      )
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.na(res$better_id))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: zero rows
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live returns empty tibble
                    for zero rows", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res <- submit_ollama_pairs_live(
    pairs             = empty_pairs,
    model             = "mistral-small3.2:24b",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    host              = "http://127.0.0.1:11434",
    verbose           = FALSE,
    progress          = FALSE
  )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_true("thoughts" %in% names(res))
  testthat::expect_false("raw_response" %in% names(res))

  # With include_raw = TRUE, empty tibble still has raw_response column
  res2 <- submit_ollama_pairs_live(
    pairs             = empty_pairs,
    model             = "mistral-small3.2:24b",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    host              = "http://127.0.0.1:11434",
    verbose           = FALSE,
    progress          = FALSE,
    include_raw       = TRUE
  )

  testthat::expect_s3_class(res2, "tbl_df")
  testthat::expect_equal(nrow(res2), 0L)
  testthat::expect_true("raw_response" %in% names(res2))
  testthat::expect_type(res2$raw_response, "list")
})

# ---------------------------------------------------------------------
# submit_ollama_pairs_live: row-wise calling of ollama_compare_pair_live
# ---------------------------------------------------------------------

testthat::test_that("submit_ollama_pairs_live calls
                    ollama_compare_pair_live row-wise", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = "mistral-small3.2:24b",
      object_type       = "ollama.generate",
      status_code       = 200L,
      error_message     = NA_character_,
      thoughts          = NA_character_,
      content           = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample     = chosen,
      better_id         = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens     = 10,
      completion_tokens = 5,
      total_tokens      = 15
    )
  }

  calls <- list()

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      host,
      tag_prefix,
      tag_suffix,
      think,
      num_ctx,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        host              = host,
        think             = think,
        num_ctx           = num_ctx,
        include_raw       = include_raw,
        dots              = list(...)
      )))

      if (ID1 == "S01") {
        fake_result(ID1, ID2, "SAMPLE_1")
      } else {
        fake_result(ID1, ID2, "SAMPLE_2")
      }
    },
    {
      res <- submit_ollama_pairs_live(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        host              = "http://127.0.0.1:11434",
        verbose           = FALSE,
        status_every      = 1,
        progress          = FALSE,
        include_raw       = FALSE,
        think             = FALSE,
        num_ctx           = 8192L
      )

      # Should call helper once per row
      testthat::expect_equal(length(calls), 2L)
      testthat::expect_equal(calls[[1]]$ID1, "S01")
      testthat::expect_equal(calls[[2]]$ID1, "S03")

      # Shared parameters should be forwarded correctly
      for (call in calls) {
        testthat::expect_equal(call$model, "mistral-small3.2:24b")
        testthat::expect_equal(call$trait_name, td$name)
        testthat::expect_equal(call$trait_description, td$description)
        testthat::expect_identical(call$prompt_template, tmpl)
        testthat::expect_equal(call$host, "http://127.0.0.1:11434")
        testthat::expect_false(call$think)
        testthat::expect_equal(call$num_ctx, 8192L)
        testthat::expect_false(call$include_raw)
      }

      # Aggregated result should be consistent with fake_result logic
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res$better_id, c("S01", "S04"))
    }
  )
})
