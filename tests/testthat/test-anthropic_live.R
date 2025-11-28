# =====================================================================
#   test-anthropic_live.R
#   Tests for anthropic_compare_pair_live() and submit_anthropic_pairs_live()
# =====================================================================

testthat::test_that("anthropic_compare_pair_live parses /v1/messages correctly", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_body <- list(
    model  = "claude-sonnet-4-5-20250929",
    id     = "msg_01XYZ",
    type   = "message",
    role   = "assistant",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Explanation.")
    ),
    usage = list(
      input_tokens  = 14L,
      output_tokens = 4L,
      total_tokens  = 18L
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    req_body_json      = function(req, body) req,
    req_perform        = function(req) structure(list(), class = "fake_resp"),
    resp_body_json     = function(...) fake_body,
    resp_status        = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        temperature       = 0,
        max_tokens        = 64,
        reasoning         = "none",
        include_raw       = TRUE
      )

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(nrow(res), 1L)

      testthat::expect_equal(res$custom_id, sprintf("LIVE_%s_vs_%s", ID1, ID2))
      testthat::expect_equal(res$ID1, ID1)
      testthat::expect_equal(res$ID2, ID2)

      testthat::expect_equal(res$model, "claude-sonnet-4-5-20250929")
      testthat::expect_equal(res$object_type, "message")
      testthat::expect_equal(res$status_code, 200L)
      testthat::expect_true(is.na(res$error_message))

      testthat::expect_equal(
        res$content,
        "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> Explanation."
      )

      testthat::expect_equal(res$better_sample, "SAMPLE_1")
      testthat::expect_equal(res$better_id, ID1)

      testthat::expect_equal(res$prompt_tokens,     14)
      testthat::expect_equal(res$completion_tokens, 4)
      testthat::expect_equal(res$total_tokens,      18)

      testthat::expect_true("raw_response" %in% names(res))
      testthat::expect_type(res$raw_response, "list")
      testthat::expect_equal(res$raw_response[[1]]$model, "claude-sonnet-4-5-20250929")
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live returns error row on JSON parse failure", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"; ID2 <- "S02"

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    req_body_json      = function(req, body) req,
    req_perform        = function(req) structure(list(), class = "fake_resp"),
    resp_body_json     = function(...) stop("boom"),
    resp_status        = function(...) 500L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = "X",
        ID2               = ID2,
        text2             = "Y",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = TRUE
      )

      testthat::expect_equal(res$status_code, 500L)
      testthat::expect_equal(res$error_message, "Failed to parse response body as JSON.")
      testthat::expect_true(is.na(res$better_sample))
      testthat::expect_true(is.null(res$raw_response[[1]]))
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("anthropic_compare_pair_live applies recommended defaults", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"; ID2 <- "S02"

  bodies <- list()

  fake_body <- list(
    model  = "claude-sonnet-4-5-20250929",
    id     = "msg_01XYZ",
    type   = "message",
    role   = "assistant",
    content = list(
      list(type = "text", text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")
    ),
    usage = list(
      input_tokens  = 10L,
      output_tokens = 4L,
      total_tokens  = 14L
    )
  )

  testthat::with_mocked_bindings(
    .anthropic_api_key = function(...) "FAKEKEY",
    req_body_json      = function(req, body) {
      bodies <<- append(bodies, list(body))
      req
    },
    req_perform        = function(req) structure(list(), class = "fake_resp"),
    resp_body_json     = function(...) fake_body,
    resp_status        = function(...) 200L,
    {
      res <- anthropic_compare_pair_live(
        ID1               = ID1,
        text1             = "A",
        ID2               = ID2,
        text2             = "B",
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = FALSE
        # note: no temperature or max_tokens supplied
      )

      testthat::expect_equal(res$better_id, ID2)

      # Check that body has our recommended defaults
      testthat::expect_equal(length(bodies), 1L)
      b <- bodies[[1]]

      testthat::expect_equal(b$model, "claude-sonnet-4-5")
      testthat::expect_equal(b$max_tokens, 768)
      testthat::expect_equal(b$temperature, 0)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("submit_anthropic_pairs_live returns empty tibble for zero rows", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  empty_pairs <- tibble::tibble(
    ID1   = character(0),
    text1 = character(0),
    ID2   = character(0),
    text2 = character(0)
  )

  res <- submit_anthropic_pairs_live(
    pairs             = empty_pairs,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl
  )

  testthat::expect_equal(nrow(res), 0L)
  testthat::expect_false("raw_response" %in% names(res))

  res_raw <- submit_anthropic_pairs_live(
    pairs             = empty_pairs,
    model             = "claude-sonnet-4-5",
    trait_name        = td$name,
    trait_description = td$description,
    prompt_template   = tmpl,
    include_raw       = TRUE
  )

  testthat::expect_equal(nrow(res_raw), 0L)
  testthat::expect_true("raw_response" %in% names(res_raw))
  testthat::expect_type(res_raw$raw_response, "list")
})

# ---------------------------------------------------------------------

testthat::test_that("submit_anthropic_pairs_live calls anthropic_compare_pair_live row-wise", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  calls <- list()

  fake_result <- function(ID1, ID2, chosen) {
    tibble::tibble(
      custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
      ID1               = ID1,
      ID2               = ID2,
      model             = "claude-sonnet-4-5-20250929",
      object_type       = "message",
      status_code       = 200L,
      error_message     = NA_character_,
      content           = sprintf("<BETTER_SAMPLE>%s</BETTER_SAMPLE>", chosen),
      better_sample     = chosen,
      better_id         = if (chosen == "SAMPLE_1") ID1 else ID2,
      prompt_tokens     = 10,
      completion_tokens = 5,
      total_tokens      = 15
    )
  }

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(
    ID1, text1, ID2, text2, model, trait_name,
    trait_description, prompt_template, api_key,
    anthropic_version, reasoning, include_raw, ...
    ) {
      calls <<- append(calls, list(list(ID1 = ID1, ID2 = ID2)))
      if (ID1 == "S01") fake_result(ID1, ID2, "SAMPLE_1") else fake_result(ID1, ID2, "SAMPLE_2")
    },
    {
      res <- submit_anthropic_pairs_live(
        pairs             = pairs,
        model             = "claude-sonnet-4-5",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        include_raw       = FALSE,
        verbose           = FALSE,
        progress          = FALSE
      )

      testthat::expect_equal(length(calls), 2L)
      testthat::expect_equal(res$better_id, c("S01", "S04"))
    }
  )
})
