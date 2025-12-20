# =====================================================================
#   test-llm_backends.R
#   Tests for llm_compare_pair() and submit_llm_pairs()
# =====================================================================

# ---------------------------------------------------------------------
# Defaults: backend and endpoint
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair uses default backend and endpoint", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "gpt-4.1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1,
                                        text1,
                                        ID2,
                                        text2,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        endpoint,
                                        api_key,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      # Note: backend and endpoint are omitted, using defaults
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Defaults should resolve to backend = "openai", endpoint =
      # "chat.completions"
      testthat::expect_equal(call$endpoint, "chat.completions")
      # api_key default is from Sys.getenv("OPENAI_API_KEY"); we only check that
      # an argument was passed through (value may vary by environment).
      testthat::expect_true("api_key" %in% names(call))

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("submit_llm_pairs uses default backend and endpoint", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "gpt-4.1",
    object_type = "chat.completion",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c(NA_character_, NA_character_),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(10, 11),
    completion_tokens = c(5, 6),
    total_tokens = c(15, 17)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_openai_pairs_live = function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        endpoint,
                                        api_key,
                                        verbose,
                                        status_every,
                                        progress,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      # Note: backend and endpoint are omitted, using defaults
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Defaults should resolve to backend = "openai", endpoint =
      # "chat.completions"
      testthat::expect_equal(call$endpoint, "chat.completions")
      testthat::expect_true("api_key" %in% names(call))
      # Defaults for verbose / status_every / progress / include_raw
      testthat::expect_true(call$verbose)
      testthat::expect_equal(call$status_every, 1L)
      testthat::expect_true(call$progress)
      testthat::expect_false(call$include_raw)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: anthropic backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to anthropic backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "claude-3-5-sonnet-latest",
    object_type       = "message",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Some internal reasoning.",
    content           = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_2",
    better_id         = ID2,
    prompt_tokens     = 20,
    completion_tokens = 10,
    total_tokens      = 30
  )

  calls <- list()

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(ID1,
                                           text1,
                                           ID2,
                                           text2,
                                           model,
                                           trait_name,
                                           trait_description,
                                           prompt_template,
                                           api_key,
                                           include_raw,
                                           ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = "ANTH_KEY",
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Should not receive an endpoint argument for anthropic
      testthat::expect_false("endpoint" %in% names(call))

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "claude-3-5-sonnet-latest")
      testthat::expect_equal(call$api_key, "ANTH_KEY")
      # include_thoughts should be forwarded via ...
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("llm_compare_pair uses Anthropic env var when
                    api_key is NULL", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "claude-3-5-sonnet-latest",
    object_type       = "message",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()
  old_env <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_env), add = TRUE)
  Sys.setenv(ANTHROPIC_API_KEY = "ENV_ANTH_KEY")

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(ID1,
                                           text1,
                                           ID2,
                                           text2,
                                           model,
                                           trait_name,
                                           trait_description,
                                           prompt_template,
                                           api_key,
                                           include_raw,
                                           ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = NULL,
        include_raw       = FALSE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # When api_key is NULL, llm_compare_pair should fall back to the env var
      testthat::expect_true(is.null(call$api_key))

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to anthropic backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "claude-3-5-sonnet-latest",
    object_type = "message",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Thoughts 1", "Thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(20, 22),
    completion_tokens = c(10, 12),
    total_tokens = c(30, 34)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_anthropic_pairs_live = function(pairs,
                                           model,
                                           trait_name,
                                           trait_description,
                                           prompt_template,
                                           api_key,
                                           verbose,
                                           status_every,
                                           progress,
                                           include_raw,
                                           ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = "ANTH_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "claude-3-5-sonnet-latest")
      testthat::expect_equal(call$api_key, "ANTH_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: gemini backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to gemini backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "gemini-2.0-pro-exp",
    object_type       = "generateContent",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Gemini thinking text.",
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 30,
    completion_tokens = 15,
    total_tokens      = 45
  )

  calls <- list()

  testthat::with_mocked_bindings(
    gemini_compare_pair_live = function(ID1,
                                        text1,
                                        ID2,
                                        text2,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        api_key,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gemini-2.0-pro-exp",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "gemini",
        api_key           = "GEMINI_KEY",
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Should not receive an endpoint argument for gemini
      testthat::expect_false("endpoint" %in% names(call))

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "gemini-2.0-pro-exp")
      testthat::expect_equal(call$api_key, "GEMINI_KEY")
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to gemini backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "gemini-2.0-pro-exp",
    object_type = "generateContent",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Gemini thoughts 1", "Gemini thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(30, 32),
    completion_tokens = c(15, 17),
    total_tokens = c(45, 49)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_gemini_pairs_live = function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        api_key,
                                        verbose,
                                        status_every,
                                        progress,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "gemini-2.0-pro-exp",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "gemini",
        api_key           = "GEMINI_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "gemini-2.0-pro-exp")
      testthat::expect_equal(call$api_key, "GEMINI_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: ollama backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to ollama backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 50,
    completion_tokens = 20,
    total_tokens      = 70
  )

  calls <- list()

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(ID1,
                                        text1,
                                        ID2,
                                        text2,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "ollama",
        include_raw       = TRUE,
        host              = "http://127.0.0.1:11434",
        think             = TRUE,
        num_ctx           = 16384
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Basic routing and core args
      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "mistral-small3.2:24b")
      testthat::expect_true(call$include_raw)

      # Ollama backend should not receive endpoint or api_key
      testthat::expect_false("endpoint" %in% names(call))
      testthat::expect_false("api_key" %in% names(call))

      # ... should carry Ollama-specific options
      testthat::expect_true("host" %in% names(call$dots))
      testthat::expect_true("think" %in% names(call$dots))
      testthat::expect_true("num_ctx" %in% names(call$dots))

      testthat::expect_equal(call$dots$host, "http://127.0.0.1:11434")
      testthat::expect_true(call$dots$think)
      testthat::expect_equal(call$dots$num_ctx, 16384)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to ollama backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "mistral-small3.2:24b",
    object_type = "ollama.generate",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c(NA_character_, NA_character_),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(50, 52),
    completion_tokens = c(20, 22),
    total_tokens = c(70, 74)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_ollama_pairs_live = function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        verbose,
                                        status_every,
                                        progress,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "ollama",
        verbose           = FALSE,
        status_every      = 2,
        progress          = FALSE,
        include_raw       = TRUE,
        host              = "http://127.0.0.1:11434",
        think             = FALSE,
        num_ctx           = 8192
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "mistral-small3.2:24b")
      testthat::expect_false(call$verbose)
      testthat::expect_equal(call$status_every, 2L)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)

      # Ollama-specific options should be forwarded via ...
      testthat::expect_true("host" %in% names(call$dots))
      testthat::expect_true("think" %in% names(call$dots))
      testthat::expect_true("num_ctx" %in% names(call$dots))

      testthat::expect_equal(call$dots$host, "http://127.0.0.1:11434")
      testthat::expect_false(call$dots$think)
      testthat::expect_equal(call$dots$num_ctx, 8192)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: together backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to together backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "deepseek-ai/DeepSeek-R1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Some internal reasoning.",
    content           = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_2",
    better_id         = ID2,
    prompt_tokens     = 20,
    completion_tokens = 10,
    total_tokens      = 30
  )

  calls <- list()

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(ID1,
                                          text1,
                                          ID2,
                                          text2,
                                          model,
                                          trait_name,
                                          trait_description,
                                          prompt_template,
                                          tag_prefix = "<BETTER_SAMPLE>",
                                          tag_suffix = "</BETTER_SAMPLE>",
                                          api_key,
                                          include_raw = FALSE,
                                          ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        tag_prefix        = tag_prefix,
        tag_suffix        = tag_suffix,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = "TOGETHER_KEY",
        include_raw       = TRUE,
        temperature       = 0.6
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "deepseek-ai/DeepSeek-R1")
      testthat::expect_equal(call$trait_name, td$name)
      testthat::expect_equal(call$trait_description, td$description)
      testthat::expect_equal(call$api_key, "TOGETHER_KEY")
      testthat::expect_true(call$include_raw)

      # Temperature should be forwarded via ...
      testthat::expect_true("temperature" %in% names(call$dots))
      testthat::expect_equal(call$dots$temperature, 0.6)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("llm_compare_pair passes NULL api_key to Together
                    backend when api_key is NULL", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "deepseek-ai/DeepSeek-R1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()
  old_env <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  on.exit(Sys.setenv(TOGETHER_API_KEY = old_env), add = TRUE)
  Sys.setenv(TOGETHER_API_KEY = "ENV_TOGETHER_KEY")

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(ID1,
                                          text1,
                                          ID2,
                                          text2,
                                          model,
                                          trait_name,
                                          trait_description,
                                          prompt_template,
                                          tag_prefix = "<BETTER_SAMPLE>",
                                          tag_suffix = "</BETTER_SAMPLE>",
                                          api_key,
                                          include_raw = FALSE,
                                          ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = NULL,
        include_raw       = FALSE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # At this level, api_key should still be NULL; the env var is handled
      # inside together_compare_pair_live() via .together_api_key().
      testthat::expect_true(is.null(call$api_key))

      testthat::expect_false(call$include_raw)
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to together backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "deepseek-ai/DeepSeek-R1",
    object_type = "chat.completion",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Thoughts 1", "Thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(20, 22),
    completion_tokens = c(10, 12),
    total_tokens = c(30, 34)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_together_pairs_live = function(pairs,
                                          model,
                                          trait_name,
                                          trait_description,
                                          prompt_template,
                                          api_key,
                                          verbose,
                                          status_every,
                                          progress,
                                          include_raw,
                                          ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = "TOGETHER_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        temperature       = 0.6
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "deepseek-ai/DeepSeek-R1")
      testthat::expect_equal(call$api_key, "TOGETHER_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)

      # Temperature forwarded via ...
      testthat::expect_true("temperature" %in% names(call$dots))
      testthat::expect_equal(call$dots$temperature, 0.6)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("llm_compare_pair prioritizes explicit api_key over environment", {
  # We mock the specific backend function (e.g. openai) to capture arguments
  captured_args <- NULL

  mock_openai <- function(...) {
    captured_args <<- list(...)
    tibble::tibble(custom_id = "id", status_code = 200) # minimal return
  }

  # Set a dummy env var
  withr::with_envvar(c("OPENAI_API_KEY" = "ENV_KEY"), {
    testthat::with_mocked_bindings(
      openai_compare_pair_live = mock_openai,
      {
        # Call generic wrapper with explicit key
        llm_compare_pair(
          "A", "txt", "B", "txt", "gpt-4", "trait", "desc",
          backend = "openai",
          api_key = "EXPLICIT_KEY"
        )
      }
    )
  })

  testthat::expect_equal(captured_args$api_key, "EXPLICIT_KEY")
})

testthat::test_that("llm_compare_pair forwards '...' arguments correctly", {
  captured_args <- NULL

  mock_anthropic <- function(...) {
    captured_args <<- list(...)
    tibble::tibble(custom_id = "id", status_code = 200)
  }

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = mock_anthropic,
    {
      llm_compare_pair(
        "A", "txt", "B", "txt", "claude", "trait", "desc",
        backend = "anthropic",
        # Extra args passed via ...
        temperature = 0.7,
        top_p = 0.9,
        max_tokens = 100
      )
    }
  )

  # Check that dots were forwarded
  testthat::expect_equal(captured_args$temperature, 0.7)
  testthat::expect_equal(captured_args$top_p, 0.9)
  testthat::expect_equal(captured_args$max_tokens, 100)
})

# Tests for backend-agnostic live comparison helpers and retry logic

test_that("llm_compare_pair dispatches to the correct backend helper", {
  # Create dummy values for a single pair
  ID1 <- "id1"
  text1 <- "text one"
  ID2 <- "id2"
  text2 <- "text two"
  model <- "fake-model"
  trait_name <- "Trait"
  trait_description <- "A trait description"
  tmpl <- set_prompt_template()
  # Prepare return objects for each backend
  openai_ret <- tibble::tibble(backend = "openai")
  anthropic_ret <- tibble::tibble(backend = "anthropic")
  gemini_ret <- tibble::tibble(backend = "gemini")
  together_ret <- tibble::tibble(backend = "together")
  ollama_ret <- tibble::tibble(backend = "ollama")
  # Helper to record the last call arguments
  last_args <- list()
  # Use with_mocked_bindings to stub each backend function
  res <- with_mocked_bindings(
    openai_compare_pair_live = function(...) {
      last_args <<- list(fn = "openai", args = list(...))
      openai_ret
    },
    anthropic_compare_pair_live = function(...) {
      last_args <<- list(fn = "anthropic", args = list(...))
      anthropic_ret
    },
    gemini_compare_pair_live = function(...) {
      last_args <<- list(fn = "gemini", args = list(...))
      gemini_ret
    },
    together_compare_pair_live = function(...) {
      last_args <<- list(fn = "together", args = list(...))
      together_ret
    },
    ollama_compare_pair_live = function(...) {
      last_args <<- list(fn = "ollama", args = list(...))
      ollama_ret
    },
    {
      # Test each backend dispatch
      # OpenAI with explicit endpoint
      res_openai <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "openai", endpoint = "chat.completions",
                                     temperature = 0.7)
      expect_equal(res_openai$backend, "openai")
      # Ensure the correct backend helper was called and temperature forwarded
      expect_identical(last_args$fn, "openai")
      expect_true("temperature" %in% names(last_args$args))
      # Anthropic
      res_anthropic <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                        trait_name, trait_description,
                                        prompt_template = tmpl,
                                        backend = "anthropic", reasoning = "low")
      expect_equal(res_anthropic$backend, "anthropic")
      expect_identical(last_args$fn, "anthropic")
      expect_true("reasoning" %in% names(last_args$args))
      # Gemini
      res_gemini <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "gemini", include_thoughts = TRUE)
      expect_equal(res_gemini$backend, "gemini")
      expect_identical(last_args$fn, "gemini")
      expect_true("include_thoughts" %in% names(last_args$args))
      # Together
      res_together <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                       trait_name, trait_description,
                                       prompt_template = tmpl,
                                       backend = "together", api_key = "key", include_raw = TRUE)
      expect_equal(res_together$backend, "together")
      expect_identical(last_args$fn, "together")
      expect_true("api_key" %in% names(last_args$args))
      expect_true(last_args$args$include_raw)
      # Ollama
      res_ollama <- llm_compare_pair(ID1, text1, ID2, text2, model,
                                     trait_name, trait_description,
                                     prompt_template = tmpl,
                                     backend = "ollama", host = "http://localhost:11434")
      expect_equal(res_ollama$backend, "ollama")
      expect_identical(last_args$fn, "ollama")
      expect_true("host" %in% names(last_args$args))
      # Unsupported backend should error
      expect_error(
        llm_compare_pair(ID1, text1, ID2, text2, model,
                         trait_name, trait_description,
                         prompt_template = tmpl,
                         backend = "invalid"),
        "should be one of \"openai\", \"anthropic\", \"gemini\", \"together\", \"ollama\""
      )
    }
  )
})

test_that("submit_llm_pairs dispatches to correct backend helper", {
  # Create a simple pairs tibble
  pairs_tbl <- tibble::tibble(ID1 = c("a","b"), text1 = c("a1","b1"),
                              ID2 = c("c","d"), text2 = c("c1","d1"))
  trait_name <- "Quality"
  trait_description <- "Description"
  model <- "modelX"
  tmpl <- set_prompt_template()
  last_call <- list()
  # Stubs for backend-specific submit helpers
  with_mocked_bindings(
    submit_openai_pairs_live = function(...) {
      last_call <<- list(fn = "openai", args = list(...))
      tibble::tibble(provider = "openai")
    },
    submit_anthropic_pairs_live = function(...) {
      last_call <<- list(fn = "anthropic", args = list(...))
      tibble::tibble(provider = "anthropic")
    },
    submit_gemini_pairs_live = function(...) {
      last_call <<- list(fn = "gemini", args = list(...))
      tibble::tibble(provider = "gemini")
    },
    submit_together_pairs_live = function(...) {
      last_call <<- list(fn = "together", args = list(...))
      tibble::tibble(provider = "together")
    },
    submit_ollama_pairs_live = function(...) {
      last_call <<- list(fn = "ollama", args = list(...))
      tibble::tibble(provider = "ollama")
    },
    {
      # Test each backend
      res_openai <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "openai",
                                     endpoint = "responses", verbose = FALSE, status_every = 1,
                                     include_raw = TRUE)
      expect_equal(res_openai$provider, "openai")
      expect_equal(last_call$fn, "openai")
      expect_true(last_call$args$include_raw)
      # Anthropic
      res_anthropic <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                        prompt_template = tmpl, backend = "anthropic",
                                        verbose = FALSE)
      expect_equal(res_anthropic$provider, "anthropic")
      expect_equal(last_call$fn, "anthropic")
      # Gemini
      res_gemini <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "gemini",
                                     verbose = FALSE)
      expect_equal(res_gemini$provider, "gemini")
      expect_equal(last_call$fn, "gemini")
      # Together
      res_together <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                       prompt_template = tmpl, backend = "together",
                                       verbose = FALSE, status_every = 1)
      expect_equal(res_together$provider, "together")
      expect_equal(last_call$fn, "together")
      # Ollama
      res_ollama <- submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                                     prompt_template = tmpl, backend = "ollama",
                                     verbose = FALSE, status_every = 1)
      expect_equal(res_ollama$provider, "ollama")
      expect_equal(last_call$fn, "ollama")
      # Unsupported backend
      expect_error(
        submit_llm_pairs(pairs_tbl, model, trait_name, trait_description,
                         prompt_template = tmpl, backend = "fake"),
        "should be one of \"openai\", \"anthropic\", \"gemini\", \"together\", \"ollama\""
      )
    }
  )
})

test_that(".retry_httr2_request retries on transient statuses and returns success", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L

  # FIX: Create a request that does NOT throw on error (is_error returns FALSE),
  # ensuring req_perform returns the response object (500) instead of throwing.
  # This matches the behavior tested by the original mock.
  dummy_req <- httr2::request("http://example.com") |>
    httr2::req_error(is_error = function(resp) FALSE)

  mk_resp <- function(status) {
    httr2::response(status_code = status)
  }

  # Mock: 1st attempt -> 500, 2nd attempt -> 200
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    if (attempt_env$count == 1L) mk_resp(500L) else mk_resp(200L)
  }

  httr2::local_mocked_responses(mock_callback)

  out <- pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 3L, base_delay = 0)

  expect_equal(attempt_env$count, 2L)
  expect_s3_class(out, "httr2_response")
  expect_equal(out$status_code, 200L)
})

test_that(".retry_httr2_request handles httr2_http errors and rethrows when non-transient", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L
  # Standard request that throws on error (default)
  dummy_req <- httr2::request("http://example.com")

  mk_error <- function(status) {
    resp <- httr2::response(status_code = status)
    structure(list(message = paste0("HTTP ", status), response = resp),
              class = c("httr2_http", "error", "condition"))
  }

  # Mock: Throw 400 immediately
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    stop(mk_error(400L))
  }

  httr2::local_mocked_responses(mock_callback)

  expect_error(
    pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 2L, base_delay = 0),
    class = "httr2_http"
  )
  expect_equal(attempt_env$count, 1L)
})

test_that(".retry_httr2_request retries on httr2_http transient errors and eventually succeeds", {
  requireNamespace("httr2")

  attempt_env <- new.env(parent = emptyenv())
  attempt_env$count <- 0L
  dummy_req <- httr2::request("http://example.com")

  mk_error <- function(status) {
    resp <- httr2::response(status_code = status)
    structure(list(message = paste0("HTTP ", status), response = resp),
              class = c("httr2_http", "error", "condition"))
  }

  # Mock: Throw 503 (transient) twice, then return 200
  mock_callback <- function(req) {
    attempt_env$count <- attempt_env$count + 1L
    if (attempt_env$count <= 2L) {
      stop(mk_error(503L))
    } else {
      httr2::response(status_code = 200L)
    }
  }

  httr2::local_mocked_responses(mock_callback)

  out <- pairwiseLLM:::.retry_httr2_request(dummy_req, max_attempts = 3L, base_delay = 0)

  expect_equal(attempt_env$count, 3L)
  expect_s3_class(out, "httr2_response")
  expect_equal(out$status_code, 200L)
})
